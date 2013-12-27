(ns clj-nkjp.tagging
  (:require [instaparse.core :as insta]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clj-nkjp.polimorf :as polimorf]
            [clj-nkjp.tagset :as tagset])
  (:use [clj-nkjp.tagset :only [split-when]]
        clojure.set
        clojure.test))

(def grammar
  "S = COND ' -> ' ACTION
   COND = SIMPLECOND | '(' COND ')' | COND ' OR ' COND | COND ' AND ' COND
   SIMPLECOND = TAGPART ' = ' VALSPEC | SEGSPEC ENDSWITH WORDSPEC | SEGSPEC STARTSWITH WORDSPEC
   WORDSPEC = '\\'' WORD '\\'' | 'capital letter'
   SEGSPEC = TAG | 'nearby segment' | 'ORTH' | 'ORTH[' NUMBER ']'
   TAG = 'T[' NUMBER ']'
   TAGPART = TAG | TAG '|' TAGSPEC
   ENDSWITH = ' ends ' ('(' NUMBER ' chars) ')? 'with '
   STARTSWITH = ' starts with '
   NUMBER = #'-?[0-9]+'
   TAGSPEC = #'[a-z,]+'
   VALSPEC = #'[a-z0-9,*:]+'
   WORD = #'[-ąćęłńóśźża-z,.]+'
   ACTION = TAGPART ' := ' VALSPEC")

(def parser (insta/parser grammar))

(defmacro descend [tree & numbers]
  `(-> ~tree ~@(map (fn [num] `(nth ~num)) numbers)))

(defn as-int [x]
  (Integer/parseInt x))

(defn postprocess-parse-tree [tree]
  (condp = (first tree)
    :S {:condition (postprocess-parse-tree (second tree)), :action (postprocess-parse-tree (nth tree 3))}
    :ACTION `(:= ~(postprocess-parse-tree (second tree)) ~(descend tree 3 1))
    :TAGPART (if (next (next tree))
               `(:project ~(postprocess-parse-tree (nth tree 1)) ~(descend tree 3 1))
               (postprocess-parse-tree (nth tree 1)))
    :TAG `(:tag ~(as-int (descend tree 2 1)))
    :COND (cond (= (second tree) "(") (postprocess-parse-tree (nth tree 2))
                (= (-> tree rest rest first) " AND ") `(:and ~(postprocess-parse-tree (nth tree 1)) ~(postprocess-parse-tree (nth tree 3)))
                (= (-> tree rest rest first) " OR ") `(:or ~(postprocess-parse-tree (nth tree 1)) ~(postprocess-parse-tree (nth tree 3)))
                :otherwise (postprocess-parse-tree (second tree)))
    :VALSPEC (second tree)
    :SIMPLECOND (list (cond (= (nth tree 2) " = ") :=
                            (= (descend tree 2 0) :STARTSWITH) :starts-with
                            (= (descend tree 2 0) :ENDSWITH) :ends-with
                            :otherwise (nth tree 2))
                      (postprocess-parse-tree (nth tree 1))
                      (postprocess-parse-tree (nth tree 3)))
    :SEGSPEC (condp = (nth tree 1)
               "nearby segment" :nearby-segment
               "ORTH" `(:orth 0)
               "ORTH[" `(:orth ~(as-int (descend tree 2 1)))
               (postprocess-parse-tree (nth tree 1)))
    :WORDSPEC (condp = (nth tree 1) 
                "capital letter" :capital-letter
                "'" (postprocess-parse-tree (nth tree 2))
                tree) 
    :WORD (nth tree 1)
    tree))

(defn starts-with [^String s ^String prefix]
  (.startsWith s prefix))

(defn ends-with [^String s ^String suffix]
  (when s
    (.endsWith s suffix)))

(defn parse-unigram [lines]
  (into {}
        (for [[word & interps] (split-when #(not (starts-with % "\t")) lines)]
          [word
           (sort-by second >
                    (for [[k v] (map #(string/split (string/trim %) #" -> ") interps)]
                      [k (as-int v)]))])))

(defn load-tagging-data []
  (with-open [f (io/reader (io/resource "tagging.dat"))]
    (let [[unigram1 unigram2 rules1 rules2] (split-when #(starts-with % "===") (line-seq f))]
      {:unigram (parse-unigram (rest unigram1)),
       :rules (doall (map #(postprocess-parse-tree (parser %)) (rest rules1)))})))

(defn tokenize [s]
  (filter seq (string/split s #"\pZ+|(?=\pP)(?<!\pP)|(?<=\pP)(?!\pP)")))

(defn compile-condition [cnd]
  (condp = (first cnd)
    :and `(and ~(compile-condition (nth cnd 1)) ~(compile-condition (nth cnd 2)))
    :or `(or ~(compile-condition (nth cnd 1)) ~(compile-condition (nth cnd 2)))
    :ends-with (cond
                (= (nth cnd 1) :nearby-segment) 
                `(some (fn [x#] (ends-with (:orth (~'tag x#)) ~(nth cnd 2))) (range -3 4)) ;; TBD
                (#{:orth :tag} (descend cnd 1 0)) `(ends-with (:orth (~'tag ~(descend cnd 1 1))) ~(nth cnd 2))
                :otherwise (throw (Exception. "unsupported ends-with")))
    :starts-with (cond
                  (and (= (descend cnd 1 0) :orth) (= (nth cnd 2) :capital-letter))
                  `(Character/isUpperCase (first (:orth (~'tag ~(descend cnd 1 1)))))
                  (= (descend cnd 1 0) :orth)
                  `(starts-with (:orth (~'tag ~(descend cnd 1 1))) ~(nth cnd 2))
                  :otherwise (throw (Exception. "unsupported starts-with")))
    := (cond
        (= (descend cnd 1 0) :tag)
        (let [parsed-tag (tagset/parse-ctag polimorf/t3 (nth cnd 2))]
          `(= (select-keys (~'tag ~(descend cnd 1 1)) ~(vec (keys parsed-tag))) ~parsed-tag))

        (= (descend cnd 1 0) :project)
        (let [tag-num (descend cnd 1 1 1)
              categories (string/split (descend cnd 1 2) #",")
              values (string/split (nth cnd 2) #",")
              _ (assert (= (count categories) (count values)))
              cv-pairs (remove #(= (second %) "*") (map vector categories values))
              tagsym (gensym)]
          `(let [~tagsym (~'tag ~tag-num)] (and ~@(for [[c v] cv-pairs] `(= (~(keyword c) ~tagsym) ~v)))))
        :otherwise (throw (Exception. "unsupported =")))))

(defn find-nearest-tag [tag considered-tags fixed-category]
  (let [matching-tags (filter #(and (= (:pos tag) (:pos %))
                                    (or (not fixed-category) (= (fixed-category tag) (fixed-category %))))
                              considered-tags)
        stag (set tag)]
    (when (seq matching-tags)
      (apply max-key #(count (intersection stag (set %))) matching-tags))))

(deftest test-find-nearest-tag
  (let [tag {:pos "subst", :case "acc"}
        considered-tags [{:person "sec", :pos "impt"} {:person "pri", :pos "fin"} {:case "gen", :pos "subst"}]]
    (is (= (find-nearest-tag tag considered-tags nil) {:case "gen", :pos "subst"}))
    (is (= (find-nearest-tag tag considered-tags :case) nil))))

(defn compile-action [[assign lval rval]]
  (assert (= assign :=))
  (cond
   (= lval '(:tag 0))
   (let [new-tag (tagset/parse-ctag polimorf/t3 rval)]
     `(if ((:considered ~'current-seg) new-tag)
        (into ~new-tag {:orth (:orth ~'current-seg) :considered (:considered ~'current-seg)})
        ~'current-seg))
   (and (= (first lval) :project) (= (second lval) '(:tag 0)))
   (let [changed-category (keyword (nth lval 2))]
     `(let [new-tag# (assoc ~'current-seg ~changed-category ~rval)
            new-tag# (find-nearest-tag (dissoc new-tag# :considered :orth) (:considered ~'current-seg) ~changed-category)]
        (if new-tag#
          (assoc new-tag# :orth (:orth ~'current-seg) :considered (:considered ~'current-seg))
          ~'current-seg)))
   :otherwise nil))

(defn vget [v i]
  (when (contains? v i) (v i)))

(defn rule-code
  [{:keys [condition action], :as rule}]
  `(fn [text# pos#]
     (let [~'tag (fn [i#] (vget text# (+ i# pos#)))
           ~'current-seg (~'tag 0)]
       (try
         (if ~(compile-condition condition)
           ~(compile-action action)
           ~'current-seg)
         (catch Exception e#
           (println "Rule failed:" ~(pr-str rule) "on text:" (pr-str text#) "segment:" pos# "namely" (~'tag 0))
           (throw e#))))))
  
;; zwraca funkcję, która bierze wektor segmentów i pozycję w nim
;; i zwraca segment na tej pozycji zmieniony regułą lub nie
(defn compile-rule 
  [rule]
  (eval (rule-code rule)))

(defn project-to-simplified-tagset [interp]
  (select-keys interp [:pos :case :person :fullstoppedness]))

(defn tag-word-simplified [word unigram]
  (let [polimorf-tags (map second (polimorf/analyze-t3 word))
        tags (unigram word)
        simplify-tag (fn [tag] (project-to-simplified-tagset (tagset/parse-ctag polimorf/t3 tag)))]
    (assoc
        (if tags
          (tagset/parse-ctag polimorf/t3 (first (first tags)))
          (simplify-tag (first polimorf-tags)))
      :considered (set (map simplify-tag polimorf-tags))
      :orth word)))

(defn tag-phase-0 [text]
  (map #(tag-word-simplified % (td :unigram)) (tokenize text)))

(defn process-rule [text rule]
  (vec (map (partial rule text) (range (count text)))))

(let [rules (map compile-rule (:rules td))]
  (defn tag-phase-1 [text]
    (reduce process-rule (vec (tag-phase-0 text)) rules)))

(defn try-rules [text position]
  (let [len (dec (count text))]
    (for [rule (map :condition (:rules td))]
      (let [to-ev `(let [~'tag (fn [x#] (let [pos# (+ ~position x#)] (when (<= 0 pos# ~len) (~text pos#))))
                         ~'serialize-tag (fn [t#] (tagset/serialize-tag polimorf/t3 t#))]
                     ~(compile-condition rule))]
        [rule (eval to-ev)]))))
  
