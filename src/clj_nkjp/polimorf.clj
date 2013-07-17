(ns clj-nkjp.polimorf
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clj-nkjp.tags :as tags]
            [clj-nkjp.tagset :as tagset])
  (:import  [morfologik.stemming PolishStemmer WordData]))

(let [stemmer (PolishStemmer.)]
  (defn analyze [word]
    (locking stemmer
      (let [res (map (fn [^WordData x] {:base (-> x .getStem str) :tag (-> x .getTag str)}) (.lookup stemmer word))]
        (when-not (empty? res) res)))))

(defn expand-single [tag]
  (map (partial string/join ":")
       (apply combinatorics/cartesian-product
              (map #(string/split % #"\.") (string/split tag #":")))))

(defn semiexpand-tag [tag]
  (string/split tag #"[+|]"))

(defn expand-tag [tag]
  (mapcat expand-single (semiexpand-tag tag)))

(defn extract-word-data [^WordData w]
  [[(str (.getWord w)) (str (.getStem w))] (semiexpand-tag (.getTag w))])

(defn all-tags-contracted []
  (let [s (PolishStemmer.)]
    (reduce (fn [accu ^WordData wd] (conj accu (str (.getTag wd)))) #{} s)))

(defn dump-dict []
  (doseq [^WordData w (PolishStemmer.) t (expand-tag (str (.getTag w)))]
    (printf "%s\t%s\t%s\n" (str (.getWord w)) (str (.getStem w)) t)))

(defn find-words-by-tag [t]
  (let [s (PolishStemmer.)]
    (filter (fn [^WordData wd] ((set (expand-tag (str (.getTag wd)))) t)) s)))

(defn load-polimorf-tab [rdr]
  (map (fn [x]
         [(first (first x)) (second (first x))
          (distinct (mapcat #(semiexpand-tag (nth % 2)) x))])
       (partition-by (partial take 2) (map #(string/split % #"\t") (line-seq rdr)))))

(defmacro with-polimorf [f & body]
  `(with-open [~f (io/reader "/home/nathell/projects/smyrna/other/polimorf/PoliMorf-0.6.5.tab")]
     ~@body))

(defn tag-frequencies-semicontracted []
  (let [s (PolishStemmer.)]
    (frequencies (mapcat semiexpand-tag (map #(str (.getTag %)) s)))))

(defn morfologik->t3 [tag]
  (if (= tag "num:comp")
    "ign"
    (-> tag
        (string/replace #"^verb:pred:pot" "pot")
        (string/replace #"^verb:" "")
        (string/replace #":(non)?refl" "")
        (string/replace #"n[12]" "n")
        (string/replace #"p[23]" "n")
        (string/replace #"p1" "m1")
        ((fn [t] (if (.startsWith t "siebie") (string/replace t #":n?akc" "") t))))))

;;; NKJP to T3

(defn parse-tag-from-segment [seg]
  (assoc
   (tagset/parse-tag (-> seg :disamb first :interpretation first))
   :orth (-> seg :orth first)
   :nps (-> seg :nps first)))

(defn sentence->segs [sentence]
  (map parse-tag-from-segment (rest sentence)))

(defn stringify-sentence [sentence]
  (apply str (map #(str (when-not (:nps %) " ") (:orth %)) (sentence->segs sentence))))

(defn sentences [document]
  (filter #(= (first %) :s) (tree-seq vector? rest document)))

(defn combine-subsegments [main aux errors]
  (if (seq aux)
    (reduce
     (fn [[curr errors] aux]
       (cond
        (= (:pos aux) "aglt") (if (#{"praet" "pot" "winien"} (:pos curr))
                                [(assoc curr :person (:person aux)) errors]
                                [curr (conj errors {:type :aglt, :aux aux, :main main, :partial curr})])
        (= (:orth aux) "by") (if (= (:pos curr) "praet")
                               [(assoc curr :pos "pot") errors]
                               [curr (conj errors {:type :pot, :aux aux, :main main, :partial curr})])
        :otherwise [curr (conj errors {:type :unsupported, :aux aux, :main main, :partial curr})]))
     [(assoc main :orth (apply str (:orth main) (map :orth aux))) errors]
     aux)
    [main errors]))

(defn nkjp->t3 [segs]
  (when (seq segs)
    (lazy-seq
     (let [fst (first segs) rst (rest segs)]
       (if (= (:pos fst) "interp")
         (cons [fst nil] (nkjp->t3 rst))
         (let [[aux nxt] (split-with #(and (:nps %) (not= (:pos %) "interp")) rst)]
           (cons (combine-subsegments fst aux []) (nkjp->t3 nxt))))))))

(defn checker
  "Try running nkjp->t3 on all sentences in the corpus, to see
how many unsupported cases we have."
  []
  (->>
   (for [f (rest (file-seq (io/file "/home/nathell/corpora/nkjp-clj/")))
         :let [docum (read-string (slurp f))]
         sent (sentences docum)]
     (apply concat (map second (nkjp->t3 (sentence->segs sent)))))
   (apply concat)))
