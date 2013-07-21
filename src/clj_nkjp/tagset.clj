(ns clj-nkjp.tagset
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn section [line]
  (second (re-find #"^\[(.+)\]" line)))

(defn comment? [line]
  (or (.startsWith line "#") (empty? line)))

;; from https://groups.google.com/forum/#!topic/clojure/T11czh8zKjA
(defn split-when [pred coll]
  (let [ipred (complement pred)
        bits (iterate
              (fn [[out coll]]
                (let [[a b] (split-with ipred (rest coll))]
                  [(cons (first coll) a) b]))
              [nil coll])]
    (map #(first (first %))
         (take-while #(seq (second (second %))) (map vector (rest bits) bits)))))

(defn parse-value [val]
  (if-let [optional (section val)] [optional] val))

(defn keywordize [x]
  (if (vector? x)
    [(keyword (first x))]
    (keyword x)))

(defn parse-line [line]
  (let [[name values-str] (map string/trim (string/split line #"=" 2))
        values (when (seq values-str) (string/split values-str #" "))]
    [name (map parse-value values)]))

(defn compile-tagset [attr pos]
  (into {:pos (into {} pos)}
        (for [[k vs] attr v vs] [v k])))

(defn read-tagset [f]
  (with-open [rdr (io/reader f)]
    (let [sections (split-when section (remove comment? (line-seq rdr)))
          section-map (into {} (for [[header & lines] sections]
                                 [(keyword (string/lower-case (section header))) lines]))
          {:keys [attr pos]} section-map]
      (compile-tagset (map parse-line attr) (map parse-line pos)))))

(def nkjp (read-tagset (io/resource "tagsets/nkjp.tagset")))

(defn serialize-tag
  ([tag] (serialize-tag nkjp tag))
  ([tagset tag]
     (string/join ":"
                  (into [(:pos tag)]
                        (for [cat (flatten ((:pos tagset) (:pos tag)))
                              :let [part (tag (keyword cat))]
                              :when part]
                          part)))))

(defn verify-tag
  [{:keys [pos]} tag]
  (let [categories (pos (:pos tag))
        allowed-categories (set (map keyword (flatten categories)))
        keyset (set (keys tag))]
    (when-not categories
      (throw (Exception. (format "Unknown class: %s" (:pos tag)))))
    (doseq [[k v] tag :when (not (#{:pos :base :orth :nps} k))]
      (when-not (allowed-categories k)
        (throw (Exception. (format "%s doesn't inflect with %s: %s" (:pos tag) k tag)))))
    (doseq [cat categories :when (not (vector? cat))]
      (when-not (keyset (keyword cat))
        (throw (Exception. (format "%s needs to inflect with %s: %s" (:pos tag) cat tag)))))
    tag))

(defn parse-tag
  ([tag] (parse-tag nkjp tag))
  ([tagset tag]
     (let [[orth [the-pos & tag-parts]] (split-when (:pos tagset) (string/split tag #":"))]
       (verify-tag tagset
                   (into {:pos the-pos, :base (string/join ":" orth)}
                         (for [part tag-parts :let [part-name (tagset part)]]
                           (if part-name
                             [(keyword part-name) part]
                             (throw (Exception. (format "Unknown tag part: %s" part))))))))))
