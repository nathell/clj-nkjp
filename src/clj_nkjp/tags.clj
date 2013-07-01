(ns clj-nkjp.tags
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clj-nkjp.tagset :as tagset]))

(defn strip-orth [interp]
  (string/replace interp #"^.*?:" ""))

(defn golden-tag [seg]
  (->> seg :disamb first :interpretation first tagset/parse-tag tagset/serialize-tag))

(defn segment-seq [x]
  (filter map? (tree-seq vector? next x)))

(defn tag-frequencies-from-file [f]
  (with-open [rdr (java.io.PushbackReader. (io/reader f))]
    (->> rdr edn/read segment-seq (map golden-tag) frequencies)))

(def corpus-dir "/home/nathell/corpora/nkjp-clj")

(defn corpus-files []
  (filter #(.isFile %) (file-seq (io/file corpus-dir))))

(defn samples
  [f coll]  
  (persistent!
   (reduce
    (fn [ret x]
      (let [k (f x)]
        (assoc! ret k x)))
    (transient {}) coll)))

(defn tag-samples-from-file [f]
  (with-open [rdr (java.io.PushbackReader. (io/reader f))]
    (->> rdr edn/read segment-seq (samples golden-tag))))

(defn frequencies-all []
  (reduce (partial merge-with +)
          {}
          (pmap tag-frequencies-from-file (corpus-files))))

(defn samples-all []
  (reduce merge {} (pmap tag-samples-from-file (corpus-files))))

(defn dump-frequencies [fa]
  (doseq [[k v] (sort-by val > fa)]
    (printf "%s,%s\n" k v)))