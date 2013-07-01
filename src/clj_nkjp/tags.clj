(ns clj-nkjp.tags
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn strip-orth [interp]
  (string/replace interp #"^.*?:" ""))

(defn golden-tag [seg]
  (let [interp (-> seg :disamb first :interpretation first)
        once (strip-orth interp)]
    (cond
     (= interp "::interp") (strip-orth once)
     (.startsWith interp "(:") (strip-orth once)
     (.startsWith interp ":)") (strip-orth once)
     (.startsWith interp ":(") (strip-orth once)
     (.startsWith interp ":-") (strip-orth once)
     (.startsWith interp ":o") (strip-orth once)
     (.startsWith interp ":]") (strip-orth once)
     (.startsWith interp ":P") (strip-orth once)
     (.startsWith interp ":D") (strip-orth once)
     (.startsWith interp ":|") (strip-orth once)
     (.startsWith interp ":\\") (strip-orth once)
     (.startsWith interp ":O") (strip-orth once)
     (.startsWith once "//") (let [twice (strip-orth once)]
                               (if (.startsWith twice "80")
                                 (strip-orth twice)
                                 twice))
     (.startsWith interp "news:pl") (strip-orth once)
     (.startsWith interp "news:9") (strip-orth once)
     (.startsWith interp "E::") (strip-orth once)
     :otherwise once)))

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