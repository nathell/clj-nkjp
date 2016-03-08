(ns clj-nkjp.disamb
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn streamline
  [tree]
  (filter #(and (map? %) (= (:type %) "morph")) (tree-seq vector? seq tree)))

(defn orth-disamb-pairs
  [morphs]
  (for [morph morphs]
    {(first (:orth morph))
     {(-> morph :disamb first :interpretation first (string/split #":") first) 1}}))

(defn merge-freqs
  [freq-maps]
  (apply merge-with (partial merge-with +) freq-maps))

(defn file-freqs
  [f]
  (-> f slurp read-string streamline orth-disamb-pairs merge-freqs))

(def input-dir "/home/nathell/corpora/nkjp-clj")

(defn grand-freq-map
  []
  (let [files (filter #(.endsWith (str %) ".clj") (file-seq (io/file input-dir)))]
    (merge-freqs (map file-freqs files))))

(defn disamb-single
  [v]
  (when-let [kv (first (sort-by val > (remove (comp empty? key) v)))]
    (key kv)))

(defn disamb-map
  [m]
  (into {}
        (for [[k v] m :let [v' (disamb-single v)] :when v']
          [k v'])))

(defn generate
  [out]
  (with-open [f (io/writer out)]
    (binding [*out* f]
      (prn (disamb-map (grand-freq-map))))))
