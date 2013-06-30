(ns clj-nkjp.tei
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

(defn parse-tei-tag [x]
  (condp = (:tag x)
    :string (-> x :content first)
    :symbol (-> x :attrs :value)
    :binary (condp = (-> x :attrs :value)
              "true" true
              "false" false)
    :vAlt (set (map parse-tei-tag (:content x)))
    :f [(-> x :attrs :name keyword)
        (map parse-tei-tag (:content x))]
    :fs (into {:type (-> x :attrs :type)}
              (map parse-tei-tag (:content x)))))

(defn extract-segment [x]
  (xml/->Event :segment nil nil (-> x xml/event-tree :content first parse-tei-tag)))

(defn as-sexp [x]
  (if-let [t (:tag x)]
    (into [t] (map as-sexp (:content x)))
    x))

(defn extract-segments [l]
  (lazy-seq
   (let [[upto-start start] (split-with #(or (not= (:type %) :start-element) (not= (:name %) :seg)) l)]
     (concat upto-start
             (when (seq start)
               (let [[seg rst] (split-with #(or (not= (:type %) :end-element) (not= (:name %) :seg)) start)]
                 (cons
                  (extract-segment seg)
                  (extract-segments (next rst)))))))))

(defn dump-segments [segs]
  (doseq [x segs]
    (condp = (:type x)
      :start-element (print (str " [" (:name x) ""))
      :end-element (print "]")
      :segment (do (print " ") (pr (:str x))))))

(def input-dir "/home/nathell/corpora/NKJP")

(defn run []
  (doseq [x (file-seq (io/file input-dir)) :when (.endsWith (str x) "ann_morphosyntax.xml")]
    (with-open [f (io/writer (str (.getParent x) ".clj"))]
      (binding [*out* f]
        (-> x io/reader xml/source-seq extract-segments dump-segments)))))