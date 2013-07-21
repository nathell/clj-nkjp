(ns clj-nkjp.xces
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clj-nkjp.polimorf :as polimorf]
            [clj-nkjp.tagset :as tagset]
            [clojure.string :as string]))

(defn seg->tok [seg]
  (let [disamb [(:base seg) (tagset/serialize-tag polimorf/t3 seg)]
        interps (conj (set (polimorf/analyze-t3 (:orth seg))) disamb)]
    (conj
     (if (:nps seg) [[:ns]] [])
     (into [:tok [:orth (:orth seg)]]
           (for [interp interps]
             [:lex
              (if (= interp disamb) {:disamb "1"} {})
              [:base (first interp)]
              [:ctag (second interp)]])))))

(defn elt->xces [elt]
  (into (if (= (first elt) :body)
          [:chunkList]
          [:chunk {:type (name (first elt))}])
        (if (map? (second elt))
          (mapcat seg->tok (map first (polimorf/nkjp->t3 (polimorf/sentence->segs elt))))
          (map elt->xces (rest elt)))))

(defn doc->xces [doc]
  [:cesAna {:version "1.0" :type "lex disamb"}
   (elt->xces (-> doc (nth 2) (nth 2) (nth 1)))])

(defn recode-corpus [input-dir output-dir]
  (doseq [input-file (file-seq (io/file input-dir))
          :when (.endsWith (str input-file) ".clj")
          :let [output-file (io/file (format "%s/%s/morph.xml" output-dir
                                             (string/replace (.getName input-file) ".clj" "")))]]
    (io/make-parents output-file)
    (with-open [f (io/writer output-file)]
      (xml/emit (-> input-file slurp read-string doc->xces xml/sexps-as-fragment) f))))
