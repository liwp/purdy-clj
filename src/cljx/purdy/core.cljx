(ns purdy.core
  (:require [purdy.document :as doc]
            [purdy.layout :as layout]
            [purdy.pretty-printers :as pp]
            [clojure.string :as str])
  (:gen-class))

;; TODO:
;; tests
;; empty, flatten, and concat all collide with clojure names
;; add print functions
;; add configuration params: width, max length

(defn pretty
  "Pretty-print the given IDocument in the specified number of columns."
  [w doc]
  (layout/layout-doc w doc))

;; (def t (group (concat
;;          (text "hello")
;;          (concat (line) (text "a")))))

;; (def t3 (concat (text "hello")
;;                 (concat (line) (text "a"))))

;; (def t2 (group
;;         (concat
;;          (group
;;           (concat
;;            (group
;;             (concat
;;              (group
;;               (concat
;;                (text "hello")
;;                (concat (line) (text "a"))))
;;              (concat (line) (text "b"))))
;;            (concat (line) (text "c"))))
;;          (concat (line) (text "d")))))

;;; TREES
(def tree
  {:n "aaa"
   :cs [{:n "bbbb"
         :cs [{:n "ccc"
               :cs []}
              {:n "dd"
               :cs []}]}
        {:n "eee"
         :cs []}
        {:n "ffff"
         :cs [{:n "gg"
               :cs []}
              {:n "hhh"
               :cs []}
              {:n "ii"
               :cs []}]}]})

(declare show-tree')

(defn show-trees'
  [[t & ts]]
  (if (nil? ts)
    (show-tree' t)
    (doc/concat (show-tree' t) (doc/text ",") (doc/line) (show-trees' ts))))

(defn show-bracket'
  [ts]
  (if (nil? ts)
    (doc/empty)
    (doc/concat (doc/text "[") (doc/nest 1 (show-trees' ts)) (doc/text "]"))))

(defn show-tree'
  [t]
  (doc/group (doc/concat (doc/text (:n t)) (doc/nest (count (:n t)) (show-bracket' (:cs t))))))

(defn testtree'
  [w]
  (print (pretty w (show-tree' tree))))

(declare show-tree)

(defn show-trees
  [[t & ts]]
  (if (nil? ts)
    (show-tree t)
    (concat (show-tree t) (doc/text ",") (doc/line) (show-trees ts))))

(defn show-bracket
  [ts]
  (if (empty? ts)
    (doc/empty)
    (doc/bracket "[" (show-trees ts) "]")))

(defn show-tree
  [t]
  (doc/concat (doc/text (:n t)) (show-bracket (:cs t))))

(defn testtree
  [w]
  (print (pretty w (show-tree tree))))

;;; XML
(def xml
  {:elt "p"
   :attrs [{:name "color" :value "red"}
           {:name "font"  :value "times"}
           {:name "size"  :value "10"}]
   :cs    ["Here is some"
           {:elt "em"
            :cs ["emphasized"]}
           "text."
           "Here is a "
           {:elt "a"
            :attrs [{:name "href" :value "http://www.eg.com/"}]
            :cs ["link"]}
           "elsewhere."]
   })

(defn show-fill
  [f xs]
  (if (empty? xs)
    (doc/empty)
    (doc/bracket "" (doc/fill (apply concat (map f xs))) "")))

(defn show-attr
  [a]
  ;; TODO: quote the value
  [(doc/concat (doc/text (:name a)) (doc/text "=") (doc/text (str "\"" (:value a) "\"")))])

(defn show-tag
  [x]
  (doc/concat (doc/text (:elt x)) (show-fill show-attr (:attrs x))))

(defn show-xmls
  [x]
  (cond
   (string? x) (map doc/text (str/split x #"\s+"))
   (and (:elt x) (:cs x)) [(doc/concat (doc/text "<") (show-tag x) (doc/text ">")
                                       (show-fill show-xmls (:cs x))
                                       (doc/text "</") (doc/text (:elt x)) (doc/text ">"))]
   (:elt x)               [(doc/concat (doc/text "<") (show-tag x) (doc/text "/>"))]
))

(defn show-xml
  [x]
  (doc/folddoc doc/concat (show-xmls x)))

(defn testxml
  [w]
  (print (pretty w (show-xml xml))))

(defn -main
  []
  (let [d (purdy.document/to-doc (into [] (range 100)))]
    (print (pretty 60 d))))
