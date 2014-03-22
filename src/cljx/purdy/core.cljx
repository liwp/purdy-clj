(ns purdy.core
  (:require [purdy.document :as doc]
            [purdy.layout :as layout]
            [clojure.string :as s]))

;; TODO:
;; tests
;; empty, flatten, and concat all collide with clojure names
;; add lazyness
;; add print functions
;; add configuration params: width, max length

;;; ctor functions
;; nil - nil can't be a method name in clojure
(defn empty
  []
  (doc/->DEmpty))

(defn concat
  [& docs]
  (reduce #(doc/->DConcat %2 %1) (empty) (reverse docs)))

(defn nest
  [i doc]
  (doc/->DNest i doc))

(defn text
  [s]
  (doc/->DText s))

(defn line
  []
  (doc/->DLine))

(defn alt
  [doc-a doc-b]
  (doc/->DAlt doc-a doc-b))

(defn group
  [doc]
  (alt (doc/flatten doc) doc))

;; combinators
(defn space
  []
  (text " "))

(defn <->
  [x y]
  (concat x (space) y))

(defn <|>
  [x y]
  (concat x (line) y))

(defn folddoc
  [f [x & xs]]
  (cond
   (nil? x)  (empty)
   (nil? xs) x
   :else (f x (folddoc f xs))))

(defn spread
  [& xs]
  (folddoc <-> xs))

(defn stack
  [& xs]
  (folddoc <|> xs))

(defn <-|>
  [x y]
  (concat x (alt (text " ") (line)) y))

(defn bracket
  [l x r]
  (group (concat (text l)
                 (nest 2 (concat (line) x))
                 (line)
                 (text r))))

(defn fillwords
  [s]
  (->> (s/split s #"\s+")
       (map text)
       (folddoc <-|>)))

(defn fill
  [[x & xs]]
  (cond
   (nil? x)  (empty)
   (nil? xs) x
   :else (alt (<-> (doc/flatten x)
                   (fill (clojure.core/concat [(doc/flatten (first xs))] (rest xs))))
              (<|> x (fill xs)))))

(defn pretty
  "Pretty-print the given IDocument in the specified number of columns."
  [w doc]
  (layout/layout-doc w doc))

(def t (group (concat
         (text "hello")
         (concat (line) (text "a")))))

(def t3 (concat (text "hello")
                (concat (line) (text "a"))))

(def t2 (group
        (concat
         (group
          (concat
           (group
            (concat
             (group
              (concat
               (text "hello")
               (concat (line) (text "a"))))
             (concat (line) (text "b"))))
           (concat (line) (text "c"))))
         (concat (line) (text "d")))))

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
    (concat (show-tree' t) (text ",") (line) (show-trees' ts))))

(defn show-bracket'
  [ts]
  (if (nil? ts)
    (empty)
    (concat (text "[") (nest 1 (show-trees' ts)) (text "]"))))

(defn show-tree'
  [t]
  (group (concat (text (:n t)) (nest (count (:n t)) (show-bracket' (:cs t))))))

(defn testtree'
  [w]
  (print (pretty w (show-tree' tree))))

(declare show-tree)

(defn show-trees
  [[t & ts]]
  (if (nil? ts)
    (show-tree t)
    (concat (show-tree t) (text ",") (line) (show-trees ts))))

(defn show-bracket
  [ts]
  (if (empty? ts)
    (empty)
    (bracket "[" (show-trees ts) "]")))

(defn show-tree
  [t]
  (concat (text (:n t)) (show-bracket (:cs t))))

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
    (empty)
    (bracket "" (fill (apply clojure.core/concat (map f xs))) "")))

(defn show-attr
  [a]
  ;; TODO: quote the value
  [(concat (text (:name a)) (text "=") (text (str "\"" (:value a) "\"")))])

(defn show-tag
  [x]
  (concat (text (:elt x)) (show-fill show-attr (:attrs x))))

(defn show-xmls
  [x]
  (cond
   (string? x) (map text (s/split x #"\s+"))
   (and (:elt x) (:cs x)) [(concat (text "<") (show-tag x) (text ">")
                                   (show-fill show-xmls (:cs x))
                                   (text "</") (text (:elt x)) (text ">"))]
   (:elt x)               [(concat (text "<") (show-tag x) (text "/>"))]
))

(defn show-xml
  [x]
  (folddoc concat (show-xmls x)))

(defn testxml
  [w]
  (print (pretty w (show-xml xml))))
