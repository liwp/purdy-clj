(ns purdy.core
  (:require [purdy.document :as doc]
            [purdy.layout :as layout]))

;; TODO:
;; add combinators
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
  [doc-a doc-b]
  (doc/->DConcat doc-a doc-b))

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
;; TODO

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
