(ns purdy.document
  (:require [clojure.string :as str]))

(defprotocol IPrintable
  (to-doc [this] "Convert the type to a IDocument."))

(defprotocol IDocument
  (flatten [this] "Flatten this document to one line"))

(defrecord DEmpty []
  IDocument
  (flatten [this]
    this))

(defrecord DConcat [head tail]
  IDocument
  (flatten [this]
    (->DConcat (flatten head) (flatten tail))))

(defrecord DNest [indent doc]
  IDocument
  (flatten [this]
    (->DNest indent (flatten doc))))

(defrecord DText [text]
  IDocument
  (flatten [this]
    this))

(defrecord DLine []
  IDocument
  (flatten [this]
    (->DText " ")))

(defrecord DAlt [a b]
  IDocument
  (flatten [this]
    (flatten a)))

;;; ctor functions
(defn- rfold
  [f xs]
  (reduce #(f %2 %1) (reverse xs)))

(defn empty
  []
  (->DEmpty))

(defn concat
  [& docs]
  (rfold ->DConcat docs))

(defn nest
  [i doc]
  (->DNest i doc))

(defn text
  [s]
  (->DText s))

(defn line
  []
  (->DLine))

(defn alt
  [a b]
  (->DAlt a b))

(defn group
  [doc]
  (alt (flatten doc) (delay doc)))

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
  [xs]
  (rfold <|> xs)
  #_  (folddoc <|> xs))

(defn <-|>
  [x y]
  (concat x (alt (text " ") (delay (line))) y))

(defn bracket
  [l x r]
  (group (concat (text l)
                 (nest 2 (concat (line) x))
                 (line)
                 (text r))))

(defn fillwords
  [s]
  (->> (str/split s #"\s+")
       (map text)
       (folddoc <-|>)))

;; TODO: clean this up.
;; TODO: can we avoid spaces when using with bracket
(defn fill
  [[x & xs]]
  (cond
   (nil? x)  (empty)
   (nil? xs) x
   :else (alt (<-> (flatten x)
                   (fill (clojure.core/concat [(flatten (first xs))] (rest xs))))
              (delay (<|> x (fill xs))))))

(defn fill-str
  [xs]
  (fill (map (comp text str) xs)))
