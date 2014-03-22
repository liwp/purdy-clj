(ns purdy.document)

(defprotocol IDocument
  (flatten [this] "Flatten this document to one line"))

(defrecord DEmpty []
  IDocument
  (flatten [this]
    this))

;; TODO: head, tail?
(defrecord DConcat [doc-a doc-b]
  IDocument
  (flatten [this]
    (->DConcat (flatten doc-a) (flatten doc-b))))

;; TODO: i -> indent
(defrecord DNest [i doc]
  IDocument
  (flatten [this]
    (->DNest i (flatten doc))))

;; TODO: s -> text
(defrecord DText [s]
  IDocument
  (flatten [this]
    this))

(defrecord DLine []
  IDocument
  (flatten [this]
    (->DText " ")))

;; TODO: just a & b?
(defrecord DAlt [doc-a doc-b]
  IDocument
  (flatten [this]
    (flatten doc-a)))
