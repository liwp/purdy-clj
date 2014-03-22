(ns purdy.core)

;; TODO:
;; empty, flatten, and concat all collide with clojure names

;; DOC
(defprotocol IDocument
  (flatten [this] "Flatten this document to one line"))

(declare alt)
(declare concat)
(declare group)
(declare nest)
(declare text)

;; DOC | NIL
(defrecord DEmpty []
  IDocument
  (flatten [this]
    this))

;; DOC | DOC :<> DOC
(defrecord DConcat [doc-a doc-b]
  IDocument
  (flatten [this]
    (concat (flatten doc-a) (flatten doc-b))))

;; DOC | NEST Int DOC
(defrecord DNest [i doc]
  IDocument
  (flatten [this]
    (nest i (flatten doc))))

;; DOC | TEXT String
(defrecord DText [s]
  IDocument
  (flatten [this]
    this))

;; DOC | LINE
(defrecord DLine []
  IDocument
  (flatten [this]
    (text " ")))

;; DOC | DOC :<|> DOC
(defrecord DAlt [doc-a doc-b]
  IDocument
  (flatten [this]
    (flatten doc-a)))

;;; ctor functions
;; nil - nil can't be a method name in clojure
(defn empty
  []
  (->DEmpty))

(defn concat
  [doc-a doc-b]
  (->DConcat doc-a doc-b))

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
  [doc-a doc-b]
  (->DAlt doc-a doc-b))

(defn group
  [doc]
  (alt (flatten doc) doc))

;; Doc
(defprotocol ILayout
  ;; rename to to-string?
  (layout [this] "Render this layout item as a string.")
  (fits [this w] "Does this item fit in the available space?"))

;; Doc | Nil
(defrecord LNil []
  ILayout
  ;; rename to-string?
  (layout [this]
    "")
  (fits [this w]
    (pos? w)))

;; Doc | String `Text` Doc
(defrecord LText [s doc]
  ILayout
  (layout [this]
    (str s (layout doc)))
  
  (fits [this w]
    (fits doc (- w (count s)))))

(defn indent
  [n]
  (apply str \newline (repeat n \space)))

;; Doc | Int `Line` Doc
(defrecord LLine [i doc]
  ILayout
  (layout [this]
    (str (indent i) (layout doc)))

  (fits [this w]
    (pos? w)))

;; rename to layout?
(defmulti be (fn [w k [[i doc] & docs]] (type doc)))

(defmethod be nil
  [w k docs]
  (->LNil))

(defmethod be DEmpty
  [w k [[i doc] & docs]]
  (be w k doc))

(defmethod be DConcat
  [w k [[i doc] & docs]]
  (be w k (clojure.core/concat [[i (:doc-a doc)]] [[i (:doc-b doc)]] docs)))

(defmethod be DNest
  [w k [[i doc] & docs]]
  (be w k (clojure.core/concat [[(+ k i) doc]] docs)))

(defmethod be DText
  [w k [[i doc] & docs]]
  (let [s (:s doc)]
    (->LText s (be w (+ k (count s)) docs))))

(defmethod be DLine
  [w k [[i doc] & docs]]
  (->LLine i (be w i docs)))

;; TODO: lazy y
(defn better
  [w k x y]
  (if (fits x (- w k))
    x
    y))

(defmethod be DAlt
  [w k [[i doc] & docs]]
  (better w k
          (be w k (clojure.core/concat [[i (:doc-a doc)]] docs))
          (be w k (clojure.core/concat [[i (:doc-b doc)]] docs))))

(defn best
  [w k doc]
  (be w k [[0 doc]]))

(defn pretty
  "Pretty-print the given IDocument in the specified number of columns."
  [w doc]
  (layout (best w 0 doc)))

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
