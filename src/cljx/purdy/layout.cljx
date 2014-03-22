(ns purdy.layout
  (:require [purdy.document :as doc]))

(defprotocol ILayout
  ;; rename to to-string?
  (layout [this] "Render this layout item as a string.")
  (fits [this w] "Does this item fit in the available space?"))

(defrecord LNil []
  ILayout
  (layout [this]
    "")

  (fits [this w]
    (>= w 0)))

(defrecord LText [s doc]
  ILayout
  (layout [this]
    (str s (layout doc)))
  
  (fits [this w]
    (fits doc (- w (count s)))))

(defn indent
  [n]
  (apply str \newline (repeat n \space)))

(defrecord LLine [i doc]
  ILayout
  (layout [this]
    (str (indent i) (layout doc)))

  (fits [this w]
    (>= w 0)))

;; TODO: lazy y
(defn better
  [w k x y]
  (if (fits x (- w k))
    x
    y))

;; TODO: rename to layout?
(defmulti be (fn [w k [[i doc] & docs]] (type doc)))

(defmethod be nil
  [w k docs]
  (->LNil))

(defmethod be purdy.document.DEmpty
  [w k [[i doc] & docs]]
  (be w k docs))

(defmethod be purdy.document.DConcat
  [w k [[i doc] & docs]]
  (be w k (clojure.core/concat [[i (:doc-a doc)]] [[i (:doc-b doc)]] docs)))

(defmethod be purdy.document.DNest
  [w k [[i doc] & docs]]
  (be w k (clojure.core/concat [[(+ i (:i doc)) (:doc doc)]] docs)))

(defmethod be purdy.document.DText
  [w k [[i doc] & docs]]
  (let [s (:s doc)]
    (->LText s (be w (+ k (count s)) docs))))

(defmethod be purdy.document.DLine
  [w k [[i doc] & docs]]
  (->LLine i (be w i docs)))

;; TODO: doc-b should be lazy
(defmethod be purdy.document.DAlt
  [w k [[i doc] & docs]]
  (better w
          k
          (be w k (clojure.core/concat [[i (:doc-a doc)]] docs))
          (be w k (clojure.core/concat [[i (:doc-b doc)]] docs))))

(defn best
  [w k doc]
  (be w k [[0 doc]]))

(defn layout-doc
  [w doc]
  (layout (best w 0 doc)))
