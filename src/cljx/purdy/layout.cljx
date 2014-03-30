(ns purdy.layout
  (:require [purdy.document :as doc]))

(defprotocol ILayout
  (to-string [this] "Render this layout item as a string.")
  (fits [this w] "Does this item fit in the available space?"))

(defrecord LNil []
  ILayout
  (to-string [this]
    "")

  (fits [this w]
    (>= w 0)))

(defrecord LText [text doc]
  ILayout
  (to-string [this]
    (str text (to-string doc)))
  
  (fits [this w]
    (fits doc (- w (count text)))))

(defn spaces
  [n]
  (apply str \newline (repeat n \space)))

(defrecord LLine [indent doc]
  ILayout
  (to-string [this]
    (str (spaces indent) (to-string doc)))

  (fits [this w]
    (>= w 0)))

(defn better
  [w k x y]
  (if (fits x (- w k))
    x
    (force y)))

(defmulti layout (fn [w k [[i doc] & docs]] (type doc)))

(defmethod layout nil
  [w k docs]
  (->LNil))

(defmethod layout purdy.document.DEmpty
  [w k [[i doc] & docs]]
  (layout w k docs))

(defmethod layout purdy.document.DConcat
  [w k [[i doc] & docs]]
  (layout w k (concat [[i (:head doc)]] [[i (:tail doc)]] docs)))

(defmethod layout purdy.document.DNest
  [w k [[i doc] & docs]]
  (layout w k (concat [[(+ i (:indent doc)) (:doc doc)]] docs)))

(defmethod layout purdy.document.DText
  [w k [[i doc] & docs]]
  (let [s (:text doc)]
    (->LText s (layout w (+ k (count s)) docs))))

(defmethod layout purdy.document.DLine
  [w k [[i doc] & docs]]
  (->LLine i (layout w i docs)))

(defmethod layout purdy.document.DAlt
  [w k [[i doc] & docs]]
  (better w
          k
          (layout w k (concat [[i (:a doc)]] docs))
          (delay (layout w k (concat [[i (force (:b doc))]] docs)))))

(defn best
  [w k doc]
  (layout w k [[0 doc]]))

(defn layout-doc
  [w doc]
  (to-string (best w 0 doc)))
