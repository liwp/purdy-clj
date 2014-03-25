(ns purdy.test.benchmark
  (:require [purdy.core :refer :all]
            [perforate.core :refer :all]
            [clojure.pprint :refer [pprint]]))

(def test-obj
  (group
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

(defgoal simple-bench "A simple benchmark.")

(defcase simple-bench :really-simple
  []
  (pretty 80 test-obj))

(defgoal string-print "Pretty-print a string"
  :setup (fn []
           (let [n 10
                 s (apply str (take n (repeat "hello ")))]
             [s])))

(defcase string-print :purdy
  [s]
  (pretty 80 (fillwords s)))

(defcase string-print :pprint
  [s]
  (with-out-str
    (pprint s)))
