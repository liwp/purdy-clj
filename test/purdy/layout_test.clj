(ns purdy.layout-test
  (:require [clojure.test :refer :all]
            [purdy.layout :refer :all]
            [purdy.document :refer :all]))

(def w 80)
(def k 0)

(deftest layout-basic-tests
  (testing ""
    (is (= (layout w k []) (->LNil)) "layout of nothing should be LNil")
    (is (= (layout w k [[10 (empty)]]) (->LNil)) "empty documents should be ignored")
    (is (= (layout w k [[10 (text "a")]]) (->LText "a" (->LNil))) "text should be laid out as LText")
    (is (= (layout w k [[10 (line)]]) (->LLine 10 (->LNil))) "line should be laid out as LLine")))

(deftest layout-nest-tests
  (testing ""
    (is (= (layout w k [[0 (concat (text "a") (nest 10 (concat (line) (text "b"))))]])
           (->LText "a"
                    (->LLine 10
                             (->LText "b"
                                      (->LNil))))))))

(deftest layout-concat-tests
  (testing ""
    (is (= (layout w k [[0 (concat (text "a") (text "b"))]])
           (->LText "a"
                    (->LText "b"
                             (->LNil)))))))

(deftest layout-alt-tests
  (testing ""
    (is (= (layout w k [[0 (alt (text "a") (text "b"))]])
           (->LText "a"
                    (->LNil))))))

(def two-elem-vector
  (alt (concat (text "[") (nest 2 (concat (text " ") (text "1") (text " ") (text "2"))) (text " ") (text "]"))
       (delay (concat (text "[") (nest 2 (concat (line) (text "1") (line) (text "2"))) (line) (text "]")))))

(deftest layout-vector-tests
  (testing "laid out on one line"
    (is (= (layout 80 0 [[0 two-elem-vector]])
           (->LText "["
                    (->LText " "
                             (->LText "1"
                                      (->LText " "
                                               (->LText "2"
                                                        (->LText " "
                                                                 (->LText "]"
                                                                          (->LNil)))))))))))

  (testing "laid out horizontally"
    (is (= (layout 2 0 [[0 two-elem-vector]])
           (->LText "["
                    (->LLine 2
                             (->LText "1"
                                      (->LLine 2
                                               (->LText "2"
                                                        (->LLine 0
                                                                 (->LText "]"
                                                                          (->LNil))))))))))))
