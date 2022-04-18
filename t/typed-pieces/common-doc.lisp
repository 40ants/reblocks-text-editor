(uiop:define-package #:reblocks-text-editor-tests/typed-pieces/common-doc
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:testing
                #:deftest)
  (:import-from #:reblocks-text-editor/typed-pieces/html
                #:make-html-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/base
                #:document
                #:caret
                #:convert)
  (:import-from #:hamcrest/rove
                #:has-all
                #:has-slots
                #:assert-that)
  (:import-from #:reblocks-text-editor/typed-pieces/scribdown
                #:make-scribdown-piece)
  (:import-from #:common-doc
                #:make-text
                #:make-italic
                #:make-bold
                #:make-paragraph)
  (:import-from #:reblocks-text-editor/typed-pieces/common-doc
                #:make-common-doc-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/common-doc-impl
                #:calculate-caret-position-for-html
                #:markup-caret-position-decrement))
(in-package #:reblocks-text-editor-tests/typed-pieces/common-doc)


(deftest caret-is-moved-depending-on-preceeding-markdown-markup
  (let* ((from (make-scribdown-piece "**Hello** World!"
                                     ;; Caret is after the "W"
                                     11))
         (result (convert from :common-doc nil nil)))
    (assert-that result
                 (has-slots 'caret
                            (- 11
                               ;; Four * are missing in CommonDoc tree
                               4)))))


(deftest test-decrement-calculation
  (let ((doc (make-paragraph
              ;; This text should be rendered like this:
              ;; **Hello** *Lisp* **World**
              (list (make-bold
                     (make-text "Hello"))
                    (make-text " ")
                    (make-italic
                     (make-text "Lisp"))
                    (make-text " ")
                    (make-bold
                     (make-text "World"))))))
    (testing "At the beginning of line"
      (ok (= (markup-caret-position-decrement doc 0)
             0)))
    (testing "After the first star"
      (ok (= (markup-caret-position-decrement doc 1)
             1)))
    (testing "After the second star"
      (ok (= (markup-caret-position-decrement doc 2)
             2)))
    (testing "After the \"**He\""
      (ok (= (markup-caret-position-decrement doc 4)
             2)))
    (testing "After the \"**Hello** *Li\""
      (ok (= (markup-caret-position-decrement doc 13)
             5)))))


(deftest test-calculate-caret-position-for-html
  (let ((doc (make-paragraph
              ;; This text should be rendered like this:
              ;; **Hello** *Lisp* **World**
              (list (make-bold
                     (make-text "Hello"))
                    (make-text " ")
                    (make-italic
                     (make-text "Lisp"))
                    (make-text " ")
                    (make-bold
                     (make-text "World"))))))
    (testing "Before the \"Hello\""
      (ok (= (calculate-caret-position-for-html
              (make-common-doc-piece doc 0))
             2)))
    (testing "Before the \"ello\""
      (ok (= (calculate-caret-position-for-html
              (make-common-doc-piece doc 1))
             3)))
    (testing "After the \"Hello\""
      ;; caret should not skip closing markup
      (ok (= (calculate-caret-position-for-html
              (make-common-doc-piece doc 5))
             7)))
    (testing "Before the \"Lisp\""
      (ok (= (calculate-caret-position-for-html
              (make-common-doc-piece doc 6))
             10)))))
