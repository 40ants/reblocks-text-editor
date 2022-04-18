(uiop:define-package #:reblocks-text-editor-tests/typed-pieces/scribdown
  (:use #:cl)
  (:import-from #:rove
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
                #:assert-that))
(in-package #:reblocks-text-editor-tests/typed-pieces/scribdown)


(deftest tests-html-to-scribdown-1
  (let* ((from (make-html-piece
                "<p>Foo bar</p>"
                3))
         (result (convert from :scribdown nil nil)))
    (assert-that result
                 (has-all
                  (has-slots 'document
                             "Foo bar")
                  (has-slots 'caret
                             3)))))


(deftest tests-html-to-scribdown-2
  (let ((html-string
          "<p>Foo bar: <img src=\"blah\" id=\"some-ref\" class=\"noneditable\"/>
another <img src=\"minor\" id=\"other-ref\" class=\"noneditable\"/> image.</p>"))
    (testing "When caret before placeholders it should not be changed"
      (let* ((doc (make-html-piece html-string 3))
             (result (convert doc :scribdown nil nil)))
        (assert-that result
                     (has-all
                      (has-slots 'document
                                 "Foo bar: @placeholder[ref=some-ref]()another @placeholder[ref=other-ref]() image.")
                      (has-slots 'caret
                                 3)))))
    ;; For HTML string caret position counters
    ;; each noneditable node as one character
    (testing "When caret after the first placeholder"
      (let* ((original-position 10) ;; right after the first image
             (doc (make-html-piece html-string original-position))
             (result (convert doc :scribdown nil nil)))
        (assert-that result
                     (has-all
                      (has-slots 'document
                                 "Foo bar: @placeholder[ref=some-ref]()another @placeholder[ref=other-ref]() image.")
                      (has-slots 'caret
                                 (+ original-position 27))))))
    
    (testing "When caret after the second placeholder"
      (let* ((original-position 19) ;; right after the second image
             (doc (make-html-piece html-string original-position))
             (result (convert doc :scribdown nil nil)))
        (assert-that result
                     (has-all
                      (has-slots 'document
                                 "Foo bar: @placeholder[ref=some-ref]()another @placeholder[ref=other-ref]() image.")
                      (has-slots 'caret
                                 (+ original-position
                                    ;; first placeholder length - 1
                                    27
                                    ;; second placeholder length - 1
                                    28))))))))
