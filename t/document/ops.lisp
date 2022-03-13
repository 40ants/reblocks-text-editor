(uiop:define-package #:reblocks-text-editor-tests/document/ops
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok)
  (:import-from #:reblocks-text-editor/editor
                #:make-document-from-markdown-string)
  (:import-from #:common-doc
                #:children)
  (:import-from #:alexandria
                #:length=))
(in-package #:reblocks-text-editor-tests/document/ops)


(defun paragraph-text (node)
  (check-type node common-doc:paragraph)
  (with-output-to-string (s)
    (loop for child in (children node)
          do (check-type child common-doc:text-node)
             (write-string (common-doc:text child) s))))


(deftest test-document-creation
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

Second paragraph.")))
    (ok (length= 2 (children doc)))
    (ok (equal (paragraph-text
                (first (children doc)))
               "First paragraph."))
    (ok (equal (paragraph-text
                (second (children doc)))
               "Second paragraph."))))
