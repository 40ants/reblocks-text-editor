(uiop:define-package #:reblocks-text-editor-tests/document/ops
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok)
  (:import-from #:reblocks-text-editor/editor
                #:make-document-from-markdown-string)
  (:import-from #:common-doc
                #:make-paragraph
                #:make-text
                #:children)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:reblocks-text-editor/document/ops
                #:update-node-content)
  (:import-from #:reblocks-text-editor/utils/markdown
                #:to-markdown))
(in-package #:reblocks-text-editor-tests/document/ops)


(deftest test-document-creation
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

Second paragraph.")))
    (ok (length= 2 (children doc)))
    (ok (equal (to-markdown
                (first (children doc)))
               "First paragraph."))
    (ok (equal (to-markdown
                (second (children doc)))
               "Second paragraph."))))


(deftest test-replacing-paragraph-content-with-plain-text
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

Second paragraph."))
         (first-paragraph (first (children doc))))

    (update-node-content  doc first-paragraph
                          "New content."
                          0)
    
    (ok (length= 2 (children doc)))
    (ok (equal (to-markdown
                (first (children doc)))
               "New content."))
    (ok (equal (to-markdown
                (second (children doc)))
               "Second paragraph."))))


(deftest test-replacing-paragraph-content-with-a-list-of-inlines
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

Second paragraph."))
         (first-paragraph (first (children doc)))
         (new-content (list (make-text "Foo ")
                            (common-doc:make-bold (make-text "Bar"))
                            (make-text " Baz"))))

    (update-node-content  doc first-paragraph
                          new-content
                          0)
    
    (ok (length= 2 (children doc)))
    (ok (equal (to-markdown
                (first (children doc)))
               "Foo **Bar** Baz"))
    (ok (equal (to-markdown
                (second (children doc)))
               "Second paragraph."))))


(deftest test-replacing-paragraph-content-with-another-paragraph
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

Second paragraph."))
         (first-paragraph (first (children doc)))
         (new-content (make-paragraph
                       (list (make-text "Foo ")
                             (common-doc:make-bold (make-text "Bar"))
                             (make-text " Baz")))))

    (update-node-content  doc first-paragraph
                          new-content
                          0)
    
    (ok (length= 2 (children doc)))
    (ok (equal (to-markdown
                (first (children doc)))
               "Foo **Bar** Baz"))
    (ok (equal (to-markdown
                (second (children doc)))
               "Second paragraph."))))


(deftest test-replacing-paragraph-content-with-another-paragraph
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

Second paragraph."))
         (first-paragraph (first (children doc)))
         (new-content (make-paragraph
                       (list (make-text "Foo ")
                             (common-doc:make-bold (make-text "Bar"))
                             (make-text " Baz")))))

    (update-node-content  doc first-paragraph
                          new-content
                          0)
    
    (ok (length= 2 (children doc)))
    (ok (equal (to-markdown
                (first (children doc)))
               "Foo **Bar** Baz"))
    (ok (equal (to-markdown
                (second (children doc)))
               "Second paragraph."))))
