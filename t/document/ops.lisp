(uiop:define-package #:reblocks-text-editor-tests/document/ops
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok)
  (:import-from #:reblocks-text-editor/editor
                #:make-document-from-markdown-string)
  (:import-from #:common-doc
                #:make-list-item
                #:make-unordered-list
                #:make-paragraph
                #:make-text
                #:children)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:reblocks-text-editor/document/ops
                #:find-previous-paragraph
                #:find-next-paragraph
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


(deftest test-replacing-paragraph-content-a-list-and-attaching-it-to-a-list-before
  (let* ((doc (make-document-from-markdown-string "
* First paragraph.

Second paragraph."))
         (second-paragraph (second (children doc)))
         (new-content (make-unordered-list
                       (make-list-item (list (make-paragraph
                                              (list (make-text "Foo")))
                                             (make-paragraph
                                              (list (make-text "Bar")))))))
         ;; TODO: Here Bar is rendered not as the part of the
         ;; second list item. But this is the bug of commondoc-markdown
         ;; and should be fixed there:
         ;; https://github.com/40ants/commondoc-markdown/issues/4
         (expected                "* First paragraph.

* Foo

Bar"))

    (update-node-content  doc second-paragraph
                          new-content
                          0)

    ;; After this action, we should have only one
    ;; child in the document
    (ok (length= 1 (children doc)))
    ;; and it should be a list of two items
    ;; where second item has two paragraphs
    (ok (equal (to-markdown
                doc)
               expected))))


(deftest test-replacing-paragraph-content-a-list-and-attaching-it-to-a-list-after
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

* Second paragraph."))
         (first-paragraph (first (children doc)))
         (new-content (make-unordered-list
                       (make-list-item (list (make-paragraph
                                              (list (make-text "Foo")))))))
         (expected "* Foo

* Second paragraph."))

    (update-node-content  doc first-paragraph
                          new-content
                          0)

    ;; After this action, we should have only one
    ;; child in the document
    (ok (length= 1 (children doc)))
    ;; and it should be a list of two items
    ;; where second item has two paragraphs
    (ok (equal (to-markdown
                doc)
               expected))))


(deftest test-replacing-paragraph-content-a-list-when-it-is-surrounded-by-other-paragraphs
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

Second paragraph.

Third paragraph."))
         (second-paragraph (second (children doc)))
         (new-content (make-unordered-list
                       (make-list-item (list (make-paragraph
                                              (list (make-text "Foo")))))))
         (expected "First paragraph.

* Foo

Third paragraph."))

    (update-node-content  doc
                          second-paragraph
                          new-content
                          0)

    (ok (length= 3 (children doc)))
    ;; and it should be a list of two items
    ;; where second item has two paragraphs
    (ok (equal (to-markdown
                doc)
               expected))))


(deftest test-replacing-code-block-content
  (let* ((doc (make-document-from-markdown-string "
First paragraph.

```
Some code
```

Third paragraph."))
         (code-node (second (children doc)))
         (new-content "New code")
         ;; TODO: Seems here another error
         ;; in markdown serializer and
         ;; there should be an empty line
         ;; before the "Third paragraph"?
         (expected "First paragraph.

```
New code
```
Third paragraph."))

    (update-node-content  doc
                          code-node
                          new-content
                          0)

    (ok (length= 3 (children doc)))
    ;; and it should be a list of two items
    ;; where second item has two paragraphs
    (ok (equal (to-markdown
                doc)
               expected))))


(deftest test-find-prev-paragraph-1
  (let* ((doc (make-document-from-markdown-string "
First.

Second.

Third."))
         (first (first (children doc)))
         (second (second (children doc)))
         (third (third (children doc))))
    (ok (equal (to-markdown (find-previous-paragraph doc third))
               (to-markdown second)))
    (ok (equal (to-markdown (find-previous-paragraph doc second))
               (to-markdown first)))
    (ok (null (find-previous-paragraph doc first)))))


(deftest test-find-next-paragraph-1
  (let* ((doc (make-document-from-markdown-string "
First.

Second.

Third."))
         (first (first (children doc)))
         (second (second (children doc)))
         (third (third (children doc))))
    (ok (equal (to-markdown (find-next-paragraph doc first))
               (to-markdown second)))
    (ok (equal (to-markdown (find-next-paragraph doc second))
               (to-markdown third)))
    (ok (null (find-next-paragraph doc third)))))
