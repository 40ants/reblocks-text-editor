(uiop:define-package #:reblocks-text-editor-tests/document/ops
  (:use #:cl)
  (:import-from #:rove
                #:testing
                #:deftest
                #:ok)
  (:import-from #:hamcrest)
  (:import-from #:hamcrest/rove
                #:has-type
                #:contains
                #:has-slots
                #:assert-that)
  (:import-from #:reblocks-text-editor/editor
                #:make-document-from-markdown-string)
  (:import-from #:common-doc
                #:make-image
                #:make-content
                #:make-list-item
                #:make-unordered-list
                #:make-paragraph
                #:make-text
                #:children)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:reblocks-text-editor/document/ops
                #:find-node-at-position
                #:parse-scriba-nodes
                #:split-node
                #:split-nodes
                #:flatten-nodes
                #:map-document
                #:prepare-new-content
                #:find-previous-paragraph
                #:find-next-paragraph
                #:update-node-content)
  (:import-from #:reblocks-text-editor/utils/markdown
                #:to-markdown)
  (:import-from #:hamcrest/matchers
                #:has-all)
  (:import-from #:reblocks-text-editor/typed-pieces/scribdown
                #:make-scribdown-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/common-doc
                #:make-common-doc-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/common-doc-impl
                #:decrement-of-placeholders-before-caret)
  (:import-from #:reblocks-text-editor/html
                #:add-markup-to))
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

    (update-node-content doc
                         first-paragraph
                         (make-scribdown-piece "New content." 0))
    
    (ok (length= 2 (children doc)))
    (ok (equal (to-markdown
                (first (children doc)))
               "New content."))
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

    (update-node-content doc
                         first-paragraph
                         (make-common-doc-piece new-content 0))
    
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

    (update-node-content doc second-paragraph
                         (make-common-doc-piece new-content 0))

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
                          (make-common-doc-piece new-content 0))

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
                          (make-common-doc-piece new-content 0))

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

    (update-node-content doc
                         code-node
                         (make-scribdown-piece new-content 0))

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


(deftest test-prepare-new-content-with-space-after-placeholder
  (let* ((image (common-doc:make-image "source-does-not-matter"))
         (doc (make-instance 'reblocks-text-editor/document/editable::editable-document
                             :children (list image))))
    (reblocks-text-editor/document/ops::add-reference-ids doc)

    ;; Just to be sure the reference was written into the image node:    
    (ok (common-doc:reference image))
    
    (let* ((string (format nil "Ok@placeholder[ref=~A]() "
                           (common-doc:reference image)))
           (result (prepare-new-content doc
                                        string)))
      (ok (typep result 'common-doc:paragraph))
      (let ((content (children result)))
        (assert-that content
                     (contains
                      (has-slots 'common-doc:text
                                 "Ok")
                      (has-slots 'common-doc:source
                                 "source-does-not-matter")
                      (has-slots 'common-doc:text
                                 " ")))))))


(deftest test-prepare-new-content-when-there-is-no-markup
  (let* ((doc (make-instance 'reblocks-text-editor/document/editable::editable-document)))
    (reblocks-text-editor/document/ops::add-reference-ids doc)

    (let* ((string "foo bar")
           (result (prepare-new-content doc
                                        string)))
      (ok (typep result 'common-doc:paragraph))
      (let ((content (children result)))
        (assert-that content
                     (contains
                      (has-slots 'common-doc:text
                                 "foo bar")))))))


(deftest test-prepare-new-content-with-beginning-of-image-markup
  (let* ((doc (make-instance 'reblocks-text-editor/document/editable::editable-document)))
    (reblocks-text-editor/document/ops::add-reference-ids doc)

    (let* ((string "![](")
           (result (prepare-new-content doc
                                        string)))
      (ok (typep result 'common-doc:paragraph))
      (let ((content (children result)))
        (assert-that content
                     (contains
                      (has-slots 'common-doc:text
                                 "![](")))))))


(deftest test-map-document-is-able-to-replace-a-single-text-node-with-multiple
  (let* ((foo (make-text "foo"))
         (bar (make-text "bar"))
         (baz (make-text "baz"))
         (blah (make-text "blah"))
         (minor (make-text "minor"))
         (doc (make-instance 'reblocks-text-editor/document/editable::editable-document
                             :children (list foo bar baz))))
    ;; Now we'll try to replace "bar" node with two nodes "blah" and "minor"
    (flet ((replace-bar (node depth)
             (declare (ignore depth))
             (if (eql node bar)
                 (list blah minor)
                 node)))
      (map-document doc #'replace-bar))

    ;; Checking the result
    (assert-that doc
                 (has-slots 'common-doc:children
                            (contains foo blah minor baz)))))


(deftest test-flatten-nodes
  (let* ((foo (make-text "foo"))
         (bar (make-text "bar"))
         (blah (make-text "blah"))
         (minor (make-text "minor"))
         (root (make-paragraph 
                (list foo
                      (make-content (list blah minor))
                      bar)))
         (result (flatten-nodes root)))
    ;; Checking the result
    (assert-that result
                 (contains foo blah minor bar))))



(deftest test-split-text-nodes ()
  (let ((nodes (list (make-text "foo")
                     (make-text "bar")
                     (make-text "blah"))))
    (testing "When caret is at the end"
      (assert-that (split-nodes nodes 10)
                   (contains
                    (contains (has-slots 'common-doc:text "foo")
                              (has-slots 'common-doc:text "bar")
                              (has-slots 'common-doc:text "blah"))
                    ;; The second list should be empty, because
                    ;; caret is at the end:
                    (hamcrest/rove:has-length 0))))
    
    (testing "When caret is at the beginning"
      (destructuring-bind (left right)
          (split-nodes nodes 0)
        (assert-that left
                     ;; The first list should be empty, because
                     ;; caret is at the beginning:
                     (hamcrest/rove:has-length 0))
        (assert-that right
                     (contains (has-slots 'common-doc:text "foo")
                               (has-slots 'common-doc:text "bar")
                               (has-slots 'common-doc:text "blah")))))
    
    (testing "When caret is at the middle of \"blah\""
      (assert-that (split-nodes nodes 7)
                   (contains
                    (contains (has-slots 'common-doc:text "foo")
                              (has-slots 'common-doc:text "bar")
                              (has-slots 'common-doc:text "b"))
                    (contains (has-slots 'common-doc:text "lah")))))))


(deftest test-split-text-nodes-with-markup ()
  ;; foobar***blah*minor**
  (let ((nodes (list (make-text "foo")
                     (make-text "bar")
                     (add-markup-to
                      (common-doc:make-bold
                       (list
                        (add-markup-to
                         (common-doc:make-italic
                          (make-text "blah")))
                        (make-text "minor")))))))
    (testing "When caret is after the \"b\" symbol of \"blah\""
      (destructuring-bind (left right)
          (split-nodes nodes 10)
        (assert-that left
                     (contains (has-slots 'common-doc:text "foo")
                               (has-slots 'common-doc:text "bar")
                               (has-all
                                (has-type 'common-doc:bold)
                                (has-slots 'common-doc:children
                                           (contains
                                            (has-slots 'common-doc:text "**")
                                            (has-all
                                             (has-type 'common-doc:italic)
                                             (has-slots 'common-doc:children
                                                        (contains
                                                         (has-slots 'common-doc:text "*")
                                                         (has-slots 'common-doc:text "b")
                                                         (has-slots 'common-doc:text "*"))))
                                            (has-slots 'common-doc:text "**"))))))
        (assert-that right
                     (contains (has-all
                                (has-type 'common-doc:bold)
                                (has-slots 'common-doc:children
                                           (contains
                                            (has-slots 'common-doc:text "**")
                                            (has-all
                                             (has-type 'common-doc:italic)
                                             (has-slots 'common-doc:children
                                                        (contains
                                                         (has-slots 'common-doc:text "*")
                                                         (has-slots 'common-doc:text "lah")
                                                         (has-slots 'common-doc:text "*"))))
                                            (has-all
                                             (has-type 'common-doc:text-node)
                                             (has-slots 'common-doc:text "minor"))
                                            (has-slots 'common-doc:text "**"))))))))))


(deftest test-split-empty-text-node-should-not-duplicate-it ()
  (let ((node (make-text "" :reference "initial")))
    (destructuring-bind (left right)
        (split-node node 0)
      (ok (not (eq left
                   right)))
      (assert-that left
                   (has-slots 'common-doc:text ""
                              'common-doc:reference nil))
      (assert-that right
                   (has-slots 'common-doc:text ""
                              'common-doc:reference nil)))))


(deftest test-split-empty-text-node-should-not-duplicate-it-2 ()
  ;; This test like the previous one, but checks SPLIT-NODES instead of a single SPLIT-NODE
  (let ((nodes (list (make-text "" :reference "initial"))))
    (destructuring-bind (left right)
        (split-nodes nodes 0)
      (ok (not (eq (first left)
                   (first right))))
      (assert-that left
                   (contains (has-slots 'common-doc:text ""
                                        'common-doc:reference nil)))
      (assert-that right
                   (contains (has-slots 'common-doc:text ""
                                        'common-doc:reference nil))))))


(deftest test-decrement-by-placeholders
  (let ((text "Image попробуем в середине строки: @placeholder[ref=some]() another text and @placeholder[ref=ba]() another placeholder."))
    (let ((result (decrement-of-placeholders-before-caret text 60)))
      (ok (= result 23)))
    
    (let ((result (decrement-of-placeholders-before-caret text 120)))
      (ok (= result 44)))))



(deftest test-scriba-nodes-parsing-should-ignore-incomplete-input
  (let* ((node (make-text "Image: @placeholder[ref=el330]() another ![]("))
         (result (parse-scriba-nodes node)))
    (assert-that result
                 (contains
                  (has-slots 'common-doc:text "Image: ")
                  (has-type 'reblocks-text-editor/blocks/placeholder::placeholder)
                  (has-slots 'common-doc:text " another ![](")))))


(deftest test-find-node-at-position-1
  (testing "When paragraph contains an image and then text node"
    (let* ((image (make-image "some.jpg"))
           (text (make-text ""))
           (node (make-paragraph
                  (list image
                        text))))
      (multiple-value-bind (current-node new-pos)
          (find-node-at-position node 0)
        ;; Cursor is before the image thus this image node
        ;; should be returned
        (ok (eql current-node image))
        (ok (= new-pos 0))))))
