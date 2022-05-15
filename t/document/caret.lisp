(uiop:define-package #:reblocks-text-editor-tests/document/caret
  (:use #:cl)
  (:import-from #:rove
                #:deftest)
  (:import-from #:hamcrest/rove
                #:has-type
                #:contains
                #:has-slots
                #:assert-that)
  (:import-from #:reblocks-text-editor/html
                #:ensure-markup-nodes)
  (:import-from #:common-doc
                #:make-text
                #:make-bold)
  (:import-from #:reblocks-text-editor/document/ops
                #:move-caret)
  (:import-from #:reblocks-text-editor/document/editable
                #:make-editable-document))
(in-package #:reblocks-text-editor-tests/document/caret)


(deftest test-moving-caret-to-end-of-the-bold-content
  (let* ((bold (make-bold (make-text "Lisp"
                                     :reference "el2")
                          :reference "el1"))
         (doc (make-editable-document bold)))
    (ensure-markup-nodes doc)
    (move-caret doc "el2" 4)
    ;; Now, bold element should have three children
    ;; and third one should be a caret wrapper
    (let ((children (common-doc:children bold)))
      (assert-that
       children
       (contains
        (hamcrest/rove:has-all
         (has-type 'common-doc:text-node)
         (has-slots 'common-doc:text "**"
                    'common-doc:reference "el1-left"))
        (hamcrest/rove:has-all
         (has-type 'common-doc:text-node)
         (has-slots 'common-doc:text "Lisp"
                    'common-doc:reference "el2"))
        (has-type 'reblocks-text-editor/blocks/caret::caret))))))
