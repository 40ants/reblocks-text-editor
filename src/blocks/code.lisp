(uiop:define-package #:reblocks-text-editor/blocks/code
  (:use #:cl))
(in-package #:reblocks-text-editor/blocks/code)


(defun code (node)
  (check-type node common-doc:code-block)
  (let ((children (common-doc:children node)))
    (if children
        (common-doc:text (first children))
        "")))
