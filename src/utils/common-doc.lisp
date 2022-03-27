(uiop:define-package #:reblocks-text-editor/utils/common-doc
  (:use #:cl)
  (:export
   #:print-tree))
(in-package #:reblocks-text-editor/utils/common-doc)


(defun print-tree (doc)
  "Prints CommonDoc nodes as a nested tree, to visualize document structure."
  (common-doc.ops:with-document-traversal (doc node depth)
    (loop repeat (1- depth)
          do (write-char #\Space)
             (write-char #\Space))
    (format t "~A~%" node))
  (values doc))
