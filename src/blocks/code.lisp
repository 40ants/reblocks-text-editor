(uiop:define-package #:reblocks-text-editor/blocks/code
  (:use #:cl)
  (:import-from #:reblocks-text-editor/document/ops
                #:map-document))
(in-package #:reblocks-text-editor/blocks/code)


(defun code (node)
  (check-type node common-doc:code-block)
  (let ((children (common-doc:children node)))
    (if children
        (common-doc:text (first children))
        "")))


(defun normalize (root-node)
  "Normalizes code-block nodes by joining it's children into a single text-node.

   This helper is required until this issue will be resolved:

   https://github.com/CommonDoc/scriba/issues/15"
  (labels ((normalize-code-blocks (current-node depth)
             (declare (ignore depth))
             (when (typep current-node 'common-doc:code-block)
               (let ((children (common-doc:children current-node)))
                 (when (not (and (= (length children) 1)
                                 (typep (first children)
                                        'common-doc:text-node)))
                   (setf (common-doc:children current-node)
                         (list (squash current-node))))))
             current-node)
           (squash (code-block)
             (common-doc:make-text
              (with-output-to-string (s)
                (common-doc.ops:with-document-traversal (code-block node)
                  (etypecase node
                    (common-doc:text-node
                     (write-string (common-doc:text node) s))
                    (common-doc:content-node
                     nil)))))))
    (map-document root-node #'normalize-code-blocks)))
