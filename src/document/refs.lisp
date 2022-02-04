(uiop:define-package #:reblocks-text-editor/document/refs
  (:use #:cl)
  (:import-from #:common-doc.ops))
(in-package #:reblocks-text-editor/document/refs)


(defun add-reference-ids (document &key (next-id 1))
  (flet ((set-reference-id (node depth)
           (declare (ignore depth))
           (setf (common-doc:reference node)
                 (format nil "el~A" next-id))
           (incf next-id)
           (values)))
    (common-doc.ops:traverse-document document
                                      #'set-reference-id)
    (values document
            next-id)))


