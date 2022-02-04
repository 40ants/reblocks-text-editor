(uiop:define-package #:reblocks-text-editor/document/editable
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock))
(in-package #:reblocks-text-editor/document/editable)


(defclass editable-document (common-doc:content-node)
  ((lock :initform (make-lock "Editable Document Lock")
         :reader document-lock)
   (revision :initform 0
             :accessor content-version)
   (next-id :initform 1
            :accessor next-id)))


(defun get-next-reference-id (document)
  (check-type document editable-document)
  
  (prog1 (format nil "el~A"
                 (next-id document))
    (incf (next-id document))))
