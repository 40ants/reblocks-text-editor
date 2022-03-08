(uiop:define-package #:reblocks-text-editor/document/editable
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock)
  (:import-from #:metacopy
                #:copy-thing))
(in-package #:reblocks-text-editor/document/editable)


(defclass editable-document (common-doc:content-node)
  ((lock :initform (make-lock "Editable Document Lock")
         :reader document-lock)
   (revision :initform 0
             :accessor content-version)
   (next-id :initform 1
            :accessor next-id)
   (undo-history :type list
                 :initform nil
                 :documentation "Simplest form of undo, using a document stack."
                 :accessor undo-history)
   (caret-position :initform nil
                   :documentation "Stores a tuple of two elements.
                                    The first is a node where caret is located and
                                    the second is a number of the caret offset."
                   :accessor caret-position)))


(defun get-next-reference-id (document)
  (check-type document editable-document)
  
  (prog1 (format nil "el~A"
                 (next-id document))
    (incf (next-id document))))


(defun history-push (document)
  (push (copy-thing document)
        (undo-history document)))


(defun history-pop (document)
  (let ((old-document (car (undo-history document))))
    (setf (undo-history document)
          (cdr
           (undo-history document)))
    (values old-document)))
