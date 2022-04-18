(uiop:define-package #:reblocks-text-editor/typed-pieces/base
  (:use #:cl))
(in-package #:reblocks-text-editor/typed-pieces/base)


(defclass typed-piece ()
  ((document :initarg :document
             :reader document)
   (caret :initarg :caret
          :reader caret)))


(defmethod print-object ((obj typed-piece) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S :caret-at ~A"
            (document obj)
            (caret obj))))


(defgeneric convert (from
                     to
                     editable-document
                     changed-node))
