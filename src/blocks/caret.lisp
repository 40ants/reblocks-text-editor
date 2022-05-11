(uiop:define-package #:reblocks-text-editor/blocks/caret
  (:use #:cl)
  (:import-from #:common-doc
                #:define-node))
(in-package #:reblocks-text-editor/blocks/caret)


(define-node caret (common-doc:document-node)
             ((child :initarg :child
                     :accessor child
                     :documentation "A child inside of which caret was placed. Usually, it will be TEXT-NODE.")
              (caret-position :initarg :position
                              :accessor caret-position))
  (:tag-name "caret")
  (:documentation "This node represents a current caret position. Only one caret should exist in a document."))


(defun make-caret (child caret-position)
  (make-instance 'caret
                 :child child
                 :position caret-position))


(defmethod scriba.emitter:emit ((node caret) stream)
  (scriba.emitter:emit (child node) stream))


(defmethod reblocks-text-editor/html::to-html ((node caret))
  (reblocks/html:with-html
    (:span :id (common-doc:reference node)
           :class (format nil "caret ~A"
                          (reblocks-text-editor/html::html-class node))
           (reblocks-text-editor/html::to-html (child node)))))
