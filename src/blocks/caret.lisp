(uiop:define-package #:reblocks-text-editor/blocks/caret
  (:use #:cl)
  (:import-from #:common-doc
                #:define-node)
  (:import-from #:serapeum
                #:length<=
                #:length>=)
  (:import-from #:reblocks-text-editor/utils/text
                #:+zero-width-space+))
(in-package #:reblocks-text-editor/blocks/caret)


(define-node caret (common-doc:document-node)
             ((child :initarg :child
                     :accessor child
                     :documentation "A child inside of which caret was placed. Usually, it will be TEXT-NODE.")
              (caret-position :initarg :position
                              :accessor caret-position)
              (path :initarg :path
                    :reader caret-path))
  (:tag-name "caret")
  (:documentation "This node represents a current caret position. Only one caret should exist in a document."))


(defun make-caret (child caret-position path)
  (make-instance 'caret
                 :child child
                 :position caret-position
                 :path path))


(defmethod scriba.emitter:emit ((node caret) stream)
  (scriba.emitter:emit (child node) stream))


(defmethod reblocks-text-editor/html::to-html ((caret caret))
  (let ((reblocks/html:*pretty-html* nil))
    (reblocks/html:with-html
      ;; TODO: add a check that there is no nodes prohibited as a span container  
      (let ((child (child caret))
            (position (caret-position caret)))
        (:span :id (common-doc:reference caret)
               :class "caret-wrapper"
               (typecase child
                 (common-doc:text-node
                  (:span :id (common-doc:reference child)
                         :class (reblocks-text-editor/html::html-class child)
                         (let* ((text (common-doc:text child))
                                (left (subseq text 0 position))
                                (rest (subseq text position))
                                (caret (if (length<= 1 rest)
                                           (subseq rest 0 1)
                                           " "))
                                (right (when (length<= 2 rest)
                                         (subseq rest 1))))
                           (when left
                             (:span :id (format nil "~A-caret-left"
                                                (common-doc:reference child))
                                    (plump:encode-entities left)))
                           (:span :id (format nil "~A-caret"
                                              (common-doc:reference child))
                                  :class "caret"
                                  caret)
                           (when right
                             (:span :id (format nil "~A-caret-right"
                                                (common-doc:reference child))
                                    (plump:encode-entities right))))))
                 (t
                  (reblocks-text-editor/html::to-html (child caret)))))))))
