(uiop:define-package #:reblocks-text-editor/blocks/progress
  (:use #:cl)
  (:import-from #:common-doc
                #:define-node))
(in-package #:reblocks-text-editor/blocks/progress)


(define-node progress (common-doc:document-node)
             ((percent :initform 0
                       :accessor percent))
  (:tag-name "progress")
  (:documentation "This node will be replaced by a real one with the same reference."))


(defmethod reblocks-text-editor/html::to-html ((node progress))
  (let ((percent (ceiling (percent node))))
    (let  ((reblocks/html:*pretty-html* nil))
      (reblocks/html:with-html
        (:span :id (common-doc:reference node)
               :class "noneditable progress"
               :style "display:inline-block; background-color: #83c6ff; width: 100px; height: 0.8em; line-height: 0.8em"
               (:span :style (format nil
                                     "display: inline-block; background-color: #2dd12d; width: ~A%; height: 100%"
                                     percent)))))))


(defmethod scriba.emitter:emit ((node progress) stream))
