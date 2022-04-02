(uiop:define-package #:reblocks-text-editor/html/image
  (:use #:cl)
  (:import-from #:reblocks-text-editor/html
                #:to-html
                #:html-class))
(in-package #:reblocks-text-editor/html/image)


(defmethod to-html ((node common-doc:image))
  (reblocks/html:with-html
    (let ((classes (remove nil
                           (list "noneditable"
                                 (html-class node)))))
      (:img :id (common-doc:reference node)
            :class (format nil "~{~A~^ ~}"
                           classes)
            :src (when (slot-boundp node 'common-doc:source)
                   (common-doc:source node))
            :title (when (slot-boundp node 'common-doc:description)
                     (common-doc:description node))))))
