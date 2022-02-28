(uiop:define-package #:reblocks-text-editor/html/document-link
  (:use #:cl)
  (:import-from #:reblocks/html)
  (:import-from #:scriba)
  (:import-from #:reblocks-text-editor/html/markup
                #:make-markup2)
  (:import-from #:common-doc))
(in-package #:reblocks-text-editor/html/document-link)


(defmethod to-html ((node common-doc:document-link))
  (common-doc.format:emit-document (make-instance 'scriba:scriba)
                                   node
                                   reblocks/html:*stream*)
  ;; @ref[doc=7BFEDDB9F013428898F840642721FC49 id=NIL](Seems link work)
  
  ;; (reblocks/html:with-html
  ;;   (let ((uri (format nil "hypernot://~@[~A~]~@[#~A~]"
  ;;                      (common-doc:document-reference node)
  ;;                      (common-doc:node-reference node))))
  ;;     (:a :id (common-doc:reference node)
  ;;         :href uri
  ;;         :class (html-class node)
  ;;         (mapc #'to-html (common-doc:children node)))))
  )
