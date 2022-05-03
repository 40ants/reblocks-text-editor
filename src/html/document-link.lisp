(uiop:define-package #:reblocks-text-editor/html/document-link
  (:use #:cl)
  (:import-from #:reblocks/html)
  (:import-from #:scriba)
  (:import-from #:reblocks-text-editor/html/markup
                #:make-markup2)
  (:import-from #:reblocks-text-editor/html
                #:to-html)
  (:import-from #:common-doc)
  (:import-from #:reblocks-text-editor/html/web-link
                #:make-visible-weblink))
(in-package #:reblocks-text-editor/html/document-link)


(defun make-internal-uri (node)
  (format nil "internal://~@[~A~]~@[#~A~]"
          (common-doc:document-reference node)
          (common-doc:node-reference node)))


(defmethod reblocks-text-editor/html::to-html ((node common-doc:document-link))
  (reblocks/html:with-html
    (:span :id (common-doc:reference node)
           :class (reblocks-text-editor/html::html-class node)
           (mapc #'to-html (common-doc:children node)))))


;; (defmethod common-doc:children :around ((node common-doc:document-link))
;;   (if reblocks-text-editor/html::*render-markup*
;;       (let ((uri-as-a-string (make-internal-uri node)))
;;         (list (make-markup2 node "[" "lb-title")
;;               (make-visible-weblink node
;;                                     (call-next-method)
;;                                     (quri:uri uri-as-a-string))
;;               (make-markup2 node "]" "rb-title")
;;               (make-markup2 node "[" "lb-def")
;;               (make-markup2 node uri-as-a-string "definition")
;;               (make-markup2 node "]" "rb-def")))
;;       (call-next-method)))


;; (defmethod to-html ((node common-doc:document-link))
  
;;   (common-doc.format:emit-document (make-instance 'scriba:scriba)
;;                                    node
;;                                    reblocks/html:*stream*)
  
;;   ;; @ref[doc=7BFEDDB9F013428898F840642721FC49 id=NIL](Seems link work)
  
;;   ;; (reblocks/html:with-html
;;   ;;   (let ((uri (format nil "hypernot://~@[~A~]~@[#~A~]"
;;   ;;                      (common-doc:document-reference node)
;;   ;;                      (common-doc:node-reference node))))
;;   ;;     (:a :id (common-doc:reference node)
;;   ;;         :href uri
;;   ;;         :class (html-class node)
;;   ;;         (mapc #'to-html (common-doc:children node)))))
;;   )
