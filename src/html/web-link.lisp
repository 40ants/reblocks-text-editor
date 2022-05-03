(uiop:define-package #:reblocks-text-editor/html/web-link
  (:use #:cl)
  (:import-from #:reblocks-text-editor/html
                #:to-html
                #:html-class
                #:*render-markup*)
  (:import-from #:reblocks-text-editor/html/markup
                #:make-markup2)
  (:import-from #:common-doc))
(in-package #:reblocks-text-editor/html/web-link)


(defclass visible-weblink (common-doc:web-link)
  ())


(defun make-visible-weblink (for-node children uri)
  (make-instance 'visible-weblink
                 :children children
                 :uri uri
                 :reference (format nil "~A-link"
                                    (common-doc:reference for-node))))

(defmethod to-html ((node common-doc:web-link))
  (reblocks/html:with-html
    (:span :id (common-doc:reference node)
           :class (html-class node)
           (mapc #'to-html (common-doc:children node)))))


(defmethod to-html ((node visible-weblink))
  (reblocks/html:with-html
    (:a :id (common-doc:reference node)
        :class (html-class node)
        :href (quri:render-uri (common-doc:uri node))
        (mapc #'to-html (common-doc:children node)))))


;; (defmethod common-doc:children :around ((node common-doc:web-link))
;;   (if (and *render-markup*
;;            (not (typep node 'visible-weblink)))
;;       (let ((uri (common-doc:uri node)))
;;         (list (make-markup2 node "[" "left-bracket")
;;               (make-visible-weblink node
;;                                     (call-next-method)
;;                                     uri)
;;               (make-markup2 node "]" "right-braket")
;;               (make-markup2 node "(" "left-paren")
;;               (make-markup2 node (quri:render-uri uri) "uri")
;;               (make-markup2 node ")" "right-paren")))
;;       (call-next-method)))



(defmethod reblocks-text-editor/html::add-markup-to ((node visible-weblink))
  ;; To prevent infinite recursion, because web-link creates
  ;; visible-link in it's add-markup-to method, we need to define
  ;; this empty method for visible-weblink
  node)


(defmethod scriba.emitter:emit :before ((node common-doc:web-link) stream)
  ;; Here we need to ensure there is uri
  ;; in the metadata, because scriba's emitter renders it
  ;; instead of uri slot:
  (setf (common-doc:get-meta node "uri")
        (quri:render-uri
         (common-doc:uri node))))


(defmethod scriba.emitter:emit ((node visible-weblink) stream)
  ;; This empty method prevents rendering of the visible weblink into the
  ;; document on disk. Because visible-weblink should only be visible
  ;; when rendered to HTML.
  (scriba.emitter::emit-children node stream))
