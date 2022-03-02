(uiop:define-package #:reblocks-text-editor/html/markdown-link
  (:use #:cl)
  (:import-from #:commondoc-markdown)
  (:import-from #:common-doc)
  (:import-from #:reblocks-text-editor/html
                #:*render-markup*
                #:html-class)
  (:import-from #:reblocks-text-editor/html/markup
                #:make-markup2)
  (:import-from #:reblocks-text-editor/html/web-link
                #:make-visible-weblink))
(in-package #:reblocks-text-editor/html/markdown-link)


(defmethod to-html ((node commondoc-markdown:markdown-link))
  (error "Not implemented")
  (reblocks/html:with-html
    (:span :id (common-doc:reference node)
           :class (html-class node)
           (mapc #'to-html (common-doc:children node)))))


(defmethod common-doc:children :around ((node commondoc-markdown:markdown-link))
  ;; (error "Not implemented")
  (call-next-method)
  ;; (if *render-markup*
  ;;     (let* ((definition (commondoc-markdown:markdown-link-definition node))
  ;;            ;; TODO: here we need somehow to create a real link:
  ;;            (uri (quri:make-uri :scheme "internal"
  ;;                                :path definition)))
  ;;       (list (make-markup2 node "[" "lb-title")
  ;;             (make-visible-weblink node
  ;;                                   (call-next-method)
  ;;                                   uri)
  ;;             (make-markup2 node "]" "rb-title")
  ;;             (make-markup2 node "[" "lb-def")
  ;;             (make-markup2 node definition "definition")
  ;;             (make-markup2 node "]" "rb-def")))
  ;;     (call-next-method))
  )


;; Markdown link should be rendered to Scriba Document, as usual REF
(defmethod scriba.emitter:emit ((node commondoc-markdown:markdown-link) stream)
  (error "Not implemented")
  (let ((definition (commondoc-markdown:markdown-link-definition node)))
    (scriba.emitter::with-tag-name ("ref" stream)
      (scriba.emitter::with-tag-attrs ((common-doc:metadata node)
                                       stream
                                       :extra (list (cons "id" definition)))
        (scriba.emitter::with-tag-body (stream)
          (scriba.emitter::emit-children node stream))))))
