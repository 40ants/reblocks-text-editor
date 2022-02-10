(uiop:define-package #:reblocks-text-editor/html
  (:use #:cl)
  (:import-from #:reblocks)
  (:import-from #:common-doc)
  (:import-from #:commondoc-markdown)
  (:import-from #:anaphora
                #:it
                #:alet
                #:awhen)
  (:import-from #:alexandria
                #:alist-hash-table))
(in-package #:reblocks-text-editor/html)


(defgeneric to-html (node)
  (:documentation "Renders common-doc node to HTML using reblocks/html:with-html"))

(defgeneric html-class (node)
  (:documentation "Returns an optional string with HTML class of the element."))


(defun to-html-string (node)
  (with-output-to-string (reblocks/html:*stream*)
    (to-html node)))

(defmethod html-class ((node common-doc:document-node))
  (awhen (common-doc:metadata node)
    (awhen (gethash "class" it)
      it)))


(defparameter *common-doc-key-to-html-tag*
  '(("item" . "li")
    ("list" . "ul")))


(defmethod to-html ((node common-doc:document))
  (to-html (common-doc:children node)))


(defmethod to-html ((node common-doc:content-node))
  (let* ((node-type (class-of node))
         (tag (or (loop for key being the hash-key of common-doc::*registry*
                          using (hash-value value)
                        when (eql value node-type)
                          do (return (or (alexandria:assoc-value *common-doc-key-to-html-tag* key
                                                                 :test #'string-equal)
                                         key)))
                  "div")))
    (reblocks/html:with-html
      ;; TODO: add a check that there is no nodes prohibited as a span's content
      (:tag :name tag
            :id (common-doc:reference node)
            (to-html (common-doc:children node))))))

(defmethod to-html ((node common-doc:unordered-list))
  (reblocks/html:with-html
    (:ul
     :id (common-doc:reference node)
     (to-html (common-doc:children node)))))


(defmethod to-html ((nodes list))
  (mapc #'to-html nodes)
  (values))

;; (defmethod to-html ((node common-doc:paragraph))
;;   (reblocks/html:with-html
;;     ;; TODO: add a check that there is no nodes prohibited as a P container's content
;;     (:p :id (common-doc:reference node)
;;         (mapc #'to-html (common-doc:children node)))))


(defmethod to-html ((node common-doc:text-node))
  (reblocks/html:with-html
    ;; TODO: add a check that there is no nodes prohibited as a span container  
    (:span :id (common-doc:reference node)
           :class (html-class node)
           (mapc #'to-html (uiop:ensure-list
                            (common-doc:text node))))))


(defmethod to-html ((node commondoc-markdown/raw-html:raw-inline-html))
  (reblocks/html:with-html
    (:raw (commondoc-markdown/raw-html:html node))))


(defmethod to-html ((node common-doc:bold))
  (reblocks/html:with-html
    (:b :id (common-doc:reference node)
        (mapc #'to-html (common-doc:children node)))))


(defmethod to-html ((node common-doc:italic))
  (reblocks/html:with-html
    (:i :id (common-doc:reference node)
        :class (html-class node)
        (mapc #'to-html (common-doc:children node)))))


(defmethod to-html ((node commondoc-markdown:markdown-link))
  (reblocks/html:with-html
    (:span :id (common-doc:reference node)
           :class (html-class node)
           (mapc #'to-html (common-doc:children node)))))


(defmethod common-doc:children :around ((node commondoc-markdown:markdown-link))
  (if *render-markup*
      (let* ((definition (commondoc-markdown:markdown-link-definition node))
             ;; TODO: here we need somehow to create a real link:
             (uri (quri:make-uri :scheme "internal"
                                 :path definition)))
        (list (make-markup2 node "[" "left-bracket")
              (make-visible-weblink (call-next-method)
                                    uri)
              (make-markup2 node "]" "right-bracket")
              (make-markup2 node "[" "right-bracket")
              (make-markup2 node definition "definition")
              (make-markup2 node "]" "right-bracket")))
      (call-next-method)))


(defclass visible-weblink (common-doc:web-link)
  ())


(defun make-visible-weblink (children uri)
  (make-instance 'visible-weblink
                 :children children
                 :uri uri))

(defmethod to-html ((node visible-weblink))
  (reblocks/html:with-html
    (:a :id (common-doc:reference node)
        :class (html-class node)
        :href (quri:render-uri (common-doc:uri node))
        (mapc #'to-html (common-doc:children node)))))


(defmethod to-html ((node common-doc:web-link))
  (reblocks/html:with-html
    (:span :id (common-doc:reference node)
           :class (html-class node)
           (mapc #'to-html (common-doc:children node)))))


(defmethod common-doc:children :around ((node common-doc:web-link))
  (if (and *render-markup*
           (not (typep node 'visible-weblink)))
      (let ((uri (common-doc:uri node)))
        (list (make-markup2 node "[" "left-bracket")
              (make-visible-weblink (call-next-method)
                                    uri)
              (make-markup2 node "]" "right-braket")
              (make-markup2 node "(" "left-paren")
              (make-markup2 node (quri:render-uri uri) "uri")
              (make-markup2 node ")" "right-paren")))
      (call-next-method)))


(defmethod to-html ((node string))
  (reblocks/html:with-html
    (:raw (plump:encode-entities node))))


;; markup

(defclass markup (common-doc:text-node)
  ())


(defun make-markup (node side)
  (unless (member side '("left" "right") :test #'string=)
    (error "SIDE argument should be either \"left\" or \"right\"."))

  (let ((text (etypecase node
                (common-doc:bold "**")
                (common-doc:italic "*"))))
    (make-instance 'markup
                   :text text
                   :metadata (alist-hash-table
                              (list (cons "class"
                                          "markup"))
                              :test 'equal)
                   :reference (format nil "~A-~A-markup"
                                      (common-doc:reference node)
                                      side))))

(defun make-markup2 (node text markup-type)
  (make-instance 'markup
                 :text text
                 :metadata (alist-hash-table
                            (list (cons "class"
                                        "markup"))
                            :test 'equal)
                 :reference (format nil "~A-~A"
                                    (common-doc:reference node)
                                    markup-type)))

(defvar *render-markup* t)


(defun markup-p (node)
  (typep node 'markup))


(defmethod common-doc:children :around ((node common-doc:italic))
  (if *render-markup*
      (append (list (make-markup node "left"))
              (call-next-method)
              (list (make-markup node "right")))
      (call-next-method)))


(defmethod common-doc:children :around ((node common-doc:bold))
  (if *render-markup*
      (append (list (make-markup node "left"))
              (call-next-method)
              (list (make-markup node "right")))
      (call-next-method)))
