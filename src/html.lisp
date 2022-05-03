(uiop:define-package #:reblocks-text-editor/html
  (:use #:cl)
  (:import-from #:reblocks)
  (:import-from #:common-doc)
  (:import-from #:commondoc-markdown)
  (:import-from #:anaphora
                #:it
                #:alet
                #:awhen)
  (:import-from #:reblocks-text-editor/html/markup
                #:markup-p
                #:markup
                #:make-markup2)
  (:import-from #:alexandria
                #:alist-hash-table))
(in-package #:reblocks-text-editor/html)


(defvar *render-markup* nil)

(defvar *hide-markup-nodes* nil)


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
         (tag (loop for key being the hash-key of common-doc::*registry*
                      using (hash-value value)
                    when (eql value node-type)
                      do (return (or (alexandria:assoc-value *common-doc-key-to-html-tag* key
                                                             :test #'string-equal)
                                     key)))))
    (reblocks/html:with-html
      (if tag
          (:tag :name tag
                :id (common-doc:reference node)
                (to-html (common-doc:children node)))
          ;; by default, content node has no HTML representation
          (to-html (common-doc:children node))))))

(defmethod to-html ((node common-doc:unordered-list))
  (reblocks/html:with-html
    (:ul
     :id (common-doc:reference node)
     (to-html (common-doc:children node)))))


(defmethod to-html ((nodes list))
  (mapc #'to-html nodes)
  (values))

(defmethod to-html ((node common-doc:paragraph))
  (let ((class (format nil "~@[~A ~]block"
                       (html-class node))))
    (reblocks/html:with-html
      (:p :id (common-doc:reference node)
          :class class
          (mapc #'to-html (common-doc:children node))))))


(defmethod to-html ((node common-doc:text-node))
  (reblocks/html:with-html
    ;; TODO: add a check that there is no nodes prohibited as a span container  
    (:span :id (common-doc:reference node)
           :class (html-class node)
           (mapc #'to-html (uiop:ensure-list
                            (common-doc:text node))))))

(defmethod to-html ((node common-doc:code))
  (reblocks/html:with-html
    (:code :id (common-doc:reference node)
           :class (html-class node)
           (mapc #'to-html (uiop:ensure-list
                            (common-doc:children node))))))

(defmethod to-html ((node common-doc:code-block))
  (let ((class (format nil "~@[~A ~]code-block block"
                       (html-class node))))
    (reblocks/html:with-html
      (:pre :id (common-doc:reference node)
            :class class
            ;; (:code :id (format nil "~A-code"
            ;;                    (common-doc:reference node))
            ;;        ;; (if (common-doc:children node)
            ;;        ;;     (common-doc:text
            ;;        ;;      (first (common-doc:children node)))
            ;;        ;;     "")
            ;;        )
            (:code (mapc #'to-html (uiop:ensure-list
                                    (common-doc:children node))))))))


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


(defmethod to-html ((node common-doc:strikethrough))
  (reblocks/html:with-html
    (:s :id (common-doc:reference node)
        :class (html-class node)
        (mapc #'to-html (common-doc:children node)))))


(defmethod to-html ((node string))
  (reblocks/html:with-html
    (:raw (plump:encode-entities node))))


;; markup

(defgeneric add-markup-to (node)
  (:method ((node t))
    node)
  (:method ((node common-doc:italic))
    (let ((children (common-doc:children node)))
      (setf (common-doc:children node)
            (append (unless (typep (first children)
                                   'markup)
                      (list (make-markup2 node "*" "left")))
                    children
                    (unless (typep (alexandria:lastcar children)
                                   'markup)
                      (list (make-markup2 node "*" "right")))))
      node))
  (:method ((node common-doc:bold))
    (let ((children (common-doc:children node)))
      (setf (common-doc:children node)
            (append (unless (typep (first children)
                                   'markup)
                      (list (make-markup2 node "**" "left")))
                    children
                    (unless (typep (alexandria:lastcar children)
                                   'markup)
                      (list (make-markup2 node "**" "right")))))
      node))
  (:method ((node common-doc:code))
    (let ((children (common-doc:children node)))
      (setf (common-doc:children node)
            (append (unless (typep (first children)
                                   'markup)
                      (list (make-markup2 node "`" "left")))
                    children
                    (unless (typep (alexandria:lastcar children)
                                   'markup)
                      (list (make-markup2 node "`" "right")))))
      node))
  (:method ((node common-doc:strikethrough))
    (let ((children (common-doc:children node)))
      (setf (common-doc:children node)
            (append (unless (typep (first children)
                                   'markup)
                      (list (make-markup2 node "--" "left")))
                    children
                    (unless (typep (alexandria:lastcar children)
                                   'markup)
                      (list (make-markup2 node "--" "right")))))
      node))
  (:method ((node common-doc:web-link))
    (let ((children (remove-if #'markup-p
                               (common-doc:children node)))
          (uri (common-doc:uri node)))
      (setf (common-doc:children node)
            (list (make-markup2 node "[" "left-bracket")
                  (uiop:symbol-call :reblocks-text-editor/html/web-link
                                    :make-visible-weblink
                                    node
                                    children
                                    uri)
                  (make-markup2 node "]" "right-bracket")
                  (make-markup2 node "(" "left-paren")
                  (make-markup2 node
                                (quri:render-uri uri)
                                "uri")
                  (make-markup2 node ")" "right-paren")))
      node)))


(defmethod common-doc:children :around ((node t))
  (let ((result (call-next-method)))
    (if *hide-markup-nodes*
        (remove-if #'markup-p result)
        result)))


(defun ensure-markup-nodes (root-node)
  (common-doc.ops:with-document-traversal (root-node node)
    (add-markup-to node))
  root-node)
