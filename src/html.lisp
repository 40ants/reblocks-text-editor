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
                #:make-markup2)
  (:import-from #:alexandria
                #:alist-hash-table))
(in-package #:reblocks-text-editor/html)


(defvar *render-markup* nil)


(defgeneric to-html (node)
  (:documentation "Renders common-doc node to HTML using reblocks/html:with-html"))

(defgeneric html-class (node)
  (:documentation "Returns an optional string with HTML class of the element."))


(defun children-including-markup (node)
  (let ((*render-markup* t))
    (common-doc:children node)))


(defun to-html-string (node)
  (let ((*render-markup* t))
    (with-output-to-string (reblocks/html:*stream*)
      (to-html node))))

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


(defmethod to-html ((node common-doc:image))
  (reblocks/html:with-html
    (:img :id (common-doc:reference node)
          :class (html-class node)
          :src (common-doc:source node)
          :title (common-doc:description node))))


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

(defmethod common-doc:children :around ((node common-doc:italic))
  (if *render-markup*
      (append (list (make-markup2 node "*" "left"))
              (call-next-method)
              (list (make-markup2 node "*" "right")))
      (call-next-method)))


(defmethod common-doc:children :around ((node common-doc:bold))
  (if *render-markup*
      (append (list (make-markup2 node "**" "left"))
              (call-next-method)
              (list (make-markup2 node "**" "right")))
      (call-next-method)))

(defmethod common-doc:children :around ((node common-doc:code))
  (if *render-markup*
      (append (list (make-markup2 node "`" "left"))
              (call-next-method)
              (list (make-markup2 node "`" "right")))
      (call-next-method)))

(defmethod common-doc:children :around ((node common-doc:strikethrough))
  (if *render-markup*
      (append (list (make-markup2 node "--" "left"))
              (call-next-method)
              (list (make-markup2 node "--" "right")))
      (call-next-method)))

