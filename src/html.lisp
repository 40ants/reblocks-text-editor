(uiop:define-package #:zibaldone/html
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
(in-package #:zibaldone/html)


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


(defmethod to-html ((node common-doc:content-node))
  (let* ((node-type (class-of node))
         (tag (or (loop for key being the hash-key of common-doc::*registry*
                          using (hash-value value)
                        when (eql value node-type)
                          do (return key))
                  "div")))
    (reblocks/html:with-html
      ;; TODO: add a check that there is no nodes prohibited as a span's content
      (:tag :name tag
            :id (common-doc:reference node)
            (to-html (common-doc:children node))))))


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
        ;; (:span :class "markup"
        ;;        "**")

        (mapc #'to-html (common-doc:children node))
           
        ;; (:span :class "markup"
        ;;        "**")
        )))

(defmethod to-html ((node common-doc:italic))
  (reblocks/html:with-html
    (:i :id (common-doc:reference node)
        :class (html-class node)
        ;; (:span :class "markup"
        ;;        "*")

        (mapc #'to-html (common-doc:children node))
           
        ;; (:span :class "markup"
        ;;        "*")
        )))


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
                                          "markup")))
                   :reference (format nil "~A-~A-markup"
                                      (common-doc:reference node)
                                      side))))

(defun markup-p (node)
  (typep node 'markup))


(defmethod common-doc:children :around ((node common-doc:italic))
  (append (list (make-markup node "left"))
          (call-next-method)
          (list (make-markup node "right"))))


(defmethod common-doc:children :around ((node common-doc:bold))
  (append (list (make-markup node "left"))
          (call-next-method)
          (list (make-markup node "right"))))
