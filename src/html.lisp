(uiop:define-package #:zibaldone/html
  (:use #:cl)
  (:import-from #:reblocks)
  (:import-from #:common-doc)
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
            (mapc #'to-html (common-doc:children node))))))

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

(defmethod common-doc:children :around ((node common-doc:italic))
  (append (list (common-doc:make-text "*"
                                      :metadata (alist-hash-table
                                                 (list (cons "class"
                                                             "markup")))
                                      :reference (format nil "~A-left-markup"
                                                         (common-doc:reference node))))
          (call-next-method)
          (list (common-doc:make-text "*"
                                      :metadata (alist-hash-table
                                                 (list (cons "class"
                                                             "markup")))
                                      :reference (format nil "~A-right-markup"
                                                         (common-doc:reference node))))))


(defmethod common-doc:children :around ((node common-doc:bold))
  (append (list (common-doc:make-text "**"
                                      :metadata (alist-hash-table
                                                 (list (cons "class"
                                                             "markup")))
                                      :reference (format nil "~A-left-markup"
                                                         (common-doc:reference node))))
          (call-next-method)
          (list (common-doc:make-text "**"
                                      :metadata (alist-hash-table
                                                 (list (cons "class"
                                                             "markup")))
                                      :reference (format nil "~A-right-markup"
                                                         (common-doc:reference node))))))
