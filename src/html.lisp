(uiop:define-package #:zibaldone/html
  (:use #:cl)
  (:import-from #:reblocks)
  (:import-from #:common-doc))
(in-package #:zibaldone/html)


(defgeneric to-html (node)
  (:documentation "Renders common-doc node to HTML using reblocks/html:with-html"))

(defgeneric is-editable (node)
  (:method ((node t))
    nil))


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
            :contenteditable (is-editable node)
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
           :contenteditable (is-editable node)
           (mapc #'to-html (uiop:ensure-list
                            (common-doc:text node))))))


(defmethod to-html ((node common-doc:bold))
  (reblocks/html:with-html
    (:span :contenteditable ""
           :id (common-doc:reference node)
           (:span :class "markup"
                  "**")

           (:b (mapc #'to-html (common-doc:children node)))
           
           (:span :class "markup"
                  "**"))))


(defmethod is-editable ((node common-doc:text-node))
  t)


(defmethod to-html ((node string))
  (reblocks/html:with-html
    (:raw (plump:encode-entities node))))
