(uiop:define-package #:reblocks-text-editor/html/markup
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table))
(in-package #:reblocks-text-editor/html/markup)


(defclass markup (common-doc:text-node)
  ())


;; (defun make-markup (node side)
;;   (unless (member side '("left" "right") :test #'string=)
;;     (error "SIDE argument should be either \"left\" or \"right\"."))

;;   (let ((text (etypecase node
;;                 (common-doc:bold "**")
;;                 (common-doc:italic "*"))))
;;     (make-instance 'markup
;;                    :text text
;;                    :metadata (alist-hash-table
;;                               (list (cons "class"
;;                                           "markup"))
;;                               :test 'equal)
;;                    :reference (format nil "~A-~A-markup"
;;                                       (common-doc:reference node)
;;                                       side))))



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

(defun markup-p (node)
  (typep node 'markup))

