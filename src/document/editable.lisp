(uiop:define-package #:reblocks-text-editor/document/editable
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock)
  (:import-from #:metacopy
                #:copy-thing)
  (:import-from #:reblocks-text-editor/utils/markdown
                #:to-markdown)
  ;; (:import-from #:reblocks-text-editor/document/ops
  ;;               #:select-outer-block
  ;;               #:split-nodes)
  (:import-from #:reblocks-text-editor/utils/scribdown
                #:to-scribdown)
  (:import-from #:reblocks-text-editor/utils/text
                #:collect-all-text))
(in-package #:reblocks-text-editor/document/editable)


(defclass editable-document (common-doc:content-node)
  ((lock :initform (make-lock "Editable Document Lock")
         :reader document-lock)
   (revision :initform 0
             :accessor content-version)
   (next-id :initform 1
            :accessor next-id)
   (title :initform "Untitled"
          :initarg :title
          :accessor common-doc:title)
   (undo-history :type list
                 :initform nil
                 :documentation "Simplest form of undo, using a document stack."
                 :accessor undo-history)
   (caret-position :initform nil
                   :documentation "Stores a tuple of two elements.
                                    The first is a node where caret is located and
                                    the second is a number of the caret offset."
                   :accessor caret-position)
   (text-before-caret :initform ""
                      :type string
                      :documentation "Stores current paragraph's text before the caret."
                      :accessor text-before-caret)
   (text-after-caret :initform ""
                     :type string
                     :documentation "Stores current paragraph's text after the caret."
                     :accessor text-after-caret)))


(defun make-editable-document (children)
  (make-instance 'editable-document
                 :children (uiop:ensure-list children)))


(defmethod (setf caret-position) :after ((value t) (document editable-document))
  (destructuring-bind (node position)
      value
    (let ((nodes (if (slot-exists-p node 'common-doc:children)
                     (common-doc:children node)
                     (list node))))
      (destructuring-bind (left right)
          (uiop:symbol-call :reblocks-text-editor/document/ops
                            :split-nodes
                            nodes
                            position)
        ;; TODO: надо придумать что-то с тем, что в разорванной посередине markup
        ;; конструкции каждая часть обрамляется в markup.
        ;; возможно тут to-scribdown не лучший вариант и надо просто делать to-text
        (let ((text-before
                (collect-all-text (common-doc:make-content left))
                ;; (to-scribdown (common-doc:make-content left)
                ;;               ;; It is important to not trim a space,
                ;;               ;; otherwise a space before the cursor will be lost:
                ;;               :trim-spaces nil)
                )
              (text-after
                (collect-all-text (common-doc:make-content right))
                ;; (to-scribdown (common-doc:make-content right)
                ;;               ;; It is important to not trim a space,
                ;;               ;; otherwise a space before the cursor will be lost:
                ;;               :trim-spaces nil)
                ))
          (setf (text-before-caret document)
                text-before)
          (setf (text-after-caret document)
                text-after))))
    ;; (let ((block-node (select-outer-block document node)))
    ;;   (destructuring-bind (left right)
    ;;       (split-nodes (list block-node) position)
    ;;     (setf (text-before-caret document)
    ;;           (to-scribdown (common-doc:make-content left)
    ;;                         ;; It is important to not trim a space,
    ;;                         ;; otherwise a space before the cursor will be lost:
    ;;                         :trim-spaces nil))
    ;;     (setf (text-after-caret document)
    ;;           (to-scribdown (common-doc:make-content right)
    ;;                         ;; It is important to not trim a space,
    ;;                         ;; otherwise a space before the cursor will be lost:
    ;;                         :trim-spaces nil))))
    ))


(defun get-next-reference-id (document)
  (check-type document editable-document)
  
  (prog1 (format nil "el~A"
                 (next-id document))
    (incf (next-id document))))


(defun history-push (document)
  (push (copy-thing document)
        (undo-history document)))


(defun history-pop (document)
  (let ((old-document (car (undo-history document))))
    (setf (undo-history document)
          (cdr
           (undo-history document)))
    (values old-document)))
