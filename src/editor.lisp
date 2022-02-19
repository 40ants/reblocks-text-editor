(uiop:define-package #:reblocks-text-editor/editor
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-html)
  (:import-from #:reblocks-parenscript)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks-text-editor/html)
  (:import-from #:reblocks-text-editor/frontend/js)
  (:import-from #:reblocks-text-editor/frontend/css)
  (:import-from #:reblocks-text-editor/document/editable)
  (:import-from #:reblocks-text-editor/document/ops
                #:map-document)
  (:import-from #:reblocks-text-editor/utils/markdown
                #:to-markdown)
  (:import-from #:parenscript
                #:create
                #:chain
                #:@)
  (:import-from #:bordeaux-threads
                #:make-lock)
  (:import-from #:alexandria
                #:lastcar
                #:curry)
  (:import-from #:reblocks-text-editor/utils/http
                #:retrieve-url-title)
  (:local-nicknames (#:ops #:reblocks-text-editor/document/ops))
  (:export
   #:on-document-update))
(in-package #:reblocks-text-editor/editor)


(defun make-initial-document ()
  (let* ((content (reblocks-text-editor/utils/markdown::from-markdown "
Hello **Lisp** World!

Second Line.

"))
         (doc (make-instance 'reblocks-text-editor/document/editable::editable-document
                             :children (list content))))
    
    (reblocks-text-editor/document/ops::add-reference-ids doc)))


(reblocks/widget:defwidget editor ()
  ((document :type reblocks-text-editor/document/editable::editable-document
             :initform (make-initial-document)
             :reader document)))


(reblocks/widget:defwidget editor-demo ()
  ((editor :type editor
           :initform (make-instance 'editor)
           :reader editor-demo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This is our BACKEND code doing most business logic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *document* nil)
(defvar *widget* nil)


(defun process-usual-update (document path new-html cursor-position)
  (let* ((paragraph (reblocks-text-editor/document/ops::find-changed-node document path))
         (plain-text (reblocks-text-editor/utils/text::remove-html-tags new-html)))
    (cond
      (paragraph
       (log:error "Updating paragraph at" path)
       (multiple-value-bind (current-node cursor-position)
           (reblocks-text-editor/document/ops::update-paragraph-content
            document paragraph plain-text cursor-position)

         (reblocks-text-editor/document/ops::ensure-cursor-position-is-correct
          current-node cursor-position)))
      (t
       (log:warn "Cant find paragraph at" path)))))


(defun move-cursor-up (document path cursor-position)
  (let* ((current-paragraph (ops::find-changed-node document path))
         (prev-paragraph (ops::find-previous-paragraph document current-paragraph)))
    (check-type current-paragraph common-doc:paragraph)
    (when prev-paragraph
      (ops::ensure-cursor-position-is-correct prev-paragraph cursor-position))))


(defun move-cursor-down (document path cursor-position)
  (let* ((current-paragraph (ops::find-changed-node document path))
         (next-paragraph (ops::find-next-paragraph document current-paragraph)))
    (check-type current-paragraph common-doc:paragraph)
    (when next-paragraph
      (ops::ensure-cursor-position-is-correct next-paragraph cursor-position))))


(defun paste-text (document path cursor-position pasted-text)
  (log:info "User wants to insert ~A" pasted-text)

  (let* ((paragraph (reblocks-text-editor/document/ops::find-changed-node document path)))
    (cond
      ((or (str:starts-with-p "http://" pasted-text)
           (str:starts-with-p "https://" pasted-text))
       (multiple-value-bind (node new-cursor-position)
           (ops::find-node-at-position paragraph
                                       cursor-position)
         (declare (ignore new-cursor-position))
         (cond
           (node
            (let* ((title (or (retrieve-url-title pasted-text)
                              pasted-text))
                   (new-node (common-doc:make-web-link
                              pasted-text
                              (list (common-doc:make-text title)))))
              (ops::add-reference-ids document
                                      :to-node new-node)
              (ops::insert-node document
                                new-node
                                :relative-to node)

              (let* ((last-child (lastcar (common-doc:children new-node)))
                     (last-child-len (length (to-markdown last-child))))
                (ops::ensure-cursor-position-is-correct last-child last-child-len))))
           (t
            (log:error "Unable to find node for"
                       cursor-position
                       (reblocks-text-editor/html::to-html-string paragraph)))))))))


(defgeneric on-document-update (widget)
  (:documentation "Called after the each document update.")
  (:method ((widget editor))
    (values)))


(defmethod reblocks/widget:render ((widget editor))
  (let ((document (document widget)))
    (setf *document*
          document)
    (setf *widget*
          widget)


    (labels ((process-update (&key change-type version new-html path cursor-position pasted-text &allow-other-keys)
               (bordeaux-threads:with-lock-held ((reblocks-text-editor/document/editable::document-lock document))
                 (when (> version (reblocks-text-editor/document/editable::content-version document))
                   (log:error "Processing" new-html path cursor-position version change-type)
                  
                   (setf (reblocks-text-editor/document/editable::content-version document)
                         version)

                   (cond
                     ;; This operation is similar to "split-paragraph"
                     ;; but it splits a paragraph and created a new list
                     ;; item when the cursor is in the list item.
                     ((string= change-type
                               "split")
                      (reblocks-text-editor/document/ops::split-paragraph
                       document path new-html cursor-position
                       :dont-escape-from-list-item t)

                     
                      (let* ((changed-paragraph
                               (reblocks-text-editor/document/ops::find-changed-node document path))
                             (list-item
                               (reblocks-text-editor/document/ops::select-outer-list-item
                                document changed-paragraph)))

                        (when list-item
                          (let ((next-paragraphs
                                  (reblocks-text-editor/document/ops::select-siblings-next-to
                                   list-item changed-paragraph)))
                            (mapcar (curry #'reblocks-text-editor/document/ops::delete-node document)
                                    next-paragraphs)
                           
                            (let ((new-list-item
                                    (common-doc:make-list-item next-paragraphs
                                                               :reference (reblocks-text-editor/document/editable::get-next-reference-id
                                                                           document))))
                              (reblocks-text-editor/document/ops::insert-node
                               document new-list-item :relative-to list-item)
                              ;; When a new list item is inserted
                              ;; the cursor should be placed on the
                              ;; first paragraph.
                              (when next-paragraphs
                                (reblocks-text-editor/document/ops::ensure-cursor-position-is-correct
                                 (first next-paragraphs)
                                 0)))))))
                     ((string= change-type
                               "split-paragraph")
                      (reblocks-text-editor/document/ops::split-paragraph
                       document path new-html cursor-position))
                     ((string= change-type
                               "join-with-prev-paragraph")
                      (reblocks-text-editor/document/ops::join-with-prev-paragraph
                       document path new-html cursor-position))
                     ((string= change-type
                               "indent")
                      (reblocks-text-editor/document/ops::indent document path cursor-position))
                     ((string= change-type
                               "dedent")
                      (reblocks-text-editor/document/ops::dedent document path cursor-position))
                     ((string= change-type
                               "move-cursor-up")
                      (move-cursor-up document path cursor-position))
                     ((string= change-type
                               "move-cursor-down")
                      (move-cursor-down document path cursor-position))
                     ((string= change-type
                               "paste")
                      (paste-text document path cursor-position
                                  pasted-text))
                     (t
                      (process-usual-update document path new-html cursor-position))))

                 (on-document-update widget))))
     
      (let ((action-code (reblocks/actions:make-action #'process-update))
            ;; We need this flag to make sure our document will have
            ;; a visible markup on active paragraph.
            (reblocks-text-editor/html::*render-markup* t))
        (reblocks/html:with-html
          (:div :class "content"
                :data-action-code action-code
                :data-version (reblocks-text-editor/document/editable::content-version document)
                :contenteditable ""
                :onload "setup()"
                (reblocks-text-editor/html::to-html document)))))))


(defmethod reblocks/widget:render ((widget editor-demo))
  (labels ((reset-text (&rest args)
             (declare (ignore args))
             (setf (slot-value (editor-demo widget) 'document)
                   (make-initial-document))
             (reblocks/widget:update (editor-demo widget))))
    
    (reblocks/html:with-html
      (:h1 "Experimental HTML editor")
      (:h2 "Using Common Lisp + Reblocks")
      (reblocks/widget:render (editor-demo widget))

      (:p :id "debug"
          "Path will be shown here.")

      (:p (:button :onclick (reblocks/actions:make-js-action #'reset-text)
                   "Reset Text")))))


(defmethod reblocks/dependencies:get-dependencies ((widget editor))
  (list* (reblocks-text-editor/frontend/js::make-js-code)
         (reblocks-text-editor/frontend/css::make-css-code)
         (call-next-method)))
