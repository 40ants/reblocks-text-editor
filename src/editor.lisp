(uiop:define-package #:reblocks-text-editor/editor
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-html)
  (:import-from #:reblocks-parenscript)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks-text-editor/html
                #:children-including-markup)
  (:import-from #:reblocks-text-editor/html/markdown-link)
  (:import-from #:reblocks-text-editor/html/web-link)
  (:import-from #:reblocks-text-editor/html/document-link)
  (:import-from #:reblocks-text-editor/frontend/js)
  (:import-from #:reblocks-text-editor/frontend/css)
  (:import-from #:reblocks-text-editor/document/copying)
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
  (:import-from #:serapeum
                #:slice)
  (:import-from #:metacopy
                #:copy-thing)
  (:local-nicknames (#:ops #:reblocks-text-editor/document/ops))
  (:export #:on-document-update))
(in-package #:reblocks-text-editor/editor)


(defun unwrap-content (node)
  "Returns a list of common doc node. If NODE is a common-doc:content-node,
   then it's children are returned wheren unwrap-content is applied to each
   of them recursively."
  (cond
    ((eql (type-of node)
          'common-doc:content-node)
     (loop for child in (common-doc:children node)
           append (unwrap-content child)))
    (t
     (list node))))


(defun make-document-from-markdown-string (string)
  (let* ((content (reblocks-text-editor/utils/markdown::from-markdown string))
         (doc (make-instance 'reblocks-text-editor/document/editable::editable-document
                             :children (unwrap-content content))))
    
    (reblocks-text-editor/document/ops::add-reference-ids doc)))


(defun make-initial-document ()
  (make-document-from-markdown-string "
Hello **Lisp** World!

```
Block of code

Second line
```
"))


(reblocks/widget:defwidget editor ()
  ((document :type reblocks-text-editor/document/editable::editable-document
             :initform (make-initial-document)
             :accessor document)))


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
       (multiple-value-bind (current-node new-cursor-position)
           (reblocks-text-editor/document/ops::update-node-content
            document paragraph plain-text cursor-position)

         (reblocks-text-editor/document/ops::ensure-cursor-position-is-correct
          document current-node new-cursor-position)))
      (t
       (log:warn "Cant find paragraph at" path)))))


(defun move-cursor-up (document path cursor-position)
  (let* ((current-paragraph (ops::find-changed-node document path))
         (prev-paragraph (ops::find-previous-paragraph document current-paragraph)))
    (check-type current-paragraph common-doc:paragraph)
    (when prev-paragraph
      (ops::ensure-cursor-position-is-correct document prev-paragraph cursor-position))))


(defun move-cursor-down (document path cursor-position)
  (let* ((current-paragraph (ops::find-changed-node document path))
         (next-paragraph (ops::find-next-paragraph document current-paragraph)))
    (check-type current-paragraph common-doc:paragraph)
    (when next-paragraph
      (ops::ensure-cursor-position-is-correct document next-paragraph cursor-position))))


(defun insert-node (document new-node relative-to &key (position :after))
  (ops::add-reference-ids document
                          :to-node new-node)
  (ops::insert-node document
                    new-node
                    :position position
                    :relative-to relative-to)

  (let* ((last-child (lastcar (children-including-markup new-node)))
         (last-child-len (length (to-markdown last-child))))
    (ops::ensure-cursor-position-is-correct document last-child last-child-len)))


(defun make-node-from-pasted-text (text-before pasted-text)
  (cond
    ((and (or (str:starts-with-p "http://" pasted-text)
              (str:starts-with-p "https://" pasted-text))
          ;; We don't want to expand link when inserting
          ;; url after the text like "[some]("
          (not (cl-ppcre:all-matches "\\[[^]]*\\]\\($" text-before)))
     (let* ((url pasted-text)
            (title (or (retrieve-url-title url)
                       url)))
       (common-doc:make-web-link
                      url
                      (list (common-doc:make-text title)))))
    (t
     (common-doc:make-text pasted-text))))


(defun paste-text (document path cursor-position pasted-text)
  (log:info "User wants to paste this text \"~A\"" pasted-text)

  (let* ((current-paragraph
           (reblocks-text-editor/document/ops::find-changed-node document path))
         (text (to-markdown current-paragraph
                            ;; It is important to not trim a space,
                            ;; otherwise a space before the cursor will be lost:
                            :trim-spaces nil))
         (text-before (slice text 0 cursor-position))
         (new-node (make-node-from-pasted-text text-before pasted-text)))
    (ops::insert-into-paragraph document path cursor-position
                                new-node)))


(defgeneric on-document-update (widget)
  (:documentation "Called after the each document update.")
  (:method ((widget editor))
    (reblocks-text-editor/document/editable::history-push (document widget))
    (values)))


(defun process-undo (widget &rest args)
  (declare (ignore args))
  (log:debug "Processing UNDO")
  (setf (document widget)
        (reblocks-text-editor/document/editable::history-pop
         (document widget)))
  (reblocks/widget:update widget)

  (let ((document (document widget)))
    (destructuring-bind (node caret-position)
        (reblocks-text-editor/document/editable::caret-position document)
      (reblocks-text-editor/document/ops::ensure-cursor-position-is-correct
       document node caret-position))))


(defgeneric on-shortcut (widget key-code node cursor-position)
  (:documentation "Called when user hits a registered shortcut.")
  (:method ((widget editor) key-code node cursor-position)))


(defgeneric process-message-from-frontend (widget message-type &rest args &key version &allow-other-keys)
  (:method ((widget editor) message-type &rest args &key version &allow-other-keys)
    (let ((document (document widget)))
      (bordeaux-threads:with-lock-held ((reblocks-text-editor/document/editable::document-lock document))
        (when (> version (reblocks-text-editor/document/editable::content-version document))
          (setf (reblocks-text-editor/document/editable::content-version document)
                version)
          (cond
            ((string-equal message-type "update")
             (apply #'process-update widget args))
            ((string-equal message-type "shortcut")
             (apply #'process-shortcut widget args))
            ((string-equal message-type "undo")
             (apply #'process-undo widget args))
            ((string-equal message-type "link")
             (apply #'process-link widget args))
            (t
             (log:error "Don't know how to process message of this type"
                        message-type
                        args))))))))


(defgeneric process-update (widget &key change-type new-html path cursor-position pasted-text &allow-other-keys)
  (:method (widget &key change-type new-html path cursor-position pasted-text &allow-other-keys)
    (check-type cursor-position integer)
    
    (let ((document (document widget)))
      (log:error "Processing" new-html path cursor-position change-type)
    
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
               (mapc (curry #'reblocks-text-editor/document/ops::delete-node document)
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
                    document
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
         (process-usual-update document path new-html cursor-position)))

      (on-document-update widget))))


(defgeneric process-shortcut (widget &key key-code path cursor-position &allow-other-keys)
  (:method (widget &key key-code path cursor-position &allow-other-keys)
    (let ((document (document widget)))
      (log:info "Key" key-code "pressed")

      (let ((paragraph (reblocks-text-editor/document/ops::find-changed-node document path)))
        (multiple-value-bind (node new-cursor-position)
            (ops::find-node-at-position paragraph
                                        cursor-position)
          (on-shortcut widget key-code node new-cursor-position))))))

(defgeneric process-link (widget &key href &allow-other-keys)
  (:method (widget &key href &allow-other-keys)
    (log:info "Link" href "was clicked")))


(defmethod reblocks/widget:render ((widget editor))
  (let ((document (document widget)))
    (setf *document*
          document)
    (setf *widget*
          widget)


    (labels ((process-message (&rest args &key type &allow-other-keys)
               (let ((args (alexandria:remove-from-plist args :type)))
                 (apply #'process-message-from-frontend widget type args))))
      (let ((action-code (reblocks/actions:make-action #'process-message))
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
  ;; https://keycode.info/
  (list* (reblocks-text-editor/frontend/js::make-js-code "/")
         (reblocks-text-editor/frontend/css::make-css-code)
         (call-next-method)))
