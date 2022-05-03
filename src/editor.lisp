(uiop:define-package #:reblocks-text-editor/editor
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-html)
  (:import-from #:reblocks-parenscript)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks-file-server)
  (:import-from #:reblocks-text-editor/html
                #:ensure-markup-nodes)
  (:import-from #:reblocks-text-editor/html/markdown-link)
  (:import-from #:reblocks-text-editor/html/web-link)
  (:import-from #:reblocks-text-editor/html/document-link)
  (:import-from #:reblocks-text-editor/html/image)
  (:import-from #:reblocks-text-editor/frontend/js)
  (:import-from #:reblocks-text-editor/frontend/css)
  (:import-from #:reblocks-text-editor/document/copying)
  (:import-from #:reblocks-text-editor/document/editable
                #:text-before-caret)
  (:import-from #:reblocks-text-editor/typed-pieces/common-doc
                #:make-common-doc-piece)
  (:import-from #:reblocks-text-editor/document/ops
                #:ensure-cursor-position-is-correct
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
                #:retrieve-url-info)
  (:import-from #:serapeum
                #:slice)
  (:import-from #:metacopy
                #:copy-thing)
  (:import-from #:reblocks-text-editor/utils/text
                #:+zero-width-space+
                #:trim-spaces
                #:remove-html-tags)
  (:import-from #:reblocks-text-editor/blocks/code
                #:code)
  (:import-from #:reblocks-text-editor/typed-pieces/html
                #:make-html-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/base
                #:caret
                #:convert)
  (:import-from #:reblocks-text-editor/typed-pieces/scribdown
                #:make-scribdown-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/common-doc-impl
                #:calculate-caret-position-for-html)
  (:local-nicknames (#:ops #:reblocks-text-editor/document/ops))
  (:export #:on-document-update))
(in-package #:reblocks-text-editor/editor)


;; TODO: Figure out a better way to store uploads.
;;       Probably, all code about storing documents should be moved from Hypernot
;;       into the reblocks-text-editor?
(defparameter +documents-root+ #P"~/Documents/hypernot/")


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


;; (reblocks/widget:defwidget editor-demo ()
;;   ((editor :type editor
;;            :initform (make-instance 'editor)
;;            :reader editor-demo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This is our BACKEND code doing most business logic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *document* nil)
(defvar *widget* nil)


(defun process-usual-update (document path new-html cursor-position)
  (let* ((changed-node (reblocks-text-editor/document/ops::find-changed-node document path)))
    (cond
      (changed-node
       (let* ((html-doc (make-html-piece new-html cursor-position))
              (scribdown (convert html-doc :scribdown
                                  document
                                  changed-node))
             ;; (plain-text (remove-html-tags
             ;;              new-html
             ;;              :remove-new-lines (not (typep node 'common-doc:code-block))))
             )
         (log:debug "Updating node content at" path scribdown)
         (multiple-value-bind (current-node new-cursor-position)
             (reblocks-text-editor/document/ops::update-node-content
              document changed-node scribdown)

           ;; TODO: probably we need to return a common-doc-piece
           ;; from update-node-content?
           (let* ((piece (make-common-doc-piece current-node new-cursor-position))
                  ;; (caret-in-html (calculate-caret-position-for-html piece))
                  )
             (ensure-cursor-position-is-correct document
                                                current-node
                                                (caret piece))))))
      (t
       (log:warn "Cant find node at" path)))))


(defgeneric move-cursor-up (document path-or-node cursor-position)
  (:method ((document t) (path-or-node t) (cursor-position t))))


(defmethod move-cursor-up (document (path list) cursor-position)
  (let ((node (ops::find-changed-node document path)))
    (move-cursor-up document node cursor-position)))


(defmethod move-cursor-up (document (node common-doc:paragraph) cursor-position)
  (let* ((prev-paragraph (ops::find-previous-paragraph document node)))
    (when prev-paragraph
      (ops::ensure-cursor-position-is-correct document prev-paragraph cursor-position))))


(defmethod move-cursor-up (document (node common-doc:code-block) caret-position)
  (let* ((text (code node))
         (new-caret-position (reblocks-text-editor/utils/text::move-caret-on-the-prev-line text caret-position)))
    (cond
      (new-caret-position
       (ops::ensure-cursor-position-is-correct document node new-caret-position))
      (t
       (let ((prev-paragraph (ops::find-previous-paragraph document node))
             (new-caret-position (reblocks-text-editor/utils/text::caret-position-from-beginning-of-the-line text caret-position)))
         ;; TODO: solve problem of moving cursor inside code blocks and other
         ;; blocks rendered in multiple lines:
         (when prev-paragraph
           (ops::ensure-cursor-position-is-correct document prev-paragraph new-caret-position)))))))


(defgeneric move-cursor-down (document path-or-node cursor-position)
  (:method ((document t) (path-or-node t) (cursor-position t))))


(defmethod move-cursor-down (document (path list) cursor-position)
  (let ((node (ops::find-changed-node document path)))
    (move-cursor-down document node cursor-position)))


(defmethod move-cursor-down (document (node common-doc:paragraph) cursor-position)
  (let ((next-paragraph (ops::find-next-paragraph document node)))
    ;; TODO: solve problem of moving cursor inside code blocks and other
    ;; blocks rendered in multiple lines:
    (when next-paragraph
      (ops::ensure-cursor-position-is-correct document next-paragraph cursor-position))))


(defmethod move-cursor-down (document (node common-doc:code-block) caret-position)
  (let* ((text (code node))
         (new-caret-position (reblocks-text-editor/utils/text::move-caret-on-the-next-line text caret-position)))
    (cond
      (new-caret-position
       (ops::ensure-cursor-position-is-correct document node new-caret-position))
      (t
       (let ((next-paragraph (ops::find-next-paragraph document node))
             (new-caret-position (reblocks-text-editor/utils/text::caret-position-from-beginning-of-the-line text caret-position)))
         ;; TODO: solve problem of moving cursor inside code blocks and other
         ;; blocks rendered in multiple lines:
         (when next-paragraph
           (ops::ensure-cursor-position-is-correct document next-paragraph new-caret-position)))))))


(defun insert-node (document new-node relative-to &key (position :after))
  (ops::add-reference-ids document
                          :to-node new-node)
  (ops::insert-node document
                    new-node
                    :position position
                    :relative-to relative-to)

  (let* ((last-child (lastcar (common-doc:children new-node)))
         (last-child-len (length (to-markdown last-child))))
    (ops::ensure-cursor-position-is-correct document last-child last-child-len)))


(defun make-node-from-pasted-text (text-before pasted-text)
  (cond
    ((and (or (str:starts-with-p "http://" pasted-text)
              (str:starts-with-p "https://" pasted-text))
          ;; We don't want to expand link when inserting
          ;; url after the text like "[some]("
          (not (cl-ppcre:all-matches "\\[[^]]*\\]\\($" text-before)))
     (multiple-value-bind (title content-type)
         (retrieve-url-info pasted-text)
       (let* ((url pasted-text)
              (title (or title url)))
         (cond
           ((and content-type
                 (str:starts-with-p "image/"
                                    content-type))
            (common-doc:make-image pasted-text))
           (t
            (common-doc:make-web-link
             url
             (list (common-doc:make-text title))))))))
    (t
     (let ((result (reblocks-text-editor/document/ops::parse-scriba-nodes
                    (common-doc:make-text pasted-text))))
       (common-doc:make-content result)))))


(defgeneric paste-text (document path-or-node caret-position pasted-text)
  (:method (document path-or-node caret-position pasted-text)))


(defmethod paste-text (document (path list) caret-position pasted-text)
  (log:debug "User wants to paste this text \"~A\"" pasted-text)

  (let* ((node
           (reblocks-text-editor/document/ops::find-changed-node document path)))
    (paste-text document node caret-position pasted-text)))


(defmethod paste-text (document (node common-doc:paragraph) cursor-position pasted-text)
  (let* ((text-before (text-before-caret document)
                      ;; (to-markdown (common-doc:make-content nodes-before)
                      ;;              ;; It is important to not trim a space,
                      ;;              ;; otherwise a space before the cursor will be lost:
                      ;;              :trim-spaces nil)
                      )
         ;; Тут надо сначала поправить позицию курсора, потому что
         ;; картинки которые перед ним, будут отрендерены в markdown как ![](someurl),
         ;; а cursor-position прилетел с фронта, где под картинку выделяется 1 символ:
         ;; Может сделать сначала сплит всех нод, а потом левую часть уже преобразовывать?
         ;; (text-before (slice text 0 cursor-position))
         (new-content (ensure-markup-nodes
                       (make-node-from-pasted-text text-before pasted-text)))
         ;; because nodes-before might include images which when rendered
         ;; in markdown are bigger than 1 character
         ;; (new-cursor-position (length text-before))
         )
    (ops::insert-into-paragraph document node
                                cursor-position
                                new-content)))


(defmethod paste-text (document (node common-doc:code-block) caret-position pasted-text)
  (let* ((text (reblocks-text-editor/blocks/code::code node))
         (text-before (slice text 0 caret-position))
         (text-after (slice text caret-position))
         (new-content (concatenate 'string text-before pasted-text text-after))
         (new-caret-position (+ caret-position
                                (length pasted-text)))
         (piece (make-scribdown-piece new-content new-caret-position)))
    (multiple-value-bind (updated-node final-caret-position)
        (ops::update-node-content document node
                                  piece)
      (ops::ensure-cursor-position-is-correct document
                                              updated-node
                                              final-caret-position))))


(defgeneric maybe-delete-block (document path-or-node)
  (:documentation "Called when cursor position is at the beginning of the document block and user hit backspace.")
  (:method ((document t) (path-or-node t))
    ;; Just ignore any node which has no a special processing.
    ))


(defmethod maybe-delete-block (document (path list))
  (log:debug "User wants to delete block at" path)

  (let* ((node
           (reblocks-text-editor/document/ops::find-changed-node document path)))
    (maybe-delete-block document node)))


(defmethod maybe-delete-block (document (node common-doc:code-block))
  (let ((text (trim-spaces
               (code node))))
    (when (string= text "")
      (let ((prev-node
              (ops::find-previous-sibling document
                                          node)))
        (ops::delete-node document node)
        (when prev-node
          (ops::ensure-cursor-position-is-correct document
                                                  prev-node
                                                  0
                                                  :from-the-end t))))))

(defgeneric start-new-paragraph (document path-or-node)
  (:documentation "Creates a new paragraph right after the current node and moves caret into it.")
  (:method ((document t) (path-or-node t))))


(defmethod start-new-paragraph (document (path list))
  (log:debug "Creating a new paragraph after the node at" path)

  (let* ((node
           (reblocks-text-editor/document/ops::find-changed-node document path)))
    (start-new-paragraph document node)))


(defmethod start-new-paragraph (document (node common-doc:document-node))
  (let ((paragraph (common-doc:make-paragraph
                    (common-doc:make-text +zero-width-space+))))
    (ops::add-reference-ids document :to-node paragraph)
    (ops::insert-node document paragraph
                      :relative-to node)
    (ops::ensure-cursor-position-is-correct document
                                            (first (common-doc:children paragraph))
                                            0)))


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


(defun update-progress (document &key progress-id percent &allow-other-keys)
  (log:debug "Updating progress" progress-id percent)
  (let ((progress-node (ops::find-node-by-reference document progress-id)))
    (cond
      (progress-node
       (setf (reblocks-text-editor/blocks/progress::percent progress-node)
             percent)
       (ops::replace-node document progress-node progress-node))
      (t
       (log:error "Unable to find PROGRESS node with reference ~A"
                  progress-id)))))


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
            ((string-equal message-type "update-progress")
             (apply #'update-progress document args))
            (t
             (log:error "Don't know how to process message of this type"
                        message-type
                        args))))))))


(defgeneric process-update (widget &key)
  (:method (widget &key change-type new-html path cursor-position pasted-text progress-id percent &allow-other-keys)
    (check-type cursor-position integer)

    (log:debug "Processing" new-html path cursor-position change-type)
    
    (let ((document (document widget)))
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
        ((string= change-type
                  "maybe-delete-block")
         (maybe-delete-block document path))
        ((string= change-type
                  "start-new-paragraph")
         (start-new-paragraph document path))
        ((string= change-type
                  "update-progress")
         (update-progress document progress-id percent))
        (t
         (process-usual-update document path new-html cursor-position)))

      (on-document-update widget))))


(defgeneric process-shortcut (widget &key key-code path cursor-position &allow-other-keys)
  (:method (widget &key key-code path cursor-position &allow-other-keys)
    (let ((document (document widget)))
      (log:debug "Key" key-code "pressed")

      (let ((paragraph (reblocks-text-editor/document/ops::find-changed-node document path)))
        (multiple-value-bind (node new-cursor-position)
            (ops::find-node-at-position paragraph
                                        cursor-position)
          (on-shortcut widget key-code node new-cursor-position))))))

(defgeneric process-link (widget &key href &allow-other-keys)
  (:method (widget &key href &allow-other-keys)
    (log:debug "Link" href "was clicked")))


(defmethod reblocks/widget:render ((widget editor))
  (let ((document (document widget)))
    (setf *document*
          document)
    (setf *widget*
          widget)

    ;; TODO: надо придумать, как сделать настраиваемым этот URL
    ;;       и директорию documents-root
    ;;       Может сделать протокол из (SUPPORTS-UPLOAD-P document)
    ;;       и (MEDIA-DIR document) и (MEDIA-URI document),
    ;;       а создание роута унести в инициализацию приложения Hypernot?
    (reblocks-file-server:make-route :uri "/images/"
                                     :root +documents-root+)
    
    (labels ((process-message (&rest args &key type &allow-other-keys)
               (let ((args (alexandria:remove-from-plist args :type)))
                 (apply #'process-message-from-frontend widget type args)))
             (upload-data (&rest args)
               (log:debug "Processing uploaded data" args)
               (let* ((request reblocks/request::*request*)
                      (filename (quri:url-decode (reblocks/request:get-header "x-file-name")))
                      (body (lack.request:request-raw-body request))
                      (destination-file (uiop:merge-pathnames* filename
                                                               +documents-root+))
                      (content-length (lack.request:request-content-length request)))
                 (alexandria:with-output-to-file (output destination-file
                                                         :element-type (stream-element-type body)
                                                         :if-exists :supersede)
                   (alexandria:copy-stream body output :end content-length))
                 (let* ((image-node (common-doc:make-image
                                     (format nil "/images/~A"
                                             (quri:url-encode filename))))
                        (progress-id (reblocks/request:get-header "x-progress-id"))
                        (progress-node (ops::find-node-by-reference document progress-id)))
                   (reblocks-text-editor/document/ops::add-reference-ids document
                                                                         :to-node image-node)
                   (ops::replace-node document
                                      progress-node
                                      image-node))
                 (log:debug "Data saved to" destination-file))))
      (let ((action-code (reblocks/actions:make-action #'process-message))
            (upload-code (reblocks/actions:make-action #'upload-data))
            ;; We need this flag to make sure our document will have
            ;; a visible markup on active paragraph.
            (reblocks-text-editor/html::*render-markup* t))
        (reblocks/html:with-html
          (:div :class "content"
                :ondragover "event.preventDefault(); return false"
                :ondrop "uploadFile(event); return false"
                :data-upload-code upload-code
                :data-action-code action-code
                :data-version (reblocks-text-editor/document/editable::content-version document)
                :contenteditable ""
                :onload "setup()"
                (reblocks-text-editor/html::to-html document)))))))


;; (defmethod reblocks/widget:render ((widget editor-demo))

;;   (labels ((reset-text (&rest args)
;;              (declare (ignore args))
;;              (setf (slot-value (editor-demo widget) 'document)
;;                    (make-initial-document))
;;              (reblocks/widget:update (editor-demo widget))))
    
;;     (reblocks/html:with-html
;;       (:h1 "Experimental HTML editor")
;;       (:h2 "Using Common Lisp + Reblocks")
;;       (reblocks/widget:render (editor-demo widget))
      
;;       (:p :id "debug"
;;           "Path will be shown here.")
      
;;       (:p (:button :onclick (reblocks/actions:make-js-action #'reset-text)
;;                    "Reset Text")))))


(defmethod reblocks/dependencies:get-dependencies ((widget editor))
  ;; https://keycode.info/
  (list* (reblocks-text-editor/frontend/js::make-js-code "/")
         (reblocks-text-editor/frontend/css::make-css-code)
         (call-next-method)))
