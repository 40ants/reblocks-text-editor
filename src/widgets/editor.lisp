(uiop:define-package #:zibaldone/widgets/editor
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-html)
  (:import-from #:reblocks-parenscript)
  (:import-from #:reblocks-lass)
  (:import-from #:zibaldone/html)
  (:import-from #:parenscript
                #:create
                #:chain
                #:@)
  (:import-from #:bordeaux-threads
                #:make-lock))
(in-package #:zibaldone/widgets/editor)


(defparameter +zero-width-space+
  "​"
  "This string contains a character which corresponds to a &ZeroWidthSpace; HTML entity.")


(defun add-reference-ids (document &key (next-id 1))
  (flet ((set-reference-id (node depth)
           (declare (ignore depth))
           (setf (common-doc:reference node)
                 (format nil "el~A" next-id))
           (incf next-id)
           (values)))
    (common-doc.ops:traverse-document document
                                      #'set-reference-id)
    (values document
            next-id)))


(defun (setf next-id) (next-id document)
  (setf (gethash "next-id"
                 (common-doc:metadata document))
        next-id))

(defun next-id (document)
  (gethash "next-id"
           (common-doc:metadata document)))

(defun make-initial-document ()
  (let ((doc (common-doc:make-content
              (list (common-doc:make-paragraph 
                     (list 
                      (common-doc:make-text "Hello ")
                      (common-doc:make-bold
                       (common-doc:make-text
                        "Lisp"))
                      (common-doc:make-text " World!")))
                    
                    (common-doc:make-paragraph 
                     (list 
                      (common-doc:make-text "Second line"))))
              :metadata (make-hash-table :test 'equal))))
    (multiple-value-bind (doc next-id)
        (add-reference-ids doc)
      (setf (next-id doc)
            next-id)
      (values doc))))


;; (defun to-html (document)
;;   (common-html.emitter:node-to-html-string document))


(defun to-markdown (node)
  (let ((commondoc-markdown/emitter:*emit-section-anchors* nil)
        (zibaldone/html::*render-markup*))
    (common-doc.format:emit-to-string (make-instance 'commondoc-markdown:markdown)
                                      node)))


(defun from-markdown (text)
  (let ((node (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                                text)))
    ;; This is a workaround for a bug inside commondoc-markdown or 3bmd
    ;; When it parses an empty text, it returns an empty CONTENT-NODE,
    ;; But we need a PARAGRAPH.
    (cond
      ((and (typep node 'common-doc:content-node)
            (null (common-doc:children node)))
       (common-doc:make-paragraph
        (list
         ;; Without this hack with &ZeroWidthSpace;
         ;; we'll be unable to put cursor into a new paragraph :(
         (common-doc:make-text +zero-width-space+))))
      (t node))))


(reblocks/widget:defwidget editor ()
  ((document :type common-doc:document-node
             :initform (make-initial-document)
             :reader document)
   (lock :initform (make-lock "Editor Update Lock")
         :reader editor-lock)
   (version :initform 0
            :accessor content-version)))


(defgeneric map-document (node function &optional depth)
  (:documentation "Map a function recursively (depth-first),
                   possibly replacing nodes with ones a FUNCTION will return.

                   Warning, this function may modify the original nodes tree!

                   The function should return the same node or the new one.
                   If a new node was returned, the function will not
                   be applied to its content.")

  (:method ((doc common-doc:document) function &optional (depth 0))
    (setf (common-doc:children doc)
          (loop for child in (common-doc:children doc)
                unless (zibaldone/html::markup-p child)
                  collect (map-document child function (1+ depth))))
    (values doc))

  (:method ((cnode common-doc:content-node) function &optional (depth 0))
    (let ((possibly-new-node (funcall function cnode depth)))
      (when (eql possibly-new-node cnode)
        (setf (common-doc:children cnode)
              (loop for child in (common-doc:children cnode)
                    unless (zibaldone/html::markup-p child)
                      collect (map-document child function (1+ depth)))))
      (values possibly-new-node)))

  (:method ((dnode common-doc:document-node) function &optional (depth 0))
    (funcall function dnode depth)))


(defun find-node-by-reference (document reference)
  (flet ((search-node (node depth)
           (declare (ignore depth))
           (when (and (common-doc:reference node)
                      (string= (common-doc:reference node)
                               reference))
             (return-from find-node-by-reference node))))
    (common-doc.ops:traverse-document document #'search-node)
    ;; If not found, return nothing
    (values)))


(defun replace-node (document node-to-replace new-node)
  (flet ((do-replace (node depth)
           (declare (ignore depth))
           (if (eql node node-to-replace)
               new-node
               node)))
    (map-document document #'do-replace)))

(defun replace-node-content (document node-to-replace new-children)
  (flet ((do-replace (node depth)
           (declare (ignore depth))
           (when (eql node node-to-replace)
             (unless (typep node 'common-doc:content-node)
               (error "Unable to replace content for node ~A" node))
             (setf (common-doc:children node)
                   new-children))
           node))
    (map-document document #'do-replace)))


(defun find-node-at-position (node cursor-position &aux
                                                     last-visited-node
                                                     last-visited-node-content-length)
  (labels ((recursive-find (node)
             (setf last-visited-node
                   node)
             
             (etypecase node
               ((or common-doc:text-node
                    commondoc-markdown/raw-html:raw-inline-html)
                (let* ((text (typecase node
                               (commondoc-markdown/raw-html:raw-inline-html
                                (commondoc-markdown/raw-html:html node))
                               (common-doc:text-node
                                (common-doc:text node))))
                       (content-length
                         (length text)))
                  (setf last-visited-node-content-length
                        content-length)
                  
                  (if (<= cursor-position
                          content-length)
                      (return-from find-node-at-position
                        (values node
                                cursor-position))
                      (decf cursor-position
                            content-length))))
               (common-doc:content-node
                ;; (setf last-visited-node-content-length
                ;;       (1+
                ;;        (* (markup-length node)
                ;;           2)))
                
                ;; (unless (zerop (markup-length node))
                ;;   (log:info "Skipping" (markup-length node) "for" node)
                ;;   (decf cursor-position
                ;;         (markup-length node)))

                (cond
                  ((common-doc:children node)
                   (mapc #'recursive-find
                         (common-doc:children node)))

                  ;; The case, when cursor points to the empty
                  ;; node, like a new paragraph with no content:
                  ((zerop cursor-position)
                   (return-from find-node-at-position
                     (values node
                             cursor-position))))
                
                ;; (unless (zerop (markup-length node))
                ;;   (log:info "Skipping" (markup-length node) "for" node)
                ;;   (decf cursor-position
                ;;         (markup-length node)))
                )))
           ;; (markup-length (node)
           ;;   (typecase node
           ;;     (common-doc:bold 2)
           ;;     (common-doc:italic 1)
           ;;     (t 0)))
           )
    
    (recursive-find node)
    (values last-visited-node
            last-visited-node-content-length)))


(defun remove-html-tags (html-string)
  (let* ((result (cl-ppcre:regex-replace-all "<[^>]+>" html-string
                                             "")))
    (if (string= result +zero-width-space+)
        result
        (cl-ppcre:regex-replace-all +zero-width-space+
                                    result
                                    ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This is our BACKEND code doing most business logic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *document* nil)
(defvar *widget* nil)


(defun prepare-new-content (widget text)
  (let ((paragraph (from-markdown text)))
    (multiple-value-bind (paragraph next-id)
        (add-reference-ids paragraph :next-id (next-id (document widget)))
      (setf (next-id (document widget))
            next-id)
      (values paragraph))))


(defun find-changed-node (widget path)
  (let* ((node-id (car (last path)))
         (node (find-node-by-reference (document widget)
                                       node-id)))
    (unless node
      (log:error "Unable to find CommonDoc node with" node-id))
    (unless (typep node 'common-doc:paragraph)
      (log:warn "Changed node should be a whole PARAGRAPH."))

    (values node)))


(defun ensure-cursor-position-is-correct (changed-node cursor-position)
  ;; We need to move cursor because in HTML cursor
  ;; position is relative to the most inner element
  ;; and we might introduce some markup elements during
  ;; PREPARE-NEW-CONTENT phase.
  (multiple-value-bind (node new-cursor-position)
      (find-node-at-position changed-node
                             cursor-position)
    (cond
      (node
       (reblocks/commands:add-command 'set-cursor
                                      :node-id (common-doc:reference node)
                                      ;; We should figure out how to pass this from the frontend first
                                      :position new-cursor-position))
      (t
       (log:error "Unable to find node for"
                  cursor-position
                  (zibaldone/html::to-html-string changed-node))))))

(defun update-paragraph-content (widget paragraph plain-text)
  ;; Here we are updating our document tree
  (let ((new-content (prepare-new-content widget plain-text)))
    (replace-node-content (document widget)
                          paragraph
                          (common-doc:children new-content))

    (reblocks/commands:add-command 'update-text
                                   :version (content-version widget)
                                   :replace-node-id (common-doc:reference paragraph)
                                   :with-html (zibaldone/html::to-html-string
                                               paragraph))))

(defun create-new-paragraph (widget markdown-text)
  (prepare-new-content widget markdown-text))


(defgeneric insert-node (container node &key after)
  (:documentation "Inserts one node after another."))

(defgeneric delete-node (container node)
  (:documentation "Deletes a node from container"))


(defmethod insert-node ((widget reblocks/widget:widget) node &key (after (alexandria:required-argument)))
  (insert-node (document widget) node :after after)
  (reblocks/commands:add-command 'insert-node
                                 :version (content-version widget)
                                 :after-node-id (common-doc:reference after)
                                 :html (zibaldone/html::to-html-string node))
  (values))

(defmethod insert-node ((document common-doc:document-node) node &key (after (alexandria:required-argument)))
  (flet ((find-and-insert (current-node depth)
           (declare (ignore depth))
           (when (typep current-node 'common-doc:content-node)
             (let ((found-pos (position after
                                        (common-doc:children current-node))))
               (when found-pos
                 (push node
                       (cdr (nthcdr found-pos
                                    (common-doc:children current-node)))))))))
    (map-document document #'find-and-insert))
  (values))


(defmethod delete-node ((widget reblocks/widget:widget) node)
  (delete-node (document widget) node)
  (reblocks/commands:add-command 'delete-node
                                 :version (content-version widget)
                                 :node-id (common-doc:reference node))
  (values))

(defmethod delete-node ((document common-doc:document-node) node)
  (flet ((find-and-delete (current-node depth)
           (declare (ignore depth))
           (when (typep current-node 'common-doc:content-node)
             (setf (common-doc:children current-node)
                   (remove node (common-doc:children current-node))))))
    (map-document document #'find-and-delete))
  (values))


(defun find-previous-sibling (document node)
  (flet ((find-node (current-node depth)
           (declare (ignore depth))
           (when (typep current-node 'common-doc:content-node)
             (let ((found-pos (position node
                                        (common-doc:children current-node))))
               (when (and found-pos
                          (not (zerop found-pos)))
                 (return-from find-previous-sibling
                   (nth (1- found-pos)
                        (common-doc:children current-node))))))))
    (map-document document #'find-node)
    (values)))


(defun process-usual-update (widget path new-html cursor-position)
  (let* ((paragraph (find-changed-node widget path))
         (plain-text (remove-html-tags new-html)))
    (when paragraph
      (update-paragraph-content widget paragraph plain-text)
      (ensure-cursor-position-is-correct paragraph
                                         cursor-position))))


(defun insert-paragraph (widget path new-html cursor-position)
  (let ((changed-paragraph (find-changed-node widget path)))
    (when changed-paragraph
      (let* ((plain-text (remove-html-tags new-html))
             (text-before-cursor (subseq plain-text 0 cursor-position))
             (text-after-cursor (subseq plain-text cursor-position))
             (new-paragraph (create-new-paragraph widget text-after-cursor)))
        (update-paragraph-content widget changed-paragraph text-before-cursor)
        (insert-node widget
                     new-paragraph
                     :after changed-paragraph)
        (ensure-cursor-position-is-correct new-paragraph
                                           ;; When newline is inserted
                                           ;; the cursor will be at the beginning
                                           0)))))


(defun join-with-prev-paragraph (widget path new-html cursor-position)
  (let ((paragraph-to-delete (find-changed-node widget path))
        (text-to-append (remove-html-tags new-html)))
    (log:error "Joining paragraph" path new-html cursor-position paragraph-to-delete)
    (when paragraph-to-delete
      (let* ((previous-paragraph (find-previous-sibling (document widget)
                                                        paragraph-to-delete)))
        (when previous-paragraph
          (let* ((first-part (string-trim '(#\Newline #\Space #\Tab)
                                          (to-markdown previous-paragraph)))
                 (full-text (concatenate 'string
                                         first-part
                                         text-to-append)))
            (update-paragraph-content widget previous-paragraph full-text)
            (delete-node widget
                         paragraph-to-delete)
            (ensure-cursor-position-is-correct previous-paragraph
                                               ;; The cursor now should be
                                               ;; somewhere in the middle of the new
                                               ;; paragraph. Right at the end of the
                                               ;; paragraph, we've joined our current one:
                                               (length first-part))))))))


(defmethod reblocks/widget:render ((widget editor))
  (setf *document*
        (document widget))
  (setf *widget*
        widget)
  
  (labels ((process-update (&key change-type version new-html path cursor-position &allow-other-keys)
             (bordeaux-threads:with-lock-held ((editor-lock widget))
               (when (> version (content-version widget))
                 (log:error "Processing" new-html path cursor-position version change-type)
                 
                 (setf (content-version widget)
                       version)

                 (cond
                   ((string= change-type
                             "insert-paragraph")
                    (insert-paragraph widget path new-html cursor-position))
                   ((string= change-type
                             "join-with-prev-paragraph")
                    (join-with-prev-paragraph widget path new-html cursor-position))
                   (t
                    (process-usual-update widget path new-html cursor-position))))))
           
           (reset-text (&rest args)
             (declare (ignore args))
             (bordeaux-threads:with-lock-held ((editor-lock widget))
               (setf (slot-value widget 'document)
                     (make-initial-document)
                     (content-version widget)
                     0)
               (reblocks/widget:update widget))))
     
    (let ((action-code (reblocks/actions:make-action #'process-update)))
      (reblocks/html:with-html
        (:h1 "Experimental HTML editor")
        (:h2 "Using Common Lisp + Reblocks")
        (:div :class "content"
              :data-action-code action-code
              :data-version (content-version widget)
              :contenteditable ""
              :onload "setup()"
              (zibaldone/html::to-html (document widget)))

        (:p :id "debug"
            "Path will be shown here.")

        (:p (:button :onclick (reblocks/actions:make-js-action #'reset-text)
                     "Reset Text"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This is our FRONTEND PART                              ;;
;; Code from this method will be translated to JavaScript ;;
;; using Parenscrip system                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reblocks/dependencies:get-dependencies ((widget editor))
  (list* (reblocks-parenscript:make-dependency
           (progn
             (chain (j-query document)
                    (ready (lambda ()
                             (chain (j-query ".editor")
                                    (each setup)))))

             (setf (chain window
                          command-handlers
                          set-cursor)
                   set-cursor)
             (setf (chain window
                          command-handlers
                          update-text)
                   update-text)
             (setf (chain window
                          command-handlers
                          insert-node)
                   insert-node)
             (setf (chain window
                          command-handlers
                          delete-node)
                   delete-node)

             (defun from-html (string)
               (let* ((parser (ps:new -d-o-m-parser))
                      (doc (chain parser
                                  (parse-from-string string "text/html"))))
                 (@ doc
                    body
                    first-child)))

             (defun update-text (args)
               (let* ((version (@ args version))
                      (node-id (@ args replace-node-id))
                      (node (chain document
                                   (get-element-by-id node-id)))
                      (editor (@ node parent-node parent-node))
                      (current-version (@ editor dataset version)))
                 
                 (unless (< version current-version)
                   (let* ((html-string (@ args with-html))
                          (html (from-html html-string)))
                     (chain node
                            (replace-with html))))))
             
             (defun insert-node (args)
               (let* ((version (@ args version))
                      (after-node-id (@ args after-node-id))
                      (after-node (chain document
                                         (get-element-by-id after-node-id)))
                      (editor (@ after-node parent-node parent-node))
                      (current-version (@ editor dataset version)))
                 
                 (unless (< version current-version)
                   (let* ((html-string (@ args html))
                          (html (from-html html-string)))
                     (chain console
                            (log "INSERTING " html-string))
                     (chain after-node
                            (insert-adjacent-h-t-m-l "afterend" html-string))))))
             
             (defun delete-node (args)
               (let* ((version (@ args version))
                      (node-id (@ args node-id))
                      (node (chain document
                                   (get-element-by-id node-id)))
                      (editor (@ node parent-node parent-node))
                      (current-version (@ editor dataset version)))
                 
                 (unless (< version current-version)
                   (let* ((html-string (@ args html))
                          (html (from-html html-string)))
                     (chain console
                            (log "Deleting node" node-id))
                     (chain node
                            (remove))))))
             
             (defun set-cursor (args)
               (let* ((element-id (@ args node-id))
                      (position (@ args position))
                      (element
                        (chain document
                               (get-element-by-id element-id)))
                      (range (chain document (create-range)))
                      (sel (chain window (get-selection))))

                 (cond
                   (element
                    (chain range
                           (set-start
                            (@ element
                               child-nodes
                               0)
                            position))
                    (chain range
                           (collapse t))
                    (chain sel
                           (remove-all-ranges))
                    (chain sel
                           (add-range range)))
                   (t
                    (chain console (log "Unable to find element to place cursor to" element-id))))

                 (update-active-paragraph)))

             (defun take (n arr)
               (loop for item in arr
                     for i from 1 to n
                     collect item))

             (defun trim-path-to-nearest-paragraph (path)
               (loop for idx = (1- (@ path length))
                       then (- idx 1)
                     for id = (aref path idx)
                     for el = (chain document
                                     (get-element-by-id id))
                     when (= (@ el tag-name)
                             "P")
                       do (return (take (1+ idx) path))))
             
             (defun change-text (event change-type)
               (let ((current-version
                       (incf (@ event target dataset version))))

                 (let* ((path (trim-path-to-nearest-paragraph
                               (calculate-path)))
                        (target (@ event target inner-h-t-m-l))
                        (edited-node-id (@ path
                                           (1- (@ path length))))
                        (edited-node
                          (chain document
                                 (get-element-by-id edited-node-id)))
                        (text (@ edited-node inner-h-t-m-l))
                        (cursor-position (caret-position))
                        (args (create
                               :change-type change-type
                               :new-html text
                               :path path
                               :cursor-position cursor-position
                               :version current-version)))

                   ;; Before we send an action, we need to remember which
                   ;; element was edited, to restore cursor position
                   ;; after the widget will be updated.
                   ;;
                   ;; Or may be we might initiate update from the server-side?
                   (initiate-action (@ event target dataset action-code)
                                    (create :args args)))))
             
             (defun go-up-to (tag-name starting-node)
               (loop for node = starting-node
                       then (@ node parent-node)
                     when (= (@ node tag-name)
                             tag-name)
                       do (return node)))
             
             (defun get-editor-node (starting-node)
               (loop for node = starting-node
                       then (@ node parent-node)
                     when (chain node
                                 class-list
                                 (contains "editor"))
                       do (return node)))


             (defun caret-position ()
               ;; Idea was taken from
               ;; https://github.com/accursoft/caret/blob/922257adae80c529c237deaddc49f65d7c794534/jquery.caret.js#L17-L29
               (let* ((selection (chain window
                                        (get-selection)))
                      (node (@ selection
                               base-node))
                      (paragraph (go-up-to "P" node))
                      (range-1 (chain selection
                                      (get-range-at 0)))
                      (range-2 (chain range-1
                                      (clone-range))))
                 (chain console
                        (log "Current selection"
                             selection))
                 
                 (chain range-2
                        (select-node-contents paragraph))
                 (chain range-2
                        (set-end (@ range-1 end-container)
                                 (@ range-1 end-offset)))
                 (chain range-2
                        (to-string)
                        length)))
             
             (defun calculate-path ()
               "Returns a #ids of the currently selected node and all its parents,
                starting from the most outer parent.

                We stop collecting parents on a parent with id starting from \"dom\",
                because such id denotes an outer HTML element of the editor widget."
               (labels ((make-path (node)
                          (let* ((id (@ node id))
                                 (path (if id
                                           (list id)
                                           (list)))
                                 (parent (@ node parent-node)))
                            (if (and parent
                                     (not (chain (or (@ parent id)
                                                     "")
                                                 (starts-with "dom"))))
                                (append (make-path parent)
                                        path)
                                path))))
                 (let* ((selection (chain window
                                          (get-selection)))
                        (node (@ selection
                                 base-node))
                        (path (make-path node)))
                   path)))
             
             (defun show-path ()
               (let ((path (calculate-path))
                     (position (caret-position)))
                 (chain (j-query "#debug")
                        (html (chain -j-s-o-n
                                     (stringify
                                      (create :path path
                                              :caret position)))))))

             (defvar +prev-current-node+ nil)
             
             (defun update-active-paragraph ()
               (let* ((selection (chain window
                                        (get-selection)))
                      (node (@ selection
                               base-node)))
                 (unless (eql +prev-current-node+
                              node)
                   (let* ((current-paragraph (go-up-to "P" node))
                          (editor (get-editor-node current-paragraph))
                          (all-paragraphs (chain editor
                                                 (get-elements-by-tag-name "P"))))
                     (loop for p in all-paragraphs
                           do (chain p
                                     class-list
                                     (remove "active")))
                     (chain current-paragraph
                            class-list
                            (add "active"))))))

             (defun on-caret-change ()
               (show-path)
               (update-active-paragraph))
             
             (defun on-keydown ()
               (update-active-paragraph))
             
             (defun setup ()
               (chain this
                      (add-event-listener "click"
                                          on-caret-change))
               (chain this
                      (add-event-listener "beforeinput"
                                          before-input)) 
               (chain this
                      (add-event-listener "input"
                                          on-editor-input))
               (chain this
                      (add-event-listener "keydown"
                                          on-keydown)))

             (defun on-editor-input (event)
               ;; (chain console
               ;;        (log "Handling oninput event" event current-version))
               (change-text event "modify"))
             
             (defun before-input (event)
               (let ((type (@ event
                              input-type)))
                 (chain console
                        (log "Before input" event))
                 
                 (when (= type "insertParagraph")
                   (change-text event "insert-paragraph")
                   
                   (chain event
                          (prevent-default)))

                 (when (and (= type "deleteContentBackward")
                            (= (caret-position)
                               0))
                   (change-text event "join-with-prev-paragraph")
                   
                   (chain event
                          (prevent-default)))))))
         
         (reblocks-lass:make-dependency
           '(body
             (.editor
              (.content :outline none)
              (.content
               (p
                :white-space pre-wrap))
              (.bold :font-weight bold)
              (.markup :display none)
              ((:and p .active)
               (.markup :display inline-block)))))
         (call-next-method)))
