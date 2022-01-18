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
                #:@))
(in-package #:zibaldone/widgets/editor)


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


;; (defclass markup-node (common-doc:content-node)
;;   ())


;; (defmethod zibaldone/html::to-html ((node markup-node))
;;   (reblocks/html:with-html
;;     ;; TODO: add a check that there is no nodes prohibited as a span container  
;;     (:span :id (common-doc:reference node)
;;            (mapc #'zibaldone/html::to-html
;;                  (common-doc:children node)))))


;; (defun make-span (content)
;;   ;; This is a hack because common-html
;;   ;; renders text nodes with metadata
;;   ;; as spans even if metadata is an empty hash table
;;   (let ((content (typecase content
;;                    (string (common-doc:make-text content))
;;                    (t content))))
;;     (make-instance 'markup-node
;;                    :children (uiop:ensure-list content))))


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


(defun to-html (document)
  (common-html.emitter:node-to-html-string document))


(defun from-markdown (text)
  (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                    text))

(defun has-markup-p (paragraph)
  (> (length (common-doc:children paragraph))
     1))


(reblocks/widget:defwidget editor ()
  (;; (updated-content :type (or null string)
   ;;                  :initform nil
   ;;                  :accessor updated-content)
   (document :type common-doc:document-node
             :initform (make-initial-document)
             :reader document)))


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


(defun find-node-at-position (node cursor-position &aux
                                                     last-visited-node
                                                     last-visited-node-content-length)
  (labels ((recursive-find (node)
             (setf last-visited-node
                   node)
             
             (etypecase node
               (common-doc:text-node
                (let* ((text (common-doc:text node))
                       (content-length
                         (length text)))
                  (setf last-visited-node-content-length
                        content-length)
                  
                  (if (<= cursor-position
                          content-length)
                      (return-from find-node-at-position
                        (values node
                                cursor-position))
                      (progn (log:info "Skipping" text content-length)
                             (decf cursor-position
                                   content-length)))))
               (common-doc:content-node
                ;; (setf last-visited-node-content-length
                ;;       (1+
                ;;        (* (markup-length node)
                ;;           2)))
                
                ;; (unless (zerop (markup-length node))
                ;;   (log:info "Skipping" (markup-length node) "for" node)
                ;;   (decf cursor-position
                ;;         (markup-length node)))
                
                (mapc #'recursive-find
                      (common-doc:children node))
                
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
  (cl-ppcre:regex-replace-all "<[^>]+>" html-string
                              ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This is our BACKEND code doing most business logic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *document* nil)

(defmethod reblocks/widget:render ((widget editor))
  (setf *document*
        (document widget))
  
  (labels ((prepare-new-content (text)
             (let ((paragraph (from-markdown text)))
               (multiple-value-bind (paragraph next-id)
                   (add-reference-ids paragraph :next-id (next-id (document widget)))
                 (setf (next-id (document widget))
                       next-id)
                 (values paragraph))
               
               ;; (cond
               ;;   ((has-markup-p paragraph)
               ;;    ;; Here we also need to assign
               ;;    ;; a new references to our new nodes.
               ;;    ;; but to do this, we need to store a maximum
               ;;    ;; ID already used.
               ;;    (let ((subtree (make-span
               ;;                    (common-doc:children paragraph))))
               ;;      (multiple-value-bind (subtree next-id)
               ;;          (add-reference-ids subtree :next-id (next-id (document widget)))
               ;;        (setf (next-id (document widget))
               ;;              next-id)
               ;;        (values subtree))))
               ;;   (t
               ;;    text))
               ))
           (process-update (&key new-html path cursor-position &allow-other-keys)
             (log:warn "Processing" new-html path cursor-position)
             (let* ((node-id (car (last path)))
                    (changed-node (find-node-by-reference (document widget)
                                                          node-id))
                    (plain-text (remove-html-tags new-html))
                    ;; Otherwise, we need to replace the text node's
                    ;; content with a plain text
                    (new-content (prepare-new-content plain-text)))
               (cond
                 (changed-node
                  (let ((cursor-node changed-node))
                    (typecase new-content
                      ;; If no markup was introduced, then we need
                      ;; to replace a text content of the edited node.
                      (string
                       (log:warn "Replacing content inside node"
                                 changed-node)
                       (let ((edited-node
                               (typecase changed-node
                                 (common-doc:text-node changed-node)
                                 (t
                                  (first (common-doc:children changed-node))))))
                         (cond
                           ((typep edited-node 'common-doc:text-node)
                            (setf (common-doc:text edited-node)
                                  new-content))
                           (t (log:error "Node is not TEXT-NODE" edited-node)))))
                      ;; Otherwise, we need to replace the whole edited node
                      ;; with a new CommonDoc element containing markup
                      (t
                       (log:warn "Replacing usual node"
                                 changed-node)
                       (replace-node (document widget)
                                     changed-node
                                     new-content)
                       ;; We need to move cursor into our new node,
                       ;; because CHNAGED-NODE will be removed from the DOM
                       ;; after the widget's update:
                       (multiple-value-bind (node new-cursor-position)
                           (find-node-at-position new-content cursor-position)
                         (unless node
                           (log:error "Unable to find node for"
                                      cursor-position
                                      plain-text))
                         (setf cursor-node
                               node
                               cursor-position
                               new-cursor-position))))

                    (reblocks/widget:update widget)
                    (reblocks/commands:add-command 'set-cursor
                                                   :node-id (common-doc:reference cursor-node)
                                                   ;; We should figure out how to pass this from the frontend first
                                                   :position cursor-position)))
                 (t
                  (log:error "Unable to find CommonDoc node with" node-id)))))
           (reset-text (&rest args)
             (declare (ignore args))
             (setf (slot-value widget 'document)
                   (make-initial-document))
             (reblocks/widget:update widget)))
    
    (let ((action-code (reblocks/actions:make-action #'process-update)))
      (reblocks/html:with-html
        (:h1 "Making HTML editor with Reblocks and Common Lisp")
        (:div :class "content"
              :data-action-code action-code
              :contenteditable ""
              :onload "setup()"
              :oninput "updateEditor(event)"
              :onclick "showPath()"
              ;; :beforeinput "beforeInput(event)"

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
             
             (defun set-cursor (args)
               (chain console
                      (log "Here we should set a cursor back into the element" args))
               (let* ((element-id (@ args node-id))
                      (position (@ args position))
                      (element ;; element-id
                        (chain document
                               (get-element-by-id element-id)))
                      (range (chain document (create-range)))
                      (sel (chain window (get-selection)))
                      )
                 
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
                        (add-range range))
                 (chain console (log "Selection should be changed now to" sel))))

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
             
             (defun update-editor (event)
               (chain console
                      (log "Handling oninput event"))
               (chain console
                      (log event))
               (let* ((path (trim-path-to-nearest-paragraph
                             (calculate-path)))
                      (target (@ event target inner-h-t-m-l))
                      (edited-node-id (@ path
                                         (1- (@ path length))))
                      (edited-node ;; (find-nearest-paragraph path)
                        (chain document
                               (get-element-by-id edited-node-id)))
                      (text (@ edited-node inner-h-t-m-l))
                      (cursor-position ;; (chain window
                                       ;;        (get-selection)
                                       ;;        anchor-offset)
                                       (caret-position))
                      (args (create
                             :new-html text
                             :path path
                             :cursor-position cursor-position)))
                 (chain console
                        (log edited-node))

                 ;; Before we send an action, we need to remember which
                 ;; element was edited, to restore cursor position
                 ;; after the widget will be updated.
                 ;;
                 ;; Or may be we might initiate update from the server-side?
                 (initiate-action (@ event target dataset action-code)
                                  (create :args args))))


             (defun go-up-to (tag-name starting-node)
               (loop for node = starting-node
                       then (@ node parent-node)
                     when (= (@ node tag-name)
                             tag-name)
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
                   (chain console
                          (log selection))
                   path)))
             
             (defun show-path ()
               (let ((path (calculate-path))
                     (position (caret-position)))
                 (chain (j-query "#debug")
                        (html (chain -j-s-o-n
                                     (stringify
                                      (create :path path
                                              :caret position)))))))

             (defun setup ()
               (chain console
                      (log "Setting up the editor"))
               (chain this
                      (add-event-listener "beforeinput"
                                          before-input)))
             (defun before-input (event)
               ;; (save-selection)

               (chain console
                      (log "Handling before-input event"))
               
               ;; (chain console
               ;;        (log event))

               ;; (chain console
               ;;        (log (chain event
               ;;                    (get-target-ranges))))

               ;; (chain console
               ;;        (log (chain event
               ;;                    (get-target-ranges)
               ;;                    0
               ;;                    start-container)))
               
               ;; (chain event
               ;;        (prevent-default))
               )))
         (reblocks-lass:make-dependency
           '(body
             (.editor
              (.content :white-space pre-wrap)
              (.bold :font-weight bold)
              ;; (.markup :display none)
              )

             ;; (.editor
             ;;  (:focus
             ;;   :outline none
             ;;   (.markup :display inline-block)))
             
             ;; (.editor
             ;;  ((:and p :focus-within)
             ;;   :background red))

             ;; ((:and .editor :focus-within)
             ;;  :background yellow)

             ))
         (call-next-method)))
