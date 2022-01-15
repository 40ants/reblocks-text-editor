(uiop:define-package #:zibaldone/widgets/editor
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-html)
  (:import-from #:reblocks-parenscript)
  (:import-from #:reblocks-lass)
  (:import-from #:parenscript
                #:create
                #:chain
                #:@))
(in-package #:zibaldone/widgets/editor)


(defun add-reference-ids (document &aux (next-id 1))
  (flet ((set-reference-id (node depth)
           (declare (ignore depth))
           (setf (common-doc:reference node)
                 (format nil "el~A" next-id))
           (incf next-id)
           (values)))
    (common-doc.ops:traverse-document document
                                      #'set-reference-id)
    document))


(defun make-span (text)
  ;; This is a hack because common-html
  ;; renders text nodes with metadata
  ;; as spans even if metadata is an empty hash table
  (common-doc:make-text text
                        :metadata (make-hash-table)))


(defun make-initial-document ()
  (add-reference-ids
   (common-doc:make-paragraph 
    (list 
     (make-span "Hello ")
     (common-doc:make-bold
      (common-doc:make-text
       "Lisp"))
     (make-span " World!")))))


(defun to-html (document)
  (common-html.emitter:node-to-html-string document))


(reblocks/widget:defwidget editor ()
  (;; (updated-content :type (or null string)
   ;;                  :initform nil
   ;;                  :accessor updated-content)
   (document :type common-doc:document-node
             :initform (make-initial-document)
             :reader document)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This is our BACKEND code doing most business logic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reblocks/widget:render ((widget editor))
  (flet ((process-update (&key new-html path cursor-position &allow-other-keys)
           (log:info "Processing" new-html path)
           (let* ((node-id (car (last path)))
                  (changed-node (find-node-by-reference (document widget)
                                                        node-id)))
             (cond
               (changed-node
                (let ((edited-node (typecase changed-node
                                     (common-doc:text-node changed-node)
                                     (t
                                      (first (common-doc:children changed-node))))))
                  (cond
                    ((typep edited-node 'common-doc:text-node)
                     (setf (common-doc:text edited-node)
                           (plump:decode-entities new-html)))
                    (t (log:error "Node is not TEXT-NODE" edited-node)))
                  
                  ;; (reblocks/commands:add-command 'just-a-check)
                  (reblocks/widget:update widget)
                  (reblocks/commands:add-command 'set-cursor
                                                 :node-id (common-doc:reference changed-node)
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
        (:pre :contenteditable ""
              :data-action-code action-code
              :onload "setup()"
              :oninput "updateEditor(event)"
              :onclick "showPath()"
              ;; :beforeinput "beforeInput(event)"

              (:raw (to-html (document widget))))

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
                          just-a-check)
                   just-a-check)

             (defun just-a-check (args)
               (chain console
                      (log "JUST A CHECK"))
               )

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
                 ;; (chain element
                 ;;        (focus))
                 (chain range
                        (set-start (@ element
                                      child-nodes
                                      0)
                                   position))
                 (chain range
                        (collapse t))
                 (chain sel
                        (remove-all-ranges))
                 (chain sel
                        (add-range range))
                 (chain console (log "Selection should be changed now to" sel))
                 )
               )
             
             (defun update-editor (event)
               (chain console
                      (log "Handling oninput event"))
               (chain console
                      (log event))
               (let* ((path (calculate-path))
                      (target (@ event target inner-h-t-m-l))
                      (edited-node-id (@ path
                                         (1- (@ path length))))
                      (edited-node (chain document
                                          (get-element-by-id edited-node-id)))
                      (text (@ edited-node inner-h-t-m-l))
                      (cursor-position (chain window
                                              (get-selection)
                                              anchor-offset))
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
                                     (@ parent id)
                                     (not (chain (@ parent id)
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
               (let ((path (calculate-path)))
                 (chain (j-query "#debug")
                        (html (chain -j-s-o-n
                                     (stringify path))))))

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
           '(.editor
             (.bold :font-weight bold)))
         (call-next-method)))
