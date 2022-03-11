(uiop:define-package #:reblocks-text-editor/frontend/js
  (:use #:cl)
  (:import-from #:reblocks-parenscript)
  (:import-from #:parenscript
                #:regex
                #:chain
                #:@
                #:create))
(in-package #:reblocks-text-editor/frontend/js)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; This is our FRONTEND PART                              ;;
;; Code from this method will be translated to JavaScript ;;
;; using Parenscript system                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-js-code (shortcut)
  (reblocks-parenscript:make-dependency*
   `(progn
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
                   replace-node)
            replace-node)
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

      (defun replace-node (args)
        (let* ((version (@ args version))
               (node-id (@ args node-id))
               (node (chain document
                            (get-element-by-id node-id)))
               (editor (get-editor-content-node node))
               (current-version (@ editor dataset version)))

          (unless (< version current-version)
            (let* ((html-string (@ args with-html))
                   (html (from-html html-string)))
              (chain console
                     (log "Replacing" node "with" html))
              (chain node
                     (replace-with html))))))
      
      (defun insert-node (args)
        (let* ((version (@ args version))
               (relative-to-node-id (@ args relative-to-node-id))
               (position (or (@ args position)
                             "afterend"))
               (after-node (chain document
                                  (get-element-by-id relative-to-node-id)))
               (editor (get-editor-content-node after-node))
               (current-version (@ editor dataset version)))
          
          (unless (< version current-version)
            (let* ((html-string (@ args html))
                   (html (from-html html-string)))
              (chain console
                     (log "INSERTING " html-string))
              (chain after-node
                     (insert-adjacent-h-t-m-l position html-string))))))
      
      (defun delete-node (args)
        (let* ((version (@ args version))
               (node-id (@ args node-id))
               (node (chain document
                            (get-element-by-id node-id)))
               (editor (get-editor-content-node node))
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
               (from-the-end (@ args from-the-end))
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
                     (if from-the-end
                         (- (chain element
                                   inner-text
                                   length)
                            position)
                         position)))
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
                 ;; (target (@ event target inner-h-t-m-l))
                 (edited-node-id (@ path
                                    (1- (@ path length))))
                 (edited-node
                   (chain document
                          (get-element-by-id edited-node-id)))
                 (text (@ edited-node inner-h-t-m-l))
                 (cursor-position (caret-position))
                 (args (create
                        :type "update"
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
      
      (defun process-shortcut (event)
        (let* ((path (trim-path-to-nearest-paragraph
                      (calculate-path)))
               (paragraph-id (elt path
                                  (1- (@ path length))))
               (paragraph
                 (chain document
                        (get-element-by-id paragraph-id)))
               (cursor-position (caret-position))
               (previous-text (chain paragraph
                                     inner-text
                                     (substring 0 cursor-position)))
               ;; We need this to prevent commands window
               ;; to popup when the user is writing an URL.
               (writing-an-url
                 (chain previous-text
                        (match (regex "^.*https?:[^ ]*$" )))))
          
          (unless writing-an-url
            (let* ((current-version
                     (incf (@ event target dataset version)))
                   (args (create
                          :type "shortcut"
                          :key-code (@ event key-code)
                          :path path
                          :cursor-position cursor-position
                          :version current-version)))

              (initiate-action (@ event target dataset action-code)
                               (create :args args))
              (values t)))))
      
      (defun process-undo (event)
        (let* ((current-version
                 (incf (@ event target dataset version)))
               (args (create :type "undo"
                             :version current-version)))
          (initiate-action (@ event target dataset action-code)
                           (create :args args))
          (values t)))
      
      (defun paste-text (event text)
        (let* ((content-node (get-editor-content-node
                              (@ event target)))
               (current-version
                 (incf (@ content-node dataset version)))
               (path (trim-path-to-nearest-paragraph
                      (calculate-path)))
               (cursor-position (caret-position))
               (args (create
                      :type "update"
                      :change-type "paste"
                      :pasted-text text
                      :path path
                      :cursor-position cursor-position
                      :version current-version))
               (action-code (@ content-node dataset action-code)))

          (initiate-action action-code
                           (create :args args))))
      
      (defun go-up-to (tag-name starting-node)
        (loop for node = starting-node
                then (@ node parent-node)
              while (not (null node))
              when (= (@ node tag-name)
                      tag-name)
                do (return node)))
      
      (defun get-editor-node (starting-node)
        (loop for node = starting-node
                then (@ node parent-node)
              when (and (@ node class-list)
                        (chain node
                               class-list
                               (contains "editor")))
                do (return node)))

      (defun get-editor-content-node (starting-node)
        (loop for node = starting-node
                then (@ node parent-node)
              while node
              when (and (@ node class-list)
                        (chain node
                               class-list
                               (contains "content")))
                do (return node)))


      (defun get-current-paragraph ()
        (let* ((selection (chain window
                                 (get-selection)))
               (node (@ selection
                        base-node))
               (paragraph (go-up-to "P" node)))
          paragraph))

      (defun caret-position (options)
        ;; Idea was taken from
        ;; https://github.com/accursoft/caret/blob/922257adae80c529c237deaddc49f65d7c794534/jquery.caret.js#L17-L29
        (let* ((options (or options (create)))
               (inside-current-node
                 (@ options inside-current-node))
               (selection (chain window
                                 (get-selection)))
               (node (@ selection
                        base-node))
               (paragraph (if inside-current-node
                              node
                              (go-up-to "P" node))))
          ;; If there is no any range, then we can't
          ;; determine a cursor position:
          (when (and paragraph
                     (> (@ selection range-count)
                        0))
            (let* ((range-1 (chain selection
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
                     length)))))
      
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
                 (path (when node
                         (make-path node))))
            path)))
      
      (defun show-path ()
        (let ((path (calculate-path))
              (position (caret-position)))
          (chain console
                 (log "New path" path))
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
                   (editor (get-editor-content-node current-paragraph)))
              (when editor
                (let ((all-paragraphs (chain editor
                                             (get-elements-by-tag-name "P"))))
                  (loop for p in all-paragraphs
                        do (chain p
                                  class-list
                                  (remove "active")))
                  (chain current-paragraph
                         class-list
                         (add "active"))))))))

      (defun open-link (event href)
        (let* ((content (get-editor-content-node (@ event target)))
               (current-version
                 (incf (@ content dataset version))))

          (let* ((path (trim-path-to-nearest-paragraph
                        (calculate-path)))
                 (cursor-position (caret-position))
                 (args (create
                        :type "link"
                        :href href
                        :path path
                        :cursor-position cursor-position
                        :version current-version)))

            (initiate-action (@ content dataset action-code)
                             (create :args args)))))

      (defun get-link-href (path)
        (let* ((current-element-id
                 (aref path
                       (1- (length path))))
               (current-node (chain document
                                    (get-element-by-id
                                     current-element-id)))
               (current-node-length
                 (@ current-node
                    inner-text
                    length))
               (caret (caret-position (create "insideCurrentNode" t))))
          ;; We only return HREF if cursor was placed
          ;; in the middle of the link, because if it
          ;; was placed to the end, we only need to
          ;; activate the current paragraph.
          (unless (or (= caret
                         0)
                      (= caret
                         current-node-length))
            (loop for idx from (1- (length path)) downto 0
                  for id = (aref path idx)
                  for node = (chain document
                                    (get-element-by-id id))
                  for tag = (@ node tag-name)
                  when (= tag "A")
                    do (return (@ node href))))))
      
      (defun on-click (event)
        (let* ((path (calculate-path))
               (link-href (get-link-href path)))
          (chain console (log "PROCESSING ON CLICK"
                              event
                              path
                              link-href))
          (cond
            (link-href
             (open-link event link-href))
            (t
             (show-path)
             (update-active-paragraph)))))
      
      (defun on-keydown (event)
        (chain console
               (log "on-keydown event" event))
        (cond
          ((and (= (@ event key)
                   "Enter")
                (@ event alt-key))
           ;; When inside a list item,
           ;; this split will add a new item.
           ;; Otherwise, it works as a usual Enter,
           ;; adding a new paragraph:
           (change-text event "split")
           (chain event
                  (prevent-default)))
          
          ;; Arrow movements
          ((and (= (@ event key)
                   "ArrowRight")
                (@ event alt-key))
           
           (change-text event "indent")
           (chain event
                  (prevent-default)))
          
          ((and (= (@ event key)
                   "ArrowLeft")
                (@ event alt-key))
           
           (change-text event "dedent")
           (chain event
                  (prevent-default)))
          
          ;; Up&Down without modifiers
          ((and (= (@ event key)
                   "ArrowUp"))
           (change-text event "move-cursor-up")
           (chain event
                  (prevent-default)))
          
          ((and (= (@ event key)
                   "ArrowDown"))
           (change-text event "move-cursor-down")
           (chain event
                  (prevent-default)))
          
          ((= (@ event key)
              ,shortcut)
           (when (process-shortcut event)
             (chain event
                    (prevent-default))))

          ;; Cmd-Z
          ((and (= (@ event key-code)
                   90)
                (@ event meta-key))
           (process-undo event)
           (chain event
                  (prevent-default)))
          (t
           (update-active-paragraph))))

      
      (defun on-paste (event)
        (chain console
               (log "on-paste event" event))
        (let* ((pasted-text (chain (@ event clipboard-data)
                                   (get-data "text"))))
          (paste-text event pasted-text)
          (chain event
                 (prevent-default))))
      
      (defun setup ()
        (defun if-inside-editor (handler)
          "Returns a handler which is executed only if event corresponds to a current editor widget."
          (lambda (event)
            (let ((content (get-editor-content-node (@ event target))))
              (when (and content
                         (eql (@ content parent-node)
                              this))
                (handler event)))))
        (chain this
               (add-event-listener "click"
                                   (if-inside-editor on-click)))
        (chain this
               (add-event-listener "beforeinput"
                                   (if-inside-editor before-input))) 
        (chain this
               (add-event-listener "input"
                                   (if-inside-editor on-editor-input)))
        (chain this
               (add-event-listener "keydown"
                                   (if-inside-editor on-keydown )))
        (chain this
               (add-event-listener "paste"
                                   (if-inside-editor on-paste))))

      (defun on-editor-input (event)
        ;; (chain console
        ;;        (log "Handling oninput event" event current-version))
        (change-text event "modify"))

      (defun at-the-paragraph-beginning ()
        (let* ((paragraph (get-current-paragraph))
               (position (caret-position)))
          (when (and paragraph
                     (>= position 0))
            (let* ((content (@ paragraph
                               inner-text))
                   (has-only-zero-spaces t))
              (loop for idx from (1- position) downto 0
                    for symbol = (elt content idx)
                    unless (= symbol "​")
                      do (setf has-only-zero-spaces nil))
              (values has-only-zero-spaces)))))
      
      (defun before-input (event)
        (let ((type (@ event
                       input-type)))
          (chain console
                 (log "Before input" event))
          
          (when (= type "insertParagraph")
            (change-text event "split-paragraph")
            
            (chain event
                   (prevent-default)))

          (when (and (= type "deleteContentBackward")
                     (at-the-paragraph-beginning))
            (change-text event "join-with-prev-paragraph")
             
            (chain event
                   (prevent-default))))))))
