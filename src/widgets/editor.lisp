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
                 (format nil "~A" next-id))
           (incf next-id)
           (values)))
    (common-doc.ops:traverse-document document
                                      #'set-reference-id)
    document))


(defun make-initial-document ()
  (add-reference-ids
   (common-doc:make-paragraph 
    (list 
     (common-doc:make-text "Hello ")
     (common-doc:make-bold
      (common-doc:make-text
       "Lisp"))
     (common-doc:make-text " World!")))))


(defun to-html (document)
  (common-html.emitter:node-to-html-string document))


(reblocks/widget:defwidget editor ()
  ((updated-content :type (or null string)
                    :initform nil
                    :accessor updated-content)
   (document :type common-doc:document-node
             :initform (make-initial-document)
             :reader document)))



(defmethod reblocks/widget:render ((widget editor))
  (flet ((process-update (&key new-html &allow-other-keys)
           (log:info "Processing ~A" new-html)
           (setf (updated-content widget)
                 new-html)
           (reblocks/widget:update widget)))
    (let ((action-code (reblocks/actions:make-action #'process-update)))
      (reblocks/html:with-html
        (:div :contenteditable ""
              :data-action-code action-code
              :onload "setup()"
              :oninput "updateEditor(event)"
              :onclick "showPath()"
              ;; :beforeinput "beforeInput(event)"

              (:raw (to-html (document widget)))

              (:p :id "debug"
                  "Path will be shown here.")

              ;; (cond
              ;;   ((updated-content widget)
              ;;    (:raw (updated-content widget)))
              ;;   (t
              ;;    (reblocks/html:with-html
              ;;      (:span "My ")
              ;;      (:span :class "bold"
              ;;             "EDITOR")
              ;;      (:span "window"))))
              )))))


(defmethod reblocks/dependencies:get-dependencies ((widget editor))
  (list* (reblocks-parenscript:make-dependency
           (progn
             (chain (j-query document)
                    (ready (lambda ()
                             (chain (j-query ".editor")
                                    (each setup)))))
             
             (defun update-editor (event)
               (chain console
                      (log "Handling oninput event"))
               (chain console
                      (log event))
               (initiate-action (@ event target dataset action-code)
                                (create :args (create :new-html (@ event target inner-h-t-m-l))))
               (set-timeout restore-selection
                            1000))

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
               (save-selection)

               (chain console
                      (log "Handling before-input event"))
               
               (chain console
                      (log event))

               (chain console
                      (log (chain event
                                  (get-target-ranges))))

               (chain console
                      (log (chain event
                                  (get-target-ranges)
                                  0
                                  start-container)))
               
               ;; (chain event
               ;;        (prevent-default))
               )

             (defun save-selection ()
               (setf saved-selection
                     (chain
                      (chain window
                             (get-selection))
                      (get-range-at 0))))

             (defun restore-selection ()
               (chain console
                      (log "Restoring 1"))
               (when saved-selection
                 (chain console
                        (log "Restoring 2"))
                 
                 (let ((sel (chain window
                                   (get-selection))))
                   (chain sel
                          (remove-all-ranges))
                   (chain sel
                          (add-range saved-selection)))))))
         (reblocks-lass:make-dependency
           '(.editor
             (.bold :font-weight bold)))
         (call-next-method)))
