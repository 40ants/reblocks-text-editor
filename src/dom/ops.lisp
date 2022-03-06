(uiop:define-package #:reblocks-text-editor/dom/ops
  (:use #:cl)
  (:import-from #:reblocks/commands
                #:add-command)
  (:import-from #:reblocks-text-editor/document/editable
                #:editable-document
                #:content-version)
  (:import-from #:common-doc
                #:reference)
  (:import-from #:reblocks-text-editor/html
                #:to-html-string)
  (:import-from #:serapeum
                #:soft-list-of)
  (:import-from #:alexandria
                #:required-argument))
(in-package #:reblocks-text-editor/dom/ops)


(defparameter +allowed-positions+
  '(:before :after :as-first-child :as-last-child))


(defun insert-node (document content &key (relative-to (required-argument))
                                       (position :after))
  (check-type document editable-document)
  (unless (member position +allowed-positions+)
    (error "POSITION argument should be one of ~{~A~^~% ~}"
           +allowed-positions+))
  (check-type relative-to common-doc:document-node)
  (check-type content (or common-doc:document-node
                       (soft-list-of common-doc:document-node)))
  
  (add-command 'insert-node
               :version (content-version document)
               :relative-to-node-id (reference relative-to)
               :position (ecase position
                           (:after "afterend")
                           (:before "beforebegin")
                           (:as-last-child "beforeend")
                           (:as-first-child "afterbegin"))
               :html (to-html-string content)))


(defun delete-node (document node)
  (check-type document editable-document)
  (check-type node common-doc:document-node)

  (add-command 'delete-node
               :version (reblocks-text-editor/document/editable::content-version document)
               :node-id (reference node)))

(defun replace-node (document node new-node)
  (check-type document editable-document)
  (check-type node common-doc:document-node)
  (check-type new-node common-doc:document-node)
  (reblocks/commands:add-command 'replace-node
                                 :version (content-version document)
                                 :node-id (reference node)
                                 :with-html (to-html-string new-node)))

(defun update-node (document node)
  "Updates given node on the frontend"
  (replace-node document node node))

(defun move-cursor (node new-cursor-position &key from-the-end)
  (check-type node common-doc:document-node)
  (check-type new-cursor-position integer)

  (reblocks/commands:add-command 'set-cursor
                                 :node-id (reference node)
                                 :position new-cursor-position
                                 :from-the-end (if from-the-end
                                                   :true
                                                   :false)))
