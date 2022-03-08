(uiop:define-package #:reblocks-text-editor/document/copying
  (:use #:cl))
(in-package #:reblocks-text-editor/document/copying)


(metacopy:defcopy-methods reblocks-text-editor/document/editable::editable-document
  ;; These slots will be shared among all versions of the document:
  :set (reblocks-text-editor/document/editable::lock
        reblocks-text-editor/document/editable::undo-history)
  ;; And these are deep-copyied:
  :copy (common-doc::children
         common-doc::metadata
         common-doc::reference
         reblocks-text-editor/document/editable::next-id
         reblocks-text-editor/document/editable::revision))

(metacopy:defcopy-methods common-doc:document-node
  :copy-all t)

(metacopy:defcopy-methods common-doc:content-node
  :copy-all t)

(metacopy:defcopy-methods common-doc:paragraph
  :copy-all t)

(metacopy:defcopy-methods common-doc:unordered-list
  :copy-all t)

(metacopy:defcopy-methods common-doc:ordered-list
  :copy-all t)

(metacopy:defcopy-methods common-doc:code-block
  :copy-all t)

(metacopy:defcopy-methods common-doc:web-link
  :copy-all t)

(metacopy:defcopy-methods common-doc:document-link
  :copy-all t)

(metacopy:defcopy-methods commondoc-markdown:markdown-link
  :copy-all t)

(metacopy:defcopy-methods common-doc:text-node
  :copy-all t)

;; These are structures for which usual standard copying methods don't work 
(defmethod metacopy:copy-one ((obj quri.uri.http:uri-https) copy-hash)
  (quri:copy-uri obj))

(defmethod metacopy:copy-one ((obj quri.uri.http:uri-http) copy-hash)
  (quri:copy-uri obj))


