(uiop:define-package #:reblocks-text-editor/utils/scribdown
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:reblocks-text-editor/typed-pieces/base
                #:typed-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/html
                #:html-piece)
  (:import-from #:reblocks-text-editor/utils/text
                #:remove-html-tags))
(in-package #:reblocks-text-editor/utils/scribdown)


(defclass scribdown (commondoc-markdown/format:markdown)
  ()
  (:documentation "Special format which emits CommonDoc nodes as markdown, but noneditable nodes
                   are written like @placeholder items."))


(defmethod common-doc.format:emit-document ((format scribdown)
                                            (node common-doc:image)
                                            stream)
  (format stream "@placeholder[ref=~A]()"
          (common-doc:reference node)))


(defun to-scribdown (node &key (trim-spaces t))
  (let* ((commondoc-markdown/emitter:*emit-section-anchors* nil)
         (commondoc-markdown/emitter:*generate-short-link-references* nil)
         (reblocks-text-editor/html::*render-markup*)
         (result (common-doc.format:emit-to-string (make-instance 'scribdown)
                                                   node)))
    (if trim-spaces
        (reblocks-text-editor/utils/text::trim-spaces result)
        result)))
