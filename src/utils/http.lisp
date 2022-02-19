(uiop:define-package #:reblocks-text-editor/utils/http
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:clss)
  (:import-from #:serapeum
                #:length>=))
(in-package #:reblocks-text-editor/utils/http)


(defun retrieve-url-title (url)
  (let* ((data (dex:get url
                        :connect-timeout 1
                        :read-timeout 1))
         (tree (plump:parse data))
         (titles (clss:select "title" tree))
         (title (when (length>= titles 1)
                  (plump:text
                   (elt titles 0)))))
    title))
