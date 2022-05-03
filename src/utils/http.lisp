(uiop:define-package #:reblocks-text-editor/utils/http
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:clss)
  (:import-from #:serapeum
                #:length>=)
  (:import-from #:str
                #:starts-with-p))
(in-package #:reblocks-text-editor/utils/http)


(defun retrieve-url-info (url)
  (handler-case
      (multiple-value-bind (data status-code headers)
          (dex:get url
                   :connect-timeout 1
                   :read-timeout 1)
        (declare (ignore status-code))
        
        (let ((title nil)
              (content-type (gethash "content-type" headers)))
          (when (starts-with-p "text/html" content-type :ignore-case t)
            (let* ((tree (plump:parse data))
                   (titles (clss:select "title" tree)))
              (setf title
                    (when (length>= titles 1)
                      (plump:text
                       (elt titles 0))))))
          (values title
                  content-type)))
    (error ()
      (log:error "Unable to get title for" url)
      nil)))
