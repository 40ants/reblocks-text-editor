(uiop:define-package #:reblocks-text-editor/utils/text
  (:use #:cl)
  (:import-from #:cl-ppcre))
(in-package #:reblocks-text-editor/utils/text)


(defparameter +zero-width-space+
  (coerce '(#\Zero_Width_Space) 'string)
  "This string contains a character which corresponds to a &ZeroWidthSpace; HTML entity.")


(defun trim-spaces (string)
  (string-trim '(#\Newline #\Space #\Tab #\Zero_Width_Space)
               string))

(defun remove-zero-spaces-unless-string-is-empty (string)
  (cond
    ((string= string "")
     +zero-width-space+)
    ((string= string +zero-width-space+)
     string)
    (t
     (cl-ppcre:regex-replace-all +zero-width-space+
                                 string
                                 ""))))


(defun remove-html-tags (html-string &key (remove-new-lines t))
  (let* ((result (cl-ppcre:regex-replace-all "<[^>]+>" html-string
                                             ""))
         (result (remove-zero-spaces-unless-string-is-empty result))
         (result (plump:decode-entities result))
         (result (if remove-new-lines
                     ;; For some strange reason sometimes browser starts
                     ;; passing newlines even inside span elements. Why? Don't know.
                     ;; Thus by default we are removing new lines.
                     ;; However, when processing content of code blocks
                     ;; it is useful to keep newlines.
                     (str:replace-all '(#\Newline) "" result)
                     result)))
    result))


(defun ensure-two-newlines-at-the-end (string)
  (if (and (>= (length string)
               2)
           (eql (elt string (1- (length string)))
                #\Newline)
           (not (eql (elt string (1- (1- (length string))))
                     #\Newline)))
      (with-output-to-string (s)
        (write-string string s)
        (write-char #\Newline s))
      string)
  
  ;; (let ((num-newlines-to-add
  ;;         (+ (if (and (>= (length string)
  ;;                         1)
  ;;                     (not (eql (elt string (1- (length string)))
  ;;                               #\Newline)))
  ;;                1
  ;;                0)
              
  ;;             (if (and (>= (length string)
  ;;                          2)
  ;;                      (not (eql (elt string (1- (1- (length string))))
  ;;                                #\Newline))
  ;;                      (not (eql (elt string (1- (1- (length string))))
  ;;                                #\Newline)))))
  ;;         )) (if 
  ;;             (with-output-to-string (s)
  ;;               (write-string string s)
  ;;               (write-char #\Newline s)
  ;;               (write-char #\Newline s))
  ;;             string))
  )


(defun text-line-length (text pos)
  (let* ((current-char (elt text pos))
         (begin-pos (if (eql current-char #\Newline)
                        (1+ pos)
                        (let ((prev-newline
                                (position #\Newline text
                                          :from-end t
                                          :end pos)))
                          (if prev-newline
                              (1+ prev-newline)
                              0))))
         (end-pos (let ((next-newline
                          (position #\Newline text
                                    :start (if (eql current-char #\Newline)
                                               (1+ pos)
                                               pos))))
                    (or next-newline
                        (length text)))))
    (- end-pos
       begin-pos)))


(defun move-caret-on-the-next-line (text caret-position)
  "Returns a new caret position (relative to the beginning of the text)
   if there is a next line.

   If next line is shorter than the current, then new position
   will be right at the end.

   If there is no next line, then function returns nil."
  (let* ((position-in-line (caret-position-from-beginning-of-the-line text caret-position))
         (next-newline-pos (position #\Newline text
                                     :start caret-position)))
    (when next-newline-pos
      (let* ((begin-of-next-line (1+ next-newline-pos))
             (next-line-length (text-line-length text (1+ next-newline-pos)))
             (position-in-next-line (min position-in-line
                                         next-line-length)))
        (+ begin-of-next-line
           position-in-next-line)))))


(defun move-caret-on-the-prev-line (text caret-position)
  "Returns a new caret position (relative to the beginning of the text)
   if there is a next line.

   If next line is shorter than the current, then new position
   will be right at the end.

   If there is no next line, then function returns nil."
  (let* ((position-in-line (caret-position-from-beginning-of-the-line text caret-position))
         (prev-newline-pos (position #\Newline text
                                     :from-end t
                                     :end caret-position)))
    (when prev-newline-pos
      (let* ((one-more-newline-back-pos (position #\Newline text
                                                  :from-end t
                                                  :end prev-newline-pos))
             (begin-of-prev-line (if one-more-newline-back-pos
                                     (1+ one-more-newline-back-pos)
                                     0))
             (prev-line-length (text-line-length text begin-of-prev-line))
             (position-in-prev-line (min position-in-line
                                         prev-line-length)))
        (+ begin-of-prev-line
           position-in-prev-line)))))


(defun caret-position-from-beginning-of-the-line (text caret-position)
  (let ((newline-pos (position #\Newline text
                               :from-end t
                               :end caret-position)))
    (if newline-pos
        (- caret-position newline-pos)
        ;; We are at the first line:
        caret-position)))
