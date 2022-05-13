(uiop:define-package #:reblocks-text-editor/frontend/css
  (:use #:cl)
  (:import-from #:reblocks-lass))
(in-package #:reblocks-text-editor/frontend/css)


(defun make-css-code ()
  (reblocks-lass:make-dependency
    '(body
      (.editor
       (.caret-wrapper
        ;; :border 1px solid orange
        (.caret
         :background-color lightgray))
       (.content
        :caret-color transparent
        :outline none
        (p
         :white-space pre-wrap)
        (ul
         :padding-left 1.2rem))
       (.bold :font-weight bold)
       (.markup :display none)
       (code
        :border inherit
        :background-color inherit
        :padding 0)
       (.code-block
        :background-color "#EEE"
        :box-sizing border-box
        ;; Invisible border (will become visible for active block)
        :border 1px solid "#EEE"
        
        :margin-left -0.5rem
        :padding-left 0.5rem
        :margin-right -0.5rem
        :padding-right 0.5rem
        :min-width 10rem
        :min-height 2rem)
       ((:and .code-block .active)
        :border 1px solid "#444"
        (code
         :border none))
       ((:and p .active)
        (.markup :display inline-block))))))
