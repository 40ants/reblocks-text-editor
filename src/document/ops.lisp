(uiop:define-package #:reblocks-text-editor/document/ops
  (:use #:cl)
  (:import-from #:common-doc
                #:children)
  (:import-from #:reblocks-text-editor/utils/markdown)
  (:import-from #:reblocks-text-editor/html)
  (:import-from #:alexandria
                #:lastcar)
  (:import-from #:reblocks-text-editor/document/editable
                #:get-next-reference-id)
  (:local-nicknames (#:dom #:reblocks-text-editor/dom/ops)))
(in-package #:reblocks-text-editor/document/ops)


(deftype node-with-children ()
  '(or common-doc:content-node
    common-doc:base-list))


(defun %map-node-with-children (cnode function &optional (depth 0) make-bindings)
  (let ((possibly-new-node (funcall function cnode depth)))
    (when (eql possibly-new-node cnode)
      (setf (children cnode)
            (loop for child in (children cnode)
                  unless (reblocks-text-editor/html::markup-p child)
                    collect (map-document child function
                                          (1+ depth)
                                          make-bindings))))
    (values possibly-new-node)))


(defgeneric map-document (node function &optional depth make-bindings)
  (:documentation "Map a function recursively (depth-first),
                   possibly replacing nodes with ones a FUNCTION will return.

                   Warning, this function may modify the original nodes tree!

                   The function should return the same node or the new one.
                   If a new node was returned, the function will not
                   be applied to its content.")

  (:method :around (doc function &optional (depth 0) make-bindings)
    (multiple-value-bind (vars vals)
        (when make-bindings
          (funcall make-bindings doc depth))
      (progv vars vals
        (call-next-method))))
  
  (:method ((doc common-doc:document) function &optional (depth 0) make-bindings)
    (setf (children doc)
          (loop for child in (children doc)
                unless (reblocks-text-editor/html::markup-p child)
                  collect (map-document child function
                                        (1+ depth)
                                        make-bindings)))
    (values doc))

  (:method ((cnode common-doc:content-node) function &optional (depth 0) make-bindings)
    (%map-node-with-children cnode function depth make-bindings))

  (:method ((cnode common-doc:base-list) function &optional (depth 0) make-bindings)
    (%map-node-with-children cnode function depth make-bindings))

  (:method ((dnode common-doc:document-node) function &optional (depth 0) make-bindings)
    (declare (ignore make-bindings))
    (funcall function dnode depth)))


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


(defun replace-node (document node-to-replace new-node)
  (flet ((do-replace (node depth)
           (declare (ignore depth))
           (if (eql node node-to-replace)
               new-node
               node)))
    (map-document document #'do-replace)))

(defun replace-node-content (document node-to-replace new-children)
  (flet ((do-replace (node depth)
           (declare (ignore depth))
           (when (eql node node-to-replace)
             (unless (typep node 'node-with-children)
               (error "Unable to replace content for node ~A" node))
             (setf (children node)
                   new-children))
           node))
    (map-document document #'do-replace)))


(defun find-node-at-position (node cursor-position &aux
                                                     last-visited-node
                                                     last-visited-node-content-length)
  (labels ((recursive-find (node)
             (setf last-visited-node
                   node)
             
             (etypecase node
               ((or common-doc:text-node
                    commondoc-markdown/raw-html:raw-inline-html)
                (let* ((text (typecase node
                               (commondoc-markdown/raw-html:raw-inline-html
                                (commondoc-markdown/raw-html:html node))
                               (common-doc:text-node
                                (common-doc:text node))))
                       (content-length
                         (length text)))
                  (setf last-visited-node-content-length
                        content-length)
                  
                  (if (<= cursor-position
                          content-length)
                      (return-from find-node-at-position
                        (values node
                                cursor-position))
                      (decf cursor-position
                            content-length))))
               (node-with-children
                ;; (setf last-visited-node-content-length
                ;;       (1+
                ;;        (* (markup-length node)
                ;;           2)))
                
                ;; (unless (zerop (markup-length node))
                ;;   (log:info "Skipping" (markup-length node) "for" node)
                ;;   (decf cursor-position
                ;;         (markup-length node)))

                (cond
                  ((children node)
                   (mapc #'recursive-find
                         (children node)))

                  ;; The case, when cursor points to the empty
                  ;; node, like a new paragraph with no content:
                  ((zerop cursor-position)
                   (return-from find-node-at-position
                     (values node
                             cursor-position))))
                
                ;; (unless (zerop (markup-length node))
                ;;   (log:info "Skipping" (markup-length node) "for" node)
                ;;   (decf cursor-position
                ;;         (markup-length node)))
                )))
           ;; (markup-length (node)
           ;;   (typecase node
           ;;     (common-doc:bold 2)
           ;;     (common-doc:italic 1)
           ;;     (t 0)))
           )
    
    (recursive-find node)
    (values last-visited-node
            last-visited-node-content-length)))

(defun select-outer-node-of-type (root-node node searched-type)
  "Searched a nearest outer list item."
  (let ((current-outer-item nil))
    (declare (special current-outer-item))
    
    (flet ((do-find (current-node depth)
             (declare (ignore depth))
             
             (when (eql current-node node)
               (return-from select-outer-node-of-type
                 current-outer-item))
             (values current-node))
           (make-binding (current-node depth)
             (declare (ignore depth))

             (when (typep current-node
                          searched-type)
               (values (list 'current-outer-item)
                       (list current-node)))))
      (map-document root-node
                    #'do-find 0 #'make-binding)
      ;; When nothing found
      (values))))


(defun select-outer-list (root-node node)
  "Searched a nearest outer list of any type."
  (select-outer-node-of-type root-node node 'common-doc:base-list))

(defun select-outer-list-item (root-node node)
  "Searched a nearest outer list item."
  (select-outer-node-of-type root-node node 'common-doc:list-item))


(defun select-siblings-next-to (root-node node)
  (flet ((do-find (current-node depth)
           (declare (ignore depth))
           (when (typep current-node
                        'node-with-children)
             (let* ((children (children current-node))
                    (pos (position node children)))
               (when pos
                 (return-from select-siblings-next-to
                   (subseq children (1+ pos))))))
           (values node)))
    (map-document root-node
                  #'do-find 0)
    ;; When we found nothing, return NIL
    (values)))


(defun is-empty-p (node)
  "Returns T if the plain content of the node is equivalent to an empty string."
  (check-type node common-doc:document-node)
  (string= (reblocks-text-editor/utils/markdown::to-markdown node)
           ""))

(defun is-inside-the-list (document node)
  (check-type document common-doc:document-node)
  (check-type node common-doc:document-node)
  (when (select-outer-list document node)
    t))


(defun find-changed-node (document path)
  (let* ((node-id (car (last path)))
         (node (find-node-by-reference document
                                       node-id)))
    (unless node
      (log:error "Unable to find CommonDoc node with" node-id))
    (unless (typep node 'common-doc:paragraph)
      (log:warn "Changed node should be a whole PARAGRAPH."))

    (values node)))


(defun join-with-prev-paragraph (document path new-html cursor-position)
  "This functions joins the current paragraph with the previous.

   If the current paragraph is a first one inside the list item, then
   whole content of this list item is joined with the content of the
   previous list-item."
  (let ((paragraph-to-delete (find-changed-node document path))
        (text-to-append (reblocks-text-editor/utils/text::remove-html-tags new-html)))
    (log:error "Joining paragraph" path new-html cursor-position paragraph-to-delete)
    
    (when paragraph-to-delete
      (let* ((previous-paragraph (find-previous-paragraph document
                                                          paragraph-to-delete))
             (current-list-item (select-outer-list-item document paragraph-to-delete))
             (previous-list-item (when current-list-item
                                   (find-previous-sibling document current-list-item))))
        (cond
          ;; Here we are having a previous paragraph and want
          ;; to join it with the current:
          (previous-paragraph
           (log:error "Joining with the previous paragraph")
           (check-type previous-paragraph common-doc:paragraph)
           
           (let* ((first-part (reblocks-text-editor/utils/markdown::to-markdown previous-paragraph))
                  (full-text (concatenate 'string
                                          first-part
                                          text-to-append)))
             (update-paragraph-content document previous-paragraph full-text cursor-position)
             (delete-node document
                          paragraph-to-delete)
             (ensure-cursor-position-is-correct previous-paragraph
                                                ;; The cursor now should be
                                                ;; somewhere in the middle of the new
                                                ;; paragraph. Right at the end of the
                                                ;; paragraph, we've joined our current one:
                                                (length first-part))))
          ;; Here we have no an another paragraph before the current
          ;; one and also, we are in the first list item.
          ;; In this case, we want to extract the whole content
          ;; of the current list item and place it before the current
          ;; list.
          ((and current-list-item
                (not previous-list-item))
           (log:error "Extracting content of the first list item")
           
           (let ((current-list (select-outer-list document current-list-item))
                 (items-to-move (children current-list-item)))
             (delete-node document current-list-item)
             (insert-node document items-to-move
                          :relative-to current-list
                          :position :before)
             ;; If list where we was is empty now, there is no reason
             ;; to keep it inside the document:
             (when (zerop (length (children current-list)))
               (delete-node document current-list))
             
             (ensure-cursor-position-is-correct (first items-to-move)
                                                0)))
          ;; Part where we might join two list-items
          (t
           (log:error "Joining with the previous list item")
           (let ((current-list-item (select-outer-list-item document
                                                            paragraph-to-delete)))
             (when current-list-item
               (let ((previous-list-item (find-previous-sibling document
                                                                current-list-item)))

                 (when previous-list-item
                   (join-list-items document
                                    previous-list-item
                                    current-list-item)))))))))))

(defun is-last-child-p (container node)
  (check-type container node-with-children)
  (check-type node common-doc:document-node)
  (= (length (member node (children container)))
     1))


(defun split-paragraph (document path new-html cursor-position &key dont-escape-from-list-item)
  (let ((changed-paragraph (find-changed-node document path)))
    (when changed-paragraph
      (let* ((plain-text (reblocks-text-editor/utils/text::remove-html-tags new-html))
             ;; (previous-node (find-previous-sibling document changed-paragraph))
             (text-before-cursor (subseq plain-text 0 (min cursor-position
                                                           (length plain-text))))
             (text-after-cursor (subseq plain-text (min cursor-position
                                                        (length plain-text))))
             (new-paragraph (prepare-new-content document text-after-cursor)))

        (cond
          ((and
            ;; When user presses Option + Enter, we want to stay within
            ;; the list just add another list item. Thus we have
            ;; to skip this part and do the usual
            ;; paragraph splitting:
            (not dont-escape-from-list-item)
            (typep changed-paragraph 'common-doc:paragraph)
            (string= (reblocks-text-editor/utils/text::trim-spaces plain-text) "")
            (is-inside-the-list document changed-paragraph)
            (is-last-child-p (select-outer-list document changed-paragraph)
                             (select-outer-list-item document changed-paragraph)))
           (let ((list-node (select-outer-list document changed-paragraph)))
             (delete-node document changed-paragraph)
             ;; We are moving the previous node and ignoring
             ;; the new one to not create unnecessary empty paragraphs.
             ;; The empty paragraph will be extracted and placed
             ;; next after the list where it was before:
             (insert-node document changed-paragraph
                          :relative-to list-node)
             (ensure-cursor-position-is-correct changed-paragraph
                                                0)))
          (t
           (update-paragraph-content document changed-paragraph text-before-cursor cursor-position)
           (insert-node document
                        new-paragraph
                        :relative-to changed-paragraph)
           (ensure-cursor-position-is-correct new-paragraph
                                              ;; When newline is inserted
                                              ;; the cursor will be at the beginning
                                              0)))))))

(defun append-children (widget to-node nodes-to-append)
  "Appends NODES-TO-APPEND to the container TO-NODE"
  (check-type to-node node-with-children)

  (insert-node widget nodes-to-append
               :relative-to to-node
               :position :as-last-child))


(defun join-list-items (widget previous-list-item current-list-item)
  (let ((items-to-move (children current-list-item)))
    (append-children widget previous-list-item items-to-move)
    (delete-node widget current-list-item)
    (ensure-cursor-position-is-correct (first items-to-move)
                                       0)))

(defun find-previous-paragraph (document node)
  "This function searches for the previous paragraph even if it is indide a bunch of nested lists.

   For example, when called on such document:

   * Level 1
     * Level2 paragraph 1

       Level2 paragraph 2

   Level0 paragraph

   When NODE is the \"Level0 paragraph\" function should return:
   \"Level2 paragraph 2\"."
  (let ((node (find-previous-sibling document node)))
    (labels ((recurse (node)
               (etypecase node
                 (common-doc:paragraph
                  (return-from find-previous-paragraph node))
                 (node-with-children
                  (recurse (car (last (children node)))))
                 (t
                  (return-from find-previous-paragraph nil)))))
      (recurse node))))


(defmethod insert-node ((document common-doc:document-node) node &key (relative-to (alexandria:required-argument))
                                                                   (position :after))
  (check-type relative-to common-doc:document-node)
  
  (unless (member position '(:before :after :as-first-child :as-last-child))
    (error "POSITION argument should one of: :before :after :as-first-child :as-last-child. You gave ~S" position))
  
  (let ((nodes-to-insert
          (uiop:ensure-list node)))
    (flet ((find-and-insert (current-node depth)
             (declare (ignore depth))
             (when (typep current-node 'node-with-children)
               (symbol-macrolet ((current-children
                                   (children current-node)))
                 (let ((found-pos (position relative-to
                                            current-children)))
                   (when found-pos
                     (case position
                       (:after
                        (setf current-children
                              ;; Here we are using append to support
                              ;; the case when NODE is a list of nodes
                              (append
                               (subseq current-children 0 (1+ found-pos))
                               nodes-to-insert
                               (subseq current-children (1+ found-pos)))))
                       (:before
                        (setf current-children
                              ;; Here we are using append to support
                              ;; the case when NODE is a list of nodes
                              (append
                               (subseq current-children 0 found-pos)
                               nodes-to-insert
                               (subseq current-children found-pos)))))))))
             ;; Returning the same node to continue searching
             (values current-node)))
      (case position
        ((or :before :after)
         (map-document document #'find-and-insert))
        (:as-first-child
         (unless (typep relative-to 'node-with-children)
           (error "I can insert children only into container nodes. ~A node has a wrong type."
                  relative-to))
         (setf (children relative-to)
               (append nodes-to-insert
                       (children relative-to))))
        (:as-last-child
         (unless (typep relative-to 'node-with-children)
           (error "I can insert children only into container nodes. ~A node has a wrong type."
                  relative-to))
         (setf (children relative-to)
               (append (children relative-to)
                       nodes-to-insert))))))

  (dom::insert-node document
                    node
                    :relative-to relative-to
                    :position position)
  (values))


(defmethod delete-node ((document reblocks-text-editor/document/editable::editable-document) node)
  (flet ((find-and-delete (current-node depth)
           (declare (ignore depth))
           (when (typep current-node 'node-with-children)
             (setf (children current-node)
                   (remove node (children current-node))))
           ;; Returning the same node to continue searching
           (values current-node)))
    (map-document document #'find-and-delete))

  (dom::delete-node document node)
  (values))


(defun find-previous-sibling (document node)
  (flet ((find-node (current-node depth)
           (declare (ignore depth))
           (when (typep current-node 'node-with-children)
             (let ((found-pos (position node
                                        (children current-node))))
               (when (and found-pos
                          (not (zerop found-pos)))
                 (return-from find-previous-sibling
                   (nth (1- found-pos)
                        (children current-node))))))
           ;; Returning the same node to continue searching
           (values current-node)))
    (map-document document #'find-node)
    (values)))


(defun find-next-sibling (document node)
  (flet ((find-node (current-node depth)
           (declare (ignore depth))
           (when (typep current-node 'node-with-children)
             (let ((found-pos (position node
                                        (children current-node)))
                   (last-item-pos (1- (length (children current-node)))))
               (when (and found-pos
                          (not (= found-pos
                                  last-item-pos)))
                 (return-from find-next-sibling
                   (nth (1+ found-pos)
                        (children current-node))))))
           ;; Returning the same node to continue searching
           (values current-node)))
    (map-document document #'find-node)
    (values)))


(defun add-reference-ids (document &key (to-node document))
  (flet ((set-reference-id (node depth)
           (declare (ignore depth))
           (setf (common-doc:reference node)
                 (get-next-reference-id document))
           (values)))
    (common-doc.ops:traverse-document to-node
                                      #'set-reference-id)
    (values to-node)))


(defun prepare-new-content (document text)
  (let ((paragraph (reblocks-text-editor/utils/markdown::from-markdown text)))
    (add-reference-ids document
                       :to-node paragraph)))



(defgeneric insert-node (document node &key relative-to position)
  (:documentation "Inserts one node after another."))

(defgeneric delete-node (document node)
  (:documentation "Deletes a node from container"))


(defun update-paragraph-content (document paragraph plain-text cursor-position)
  ;; Here we are updating our document tree
  (let* ((new-content (prepare-new-content document plain-text))
         (previous-node (find-previous-sibling document paragraph))
         (next-node (find-next-sibling document paragraph)))

    (etypecase new-content
      (common-doc:paragraph
       (replace-node-content document
                             paragraph
                             (children new-content))

       (dom::update-node document paragraph)
       (values paragraph cursor-position))
      ;; A new list item was created by manual enter of the "* "
      ;; at the beginning of the paragraph:
      (common-doc:unordered-list
       (let ((list-node new-content))
         (cond
           ;; If user enters "* " in a beginning of the paragraph,
           ;; following a list, we should attach this new list item
           ;; to the existing list instead of creating a new one and inserting
           ;; it into the document
           ((eql (type-of previous-node)
                 (type-of list-node))
            (let ((new-children (children list-node)))
              (insert-node document new-children
                           :relative-to previous-node
                           :position :as-last-child)
              (delete-node document paragraph)
              (values (first new-children)
                      0)))
           ;; The opposite situation, when we've created a list
           ;; before another one:
           ((eql (type-of next-node)
                 (type-of list-node))
            (let ((new-children (children list-node)))
              (insert-node document new-children
                           :relative-to next-node
                           :position :as-first-child)
              (delete-node document paragraph)
              (values (first new-children)
                      0)))
           ;; Just insert a new list into the document
           (t
            (replace-node document
                          paragraph
                          list-node)

            (dom::insert-node document
                              list-node
                              :relative-to paragraph)
            (dom::delete-node document paragraph)
            (values list-node
                    (decf cursor-position 2)))))))))



(defun ensure-cursor-position-is-correct (changed-node cursor-position)
  ;; We need to move cursor because in HTML cursor
  ;; position is relative to the most inner element
  ;; and we might introduce some markup elements during
  ;; PREPARE-NEW-CONTENT phase.
  (multiple-value-bind (node new-cursor-position)
      (find-node-at-position changed-node
                             cursor-position)
    (cond
      (node
       (dom::move-cursor node new-cursor-position))
      (t
       (log:error "Unable to find node for"
                  cursor-position
                  (reblocks-text-editor/html::to-html-string changed-node))))))


(defun last-child-of (node)
  (check-type node node-with-children)
  (lastcar (children node)))


(defun first-child-of (node)
  (check-type node node-with-children)
  (first (children node)))


(defun indent (document path cursor-position)
  "This functions tries to increase indentation the current node.

   If node is a list-item, it will be transformed into a nested list."
  (let ((current-node (find-changed-node document path)))
    (log:error "Indenting" current-node)
    
    (let* ((current-list-item (select-outer-list-item document current-node))
           (previous-list-item (when current-list-item
                                 (find-previous-sibling document current-list-item))))
      (when (and current-list-item
                 ;; TODO: implement visual bell
                 ;; to let user know that first item can't be indented
                 (not (null previous-list-item)))
        (let ((last-node (last-child-of previous-list-item)))
          (delete-node document current-list-item)
          (typecase last-node
            ;; Insert node to the list inside a previous
            ;; list item:
            (common-doc:base-list
             (insert-node document current-list-item
                          :relative-to last-node
                          :position :as-last-child))
            ;; Otherwise, create a new list
            ;; with this one item
            (t
             (let ((new-list (add-reference-ids document
                                                :to-node (common-doc:make-unordered-list
                                                          (list current-list-item)))))
               (insert-node document new-list
                            :relative-to last-node
                            :position :after))))
          ;; We need to restore a cursor position
          ;; after nodes movement:
          (ensure-cursor-position-is-correct current-node
                                             cursor-position))))))


(defun dedent (document path cursor-position)
  "This functions tries to decrease indentation the current node.

   If node is a list-item, it will be transformed into a nested list.

   Only the last list item can be dedented. In this case
   it appended as a next node after the current list.

   In case if the current list also in the list item,
   then the current list item will be added after it."
  (let ((current-node (find-changed-node document path)))
    (log:error "Indenting" current-node)
    
    (let* ((current-list-item (select-outer-list-item document current-node))
           (next-sibling (find-next-sibling document current-list-item)))
      (when (and current-list-item
                 ;; TODO: implement visual bell
                 ;; to let user know that first item can't be indented
                 ;; 
                 ;; We only can dedent the last list item,
                 ;; because otherwise it is unclrear where to
                 ;; put the rest siblings:
                 (null next-sibling))
        (let* ((parent-list (select-outer-list document current-list-item))
               (parent-list-outer-item (select-outer-list-item document parent-list)))
          (cond
            (parent-list-outer-item
             (delete-node document current-list-item)
             ;; If after list item deletion list becomes
             ;; empty, we don't need it to
             (unless (children parent-list)
               (delete-node document parent-list))

             (insert-node document current-list-item
                          :relative-to parent-list-outer-item
                          :position :after))
            ;; If there is no outer list around the current,
            ;; then we should transform our current list item
            ;; into the usual content and insert it after
            ;; our current list
            (t
             (delete-node document current-list-item)
             (insert-node document (children current-list-item)
                          :relative-to parent-list
                          :position :after)
             ;; If after list item deletion list becomes
             ;; empty, we don't need it to
             (unless (children parent-list)
               (delete-node document parent-list))))
          
          (ensure-cursor-position-is-correct current-node
                                             cursor-position))))))
