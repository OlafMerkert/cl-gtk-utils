(in-package :gtk-utils)

(defparameter gtk-type-mapping
  '((string  . "gchararray")
    (integer . "gint"))
  "the gtk type string to use for frequent lisp types.")

;;; utilities for working with array list stores
(defgeneric make-store (store-ident &optional contents))

(defgeneric setup-tree-view (store-ident view))

(defmacro! define-custom-store (name columns &key
                                     (initial-contents nil)
                                     (initial-size 20))
  (let ((columns (mapcar #'mklist columns)))
    `(progn
       (defmethod make-store ((,g!ident (eql ',name)) &optional ,g!contents)
         (let ((,g!store (make-instance 'array-list-store))
               (,g!initial-contents (or ,g!contents ,initial-contents)))
           ;; create definitions of the columns
           ,@(mapcar
              (lambda (x)
                (destructuring-bind (accessor &key (type 'string) &allow-other-keys) x
                  `(store-add-column
                    ,g!store
                    ,(cond ((stringp type) type)
                           ((assoc1 type gtk-type-mapping))
                           (t (error "Uncompatible lisp type ~A for GTK store." type)))
                    (function ,accessor))))
              columns)
           ;; setup the contents of the array-list store
           (setf (slot-value ,g!store 'gtk::items)
                 (if (arrayp ,g!initial-contents)
                     ,g!initial-contents
                     (make-array (max ,initial-size
                                      (length ,g!initial-contents))
                                 :adjustable t
                                 :fill-pointer (if ,g!initial-contents
                                                   (length ,g!initial-contents)
                                                   0))))
           ,g!store))
       (defmethod setup-tree-view ((,g!ident (eql ',name)) ,g!view)
         ,@(iter (for x in columns)
                 (for i from 0)
                 (destructuring-bind (accessor &key (label "??") &allow-other-keys) x
                   (declare (ignore accessor))
                   ;; TODO maybe use type information to choose
                   ;; different renderers
                   (collect
                       `(add-tree-view-column ,g!view ,label ,i))))))))

(defun store-replace-all-items (store new-item-array)
  "Replace the backing array of an ARRAY-LIST-STORE with
NEW-ITEM-ARRAY and send signals for the deletion of all previous
entries, and signals for the insertion of all the new entries."
  (let ((l-old (store-items-count store))
        (l-new (length new-item-array)))
    ;; signal deletion of all the rows
    (iter (for i from (- l-old 1) downto 0)
          (for path = (make-instance 'tree-path))
          (setf (tree-path-indices path) (list i))
          (emit-signal store "row-deleted" path))
    ;; replace the array
    (setf (slot-value store 'gtk::items) new-item-array)
    ;; signal creation of all the new rows
    (iter (for i from 0 below l-new)
          (for path = (make-instance 'tree-path))
          (for iter = (make-instance 'tree-iter))
          (setf (tree-path-indices path) (list i))
          (setf (tree-iter-stamp iter) 0
                (tree-iter-user-data iter) i)
          (emit-signal store "row-inserted"
                       path iter))
    l-new))

;;; utilities for working with tree-views on top of these stores
(defun add-tree-view-column (view title col-index)
  "Properly add a text column to the tree-view"
  (let ((column   (make-instance 'tree-view-column :title title))
        (renderer (make-instance 'cell-renderer-text)))
    (tree-view-column-pack-start     column renderer)
    (tree-view-column-add-attribute  column renderer "text" col-index)
    (tree-view-append-column view    column)))

(defun tree-view-selected-row (view)
  "Return the selected row index from tree-view, which presents a
list-store."
  (let ((row-paths (tree-selection-selected-rows (tree-view-selection view))))
    (when row-paths
      (first (tree-path-indices (first row-paths))))))
