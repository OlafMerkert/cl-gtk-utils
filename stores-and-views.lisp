(in-package :gtk-utils)


(defun format-decimal (decimal &optional stream)
  "format DECIMAL with two following zeroes."
  (format stream "~,2F" decimal))

(defun read-decimal (string)
  "Read a decimal from a STRING where #\, is equivalent with #\."
  (aprog1
      (read-from-string (substitute #\. #\, string :test #'char=))
    ;; TODO check that we got a number (use a condition)
    (unless (numberp it)
      (error "Input not a number: ~A" it))))

(ew
(defparameter gtk-type-mapping
  '((string  . "gchararray")
    (integer . "gint")
    (number  . "gchararray")
    (decimal . "gchararray")
    (symbol  . "gchararray")
    (date    . "gchararray"))
  "the gtk type string to use for frequent lisp types."))

(defgeneric transform-reader (type reader-function))

;; by default just do nothing
(defmethod transform-reader (type  reader-function)
  reader-function)

(defmacro! define-transform-reader (type &body transformation)
  `(defmethod transform-reader ((type (eql ',type)) ,g!reader-function)
     (lambda (,g!object)
       (let ((,type (funcall ,g!reader-function ,g!object)))
         ,@transformation))))


;; special formatting for number, symbol and date
(define-transform-reader number
  (princ-to-string number))

(define-transform-reader symbol
  (princ-to-string symbol))

(define-transform-reader decimal
  (format-decimal decimal))

(define-transform-reader date
  (ol-date-utils:print-date date))

;;; utilities for working with array list stores

;; helper macro for working with the column specs
(defmacro with-column ((column) &body body)
  `(destructuring-bind
         (accessor &key (type 'string) (label "???")
                   (clear nil) (expand nil))
       (mklist ,column)
     (declare (ignorable accessor type label clear expand))
     ,@body))

(defmacro! with-column+ (additional-args &body body)
  `(lambda (,g!col ,@additional-args)
     (with-column (,g!col)
       ,@body)))


(defgeneric make-store (store-ident &optional contents))

(defgeneric setup-tree-view (store-ident store view))

;; TODO numbers and decimals aligned to the right

(defmacro! define-custom-store (name columns &key
                                     (initial-contents nil)
                                     (tree-view t))
  `(progn
     (defmethod make-store ((,g!ident (eql ',name)) &optional ,g!contents)
       (let ((,g!store (make-instance 'array-list-store)))
         ;; create definitions of the columns
         ,@(mapcar
            (with-column+ ()
              `(store-add-column
                ,g!store
                ,(cond ((stringp type) type)
                       ((assoc1 type gtk-type-mapping))
                       (t (error "Uncompatible lisp type ~A for GTK store." type)))
                (transform-reader ',type (function ,accessor))))
            columns)
         ;; setup the contents of the array-list store
         (store-load-items ,g!store (or ,g!contents ,initial-contents))
         ,g!store))
     ,(when tree-view
            `(define-custom-tree-view ,name ,columns))))

(defmacro! define-custom-tree-view (name columns &key)
  `(defmethod setup-tree-view ((,g!ident (eql ',name)) ,g!store ,g!view)
     (setf (tree-view-model ,g!view) ,g!store)
     ,@(iter (for col in columns)
             (for i from 0)
             (with-column (col)
               ;; TODO maybe use type information to choose
               ;; different renderers
               (collect
                   `(add-tree-view-column ,g!view ,label ,i))))))

(defun emit-list-store-signal (store signal row)
  "Emit the given SIGNAL (as string) on the given ROW of the
`tree-model' STORE."
  (let ((path (make-instance 'tree-path))
        (iter (make-instance 'tree-iter)))
    (setf (tree-path-indices path) (list row)
          (tree-iter-stamp iter) 0
          (tree-iter-user-data iter) row)
    (if (equal signal "row-deleted")
        (emit-signal store signal path)
        (emit-signal store signal path iter))))

(defun store-load-items (store initial-contents)
  (setf (slot-value store 'gtk::items)
        (if (and (arrayp initial-contents)
                 (adjustable-array-p initial-contents))
            initial-contents
            (make-array (length initial-contents)
                        :adjustable t
                        :initial-contents initial-contents
                        :fill-pointer t))))


(defun store-replace-all-items (store new-item-array)
  "Replace the backing array of an `array-list-store' with
NEW-ITEM-ARRAY and send signals for the deletion of all previous
entries, and signals for the insertion of all the new entries."
  (let ((l-old (store-items-count store))
        (l-new (length new-item-array)))
    ;; signal deletion of all the rows
    (iter (for i from (- l-old 1) downto 0)
          (emit-list-store-signal store "row-deleted" i))
    ;; replace the array
    (setf (slot-value store 'gtk::items) new-item-array)
    ;; signal creation of all the new rows
    (iter (for i from 0 below l-new)
          (emit-list-store-signal store "row-inserted" i))
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

;;; utilities for combo boxes
(defun add-cell-layout-column (view col-index)
  (let ((renderer (make-instance 'cell-renderer-text)))
    (cell-layout-pack-start view renderer)
    ;; TODO other renderers than text??
    (cell-layout-add-attribute view renderer "text" col-index)))

(define-custom-store standard-combo-box-model
    ((mkstr)))

(defgeneric setup-combo-box (store combo-box &optional column)
  (:documentation "Hook the COMBO-BOX to a STORE, given either by a
  `sequence', a `symbol' identifying a store or an actual
  `list-store', where the data is take from COLUMN."))

(defmethod setup-combo-box (store combo-box &optional (column 0))
  (setf (combo-box-model combo-box) store
        (combo-box-active combo-box) 0)
  (add-cell-layout-column combo-box column))

(defmethod setup-combo-box ((store symbol) combo-box &optional (column 0))
  (setup-combo-box (make-store store) combo-box column))

(defmethod setup-combo-box ((store-contents sequence) combo-box &optional column)
  (declare (ignore column))
  (setup-combo-box
   (make-store 'standard-combo-box-model
               store-contents)
   combo-box))
