(in-package :gtk-utils)

(defparameter gtk-type-mapping
  '((nil     . "gchararray")
    (string  . "gchararray")
    (integer . "gint"))
  "the gtk type string to use for frequent lisp types.")

;;; utilities for working with array list stores
(defmacro! create-custom-store (columns &key (initial-contents nil contents-p)
                                        (initial-size 20))
  `(let ((,g!store (make-instance 'array-list-store))
         (,g!initial-contents ,initial-contents))
     ;; create definitions of the columns
     ,@(mapcar
        (compose #`(store-add-column
                    ,g!store
                    ,(cond ((stringp (second a1))
                            a1)
                           ((assoc1 (second a1) gtk-type-mapping))
                           (t (error "Uncompatible lisp type ~A for GTK store."
                                     (second a1))))
                    (function ,(first a1)))
                 #'mklist)
        columns)
     ;; setup the contents of the array-list store
     (setf (slot-value ,g!store 'gtk::items)
           (if (arrayp ,g!initial-contents)
               ,g!initial-contents
               (make-array (max ,initial-size
                                (length ,g!initial-contents))
                           :adjustable t
                           :fill-pointer ,(if contents-p 't 0))))
     ,g!store))


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
