(in-package :gtk-utils)

(defgeneric copy-contents (from to)
  (:documentation "Copy all the information from FROM to TO."))

(defun crud-buttons (store view &key read-ui write-ui clear-ui default-focus)
  "Generate 5 default crud (+reset) buttons working on the `tree-view'
VIEW backed by STORE. READ-UI ought to be a function without
parameters, returning something to put in the STORE, WRITE-UI should
accept one parameter, taken straight from the store, and CLEAR-UI
should take one optional parameter, which if provided suggests a more
thorough clearing."
  ;; TODO default read/write/clear ui functions??
  (let-ui (h-box
           :var box                     ; Aktionen
           (button :label "Neu"          :var create-button) :expand nil
           (button :label "Zurücksetzen" :var clear-button)  :expand nil
           (button :label "Selektieren"  :var read-button)   :expand nil
           (button :label "Ändern"       :var update-button) :expand nil
           (button :label "Löschen"      :var delete-button) :expand nil
           (label))
    (on-clicked create-button
      (store-add-item store (funcall read-ui))
      (funcall clear-ui)
      #1=(when default-focus
           (widget-grab-focus default-focus)))
    (on-clicked read-button
      (aif (tree-view-selected-row view)
           (funcall write-ui
                    (store-item store it))
           ;; TODO signal condition
           (error "No item selected for reading."))
      #1#)
    (on-clicked update-button
      (aif (tree-view-selected-row view)
           (progn
             (copy-contents (funcall read-ui)
                            (store-item store it))
             (emit-list-store-signal store "row-changed" it)
             (funcall clear-ui))
           (error "No item selected as destination."))
      #1#)
    (on-clicked delete-button
      (aif (tree-view-selected-row view)
           (store-remove-item store
                              (store-item store it))
           (error "No item selection for deletion."))
      #1#)
    (on-clicked clear-button
      (funcall clear-ui t)
      #1#)
    box))


(ew
(defun crud-inputs (name columns)
  (let (input-vars)
    (with-gensyms!
      `(let-ui (h-box
                :var ,g!box
                ,@(mapcan
                   (with-column+ ()
                     (multiple-value-bind (entries vars)
                         (generate-ui-input-fields type)
                       (push vars input-vars)
                       `((label :label ,label)
                         ,@entries)))
                   columns))
         ;; read/write/clear
         (flet ((,g!read-ui ()
                  (make-instance
                   ',name
                   ,@(mapcan
                      (with-column+ (vars)
                        `(,(keyw accessor)
                           (read-ui-input ',type ,@vars)))
                      columns (setf input-vars (reverse input-vars)))))
                (,g!write-ui (,g!object)
                  ,@(mapcar
                     (with-column+ (vars)
                       `(write-ui-input ',type (,accessor ,g!object) ,@vars))
                     columns input-vars))
                (,g!clear-ui (&optional ,g!thorough)
                  ,@(mapcar
                     (with-column+ (vars)
                       (let ((cmd `(clear-ui-input ',type ,@vars)))
                         (if clear
                             cmd
                             `(when ,g!thorough
                                ,clear))))
                     columns input-vars)))
           (list ,g!box
                 :read-ui  ,g!read-ui
                 :write-ui ,g!write-ui
                 :clear-ui ,g!clear-ui)))))))

(defgeneric generate-ui-input-fields (type))

(defgeneric read-ui-input  (type &rest vars))
(defgeneric write-ui-input (type data &rest vars))
(defgeneric clear-ui-input (type &rest vars))

;;; TODO the ultimate macro to build a crud ui automatically from an
;;; enhanced defclass
(defun unbox (x)
  (if (atom x) x (car x)))

(defmacro defclass/crud (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass/f ,name ,direct-superclasses
      ,(mapcar #'unbox direct-slots)
      ,@options)

     (defmethod copy-contents (from to)
       (dolist (slot ',(mapcar #'unbox direct-slots))
         (setf (slot-value to slot) (slot-value from slot))))

     (define-custom-store ',(symb name '-tabelle)
         ,direct-slots
       :tree-view nil)

     (define-custom-tree-view ',(symb name '-tabelle)
         ,direct-slots)))
