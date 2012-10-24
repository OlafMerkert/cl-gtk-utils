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
(defun crud-inputs (name columns box-var body)
  "Generate code to fill an `h-box' bound to BOX-VAR with input
controls matching the types of columns. The first input control is
bound to DEFAULT-FOCUS-WIDGET in body, further we generate READ-UI,
WRITE-UI, CLEAR-UI functions to be passed to CRUD-BUTTONS, which
generate instances of class NAME."
  (let (input-vars)
    (with-gensyms!
      `(let-ui (h-box
                :var ,box-var
                ,@(mapcan
                   (with-column+ ()
                     (multiple-value-bind (entries vars)
                         (generate-ui-input-fields type)
                       (push vars input-vars)
                       `((label :label ,label) :expand nil
                         ,@(mapcan (lambda (x) `(,x :expand ,expand)) entries))))
                   columns))
         ;; read/write/clear
         (flet ((read-ui ()
                  (make-instance
                   ',name
                   ,@(mapcan
                      (with-column+ (vars)
                        `(,(keyw accessor)
                           (read-ui-input ',type ,@vars)))
                      columns (setf input-vars (reverse input-vars)))))
                (write-ui (,g!object)
                  ,@(mapcar
                     (with-column+ (vars)
                       `(write-ui-input ',type (,accessor ,g!object) ,@vars))
                     columns input-vars))
                (clear-ui (&optional ,g!thorough)
                  ,@(mapcar
                     (with-column+ (vars)
                       (let ((cmd `(clear-ui-input ',type ,@vars)))
                         (if clear
                             cmd
                             `(when ,g!thorough
                                ,cmd))))
                     columns input-vars)))
           (let ((default-focus-widget ,(unbox input-vars)))
             ,@body)))))))

(defgeneric generate-ui-input-fields (type))

(defgeneric read-ui-input  (type &rest vars))
(defgeneric write-ui-input (type data &rest vars))
(defgeneric clear-ui-input (type &rest vars))

(ew
(defun numbered-symbols (prefix list)
  (iter (for i from 1)
        (for l in list)
        (collect (symb prefix i)))))

(defmacro! define-ui-input (type &body components)
  "Define an UI input element to be used by CRUD-INPUTS for the given
TYPE. A COMPONENT has as first element the argument list to
MAKE-INSTANCE to create the gtk widget, the rest ought to be a plist
with keys :ACCESSOR, giving a form to extract the data from a given
object bound to the TYPE, and :default-value, the default data to be
put into the widget.

Two special components, with the first element being either :WRITE
or :READ, ought to give the code that either sets or gets the contents
of the normal components, which are locally bound to C-1, ..., C-N.
Extracted data or default-values for the components are bound to V-1,
..., V-N in the :WRITE part. The :READ part should return an object
matching the understanding of TYPE."
  (let ((comp-specs (remove-if-not #'listp components :key #'first))
        (read-spec  (assoc1 :read components))
        (write-spec (assoc1 :write components))
        (type-spec  `(type (eql ',type))))
    (let ((comp-symbols  (numbered-symbols 'c- comp-specs))
          (value-symbols (numbered-symbols 'v- comp-specs))
          (comp-gensyms  (list->gensyms comp-specs)))
      `(progn
         (defmethod generate-ui-input-fields (,type-spec)
           (values ',(mapcar (lambda (comp-spec gensym)
                                (append (first comp-spec) `(:var ,gensym)))
                             comp-specs comp-gensyms)
                   ',comp-gensyms))
         (defmethod read-ui-input (,type-spec &rest ,g!vars)
           (destructuring-bind ,comp-symbols ,g!vars
             ,@read-spec))
         (defmethod write-ui-input (,type-spec ,type &rest ,g!vars)
           (destructuring-bind ,comp-symbols ,g!vars
             (let ,(mapcar #2`(,a1 ,(getf (rest a2) :accessor type))
                           value-symbols
                           comp-specs)
               ,@write-spec)))
         (defmethod clear-ui-input (,type-spec &rest ,g!vars)
           (destructuring-bind ,comp-symbols ,g!vars
             (let ,(mapcar #2`(,a1 ,(getf (rest a2) :default ""))
                           value-symbols
                           comp-specs)
               ,@write-spec)))))))

(define-ui-input string
  ((entry)
   :default-value "")
  (:write (setf (entry-text c-1) v-1))
  (:read (entry-text c-1)))

(define-ui-input date
  ((entry :width-chars 2)
   :accessor (princ-to-string (local-time:timestamp-day date))
   :default-value "")
  ((entry :width-chars 2)
   :accessor (princ-to-string (local-time:timestamp-month date))
   :default-value "")
  ((entry :width-chars 4)
   :accessor (princ-to-string (local-time:timestamp-year date))
   :default-value "")
  (:write (setf (entry-text c-1) v-1
                (entry-text c-2) v-2
                (entry-text c-3) v-3))
  (:read  (ol-date-utils:encode-date
           (parse-integer (entry-text c-1))
           (parse-integer (entry-text c-2))
           (parse-integer (entry-text c-3)))))

(define-ui-input decimal
  ((entry :width-chars 8)
   :accessor (format-decimal decimal)
   :default-value "")
  (:write (setf (entry-text c-1) v-1))
  (:read (read-decimal (entry-text c-1))))

(define-ui-input symbol
  ((entry)
   :accessor (symbol-name symbol)
   :default-value "")
  (:write (setf (entry-text c-1) v-1))
  (:read (intern (string-upcase (entry-text c-1)))))

(defgeneric make-crud (crud-ident contents))

(defstruct (crud
             (:constructor make-crud%)
             (:conc-name crud%-))
  store
  view
  inputs
  buttons)

(defmacro! define-custom-crud (name columns)
  "Define a setup method MAKE-CRUD for the elements of a CRUD (the
structure) with a backing store, a `tree-view', an input widget and a
buttons widget all glued together."
  `(defmethod make-crud ((crud-ident (eql ',name)) ,g!contents)
     (let ((,g!store (make-store ',(symb name '-tabelle) ,g!contents))
           (,g!view  (make-instance 'tree-view)))
       (setup-tree-view ',(symb name '-tabelle) ,g!store ,g!view)
       ,(crud-inputs
         name columns g!inputs
         `((make-crud% :store ,g!store
                       :view ,g!view
                       :inputs ,g!inputs
                       :buttons
                       (crud-buttons ,g!store ,g!view
                          :read-ui  #'read-ui
                          :write-ui #'write-ui
                          :clear-ui #'clear-ui
                          :default-focus default-focus-widget)))))))

(defun crud-default-attach (crud v-box)
  "Given a CRUD structure, first attach the inputs, then the buttons
and finally the `tree-view' in a `scrolled-window' to the V-BOX.
Return the backing store of the CRUD."
  (let ((scwin (make-instance 'scrolled-window
                              :hscrollbar-policy nil
                              :vscrollbar-policy :automatic)))
    (box-pack-start v-box (crud%-inputs crud) :expand nil)
    (box-pack-start v-box (crud%-buttons crud) :expand nil)
    (box-pack-start scwin (crud%-view crud))
    (box-pack-start v-box scwin))
  (crud%-store crud))


;;; the ultimate macro to build a crud ui automatically from an
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

     (define-custom-store ,(symb name '-tabelle)
         ,direct-slots
       :tree-view nil)

     (define-custom-tree-view ,(symb name '-tabelle)
         ,direct-slots)

     (define-custom-crud ,name ,direct-superclasses)))

;; TODO support for combo-boxes in the crud
