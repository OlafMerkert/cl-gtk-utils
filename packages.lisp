(defpackage :gtk-utils
  (:shadowing-import-from :gtk :range)
  (:use :cl :ol
        :iterate
        :gtk :gdk :gobject)
  (:export
   :define-signals
   :on-clicked
   :on-toggled
   :on-changed
   :on-destroy
   :default-destroy
   :event
   :store-replace-all-items
   :create-custom-store
   :group-radio-buttons
   :define-custom-store
   :make-store
   :setup-tree-view
   :add-tree-view-column
   :tree-view-selected-row
   :args
   :add-cell-layout-column
   :number
   :decimal
   :symbol
   :date
   :emit-list-store-signal
   :format-decimal
   :read-decimal
   :setup-combo-box
   :crud-buttons
   :copy-contents
   :defclass/crud
   :crud-inputs
   :make-crud
   :crud-default-attach
   :define-custom-crud))

(defpackage :gtk-user
  (:shadowing-import-from :gtk :range)
  (:use :cl :ol
        :iterate
        :gtk :gdk :gobject))
