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
   :setup-tree-view))
