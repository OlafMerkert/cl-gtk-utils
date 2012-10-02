(defpackage :gtk-utils
  (:shadowing-import-from :gtk :range)
  (:use :cl :ol
        :gtk :gdk :gobject)
  (:export
   :define-signals
   :on-clicked
   :on-toggled
   :on-changed
   :on-destroy
   :default-destroy
   :event))
