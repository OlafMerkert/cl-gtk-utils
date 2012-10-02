(defpackage :gtk-utils
  (:shadowing-import-from :gtk :range)
  (:use :cl :ol
        :gtk :gdk :gobject)
  (:export))
