(defsystem cl-gtk-utils
    :depends-on (ol-utils
                 ol-data-utils
                 cl-gtk2-gtk)
    :serial t
    :components ((:file "packages")
                 (:file "events")
                 (:file "stores-and-views")))
