(defpackage :vase.app.gtk
  (:use :cl
        :gtk
        :gdk
        :gdk-pixbuf
        :gobject
        :glib
        :gio
        :pango
        :cairo))
(in-package :vase.app.gtk)

(defun main ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                   :type :toplevel
                   :title "Vase"
                   :default-width 500
                   :default-height 500)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))
