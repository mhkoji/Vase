(defpackage :vase.app.gtk
  (:use :cl
        :gtk
        :gdk
        :gdk-pixbuf
        :gobject
        :glib
        :gio
        :pango
        :cairo
        :vase.app.container)
  (:export :main))
(in-package :vase.app.gtk)

(defun main (&key (conf (vase.app.container:load-configure)))
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

      (with-container (c conf)
        (with-accessors ((db container-db)
                         (image-repos container-image-repository)
                         (folder-repos container-folder-repository)) c
          (let ((f (car (vase.folder:bulk-load-by-range
                         folder-repos
                         0
                         10))))
            (let ((images (vase.folder:folder-contents f db image-repos
                                                       :from 0
                                                       :size 5)))
              (let ((vgrid (make-instance 'gtk-grid
                                          :orientation :vertical
                                          :border-width 8)))
                (dolist (image images)
                  (let* ((pixbuf (gdk-pixbuf-new-from-file
                                  (vase.image:image-path image)))
                         (image (gtk-image-new-from-pixbuf pixbuf)))
                    (gtk-container-add vgrid image)))
                (gtk-container-add window vgrid))))))

      (gtk-widget-show-all window))))
