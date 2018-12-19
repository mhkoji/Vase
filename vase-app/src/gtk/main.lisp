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
  (:import-from :cl-arrows :->)
  (:export :main))
(in-package :vase.app.gtk)

(defun main (&key (conf (vase.app.container:load-configure)))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                   :type :toplevel
                   :title "Vase"
                   :default-width 600
                   :default-height 800)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((scrolled (make-instance 'gtk-scrolled-window
                                     :border-width 12
                                     :hscrollbar-policy :automatic
                                     :vscrollbar-policy :always)))

        (let ((vgrid (make-instance 'gtk-grid
                                    :orientation :vertical
                                    :border-width 8)))
          (with-container (c conf)
            (with-accessors ((db container-db)
                             (image-repos container-image-repository)
                             (folder-repos container-folder-repository)) c
              (dolist (f (vase.folder:bulk-load-by-range folder-repos
                                                         0
                                                         50))
                (let* ((pixbuf (-> (gdk-pixbuf-new-from-file
                                    (vase.image:image-path
                                     (vase.folder:folder-thumbnail f)))
                                   (gdk-pixbuf-scale-simple 480 640 :hyper)))
                       (image (gtk-image-new-from-pixbuf pixbuf)))
                  (gtk-container-add vgrid image)))))
          (gtk-container-add scrolled vgrid))
        (gtk-container-add window scrolled))

      (gtk-widget-show-all window))))
