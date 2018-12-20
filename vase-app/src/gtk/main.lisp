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

(defun render-folders (conf offset size)
  (with-container (c conf)
    (with-accessors ((db container-db)
                     (image-repos container-image-repository)
                     (folder-repos container-folder-repository)) c
      (let ((folders (vase.folder:bulk-load-by-range folder-repos
                                                     offset
                                                     size)))
        (let ((scrolled (make-instance 'gtk-scrolled-window
                                       :border-width 12
                                       :hscrollbar-policy :automatic
                                       :vscrollbar-policy :always))
              (vgrid (make-instance 'gtk-grid
                                    :orientation :vertical
                                    :border-width 8)))
          (dolist (f folders)
            (let* ((pixbuf (-> (gdk-pixbuf-new-from-file
                                (vase.image:image-path
                                 (vase.folder:folder-thumbnail f)))
                               (gdk-pixbuf-scale-simple 300 300 :hyper)))
                   (image (gtk-image-new-from-pixbuf pixbuf)))
              (gtk-container-add vgrid image)))
          (gtk-container-add scrolled vgrid)
          scrolled)))))

(defun main (&key (conf (vase.app.container:load-configure)))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                   :type :toplevel
                   :title "Vase"
                   :default-width 600
                   :default-height 800))
          (offset 0)
          (size 50))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (let ((table (make-instance 'gtk-table
                                  :n-columns 1
                                  :n-rows 20
                                  :homogeneous t))
            (scrolled (render-folders conf offset size)))
        (gtk-table-attach table scrolled 0 1 0 18)

        (let ((prev-button (gtk-button-new-with-label "<"))
              (next-button (gtk-button-new-with-label ">"))
              (button-box (make-instance 'gtk-box
                                         :orientation :horizontal
                                         :spacing 12
                                         :border-width 12)))
          (gtk-box-pack-start button-box prev-button)
          (gtk-box-pack-start button-box next-button)
          (g-signal-connect next-button "clicked"
                            (lambda (w)
                              (declare (ignore w))
                              (gtk-widget-destroy scrolled)
                              (incf offset size)
                              (setq scrolled
                                    (render-folders conf offset size))
                              (gtk-table-attach table scrolled 0 1 0 18)
                              (gtk-widget-show-all scrolled)))
          (gtk-table-attach table button-box 0 1 18 20))

        (gtk-container-add window table))

      (gtk-widget-show-all window))))
