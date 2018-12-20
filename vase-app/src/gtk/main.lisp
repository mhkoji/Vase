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

(defstruct state
  conf
  (offset 0)
  (size 50)
  folders)

(defun load-folders (state render-folders)
  (with-container (c (state-conf state))
    (with-accessors ((db container-db)
                     (image-repos container-image-repository)
                     (folder-repos container-folder-repository)) c
      (let ((folders (vase.folder:bulk-load-by-range folder-repos
                                                     (state-offset state)
                                                     (state-size state))))
        (setf (state-folders state) folders)
        (funcall render-folders folders))))
  state)

(defun load-next-folders (state render-folders)
  (incf (state-offset state) (state-size state))
  (load-folders state render-folders))

(defun load-prev-folders (state render-folders)
  (when (>= (state-offset state) (state-size state))
    (decf (state-offset state) (state-size state))
    (load-folders state render-folders)))


(defun render-folders (folders)
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
    scrolled))


(defun main (&key (conf (vase.app.container:load-configure)))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                   :type :toplevel
                   :title "Vase"
                   :default-width 600
                   :default-height 800))
          (state (make-state :conf conf)))

      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (let ((table (make-instance 'gtk-table
                                  :n-columns 1
                                  :n-rows 20
                                  :homogeneous t))
            (scrolled nil))

        (load-folders state
         (lambda (folders)
           (setq scrolled (render-folders folders))
           (gtk-table-attach table scrolled 0 1 0 18)))

        (labels ((re-render-folders (folders)
                   (gtk-widget-destroy scrolled)
                   (setq scrolled (render-folders folders))
                   (gtk-table-attach table scrolled 0 1 0 18)
                   (gtk-widget-show-all scrolled)))
          (let ((prev-button (gtk-button-new-with-label "<"))
                (next-button (gtk-button-new-with-label ">"))
                (button-box (make-instance 'gtk-box
                                           :orientation :horizontal
                                           :spacing 12
                                           :border-width 12)))
            (gtk-box-pack-start button-box prev-button)
            (gtk-box-pack-start button-box next-button)
            (g-signal-connect prev-button "clicked"
             (lambda (w)
               (declare (ignore w))
               (load-prev-folders state #'re-render-folders)))
            (g-signal-connect next-button "clicked"
             (lambda (w)
               (declare (ignore w))
               (load-next-folders state #'re-render-folders)))
            (gtk-table-attach table button-box 0 1 18 20)))

        (gtk-container-add window table))

      (gtk-widget-show-all window))))
