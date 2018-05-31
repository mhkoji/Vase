(defpackage :cocoa.gtk
  (:use :cl
        :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo)
  (:import-from :cl-arrows
                :->))
(in-package :cocoa.gtk)
(cl-annot:enable-annot-syntax)

(defun folders (context from &optional (size 5))
  (let ((frame (make-instance 'gtk-grid :shadow-type :in)))
    (cocoa.infra.context:with-dao (dao context)
      (dolist (folder (cocoa.use-case.folder:list/range
                       from size :folder-dao dao))
        (let ((thumbnail-frame
               (make-instance 'gtk-frame :shadow-type :in)))
          (let* ((thumbnail-path (cocoa.use-case.image:path/id
                                  (-> folder
                                      (getf :thumbnail)
                                      (getf :id))
                                  :image-dao dao))
                 (image (gtk-image-new-from-file thumbnail-path)))
            (gtk-container-add thumbnail-frame image))
          (gtk-container-add frame thumbnail-frame))))
    frame))

@export
(defun main (&optional (context (cocoa.infra.context:load-context)))
  (let ((index 0)
        (frame nil))
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cocoa"
                                 :default-width 500
                                 :default-height 500)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (let ((vgrid (make-instance 'gtk-grid
                                  :orientation :vertical
                                  :border-width 8)))
        (let ((title (gtk-label-new "Folders")))
          (gtk-container-add vgrid title))

        (let ((button (make-instance 'gtk-button :label ">")))
          (gtk-container-add vgrid button)

          (gtk-container-add vgrid (setq frame (folders context index)))

          (g-signal-connect button "clicked"
           (lambda (button)
             (declare (ignore button))
             (gtk-widget-destroy frame)
             (incf index 3)
             (gtk-container-add vgrid (setq frame (folders context index)))
             (gtk-widget-show-all window))))

        (gtk-container-add window vgrid))
      (gtk-widget-show-all window)))))
