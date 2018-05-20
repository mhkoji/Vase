(defpackage :cocoa.gtk
  (:use :cl
        :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo)
  (:import-from :cl-arrows
                :->))
(in-package :cocoa.gtk)
(cl-annot:enable-annot-syntax)

@export
(defun main (context)
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
      (cocoa.infra.context:with-dao (dao context)
        (dolist (folder (cocoa.use-case.folder.list:list/range
                         0 2 :folder-repository dao))
          (let* ((thumbnail-path (cocoa.use-case.image:path/id
                                  (-> folder
                                      (getf :thumbnail)
                                      (getf :id))
                                  :image-repository dao))
                 (image (gtk-image-new-from-file thumbnail-path)))
            (gtk-container-add window image))))
      (gtk-widget-show-all window))))
