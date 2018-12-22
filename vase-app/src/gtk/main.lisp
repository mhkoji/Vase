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

(defun load-folders (state show-folders)
  (with-container (c (state-conf state))
    (with-accessors ((db container-db)
                     (image-repos container-image-repository)
                     (folder-repos container-folder-repository)) c
      (let ((folders (vase.folder:bulk-load-by-range folder-repos
                                                     (state-offset state)
                                                     (state-size state))))
        (setf (state-folders state) folders)
        (funcall show-folders folders))))
  state)

(defun load-next-folders (state show-folders)
  (incf (state-offset state) (state-size state))
  (load-folders state show-folders))

(defun load-prev-folders (state show-folders)
  (when (>= (state-offset state) (state-size state))
    (decf (state-offset state) (state-size state))
    (load-folders state show-folders)))


(defun render-folders (folders on-click-open)
  (let ((vgrid (make-instance 'gtk-grid
                              :orientation :vertical
                              :border-width 8)))
    (dolist (f folders)
      (let ((frame (make-instance 'gtk-frame :shadow-type :in)))
        (let ((folder-vgrid (make-instance 'gtk-grid
                                    :orientation :vertical
                                    :border-width 8)))
          (let ((thumbnail-image
                 (gtk-image-new-from-pixbuf
                  (-> (gdk-pixbuf-new-from-file
                       (vase.image:image-path
                        (vase.folder:folder-thumbnail f)))
                      (gdk-pixbuf-scale-simple 300 300 :hyper)))))
            (gtk-container-add folder-vgrid thumbnail-image))
          (let ((folder-name-label
                 (make-instance 'gtk-label
                                :margin-bottom 3
                                :use-markup t
                                :label (vase.folder:folder-name f))))
            (gtk-container-add folder-vgrid folder-name-label))
          (let ((folder f)
                (open-folder-button (gtk-button-new-with-label "Open")))
            (g-signal-connect open-folder-button "clicked"
             (lambda (w)
               (declare (ignore w))
               (funcall on-click-open folder)))
            (gtk-container-add folder-vgrid open-folder-button))
          (gtk-container-add frame folder-vgrid))
        (gtk-container-add vgrid frame)))

    (let ((scrolled (make-instance 'gtk-scrolled-window
                                   :border-width 12
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :always)))
      (gtk-container-add scrolled vgrid)
      scrolled)))


(defun render-folder-contents (conf folder)
  (let ((vgrid (make-instance 'gtk-grid
                              :orientation :vertical
                              :border-width 8)))
    (with-container (c conf)
      (let ((images (vase.folder:folder-contents
                     folder
                     (container-db c)
                     (container-image-repository c)
                     :from 0 :size 20)))
        (push images ls)
        (dolist (image images)
          (let ((gtk-image (gtk-image-new-from-pixbuf
                            (gdk-pixbuf-new-from-file
                             (vase.image:image-path image)))))
            (gtk-container-add vgrid gtk-image)))))
    (let ((scrolled (make-instance 'gtk-scrolled-window
                                   :border-width 12
                                   :hscrollbar-policy :automatic
                                   :vscrollbar-policy :always)))
      (gtk-container-add scrolled vgrid)
      scrolled)))

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

      (let ((state (make-state :conf conf))
            (table (make-instance 'gtk-table
                                  :n-columns 1
                                  :n-rows 20
                                  :homogeneous t))
            (folders-widget nil))

        (labels ((on-click-open (f)
                   (gtk-widget-destroy table)
                   (let ((contents-widget
                          (render-folder-contents conf f)))
                     (gtk-container-add window contents-widget)
                     (gtk-widget-show-all contents-widget))))

          (load-folders state
           (lambda (folders)
             (setq folders-widget
                   (render-folders folders #'on-click-open))
             (gtk-table-attach table folders-widget 0 1 0 18)))

          (let ((prev-button (gtk-button-new-with-label "<"))
                (next-button (gtk-button-new-with-label ">"))
                (button-box (make-instance 'gtk-box
                                           :orientation :horizontal
                                           :spacing 12
                                           :border-width 12)))
            (labels ((re-show-folders (folders)
                       (gtk-widget-destroy folders-widget)
                       (setq folders-widget
                             (render-folders folders #'on-click-open))
                       (gtk-table-attach table folders-widget 0 1 0 18)
                       (gtk-widget-show-all folders-widget)))
              (gtk-box-pack-start button-box prev-button)
              (gtk-box-pack-start button-box next-button)
              (g-signal-connect prev-button "clicked"
               (lambda (w)
                 (declare (ignore w))
                 (load-prev-folders state #'re-show-folders)))
              (g-signal-connect next-button "clicked"
               (lambda (w)
                 (declare (ignore w))
                 (load-next-folders state #'re-show-folders)))
              (gtk-table-attach table button-box 0 1 18 20))))

        (gtk-container-add window table))

      (gtk-widget-show-all window))))
