(defpackage :vase.gui.folder.js
  (:use :cl
        :vase.entities.tag
        :vase.entities.tag-edit
        :vase.entities.folder))
(in-package :vase.gui.folder)

(defun make-gui ()
  (lambda (m db)
    (case m
      (:folder.show
       (let ((folders (vase.app.folder.repository:load-all)))
         (#j:vase:presenter:browser:pages:folders:page
          (mapcar (lambda ()
                    (let ((obj (new)))
                      (setf (oget obj "folder-id")
                            (folder-id folder))
                      (setf (oget obj "name")
                            (folder-name folder))
                      (setf (oget obj "thumbnail-url")
                            (thumbnail-url (folder-thumbnail folder)))
                      (setf (oget obj "on-tag-edit-start")
                            (lambda ()
                              (vase.use-cases.folder:tag-edit-start
                               db :folder-id (folder-id folder))))))
                  folders)))))))
