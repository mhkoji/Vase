(in-package :vase.image)

(defstruct repository db)

(defun image->image-row (image)
  (vase.db.image:make-row :image-id (image-id image)
                          :path (image-path image)))

(defun image-row->image (image-row)
  (make-instance 'image
   :id (vase.db.image:row-image-id image-row)
   :path (vase.db.image:row-path image-row)))

(defun bulk-save (repos images)
  (let ((db (repository-db repos)))
    (vase.db.image:insert db (mapcar #'image->image-row images))))

(defun bulk-load-by-ids (repos ids)
  (let ((db (repository-db repos)))
    (mapcar #'image-row->image (vase.db.image:select db ids))))

(defun bulk-delete (repos image-ids)
  (vase.db.image:delete (repository-db repos) image-ids))
