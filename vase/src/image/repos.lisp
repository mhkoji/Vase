(in-package :vase.image)

(defstruct repository db)

(defun image->image-row (image)
  (vase.image.db:make-row :image-id (image-id image)
                          :path (image-path image)))

(defun image-row->image (image-row)
  (make-instance 'image
   :id (vase.image.db:row-image-id image-row)
   :path (vase.image.db:row-path image-row)))

(defun bulk-save (repos images)
  (let ((db (repository-db repos)))
    (vase.image.db:insert db (mapcar #'image->image-row images))))

(defun bulk-load-by-ids (repos ids)
  (let ((db (repository-db repos)))
    (mapcar #'image-row->image (vase.image.db:select db ids))))

(defun bulk-delete (repos image-ids)
  (vase.image.db:delete (repository-db repos) image-ids))
