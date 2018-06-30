(in-package :cocoa.fs.image)
(cl-annot:enable-annot-syntax)

(defstruct image-repository image-dao)

@export
(defun image-repository (image-dao)
  (make-image-repository :image-dao image-dao))

@export
(defgeneric image-insert (image-dao images))
@export
(defgeneric image-delete (image-dao ids))
@export
(defgeneric image-select (image-dao ids))
@export
(defgeneric image-row-image-id (image-row))
@export
(defgeneric image-row-path (image-path))

@export
(defun save-images (image-repository images)
  (make-image-repository
   :image-dao
   (-> (image-repository-image-dao image-repository)
       (image-insert images))))

@export
(defun delete-images (image-repository ids)
  (make-image-repository
   :image-dao
   (-> (image-repository-image-dao image-repository)
       (image-delete ids))))

(defun image-row-image (image-row)
  (make-image (image-row-image-id image-row)
              (image-row-path image-row)))

@export
(defun load-images-by-ids (image-repository ids)
  (let ((image-dao (image-repository-image-dao image-repository)))
    (->> (image-select image-dao ids)
         (mapcar #'image-row-image))))

