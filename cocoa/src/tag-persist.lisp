(in-package :cocoa.tag)
(cl-annot:enable-annot-syntax)

(defstruct tag-row tag-id name)
(export 'make-tag-row)
(export 'tag-row)
(export 'tag-row-tag-id)
(export 'tag-row-name)

(defstruct content-row id type)
(export 'make-content-row)
(export 'content-row)
(export 'content-row-id)
(export 'content-row-type)

(defun content-row-content (row)
  (make-instance 'content
   :id (content-row-id row)
   :type (alexandria:make-keyword (content-row-type row))))

(defun content-content-row (content)
  (make-content-row
   :id (content-id content)
   :type (symbol-name (content-type content))))

(defstruct tag-repository tag-dao)

@export
(defun tag-repository (tag-dao)
  (make-tag-repository :tag-dao tag-dao))

@export
(defgeneric tag-insert (tag-dao name))
@export
(defgeneric tag-delete (tag-dao tag-id-list))
@export
(defgeneric tag-update (tag-dao tag-row))
@export
(defgeneric tag-select/ids (tag-dao ids))
@export
(defgeneric tag-select/range (tag-dao offset size))

@export
(defgeneric tag-content-insert (tag-dao content-row tag-id-list))
@export
(defgeneric tag-content-delete (tag-dao content-row tag-id-list))
@export
(defgeneric tag-content-select-tags (tag-dao content-row))
@export
(defgeneric tag-content-select-contents (tag-dao tag-id))

(defclass tag ()
  ((row :initarg :row :reader tag-row)
   (dao :initarg :dao :reader tag-dao)))

(defmethod tag-id ((tag tag))
  (tag-row-tag-id (tag-row tag)))

(defmethod tag-name ((tag tag))
  (tag-row-name (tag-row tag)))

(defmethod (setf tag-name) (new-name (tag tag))
  (setf (tag-row-name (tag-row tag)) new-name))

(defmethod tag-contents ((tag tag))
  (->> (tag-content-select-contents (tag-dao tag) (tag-id tag))
       (mapcar #'content-row-content)))

@export
(defun attach-tag (tag content)
  (with-accessors ((tag-id tag-id) (dao tag-dao)) tag
    (tag-content-insert dao (content-content-row content) (list tag-id))))

@export
(defun detach-tag (tag content)
  (with-accessors ((tag-id tag-id) (dao tag-dao)) tag
    (tag-content-delete dao (content-content-row content) (list tag-id))))


@export
(defun make-tag (tag-repository name)
  (the tag-row (tag-insert (tag-repository-tag-dao tag-repository)
                           name)))

@export
(defun save-tag (tag-repository tag-row)
  (declare (ignore tag-row))
  ;; Already saved when row was made
  tag-repository)

@export
(defun update-tag (tag-repository tag)
  (make-tag-repository
   :tag-dao (-> (tag-repository-tag-dao tag-repository)
                (tag-update (tag-row tag)))))

@export
(defun load-tags-by-range (tag-repository offset size)
  (let ((tag-dao (tag-repository-tag-dao tag-repository)))
    (mapcar (lambda (row)
              (make-instance 'tag :dao tag-dao :row row))
            (tag-select/range tag-dao offset size))))

@export
(defun load-tags-by-ids (tag-repository ids)
  (let ((tag-dao (tag-repository-tag-dao tag-repository)))
    (mapcar (lambda (row)
              (make-instance 'tag :dao tag-dao :row row))
            (tag-select/ids tag-dao ids))))

@export
(defun load-tags-by-content (tag-repository content)
  (let ((tag-dao (tag-repository-tag-dao tag-repository)))
    (mapcar (lambda (row) (make-instance 'tag :row row :dao tag-dao))
            (tag-content-select-tags
             tag-dao
             (content-content-row content)))))

@export
(defun delete-tags (tag-repository ids)
  (let ((tag-dao (tag-repository-tag-dao tag-repository)))
    (dolist (tag (load-tags-by-ids tag-repository ids))
      (let ((tag-ids (list (tag-id tag))))
        (dolist (content (tag-contents tag))
          (let ((content-row (content-content-row content)))
            (setq tag-dao (tag-content-delete
                           tag-dao content-row tag-ids)))))))
  tag-repository)
