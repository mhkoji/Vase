(in-package :vase.folder.content)

(defun bulk-delete (db folders)
  (vase.db.folder-content:delete db (mapcar #'folder-id folders)))


(defun content-id (content)
  (let ((type (string-downcase
               (symbol-name (content-type content))))
        (entity-id (content-entity-id content)))
    (format nil "~A:~A" type entity-id)))

(defun content-id->type (content-id)
  (alexandria:make-keyword
   (string-upcase
    (first (cl-ppcre:split ":" content-id)))))

(defun content-id->entity-id (content-id)
  (second (cl-ppcre:split ":" content-id)))


(defstruct appending folder contents)

(defun bulk-append (db appendings)
  (dolist (appending appendings)
    (let ((folder-id (folder-id (appending-folder appending)))
          (content-ids (mapcar #'content-id (appending-contents appending))))
      (vase.db.folder-content:insert db folder-id content-ids))))


(defun bulk-load-by-content-ids (entity-repos content-ids)
  (let ((content-id->content (make-hash-table :test #'equal)))
    (let ((type->entity-ids (make-hash-table :test #'equal)))
      (dolist (content-id content-ids)
        (let ((type (content-id->type content-id))
              (entity-id (content-id->entity-id content-id)))
          (push entity-id (gethash type type->entity-ids))))
      (maphash
       (lambda (type entity-ids)
         (let ((contents (bulk-load entity-repos type entity-ids)))
           (dolist (content contents)
             (let ((content-id (content-id content)))
               (setf (gethash content-id content-id->content) content)))))
       type->entity-ids))
    (mapcar (lambda (content-id)
              (gethash content-id content-id->content))
            content-ids)))

(labels ((safe-subseq (seq from size)
           (let* ((start (or from 0))
                  (end (when (numberp size)
                         (min (length seq) (+ size start)))))
             (subseq seq start end))))
  (defun bulk-load-by-folder (entity-repos db folder &key from size)
    (let ((all-content-ids (vase.db.folder-content:select-content-ids
                            db
                            (folder-id folder))))
      (bulk-load-by-content-ids entity-repos
                                (safe-subseq all-content-ids from size)))))
