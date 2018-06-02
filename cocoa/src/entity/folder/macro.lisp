(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

(export 'add!)
@export
(defmacro do-add! (dao &body body)
  "The macro for adding new folders to the current system."
  (let ((gdao (gensym))
        (configs (gensym)))
    `(let ((,gdao ,dao)
           (,configs nil))
       (labels ((add! (&key id name thumbnail modified-at)
                  (push (make-folder-config
                         :id id
                         :name name
                         :thumbnail thumbnail
                         :modified-at modified-at)
                        ,configs)))
         (progn ,@body))
       (save ,gdao (nreverse ,configs))
       ,gdao)))


@export
(defmacro do-update! (dao &body body)
  "The macro for updating folders. Updated folders can be obtained after the process got out of the scope of this macro."
  (let ((gdao (gensym))
        (id->diff (gensym)))
    `(let ((,gdao ,dao)
           (,id->diff (make-hash-table :test #'equal)))
       (labels ((update! (folder-id diff)
                  (setf (gethash folder-id ,id->diff) diff)))
         (progn ,@body))
       (dolist (folder (list-by-ids ,gdao
                                    (make-list-spec)
                                    (alexandria:hash-table-keys ,id->diff)))
         (update! folder (gethash (folder-id folder) ,id->diff)))
       ,gdao)))
