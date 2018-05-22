(defpackage :cocoa.infra.db.tag.sqlite3
  (:use :cl
        :cocoa.infra.db.sqlite3
        :cocoa.infra.db.tag.dao))
(in-package :cocoa.infra.db.tag.sqlite3)

(defun plist->tag-row (plist)
  (make-tag-row :tag-id
                (format nil "~A"  (getf plist :|tag_id|))
                :name (getf plist :|name|)))

(defun plist->content-row (plist)
  (make-content-row :id (getf plist :|content_id|)
                    :type (getf plist :|content_type|)))

(defmethod tag-insert ((dao sqlite3-dao) (name string))
  (query dao "INSERT INTO tags (name) VALUES (?)" (list name))
  (let ((id (cadar (query dao "SELECT last_insert_rowid()"))))
    (plist->tag-row (list :|name| name :|tag_id| id))))

(defmethod tag-delete ((dao sqlite3-dao) (ids list))
  (query dao
         (join "DELETE FROM tags WHERE tag_id IN (" (placeholder ids) ")")
         ids))

(defmethod tag-update ((dao sqlite3-dao) (row tag-row))
  (query dao
         "UPDATE tags SET name = (?) WHERE tag_id = (?)"
         (list (tag-row-name row) (tag-row-tag-id row))))

(defmethod tag-select/range ((dao sqlite3-dao) offset size)
  (mapcar #'plist->tag-row
   (query dao
          "SELECT tag_id, name FROM tags LIMIT ?,?"
          (list offset size))))

(defmethod tag-select/ids ((dao sqlite3-dao) (ids list))
  (when ids
    (mapcar #'plist->tag-row
     (query dao
            (join "SELECT"
                  "  tag_id, name"
                  "FROM"
                  "  tags"
                  "WHERE"
                  "  tag_id IN (" (placeholder ids) ")")
            ids))))

(defmethod tag-content-insert ((dao sqlite3-dao)
                               (row content-row) (tag-ids list))
  (insert-bulk dao "tag_contents" '("tag_id" "content_id" "content_type")
   (mapcar (lambda (tag-id)
             (list tag-id (content-row-id row) (content-row-type row)))
           tag-ids)))

(defmethod tag-content-delete ((dao sqlite3-dao)
                               (row content-row) (tag-ids list))
  (query dao
         (join "DELETE"
               "FROM"
               "  tag_contents"
               "WHERE"
               "  content_id = (?)"
               "AND"
               "  content_type = (?)"
               "AND"
               "  tag_id in (" (placeholder tag-ids) ")")
         (list* (content-row-id row) (content-row-type row) tag-ids)))

(defmethod tag-content-select-tags ((dao sqlite3-dao)
                                    (row content-row))
  (mapcar #'plist->tag-row
   (query dao
          (join "SELECT"
                "  tags.tag_id, tags.name"
                "FROM ( tags INNER JOIN tag_contents"
                "  ON"
                "    tags.tag_id = tag_contents.tag_id"
                ")"
                "WHERE"
                "  tag_contents.content_id = (?)"
                "AND"
                "  tag_contents.content_type = (?)")
          (list (content-row-id row) (content-row-type row)))))

(defmethod tag-content-select-contents ((dao sqlite3-dao) tag-id)
  (mapcar #'plist->content-row
   (query dao
          (join "SELECT"
                "  tag_contents.content_id,"
                "  tag_contents.content_type"
                "FROM ( tag_contents INNER JOIN tags"
                "  ON"
                "    tag_contents.tag_id = tags.tag_id"
                ")"
                "WHERE"
                "  tags.tag_id = ?"
                "ORDER BY"
                "  tag_contents.id DESC")
          (list tag-id))))
