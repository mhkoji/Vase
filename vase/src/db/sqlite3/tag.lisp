(defpackage :vase.db.tag.sqlite3
  (:use :cl
        :vase.db.sqlite3
        :vase.entities.tag.db))
(in-package :vase.db.tag.sqlite3)

(defun plist->tag-row (plist)
  (make-tag-row :tag-id
                (format nil "~A"  (getf plist :|tag_id|))
                :name (getf plist :|name|)))

(defun plist->content-row (plist)
  (make-content-row :id (getf plist :|content_id|)
                    :type (getf plist :|content_type|)))

(defmethod tag-insert ((db sqlite3-db) (name string))
  (query db "INSERT INTO tags (name) VALUES (?)" (list name))
  (let ((id (cadar (query db "SELECT last_insert_rowid()"))))
    (plist->tag-row (list :|name| name :|tag_id| id))))

(defmethod tag-delete ((db sqlite3-db) (ids list))
  (query db
         (join "DELETE FROM tags WHERE tag_id IN (" (placeholder ids) ")")
         ids)
  db)

(defmethod tag-update ((db sqlite3-db) (row tag-row))
  (query db
         "UPDATE tags SET name = (?) WHERE tag_id = (?)"
         (list (tag-row-name row) (tag-row-tag-id row)))
  db)

(defmethod tag-select/range ((db sqlite3-db) offset size)
  (mapcar #'plist->tag-row
   (query db
          "SELECT tag_id, name FROM tags LIMIT ?,?"
          (list offset size))))

(defmethod tag-select/ids ((db sqlite3-db) (ids list))
  (when ids
    (mapcar #'plist->tag-row
     (query db
            (join "SELECT"
                  "  tag_id, name"
                  "FROM"
                  "  tags"
                  "WHERE"
                  "  tag_id IN (" (placeholder ids) ")")
            ids))))

(defmethod tag-content-insert ((db sqlite3-db)
                               (row content-row) (tag-ids list))
  (insert-bulk db "tag_contents" '("tag_id" "content_id" "content_type")
   (mapcar (lambda (tag-id)
             (list tag-id (content-row-id row) (content-row-type row)))
           tag-ids)))

(defmethod tag-content-delete ((db sqlite3-db)
                               (row content-row) (tag-ids list))
  (query db
         (join "DELETE"
               "FROM"
               "  tag_contents"
               "WHERE"
               "  content_id = (?)"
               "AND"
               "  content_type = (?)"
               "AND"
               "  tag_id in (" (placeholder tag-ids) ")")
         (list* (content-row-id row) (content-row-type row) tag-ids))
  db)

(defmethod tag-content-select-tags ((db sqlite3-db)
                                    (row content-row))
  (mapcar #'plist->tag-row
   (query db
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

(defmethod tag-content-select-contents ((db sqlite3-db) tag-id)
  (mapcar #'plist->content-row
   (query db
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
