(defpackage :vase.tag.repos.sqlite3
  (:use :cl
        :vase.db.sqlite3
        :vase.tag.repos.db)
  (:shadowing-import-from :vase.tag.repos.db :delete))
(in-package :vase.tag.repos.sqlite3)

(defun plist->row (plist)
  (make-row
   :tag-id (format nil "~A"  (getf plist :|tag_id|))
   :name (getf plist :|name|)))

(defun plist->content-row (plist)
  (make-content-row
   :id (getf plist :|content_id|)
   :type (getf plist :|content_type|)))

(defmethod insert ((db sqlite3-db) (name string))
  (query db "INSERT INTO tags (name) VALUES (?)" (list name))
  (let ((id (cadar (query db "SELECT last_insert_rowid()"))))
    (plist->row (list :|name| name :|tag_id| id))))

(defmethod delete ((db sqlite3-db) (ids list))
  (query db
         (join "DELETE FROM tags WHERE tag_id IN (" (placeholder ids) ")")
         ids)
  db)

(defmethod update ((db sqlite3-db) (row row))
  (query db
         "UPDATE tags SET name = (?) WHERE tag_id = (?)"
         (list (row-name row) (row-tag-id row)))
  db)

(defmethod select-by-range ((db sqlite3-db) offset size)
  (mapcar #'plist->row
   (query db
          "SELECT tag_id, name FROM tags LIMIT ?,?"
          (list offset size))))

(defmethod select-by-ids ((db sqlite3-db) (ids list))
  (when ids
    (mapcar #'plist->row
     (query db
            (join "SELECT"
                  "  tag_id, name"
                  "FROM"
                  "  tags"
                  "WHERE"
                  "  tag_id IN (" (placeholder ids) ")")
            ids))))

(defmethod content/insert ((db sqlite3-db)
                           (row content-row) (tag-ids list))
  (insert-bulk db "tag_contents" '("tag_id" "content_id" "content_type")
   (mapcar (lambda (tag-id)
             (list tag-id (content-row-id row) (content-row-type row)))
           tag-ids)))

(defmethod content/delete ((db sqlite3-db)
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

(defmethod content/select ((db sqlite3-db) tag-id)
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

(defmethod content/select-tags ((db sqlite3-db) (row content-row))
  (mapcar #'plist->row
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
