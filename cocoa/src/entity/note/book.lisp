(defpackage :cocoa.entity.note.book
  (:use :cl))
(in-package :cocoa.entity.note.book)
(cl-annot:enable-annot-syntax)

@export
(defgeneric book-id (memo))
@export
(defgeneric book-memos ())
@export
(defgeneric book-add-memo! (book memo))

@export
(defgeneric make-new-book ())

@export
(defgeneric save-book (bookshelf book))
@export
(defgeneric select-books (bookshelf query))
