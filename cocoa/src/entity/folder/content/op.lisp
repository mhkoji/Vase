(defpackage :cocoa.entity.folder.content.op
  (:use :cl))
(in-package :cocoa.entity.folder.content.op)

(defstruct deleting-bulk folders)
(export '(deleteing-bulk
          deleteing-bulk-folders
          make-deleting-bulk))


(defstruct appending
  "The object that represents the action of appending contents to a folder"
  folder contents)
(export '(appending
          appending-folder
          appending-contents
          make-appending))


(defstruct moving
  "The object that represents the action of moving contents from a folder to another folder"
  source target contents)
(export '(moving
          moving-source
          moving-target
          moving-contents
          make-moving))
