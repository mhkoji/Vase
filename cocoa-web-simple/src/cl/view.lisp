(defpackage :cocoa.web-simple.view
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.web-simple.view)
(cl-annot:enable-annot-syntax)

(defmacro with-html (body)
  `(cl-who:with-html-output-to-string (s nil :prologue t)
     (:head
      (:meta :charset "utf-8")
      (:title "Cocoa")
      (:link
       :rel "stylesheet"
       :type "text/css"
       :href "/resources/css/main.css")
      (:link
       :rel "stylesheet"
       :type "text/css"
       :href "/resources/third_party/bootstrap-4.0.0/dist/css/bootstrap.min.css")
      (:link
       :rel "stylesheet"
       :href "/resources/third_party/open-iconic-1.1.0/font/css/open-iconic-bootstrap.css")

      (:body
       ,body
       (:script
        :type "text/javascript"
        :src "https://code.jquery.com/jquery-2.2.2.min.js"
        :integrity "sha256-36cp2Co+/62rEAAYHLmRCPIych47CvdM+uTBJwSzWjI="
        :crossorigin "anonymous")
       (:script
        :type "text/javascript"
        :src "/resources/third_party/bootstrap-4.0.0/dist/js/bootstrap.bundle.min.js")))))


(defun folders (folders)
  (cl-who:with-html-output-to-string (s)
    (:div :class "container"
     (:div :class "card-deck"
      (loop for rest-folders = folders
                             then (nthcdr 4 rest-folders) do
        (dolist (folder (subseq rest-folders
                                0 (min 4 (length rest-folders))))
          (cl-who:htm
           (:div :class "card mb-4 box-shadow"
            (:a :href ""
             (:img :src (cl-who:str
                         (format nil "/_i/~A"
                          (-> folder (getf :thumbnail) (getf :id))))
                   :class "card-img-top"))
            (:div :class "card-body"
             (:div :class "card-title"
              (cl-who:str (-> folder (getf :name)))
              (:p
               (:button :type "button" :class "btn" "Tags"))))))))))))

(defun nav ()
  (cl-who:with-html-output-to-string (s)
    (:div :class "container"
     (:div :class "btn-toolbar" :role "toolbar"
      (:a :href "" :class "btn disabled"
       (:span :class "oi oi-chevron-left"))
      (:a :href "" :class "btn"
       (:span :class "oi oi-chevron-right"))))))

@export
(defun folder-list (folders)
  (with-html
    (:div :id "app"
     (:main :class "pt-3 px-4"
      (:h1 :class "h2" Folders)
      (:main :class "pt-3 px-4"
       (cl-who:str (nav))
       (cl-who:str (folders folders)))))))


@export
(defun file (path)
  (funcall (lack.app.file:make-app :file path :root "/") nil))
