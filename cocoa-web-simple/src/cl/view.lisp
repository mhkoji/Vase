(defpackage :cocoa.web-simple.view
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.web-simple.view)
(cl-annot:enable-annot-syntax)

@export
(defun html (js-src)
  (cl-who:with-html-output-to-string (s nil :prologue t)
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
     (:div :id "app")
     ; Main Javascript must be loaded after the body was rendered.
     (cl-who:htm (:script :type "text/javascript" :src js-src))
     (:script
      :type "text/javascript"
      :src "https://code.jquery.com/jquery-2.2.2.min.js"
      :integrity "sha256-36cp2Co+/62rEAAYHLmRCPIych47CvdM+uTBJwSzWjI="
      :crossorigin "anonymous")
     (:script
      :type "text/javascript"
      :src "/resources/third_party/bootstrap-4.0.0/dist/js/bootstrap.bundle.min.js")))))


(defun sub-folders (folders)
  (cl-who:with-html-output-to-string (s)
    (:div :class "card-deck"
     (loop repeat 4
           for folder in folders do
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
            (:button :type "button" :class "btn" "Tags"))))))))))

@export
(defun folder-list (folders)
  (cl-who:with-html-output-to-string (s nil :prologue t)
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
      (:div
       :id "app"
       (:main
        :class "pt-3 px-4"
        (:h1 :class "h2" Folders)
        (:main :class "pt-3 px-4"
         (:div :class "container"
          (:div :class "btn-toolbar" :role "toolbar"
           (:a :href "" :class "btn disabled"
            (:span :class "oi oi-chevron-left"))
           (:a :href "" :class "btn"
            (:span :class "oi oi-chevron-right"))))
         (:div :class "container"
          (loop for offset from 0 by 4
                for sub-folders = (nthcdr offset folders)
                while sub-folders
                do (cl-who:str (sub-folders sub-folders)))))))
      (:script
       :type "text/javascript"
       :src "https://code.jquery.com/jquery-2.2.2.min.js"
       :integrity "sha256-36cp2Co+/62rEAAYHLmRCPIych47CvdM+uTBJwSzWjI="
       :crossorigin "anonymous")
      (:script
       :type "text/javascript"
       :src "/resources/third_party/bootstrap-4.0.0/dist/js/bootstrap.bundle.min.js")))))


@export
(defun file (path)
  (funcall (lack.app.file:make-app :file path :root "/") nil))

