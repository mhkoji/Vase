# cocoa

A simple image viewer written in Common Lisp and ClojureScript

![sample](https://github.com/mhkoji/cocoa/raw/master/imgs/top.png)

## Set up

1. Install imagemagick

```
$ apt install imagemagick
```

2. Install Leiningen


3. Compile the ClojureScript sources of cocoa.

```
$ cd /path/to/cocoa/cocoa-web
$ make
```

## Run the web server

1. Create directories to store the data for the server

```
$ mkdir /tmp/cooca
$ mkdir /tmp/cocoa/thumbnails
```

2. Load the lisp files using cocoa.asd

```
CL-USER> (ql:quickload :cocoa-web)
```

3. Create a context

```
CL-USER> (defvar *context*
           (cocoa.dependency.injection:make-context
            :id-generator
            (make-instance 'cocoa.entity.id:sha256-3)
            :connection-factory
            (make-instance 'proton:sqlite3-factory
                           :db-path "/tmp/cocoa/db.sqlite3")
            :thumbnail-root "/tmp/cocoa/thumbnails/"))
```

4. Run the server with the context

```
CL-USER> (cocoa.web:run :context *context*)
```

5. Add the sample folders

```
CL-USER> (cocoa.web:add-folders "./resources/contents/images/" :context *context*) 
```

Then, access http://localhost:18888/folders
