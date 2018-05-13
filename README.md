# cocoa

A simple image viwer written in Common Lisp and ClojureScript

![sample](https://github.com/mhkoji/cocoa/raw/master/imgs/top.png)

## Set up

```
$ apt install imagemagic
```

```
$ cd /path/to/cocoa
$ make
```

## Run

1. Create a directory to store the data for the server

```
$ mkdir /tmp/cooca
$ mkdir /tmp/cocoa/thumbnails
```

2. Load the lisp files with the cocoa.asd

3. Create a context

```
CL-USER> (defvar *context*
           (cocoa.controller.context:make-context
            :digest-fn
            #'cocoa.controller.context:sha256-3
            :connection-factory
            (make-instance 'proton:sqlite3-factory
                           :db-path "/tmp/cocoa/db.sqlite3")
            :thumbnail-root "/tmp/cocoa/thumbnails/"))
```

4. Run the server with the context

```
CL-USER> (cocoa.controller.ningle:run :context *context*)
```

5. Install folder data

```
CL-USER> (cocoa.controller.cli.setup-folder:setup "./resources/contents/images/" :context *context*) 
```

Then, access http://localhost:18888/folders
