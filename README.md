# Vase

A simple image viewer written in Common Lisp and ClojureScript

![sample](https://github.com/mhkoji/vase/raw/master/imgs/top.png)

## Set up

1. Install imagemagick

```
$ apt install imagemagick
```

2. Install Leiningen


3. Compile the ClojureScript sources of vase.

```
$ cd /path/to/vase/vase-spa
$ make
```

## Run the web server

1. Create directories to store the data for the server

```
$ mkdir /tmp/vase
$ mkdir /tmp/vase/thumbnails
```

2. Load the lisp files using vase.asd

```
CL-USER> (ql:quickload :vase-spa)
```

3. Create a context

```
CL-USER> (defvar *context*
           (vase:make-context
            :id-generator
            (make-instance 'vase.entities.id:sha256-3)
            :connection-factory
            (make-instance 'proton:sqlite3-factory
                           :db-path "/tmp/vase/db.sqlite3")
            :thumbnail-root "/tmp/vase/thumbnails/"))
```

4. Run the server with the context

```
CL-USER> (vase.spa:run :context *context*)
```

5. Add the sample folders

```
CL-USER> (ql:quickload :vase-cli)
CL-USER> (vase.cli.add-folders:execute "./resources/contents/images/" :context *context*) 
```

Then, access http://localhost:18888/folders
