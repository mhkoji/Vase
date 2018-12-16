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
$ cd /path/to/vase/vase-webapp
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
CL-USER> (ql:quickload :vase-webapp)
```

3. Create a configure

```
CL-USER> (defvar *conf*
           (vase.context.configure:make-configure
            :id-generator
            (make-instance 'vase.id:sha256-3)
            :connection-factory
            (make-instance 'proton:sqlite3-factory
                           :db-path "/tmp/vase/db.sqlite3")
            :thumbnail-root "/tmp/vase/thumbnails/"))
```

4. Run the server with the configure

```
CL-USER> (vase.webapp:run :conf *conf*)
```

5. Add the sample folders

```
CL-USER> (ql:quickload :vase-cli)
CL-USER> (vase.cli:add-folders "./resources/contents/images/" :conf *conf* :initialize-data-p t)
```

Then, access http://localhost:18888/folders
