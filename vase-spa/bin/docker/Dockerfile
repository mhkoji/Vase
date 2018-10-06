FROM ubuntu:16.04


## Initialize the working environment
RUN apt-get update && apt-get install -y\
 curl\
 make\
 openjdk-8-jre-headless\
 sbcl


## Setup sbcl environment (SBCL and Quicklisp)
RUN mkdir /tmp/setup-sbcl-environment\
  && cd /tmp/setup-sbcl-environment\
  && curl -O https://beta.quicklisp.org/quicklisp.lisp\
  && sbcl --load /tmp/setup-sbcl-environment/quicklisp.lisp\
          --eval "(quicklisp-quickstart:install)"\
          --eval "(ql-util:without-prompting (ql:add-to-init-file))"


## Install Leiningen
RUN cd /usr/local/bin/\
  && curl -O https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein\
  && chmod a+x ./lein\
  && lein


## Build app

# Caching some libraries...
RUN sbcl --eval "(progn\
  (ql:quickload :cl-dbi)\
  (ql:quickload :cl-interpol)\
  (ql:quickload :sqlite)\

  (ql:quickload :alexandria)\
  (ql:quickload :anaphora)\
  (ql:quickload :babel)\
  (ql:quickload :cl-annot)\
  (ql:quickload :cl-arrows)\
  (ql:quickload :cl-fad)\
  (ql:quickload :cl-ppcre)\
  (ql:quickload :ironclad))"

RUN sbcl --eval "(progn\
  (ql:quickload :cl-who)\
  (ql:quickload :cl-arrows)\
  (ql:quickload :clack)\
  (ql:quickload :jsown)\
  (ql:quickload :log4cl)\
  (ql:quickload :lack)\
  (ql:quickload :lack-middleware-static)\
  (ql:quickload :ningle)\
  (ql:quickload :swank))"


COPY vase /root/quicklisp/local-projects/vase
COPY vase-spa /root/quicklisp/local-projects/vase-spa

# Make the lisp image for the app
WORKDIR /app

RUN /root/quicklisp/local-projects/vase-spa/bin/lisp-image/build-sbcl\
  && cp /root/quicklisp/local-projects/vase-spa/bin/docker/run ./

# Make the resources for the app
RUN cd /root/quicklisp/local-projects/vase-spa\
  && make release\
  && cp -r resources /app

EXPOSE 11111

CMD ["./run"]
