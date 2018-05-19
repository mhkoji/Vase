dev:
	rm -f resources/compiled/cljs/bundle.js
	lein cljsbuild once dev

release:
	rm -f resources/compiled/cljs/bundle.js
	lein cljsbuild once release

core:
	ros run -e "\
	(progn\
	  (ql:quickload :cocoa)\
	  (require :swank)\
	  (sb-ext:save-lisp-and-die \"cocoa-sbcl.core\"))\
	"\
