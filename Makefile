dev:
	rm -f resources/compiled/cljs/bundle.js
	lein cljsbuild once dev

release:
	rm -f resources/compiled/cljs/bundle.js
	lein cljsbuild once release

core:
	ros run -e "\
	(progn\
	  (ql:quickload :carbonate)\
	  (require :swank)\
	  (sb-ext:save-lisp-and-die \"carbonate-sbcl.core\"))\
	"\
