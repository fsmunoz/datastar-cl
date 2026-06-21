.PHONY: run

# Start Hunchentoot server
run:
	sbcl --eval '(ql:quickload :horizons-demo)' \
	     --eval '(datastar-cl/horizons-demo:start-server)'

build build-hunchentoot:
	sbcl --disable-debugger \
	--load $(HOME)/quicklisp/setup.lisp \
	--eval "(push #P\"$(CURDIR)/../../\" asdf:*central-registry*)" \
	--eval "(push #P\"$(CURDIR)/\" asdf:*central-registry*)" \
	--eval '(ql:quickload :horizons-demo)' \
	--eval '(sb-ext:save-lisp-and-die "horizons" :toplevel (function datastar-cl/horizons-demo:run-server) :executable t :save-runtime-options nil :compression (when (member :sb-core-compression *features*) t))'

