WOO_PATH ?= $(HOME)/src/lisp/woo

.PHONY: run run-clack build-clack

run:
	sbcl --eval '(ql:quickload :glitch-demo/hunchentoot)' \
	     --eval '(datastar-cl/glitch-demo:start-server)'

run-clack:
	sbcl --eval "(push #P\"$(WOO_PATH)/\" asdf:*central-registry*)" \
	     --eval '(ql:quickload :glitch-demo/clack)' \
	     --eval '(datastar-cl/glitch-demo:run-clack-server)'

build-clack:
	sbcl --disable-debugger \
	     --load $(HOME)/quicklisp/setup.lisp \
	     --eval "(push #P\"$(CURDIR)/../../\" asdf:*central-registry*)" \
	     --eval "(push #P\"$(WOO_PATH)/\" asdf:*central-registry*)" \
	     --eval "(push #P\"$(CURDIR)/\" asdf:*central-registry*)" \
	     --eval '(ql:quickload :glitch-demo/clack)' \
	     --eval '(sb-ext:save-lisp-and-die "glitch-woo" \
	               :toplevel (function datastar-cl/glitch-demo:main-clack) \
	               :executable t \
	               :save-runtime-options nil \
	               :compression (when (member :sb-core-compression *features*) t))'
