SBCL ?= sbcl
LISP_FLAGS := --non-interactive

.PHONY: run run-ht run-clack

run:
	$(SBCL) $(LISP_FLAGS) \
	  --eval '(ql:quickload :glitch-simple-demo)' \
	  --eval '(datastar-cl/glitch-simple-demo:run-demo)'

