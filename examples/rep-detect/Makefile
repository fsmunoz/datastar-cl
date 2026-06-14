SBCL ?= sbcl

# Absolute path to the local patched Woo clone (branch cqrs).
# Must be on the ASDF source registry before rep-detect-demo/clack is loaded.
WOO_PATH ?= $(HOME)/src/lisp/woo

.PHONY: run build run-clack build-clack

# --- Hunchentoot backend ---

# Load the system and start the server, blocking the main thread.
run:
	$(SBCL) --eval '(ql:quickload :rep-detect-demo/hunchentoot)' \
	        --eval '(datastar-cl/rep-detect-demo:run-server)'

# Build a standalone Hunchentoot executable named rep-detect.
build:
	$(SBCL) --non-interactive \
	        --eval '(ql:quickload :rep-detect-demo/hunchentoot)' \
	        --eval "(sb-ext:save-lisp-and-die \"rep-detect\" \
	                  :toplevel (function datastar-cl/rep-detect-demo:main) \
	                  :executable t \
	                  :compression (when (member :sb-core-compression *features*) t))"

# --- Clack + Woo backend ---

# Load the system and start the Woo server, blocking the main thread.
run-clack:
	$(SBCL) --eval "(push #p\"$(WOO_PATH)/\" asdf:*central-registry*)" \
	        --eval '(ql:quickload :rep-detect-demo/clack)' \
	        --eval '(datastar-cl/rep-detect-demo:run-clack-server)'

# Build a standalone Clack+Woo executable named rep-detect-woo.
build-clack:
	$(SBCL) --non-interactive \
	        --eval "(push #p\"$(WOO_PATH)/\" asdf:*central-registry*)" \
	        --eval '(ql:quickload :rep-detect-demo/clack)' \
	        --eval "(sb-ext:save-lisp-and-die \"rep-detect-woo\" \
	                  :toplevel (function datastar-cl/rep-detect-demo:main-clack) \
	                  :executable t \
	                  :compression (when (member :sb-core-compression *features*) t))"
