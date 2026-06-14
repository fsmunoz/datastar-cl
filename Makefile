SBCL ?= sbcl
LISP_FLAGS := --non-interactive

.PHONY: test test-unit test-smoke test-sdk \
        profile-in-process profile-http docs

QUIT := (finish-output) (sb-ext:exit :code (if r 0 1) :abort t)
RUN_SUITE = $(SBCL) $(LISP_FLAGS) \
	  --eval '(ql:quickload :datastar-cl-tests/integration)' \
	  --eval '(let ((r (datastar-cl/tests/integration:run-suite $(1)))) $(QUIT))'

test:
	$(SBCL) $(LISP_FLAGS) \
	  --eval '(ql:quickload :datastar-cl-tests/integration)' \
	  --eval '(let ((r (datastar-cl/tests/integration:run-all-tests))) $(QUIT))'

test-unit:
	$(SBCL) $(LISP_FLAGS) \
	  --eval '(ql:quickload :datastar-cl-tests/unit)' \
	  --eval '(let ((r (datastar-cl/tests/unit:run-all-tests))) $(QUIT))'

test-smoke:
	$(call RUN_SUITE,:datastar-cl/integration/smoke)

test-sdk:
	$(call RUN_SUITE,:datastar-cl/integration/sdk)

profile-in-process:
	$(SBCL) $(LISP_FLAGS) \
	  --eval '(ql:quickload :datastar-cl/profile)' \
	  --eval '(datastar-cl/profile:profile-in-process)'

docs:
	$(SBCL) $(LISP_FLAGS) --load docs/make-docs.lisp
	makeinfo --plaintext docs/datastar-cl.texi -o docs/datastar-cl.txt
	makeinfo --plaintext docs/datastar-cl-snooze.texi -o docs/datastar-cl-snooze.txt
	rm -rf docs/datastar-cl.html docs/datastar-cl-snooze.html
	makeinfo --no-headers --no-split --html docs/datastar-cl.texi -o docs/datastar-cl.html
	makeinfo --no-headers --no-split --html docs/datastar-cl-snooze.texi -o docs/datastar-cl-snooze.html

profile-http:
	$(SBCL) $(LISP_FLAGS) \
	  --eval '(ql:quickload :datastar-cl-tests)' \
	  --eval '(ql:quickload :datastar-cl/profile)' \
	  --eval '(datastar-cl-tests::start-all-servers)' \
	  --eval '(sleep 1)' \
	  --eval '(datastar-cl/profile:profile-http)' \
	  --eval '(datastar-cl-tests::stop-all-servers)'
