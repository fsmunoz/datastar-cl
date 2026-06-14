SBCL  ?= sbcl
TARGET := fat-cqrs-snooze

.PHONY: run run-script build clean \
        run-rep-detect build-rep-detect clean-rep-detect

run: $(TARGET)
	./$(TARGET)

run-script:
	$(SBCL) --load $(TARGET).lisp --eval '(fat-cqrs-snooze:main)'

build: $(TARGET)

$(TARGET): $(TARGET).lisp style.css
	$(SBCL) --non-interactive \
	  --eval '(ql:quickload (quote (:spinneret :bordeaux-threads :snooze :datastar-cl/snooze)))' \
	  --load $(TARGET).lisp \
	  --eval '(save-lisp-and-die "$(TARGET)" :executable t :toplevel (function fat-cqrs-snooze:main) :compression (when (member :sb-core-compression *features*) t))'

clean:
	rm -f $(TARGET)
