# Build mingl

test: t/*.lisp *.lisp *.asd Makefile
	sbcl --eval "(ql:quickload :mingl.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
