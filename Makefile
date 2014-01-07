.PHONY: t

all:
	./py2llvm.py test.py

t:
	./py2llvm.py t/arith_test.py
