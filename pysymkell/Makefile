GHC=ghc

libffi-example.so: Example.o
	$(GHC) -o $@ -shared -dynamic -fPIC $^  -flink-rts 

Example_stub.h Example.o: Example.hs
	$(GHC) -c -dynamic -fPIC Example.hs

clean:
	rm -f *.hi *.o *_stub.[ch]

clean-all:
	rm -f *.hi *.o *_stub.[ch] *.so


# Runs the example Python program
example: libffi-example.so
	python program.py
