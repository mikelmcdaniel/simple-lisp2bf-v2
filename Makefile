run: demo.bf
	python3 bf.py demo.bf

test: *.py
	python3 test.py

%.bf: %.asm asm.py
	python3 asm.py < $< > $@

%.asm: %.lisp lisp.py
	python3 lisp.py < $< > $@

%.lisp: %.py py.py
	python3 py.py < $< > $@

clean:
	rm -f *.pyc *.asm *.bf *.lisp
