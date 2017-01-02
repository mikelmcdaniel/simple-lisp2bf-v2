run: demo.bf
	python bf.py demo.bf

%.bf: %.asm asm.py
	python asm.py < $< > $@

%.asm: %.lisp lisp.py
	python lisp.py < $< > $@

clean:
	rm -f *.pyc *.asm *.bf
