run: dm.bf
	python bf.py dm.bf

dm.bf: dm.asm asm.py
	python asm.py < $< > $@

dm.asm: dm.lisp lisp.py
	python lisp.py < $< > $@
