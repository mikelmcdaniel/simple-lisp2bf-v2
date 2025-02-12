# What?
This repo is a very (and untested) simple compiler from a simple python-like language supporting only the byte data type to Brain F*ck.

In practice, the python-like language is compiled to lisp-like language which is then compiled to a stack-based assembly, then finally compiled to Brain F*ck.

Its a work in progress and is not a serious project... I mean, just look at the lack of tests and documentation!

## The Language
The Python-like language only like Python in the sense that the syntax is similar... because it was easy to write code around the [Python `ast` module](https://docs.python.org/3/library/ast.html).

- There are no import statements
- No for-loops Only while-loops
- No classes or types other than the byte type
  - Really... even the function pointers and such are just a single byte
- No return statements
  - Everything, except for function definitions, are expressions and the last computation done is what's returned

The Lisp-like language is the same and was the original 'input' language when I wrote this a while ago.

# Why?
It was fun to write.

# How?

```shell
python3 py.py < py.lisp > demo.lisp  # converts the simple python-like language to a simple lisp language
python3 lisp.py < demo.lisp > demo.asm  # converts the simple lisp language to a simple stack based assembly language
python3 asm.py < demo.asm > demo.bf  # converts the stack based assembly language to Brain F*ck
```