import itertools
from sys import stdin, stdout, argv
from typing import Optional, Iterator, Union, Any


class CompilerException(Exception):
  pass

class VarStack:
  def __init__(self, stacks: list[list[Optional[str]]]=None):
    if stacks is None:
      stacks = []
    self.stacks: list[list[Optional[str]]] = stacks
    self.push_stack()

  def push_stack(self) -> None:
    self.stacks.append([])

  def pop_stack(self) -> None:
    self.stacks.pop()

  def push(self, name: Optional[str]=None) -> None:
      self.stacks[-1].append(name)

  def pop(self, index: Optional[int]=None) -> None:
    if index is None:
      self.stacks[-1].pop()
    else:
      del self.stacks[-1][index]

  def get_rel_pos(self, name: str) -> int:
    offset = 0
    for stack in reversed(self.stacks):
      for variable in reversed(stack):
        offset -= 1
        if variable == name:
          return offset
    raise CompilerException(f'{self}.get_pos({name!r}) failed')

  def __contains__(self, name: str) -> bool:
    return any(name in stack for stack in reversed(self.stacks))

  def __str__(self) -> str:
    return repr(self)

  def __repr__(self) -> str:
    return f'VarStack({self.stacks!r})'

def lisp_tokens(lisp_str: str) -> Iterator[str]:
  start = 0
  j = 0
  while j < len(lisp_str):
    c = lisp_str[j]
    if c in ('(', ')'):
      if start != j:
        yield lisp_str[start:j]
      yield c
      start = j + 1
    elif c in ' \t\n':
      if start != j:
        yield lisp_str[start:j]
      start = j + 1
    elif c in '"' and start == j:
      start = j
      j += 1
      while lisp_str[j] not in '"':
        j += 1
      yield lisp_str[start:j + 1]
      start = j + 1
    elif c == ';':
      while lisp_str[j] != '\n':
        j += 1
      start = j + 1
    j += 1

def lisp_to_lists(lisp_str: str) -> list[Any]:
  stack: list[list[Any]] = [[]]
  for token in lisp_tokens(lisp_str):
    if token == '(':
      stack.append([])
    elif token == ')':
      last_str = stack.pop()
      stack[-1].append(last_str)
    elif token[0] == '"' and token[-1] == '"':
      for c in token[1:-1].replace('\\n', '\n').replace('\\t', '\t'):
        stack[-1].append(f"'{c}'")
    else:
      stack[-1].append(token)
  return stack[0]

def lisp_form_to_asm_lines(lisp, var_stack: Optional[VarStack]=None, label_generator: Iterator[str]=None) -> list[str]:
  if var_stack is None:
    var_stack = VarStack()
  if label_generator is None:
    label_generator = iter(f'$LABEL_{j}' for j in itertools.count())
  output = []
  for form in lisp:
    if isinstance(form, list):
      op = form[0]
      if op == '+':
        assert len(form) >= 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.extend('add' for j in range(len(form) - 2))
        for _ in range(len(form) - 1):
          var_stack.pop()
        var_stack.push()
      elif op == '-':
        assert len(form) >= 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.extend('add' for j in range(len(form) - 3))
        output.append('sub')
        for _ in range(len(form) - 1):
          var_stack.pop()
        var_stack.push()
      elif op == '*':
        assert len(form) >= 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.extend('mul' for j in range(len(form) - 2))
        for _ in range(len(form) - 1):
          var_stack.pop()
        var_stack.push()
      elif op == '**':
        assert len(form) >= 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.extend('exp' for j in range(len(form) - 2))
        for _ in range(len(form) - 1):
          var_stack.pop()
        var_stack.push()
      elif op == '/':
        assert len(form) == 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.append('div')
        var_stack.pop()
        var_stack.pop()
        var_stack.push()
      elif op == '%':
        assert len(form) == 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.append('mod')
        var_stack.pop()
        var_stack.pop()
        var_stack.push()
      elif op == '=' or op == 'eq':
        assert len(form) == 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.append('eq')
        var_stack.pop()
        var_stack.pop()
        var_stack.push()
      elif op == '!=' or op == 'neq':
        assert len(form) == 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.append('eq')
        var_stack.pop()
        var_stack.pop()
        var_stack.push()
        output.append('not')
      elif op == '<=':
        assert len(form) == 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.append('lesseq')
        var_stack.pop()
        var_stack.pop()
        var_stack.push()
      elif op == '<':
        assert len(form) == 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.append('less')
        var_stack.pop()
        var_stack.pop()
        var_stack.push()
      elif op == '>=':
        assert len(form) == 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.append('greatereq')
        var_stack.pop()
        var_stack.pop()
        var_stack.push()
      elif op == '>':
        assert len(form) == 3
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        output.append('greater')
        var_stack.pop()
        var_stack.pop()
        var_stack.push()
      elif op == 'not':
        assert len(form) == 2
        output.extend(lisp_form_to_asm_lines(form[1], var_stack, label_generator))
        output.append('not')
        var_stack.pop()
        var_stack.push()
      elif op == 'bool':
        assert len(form) == 2
        output.extend(lisp_form_to_asm_lines(form[1], var_stack, label_generator))
        output.append('bool')
        var_stack.pop()
        var_stack.push()
      elif op == 'read':
        assert len(form) == 1
        output.append('read')
        var_stack.push()
      elif op == 'write':
        assert len(form) >= 1
        output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
        num_writes = len(form) - 1
        output.append(f'write {num_writes}')
        if num_writes > 1:
          output.append(f'set {-num_writes}')
          var_stack.pop()
          for _ in range(num_writes - 2):
            output.append('pop')
            var_stack.pop()
      elif op == 'exit':
        output.append('exit')
        var_stack.push()
        output.append(f'{next(label_generator)}:')
      elif op == 'defun':
        assert len(form) >= 4
        assert isinstance(form[1], str)
        assert isinstance(form[2], list)
        num_args = len(form[2])
        var_stack.push_stack()
        var_stack.push('$RETURN_ADDR')
        output.append(f'{form[1]}:')
        for var_name in form[2]:
          assert isinstance(var_name, str)
          var_stack.push(var_name)
        output.extend(lisp_form_to_asm_lines([form[3]], var_stack, label_generator))
        for sub_form in form[4:]:
          output.append('pop')
          var_stack.pop()
          output.extend(lisp_form_to_asm_lines([sub_form], var_stack, label_generator))
        for _ in range(num_args):
          output.append('pop -2')
          var_stack.pop(-2)
        output.append('copy -2')
        var_stack.push()
        output.append('pop -3')
        var_stack.pop(-3)
        output.append('jump')
        var_stack.pop()
        var_stack.pop_stack()
      elif op == 'cond':
        done_label = next(label_generator)
        for cond_form in form[1:]:
          true_label = next(label_generator)
          false_label = next(label_generator)
          output.extend(lisp_form_to_asm_lines([cond_form[0]], var_stack, label_generator))
          output.append(f'cond {true_label} {false_label}')
          var_stack.pop()
          var_stack.push()
          output.append('jump')
          var_stack.pop()
          output.append(f'{true_label}:')
          output.extend(lisp_form_to_asm_lines([cond_form[1]], var_stack, label_generator))
          for sub_form in cond_form[2:]:
            output.append('pop')
            var_stack.pop()
            output.extend(lisp_form_to_asm_lines([sub_form], var_stack, label_generator))
          output.append(f'jump {done_label}')
          var_stack.pop()
          output.append(f'{false_label}:')
        output.append('push 0')
        var_stack.push('0')
        var_stack.pop()
        output.append(f'jump {done_label}')
        output.append(f'{done_label}:')
        var_stack.push()
      elif op == 'loop':
        assert form[1] == 'while'
        assert form[3] == 'do'
        assert len(form) >= 5
        while_label = next(label_generator)
        done_label = next(label_generator)
        output.append('push 0')
        var_stack.push('0')

        output.extend(lisp_form_to_asm_lines([form[2]], var_stack, label_generator))
        output.append(f'cond {while_label} {done_label}')
        var_stack.pop()
        var_stack.push()
        output.append('jump')
        var_stack.pop()

        output.append(f'{while_label}:')
        for sub_form in form[4:]:
          output.append('pop')
          var_stack.pop()
          output.extend(lisp_form_to_asm_lines([sub_form], var_stack, label_generator))

        output.extend(lisp_form_to_asm_lines([form[2]], var_stack, label_generator))
        output.append(f'cond {while_label} {done_label}')
        var_stack.pop()
        var_stack.push()
        output.append('jump')
        var_stack.pop()
        output.append(f'{done_label}:')
      elif op == 'setq': # I'm sure that the behavior of my setq is not the like real thing
        assert len(form) == 3
        assert isinstance(form[1], str)
        output.extend(lisp_form_to_asm_lines([form[2]], var_stack, label_generator))
        output.append('copy')
        var_stack.push()
        var_rel_pos = var_stack.get_rel_pos(form[1])
        output.append(f'set {var_rel_pos}')
        var_stack.pop()
      elif op == 'let':
        assert len(form) >= 3
        assert isinstance(form[1], list)
        for var_form in form[1]:
          assert len(var_form) == 2
          output.extend(lisp_form_to_asm_lines([var_form[1]], var_stack, label_generator))
          var_stack.pop()
          var_stack.push(var_form[0])
        output.extend(lisp_form_to_asm_lines([form[2]], var_stack, label_generator))
        for sub_form in form[3:]:
          output.append('pop')
          var_stack.pop()
          output.extend(lisp_form_to_asm_lines([sub_form], var_stack, label_generator))
        output.append(f'set {-(len(form[1]) + 1)}')
        var_stack.pop()
        for _ in range(len(form[1]) - 1):
          output.append('pop')
          var_stack.pop()
      else: # op is a function call (not necessarily a func index)
        var_stack.push_stack()
        return_label = next(label_generator)
        output.append(f'push {return_label}')
        var_stack.push(return_label)
        output.extend(lisp_form_to_asm_lines(form, var_stack, label_generator))
        len_form = len(form)
        output.append(f'copy {-len_form}')
        var_stack.push()
        output.append(f'pop {-(len_form + 1)}')
        var_stack.push()
        output.append('jump')
        var_stack.pop()
        output.append(f'{return_label}:')
        var_stack.pop_stack()
        var_stack.push()
    elif isinstance(form, str):
      if len(form) >= 3 and form[0] == "'" and form[-1] == "'":
        form = form.replace(r'\n', '\n').replace(r'\t', '\t')
        assert len(form) == 3
        for c in form[1:-1]:
          ascii_val = ord(c)
          output.append(f'push {ascii_val}')
          var_stack.push()
      elif form in var_stack:
        rel_pos = var_stack.get_rel_pos(form)
        output.append(f'copy {rel_pos}')
        var_stack.push()
      else:
        output.append(f'push {form}')
        var_stack.push(form)
    else:
      raise Exception(f'Bad lisp form {form!r}')
  return output

def lisp_to_asm(lisp_str: str) -> str:
  lisp = lisp_to_lists(lisp_str)
  return '\n'.join(lisp_form_to_asm_lines(lisp))

def main(argv: list[str]) -> None:
  in_file = open(argv[1]) if len(argv) > 1 else stdin
  out_file = open(argv[2]) if len(argv) > 2 else stdout
  out_file.write(lisp_to_asm(in_file.read()))
  out_file.write('\n')

if __name__ == '__main__':
  main(argv)
