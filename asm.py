from sys import stdin, stdout, argv

def normalized_asm_line(asm_line: str) -> list[str]:
  if '#' in asm_line:
    asm_line = asm_line[:asm_line.index('#')]
  return asm_line.split()

def normalized_asm_lines(original_asm_lines: str) -> list[list[str]]:
  asm_lines: list[list[str]] = list(filter(bool, map(normalized_asm_line, original_asm_lines.split('\n'))))

  label_dict = {'start': '1'}
  for asm_line in asm_lines:
    inst = asm_line[0]
    if inst.endswith(':'):
      label = inst[:-1]
      if label not in label_dict:
        label_dict[label] = str(len(label_dict) + 1)

  for asm_line in asm_lines:
    inst = asm_line[0]
    if inst[:-1] in label_dict:
      asm_line[0] = f'{label_dict[inst[:-1]]}:'
    for j in range(1, len(asm_line)):
      if asm_line[j] in label_dict:
        asm_line[j] = label_dict[asm_line[j]]

  return asm_lines


def asm_line_to_bf(asm_line: list[str]) -> str:
  inst = asm_line[0]
  if inst == 'jump':
    if len(asm_line) > 1:
      return asm_line_to_bf(["push", asm_line[1]]) + ' ]'
    else:
      return ']'
  elif inst == 'push':
    num = int(asm_line[1])
    if num > 0:
      return '+' * num + '>'
    else:
      return '-' * -num + '>'
  elif inst == 'eq':
    return f'{asm_line_to_bf(["sub"])} {asm_line_to_bf(["not"])}'
  elif inst == 'lesseq':
    return '<[-<-[->>>+>+<<<<]>>>>[-<<<<+>>>>]+<[->-<[-]]>[-<+>]' + \
      '<[<+<[-]>>-]<<]<[-]>>[-<<+>>]<'
  elif inst == 'greater':
    return f'{asm_line_to_bf(["lesseq"])} {asm_line_to_bf(["not"])}'
  elif inst == 'less':
    return f'{asm_line_to_bf(["greatereq"])} {asm_line_to_bf(["not"])}'
  elif inst == 'greatereq':
    return f'{asm_line_to_bf(["copy", "-2"])} {asm_line_to_bf(["pop", "-3"])} {asm_line_to_bf(["lesseq"])}'
  elif inst == 'sub':
    return '<[-<->]'
  elif inst == 'not':
    return '+<[->-<[-]]>[-<+>]'
  elif inst == 'bool':
    return '<[->+<[-]]>[-<+>]'
  elif inst == 'copy':
    copy_num = -1 if len(asm_line) < 2 else int(asm_line[1])
    copy_num = -copy_num
    assert copy_num > 0
    lmove = '<' * copy_num
    rmove = '>' * copy_num
    return f'{lmove}[-{rmove}+>+<{lmove}]{rmove}>[-<{lmove}+{rmove}>]'
  elif inst == 'add':
    return '<[-<+>]'
  elif inst == 'mul':
    return '<<[->>+<<]>>[-<[-<+>>>+<<]>>[-<<+>>]<]<[-]'
  elif inst == 'divmod':
    return '<[->>+>+<<<]>>>[-<<<+>>>]<<<<[->-[->>>>+<<<<]>>>>' + \
      '[-<+<<<+>>>>]+<[->-<[-]]>[-<+>]<[-<<+ >[->+<]>[-<+<<+>>>]]' + \
      '<<<<]>[->>-<<]>>[-<<+>>]<[-<<+>>]'
  elif inst == 'div':
    return f'{asm_line_to_bf(["divmod"])} {asm_line_to_bf(["pop"])}'
  elif inst == 'mod':
    return f'{asm_line_to_bf(["divmod"])} {asm_line_to_bf(["pop", "-2"])}'
  elif inst == 'read':
    read_num = int(asm_line[1]) if len(asm_line) > 1 else 1
    assert read_num >= 0
    return ',>' * read_num
  elif inst == 'write':
    write_num = int(asm_line[1]) if len(asm_line) > 1 else 1
    assert write_num >= 0
    return '<' * write_num + '.>' * write_num
  elif inst == 'pop':
    if len(asm_line) > 1:
      pop_num = -int(asm_line[1])
      assert pop_num > 0
      return f'{"<" * pop_num}[-]{">[-<+>]" * (pop_num - 1)}'
    return '<[-]'
  elif inst.endswith(':'): # it's a label
    label = inst[:-1]
    return ' '.join((
      asm_line_to_bf(['copy']),
      asm_line_to_bf(['push', label]),
      asm_line_to_bf(['sub']),
      asm_line_to_bf(['not']),
      '<[-',
      asm_line_to_bf(['pop'])
    ))
  elif inst == 'cond':
    return ' '.join((
      asm_line_to_bf(['bool']),
      asm_line_to_bf(['push', asm_line[1]]),
      asm_line_to_bf(['push', asm_line[2]]),
      asm_line_to_bf(['sub']),
      asm_line_to_bf(['mul']),
      asm_line_to_bf(['push', asm_line[2]]),
      asm_line_to_bf(['add'])
    ))
  elif inst == 'exit':
    return '>]' # push 0; jump
  elif inst == 'set':
    set_num = -int(asm_line[1]) if len(asm_line) > 1 else 2
    assert set_num > 1
    set_num -= 1
    lmove = '<' * set_num
    rmove = '>' * set_num
    return f'<{lmove}[-]{rmove}[-{lmove}+{rmove}]'

  raise Exception(f'Bad instruction: {asm_line!r}')

def asm_to_bf(asm_string: str) -> str:
  asm_lines = normalized_asm_lines(asm_string)

  output = ['>+[>']
  for asm_line in asm_lines:
    output.append(asm_line_to_bf(asm_line))
  output.append('<]')

  return '\n'.join(output)

def main(argv: list[str]) -> None:
  in_file = open(argv[1], 'r') if len(argv) > 1 else stdin
  out_file = open(argv[2], 'w') if len(argv) > 2 else stdout
  out_file.write(asm_to_bf(in_file.read()))

if __name__ == '__main__':
  main(argv)

