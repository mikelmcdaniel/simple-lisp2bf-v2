from sys import stdin, stdout, argv

MAX_CELL_VALUE = 256

def matching_brace_dict(bf_str: str) -> dict[int, int]:
  matching_brace = {}
  brace_poss = []
  for j, c in enumerate(bf_str):
    if c == '[':
      brace_poss.append(j)
    elif c == ']':
      matching_brace[j] = brace_poss[-1]
      matching_brace[brace_poss.pop()] = j
  return matching_brace

def eval_bf(bf_str: str, in_file=stdin, out_file=stdout, stack=None, index=0, bf_index=0) -> int:
  if stack is None:
    stack = [0] * 3000000
  matching_braces = matching_brace_dict(bf_str)
  j = bf_index
  while j < len(bf_str):
    c = bf_str[j]
    # print(f'{j}\t{bf_str[j-3:j+4]!r}\t{stack[:10]}')
    if c == '>':
      index += 1
    elif c == '<':
      index -= 1
    elif c == '+':
      stack[index] = (stack[index] + 1) % MAX_CELL_VALUE
    elif c == '-':
      stack[index] = (stack[index] - 1) % MAX_CELL_VALUE
    elif c == '.':
      out_file.write(chr(stack[index]))
      out_file.flush()
    elif c == ',':
      stack[index] = ord(in_file.read(1)) % MAX_CELL_VALUE
    elif c == ']':
      if stack[index]:
        j = matching_braces[j]
    elif c == '[':
      if not stack[index]:
        j = matching_braces[j]
    j += 1
  return index

def main(argv: list[str]) -> None:
  in_file = open(argv[1], 'r') if len(argv) > 1 else stdin
  out_file = open(argv[2], 'w') if len(argv) > 2 else stdout
  eval_bf(in_file.read(), stdin, out_file)

if __name__ == '__main__':
  main(argv)
