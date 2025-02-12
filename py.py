import ast
import functools
from sys import stdin, stdout, argv


class VarsFinder(ast.NodeVisitor):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.vars_found = set()

    def visit_Assign(self, node: ast.Assign):
        for target in node.targets:
            if isinstance(target, ast.Name):
                self.vars_found.add(target.id)

    def visit_AugAssign(self, node: ast.AugAssign):
        if isinstance(node.target, ast.Name):
            self.vars_found.add(node.target.id)


def find_vars(code):
    vf = VarsFinder()
    for c in code:
        vf.visit(c)
    return vf.vars_found


def indented(text, amount=2):
    indent = ' ' * amount
    return indent + f'\n{indent}'.join(text.split('\n'))


@functools.singledispatch
def generate_lisp(code) -> str:
    try:
        code_str = ast.dump(code, indent=4)
    except (TypeError, SyntaxError):
        code_str = repr(code)
    raise ValueError(f'Unexpected node types: {code_str}')


@generate_lisp.register
def _(code: ast.Module):
    return '\n\n'.join(generate_lisp(b) for b in code.body)


@generate_lisp.register
def _(code: ast.FunctionDef):
    assert code.name not in ('print', 'write', 'cond', 'loop', 'let')
    arg_names = [generate_lisp(a) for a in code.args.args]
    body = '\n'.join(generate_lisp(b) for b in code.body)
    vars = find_vars(code.body)
    vars.difference_update(arg_names)
    if vars:
        vars_str = ' '.join(f'({v} 0)' for v in sorted(vars))
        body = f'(let ({vars_str})\n{indented(body)})'
    return f'(defun {code.name} ({" ".join(arg_names)})\n{indented(body)})'


@generate_lisp.register
def _(code: ast.arg):
    return code.arg


@generate_lisp.register
def _(code: ast.If):
    ifs = [code]
    conds = []
    while ifs:
        if_statement = ifs[0]
        if isinstance(if_statement, ast.If):
            test = generate_lisp(if_statement.test)
            body = '\n'.join(generate_lisp(b) for b in if_statement.body)
            conds.append(f'({test} {body})')
            ifs = if_statement.orelse
        else:
            statements = '\n'.join(generate_lisp(x) for x in ifs)
            conds.append(f'(1 {statements})')
            break
    conds_str = '\n'.join(conds)
    return f'(cond\n{indented(conds_str)})'


@generate_lisp.register
def _(code: ast.Name):
    return code.id


@generate_lisp.register
def _(code: ast.Constant):
    if code.value is True:
        return '1'
    elif code.value is False:
        return '0'
    if isinstance(code.value, int):
        return repr(code.value)
    elif isinstance(code.value, str):
        s = repr(code.value)
        if s.startswith("'") and len(s) > 3:
            s = '"' + s[1:-1].replace('"', r'\"') + '"'
        return s
    else:
        raise ValueError(f'Unknown Constant: {code.value}')


@generate_lisp.register
def _(code: ast.Compare):
    assert len(code.ops) == 1
    op = code.ops[0]
    if isinstance(op, ast.Lt):
        op_code = '<'
    elif isinstance(op, ast.LtE):
        op_code = '<='
    elif isinstance(op, ast.Gt):
        op_code = '>'
    elif isinstance(op, ast.GtE):
        op_code = '>='
    elif isinstance(op, ast.Eq):
        op_code = '='
    elif isinstance(op, ast.NotEq):
        op_code = '!='
    else:
        raise ValueError(f'Unknown Compare operator: {code.ops}')
    assert len(code.comparators) == 1
    return f'({op_code} {generate_lisp(code.left)} {generate_lisp(code.comparators[0])})'


@generate_lisp.register
def _(code: ast.Call):
    assert not code.keywords
    func_name = generate_lisp(code.func)
    if func_name == 'print':
        func_name = 'write'
    elements = [func_name]
    elements.extend(generate_lisp(a) for a in code.args)
    return f'({" ".join(elements)})'


@generate_lisp.register
def _(code: ast.BoolOp):
    if isinstance(code.op, ast.And):
        op_code = '*'
    elif isinstance(code.op, ast.Or):
        op_code = '+'
    else:
        raise ValueError(f'Unknown BoolOp operator: {code.op}')
    args = []
    for v in code.values:
        args.append(f'(bool {generate_lisp(v)})')
    args_str = ' '.join(args)
    return f'({op_code} {args_str})'


@generate_lisp.register
def _(code: ast.BinOp):
    if isinstance(code.op, ast.Add):
        op_code = '+'
    elif isinstance(code.op, ast.Sub):
        op_code = '-'
    elif isinstance(code.op, ast.Mult):
        op_code = '*'
    elif isinstance(code.op, ast.Div):
        op_code = '/'
    elif isinstance(code.op, ast.FloorDiv):
        op_code = '/'
    elif isinstance(code.op, ast.Mod):
        op_code = '%'
    elif isinstance(code.op, ast.Pow):
        op_code = '**'
    else:
        raise ValueError(f'Unknown BinOp operator: {code.op}')
    return f'({op_code} {generate_lisp(code.left)} {generate_lisp(code.right)})'


@generate_lisp.register
def _(code: ast.Expr):
    return generate_lisp(code.value)


@generate_lisp.register
def _(code: ast.Assign):
    assert len(code.targets) == 1
    return f'(setq {generate_lisp(code.targets[0])} {generate_lisp(code.value)})'


@generate_lisp.register
def _(code: ast.AugAssign):
    equivalent_code = ast.BinOp(code.target, code.op, code.value)
    return f'(setq {generate_lisp(code.target)} {generate_lisp(equivalent_code)})'


@generate_lisp.register
def _(code: ast.While):
    test = generate_lisp(code.test)
    body = '\n'.join(generate_lisp(b) for b in code.body)
    return f'(loop while {test} do\n{indented(body)})'


def main(argv: list[str]) -> None:
  in_file = open(argv[1]) if len(argv) > 1 else stdin
  out_file = open(argv[2]) if len(argv) > 2 else stdout
  out_file.write(generate_lisp(ast.parse(in_file.read())))
  out_file.write('\n')


if __name__ == '__main__':
  main(argv)
