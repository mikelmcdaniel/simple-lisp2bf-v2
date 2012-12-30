from sys import stdin, stdout, argv

class VarStack(object):
	def __init__(self, stacks=None):
		if stacks is None:
			stacks = []
		self.stacks = stacks
		self.push_stack()
	
	def push_stack(self):
		self.stacks.append([])
	
	def pop_stack(self):
		#print '# %r.pop_stack()' % self
		self.stacks.pop()
	
	def push(self, name=None):
		#print '# %r.push(%r)' % (self, name)
		if isinstance(name, int):
			#self.stacks[-1].append(self.stacks[-1][name])
			self.stacks[-1].append(None)
		else:
			self.stacks[-1].append(name)
	
	def pop(self, name=None):
		#print '# %r.pop(%r)' % (self, name)
		if name is None:
			self.stacks[-1].pop()
		elif isinstance(name, int):
			del self.stacks[-1][name]
		else:
			index = self.stacks[-1].index(name)
			del self.stacks[-1][index]
	
	def get_rel_pos(self, name):
		offset = 0
		for j in xrange(len(self.stacks) - 1, -1, -1):
			for k in xrange(len(self.stacks[j]) - 1, -1, -1):
				offset -= 1
				if self.stacks[j][k] == name:
					return offset
		#return self.stacks[-1].index(name)
		raise Exception('%s.get_pos(%r) failed' % (self, name))

	def __contains__(self, name):
		return any(name in stack for stack in reversed(self.stacks))
	
	def __str__(self):
		return repr(self)
	
	def __repr__(self):
		return 'VarStack(%r)' % (self.stacks,)

def lisp_tokens(lisp_str):
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
			#j += 1
			yield lisp_str[start:j + 1]
			start = j + 1
		elif c == ';':
			while lisp_str[j] != '\n':
				j += 1
			start = j + 1
		j += 1

def lisp_to_lists(lisp_str):
	stack = [[]]
	for token in lisp_tokens(lisp_str):
		if token == '(':
			stack.append([])
		elif token == ')':
			last_str = stack.pop()
			stack[-1].append(last_str)
		elif token[0] == '"' and token[-1] == '"':
			for c in token[1:-1].replace('\\n', '\n').replace('\\t', '\t'):
				stack[-1].append("'%s'" % c)
		else:
			stack[-1].append(token)
	return stack[0]

def lisp_form_to_asm_lines(lisp, var_stack=None, label_generator=None):
	if var_stack is None:
		var_stack = VarStack()
	if label_generator is None:
		label_generator = iter('LABEL_%d' % j for j in xrange(1000000000))
	output = []
	# TODO: think about stacks and scopes and life and stuff
	for form in lisp:
		if isinstance(form, list):
			op = form[0]
			if op == '+':
				assert len(form) >= 3
				output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
				output.extend('add' for j in xrange(len(form) - 2))
				for j in xrange(len(form) - 1):
					var_stack.pop()
				var_stack.push()
			elif op == '-':
				assert len(form) >= 3
				output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
				output.extend('add' for j in xrange(len(form) - 3))
				output.append('sub')
				for j in xrange(len(form) - 1):
					var_stack.pop()
				var_stack.push()
			elif op == '*':
				assert len(form) >= 3
				output.extend(lisp_form_to_asm_lines(form[1:], var_stack, label_generator))
				output.extend('mul' for j in xrange(len(form) - 2))
				for j in xrange(len(form) - 1):
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
				output.append(lisp_form_to_asm_lines(form[1], var_stack, label_generator))
				output.append('not')
				var_stack.pop()
				var_stack.push()
			elif op == 'bool':
				assert len(form) == 2
				output.append(lisp_form_to_asm_lines(form[1], var_stack, label_generator))
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
				output.append('write %d' % num_writes)
				if num_writes > 1:
					output.append('set %d' % -num_writes)
					var_stack.pop()
					for j in xrange(num_writes - 2):
						output.append('pop')
						var_stack.pop()
			elif op == 'exit':
				output.append('exit')
				var_stack.push()
				#var_stack.pop()
				output.append('%s:' % label_generator.next())
			elif op == 'defun':
				assert len(form) >= 4
				assert isinstance(form[1], str)
				assert isinstance(form[2], list)
				num_args = len(form[2])
				var_stack.push_stack()
				var_stack.push('$RETURN_ADDR')
				output.append('%s:' % form[1])
				for var_name in form[2]:
					assert isinstance(var_name, str)
					var_stack.push(var_name)
				output.extend(lisp_form_to_asm_lines([form[3]], var_stack, label_generator))
				for sub_form in form[4:]:
					output.append('pop')
					var_stack.pop()
					output.extend(lisp_form_to_asm_lines([sub_form], var_stack, label_generator))
				for _ in xrange(num_args):
					output.append('pop -2')
					var_stack.pop(-2)
				output.append('copy -2')
				var_stack.push(-2)
				output.append('pop -3')
				var_stack.pop(-3)
				output.append('jump')
				var_stack.pop()
				var_stack.pop_stack()
			elif op == 'cond':
				num_conds = len(form) - 1
				done_label = label_generator.next()
				for cond_form in form[1:]:
					true_label = label_generator.next()
					false_label = label_generator.next()
					output.extend(lisp_form_to_asm_lines([cond_form[0]], var_stack, label_generator))
					output.append('cond %s %s' % (true_label, false_label))
					var_stack.pop()
					var_stack.push()
					output.append('jump')
					var_stack.pop()
					output.append('%s:' % true_label)
					output.extend(lisp_form_to_asm_lines([cond_form[1]], var_stack, label_generator))
					for sub_form in cond_form[2:]:
						output.append('pop')
						var_stack.pop()
						output.extend(lisp_form_to_asm_lines([sub_form], var_stack, label_generator))
					output.append('jump %s' % done_label)
					var_stack.pop()
					output.append('%s:' % false_label)
				output.append('push 0')
				var_stack.push('0')
				var_stack.pop()
				output.append('jump %s' % done_label)
				output.append('%s:' % done_label)
				var_stack.push()
			elif op == 'loop':
				assert form[1] == 'while'
				assert form[3] == 'do'
				assert len(form) >= 5
				while_label = label_generator.next()
				done_label = label_generator.next()
				output.append('push 0')
				var_stack.push('0')

				output.extend(lisp_form_to_asm_lines([form[2]], var_stack, label_generator))
				output.append('cond %s %s' % (while_label, done_label))
				var_stack.pop()
				var_stack.push()
				output.append('jump')
				var_stack.pop()

				output.append('%s:' % while_label)
				for sub_form in form[4:]:
					output.append('pop')
					var_stack.pop()
					output.extend(lisp_form_to_asm_lines([sub_form], var_stack, label_generator))

				output.extend(lisp_form_to_asm_lines([form[2]], var_stack, label_generator))
				output.append('cond %s %s' % (while_label, done_label))
				var_stack.pop()
				var_stack.push()
				output.append('jump')
				var_stack.pop()
				output.append('%s:' % done_label)
			elif op == 'setq': # I'm sure that the behavior of my setq is not the like real thing
				assert len(form) == 3
				assert isinstance(form[1], str)
				output.extend(lisp_form_to_asm_lines([form[2]], var_stack, label_generator))
				output.append('copy')
				var_stack.push(-1)
				var_rel_pos = var_stack.get_rel_pos(form[1])
				output.append('set %d' % var_rel_pos)
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
				output.append('set %d' % -(len(form[1]) + 1))
				var_stack.pop()
				for j in xrange(len(form[1]) - 1):
					output.append('pop')
					var_stack.pop()
			else: # op is a function call (not necessarily a func name)
				var_stack.push_stack()
				return_label = label_generator.next()
				output.append('push %s' % return_label)
				var_stack.push(return_label)
				output.extend(lisp_form_to_asm_lines(form, var_stack, label_generator))
				len_form = len(form)
				output.append('copy %d' % -len_form)
				var_stack.push(-len_form)
				output.append('pop %d' % -(len_form + 1))
				var_stack.push(-len_form - 1)
				output.append('jump')
				var_stack.pop() # TODO check the number of pushs/pops here
				output.append('%s:' % return_label)
				var_stack.pop_stack()
				var_stack.push()
		elif isinstance(form, str):
			if len(form) >= 3 and form[0] == "'" and form[-1] == "'":
				# TODO: I shouldn't need these replace calls here...
				form = form.replace('\\n', '\n').replace('\\t', '\t')
				assert len(form) == 3
				for c in form[1:-1]:
					ascii_val = ord(c)
					output.append('push %s' % ascii_val)
					var_stack.push(str(ascii_val))
			elif form in var_stack:
				rel_pos = var_stack.get_rel_pos(form)
				output.append('copy %d' % rel_pos)
				var_stack.push(rel_pos)
			else:
				output.append('push %s' % form)
				var_stack.push(form)
		else:
			raise Exception("Bad lisp form %s" % repr(form))
	return output

def lisp_to_asm(lisp_str):
	lisp = lisp_to_lists(lisp_str)
	return '\n'.join(lisp_form_to_asm_lines(lisp))
	

if __name__ == '__main__':
	in_file = open(argv[1]) if len(argv) > 1 else stdin
	out_file = open(argv[2]) if len(argv) > 2 else stdout
	#eval_lisp(in_file.read(), stdin, out_file)
	out_file.write(lisp_to_asm(in_file.read()))
	out_file.write('\n')
