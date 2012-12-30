import bf
import asm
from random import sample

def test_bf_line(bf_line, stack, index, end_stack, end_index):
	index = bf.eval_bf(bf_line, None, None, stack, index)
	if (stack, index) != (end_stack, end_index):
		raise Exception("Mismatch: %s != %s" % ((stack, index), (end_stack, end_index)))

def test_asm(asm_line, func, num_inputs=2):
	assert num_inputs == 2
	stack = [0] * 10
	end_stack = [0] * 10
	bf_line = asm.asm_line_to_bf(asm.normalized_asm_line(asm_line))
	nums = range(256)
	for a in sample(nums, 5):
		for b in sample(nums, 5):
			print a, b
			stack[0] = a
			stack[1] = b
			results = func(a, b)
			if not isinstance(results, tuple):
				results = (results,)
			end_stack[:len(results)] = map(lambda x: x % 256, results)
			for j in xrange(num_inputs, len(stack)):
				stack[j] = 0
			test_bf_line(bf_line, stack, num_inputs, end_stack, len(results))

#test_asm("divmod", lambda a, b: divmod(a, b))
#test_asm("add", lambda a, b: a + b)
#test_asm("sub", lambda a, b: a - b)
#test_asm("mul", lambda a, b: a * b)
#test_asm("not", lambda a, b: (a, int(not b)))
#test_asm("bool", lambda a, b: (a, int(bool(b))))
#test_asm("lesseq", lambda a, b: int(a <= b))
#test_asm("divmod", lambda a, b: divmod(a, b))
#test_asm("div", lambda a, b: divmod(a, b)[0])
#test_asm("mod", lambda a, b: divmod(a, b)[1])

