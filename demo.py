def is_prime(x):
    j = 2
    while x != j:
        if x % j != 0:
            j += 1
            True
        else:
            j = x
            False

def square(x):
    x * x

def cube(x):
    x * square(x)

def factorial(x):
    if x == 0:
        1
    else:
        x * factorial(x - 1)

def print_primes(first, last):
    while first <= last:
        if is_prime(first):
            print_num(first)
            print('\n')
        first += 1

def print_num(x):
    if x >= 100:
        print(x / 100 + '0')
    if x >= 10:
        print(x / 10 % 10 + '0')
    print(x % 10 + '0')

def is_digit(c):
    '0' <= c and c <= '9'

def read_num():
    c = read()
    result = 0
    while is_digit(c):
        result = result * 10 + (c - '0')
        c = read()
    result

def start():
    print("Enter start: ")
    start = read_num()
    print("Enter last: ")
    last = read_num()
    print("\n")

    print_num(start)
    print(" ** ")
    print_num(last)
    print(" % 256 == ")
    print_num(start ** last)
    print("\n")

    print_num(start)
    print(" ** 3 % 256 == ")
    print_num(cube(start))
    print("\n")

    print_num(start)
    print("! % 256 == ")
    print_num(factorial(start))
    print("\n")

    print("log2(")
    print_num(last)
    print(") == ")
    print_num(log2(last))
    print("\n")

    print("Here are the primes from ")
    print_num(start)
    print(" to ")
    print_num(last)
    print(":\n")
    print_primes(start, last)

    exit()