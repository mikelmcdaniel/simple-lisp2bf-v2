is_prime:
push 2
push 0
copy -3
copy -3
eq
not
cond LABEL_0 LABEL_1
jump
LABEL_0:
pop
copy -2
copy -2
mod
push 0
eq
not
cond LABEL_3 LABEL_4
jump
LABEL_3:
copy -1
push 1
add
copy
set -3
pop
push 1
jump LABEL_2
LABEL_4:
push 1
cond LABEL_5 LABEL_6
jump
LABEL_5:
copy -2
copy
set -3
pop
push 0
jump LABEL_2
LABEL_6:
push 0
jump LABEL_2
LABEL_2:
copy -3
copy -3
eq
not
cond LABEL_0 LABEL_1
jump
LABEL_1:
set -2
pop -2
copy -2
pop -3
jump
square:
copy -1
copy -2
mul
pop -2
copy -2
pop -3
jump
cube:
copy -1
push LABEL_7
push square
copy -4
copy -2
pop -3
jump
LABEL_7:
mul
pop -2
copy -2
pop -3
jump
factorial:
copy -1
push 0
eq
cond LABEL_9 LABEL_10
jump
LABEL_9:
push 1
jump LABEL_8
LABEL_10:
push 1
cond LABEL_11 LABEL_12
jump
LABEL_11:
copy -1
push LABEL_13
push factorial
copy -4
push 1
sub
copy -2
pop -3
jump
LABEL_13:
mul
jump LABEL_8
LABEL_12:
push 0
jump LABEL_8
LABEL_8:
pop -2
copy -2
pop -3
jump
print_primes:
push 0
copy -3
copy -3
lesseq
cond LABEL_14 LABEL_15
jump
LABEL_14:
pop
push LABEL_19
push is_prime
copy -4
copy -2
pop -3
jump
LABEL_19:
cond LABEL_17 LABEL_18
jump
LABEL_17:
push LABEL_20
push print_num
copy -4
copy -2
pop -3
jump
LABEL_20:
pop
push 10
write 1
jump LABEL_16
LABEL_18:
push 0
jump LABEL_16
LABEL_16:
pop
copy -2
push 1
add
copy
set -4
copy -3
copy -3
lesseq
cond LABEL_14 LABEL_15
jump
LABEL_15:
pop -2
pop -2
copy -2
pop -3
jump
print_num:
copy -1
push 100
greatereq
cond LABEL_22 LABEL_23
jump
LABEL_22:
copy -1
push 100
div
push 48
add
write 1
jump LABEL_21
LABEL_23:
push 0
jump LABEL_21
LABEL_21:
pop
copy -1
push 10
greatereq
cond LABEL_25 LABEL_26
jump
LABEL_25:
copy -1
push 10
div
push 10
mod
push 48
add
write 1
jump LABEL_24
LABEL_26:
push 0
jump LABEL_24
LABEL_24:
pop
copy -1
push 10
mod
push 48
add
write 1
pop -2
copy -2
pop -3
jump
is_digit:
copy -1
push 48
greatereq
copy -2
push 57
lesseq
mul
pop -2
copy -2
pop -3
jump
read_num:
push 0
push 0
push 0
push LABEL_29
push is_digit
read
copy
set -7
copy -2
pop -3
jump
LABEL_29:
cond LABEL_27 LABEL_28
jump
LABEL_27:
pop
copy -1
push 10
mul
copy -3
push 48
sub
add
copy
set -3
push LABEL_30
push is_digit
read
copy
set -7
copy -2
pop -3
jump
LABEL_30:
cond LABEL_27 LABEL_28
jump
LABEL_28:
set -3
pop
copy -2
pop -3
jump
start:
push 0
push 0
push 69
push 110
push 116
push 101
push 114
push 32
push 115
push 116
push 97
push 114
push 116
push 58
push 32
write 13
set -13
pop
pop
pop
pop
pop
pop
pop
pop
pop
pop
pop
pop
push LABEL_31
push read_num
copy -1
pop -2
jump
LABEL_31:
copy
set -4
pop
push 69
push 110
push 116
push 101
push 114
push 32
push 108
push 97
push 115
push 116
push 58
push 32
write 12
set -12
pop
pop
pop
pop
pop
pop
pop
pop
pop
pop
pop
push LABEL_32
push read_num
copy -1
pop -2
jump
LABEL_32:
copy
set -3
pop
push 10
write 1
pop
push LABEL_33
push print_primes
copy -4
copy -4
copy -3
pop -4
jump
LABEL_33:
set -3
pop
pop
exit
LABEL_34:
copy -2
pop -3
jump
