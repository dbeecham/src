# at&t syntax

.global _start

.text

_start:
# syscall nr 1 is write
mov $1, %rax

# fd 1 is stdout
mov $1, %rdi

# address to string 
mov $hello, %rsi

# number of bytes
mov $13, %rdx

# call
syscall

# syscall 60 is exit
mov $60, %rax

# return code 0
# could do "mov $0, %rdi", but xor it with itself is a smaller instruction and is 0
xor %rdi, %rdi

# call
syscall



hello:
.string "Hello, world\n"
