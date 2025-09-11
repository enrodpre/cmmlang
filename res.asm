section .text
  global _start

_start:
main:
  mov rax, 254
mov rdi, rax
  jmp exit

exit:
  mov rax, 60
  syscall
