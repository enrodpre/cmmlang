section .text
  global _start

_start:
main:
  mov rdi, 254
  mov rdi, 1
  add rdi, rdi
  jmp exit

exit:
mov rax, 60
  syscall
