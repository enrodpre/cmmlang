section .text
  global _start

_start:
main:
  call fn_sint
  mov rdi, rax
  jmp exit

fn_sint:
  ret

exit:
mov rax, 60
  syscall
