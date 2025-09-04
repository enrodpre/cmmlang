section .text
    global _start

_start:
    mov rax, 0
    mov rdi, rax
    jmp exit

exit:
  mov rax, 60
  syscall