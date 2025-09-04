section .text
    global _start

_start:
    mov rax, 255
    mov rdi, rax
    jmp exit

exit:
  mov rax, 60
  syscall