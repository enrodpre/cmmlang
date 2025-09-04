section .text
    global _start

_start:
    jmp good

good:
    mov rax, 255
    mov rdi, rax
    jmp exit

exit:
  mov rax, 60
  syscall