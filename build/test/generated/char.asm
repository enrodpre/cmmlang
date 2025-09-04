section .data
  str_0 db ''', 10
  str_0_len equ $ - str_0

section .text
    global _start

_start:
;; init variable c
    mov rax, str_0
    push rax
;; end init variable c
    mov rax, 255
    mov rdi, rax
    jmp exit

exit:
  mov rax, 60
  syscall