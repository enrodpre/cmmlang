section .bss
  a resb 8

section .text
    global _start

_start:
;; init variable a
    mov rax, 255
    mov [a], rax
;; end init variable a
;; loading a
    lea rax, [a]
;; end loading a
    mov rdi, [rax]
    jmp exit

exit:
  mov rax, 60
  syscall