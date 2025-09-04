section .text
    global _start

_start:
;; init variable x
    mov rax, 255
    push rax
;; end init variable x
    mov rdi, 0
    jmp exit

exit:
  mov rax, 60
  syscall