section .text
    global _start

_start:
;; init variable i
    mov rax, 0
    push rax
;; end init variable i
;; loading i
    lea rax, [rsp]
;; end loading i
    mov rdi, [rax]
    jmp exit

exit:
  mov rax, 60
  syscall