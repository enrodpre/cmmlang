section .text
    global _start

_start:
;; init variable x
    mov rax, 125
    push rax
;; end init variable x
;; init variable y
    mov rax, 4
    push rax
;; end init variable y
;; loading x
    lea rax, [rsp + 8]
;; end loading x
    mov rdi, [rax]
    jmp exit

exit:
  mov rax, 60
  syscall