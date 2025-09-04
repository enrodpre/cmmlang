section .text
    global _start

_start:
;; init variable a
    mov rax, 255
    push rax
;; end init variable a
;; init variable x
    mov rax, 0
    push rax
;; end init variable x
;; loading x
    lea rax, [rsp]
;; end loading x
;; loading x
    lea [rax], [rsp]
;; end loading x
    mov rdi, [rax]
    jmp exit

exit:
  mov rax, 60
  syscall