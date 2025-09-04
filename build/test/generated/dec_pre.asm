section .text
    global _start

_start:
;; init variable x
    mov rax, 256
    push rax
;; end init variable x
;; loading x
    lea rdi, [rsp]
;; end loading x
    mov r11, [rdi]
    dec r11
    mov [rdi], r11
    jmp exit

exit:
  mov rax, 60
  syscall