section .text
  global _start

_start:
main:
  mov rax, 255
;; init local variable x:
  push rax
;; end init local variable x:
;; loading x:
mov rax, [rsp]
;; end loading x:
mov rdi, rax
  jmp exit

exit:
  mov rax, 60
  syscall
