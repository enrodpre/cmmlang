section .text
    global _start

_start:
    call fn
    mov rdi, rax
    jmp exit

exit:
  mov rax, 60
  syscall
fn:
      mov rax, 255
    ret
