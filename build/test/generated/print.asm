section .text
    global _start

_start:
    call print
    mov rdi, 0
    jmp exit

exit:
  mov rax, 60
  syscall
print:
    mov rax, 1
  mov rdi, 1
  lea rsi, [newline]
  mov rdx, 1
  syscall
  ret