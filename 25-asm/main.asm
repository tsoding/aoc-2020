BITS 64

%define SYS_EXIT 60
%define SYS_WRITE 1

%define STDIN 0
%define STDOUT 1
%define STDERR 2

global _start
section .text

strlen:
    mov rax, 0
.begin:
    mov bl, [rdi]
    test bl, bl
    jz .end
    inc rax
    inc rdi
    jmp .begin
.end:
    ret

; %rdi %rsi %rdx %r10 %r8 %r9

puts:
    push rdi
    call strlen

    mov rdx, rax
    mov rax, SYS_WRITE
    mov rdi, STDOUT,
    pop rsi
    syscall

    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, newline
    mov rdx, 1
    syscall

    ret

_start:
    mov rax, [rsp]
    dec rax
    mov rsi, rsp
    add rsi, 16

.begin:
    test rax, rax
    jz .end

    push rsi
    push rax
    mov rdi, [rsi]
    call puts
    pop rax
    pop rsi
    add rsi, 8
    dec rax

    jmp .begin
.end:

    mov rax, SYS_EXIT
    mov rdi, 0
    syscall

section .data
newline:
    db 10
s:
    db "null terminated hello", 0
