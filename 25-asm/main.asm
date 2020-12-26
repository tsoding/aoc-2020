BITS 64

%define SYS_READ 0
%define SYS_WRITE 1
%define SYS_OPEN 2
%define SYS_CLOSE 3
%define SYS_EXIT 60

%define STDIN 0
%define STDOUT 1
%define STDERR 2

%define O_RDONLY 0

%define BUFFER_CAPACITY 256

global _start
section .text

;; rdi - number
itoa:
    mov rbx, 10
    mov rsi, buffer_end - 1
.begin:
    test rdi, rdi
    jz .end
    mov rax, rdi
    mov rdx, 0
    div rbx
    mov rdi, rax
    add rdx, '0'
    mov [rsi], dl
    dec rsi
    jmp .begin
.end:
    mov rax, rsi
    inc rax
    ret

;; rdi - subject_number
;; rsi - state
;; --
;; rax - next state
transform_step:
    mov rax, rsi
    mov rcx, 20201227
    mul rdi
    mov rdx, 0
    div rcx
    mov rax, rdx
    ret

;; rdi - subject_number
;; rsi - loop_size
transform:
    mov rax, 1
    mov rcx, 20201227
.begin:
    test rsi, rsi
    jz .end
    mul rdi
    mov rdx, 0
    div rcx
    mov rax, rdx
    dec rsi
    jmp .begin
.end:
    ret

;; rdi - subject_number
;; rsi - public_key
;; ----
;; rax - loop_size
crack_loop_size:
    mov rax, 1
    mov rbx, 1                  ;state
.begin:
    push rax
    push rbx
    push rdi
    push rsi

    mov rsi, rbx
    call transform_step

    pop rsi
    pop rdi
    pop rbx

    cmp rax, rsi
    je .end
    mov rbx, rax

    pop rax
    inc rax
    jmp .begin
.end:
    pop rax
    ret

; %rdi %rsi %rdx %r10 %r8 %r9

_start:
    mov rdi, 7
    mov rsi, [card_public_key]
    call crack_loop_size

    ;; rdi - subject_number
    mov rdi, [door_public_key]
    ;; rsi - loop_size
    mov rsi, rax
    call transform

    mov rdi, rax
    call itoa

    mov rsi, rax
    mov rdx, buffer_end
    sub rdx, rsi
    mov rdi, STDOUT
    mov rax, SYS_WRITE
    syscall

    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, nl
    mov rdx, 1
    syscall

    mov rax, SYS_EXIT
    mov rdi, 0
    syscall

section .data
;; Sample
; card_public_key: dq 5764801
; door_public_key: dq 17807724
;; Input
card_public_key: dq 11239946
door_public_key: dq 10464955

nl: db 10

section .bss
buffer:
    resb BUFFER_CAPACITY
buffer_end:
