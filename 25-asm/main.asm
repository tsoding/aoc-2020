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

%define INPUT_BUFFER_SIZE 1024

global _start
section .text

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

;; rdi - buf
;; rsi - buf_size
;; rdx - needle
;; ---
;; rax - needle index
strchr:
.begin:
    test rsi, rsi
    jz .end

    mov al, [rdi]
    cmp rax, rdx
    je .end

    inc rdi
    dec rsi
    jmp .begin

.end:
    mov rax, rdi
    ret

;; rdi - number
;; rsi - buf
;; rdx - buf_size
itoa:
    mov rax, SYS_EXIT
    mov rdi, 69
    syscall
    ret

;; rdi - buf
;; rsi - buf_size
;; 1234
atoi:
    mov rax, 0
    mov rbx, 0
    mov rcx, 10
.begin:
    test rsi, rsi
    jz .end
    mov bl, [rdi]
    sub rbx, '0'
    mul rcx
    add rax, rbx
    dec rsi
    inc rdi
    jmp .begin
.end:
    ret

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

print:
    push rdi
    call strlen

    mov rdx, rax
    mov rax, SYS_WRITE
    mov rdi, STDOUT,
    pop rsi
    syscall

    ret

println:
    call print
    mov rdi, nl
    call print
    ret

;; rdi  - file_path
;; rsi  - buf
;; rdx  - buf_size
slurp_file:
    push rsi                    ;buf
    push rdx                    ;buf_size

    mov rax, SYS_OPEN
    ;; rdi already contains the file_path
    mov rsi, O_RDONLY
    mov rdx, 0
    syscall

    mov rdi, rax
    mov rax, SYS_READ
    pop rdx
    pop rsi

    push rdi
    syscall
    pop rdi

    push rax

    mov rax, SYS_CLOSE
    syscall

    pop rax
    ret

; rdi - file_path
parse_input:
    mov rsi, input_buffer
    mov rdx, INPUT_BUFFER_SIZE
    call slurp_file
    mov [input_buffer_size], rax

    ret

solve_file:
    ;; Cosmetic printing
    mov rdi, input_file_label
    call print
    mov rdi, [input_file_path]
    call println

    ;; Parse the input
    mov rdi, [input_file_path]
    call parse_input

    mov rdi, input_buffer
    mov rsi, [input_buffer_size]
    mov rdx, 10
    call strchr

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
    mov [input_file_path], rdi
    call solve_file
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
nl:
    db 10, 0
input_file_label:
    db "Input file: ", 0

section .bss
input_file_path:
    resq 1
input_buffer:
    resb INPUT_BUFFER_SIZE
input_buffer_size:
    resq 1
