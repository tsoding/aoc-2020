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
    mov rdi, input_file_label
    call print

    mov rdi, [input_file_path]
    call println

    mov rdi, [input_file_path]
    call parse_input

    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, input_buffer
    mov rdx, [input_buffer_size]
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
