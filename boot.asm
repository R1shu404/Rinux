; boot.asm - Simple 16-bit bootloader
BITS 16
ORG 0x7C00          ; BIOS loads bootloader

start:
    mov ax, 0x07C0    ; Set up data segment
    mov ds, ax
    mov es, ax
    mov ss, ax        ; Set up stack segment
    mov sp, 0x7C00    ; Stack pointer grows downwards from 0x7C00

    ; Print loading message
    mov si, loading_msg
    call print_string_16

    ; Load kernel from disk
    ; We'll load 8 sectors (4KB) for the kernel + embedded FS.
    ; Adjust if your kernel.bin becomes larger.
    mov ax, KERNEL_LOAD_SEGMENT ; Segment where kernel will be loaded
    mov es, ax        ; ES:BX is destination address
    mov bx, KERNEL_LOAD_OFFSET
    mov ah, 0x02      ; BIOS read sector function
    mov al, 8         ; Number of sectors to read (kernel_sectors)
    mov ch, 0         ; Cylinder 0
    mov cl, 2         ; Sector 2 (sector 1 is bootloader)
    mov dh, 0         ; Head 0
    ; dl is drive number, BIOS sets it (usually 0x00 for floppy, 0x80 for HDD)
    int 0x13          ; Call BIOS disk interrupt
    jc disk_error     ; Jump if carry flag set (error)

    ; Print kernel loaded message
    mov si, kernel_loaded_msg
    call print_string_16

    ; --- Switch to 32-bit Protected Mode ---
    cli               ; Disable interrupts
    lgdt [gdt_descriptor]
    call enable_a20
    mov eax, cr0
    or eax, 0x1       ; Set PE (Protection Enable) bit
    mov cr0, eax
    jmp CODE_SEG:protected_mode_start

; --- Subroutines (16-bit) ---
print_string_16:
    mov ah, 0x0E      ; BIOS teletype output
.loop:
    lodsb             ; Load byte from [SI] into AL, increment SI
    or al, al         ; Check if AL is zero (end of string)
    jz .done
    int 0x10          ; Call BIOS video interrupt
    jmp .loop
.done:
    ret

enable_a20:
.wait_input_empty:
    in al, 0x64
    test al, 0x02
    jnz .wait_input_empty
    mov al, 0xD1
    out 0x64, al
.wait_input_empty2:
    in al, 0x64
    test al, 0x02
    jnz .wait_input_empty2
    mov al, 0xDF
    out 0x60, al
.wait_input_empty3:
    in al, 0x64
    test al, 0x02
    jnz .wait_input_empty3
    ret

disk_error:
    mov si, disk_error_msg
    call print_string_16
    jmp $

; --- Data ---
loading_msg db 'Bootloader: Loading kernel...', 0x0D, 0x0A, 0
kernel_loaded_msg db 'Bootloader: Kernel loaded. Switching to Protected Mode...', 0x0D, 0x0A, 0
disk_error_msg db 'Bootloader: Disk read error!', 0x0D, 0x0A, 0

KERNEL_LOAD_SEGMENT equ 0x1000    ; Kernel will be loaded at 0x1000:0000 (0x10000 linear)
KERNEL_LOAD_OFFSET  equ 0x0000

; --- GDT (Global Descriptor Table) ---
gdt_start:
    dq 0x0000000000000000 ; Null Descriptor
    ; Code Segment (Ring 0)
    dw 0xFFFF; dw 0x0000; db 0x00; db 0x9A; db 0xCF; db 0x00
    ; Data Segment (Ring 0)
    dw 0xFFFF; dw 0x0000; db 0x00; db 0x92; db 0xCF; db 0x00
gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1
    dd gdt_start

CODE_SEG equ 0x08 ; Selector for Code Segment (offset from GDT start)
DATA_SEG equ 0x10 ; Selector for Data Segment

; --- Jump target for protected mode ---
BITS 32
protected_mode_start:
    mov ax, DATA_SEG
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x90000 ; Temporary stack

    ; Jump to kernel code
    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET ; Jumps to 0x10000 linear

; Padding and Boot Signature
times 510 - ($-$$) db 0
dw 0xAA55