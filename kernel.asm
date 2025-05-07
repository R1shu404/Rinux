; kernel.asm - Simple 32-bit kernel with embedded FS
BITS 32
ORG 0x10000         ; Kernel is loaded here by bootloader

VIDEO_MEMORY equ 0xB8000
SCREEN_WIDTH equ 80
SCREEN_HEIGHT equ 25
DEFAULT_COLOR equ 0x0F ; White text on black background

%define MAX_CMD_LEN 80
%define MAX_FILENAME_LEN 12 ; e.g. 8.3 + null
%define FS_MAX_FILES 5      ; Max files in our simple FS

; --- Filesystem Structures ---
struc direntry
    .name resb MAX_FILENAME_LEN
    .data_ptr dd ?      ; Pointer to file data in memory
    .size dd ?          ; Size of file in bytes
    .flags db ?         ; Bit 0: 1=Executable, 0=Data file
endstruc
FLAG_EXECUTABLE equ 1

kernel_main:
    mov esp, kernel_stack_top
    call clear_screen
    mov esi, welcome_msg
    call print_string
    call init_fs ; Initialize filesystem (not much to do for embedded)
    mov esi, prompt_msg
    call print_string

shell_loop:
    mov edi, command_buffer  ; Buffer to store command
    mov dword [command_len_ptr], 0 ; Reset command length
    call read_command

    call parse_and_execute_command

    mov esi, prompt_msg
    call print_string
    jmp shell_loop

halt:
    cli
    hlt
    jmp halt

; --- Command Processing ---
read_command: ; Reads a line of input into command_buffer
    pusha
    mov edi, command_buffer
    mov ecx, MAX_CMD_LEN - 1 ; Leave space for null terminator
.read_char_loop:
    call get_char
    cmp al, 0x0D      ; Enter key?
    je .done_reading

    cmp al, 0x08      ; Backspace?
    je .handle_backspace

    cmp al, ' '       ; Printable characters (simple check)
    jl .read_char_loop
    cmp al, '~'
    jg .read_char_loop

    stosb             ; Store AL at [EDI], increment EDI
    inc dword [command_len_ptr]
    call print_char   ; Echo character
    loop .read_char_loop ; Decrement ECX, jump if not zero (buffer full)
    ; Buffer full, treat as enter
.done_reading:
    mov byte [edi], 0   ; Null terminate
    call newline
    popa
    ret

.handle_backspace:
    cmp edi, command_buffer
    je .read_char_loop      ; At start of buffer, do nothing

    dec edi
    mov byte [edi], 0 ; Erase char in buffer
    dec dword [command_len_ptr]
    call cursor_backspace_char ; Erase on screen
    jmp .read_char_loop


parse_and_execute_command:
    pusha
    mov esi, command_buffer
    call trim_leading_spaces ; ESI now points to first non-space char or null

    ; --- `help` command ---
    mov edi, cmd_help
    call strncmp_cmd ; Compare command part
    jc .cmd_found_help

    ; --- `cls` or `clear` command ---
    mov edi, cmd_cls
    call strncmp_cmd
    jc .cmd_found_cls
    mov edi, cmd_clear
    call strncmp_cmd
    jc .cmd_found_cls

    ; --- `dir` or `ls` command ---
    mov edi, cmd_dir
    call strncmp_cmd
    jc .cmd_found_dir
    mov edi, cmd_ls
    call strncmp_cmd
    jc .cmd_found_dir

    ; --- `type <filename>` command ---
    mov edi, cmd_type
    call strncmp_cmd
    jc .cmd_found_type

    ; --- `run <filename>` command ---
    mov edi, cmd_run
    call strncmp_cmd
    jc .cmd_found_run

    ; --- `echo <message>` command (from previous) ---
    mov edi, cmd_echo
    call strncmp_cmd
    jc .cmd_found_echo

    ; --- Empty command ---
    cmp byte [esi], 0
    je .unknown_cmd_silent ; If command is empty, just show prompt again

    ; --- Unknown command ---
    mov esi, unknown_cmd_msg
    call print_string
    jmp .done_parsing

.unknown_cmd_silent:
    jmp .done_parsing

.cmd_found_help:
    mov esi, help_msg
    call print_string
    jmp .done_parsing

.cmd_found_cls:
    call clear_screen
    jmp .done_parsing

.cmd_found_dir:
    call fs_list_files
    jmp .done_parsing

.cmd_found_type:
    call extract_argument ; ESI points to argument (filename)
    cmp byte [esi], 0
    je .type_no_arg
    call fs_type_file
    jmp .done_parsing
.type_no_arg:
    mov esi, type_usage_msg
    call print_string
    jmp .done_parsing

.cmd_found_run:
    call extract_argument ; ESI points to argument (filename)
    cmp byte [esi], 0
    je .run_no_arg
    call fs_run_file
    jmp .done_parsing
.run_no_arg:
    mov esi, run_usage_msg
    call print_string
    jmp .done_parsing

.cmd_found_echo:
    call extract_argument ; ESI points to message
    call print_string
    call newline
    jmp .done_parsing

.done_parsing:
    popa
    ret

; --- String/Memory Utilities ---
trim_leading_spaces: ; Modifies string at ESI in place, ESI points to first non-space
.loop:
    cmp byte [esi], ' '
    jne .done
    cmp byte [esi], 0   ; End of string
    je .done
    inc esi
    jmp .loop
.done:
    ret

strncmp_cmd: ; Compares command part of [ESI] with null-terminated command [EDI].
             ; [ESI] is full command line. [EDI] is the command keyword e.g. "help".
             ; Sets CF if command matches and is followed by space or null.
             ; ESI is advanced past the command on match.
    push edi
.loop:
    mov al, [esi]
    mov bl, [edi]
    cmp al, bl       ; Compare characters
    jne .notequal    ; Not equal
    cmp al, 0        ; End of command string (EDI)?
    je .equal_end_cmd ; Both ended, means exact match (e.g. "help" vs "help")
    inc esi
    inc edi
    jmp .loop
.equal_end_cmd: ; Matched up to end of EDI (command keyword)
    ; Now check if char at ESI is space or null
    cmp byte [esi], ' '
    je .matched       ; Followed by space
    cmp byte [esi], 0
    je .matched       ; Followed by null (end of line)
    ; e.g. "helpme" vs "help" -> not a match
    clc
    jmp .done_cmp
.notequal:
    clc ; Clear Carry Flag (not equal)
.done_cmp:
    pop edi
    ret
.matched:
    stc ; Set Carry Flag (equal and valid terminator)
    jmp .done_cmp

extract_argument: ; ESI points to command, advances ESI past command and spaces
                  ; to the start of the argument. Modifies command_buffer.
    ; Skip command part (already implicitly done by strncmp_cmd advancing ESI)
.skip_spaces:
    cmp byte [esi], ' '
    jne .found_arg
    cmp byte [esi], 0
    je .found_arg ; End of string, no arg
    inc esi
    jmp .skip_spaces
.found_arg:
    ; ESI now points to the start of the argument or null terminator
    ret

; --- Filesystem Functions ---
init_fs:
    ; For embedded FS, not much to do. Could validate pointers/sizes here.
    mov esi, fs_init_msg
    call print_string
    ret

fs_find_file: ; Input: ESI points to null-terminated filename string
              ; Output: EBX = pointer to direntry if found, 0 otherwise. CF set if found.
    pusha
    mov ecx, FS_MAX_FILES
    mov ebx, fs_directory_start
.search_loop:
    cmp byte [ebx + direntry.name], 0 ; End of directory marker (empty name)?
    je .not_found

    push esi ; Save filename to search for
    push ebx ; Save current direntry pointer
    mov edi, ebx ; EDI = pointer to direntry.name
    call strcmp_filenames ; Compare ESI (search name) with EDI (direntry name)
    pop ebx
    pop esi

    jc .found ; CF set by strcmp if equal
    add ebx, direntry_size ; Move to next direntry
    loop .search_loop
    ; Fell through loop or max files reached
.not_found:
    mov ebx, 0
    clc ; Clear carry flag
    jmp .done
.found:
    stc ; Set carry flag
.done:
    popa
    ret

strcmp_filenames: ; Compares null-terminated string at [ESI] with fixed-length name at [EDI]
                  ; (MAX_FILENAME_LEN). Case insensitive. Sets CF if equal.
    push esi
    push edi
    push ecx
    mov ecx, MAX_FILENAME_LEN
.loop:
    mov al, [esi]
    mov ah, [edi]

    ; To lower for AL (if A-Z)
    cmp al, 'A'
    jl .al_not_upper
    cmp al, 'Z'
    jg .al_not_upper
    add al, 32 ; 'a' - 'A'
.al_not_upper:

    ; To lower for AH (if A-Z)
    cmp ah, 'A'
    jl .ah_not_upper
    cmp ah, 'Z'
    jg .ah_not_upper
    add ah, 32
.ah_not_upper:

    cmp al, ah
    jne .notequal

    cmp byte [esi], 0 ; End of search string?
    je .check_direntry_end ; If ESI is null, check if EDI name also ends or is padded

    inc esi
    inc edi
    loop .loop
    ; Reached MAX_FILENAME_LEN, and all matched so far.
    ; If ESI is also at its null terminator, then it's a match.
    cmp byte [esi], 0
    je .equal
    jmp .notequal ; Search string is longer than filename field

.check_direntry_end:
    ; ESI is at null. Check if current char in EDI is null or space (padding)
    cmp byte [edi], 0
    je .equal ; Both null, exact match
    cmp byte [edi], ' ' ; Allow space padding in dir entry
    je .equal_padding_loop ; Check rest of dir entry for null or spaces
    jmp .notequal ; ESI is null, EDI has more non-space chars

.equal_padding_loop: ; Check if rest of EDI name is spaces or null
    inc edi
    dec ecx
    jz .equal ; Reached end of MAX_FILENAME_LEN while padding
    cmp byte [edi], 0
    je .equal
    cmp byte [edi], ' '
    jne .notequal
    jmp .equal_padding_loop

.notequal:
    clc
    jmp .done_strcmp
.equal:
    stc
.done_strcmp:
    pop ecx
    pop edi
    pop esi
    ret


fs_list_files:
    pusha
    mov esi, fs_dir_header_msg
    call print_string
    mov ecx, FS_MAX_FILES
    mov ebx, fs_directory_start
.loop:
    cmp byte [ebx + direntry.name], 0 ; End of dir?
    je .done

    ; Print filename (first field of direntry)
    push ebx
    mov esi, ebx ; ESI points to direntry.name
    call print_fixed_len_string_pad ; Prints MAX_FILENAME_LEN chars, padded
    pop ebx

    ; Print size
    mov eax, [ebx + direntry.size]
    call print_number_padded  ; Prints number in EAX, padded
    call print_space

    ; Print type (Exec/Data)
    cmp byte [ebx + direntry.flags], FLAG_EXECUTABLE
    je .is_exec
    mov esi, fs_type_data_msg
    jmp .print_type_attr
.is_exec:
    mov esi, fs_type_exec_msg
.print_type_attr:
    call print_string

    call newline
    add ebx, direntry_size
    loop .loop
.done:
    popa
    ret

fs_type_file: ; ESI = filename
    pusha
    call fs_find_file ; EBX = direntry or 0, CF set if found
    jnc .file_not_found

    ; File found, EBX has direntry pointer
    mov esi, [ebx + direntry.data_ptr]
    mov ecx, [ebx + direntry.size]
    cmp ecx, 0
    je .empty_file ; If size is 0, nothing to print

.print_loop:
    lodsb       ; AL = [ESI], ESI++
    or al, al   ; Check for null terminator (if file is C-string like)
                ; For binary files, this check is wrong. Our text files are null-term.
    jz .done_printing
    call print_char
    loop .print_loop ; Decrement ECX

.done_printing:
    call newline
    jmp .exit
.file_not_found:
    mov esi, file_not_found_msg
    call print_string
    jmp .exit
.empty_file:
    mov esi, file_empty_msg
    call print_string
.exit:
    popa
    ret

fs_run_file: ; ESI = filename
    pusha
    call fs_find_file ; EBX = direntry or 0, CF set if found
    jnc .file_not_found_run

    ; File found, EBX has direntry pointer
    cmp byte [ebx + direntry.flags], FLAG_EXECUTABLE
    jne .not_executable

    mov esi, running_msg
    call print_string
    mov esi, [ebx + direntry.name] ; Print filename being run
    call print_string
    call newline

    mov eax, [ebx + direntry.data_ptr] ; Get function pointer
    call eax                           ; Execute the "program"

    ; After program returns
    call newline
    jmp .exit_run

.file_not_found_run:
    mov esi, file_not_found_msg
    call print_string
    jmp .exit_run
.not_executable:
    mov esi, not_executable_msg
    call print_string
.exit_run:
    popa
    ret

; --- Video Functions ---
clear_screen:
    pusha
    mov edi, VIDEO_MEMORY
    mov ecx, SCREEN_WIDTH * SCREEN_HEIGHT
    mov ax, (DEFAULT_COLOR << 8) | ' '
.loop_clear:
    stosw
    loop .loop_clear
    mov byte [cursor_x_ptr], 0
    mov byte [cursor_y_ptr], 0
    call update_cursor_hw
    popa
    ret

print_char: ; Prints char in AL
    pusha
    cmp al, 0x0A ; Newline
    je .handle_newline_char
    cmp al, 0x0D ; Carriage return (treat as newline for simplicity)
    je .handle_newline_char

    ; Calculate video memory offset: (y * SCREEN_WIDTH + x) * 2
    movzx ebx, byte [cursor_y_ptr]
    imul ebx, SCREEN_WIDTH
    movzx edx, byte [cursor_x_ptr]
    add ebx, edx
    shl ebx, 1 ; Multiply by 2 for char + attribute
    mov edi, VIDEO_MEMORY
    add edi, ebx

    mov ah, DEFAULT_COLOR
    mov [edi], ax ; Store char and attribute

    inc byte [cursor_x_ptr]
    cmp byte [cursor_x_ptr], SCREEN_WIDTH
    jl .update_cursor_only_char
    call newline_logic ; Handles moving to next line and scrolling
    jmp .done_char_print

.handle_newline_char:
    call newline_logic
    jmp .done_char_print

.update_cursor_only_char:
    call update_cursor_hw
.done_char_print:
    popa
    ret

print_string: ; Prints null-terminated string from ESI
.loop_str:
    lodsb
    or al, al
    jz .done_str
    call print_char
    jmp .loop_str
.done_str:
    ret

print_fixed_len_string_pad: ; Prints string from ESI, up to MAX_FILENAME_LEN, pads with spaces
    pusha
    mov ecx, MAX_FILENAME_LEN
.loop_fixed:
    mov al, [esi]
    or al, al ; Null terminator?
    jz .pad_rest
    cmp al, ' ' ; Also treat leading spaces in name as end for printing if desired
                ; but for fixed len, usually print all then pad
    call print_char
    inc esi
    loop .loop_fixed
    jmp .done_fixed ; Max length reached
.pad_rest:
    mov al, ' '
    call print_char
    loop .loop_fixed ; loop will continue with ecx
.done_fixed:
    popa
    ret

print_space:
    push eax
    mov al, ' '
    call print_char
    pop eax
    ret

print_number_padded: ; Prints EAX as unsigned decimal, padded to 8 chars
    pusha
    mov edi, num_buffer_padded + 7 ; End of temp buffer (8 chars for num + null)
    mov byte [edi], 0       ; Null terminator
    dec edi
    mov ebx, 10             ; Divisor
    mov byte [num_print_len_counter], 8 ; Pad to 8 digits
.loop_num:
    xor edx, edx            ; Clear EDX for division
    div ebx                 ; EAX = EAX / 10, EDX = EAX % 10
    add dl, '0'             ; Convert remainder to ASCII
    mov [edi], dl           ; Store digit
    dec edi
    dec byte [num_print_len_counter]
    cmp eax, 0              ; Quotient is 0?
    jne .loop_num           ; No, continue
    ; Fill remaining with spaces if any
.pad_loop_num:
    cmp byte [num_print_len_counter], 0
    je .done_padding_num
    mov byte [edi], ' '
    dec edi
    dec byte [num_print_len_counter]
    jmp .pad_loop_num
.done_padding_num:
    inc edi                 ; Point to start of number string
    mov esi, edi
    call print_string
    popa
    ret
num_buffer_padded: times 9 db 0
num_print_len_counter: db 0


newline: ; Just calls newline_logic
    call newline_logic
    ret

newline_logic: ; Handles cursor to start of next line, scroll if needed
    mov byte [cursor_x_ptr], 0
    inc byte [cursor_y_ptr]
    cmp byte [cursor_y_ptr], SCREEN_HEIGHT
    jl .update_cursor_hw_only
    call scroll_screen_one_line
    mov byte [cursor_y_ptr], SCREEN_HEIGHT - 1
.update_cursor_hw_only:
    call update_cursor_hw
    ret

scroll_screen_one_line:
    pusha
    mov esi, VIDEO_MEMORY + (SCREEN_WIDTH * 2) ; Source: second line
    mov edi, VIDEO_MEMORY                     ; Destination: first line
    mov ecx, SCREEN_WIDTH * (SCREEN_HEIGHT - 1) ; Number of words (char+attr)
    rep movsw                                  ; Copy words

    ; Clear last line
    mov edi, VIDEO_MEMORY + (SCREEN_WIDTH * (SCREEN_HEIGHT - 1) * 2)
    mov ecx, SCREEN_WIDTH
    mov ax, (DEFAULT_COLOR << 8) | ' '
.clear_loop_scroll:
    stosw
    loop .clear_loop_scroll
    popa
    ret

cursor_backspace_char: ; Moves cursor back, prints space, moves cursor back again
    pusha
    cmp byte [cursor_x_ptr], 0
    je .at_line_start ; If at start of line, complex logic (wrap or do nothing)
                      ; For simplicity, do nothing if at col 0
    dec byte [cursor_x_ptr]
    call update_cursor_hw
    mov al, ' '
    call print_char ; This will print space and advance cursor
    dec byte [cursor_x_ptr] ; So move it back again
    call update_cursor_hw
.at_line_start:
    popa
    ret

; Corrected hardware cursor update
update_cursor_hw:
    pushad
    movzx eax, byte [cursor_y_ptr]  ; eax = y
    mov ebx, SCREEN_WIDTH           ; ebx = 80
    mul ebx                         ; edx:eax = y * 80 (eax has result as y < 255)
    movzx ebx, byte [cursor_x_ptr]  ; ebx = x
    add eax, ebx                    ; eax = (y * 80) + x = offset

    mov dx, 0x3D4       ; CRT Control Register port
    mov al, 14          ; Cursor Location High Register index
    out dx, al
    mov dx, 0x3D5       ; CRT Data Register port
    mov al, ah          ; AH contains high byte of offset from (mul ebx)
                        ; If offset > 255, this is correct. If offset < 255, ah is 0.
                        ; EAX from "add eax, ebx" contains the full 16-bit offset.
                        ; So we need the high byte of *that* EAX.
    push eax            ; Save calculated offset
    shr eax, 8          ; eax = high byte
    out dx, al          ; Send high byte

    pop eax             ; Restore calculated offset (low byte is in AL)
    mov dx, 0x3D4
    mov al, 15          ; Cursor Location Low Register index
    out dx, al
    mov dx, 0x3D5       ; CRT Data Register port
    ; AL already contains low byte of offset
    out dx, al          ; Send low byte
    popad
    ret


; --- Keyboard Functions ---
get_char: ; Polls keyboard, returns ASCII in AL (very basic)
.poll:
    in al, 0x64       ; Read status port
    test al, 0x01     ; Test bit 0 (output buffer full)
    jz .poll
    in al, 0x60       ; Read scancode from data port
    test al, 0x80     ; Key release?
    jnz .poll
    cmp al, MAX_SCANCODE
    jnb .unknown_scancode
    movzx ebx, al
    mov al, [scancode_to_ascii + ebx]
    cmp al, 0 ; Non-mappable?
    je .poll
    ret
.unknown_scancode:
    jmp .poll

; --- Data ---
welcome_msg db 'My Simple OS Kernel v0.2 - Filesystem Demo', 0x0D, 0x0A, 0x0A, 0
prompt_msg db 'MYOS> ', 0
unknown_cmd_msg db 'Unknown command. Type "help".', 0x0D, 0x0A, 0
help_msg  db 'Available commands:', 0x0D, 0x0A
          db '  help              Show this help', 0x0D, 0x0A
          db '  cls / clear       Clear screen', 0x0D, 0x0A
          db '  dir / ls          List files', 0x0D, 0x0A
          db '  type <filename>   Display file content', 0x0D, 0x0A
          db '  run <filename>    Execute a program', 0x0D, 0x0A
          db '  echo <message>    Print message', 0x0D, 0x0A, 0
file_not_found_msg db 'File not found.', 0x0D, 0x0A, 0
not_executable_msg db 'File is not executable.', 0x0D, 0x0A, 0
running_msg db 'Running: ', 0
type_usage_msg db 'Usage: type <filename>', 0x0D, 0x0A, 0
run_usage_msg db 'Usage: run <filename>', 0x0D, 0x0A, 0
fs_init_msg db 'Filesystem initialized.', 0x0D, 0x0A, 0
fs_dir_header_msg db 'Directory Listing:', 0x0D, 0x0A
                  db 'Name         Size     Type', 0x0D, 0x0A
                  db '------------ -------- --------', 0x0D, 0x0A, 0
fs_type_data_msg db 'Data', 0
fs_type_exec_msg db 'Exec', 0
file_empty_msg db '(File is empty)', 0x0D, 0x0A, 0

cmd_help db 'help', 0
cmd_cls db 'cls', 0
cmd_clear db 'clear', 0
cmd_dir db 'dir', 0
cmd_ls db 'ls', 0
cmd_type db 'type', 0
cmd_run db 'run', 0
cmd_echo db 'echo', 0

command_buffer: times MAX_CMD_LEN db 0
command_len_ptr: dd 0 ; Stores current length of command in buffer

cursor_x_ptr:
cursor_x db 0
cursor_y_ptr:
cursor_y db 0

; --- Embedded Filesystem Data ---
fs_directory_start:
    ; File 1: HELLO.TXT
    istruc direntry
        at direntry.name,     db "HELLO.TXT",0
        at direntry.data_ptr, dd file_hello_data
        at direntry.size,     dd file_hello_size
        at direntry.flags,    db 0 ; Data file
    iend
    ; File 2: README.MD
    istruc direntry
        at direntry.name,     db "README.MD",0
        at direntry.data_ptr, dd file_readme_data
        at direntry.size,     dd file_readme_size
        at direntry.flags,    db 0 ; Data file
    iend
    ; File 3: PROGRAM1.APP (Executable)
    istruc direntry
        at direntry.name,     db "PROG1.APP",0
        at direntry.data_ptr, dd program1_entry
        at direntry.size,     dd 1 ; Size for executables can be nominal, or actual code size
        at direntry.flags,    db FLAG_EXECUTABLE
    iend
    ; End of directory marker (name starts with null)
    istruc direntry
        at direntry.name, db 0
    iend

file_hello_data:
    db "Hello from HELLO.TXT!", 0x0D, 0x0A
    db "This is a simple text file embedded in the kernel.", 0x0D, 0x0A, 0
file_hello_size equ $ - file_hello_data

file_readme_data:
    db "** README **", 0x0D, 0x0A
    db "This is a very basic operating system demo.", 0x0D, 0x0A
    db "It features an embedded filesystem and a simple shell.", 0x0D, 0x0A
    db "Commands: help, cls, dir, type, run, echo.", 0x0D, 0x0A, 0
file_readme_size equ $ - file_readme_data

; "Executable" program
program1_entry:
    pusha
    mov esi, prog1_msg
    call print_string
    ; Simulate some work
    mov ecx, 0xFFFFFF
.delay_loop:
    dec ecx
    jnz .delay_loop
    mov esi, prog1_done_msg
    call print_string
    popa
    ret
prog1_msg: db "[PROG1.APP] Hello! I am a simple program running.", 0x0D, 0x0A, 0
prog1_done_msg: db "[PROG1.APP] Work complete. Returning to shell.", 0x0D, 0x0A, 0


; Scancode to ASCII lookup table (Set 1, partial)
scancode_to_ascii:
    db 0, 27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', 8, 9
    db 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', 13, 0
    db 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '''' , '`', 0, '\'
    db 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', 0, '*', 0, ' ', 0
MAX_SCANCODE equ $ - scancode_to_ascii - 1

; --- Kernel Stack ---
KERNEL_STACK_SIZE equ 8192 ; 8KB stack (increased a bit)
kernel_stack_bottom:
    times KERNEL_STACK_SIZE db 0
kernel_stack_top: