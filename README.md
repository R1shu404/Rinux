# Rinix - A Tiny x86 Hobby OS

Rinix (a playful blend of "Rishi's OS" or "Really Tiny Unix-like") is a minimal, educational, 16/32-bit x86 operating system written entirely in NASM assembly. It's designed to demonstrate the fundamental concepts of OS development, from booting to a simple interactive shell with an embedded filesystem.

**This project is for learning purposes ONLY. It is NOT a production-ready OS.**

## Features

*   **16-bit Bootloader (`boot.asm`):**
    *   Loads from a floppy disk image.
    *   Prints loading messages via BIOS.
    *   Loads the 32-bit kernel into memory.
    *   Initializes the Global Descriptor Table (GDT).
    *   Enables the A20 line.
    *   Switches the CPU from 16-bit Real Mode to 32-bit Protected Mode.
*   **32-bit Kernel (`kernel.asm`):**
    *   Basic text-mode video output (direct memory writing to `0xB8000`).
    *   Hardware cursor control.
    *   Polling-based keyboard input (scancode to ASCII conversion for US QWERTY).
    *   Simple command-line shell with an input buffer and backspace support.
    *   **Embedded Filesystem:**
        *   Directory structure stored within the kernel.
        *   Support for "text files" (data embedded in kernel) and simple "executables" (subroutines within the kernel).
    *   **Shell Commands:**
        *   `help`: Displays available commands.
        *   `cls` / `clear`: Clears the screen.
        *   `dir` / `ls`: Lists files in the embedded filesystem.
        *   `type <filename>`: Displays the content of a text file.
        *   `run <filename>`: "Executes" a predefined program (a kernel subroutine).
        *   `echo <message>`: Prints a message to the screen.

## Current State & Limitations

Rinix is extremely barebones. Its "Unix-like" nature is mostly in the command-prompt style and conceptual separation of bootloader/kernel.

*   **No Real Hardware Drivers:** Relies on BIOS for initial disk read and basic keyboard/VGA text mode. No custom drivers for other hardware.
*   **No Interrupt Handling (by Kernel):** Interrupts are disabled (`cli`) before switching to protected mode and are not re-enabled or handled by the kernel. Input is done via polling.
*   **No Multitasking/Processes:** Single-tasking environment. The `run` command calls a subroutine directly.
*   **No Memory Protection (beyond GDT segments):** No paging, so no true process separation or virtual memory.
*   **No Dynamic Memory Allocation:** All memory (stack, buffers, filesystem) is statically defined at compile time.
*   **No Real Filesystem:** The "filesystem" is embedded within the kernel binary. No support for reading from/writing to actual disk partitions (beyond the initial kernel load).
*   **Limited Error Handling:** Very basic error messages, minimal crash resistance.
*   **No Networking, Sound, GUI, etc.**
*   **Security:** Non-existent.
*   **Portability:** Strictly x86.

## Prerequisites

*   **NASM (Netwide Assembler):** To assemble the `.asm` files.
*   **QEMU (or another x86 emulator like Bochs, VirtualBox):** To run the OS image. `qemu-system-i386` is recommended.

## Building and Running

1.  **Assemble the Bootloader:**
    ```bash
    nasm boot.asm -f bin -o boot.bin
    ```
2.  **Assemble the Kernel:**
    ```bash
    nasm kernel.asm -f bin -o kernel.bin
    ```
3.  **Create the Floppy Image:**
    Concatenate the bootloader and kernel binaries. The bootloader (`boot.bin`) must be first.
    ```bash
    cat boot.bin kernel.bin > rinix.img
    ```
    On Windows, you might use:
    ```batch
    copy /b boot.bin + kernel.bin rinix.img
    ```
4.  **Run in QEMU:**
    ```bash
    qemu-system-i386 -fda rinix.img
    ```
    If you encounter issues, especially with disk geometry or kernel size, ensure `boot.asm` reads enough sectors to load the entire `kernel.bin`. `kernel.bin`'s size should be checked after assembly.

## Usage

Once Rinix boots, you'll see a welcome message and the `MYOS>` prompt. You can type the following commands:

*   `help`: Shows the help message.
*   `cls` or `clear`: Clears the terminal screen.
*   `dir` or `ls`: Lists files in the embedded "filesystem".
*   `type FILENAME.EXT`: Displays the content of the specified text file (e.g., `type HELLO.TXT`). Filenames are case-insensitive for lookup.
*   `run FILENAME.EXT`: Executes a pre-defined "program" (e.g., `run PROG1.APP`).
*   `echo This is a message`: Prints "This is a message" to the screen.

## How It Works (For Learners)

See the detailed explanation below this README.

## Future Ideas (If this were to be expanded significantly)

*   Implement an Interrupt Descriptor Table (IDT) and handle basic hardware interrupts (timer, keyboard).
*   Develop a simple memory manager (e.g., bitmap allocator).
*   Implement paging for virtual memory.
*   Write a basic disk driver (e.g., for ATA PIO) and a simple filesystem (like FAT12).
*   Introduce multitasking with process switching.
*   Develop a simple system call interface.
*   Basic VGA graphics mode.

## License

This project is released under the [MIT License](LICENSE.md)

## Disclaimer

Rinix is a hobby project created for educational purposes. It comes with absolutely no warranty. Do not use it for any critical tasks. Have fun exploring and learning!