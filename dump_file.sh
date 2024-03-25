#!/bin/bash
riscv64-unknown-elf-objdump -b binary -m riscv:rv32e -D file.bin
