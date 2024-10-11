cmake_minimum_required(VERSION 3.29)
project(clox C)

set(CMAKE_C_STANDARD 11)

add_executable(clox main.c
        chunk.h
        chunk.c
        memory.h
        memory.c
        debug.h
        debug.c
        value.h
        value.c
        vm.h
        vm.c
        compiler.h
        compiler.c
        scanner.h
        scanner.c
        object.h
        object.c
        table.h
        table.c)
