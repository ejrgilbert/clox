#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// See: https://craftinginterpreters.com/calls-and-functions.html#the-call-stack
// represents a single ongoing function call
typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    // points into the VM’s value stack at the first slot that this function can use
    Value* slots;
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX];
    Value* stackTop;
    Table globals;
    // String interning: https://craftinginterpreters.com/hash-tables.html#string-interning
    Table strings;
    ObjUpvalue* openUpvalues;
    // GC: a pointer to the head of the list of objects
    Obj* objects;
    // See: https://craftinginterpreters.com/garbage-collection.html#a-worklist-for-gray-objects
    int grayCount;
    int grayCapacity;
    Obj** grayStack;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif