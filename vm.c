#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

// The choice to have a static VM instance is a concession for the book,
// but not necessarily a sound engineering choice for a real language
// implementation. If you’re building a VM that’s designed to be embedded
// in other host applications, it gives the host more flexibility if you do
// explicitly take a VM pointer and pass it around.
//
// That way, the host app can control when and where memory for the VM is
// allocated, run multiple VMs in parallel, etc.
//
// What I’m doing here is a global variable, and everything bad you’ve heard
// about global variables is still true when programming in the large.
// But when keeping things small for a book...
VM vm;

static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}
static void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
}
static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    // After printing the error message itself, we walk the call stack from top (the most
    // recently called function) to bottom (the top-level code). For each frame, we find
    // the line number that corresponds to the current ip inside that frame’s function.
    // Then we print that line number along with the function name.
    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        // The - 1 is because the IP is already sitting on the next instruction to be executed,
        // but we want the stack trace to point to the previous failed instruction.
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ",
                function->chunk.lines[instruction]);
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }
    resetStack();
}
static void defineNative(const char* name, NativeFn function) {
    // GC: You’re probably wondering why we push and pop the name and function on the stack.
    // That looks weird, right? This is the kind of stuff you have to worry about when garbage
    // collection gets involved. Both copyString() and newNative() dynamically allocate memory.
    // That means once we have a GC, they can potentially trigger a collection. If that happens,
    // we need to ensure the collector knows we’re not done with the name and ObjFunction so that
    // it doesn’t free them out from under us. Storing them on the value stack accomplishes that.
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM() {
    resetStack();
    vm.objects = NULL;

    // The starting threshold here is arbitrary. It’s similar to the initial capacity we picked for
    // our various dynamic arrays. The goal is to not trigger the first few GCs too quickly but also
    // to not wait too long. If we had some real-world Lox programs, we could profile those to tune
    // this. But since all we have are toy programs, I just picked a number.
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    initTable(&vm.globals);
    initTable(&vm.strings);

    // Some GC protection
    vm.initString = NULL;
    vm.initString = copyString("init", 4);

    defineNative("clock", clockNative);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    vm.initString = NULL;
    // Eventually, the garbage collector will free memory while the VM is still running.
    // But, even then, there will usually be unused objects still lingering in memory
    // when the user’s program completes. The VM should free those too.
    freeObjects();
}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}
static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}
static bool call(ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments but got %d.",
            closure->function->arity, argCount);
        return false;
    }
    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    // This simply initializes the next CallFrame on the stack. It stores a pointer to
    // the function being called and points the frame’s ip to the beginning of the
    // function’s bytecode. Finally, it sets up the slots pointer to give the frame
    // its window into the stack. The arithmetic there ensures that the arguments
    // already on the stack line up with the function’s parameters
    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;

    // The funny little - 1 is to account for stack slot zero which the compiler set
    // aside for when we add methods later
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}
static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
                // place receiver in slot zero
                vm.stackTop[-argCount - 1] = bound->receiver;
                return call(bound->method, argCount);
            }
            case OBJ_CLASS: {
                ObjClass* klass = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
                Value initializer;
                if (tableGet(&klass->methods, vm.initString,
                             &initializer)) {
                    return call(AS_CLOSURE(initializer), argCount);
                } else if (argCount != 0) {
                    runtimeError("Expected 0 arguments but got %d.",
                          argCount);
                    return false;
                }

                return true;
            }
            case OBJ_CLOSURE:
                // We remove the code for calling objects whose type is OBJ_FUNCTION. Since we wrap all
                // functions in ObjClosures, the runtime will never try to invoke a bare ObjFunction anymore.
                // Those objects live only in constant tables and get immediately wrapped in closures before
                // anything else sees them.
                return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE: {
                NativeFn native = AS_NATIVE(callee);
                Value result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default:
                break; // Non-callable object type.
        }
    }
    runtimeError("Can only call functions and classes.");
    return false;
}
static bool bindMethod(ObjClass* klass, ObjString* name) {
    // First we look for a method with the given name in the class’s method table. If we don’t find one,
    // we report a runtime error and bail out. Otherwise, we take the method and wrap it in a new ObjBoundMethod.
    // We grab the receiver from its home on top of the stack. Finally, we pop the instance and replace the top
    // of the stack with the bound method.
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peek(0),
                                           AS_CLOSURE(method));
    pop();
    push(OBJ_VAL(bound));
    return true;
}
static ObjUpvalue* captureUpvalue(Value* local) {
    // See: https://craftinginterpreters.com/closures.html#tracking-open-upvalues
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }
    ObjUpvalue* createdUpvalue = newUpvalue(local);

    // The local slot we stopped at is the slot we’re looking for. We found an existing
    // upvalue capturing the variable, so we reuse that upvalue.
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        // We ran out of upvalues to search. When upvalue is NULL, it means every open upvalue
        // in the list points to locals above the slot we’re looking for, or (more likely) the
        // upvalue list is empty. Either way, we didn’t find an upvalue for our slot.
        vm.openUpvalues = createdUpvalue;
    } else {
        // We found an upvalue whose local slot is below the one we’re looking for. Since the list is
        // sorted, that means we’ve gone past the slot we are closing over, and thus there must not
        // be an existing upvalue for it.
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}
static void closeUpvalues(Value* last) {
    // See: https://craftinginterpreters.com/closures.html#closing-upvalues-at-runtime
    while (vm.openUpvalues != NULL &&
           vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}
static void defineMethod(ObjString* name) {
    Value method = peek(0);
    ObjClass* klass = AS_CLASS(peek(1));
    tableSet(&klass->methods, name, method);
    pop();
}
static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}
static void concatenate() {
    ObjString* b = AS_STRING(peek(0));
    ObjString* a = AS_STRING(peek(1));

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    pop();
    pop();
    push(OBJ_VAL(result));
}

static InterpretResult run() {
    // store the current topmost CallFrame in a local variable inside the main bytecode execution function.
    CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
    (frame->ip += 2, \
    (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() \
    (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
    do { \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
    runtimeError("Operands must be numbers."); \
    return INTERPRET_RUNTIME_ERROR; \
    } \
    double b = AS_NUMBER(pop()); \
    double a = AS_NUMBER(pop()); \
    push(valueType(a op b)); \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(&frame->closure->function->chunk,
            (int)(frame->ip - frame->closure->function->chunk.code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                printf("\n");
                break;
            }
            case OP_NIL: push(NIL_VAL); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_POP: pop(); break;
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                // accesses the given numbered slot relative to the beginning of that frame
                push(frame->slots[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                // It takes the assigned value from the top of the stack and stores
                // it in the stack slot corresponding to the local variable. Note that
                // it doesn’t pop the value from the stack. Remember, assignment is an
                // expression, and every expression produces a value. The value of an
                // assignment expression is the assigned value itself, so the VM just
                // leaves the value on the stack.
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                // GC: Note that we don’t pop the value until after we add it to the hash table.
                //     That ensures the VM can still find the value if a garbage collection is
                //     triggered right in the middle of adding it to the hash table. That’s a
                //     distinct possibility since the hash table requires dynamic allocation
                //     when it resizes.
                ObjString* name = READ_STRING();
                tableSet(&vm.globals, name, peek(0));
                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }
            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(0));
                ObjString* name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    pop(); // Instance.
                    push(value);
                    break;
                }

                // We insert this after the code to look up a field on the receiver instance. Fields take priority
                // over and shadow methods, so we look for a field first. If the instance does not have a field
                // with the given property name, then the name may refer to a method.
                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(1));
                tableSet(&instance->fields, READ_STRING(), peek(0));
                Value value = pop();
                pop();
                push(value);
                break;
            }
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError(
                        "Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT:
                push(BOOL_VAL(isFalsey(pop())));
            break;
            case OP_NEGATE:
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
            push(NUMBER_VAL(-AS_NUMBER(pop())));
            break;
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                // See: https://craftinginterpreters.com/calls-and-functions.html#binding-arguments-to-parameters
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                // If callValue() is successful, there will be a new frame on the CallFrame stack for the called
                // function. The run() function has its own cached pointer to the current frame, so we need to
                // update that.
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);
                push(OBJ_VAL(closure));

                // This code is the magic moment when a closure comes to life. We iterate over each upvalue
                // the closure expects. For each one, we read a pair of operand bytes. If the upvalue closes
                // over a local variable in the enclosing function, we let captureUpvalue() do the work.
                // Otherwise, we capture an upvalue from the surrounding function.
                // An OP_CLOSURE instruction is emitted at the end of a function declaration. At the moment
                // that we are executing that declaration, the current function is the surrounding one. That
                // means the current function’s closure is stored in the CallFrame at the top of the callstack.
                // So, to grab an upvalue from the enclosing function, we can read it right from the frame local
                // variable, which caches a reference to that CallFrame.
                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] =
                            captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                break;
            }
            case OP_CLOSE_UPVALUE:
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;
            case OP_RETURN: {
                // See: https://craftinginterpreters.com/calls-and-functions.html#returning-from-functions
                Value result = pop();
                closeUpvalues(frame->slots);
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLASS:
                push(OBJ_VAL(newClass(READ_STRING())));
            break;
            case OP_METHOD:
                defineMethod(READ_STRING());
            break;
        }
    }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    // Now you can see why the compiler sets aside stack slot zero—that stores the function being called
    push(OBJ_VAL(function));
    // set up the first CallFrame for executing the top-level code
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);
    return run();
}