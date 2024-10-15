#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch (object->type) {
        case OBJ_CLASS: {
            FREE(ObjClass, object);
            break;
        }
        case OBJ_CLOSURE: {
            // ObjClosure does not own the ObjUpvalue objects themselves, but it does own the array containing
            // pointers to those upvalues.
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues,
                       closure->upvalueCount);
            // We free only the ObjClosure itself, not the ObjFunction. That’s because the closure
            // doesn’t own the function. There may be multiple closures that all reference the same
            // function, and none of them claims any special privilege over it. We can’t free the
            // ObjFunction until all objects referencing it are gone—including even the surrounding
            // function whose constant table contains it.
            // Tracking that sounds tricky, and it is! That’s why we’ll write a garbage collector
            // soon to manage it for us.
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_FUNCTION: {
            // We don’t need to explicitly free the function’s name because it’s an ObjString.
            // That means we can let the garbage collector manage its lifetime for us.
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        case OBJ_NATIVE:
            FREE(ObjNative, object);
        break;
        case OBJ_STRING: {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        case OBJ_UPVALUE:
            // Multiple closures can close over the same variable, so ObjUpvalue does not own the variable it references.
            FREE(ObjUpvalue, object);
        break;
    }
}
void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.grayStack);
}
void markObject(Obj* object) {
    if (object == NULL) return;

    // References between objects are directed, but that doesn't mean they’re acyclic! It’s entirely possible to
    // have cycles of objects. When that happens, we need to ensure our collector doesn't get stuck in an infinite
    // loop as it continually re-adds the same series of objects to the gray stack.

    // If the object is already marked, we don’t mark it again and thus don’t add it to the gray stack. This ensures
    // that an already-gray object is not redundantly added and that a black object is not inadvertently turned back
    // to gray. In other words, it keeps the wavefront moving forward through only the white objects.
    if (object->isMarked) return;
#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;

    // See: https://craftinginterpreters.com/garbage-collection.html#tracing-object-references
    if (vm.grayCapacity < vm.grayCount + 1) {
        // We could use any kind of data structure that lets us put items in and take them out easily.
        // I picked a stack because that’s the simplest to implement with a dynamic array in C. It works
        // mostly like other dynamic arrays we’ve built in Lox, except, note that it calls the system
        // realloc() function and not our own reallocate() wrapper. The memory for the gray stack itself
        // is not managed by the garbage collector. We don’t want growing the gray stack during a GC to
        // cause the GC to recursively start a new GC. That could tear a hole in the space-time continuum.
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack = (Obj**)realloc(vm.grayStack,
                                      sizeof(Obj*) * vm.grayCapacity);

        // Should be more robust, but okay for now
        if (vm.grayStack == NULL) exit(1);
    }

    vm.grayStack[vm.grayCount++] = object;
}
void markValue(Value value) {
    // Some Lox values—numbers, Booleans, and nil—are stored directly inline in Value and require
    // no heap allocation. The garbage collector doesn’t need to worry about them at all, so the
    // first thing we do is ensure that the value is an actual heap object.
    if (IS_OBJ(value)) markObject(AS_OBJ(value));
}
static void markArray(ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}
static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif
    // Each object kind has different fields that might reference other objects, so we need a specific
    // blob of code for each type.

    // An easy optimization we could do in markObject() is to skip adding strings and native functions to
    // the gray stack at all since we know they don’t need to be processed. Instead, they could darken from
    // white straight to black.

    // Note that we don’t set any state in the traversed object itself. There is no direct encoding of
    // “black” in the object’s state. A black object is any object whose isMarked field is set and that
    // is no longer in the gray stack.
    switch (object->type) {
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*)object;
            markObject((Obj*)klass->name);
            break;
        }
        case OBJ_CLOSURE: {
            // Each closure has a reference to the bare function it wraps, as well as an array of pointers to
            // the upvalues it captures. We trace all of those.
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
        case OBJ_UPVALUE:
            // When an upvalue is closed, it contains a reference to the closed-over value. Since the value
            // is no longer on the stack, we need to make sure we trace the reference to it from the upvalue.
            markValue(((ObjUpvalue*)object)->closed);
        break;
        case OBJ_NATIVE:
            // contain no outgoing references so there is nothing to traverse.
        case OBJ_STRING:
            // contain no outgoing references so there is nothing to traverse.
          break;
    }
}
static void markRoots() {
    // Most roots are local variables or temporaries sitting right in the VM’s stack,
    // so we start by walking that.
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    // Most function call state lives in the value stack, but the VM maintains a separate stack
    // of CallFrames. Each CallFrame contains a pointer to the closure being called. The VM uses
    // those pointers to access constants and upvalues, so those closures need to be kept around too.
    for (int i = 0; i < vm.frameCount; i++) {
        markObject((Obj*)vm.frames[i].closure);
    }

    // the open upvalue list is another set of values that the VM can directly reach.
    for (ObjUpvalue* upvalue = vm.openUpvalues;
           upvalue != NULL;
           upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    // Marking the stack takes care of local variables and temporaries. The other main source
    // of roots are the global variables. Those live in a hash table owned by the VM.
    markTable(&vm.globals);

    // Remember also that a collection can begin during any allocation. Those allocations don’t just
    // happen while the user’s program is running. The compiler itself periodically grabs memory from
    // the heap for literals and the constant table. If the GC runs while we’re in the middle of compiling,
    // then any values the compiler directly accesses need to be treated as roots too.

    // To keep the compiler module cleanly separated from the rest of the VM, we'll do that in a separate function.
    markCompilerRoots();
}
static void traceReferences() {
    // It’s as close to that textual algorithm as you can get. Until the stack empties, we keep pulling out gray
    // objects, traversing their references, and then marking them black. Traversing an object’s references may
    // turn up new white objects that get marked gray and added to the stack. So this function swings back and forth
    // between turning white objects gray and gray objects black, gradually advancing the entire wavefront forward.
    while (vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}
static void sweep() {
    // See: https://craftinginterpreters.com/garbage-collection.html#sweeping-unused-objects
    Obj* previous = NULL;
    Obj* object = vm.objects;
    while (object != NULL) {
        if (object->isMarked) {
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

#define GC_HEAP_GROW_FACTOR 2
void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

    // MARK
    markRoots();
    traceReferences();

    // handle string pool weak references
    // See: https://craftinginterpreters.com/garbage-collection.html#weak-references-and-the-string-pool
    tableRemoveWhite(&vm.strings);

    // When the loop in traceReferences() exits, we have processed all the objects we could get our hands on.
    // The gray stack is empty, and every object in the heap is either black or white. The black objects are
    // reachable, and we want to hang on to them. Anything still white never got touched by the trace and is
    // thus garbage. All that’s left is to reclaim them.
    // SWEEP
    sweep();

    // The sweep phase frees objects by calling reallocate(), which lowers the value of bytesAllocated,
    // so after the collection completes, we know how many live bytes remain. We adjust the threshold of
    // the next GC based on that.

    // The threshold is a multiple of the heap size. This way, as the amount of memory the program uses grows,
    // the threshold moves farther out to limit the total time spent re-traversing the larger live set.
    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
    printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
         before - vm.bytesAllocated, before, vm.bytesAllocated,
         vm.nextGC);
#endif
}
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    // Every time we allocate or free some memory, we adjust the counter by that delta.
    vm.bytesAllocated += newSize - oldSize;
    if (newSize > oldSize) {
        // Whenever we call reallocate() to acquire more memory, we force a collection to run.
        // The if check is because reallocate() is also called to free or shrink an allocation.
        // We don’t want to trigger a GC for that—in particular because the GC itself will call
        // reallocate() to free memory.

        // If you don’t use allocation to trigger a GC, you have to make sure every possible place in
        // code where you can loop and allocate memory also has a way to trigger the collector. Otherwise,
        // the VM can get into a starved state where it needs more memory but never collects any.

        // More sophisticated collectors might run on a separate thread or be interleaved periodically
        // during program execution—often at function call boundaries or when a backward jump occurs.
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif

        // When the total crosses the limit, we run the collector.
        if (vm.bytesAllocated > vm.nextGC) {
            collectGarbage();
        }
    }
    // Since all we passed in was a bare pointer to the first byte of memory,
    // what does it mean to “update” the block’s size? Under the hood, the
    // memory allocator maintains additional bookkeeping information for each
    // block of heap-allocated memory, including its size.
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);

    return result;
}