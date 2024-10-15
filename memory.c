#include <stdlib.h>

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
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
#endif

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
#endif
}

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
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