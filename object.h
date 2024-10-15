#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_CLASS(value)        isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value)      isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value)     isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value)       isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)       isObjType(value, OBJ_STRING)

#define AS_CLASS(value)        ((ObjClass*)AS_OBJ(value))
#define AS_CLOSURE(value)      ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value)     ((ObjFunction*)AS_OBJ(value))
#define AS_INSTANCE(value)     ((ObjInstance*)AS_OBJ(value))
#define AS_NATIVE(value) \
    (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))

#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE
} ObjType;

struct Obj {
    ObjType type;
    bool isMarked;
    // See: https://craftinginterpreters.com/strings.html#freeing-objects
    // GC: pointer to the next Obj in the chain
    struct Obj* next;
};
typedef struct {
    // Functions are first class in Lox, so they need to be actual Lox objects
    // Thus ObjFunction has the same Obj header that all object types share.
    Obj obj;
    // the number of parameters the function expects
    int arity;
    int upvalueCount;
    Chunk chunk;
    // the function’s name
    ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);
typedef struct {
    Obj obj;
    // pointer to the C function that implements the native behavior
    NativeFn function;
} ObjNative;

struct ObjString {
    Obj obj;
    int length;
    char* chars;
    // Hash cache, see: https://craftinginterpreters.com/hash-tables.html#hashing-strings
    uint32_t hash;
};
typedef struct ObjUpvalue {
    Obj obj;
    Value* location;
    Value closed;
    struct ObjUpvalue* next;
} ObjUpvalue;

typedef struct {
    // points to an ObjFunction and adds the necessary object header stuff
    Obj obj;
    ObjFunction* function;
    // https://craftinginterpreters.com/closures.html#upvalues-in-closures
    ObjUpvalue** upvalues;
    int upvalueCount;
} ObjClosure;

typedef struct {
    Obj obj;
    ObjString* name;
} ObjClass;

typedef struct {
    Obj obj;
    ObjClass* klass;
    // Lox lets users freely add fields to an instance at runtime. This means we need a storage mechanism
    // that can grow. We could use a dynamic array, but we also want to look up fields by name as quickly as
    // possible. There’s a data structure that’s just perfect for quickly accessing a set of values by name
    // and—even more conveniently—we’ve already implemented it. Each instance stores its fields using a hash table.
    Table fields;
} ObjInstance;

ObjClass* newClass(ObjString* name);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjInstance* newInstance(ObjClass* klass);
ObjNative* newNative(NativeFn function);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif