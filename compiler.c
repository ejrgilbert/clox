#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

// All Lox precedence levels in order from lowest to highest.
typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;
typedef void (*ParseFn)(bool canAssign);
typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    // records the scope depth of the block where the local variable was declared
    int depth;
    // This field is true if the local is captured by any later nested function declaration.
    // Initially, all locals are not captured.
    bool isCaptured;
} Local;
typedef struct {
    // stores which local slot the upvalue is capturing
    uint8_t index;
    bool isLocal;
} Upvalue;
typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT
} FunctionType;

// See: https://craftinginterpreters.com/calls-and-functions.html#a-stack-of-compilers
typedef struct Compiler {
    struct Compiler* enclosing;
    // The function object being built
    ObjFunction* function;
    // This lets the compiler tell when it’s compiling top-level code versus the body of a function.
    FunctionType type;
    Local locals[UINT8_COUNT];
    // tracks how many locals are in scope — how many of those array slots are in use
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    // the number of blocks surrounding the current bit of code we’re compiling
    int scopeDepth;
} Compiler;

Parser parser;
Compiler* current = NULL;


Chunk* compilingChunk;
static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}
static void error(const char* message) {
    errorAt(&parser.previous, message);
}
static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    // steps forward through the token stream
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}
static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}
static bool check(TokenType type) {
    return parser.current.type == type;
}
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}
static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}
static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}
static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}
static void emitReturn() {
    emitByte(OP_NIL);
    emitByte(OP_RETURN);
}
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}
static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}
static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}
static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    // I know, it looks dumb to null the function field only to immediately assign
    // it a value a few lines later. More garbage collection-related paranoia.
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        // Note that we’re careful to create a copy of the name string. Remember,
        // the lexeme points directly into the original source code string. That
        // string may get freed once the code is finished compiling. The function
        // object we create in the compiler outlives the compiler and persists until
        // runtime. So it needs its own heap-allocated name string that it can keep
        // around.
        current->function->name = copyString(parser.previous.start,
                                             parser.previous.length);
    }

    // Remember that the compiler’s locals array keeps track of which stack slots are
    // associated with which local variables or temporaries. From now on, the compiler
    // implicitly claims stack slot zero for the VM’s own internal use. We give it an
    // empty name so that the user can’t write an identifier that refers to it.
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    local->name.start = "";
    local->name.length = 0;
}
static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL
            ? function->name->chars : "<script>");
    }
#endif
    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}
static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth >
           current->scopeDepth) {

        // See: https://craftinginterpreters.com/closures.html#closing-upvalues
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }

        current->localCount--;
    }
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static int resolveLocal(Compiler* compiler, Token* name);
static int resolveUpvalue(Compiler* compiler, Token* name);
static uint8_t identifierConstant(Token* name);
static void and_(bool canAssign);
static uint8_t argumentList();

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        default: return; // Unreachable.
    }
}
static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}
static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}
static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: return; // Unreachable.
    }
}
static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}
static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}
static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}
static void string(bool canAssign) {
    // The + 1 and - 2 parts trim the leading and trailing quotation marks
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                    parser.previous.length - 2)));
}
static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        // https://craftinginterpreters.com/closures.html#compiling-upvalues
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}
static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}
static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return; // Unreachable.
    }
}

// table is full of NULL and PREC_NONE. Most of those empty cells are because
// there is no expression associated with those tokens. You can’t start an
// expression with, say, else, and } would make for a pretty confusing infix
// operator.
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};
static void parsePrecedence(Precedence precedence) {
    // See: https://craftinginterpreters.com/compiling-expressions.html#parsing-with-precedence

    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    // See: https://craftinginterpreters.com/global-variables.html#assignment
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start,
                                           name->length)));
}
static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}
static int resolveLocal(Compiler* compiler, Token* name) {
    // At runtime, we load and store locals using the stack slot index, so that’s
    // what the compiler needs to calculate after it resolves the variable. Whenever
    // a variable is declared, we append it to the locals array in Compiler. That
    // means the first local variable is at index zero, the next one is at index one,
    // and so on. In other words, the locals array in the compiler has the exact same
    // layout as the VM’s stack will have at runtime. The variable’s index in the locals
    // array is the same as its stack slot.
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}
static int addUpvalue(Compiler* compiler, uint8_t index,
                      bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;
    for (int i = 0; i < upvalueCount; i++) {
        // If we find an upvalue in the array whose slot index matches the one we’re adding, we just return
        // that upvalue index and reuse it. Otherwise, we fall through and add the new upvalue.
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }
    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }
    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}
static int resolveUpvalue(Compiler* compiler, Token* name) {
    // We call this after failing to resolve a local variable in the current function’s scope, so we know the
    // variable isn’t in the current compiler. Recall that Compiler stores a pointer to the Compiler for the
    // enclosing function, and these pointers form a linked chain that goes all the way to the root Compiler
    // for the top-level code. Thus, if the enclosing Compiler is NULL, we know we’ve reached the outermost
    // function without finding a local variable. The variable must be global, so we return -1.
    // Otherwise, we try to resolve the identifier as a local variable in the enclosing compiler.
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // See: https://craftinginterpreters.com/closures.html#flattening-upvalues
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }
    return -1;
}
static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}
static void declareVariable() {
    // This is the point where the compiler records the existence of the variable.
    // We only do this for locals, so if we’re in the top-level global scope, we just
    // bail out. Because global variables are late bound, the compiler doesn’t keep
    // track of which declarations for them it has seen.
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;

    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}
static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);
    declareVariable();
    // At runtime, locals aren’t looked up by name. There’s no need to stuff the variable’s
    // name into the constant table, so if the declaration is inside a local scope,
    // we return a dummy table index instead.
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}
static void markInitialized() {
    // Before, we called markInitialized() only when we already knew we were in a local scope.
    // Now, a top-level function declaration will also call this function. When that happens,
    // there is no local variable to mark initialized—the function is bound to a global variable.
    if (current->scopeDepth == 0) return;
    current->locals[current->localCount - 1].depth =
        current->scopeDepth;
}
static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        // There is no code to create a local variable at runtime. Think about what state the VM is in.
        // It has already executed the code for the variable’s initializer (or the implicit nil if the
        // user omitted an initializer), and that value is sitting right on top of the stack as the only
        // remaining temporary. We also know that new locals are allocated at the top of the stack...right
        // where that value already is. Thus, there’s nothing to do. The temporary simply becomes the local
        // variable.
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}
static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}
static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}
static ParseRule* getRule(TokenType type) {
    return &rules[type];
}
static void expression() {
    // We simply parse the lowest precedence level,
    // which subsumes all the higher-precedence expressions too
    parsePrecedence(PREC_ASSIGNMENT);
}
static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}
static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        // See: https://craftinginterpreters.com/calls-and-functions.html#function-parameters
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}
static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
}
static void funDeclaration() {
    // See: https://craftinginterpreters.com/calls-and-functions.html#function-declarations
    // A function declaration at the top level will bind the function to a global variable.
    // Inside a block or other function, a function declaration creates a local variable.
    uint8_t global = parseVariable("Expect function name.");
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global);
}
static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON,
            "Expect ';' after variable declaration.");

    defineVariable(global);
}
static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}
static void forStatement() {
    // If a for statement declares a variable, that variable should be scoped to
    // the loop body. We ensure that by wrapping the whole statement in a scope.
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP); // Condition.
    }
    if (!match(TOKEN_RIGHT_PAREN)) {
        // See: https://craftinginterpreters.com/jumping-back-and-forth.html#increment-clause
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);
    if (exitJump != -1) {
        // We do this only when there is a condition clause. If there isn’t, there’s no
        // jump to patch and no condition value on the stack to pop.
        patchJump(exitJump);
        emitByte(OP_POP); // Condition.
    }
    endScope();
}
static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // conditional jump
    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // remove condition from stack (on then entry)
    statement();
    // unconditional jump
    int elseJump = emitJump(OP_JUMP);

    patchJump(thenJump);
    // remove condition from stack (if was false, will be jumped over by OP_JUMP above)
    emitByte(OP_POP);
    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}
static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}
static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}
static void whileStatement() {
    // After executing the body of a while loop, we jump all the way back to before the condition.
    // That way, we re-evaluate the condition expression on each iteration. We store the chunk’s
    // current instruction count in loopStart to record the offset in the bytecode right before the
    // condition expression we’re about to compile.
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
}
static void synchronize() {
    // See: https://craftinginterpreters.com/global-variables.html#error-synchronization
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
              return;

            default:
                ; // Do nothing.
        }

        advance();
    }
}
static void declaration() {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    // See: https://craftinginterpreters.com/global-variables.html#error-synchronization
    if (parser.panicMode) synchronize();
}
static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    // will reset these at synchronization point (statement parsing)
    parser.hadError = false;
    parser.panicMode = false;

    int line = -1;
    advance();
    while (!match(TOKEN_EOF)) {
        declaration();
    }
    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}
void markCompilerRoots() {
    // Fortunately, the compiler doesn't have too many values that it hangs on to. The only object
    // it uses is the ObjFunction it is compiling into. Since function declarations can nest, the
    // compiler has a linked list of those and we walk the whole list.
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}