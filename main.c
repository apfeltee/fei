

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>


// the hash table can only be 75% full
#define TABLE_MAX_LOAD 0.75

// growth factor for garbage collection heap
#define GC_HEAP_GROW_FACTOR 2

// macro to avoid redundantly cast void* back to desired type
#define ALLOCATE_OBJ(type, objectType) (type*)mem_allocobject(vm, sizeof(type), objectType)

// local variables array in compiler, max of 1 byte is 255
#define UINT8_COUNT (UINT8_MAX + 1)

// track the compiler
//#define DEBUG_PRINT_CODE

// execution tracing of the VM
//#define DEBUG_TRACE_EXECUTION

// diagnostic tools for garbage collector
// 'stress' mode; if this is on, GC runs as often as it possibly can
//#define DEBUG_STRESS_GC
//#define DEBUG_LOG_GC


// max frames is fixed
#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// type comparisons
#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NULL(value) ((value).type == VAL_NULL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)


// from VALUE STRUCT to RAW  C nicely used in printing
// also for comparisons -> use values ALREADY CONVERTED to Value struct union to raw C
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)


// macros for conversions from code type to struct Value union type
// pass Value struct to the macro
/*	IMPORTANT: macro syntax
#define macroname(parameter) (returntype)
-> here, return a Value type. initializing it inside the macro
-> . means as
IMPORTANT = these macros give a 'tag' to each respective values
*/
#define BOOL_VAL(value) ((Value){ VAL_BOOL, { .boolean = value } })
#define NULL_VAL ((Value){ VAL_NULL, { .number = 0 } })
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, { .number = value } })
#define OBJ_VAL(object) ((Value){ VAL_OBJ, { .obj = (Obj*)object } })// pass in as a pointer to the object, receives the actual object


#define OBJ_TYPE(value) (AS_OBJ(value)->type)// extracts the tag

// macros for checking(bool) whether an object is a certain type
#define IS_BOUND_METHOD(value) obj_istype(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value) obj_istype(value, OBJ_CLASS)
#define IS_FUNCTION(value) obj_istype(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) obj_istype(value, OBJ_INSTANCE)
#define IS_NATIVE(value) obj_istype(value, OBJ_NATIVE)
#define IS_STRING(value) obj_istype(value, OBJ_STRING)// takes in raw Value, not raw Obj*
#define IS_CLOSURE(value) obj_istype(value, OBJ_CLOSURE)

// macros to tell that it is safe when creating a tag, by returning the requested type
// take a Value that is expected to conatin a pointer to the heap, first returns pointer second the charray itself
// used to cast as an ObjType pointer, from a Value type
#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJ(value))
#define AS_CLASS(value) ((ObjClass*)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance*)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)// get chars(char*) from ObjString pointer
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)


// macro to allocate memory, usedin obj/heap
// use mem_realloc as malloc here; start from null pointer, old size is 0, and new size is count
#define ALLOCATE(vm, type, count) (type*)mem_realloc(vm, NULL, 0, sizeof(type) * (count))


// free memory, pass in new size as 0 to free
#define FREE(vm, type, pointer) mem_realloc(vm, pointer, sizeof(type), 0)


// C macros
// calculates a new capacity based on a given current capacity, it should SCALE based on the old one
// this one grows by * 2
#define GROW_CAPACITY(vm, capacity) ((capacity) < 8 ? 8 : (capacity)*2)// capacity becomes 8 for the first time(starts from 0), later it multiplies by 2

// macro to grow array
// make own mem_realloc function
// basically declare our return type here with (type*)
#define GROW_ARRAY(vm, type, pointer, oldCount, newCount) (type*)mem_realloc(vm, pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

// no (type*) because function does not return a type
// 0 is the new capacity
// used to free eg. char arrays
#define FREE_ARRAY(vm, type, pointer, oldCount) mem_realloc(vm, pointer, sizeof(type) * (oldCount), 0)

    /* info on the macros below
Below macros are FUNCTIONSt that take ZERO arguments, and what is inside () is their return value
vmc_readbyte:	
	macro to ACCESS the BYTE(uin8_t) from the POINTER(ip), and increment it
	reads byte currently pointed at ip, then advances the instruction pointer
vmc_readconst:
	return constants.values element, from vmc_readbyte(), which points exactly to the NEXT index
READ STRING:
	return as object string, read directly from the globalvm(oip)
*/

#define vmc_readbyte() \
    (*frame->ip++)

#define vmc_readconst() \
    (frame->closure->function->chunk.constants.values[vmc_readbyte()])

#define vmc_readstring() \
    AS_STRING(vmc_readconst())

// for patch jumps
// yanks next two bytes from the chunk(used to calculate the offset earlier) and return a 16-bit integer out of it
// use bitwise OR
#define vmc_readshort() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

// MACRO for binary operations
// take two last constants, and vm_push ONE final value doing the operations on both of them
// this macro needs to expand to a series of statements, read a-virtual-machine for more info, this is a macro trick or a SCOPE BLOCK
// pass in an OPERAOTR as a MACRO
// valueType is a Value struct
// first check that both operands are numbers
#define vmc_binaryop(valueType, op, downcastType)           \
    do                                                   \
    {                                                    \
        if(!IS_NUMBER(vm_peek(vm, 0)) || !IS_NUMBER(vm_peek(vm, 1)))   \
        {                                                \
            vm_rterror(vm, "Operands must be numbers.");   \
            return INTERPRET_RUNTIME_ERROR;              \
        }                                                \
        downcastType b = (downcastType)AS_NUMBER(vm_pop(vm)); \
        downcastType a = (downcastType)AS_NUMBER(vm_pop(vm)); \
        vm_push(vm, valueType(a op b));                         \
    } while(false)


// for precedence in unary operations
// ordered from lowest precedence to highest precedence
enum Precedence
{
    PREC_NONE,
    PREC_ASSIGNMENT,// =
    PREC_OR,// or
    PREC_AND,// and
    PREC_EQUALITY,// == !=
    PREC_COMPARISON,// > >= < <=
    PREC_TERM,// + -
    PREC_FACTOR,// * /
    PREC_UNARY,// ! -
    PREC_CALL,// . ()
    PREC_PRIMARY
};

enum FunctionType
{
    TYPE_FUNCTION,
    TYPE_SCRIPT,// top level main()
    TYPE_INITIALIZER,// class constructors
    TYPE_METHOD,// class methods
};

// storing constants/literals
// separate "constant data" region
/*	IMPORTANT
as in C, everything is just bytes. 2 things to be aware of:
1. How to represent the TYPE of value?
2. How to store the VALUE itself?

EVERYTHING IN BITS

we use a TAGGED UNION, a value containg a TYPE TAG, and the PAYLOd / ACTUAL VALUE
*/


// type tags for the tagged union
enum ValueType
{
    VAL_BOOL,
    VAL_NULL,
    VAL_NUMBER,
    VAL_OBJ,// for bigger instances such as strings, functions, heap-allocated; the payload is a heap pointer
};
// in bytecode format, each instruction has a one-byte operation code(opcode)
// the number controls what kind of instruction we're dealing with- add, subtract, etc
// typedef enums are bytes apparently
// these are INSTRUCTIONS

enum OpCode
{
    OP_CONSTANT,// chunk needs to know when to produce constants and print them in the right order
    // they have operands, to eg. identify which variable to load
    // OP_CONSTANT take up 2 bytes, one is the opcode itself and the other the constant index

    OP_NULL,
    OP_TRUE,
    OP_FALSE,

    // unary operators
    OP_NEGATE,// operand to negate, utilized in virtual machine

    // literals/declarations
    OP_POP,// basically pops a value off the stack and forgets it, used for expression statements
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,

    // binary operators
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MODULO,

    // logical, unary
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,

    OP_SWITCH_EQUAL,
    OP_CLOSE_UPVALUE,

    OP_JUMP,
    OP_JUMP_IF_FALSE,// takes a 16-bit operand
    OP_CALL,

    OP_LOOP,
    OP_LOOP_IF_FALSE,// repeat until
    OP_LOOP_IF_TRUE,// do while

    OP_CLOSURE,
    OP_CLASS,
    OP_METHOD,
    OP_INVOKE,

    OP_INHERIT,// class inheritance
    OP_GET_SUPER,// for superclasses
    OP_SUPER_INVOKE,

    OP_RETURN,// means return from current function
};// basically a typdef vm_call to an enum
// in C, you cannot have enums called simply by their rvalue 'string' names, use typdef to define them


enum ObjType
{
    OBJ_BOUND_METHOD,
    OBJ_INSTANCE,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE
};

// rseult that responds from the running VM
enum InterpretResult
{
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
};

enum TokenType
{
    // single character
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,// ( )
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,// { }
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    TOKEN_MODULO,

    // one or two compare operators
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,// !, !=
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,

    // literals
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,

    // keywords
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELF,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NULL,
    TOKEN_OR,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_SWITCH,
    TOKEN_DEFAULT,
    TOKEN_CASE,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_THEN,

    // do while, repeat until
    TOKEN_DO,
    TOKEN_REPEAT,
    TOKEN_UNTIL,

    // class inheritance
    TOKEN_FROM,

    TOKEN_ERROR,
    TOKEN_EOF
};


typedef enum Precedence Precedence;
typedef enum FunctionType FunctionType;
typedef enum TokenType TokenType;
typedef enum InterpretResult InterpretResult;
typedef enum ObjType ObjType;
typedef enum OpCode OpCode;
typedef enum ValueType ValueType;
typedef struct ParseRule ParseRule;
typedef struct Local Local;
typedef struct Upvalue Upvalue;
typedef struct ClassCompiler ClassCompiler;
typedef struct Compiler Compiler;
typedef struct Parser Parser;
typedef struct Scanner Scanner;
typedef struct Obj Obj;// basically giving struct Obj the name Struct
typedef struct ObjString ObjString;
typedef struct Token Token;
typedef struct CallFrame CallFrame;
typedef struct ObjBoundMethod ObjBoundMethod;
typedef struct ObjInstance ObjInstance;
typedef struct ObjClass ObjClass;
typedef struct ObjNative ObjNative;
typedef struct ObjClosure ObjClosure;
typedef struct ObjUpvalue ObjUpvalue;// define ObjUpvalue here to use them inside the struct
typedef struct ObjFunction ObjFunction;
typedef struct Table Table;
typedef struct Entry Entry;
typedef struct Chunk Chunk;
typedef struct ValueArray ValueArray;
typedef struct Value Value;
typedef struct VM VM;

typedef Value (*NativeFn)(VM*, int, Value*);

// simple typdef function type with no arguments and returns nothing
// acts like a "virtual" function , a void function that cam be overidden; actually a void but override it with ParseFn
typedef void (*ParseFn)(VM* vm, bool canAssign);

/* IMPORTANT 
-> use C unions to OVERLAP in memory for the STRUCT
->  size of the union is its LARGEST FIELD
-> unions are like structs but they only allocate memory for the LARGEST FIELD
*/
struct Value
{
    ValueType type;
    union// the union itself, implemented here
    {
        bool boolean;
        double number;
        Obj* obj;// pointer to the heap, the payload for bigger types of data
    } as;// can use . to represent this union
};

// the constant pool is array of values

struct ValueArray
{
    int capacity;
    int count;
    Value* values;
};


/* dynamic array for bytecode */
// btyecode is a series of instructions, this is a struct to hold instructions
// create own dynamic array

struct Chunk
{
    int count;// current size
    int capacity;// max array size
    uint8_t* code;// 1 byte unsigned int, to store the CODESTREAM
    int* lines;// array of integers that parallels the bytecode/codestream, to get where each location of the bytecode is
    ValueArray constants;// store double value literals
};


struct Entry
{
    ObjString* key;// use ObjString pointer as key
    Value value;// the value/data type
};

// the table, an array of entries

struct Table
{
    int count;
    int capacity;
    Entry* entries;
};


struct Obj// as no typedef is used, 'struct' itself will always havae to be typed
{
    ObjType type;
    struct Obj* next;// linked list or intrusive list, to avoid memory leaks, obj itself as a node
    // traverse the list to find every object that has been allocated on the heap

    // for mark-sweep garbage collection
    bool isMarked;
};

// for functions and calls

struct ObjFunction
{
    Obj obj;
    int arity;// store number of parameters
    int upvalueCount;// to track upValues
    Chunk chunk;// to store the function information
    ObjString* name;
};


struct ObjUpvalue
{
    Obj obj;
    Value* location;// pointer to value in the enclosing ObjClosure

    Value closed;// to store closed upvalue

    // intrusive/linked list to track sorted openvalues
    // ordered by the stack slot they point to
    struct ObjUpvalue* next;
};

// for closures

struct ObjClosure
{
    // points to an ObjFunction and Obj header
    Obj obj;// Obj header
    ObjFunction* function;

    // for upvalues
    ObjUpvalue** upvalues;// array of upvalue pointers
    int upvalueCount;
};


/*  NATIVE FUNCTIONS(file systems, user input etc.)
-> native functions reference a vm_call to native C code insted of bytecode */

struct ObjNative
{
    Obj obj;
    NativeFn function;
};


struct ObjString// using struct inheritance
{
    Obj obj;
    int length;
    char* chars;
    uint32_t hash;// for hash table, for cache(temporary storage area); each ObjString has a hash code for itself
};


// class object type

struct ObjClass
{
    Obj obj;
    ObjString* name;// not needed for uer's program, but helps the dev in debugging
    Table methods;// hash table for storing methods
};


struct ObjInstance
{
    Obj obj;// inherits from object, the "object" tag
    ObjClass* kelas;// pointer to class types
    Table fields;// use a hash table to store fields
};


// struct for class methods
struct ObjBoundMethod
{
    Obj obj;
    Value receiver;// wraps receiver and function/method/closure together, receiver is the ObjInstance / lcass type
    ObjClosure* method;
};


// the vm_call stack
// keep track where on the stack a function's local begin, where the caller should resume, etc.
// a vm_call frame represents a single ongoing function vm_call
// each time a function is called, create this struct
struct CallFrame
{
    ObjClosure* closure;
    uint8_t* ip;// store ip on where in the VM the function is
    Value* slots;// this points into the VM's value stack at the first slot the function can use
};
struct VM
{
    // since the whole program is one big 'main()' use callstacks
    CallFrame frames[FRAMES_MAX];
    int frameCount;// stores current height of the stack

    Value stack[STACK_MAX];// stack array is 'indirectly' declared inline here
    Value* stackTop;// pointer to the element just PAST the element containing the top value of the stack

    Table globals;// for storing global variables
    Table strings;// for string interning, to make sure every equal string takes one memory

    ObjString* initString;// init string for class constructors

    ObjUpvalue* openUpvalues;// track all upvalues; points to the first node of the linked list

    Obj* objects;// pointer to the header of the Obj itself/node, start of the list
    // nicely used in GARBAGE COLLECTION, where objects are nicely erased in the middle

    // stack to store gray marked Objects for garbage collection
    int grayCapacity;
    int grayCount;
    Obj** grayStack;// array of pointers pointing to a particular subgraph

    // self-adjusting-g-heap, to control frequency of GC, bytesAllocated is the running total
    size_t bytesAllocated;// size_t is a 32 bit(integer/4bytes), represents size of an object in bytes
    size_t nextGC;// threhsold that triggers the GC
};


struct Token
{
    TokenType type;// identifier to type of token, eg. number, + operator, identifier
    const char* start;
    int length;
    int line;
};

// scanner to run through the source code
struct Scanner
{
    const char* start;// marks the beginning of the current lexeme('word', you can say_
    const char* current;// points to the character being looked at
    int line;// int to tell the current line being looked at
};

// to store current and previous tokens
struct Parser
{
    Token current;
    Token previous;
    bool hadError;// flag to tell whether the code has a syntax error or no
    bool panicMode;// flag for error cascades/multiple errors so the parser does not get confused, only returns the first
};

/*	parse rule, what is needed:
-> a function to compile a PREFIX expression starting with token of that type
-> a function to cimpile an INFIX expression whose left operand is followed by a token of that type
-> precedence of an infix expression with the tokenas an operator
*/
struct ParseRule
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
};

struct Local
{
    Token name;
    int depth;// depth of the variable, corresponding to scoreDepth in the struct below
    bool isCaptured;// track whether the local is captured by a closure or no
};


struct Upvalue
{
    bool isLocal;
    int index;// matches the index of the local variable in ObjClosure
};

// stack for local variables
struct Compiler
{
    Compiler* enclosing;// pointer to the 'outer'/enclosing compiler, to return to after function

    // wrapping the whole program into one big main() function
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];// array to store locals, ordered in the order of declarations
    int localCount;// tracks amount of locals in a scope
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;// number of scopes/blocks surrounding the code

    // for loop breaks and continues, loop enclosing
    int loopCountTop;
    int* continueJumps;
    int continueJumpCapacity;// only for continue jumpbs

    // for patching all break statements
    int breakPatchJumps[UINT8_COUNT][UINT8_COUNT];
    int breakJumpCounts[UINT8_COUNT];

};

// for 'this' tokens, a main() for class
struct ClassCompiler
{
    ClassCompiler* enclosing;
    Token name;
    bool hasSuperclass;// to end scope in superclass declaration
};

Scanner g_scanner;
Parser parser;
ClassCompiler* currentClass = NULL;
Compiler* current = NULL;

/* main.c */

/* main.c */
void repl(VM *vm);
char *readFile(VM *vm, const char *path);
void htable_init(VM *vm, Table *table);
void htable_free(VM *vm, Table *table);
Entry *htable_findentry(Entry *entries, int capacity, ObjString *key);
_Bool htable_get(VM *vm, Table *table, ObjString *key, Value *value);
void htable_adjustcap(VM *vm, Table *table, int capacity);
_Bool htable_set(VM *vm, Table *table, ObjString *key, Value value);
_Bool htable_delete(VM *vm, Table *table, ObjString *key);
void htable_addall(VM *vm, Table *from, Table *to);
ObjString *htable_findstring(VM *vm, Table *table, const char *chars, int length, uint32_t hash);
void htable_removewhite(VM *vm, Table *table);
void htable_mark(VM *vm, Table *table);
void chunk_init(VM *vm, Chunk *chunk);
void chunk_write(VM *vm, Chunk *chunk, uint8_t byte, int line);
void chunk_free(VM *vm, Chunk *chunk);
int chunk_addconstant(VM *vm, Chunk *chunk, Value value);
void scanner_init(const char *source);
_Bool scanner_isalpha(char c);
_Bool scanner_isdigit(char c);
_Bool scanner_isatend(void);
char scanner_advance(void);
_Bool scanner_match(char expected);
Token scanner_maketoken(TokenType type);
Token scanner_errortoken(const char *message);
char scanner_peek(void);
char scanner_peeknext(void);
void scanner_skipspace(void);
TokenType scanner_checkkeyword(int start, int length, const char *rest, TokenType type);
TokenType scanner_parseidenttype(void);
Token scanner_parseident(void);
Token scanner_parsenumber(void);
Token scanner_parsestring(void);
Token scanner_scantoken(void);
void prs_parseprecedence(VM *vm, Precedence precedence);
ParseRule *prs_getrule(VM *vm, TokenType type);
void prs_expression(VM *vm);
void prs_block(VM *vm);
void prs_function(VM *vm, FunctionType type);
void prs_method(VM *vm);
void prs_classdecl(VM *vm);
void prs_funcdecl(VM *vm);
void prs_vardecl(VM *vm);
void prs_exprstmt(VM *vm);
void prs_ifstmt(VM *vm);
void prs_switchstmt(VM *vm);
void prs_returnstmt(VM *vm);
void prs_forstmt(VM *vm);
void prs_whilestmt(VM *vm);
void prs_breakstmt(VM *vm);
void prs_continuestmt(VM *vm);
void prs_repeatuntilstmt(VM *vm);
void prs_dowhilestmt(VM *vm);
void prs_synchronize(VM *vm);
void prs_declaration(VM *vm);
void prs_statement(VM *vm);
ObjFunction *prs_compile(VM *vm, const char *source);
void prs_markroots(VM *vm);
Chunk *prs_currentchunk(VM *vm);
void prs_errorat(VM *vm, Token *token, const char *message);
void prs_error(VM *vm, const char *message);
void prs_erroratcurrent(VM *vm, const char *message);
void prs_advance(VM *vm);
void prs_advancewhileskipping(VM *vm, TokenType type);
void prs_consume(VM *vm, TokenType type, const char *message);
_Bool prs_check(VM *vm, TokenType type);
_Bool prs_matchtoken(VM *vm, TokenType type);
void prs_emitbyte(VM *vm, uint8_t byte);
void prs_emitbytes(VM *vm, uint8_t byte1, uint8_t byte2);
void prs_emitloop(VM *vm, int loopStart);
void prs_emitcondloop(VM *vm, int loopStart, _Bool state);
int prs_emitjump(VM *vm, uint8_t instruction);
void prs_emitreturn(VM *vm);
uint8_t prs_makeconst(VM *vm, Value value);
void prs_emitconst(VM *vm, Value value);
void prs_patchjump(VM *vm, int offset);
void prs_initcompiler(VM *vm, Compiler *compiler, FunctionType type);
ObjFunction *prs_endcompiler(VM *vm);
void prs_beginscope(VM *vm);
void prs_endscope(VM *vm);
void prs_beginloopscope(VM *vm);
void prs_endloopscope(VM *vm);
void prs_markcontjump(VM *vm);
void prs_patchbreakjumps(VM *vm);
uint8_t prs_makeconstident(VM *vm, Token *name);
_Bool prs_identequal(VM *vm, Token *a, Token *b);
int prs_resolvelocal(VM *vm, Compiler *compiler, Token *name);
int prs_addupvalue(VM *vm, Compiler *compiler, uint8_t index, _Bool isLocal);
int prs_resolveupvalue(VM *vm, Compiler *compiler, Token *name);
void prs_addlocal(VM *vm, Token name);
void prs_declarevariable(VM *vm);
uint8_t prs_parsevariable(VM *vm, const char *errorMessage);
void prs_markinitialized(VM *vm);
void prs_definevariable(VM *vm, uint8_t global);
uint8_t prs_parsearglist(VM *vm);
void rule_and(VM *vm, _Bool canAssign);
void rule_binary(VM *vm, _Bool canAssign);
void rule_parsecall(VM *vm, _Bool canAssign);
void rule_dot(VM *vm, _Bool canAssign);
void rule_literal(VM *vm, _Bool canAssign);
void rule_grouping(VM *vm, _Bool canAssign);
void rule_number(VM *vm, _Bool canAssign);
void rule_or(VM *vm, _Bool canAssign);
void rule_string(VM *vm, _Bool canAssign);
void rule_namedvar(VM *vm, Token name, _Bool canAssign);
void rule_variable(VM *vm, _Bool canAssign);
Token prs_makesyntoken(VM *vm, const char *text);
void rule_super(VM *vm, _Bool canAssign);
void rule_this(VM *vm, _Bool canAssign);
void rule_unary(VM *vm, _Bool canAssign);
void runFile(VM *vm, const char *path);
int main(int argc, const char *argv[]);
int dbg_print_simpleinst(VM *vm, const char *name, int offset);
int dbg_print_byteinst(VM *vm, const char *name, Chunk *chunk, int offset);
int dbg_print_constinst(VM *vm, const char *name, Chunk *chunk, int offset);
int dbg_print_invokeinst(VM *vm, const char *name, Chunk *chunk, int offset);
int dbg_print_jumpinst(VM *vm, const char *name, int sign, Chunk *chunk, int offset);
void dbg_disasmchunk(VM *vm, Chunk *chunk, const char *name);
int dbg_disasminst(VM *vm, Chunk *chunk, int offset);
void *mem_realloc(VM *vm, void *pointer, size_t oldSize, size_t newSize);
void mem_freeobject(VM *vm, Obj *object);
void mem_markobject(VM *vm, Obj *object);
void mem_markvalue(VM *vm, Value value);
void mem_markarray(VM *vm, ValueArray *array);
void mem_markroots(VM *vm);
void mem_blackenobject(VM *vm, Obj *object);
void mem_tracerefs(VM *vm);
void mem_sweep(VM *vm);
void mem_collectgarbage(VM *vm);
void mem_freeobjlist(VM *vm);
Obj *mem_allocobject(VM *vm, size_t size, ObjType type);
_Bool obj_istype(Value value, ObjType type);
ObjBoundMethod *obj_mkboundmethod(VM *vm, Value receiver, ObjClosure *method);
ObjClosure *obj_mkclosure(VM *vm, ObjFunction *function);
ObjString *obj_mkstring(VM *vm, char *chars, int length, uint32_t hash);
ObjClass *obj_mkclass(VM *vm, ObjString *name);
ObjInstance *obj_mkinstance(VM *vm, ObjClass *kelas);
ObjFunction *obj_mkfunction(VM *vm);
ObjNative *obj_mknative(VM *vm, NativeFn function);
uint32_t obj_hashstring(VM *vm, const char *key, int length);
ObjString *obj_takestring(VM *vm, char *chars, int length);
ObjString *obj_copystring(VM *vm, const char *chars, int length);
ObjUpvalue *obj_mkupvalue(VM *vm, Value *slot);
void obj_printfunction(VM *vm, ObjFunction *function);
void obj_printobject(VM *vm, Value value);
void obj_initvalarray(VM *vm, ValueArray *array);
void obj_writevalarray(VM *vm, ValueArray *array, Value value);
void obj_freevalarray(VM *vm, ValueArray *array);
void obj_printvalue(VM *vm, Value value);
_Bool obj_valequal(VM *vm, Value a, Value b);
void vm_resetstack(VM *vm);
void vm_rterror(VM *vm, const char *format, ...);
void vm_defnative(VM *vm, const char *name, NativeFn function);
Value cfn_clock(VM *vm, int argCount, Value *args);
Value cfn_print(VM *vm, int argCount, Value *args);
Value cfn_println(VM *vm, int argc, Value *args);
Value cfn_chr(VM *vm, int argc, Value *args);
void vm_init(VM *vm);
void vm_free(VM *vm);
void vm_push(VM *vm, Value value);
Value vm_pop(VM *vm);
Value vm_peek(VM *vm, int distance);
_Bool vm_call(VM *vm, ObjClosure *closure, int argCount);
_Bool vm_callvalue(VM *vm, Value callee, int argCount);
_Bool vm_invokefromclass(VM *vm, ObjClass *kelas, ObjString *name, int argCount);
_Bool vm_invoke(VM *vm, ObjString *name, int argCount);
_Bool vm_bindmethod(VM *vm, ObjClass *kelas, ObjString *name);
ObjUpvalue *vm_captureupvalue(VM *vm, Value *local);
void vm_closeupvalues(VM *vm, Value *last);
void vm_defmethod(VM *vm, ObjString *name);
_Bool vm_isfalsey(VM *vm, Value value);
void vm_concatenate(VM *vm);
InterpretResult vm_interpret(VM *vm, const char *source);
InterpretResult vm_run(VM *vm);


/* the array of ParseRules 
uses C99 DESIGNATED INITIALIZER syntax
use {struct members} to initialize a struct
[index number] = {struct members}, the index number can be seen clearly
token enums from scanner is reused
*/
static ParseRule rules[] =
{
    // function calls are like infixes, with high precedence on the left, ( in the middle for arguments, then ) at the end
    [TOKEN_LEFT_PAREN] = { rule_grouping, rule_parsecall, PREC_CALL },// vm_call for functions
    [TOKEN_RIGHT_PAREN] = { NULL, NULL, PREC_NONE },
    [TOKEN_LEFT_BRACE] = { NULL, NULL, PREC_NONE },
    [TOKEN_RIGHT_BRACE] = { NULL, NULL, PREC_NONE },
    [TOKEN_COMMA] = { NULL, NULL, PREC_NONE },
    [TOKEN_DOT] = { NULL, rule_dot, PREC_CALL },
    [TOKEN_MINUS] = { rule_unary, rule_binary, PREC_TERM },
    [TOKEN_PLUS] = { NULL, rule_binary, PREC_TERM },
    [TOKEN_SEMICOLON] = { NULL, NULL, PREC_NONE },
    [TOKEN_SLASH] = { NULL, rule_binary, PREC_FACTOR },
    [TOKEN_STAR] = { NULL, rule_binary, PREC_FACTOR },
    [TOKEN_MODULO] = { NULL, rule_binary, PREC_FACTOR },
    [TOKEN_BANG] = { rule_unary, NULL, PREC_NONE },
    [TOKEN_BANG_EQUAL] = { NULL, rule_binary, PREC_EQUALITY },// equality precedence
    [TOKEN_EQUAL] = { NULL, rule_binary, PREC_COMPARISON },// comaprison precedence
    [TOKEN_EQUAL_EQUAL] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_GREATER] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_GREATER_EQUAL] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_LESS] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_LESS_EQUAL] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_IDENTIFIER] = { rule_variable, NULL, PREC_NONE },
    [TOKEN_STRING] = { rule_string, NULL, PREC_NONE },
    [TOKEN_NUMBER] = { rule_number, NULL, PREC_NONE },
    [TOKEN_AND] = { NULL, rule_and, PREC_AND },
    [TOKEN_CLASS] = { NULL, NULL, PREC_NONE },
    [TOKEN_ELSE] = { NULL, NULL, PREC_NONE },
    [TOKEN_FALSE] = { rule_literal, NULL, PREC_NONE },
    [TOKEN_FOR] = { NULL, NULL, PREC_NONE },
    [TOKEN_FUN] = { NULL, NULL, PREC_NONE },
    [TOKEN_IF] = { NULL, NULL, PREC_NONE },
    [TOKEN_SWITCH] = { NULL, NULL, PREC_NONE },
    [TOKEN_NULL] = { rule_literal, NULL, PREC_NONE },
    [TOKEN_OR] = { NULL, rule_or, PREC_OR },
    [TOKEN_RETURN] = { NULL, NULL, PREC_NONE },
    [TOKEN_SUPER] = { rule_super, NULL, PREC_NONE },
    [TOKEN_THIS] = { rule_this, NULL, PREC_NONE },
    [TOKEN_TRUE] = { rule_literal, NULL, PREC_NONE },
    [TOKEN_VAR] = { NULL, NULL, PREC_NONE },
    [TOKEN_WHILE] = { NULL, NULL, PREC_NONE },
    [TOKEN_ERROR] = { NULL, NULL, PREC_NONE },
    [TOKEN_EOF] = { NULL, NULL, PREC_NONE },
};


// for REPL, the print eval read loop
void repl(VM* vm)
{
    char line[1024];// char array to hold everything, with a length limit
    for(;;)
    {
        printf(">> ");

        /* info on fgets
		- fgets is like cin or getline cin 
		- basically get a line everytime
		- parameters: fgets(char array, char size, filestream). In this case the file stream is stdin, the current keyboard or from the standard input
		- char array is a pointer to where the string will be copied
		- use if so that if line overloads, we go to the next line
		*/
        if(!fgets(line, sizeof(line), stdin))
        {
            printf("\n");
            break;
        }

        vm_interpret(vm, line);
    }
}

// get raw source code from file
char* readFile(VM* vm, const char* path)
{
    (void)vm;
    /*	Reading files in C
	FILE* file = fopen(const char *file_name, const char *mode_of_operation
	r(read) = searches file, and sets up a pointer to the first character. If not found returns null
	w(write)
	a(read, set to last pointer)
	rb(special read to open non-text files, a binary file)
	*/
    FILE* file = fopen(path, "rb");
    if(file == NULL)// if file does not exist or user does not have access
    {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    // fseek - move file pointer to a specific position, offset is number of byte to offset form POSITION(last parameter)
    // int fseek(file pointer, long int offset, int position)
    // SEEK_END, SEEK_SET(start), SEEK_CUR
    // basically set pointer to end of file
    // 0l = 0 long
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);// ftell is used to find position of file pointer, used to denote size
    // top two lines used to get file size
    rewind(file);// sets file pointer to the beginning of the file

    char* buffer = (char*)malloc(fileSize + 1);// allocate a char*(string) to the size of the file
    if(buffer == NULL)
    {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }

    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);// read the file
    /* notes on fread in C
	size_t fread(void * buffer, size_t size, size_t count, FILE * stream)
	buffer =  a pointer to the block of memoery with a least (size*count) byte
	size = size of the result type
	count = number of elements/ file size
	*/

    if(bytesRead < fileSize)// if read size less than file size
    {
        fprintf(stderr, "Could not read \"%s\".\n", path);
        exit(74);
    }

    buffer[bytesRead] = '\0';// mark the last character as '\0', the end of file symbol

    fclose(file);
    return buffer;
}

// A void pointer is a pointer that has no associated data type with it.
// A void pointer can hold address of any type and can be typcasted to any type.
void* mem_realloc(VM* vm, void* pointer, size_t oldSize, size_t newSize)
{
    vm->bytesAllocated += newSize - oldSize;// self adjusting heap for garbage collection

    if(newSize > oldSize)// when allocating NEW memory, not when freeing as collecGarbage will cal void* mem_realloc itself
    {
#ifdef DEBUG_STRESS_GC
        mem_collectgarbage(vm);
#endif

        // run collecter if bytesAllocated is above threshold
        if(vm->bytesAllocated > vm->nextGC)
        {
            mem_collectgarbage(vm);
        }
    }

    if(newSize == 0)
    {
        free(pointer);
        return NULL;
    }

    // C realloc
    void* result = realloc(pointer, newSize);

    // if there is not enought memory, realloc will return null
    if(result == NULL)
        exit(1);// exit with code 1

    return result;
}


Obj* mem_allocobject(VM* vm, size_t size, ObjType type)
{
    Obj* object = (Obj*)mem_realloc(vm, NULL, 0, size);// allocate memory for obj
    object->type = type;
    object->isMarked = false;

    // every time an object is allocated, insert to the list
    // insert as the HEAD; the latest one inserted will be at the start
    object->next = vm->objects;// globalvm from virtualm.h, with extern
    vm->objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zd for %d\n", (void*)object, size, type);// %ld prints LONG INT
    // (void*) for 'native pointer type'
#endif
    return object;
}


// you can pass in a'lower' struct pointer, in this case Obj*, and get the higher level which is ObjFunction
void mem_freeobject(VM* vm, Obj* object)// to handle different types
{
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch(object->type)
    {
        case OBJ_BOUND_METHOD:
            FREE(vm, ObjBoundMethod, object);
            break;

        case OBJ_CLASS:
        {
            // free class type
            ObjClass* kelas = (ObjClass*)object;
            htable_free(vm, &kelas->methods);
            FREE(vm, ObjClass, object);
            break;
        }
        case OBJ_INSTANCE:
        {
            ObjInstance* instance = (ObjInstance*)object;
            htable_free(vm, &instance->fields);
            FREE(vm, ObjInstance, object);
            break;
        }
        case OBJ_CLOSURE:
        {
            // free upvalues
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(vm, ObjUpvalue*, closure->upvalues, closure->upvalueCount);

            FREE(vm, ObjClosure, object);// only free the closure, not the function itself
            break;
        }
        case OBJ_FUNCTION:// return bits(chunk) borrowed to the operating syste,
        {
            ObjFunction* function = (ObjFunction*)object;
            chunk_free(vm, &function->chunk);
            FREE(vm, ObjFunction, object);
            break;
        }
        case OBJ_NATIVE:
        {
            FREE(vm, ObjNative, object);
            break;
        }
        case OBJ_STRING:
        {
            ObjString* string = (ObjString*)object;
            FREE_ARRAY(vm, char, string->chars, string->length + 1);
            FREE(vm, ObjString, object);
            break;
        }
        case OBJ_UPVALUE:
        {
            FREE(vm, ObjUpvalue, object);
            break;
        }
    }
}

/*		garbage collection		 */

void mem_markobject(VM* vm, Obj* object)
{
    if(object == NULL)
        return;// in some places the pointer is empty
    if(object->isMarked)
        return;// object is already marked

    object->isMarked = true;

    // create a worklist of grayobjects to traverse later, use a stack to implement it
    if(vm->grayCapacity < vm->grayCount + 1)// if need more space, allocate
    {
        vm->grayCapacity = GROW_CAPACITY(vm, vm->grayCapacity);
        vm->grayStack = realloc(vm->grayStack, sizeof(Obj*) * vm->grayCapacity);// use native realloc here
    }

    if(vm->grayStack == NULL)
        exit(1);// if fail to allocate memory for the gray stack

    // add the 'gray' object to the working list
    vm->grayStack[vm->grayCount++] = object;

#ifdef DEBUG_LOG_GC
    printf("%p marked ", (void*)object);
    obj_printvalue(vm, OBJ_VAL(object));// you cant print first class objects, like how you would print in the actual repl
    printf("\n");
#endif
}

void mem_markvalue(VM* vm, Value value)
{
    if(!IS_OBJ(value))
        return;// if value is not first class Objtype return
    mem_markobject(vm, AS_OBJ(value));
}


// marking array of values/constants of a function, used in mem_blackenobject, case OBJ_FUNCTION
void mem_markarray(VM* vm, ValueArray* array)
{
    for(int i = 0; i < array->count; i++)
    {
        mem_markvalue(vm, array->values[i]);// mark each Value in the array
    }
}


void mem_markroots(VM* vm)
{
    // assiging a pointer to a full array means assigning the pointer to the FIRST element of that array
    for(Value* slot = vm->stack; slot < vm->stackTop; slot++)// walk through all values/slots in the Value* array
    {
        mem_markvalue(vm, *slot);
    }

    // mark closures
    for(int i = 0; i < vm->frameCount; i++)
    {
        mem_markobject(vm, (Obj*)vm->frames[i].closure);// mark ObjClosure  type
    }

    // mark upvalues, walk through the linked list of upvalues
    for(ObjUpvalue* upvalue = vm->openUpvalues; upvalue != NULL; upvalue = upvalue->next)
    {
        mem_markobject(vm, (Obj*)upvalue);
    }


    htable_mark(vm, &vm->globals);// mark global variables, belongs in the VM/hashtable

    // compiler also grabs memory; special function only for 'backend' processes
    prs_markroots(vm);// declared in compiler.h

    mem_markobject(vm, (Obj*)vm->initString);// mark objstring for init
}


// actual tracing of each gray object and marking it black
void mem_blackenobject(VM* vm, Obj* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p blackened ", (void*)object);
    obj_printvalue(vm, OBJ_VAL(object));
    printf("\n");
#endif


    switch(object->type)
    {
        case OBJ_BOUND_METHOD:
        {
            ObjBoundMethod* bound = (ObjBoundMethod*)object;
            mem_markvalue(vm, bound->receiver);
            mem_markobject(vm, (Obj*)bound->method);
            break;
        }

        case OBJ_UPVALUE:// simply mark the closed value
            mem_markvalue(vm, ((ObjUpvalue*)object)->closed);
            break;

        case OBJ_FUNCTION:// mark the name and its value array of constants
        {
            // you can get the coressponding 'higher' object type from a lower derivation struct in C using (higher*)lower
            ObjFunction* function = (ObjFunction*)object;
            mem_markobject(vm, (Obj*)function->name);// mark its name, an ObjString type
            mem_markarray(vm, &function->chunk.constants);// mark value array of chunk constants, pass it in AS A POINTER using &
            break;
        }

        case OBJ_CLOSURE:// mark the function and all of the closure's upvalues
        {
            ObjClosure* closure = (ObjClosure*)object;
            mem_markobject(vm, (Obj*)closure->function);
            for(int i = 0; i < closure->upvalueCount; i++)
            {
                mem_markobject(vm, (Obj*)closure->upvalues[i]);
            }
            break;
        }

        case OBJ_CLASS:
        {
            ObjClass* kelas = (ObjClass*)object;
            mem_markobject(vm, (Obj*)kelas->name);
            htable_mark(vm, &kelas->methods);
            break;
        }

        case OBJ_INSTANCE:
        {
            ObjInstance* instance = (ObjInstance*)object;
            mem_markobject(vm, (Obj*)instance->kelas);
            htable_mark(vm, &instance->fields);
            break;
        }
            // these two objects contain NO OUTGOING REFERENCES there is nothing to traverse
        case OBJ_NATIVE:
        case OBJ_STRING:
            break;
    }
}


// traversing the gray stack work list
void mem_tracerefs(VM* vm)
{
    while(vm->grayCount > 0)
    {
        // vm_pop Obj* (pointer) from the stack
        // note how -- is the prefix; subtract first then use it as an index
        // --vm->grayCount already decreases its count, hence everything is already 'popped'
        Obj* object = vm->grayStack[--vm->grayCount];
        mem_blackenobject(vm, object);
    }
}


// sweeping all unreachable values
void mem_sweep(VM* vm)
{
    Obj* previous = NULL;
    Obj* object = vm->objects;// linked intrusive list of Objects in the VM

    while(object != NULL)
    {
        if(object->isMarked)// object marked, do not free
        {
            object->isMarked = false;// reset the marking to 'white'
            previous = object;
            object = object->next;
        }
        else// free the unreachable object
        {
            Obj* unreached = object;
            object = object->next;

            if(previous != NULL)// link to previous object if previous not null
            {
                previous->next = object;
            }
            else// if not set the next as the start of the list
            {
                vm->objects = object;
            }

            mem_freeobject(vm, unreached);// method that actually frees the object
        }
    }
}

void mem_collectgarbage(VM* vm)
{
#ifdef DEBUG_LOG_GC
    printf("--Garbage Collection Begin\n");
    size_t before = vm->bytesAllocated;
#endif

    mem_markroots(vm);// function to start traversing the graph, from the root and marking them
    mem_tracerefs(vm);// tracing each gray marked object

    // removing intern strings, BEFORE the sweep so the pointers can still access its memory
    // function defined in hahst.c
    htable_removewhite(vm, &vm->strings);

    mem_sweep(vm);// free all unreachable roots

    // adjust size of threshold
    vm->nextGC = vm->bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("--Garbage Collection End\n");
    printf("	collected %zd bytes (from %zd to %zd) next at %zd\n", before - vm->bytesAllocated, before, vm->bytesAllocated, vm->nextGC);
#endif
}

void mem_freeobjlist(VM* vm)// free from VM
{
    Obj* object = vm->objects;
    // free from the whole list
    while(object != NULL)
    {
        Obj* next = object->next;
        mem_freeobject(vm, object);
        object = next;
    }

    free(vm->grayStack);// free gray marked obj stack used for garbage collection
}


int dbg_print_simpleinst(VM* vm, const char* name, int offset)
{
    (void)vm;
    printf("%s\n", name);// print as a string, or char*
    return offset + 1;
}

int dbg_print_byteinst(VM* vm, const char* name, Chunk* chunk, int offset)
{
    (void)vm;
    uint8_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}

int dbg_print_constinst(VM* vm, const char* name, Chunk* chunk, int offset)
{
    (void)vm;
    uint8_t constant = chunk->code[offset + 1];// pullout the constant index from the subsequent byte in the chunk
    printf("%-16s %4d '", name, constant);// print out name of the opcode, then the constant index
    obj_printvalue(vm, chunk->constants.values[constant]);//	display the value of the constant,  user defined function
    printf("'\n");
    return offset + 2;//OP_RETURN is a single byte, and the other byte is the operand, hence offsets by 2
}

int dbg_print_invokeinst(VM* vm, const char* name, Chunk* chunk, int offset)
{
    (void)vm;
    uint8_t constant = chunk->code[offset + 1];// get index of the name first
    uint8_t argCount = chunk->code[offset + 2];// then get number of arguments
    printf("%-16s (%d args) %4d", name, argCount, constant);
    obj_printvalue(vm, chunk->constants.values[constant]);// print the method
    printf("\n");
    return offset + 3;
}

int dbg_print_jumpinst(VM* vm, const char* name, int sign, Chunk* chunk, int offset)
{
    (void)vm;
    uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);// get jump
    jump |= chunk->code[offset + 2];
    printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

void dbg_disasmchunk(VM* vm, Chunk* chunk, const char* name)
{
    printf("== %s ==\n", name);// print a little header for debugging

    for(int offset = 0; offset < chunk->count;)// for every existing instruction in the chunk
    {
        offset = dbg_disasminst(vm, chunk, offset);// disassemble individually, offset will be controlled from this function
    }
}

int dbg_disasminst(VM* vm, Chunk* chunk, int offset)
{
    printf("%04d ", offset);// print byte offset of the given instruction, or the index
    /* quick note on C placeholders
	say, we have int a = 2
	if %2d, it will be " 2"
	if %02d, it will be "02'
	*/


    // show source line each instruction was compiled from
    if(offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1])// show a | for any instruction that comes from the
    //same source as its preceding one

    {
        printf("	| ");
    }
    else
    {
        printf("%4d ", chunk->lines[offset]);
    }

    uint8_t instruction = chunk->code[offset];// takes one byte, or an element, from the container
    switch(instruction)
    {
        case OP_CONSTANT:
            return dbg_print_constinst(vm, "OP_CONSTANT", chunk, offset);// pass in chunk to get ValueArray element

        // literals
        case OP_NULL:
            return dbg_print_simpleinst(vm, "OP_NULL", offset);
        case OP_TRUE:
            return dbg_print_simpleinst(vm, "OP_TRUE", offset);
        case OP_FALSE:
            return dbg_print_simpleinst(vm, "OP_FALSE", offset);

        case OP_EQUAL:
            return dbg_print_simpleinst(vm, "OP_EQUAL", offset);
        case OP_GREATER:
            return dbg_print_simpleinst(vm, "OP_GREATER", offset);
        case OP_LESS:
            return dbg_print_simpleinst(vm, "OP+LESS", offset);

        // unary
        case OP_NEGATE:
            return dbg_print_simpleinst(vm, "OP_NEGATE", offset);

        // binary
        case OP_ADD:
            return dbg_print_simpleinst(vm, "OP_ADD", offset);
        case OP_SUBTRACT:
            return dbg_print_simpleinst(vm, "OP_MINUS", offset);
        case OP_MULTIPLY:
            return dbg_print_simpleinst(vm, "OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return dbg_print_simpleinst(vm, "OP_DIVIDE", offset);
        case OP_MODULO:
            return dbg_print_simpleinst(vm, "OP_MODULO", offset);

        case OP_NOT:
            return dbg_print_simpleinst(vm, "OP_NOT", offset);

        case OP_POP:
            return dbg_print_simpleinst(vm, "OP_POP", offset);

        // names for local variables do not get carried over, hence only the slot number is shown
        case OP_GET_LOCAL:
            return dbg_print_byteinst(vm, "OP_GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return dbg_print_byteinst(vm, "OP_SET_LOCAL", chunk, offset);

        case OP_GET_UPVALUE:
            return dbg_print_byteinst(vm, "OP_GET_UPVALUE", chunk, offset);
        case OP_SET_UPVALUE:
            return dbg_print_byteinst(vm, "OP_SET_UPVALUE", chunk, offset);
        case OP_GET_PROPERTY:
            return dbg_print_constinst(vm, "OP_GET_PROPERTY", chunk, offset);
        case OP_SET_PROPERTY:
            return dbg_print_constinst(vm, "OP_SET_PROPERTY", chunk, offset);

        case OP_CLOSE_UPVALUE:
            return dbg_print_simpleinst(vm, "OP_CLOSE_VALUE", offset);

        case OP_DEFINE_GLOBAL:
            return dbg_print_simpleinst(vm, "OP_DEFINE_GLOBAL", offset);
        case OP_GET_GLOBAL:
            return dbg_print_simpleinst(vm, "OP_GET_GLOBAL", offset);
        case OP_SET_GLOBAL:
            return dbg_print_simpleinst(vm, "OP_SET_GLOBAL", offset);
        case OP_SWITCH_EQUAL:
            return dbg_print_simpleinst(vm, "OP_SWITCH_EQUAL", offset);

        case OP_JUMP:
            return dbg_print_jumpinst(vm, "OP_JUMP", 1, chunk, offset);
        case OP_JUMP_IF_FALSE:
            return dbg_print_jumpinst(vm, "OP_JUMP_IF_FALSE", 1, chunk, offset);

        case OP_CALL:
            return dbg_print_byteinst(vm, "OP_CALL", chunk, offset);

        case OP_METHOD:
            return dbg_print_constinst(vm, "OP_METHOD", chunk, offset);

        case OP_INVOKE:
            return dbg_print_invokeinst(vm, "OP_INVOKE", chunk, offset);


        case OP_CLOSURE:
        {
            offset++;
            uint8_t constant = chunk->code[offset++];// index for Value
            printf("%-16s %4d ", "OP_CLOSURE", constant);
            obj_printvalue(vm, chunk->constants.values[constant]);// accessing the value using the index
            printf("\n");

            ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
            for(int j = 0; j < function->upvalueCount; j++)// walk through upvalues
            {
                int isLocal = chunk->code[offset++];
                int index = chunk->code[offset++];
                printf("%04d	|	%s %d\n", offset - 2, isLocal ? "local" : "upvalue", index);
            }

            return offset;
        }

        case OP_CLASS:
            return dbg_print_constinst(vm, "OP_CLASS", chunk, offset);

        case OP_INHERIT:
            return dbg_print_simpleinst(vm, "OP_INEHEIRT", offset);


        case OP_GET_SUPER:// class inheritance
            return dbg_print_constinst(vm, "OP_GET_SUPER", chunk, offset);

        case OP_SUPER_INVOKE:
            return dbg_print_invokeinst(vm, "OP_SUPER_INVOKE", chunk, offset);

        case OP_RETURN:
            return dbg_print_simpleinst(vm, "OP_RETURN", offset);// dispatch to a utility function to display it

        case OP_LOOP:
            return dbg_print_jumpinst(vm, "OP_LOOP", -1, chunk, offset);

        case OP_LOOP_IF_TRUE:
            return dbg_print_jumpinst(vm, "OP_LOOP_IF_TRUE", -1, chunk, offset);

        case OP_LOOP_IF_FALSE:
            return dbg_print_jumpinst(vm, "OP_LOOP_IF_FALSE", -1, chunk, offset);

        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}

void htable_init(VM* vm, Table* table)
{
    (void)vm;
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void htable_free(VM* vm, Table* table)
{
    FREE_ARRAY(vm, Entry, table->entries, table->capacity);
    htable_init(vm, table);
    table->entries = NULL;
}

Entry* htable_findentry(Entry* entries, int capacity, ObjString* key)
{
    uint32_t index = key->hash % capacity;// use modulo to map the key's hash to the code index
    Entry* tombstone = NULL;

    for(;;)
    {
        Entry* entry = &entries[index];// index is 'inserted' here

        if(entry->key == NULL)
        {
            if(IS_NULL(entry->value))
            {
                return tombstone != NULL ? tombstone : entry;// empty entry
            }
            else
            {
                if(tombstone == NULL)
                    tombstone = entry;// can return tombstone bucket as empty and reuse it
            }
        }
        if(entry->key == key)// compare them in MEMORY
        {
            return entry;
        }


        index = (index + 1) % capacity;
    }
}

bool htable_get(VM* vm, Table* table, ObjString* key, Value* value)
{
    (void)vm;
    if(table->count == 0)
        return false;

    Entry* entry = htable_findentry(table->entries, table->capacity, key);
    if(entry->key == NULL)
        return false;

    *value = entry->value;// asign the value parameter the entry value
    return true;
}


void htable_adjustcap(VM* vm, Table* table, int capacity)
{
    int i;
    Entry* dest;
    Entry* entry;
    Entry* entries;
    entries = ALLOCATE(vm, Entry, capacity);// create a bucket with capacity entries, new array
    for(i = 0; i < capacity; i++)// initialize every element
    {
        entries[i].key = NULL;
        entries[i].value = NULL_VAL;
    }
    table->count = 0;// do not copy tombstones over when growing
    // NOTE: entries may end up in different buckets
    // with the same hash as it is divided by the modulo; loop below recalculates everything
    for(i = 0; i < table->capacity; i++)// travers through old array
    {
        entry = &table->entries[i];
        if(entry->key == NULL)
        {
            continue;
        }
        // insert into new array
        dest = htable_findentry(entries, capacity, entry->key);// pass in new array
        dest->key = entry->key;// match old array to new array
        dest->value = entry->value;
        table->count++;// recound the number of entries
    }

    FREE_ARRAY(vm, Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

// inserting into the table, return false if collision
bool htable_set(VM* vm, Table* table, ObjString* key, Value value)
{
    // make sure array is big enough
    if(table->count + 1 > table->capacity * TABLE_MAX_LOAD)
    {
        int capacity = GROW_CAPACITY(vm, table->capacity);
        htable_adjustcap(vm, table, capacity);
    }


    Entry* entry = htable_findentry(table->entries, table->capacity, key);

    bool isNewKey = entry->key == NULL;
    if(isNewKey && IS_NULL(entry->value))
        table->count++;// IS_NULL for tombstones; treat them as full objects

    entry->key = key;
    entry->value = value;

    return isNewKey;
}


bool htable_delete(VM* vm, Table* table, ObjString* key)
{
    (void)vm;
    if(table->count == 0)
        return false;

    // find entry
    Entry* entry = htable_findentry(table->entries, table->capacity, key);
    if(entry->key == NULL)
        return false;

    // place tombstone
    entry->key = NULL;
    entry->value = BOOL_VAL(true);//BOOL_VAL(true) as the tombstone

    return true;
}

void htable_addall(VM* vm, Table* from, Table* to)
{
    for(int i = 0; i < from->capacity; i++)
    {
        Entry* entry = &from->entries[i];
        if(entry->key != NULL)
        {
            htable_set(vm, to, entry->key, entry->value);
        }
    }
}

// used in VM to find the string
ObjString* htable_findstring(VM* vm, Table* table, const char* chars, int length, uint32_t hash)// pass in raw character array
{
    (void)vm;
    if(table->count == 0)
        return NULL;

    uint32_t index = hash % table->capacity;// get index

    for(;;)
    {
        Entry* entry = &table->entries[index];// get entry pointer
        if(entry->key == NULL)
        {
            // stop if found empty non-tombstone entry
            if(IS_NULL(entry->value))
                return NULL;// return null if not tombstone(tombstone value is BOOL_VAL(true))
        }
        else if(entry->key->length == length && entry->key->hash == hash && memcmp(entry->key->chars, chars, length) == 0)
        {
            return entry->key;// found the entry
        }

        index = (index + 1) % table->capacity;
    }
}

// removing unreachable pointers, used to remove string interns in garbage collection
void htable_removewhite(VM* vm, Table* table)
{
    for(int i = 0; i < table->capacity; i++)
    {
        Entry* entry = &table->entries[i];
        if(entry->key != NULL && !entry->key->obj.isMarked)// remove not marked (string) object pointers
        {
            htable_delete(vm, table, entry->key);
        }
    }
}


// mark global variables, used in VM for garbage collection
void htable_mark(VM* vm, Table* table)
{
    for(int i = 0; i < table->capacity; i++)
    {
        Entry* entry = &table->entries[i];
        // need to mark both the STRING KEYS and the actual value/obj itself
        mem_markobject(vm, (Obj*)entry->key);// mark the string key(ObjString type)
        mem_markvalue(vm, entry->value);// mark the actual avlue
    }
}

void chunk_init(VM* vm, Chunk* chunk)
{
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;// dynamic array starts off completely empty
    chunk->lines = NULL;// to store current line of code
    obj_initvalarray(vm, &chunk->constants);// initialize constant list
}

void chunk_write(VM* vm, Chunk* chunk, uint8_t byte, int line)
{
    if(chunk->capacity < chunk->count + 1)// check if chunk is full
    {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(vm, oldCapacity);// get size of new capacity
        chunk->code = GROW_ARRAY(vm, uint8_t, chunk->code, oldCapacity, chunk->capacity);// mem_realloc memory and grow array
        chunk->lines = GROW_ARRAY(vm, int, chunk->lines, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;// code is an array, [] is just the index number
    chunk->lines[chunk->count] = line;
    chunk->count++;
}


void chunk_free(VM* vm, Chunk* chunk)
{
    FREE_ARRAY(vm, uint8_t, chunk->code, chunk->capacity);// chunk->code is the pointer to the array, capacity is the size
    FREE_ARRAY(vm, int, chunk->lines, chunk->capacity);
    obj_freevalarray(vm, &chunk->constants);
    chunk_init(vm, chunk);
}

int chunk_addconstant(VM* vm, Chunk* chunk, Value value)
{
    vm_push(vm, value);// garbage collection
    obj_writevalarray(vm, &chunk->constants, value);
    vm_pop(vm);// garbage collection
    return chunk->constants.count - 1;// return index of the newly added constant
}

bool obj_istype(Value value, ObjType type)
{
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

// new bound method for classes
ObjBoundMethod* obj_mkboundmethod(VM* vm, Value receiver, ObjClosure* method)
{
    ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}


// create new closure
ObjClosure* obj_mkclosure(VM* vm, ObjFunction* function)
{
    // initialize array of upvalue pointers
    // upvalues carry over
    ObjUpvalue** upvalues = ALLOCATE(vm, ObjUpvalue*, function->upvalueCount);

    for(int i = 0; i < function->upvalueCount; i++)
    {
        upvalues[i] = NULL;// initialize all as null
    }


    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;
    return closure;
}

ObjString* obj_mkstring(VM* vm, char* chars, int length, uint32_t hash)// pass in hash
{
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    vm_push(vm, OBJ_VAL(string));// garbage collection
    //printf("allocate\n");
    htable_set(vm, &vm->strings, string, NULL_VAL);// for string interning
    vm_pop(vm);// garbage collection

    return string;
}

ObjClass* obj_mkclass(VM* vm, ObjString* name)
{
    ObjClass* kelas = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);// kelas not class for compiling in c++
    kelas->name = name;
    htable_init(vm, &kelas->methods);
    return kelas;
}


// create new class instance
ObjInstance* obj_mkinstance(VM* vm, ObjClass* kelas)
{
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->kelas = kelas;
    htable_init(vm, &instance->fields);// memory address of the fields
    return instance;
}


ObjFunction* obj_mkfunction(VM* vm)
{
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);

    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    chunk_init(vm, &function->chunk);
    return function;
}

// new native function
ObjNative* obj_mknative(VM* vm, NativeFn function)
{
    (void)vm;
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    return native;
}


// hash function, the FNV-1a
uint32_t obj_hashstring(VM* vm, const char* key, int length)
{
    (void)vm;
    uint32_t hash = 2116136261u;// initial hash value, u at end means unsigned

    for(int i = 0; i < length; i++)// traverse through the data to be hashed
    {
        hash ^= key[i];// munge the bits from the string key to the hash value; ^= is a bitwise operator
        hash *= 16777619;
    }

    return hash;
}


// shorten than obj_copystring because owernship of the char* itself is declared in vm_concatenate(), hence no need to declare memory again
ObjString* obj_takestring(VM* vm, char* chars, int length)
{
    uint32_t hash = obj_hashstring(vm, chars, length);
    ObjString* interned = htable_findstring(vm, &vm->strings, chars, length, hash);


    if(interned != NULL)// if the same string already exists
    {
        FREE_ARRAY(vm, char, chars, length + 1);// free the memory for use
        return interned;
    }


    return obj_mkstring(vm, chars, length, hash);
}

// copy string from source code to memory
ObjString* obj_copystring(VM* vm, const char* chars, int length)
{
    uint32_t hash = obj_hashstring(vm, chars, length);
    ObjString* interned = htable_findstring(vm, &vm->strings, chars, length, hash);

    if(interned != NULL)
    {
        return interned;// if we find a string already in vm->srings, no need to copy just return the pointer
    }
    char* heapChars = ALLOCATE(vm, char, length + 1);// length +1 for null terminator
    memcpy(heapChars, chars, length);// copy memory from one location to another; memcpy(*to, *from, size_t (from))
    heapChars[length] = '\0';// '\0', a null terminator used to signify the end of the string, placed at the end

    return obj_mkstring(vm, heapChars, length, hash);
}


ObjUpvalue* obj_mkupvalue(VM* vm, Value* slot)
{
    (void)vm;
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->next = NULL;
    upvalue->closed = NULL_VAL;
    return upvalue;
}

void obj_printfunction(VM* vm, ObjFunction* function)
{
    (void)vm;
    if(function->name == NULL)
    {
        printf("<script>");
        return;
    }
    printf("fun %s(%d params)", function->name->chars, function->arity);// print name and number of parameters
}

void obj_printobject(VM* vm, Value value)
{
    // first class objects can be printed; string and functions
    switch(OBJ_TYPE(value))
    {
        case OBJ_BOUND_METHOD:
            obj_printfunction(vm, AS_BOUND_METHOD(value)->method->function);
            break;
        case OBJ_CLASS:
            printf("%s", AS_CLASS(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            printf("%s instance", AS_INSTANCE(value)->kelas->name->chars);
            break;
        case OBJ_CLOSURE:
            obj_printfunction(vm, AS_CLOSURE(value)->function);
            break;
        case OBJ_FUNCTION:
            obj_printfunction(vm, AS_FUNCTION(value));
            break;
        case OBJ_NATIVE:
            printf("<native fun>");
            break;
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_UPVALUE:
            printf("upvalue");
            break;
        default:
            return;
    }
}


void obj_initvalarray(VM* vm, ValueArray* array)
{
    (void)vm;
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}

void obj_writevalarray(VM* vm, ValueArray* array, Value value)
{
    if(array->capacity < array->count + 1)
    {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(vm, oldCapacity);
        array->values = GROW_ARRAY(vm, Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void obj_freevalarray(VM* vm, ValueArray* array)
{
    FREE_ARRAY(vm, Value, array->values, array->capacity);
    obj_initvalarray(vm, array);
}

// actual printing on the virtual machine is done here
void obj_printvalue(VM* vm, Value value)
{
    switch(value.type)
    {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NULL:
            printf("null");
            break;
        case VAL_NUMBER:
            printf("%g", AS_NUMBER(value));
            break;
        case VAL_OBJ:
            obj_printobject(vm, value);
            break;// print heap allocated value, from object.h
    }
}

// comparison function used in VM run()
// used in ALL types of data(num, string, bools)
bool obj_valequal(VM* vm, Value a, Value b)
{
    (void)vm;
    if(a.type != b.type)
        return false;// if type is different return false

    switch(a.type)
    {
        case VAL_BOOL:
            return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NUMBER:
            return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_NULL:
            return true;// true for all nulls
        case VAL_OBJ:
            return AS_OBJ(a) == AS_OBJ(b);// already interned, occupies the same address
        default:
            return false;// unreachable
    }
}

void scanner_init(const char* source)
{
    g_scanner.start = source;// again, pointing to a string array means pointing to the beginning
    g_scanner.current = source;
    g_scanner.line = 1;
}


// to check for identifiers(eg. for, while, print)
bool scanner_isalpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool scanner_isdigit(char c)
{
    return c >= '0' && c <= '9';// let string comparison handle it
}

// to get EOF symbol -> '\0'
bool scanner_isatend()
{
    return *g_scanner.current == '\0';
}


// goes to next char
char scanner_advance()
{
    g_scanner.current++;// advance to next
    return g_scanner.current[-1];// return previous one
}

// logical conditioning to check if 2nd character is embedded to first(e.g two char token)
bool scanner_match(char expected)
{
    if(scanner_isatend())
        return false;// if already at end, error
    if(*g_scanner.current != expected)
    {
        //printf("no match");
        return false;// if current char does not equal expected char, it is false
    }
    //printf("match");
    g_scanner.current++;// if yes, advance to next
    return true;
}

// make a token, uses the scanner's start and current to capture the lexeme and its size
Token scanner_maketoken(TokenType type)
{
    Token token;
    token.type = type;
    token.start = g_scanner.start;
    token.length = (int)(g_scanner.current - g_scanner.start);
    token.line = g_scanner.line;

    return token;
}

// similar to scanner_maketoken, but the "lexeme" points to the error message string instead of the scanner's current and start pointers
Token scanner_errortoken(const char* message)
{
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);// get string length and turn to int
    token.line = g_scanner.line;

    return token;
}

// returns current character
char scanner_peek()
{
    return *g_scanner.current;
}

// returns next character
char scanner_peeknext()
{
    if(scanner_isatend())
        return '\0';
    return g_scanner.current[1];// C syntax, basically return index 1 (or second) from the array/pointer
}

// skipping white spaces, tabs, etc.
void scanner_skipspace()
{
    for(;;)
    {
        char c = scanner_peek();
        switch(c)
        {
            case ' ':
            case '\r':
            case '\t':
                {
                    scanner_advance();
                }
                break;
            case '\n':
                {
                    g_scanner.line++;
                    scanner_advance();
                }
                break;
            // for comments
            case '/':
                {
                    if(scanner_peeknext() == '/')
                    {
                        // comment goes until end of line
                        while(scanner_peek() != '\n' && !scanner_isatend())
                            scanner_advance();// if not new line or not end, treat as whitespace and advance
                    }
                    else
                    {
                        return;
                    }
                }
                break;
            default:
                {
                    return;
                }
                break;
        }
    }
}


// to check for identifiers, if they are keyword or not. rest means the rest of the letter
TokenType scanner_checkkeyword(int start, int length, const char* rest, TokenType type)
{
    /* hard expression here
	bascially if they are exactly the same, and compares their memory(memcmp)
	int memcmp(const void *str1, const void *str2, size_t n) -> if it is exactly the same, then it is 0
	*/
    if(g_scanner.current - g_scanner.start == start + length && memcmp(g_scanner.start + start, rest, length) == 0)
    {
        return type;
    }

    return TOKEN_IDENTIFIER;
}
// the 'trie' to store the set of strings
TokenType scanner_parseidenttype()
{
    switch(g_scanner.start[0])// start of the lexeme
    {
        //case 'a': return scanner_checkkeyword(1, 2, "nd", TOKEN_AND);
        case 'a':
            {
                if(g_scanner.current - g_scanner.start > 1)
                {
                    switch(g_scanner.start[1])
                    {
                        case 'n':
                            {
                                return scanner_checkkeyword(2, 1, "d", TOKEN_AND);
                            }
                            break;
                        case 's':
                            {
                                return scanner_checkkeyword(2, 6, "signed", TOKEN_EQUAL);
                            }
                            break;
                    }
                }
            }
            break;
        case 'b':
            {
                return scanner_checkkeyword(1, 4, "reak", TOKEN_BREAK);
            }
            break;
        case 'c':
            {
                if(g_scanner.current - g_scanner.start > 1)
                {
                    switch(g_scanner.start[1])
                    {
                        case 'a':
                            {
                                return scanner_checkkeyword(2, 2, "se", TOKEN_CASE);
                            }
                            break;
                        case 'l':
                            {
                                return scanner_checkkeyword(2, 3, "ass", TOKEN_CLASS);
                            }
                            break;
                        case 'o':
                            {
                                return scanner_checkkeyword(2, 6, "ntinue", TOKEN_CONTINUE);
                            }
                            break;
                    }
                }
            }
            break;
        case 'd':
            {
                if(g_scanner.current - g_scanner.start > 1)
                {
                    switch(g_scanner.start[1])
                    {
                        case 'e':
                            {
                                return scanner_checkkeyword(2, 5, "fault", TOKEN_DEFAULT);
                            }
                            break;
                        case 'o':
                            {
                                return scanner_checkkeyword(2, 0, "", TOKEN_DO);
                            }
                            break;
                    }
                }
            }
            break;
        case 'e':
            {
                if(g_scanner.current - g_scanner.start > 1)
                {
                    switch(g_scanner.start[1])// check if there is a second letter
                    {
                        case 'l':
                            {
                                if(g_scanner.current - g_scanner.start > 2)// check if there is a third letter
                                {
                                    switch(g_scanner.start[2])
                                    {
                                        case 's':
                                            return scanner_checkkeyword(3, 1, "e", TOKEN_ELSE);
                                        case 'f':
                                            return scanner_checkkeyword(3, 0, "", TOKEN_ELF);// already matched
                                    }
                                }
                            }
                            break;
                        case 'q':
                            {
                                return scanner_checkkeyword(2, 4, "uals", TOKEN_EQUAL_EQUAL);
                            }
                            break;
                    }
                }
            }
            break;
        case 'f':
            {
                if(g_scanner.current - g_scanner.start > 1)// check if there is a second letter
                {
                    switch(g_scanner.start[1])
                    {
                        case 'a':
                            {
                                return scanner_checkkeyword(2, 3, "lse", TOKEN_FALSE);// starts from 2 not 3, as first letter is already an f
                            }
                            break;
                        case 'o':
                            {
                                return scanner_checkkeyword(2, 1, "r", TOKEN_FOR);
                            }
                            break;
                        case 'r':
                            {
                                return scanner_checkkeyword(2, 2, "om", TOKEN_FROM);
                            }
                            break;
                        case 'n':
                            {
                                return scanner_checkkeyword(2, 0, "", TOKEN_FUN);
                            }
                            break;
                        case 'u':
                            {
                                return scanner_checkkeyword(2, 6, "nction", TOKEN_FUN);
                            }
                            break;
                    }
                }
            }
            break;
        case 'i':
            {
                if(g_scanner.current - g_scanner.start > 1)
                {
                    switch(g_scanner.start[1])
                    {
                        case 'f':
                            {
                                return scanner_checkkeyword(2, 0, "", TOKEN_IF);
                            }
                            break;
                        case 's':
                            {
                                return scanner_checkkeyword(2, 0, "", TOKEN_EQUAL_EQUAL);
                            }
                            break;
                    }
                }
            }
            break;
        case 'n':
            {
                return scanner_checkkeyword(1, 3, "ull", TOKEN_NULL);
            }
            break;
        case 'o':
            {
                return scanner_checkkeyword(1, 1, "r", TOKEN_OR);
            }
            break;
        case 'r':
            {
                if(g_scanner.current - g_scanner.start > 1)
                {
                    switch(g_scanner.start[1])
                    {
                        case 'e':
                            if(g_scanner.current - g_scanner.start > 2)
                            {
                                switch(g_scanner.start[2])
                                {
                                    case 't':
                                        {
                                            return scanner_checkkeyword(3, 3, "urn", TOKEN_RETURN);
                                        }
                                        break;
                                    case 'p':
                                        {
                                            return scanner_checkkeyword(3, 3, "eat", TOKEN_REPEAT);
                                        }
                                        break;
                                }
                            }
                    }
                }
            }
            break;
        case 's':
            {
                if(g_scanner.current - g_scanner.start > 1)// if there is a second letter
                {
                    switch(g_scanner.start[1])
                    {
                        case 'u':
                            {
                                return scanner_checkkeyword(2, 3, "per", TOKEN_SUPER);
                            }
                            break;
                        case 'w':
                            {
                                return scanner_checkkeyword(2, 4, "itch", TOKEN_SWITCH);
                            }
                            break;
                    }
                }
            }
            break;
        case 't':
            {
                if(g_scanner.current - g_scanner.start > 1)
                {
                    switch(g_scanner.start[1])
                    {
                        //case 'h': return scanner_checkkeyword(2, 2, "is", TOKEN_THIS);
                        case 'h':
                            {
                                if(g_scanner.current - g_scanner.start > 2)// check if there is a third letter
                                {
                                    switch(g_scanner.start[2])
                                    {
                                        case 'e':
                                            return scanner_checkkeyword(3, 1, "n", TOKEN_THEN);
                                        case 'i':
                                            return scanner_checkkeyword(3, 1, "s", TOKEN_THIS);// already matched
                                    }
                                }
                            }
                            break;
                        case 'r':
                            {
                                return scanner_checkkeyword(2, 2, "ue", TOKEN_TRUE);
                            }
                            break;
                    }
                }
            }
            break;
        case 'u':
            {
                return scanner_checkkeyword(1, 4, "ntil", TOKEN_UNTIL);
            }
            break;
        case 'v':
            {
                return scanner_checkkeyword(1, 2, "ar", TOKEN_VAR);
            }
            break;
        case 'w':
            {
                return scanner_checkkeyword(1, 4, "hile", TOKEN_WHILE);
            }
            break;
    }
    return TOKEN_IDENTIFIER;
}

Token scanner_parseident()
{
    while(scanner_isalpha(scanner_peek()) || scanner_isdigit(scanner_peek()))
        scanner_advance();// skip if still letters or digits
    return scanner_maketoken(scanner_parseidenttype());
}

Token scanner_parsenumber()
{
    while(scanner_isdigit(scanner_peek()))
        scanner_advance();// while next is still a digit advance

    // look for fractional part
    if(scanner_peek() == '.' && scanner_isdigit(scanner_peeknext()))// if there is a . and next is still digit
    {
        // consume '.'
        scanner_advance();

        while(scanner_isdigit(scanner_peek()))
            scanner_advance();
    }

    return scanner_maketoken(TOKEN_NUMBER);
}

// for string tokens
Token scanner_parsestring()
{
    while(scanner_peek() != '"' && !scanner_isatend())
    {
        if(scanner_peek() == '\n')
            g_scanner.line++;// allow strings to go until next line
        scanner_advance();// consume characters until the closing quote is reached
    }

    if(scanner_isatend())
        return scanner_errortoken("Unterminated string.");

    // closing quote
    scanner_advance();
    return scanner_maketoken(TOKEN_STRING);

    // convert lexeme to runtime value later
}

// reading the char, and return a token
Token scanner_scantoken()
{
    scanner_skipspace();

    g_scanner.start = g_scanner.current;// reset the g_scanner to current

    if(scanner_isatend())
        return scanner_maketoken(TOKEN_EOF);// check if at end

    // if not end of file
    char c = scanner_advance();

    if(scanner_isalpha(c))
        return scanner_parseident();
    if(scanner_isdigit(c))
        return scanner_parsenumber();


    // lexical grammar for the language
    switch(c)
    {
            // for single characters
        case '(':
            return scanner_maketoken(TOKEN_LEFT_PAREN);
        case ')':
            return scanner_maketoken(TOKEN_RIGHT_PAREN);
        case '{':
            return scanner_maketoken(TOKEN_LEFT_BRACE);
        case '}':
            return scanner_maketoken(TOKEN_RIGHT_BRACE);
        case ';':
            return scanner_maketoken(TOKEN_SEMICOLON);
        case ':':
            return scanner_maketoken(TOKEN_COLON);
        case ',':
            return scanner_maketoken(TOKEN_COMMA);
        case '.':
            return scanner_maketoken(TOKEN_DOT);
        case '-':
            return scanner_maketoken(TOKEN_MINUS);
        case '+':
            return scanner_maketoken(TOKEN_PLUS);
        case '*':
            return scanner_maketoken(TOKEN_STAR);
        case '/':
            return scanner_maketoken(TOKEN_SLASH);
        case '%':
            return scanner_maketoken(TOKEN_MODULO);

            // for two characters
        case '!':
            return scanner_maketoken(scanner_match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '=':
            return scanner_maketoken(scanner_match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '>':
            return scanner_maketoken(scanner_match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '<':
            return scanner_maketoken(scanner_match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);

            // literal tokens
        case '"':
            return scanner_parsestring();// string token
    }


    return scanner_errortoken("Unexpected character.");
}

// for inserting where the unary operator should lie
// starts at current token and parses any expression at the given precedence level or higher
// for example, if prs_parseprecedence(PREC_COMPARISON) is called, it will parse unaries, terms, and factors
// ubt not or, and or assignment operators as they are lower. Basically parse anything that is ABOVE the given precedence
void prs_parseprecedence(VM* vm, Precedence precedence)
{
    /*	PREFIX FIRST
	look up for a prefix token, and the FIRSt token is ALWAYS going to be a prefix
	*/
    prs_advance(vm);// again, go next first then use previous type as the 'current' token
    // the way the compiler is designed is that it has to always have a prefix
    ParseFn prefixRule = prs_getrule(vm, parser.previous.type)->prefix;

    if(prefixRule == NULL)
    {
        prs_error(vm, "Expect expression.");
        return;
    }

    //

    bool canAssign = precedence <= PREC_ASSIGNMENT;// for assignment precedence
    prefixRule(vm, canAssign);// vm_call the prefix function, may consume a lot of tokens


    /* after prefix expression is done, look for infix expression
	IMPORTANT: infix only runs if given precedence is LOWER than the operator for the infix
	or more nicely if NEXT/INFIX PRECEDENCE IS HIGHER THAN PREC ASSIGNMENT(parameter above_
	*/


    while(precedence <= prs_getrule(vm, parser.current.type)->precedence)
    {
        prs_advance(vm);
        ParseFn infixRule = prs_getrule(vm, parser.previous.type)->infix;

        infixRule(vm, canAssign);
    }

    //prs_consume(vm, TOKEN_AND, "consume and failed");

    if(canAssign && prs_matchtoken(vm, TOKEN_EQUAL))// if = is not consumed as part of the expression, nothing will , hence an error
    {
        prs_error(vm, "Invalid Assignment target.");
    }
}

// get pointer to ParseRule struct according to type parameter
ParseRule* prs_getrule(VM* vm, TokenType type)
{
    (void)vm;
    return &rules[type];
}

void prs_expression(VM* vm)// a single 'statement' or line
{
    prs_parseprecedence(vm, PREC_ASSIGNMENT);// as assignment is the 2nd lowest, parses evrything
}

void prs_block(VM* vm)
{
    while(!prs_check(vm, TOKEN_RIGHT_BRACE) && !prs_check(vm, TOKEN_EOF))// parse until EOF or right brace is 'peeked'
    {
        prs_declaration(vm);// compile rest of block, keeps on parsing until right brace or EOF is 'peeked'
    }

    prs_consume(vm, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}


/* functions */
void prs_function(VM* vm, FunctionType type)
{
    // create separate Compiler for each function
    Compiler compiler;
    prs_initcompiler(vm, &compiler, type);// set new compiler(function) as the current one
    prs_beginscope(vm);

    // compile parameters
    prs_consume(vm, TOKEN_LEFT_PAREN, "Expect '(' after function name.");

    if(!prs_check(vm, TOKEN_RIGHT_PAREN))// if end ) has not been reached
    {
        do
        {
            current->function->arity++;// add number of parameters
            if(current->function->arity > 255)
            {
                prs_erroratcurrent(vm, "Cannot have more than 255 parameters.");
            }

            uint8_t paramConstant = prs_parsevariable(vm, "Expect variable name.");// get name
            prs_definevariable(vm, paramConstant);// scope handled here already
        } while(prs_matchtoken(vm, TOKEN_COMMA));
    }

    prs_consume(vm, TOKEN_RIGHT_PAREN, "Expect ')' after parameter list.");

    // body
    prs_consume(vm, TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    prs_block(vm);

    // create function object
    ObjFunction* function = prs_endcompiler(vm);// ends the current compiler
    // compilers are treated like a stack; if current one is ended, like above, return to the previous one

    // prs_emitbytes(vm, OP_CONSTANT, prs_makeconst(vm, OBJ_VAL(function)));
    prs_emitbytes(vm, OP_CLOSURE, prs_makeconst(vm, OBJ_VAL(function)));

    /*	by the time the compiler reaches the end of a function prs_declaration,
	every variable reference hass been resolved as either local, upvalue or global.
	each upvalue may return a local var or another upvalue

	-> for each upvalue there are two single-byte operands
	-> if first byte is one, then it captures a local variable in the enclosing function
	-> if first byte is 0, it captures the function's upvalues
	*/

    for(int i = 0; i < function->upvalueCount; i++)
    {
        prs_emitbyte(vm, compiler.upvalues[i].isLocal ? 1 : 0);
        prs_emitbyte(vm, compiler.upvalues[i].index);// emit index
    }
}

// create method for class type
void prs_method(VM* vm)
{
    prs_consume(vm, TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = prs_makeconstident(vm, &parser.previous);// get method name

    // method body
    FunctionType type = TYPE_METHOD;

    // if initializer
    if(parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0)
    {
        type = TYPE_INITIALIZER;
    }

    prs_function(vm, type);// process the function

    prs_emitbytes(vm, OP_METHOD, constant);
}


void prs_classdecl(VM* vm)
{
    prs_consume(vm, TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;// get class name
    uint8_t nameConstant = prs_makeconstident(vm, &parser.previous);// add to constant table as a string, return its index
    prs_declarevariable(vm);// declare that name variable

    prs_emitbytes(vm, OP_CLASS, nameConstant);// takes opcode and takes the constant table index
    prs_definevariable(vm, nameConstant);// add it to the global hasht; we must DEFINE AFTER DECLARE to use it

    // handle class enclosing for 'this'
    ClassCompiler classCompiler;
    classCompiler.name = parser.previous;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;// set new class as current

    // class inheritance
    if(prs_matchtoken(vm, TOKEN_FROM))
    {
        prs_consume(vm, TOKEN_IDENTIFIER, "Expect parent class name.");
        rule_variable(vm, false);// get the class variable, looks up the parent class by name and vm_push it to the stack

        // check that the class names must be different
        if(prs_identequal(vm, &className, &parser.previous))
        {
            prs_error(vm, "Cannot inherit class from itself");
        }

        /* super classes
		- create new lexical scope to ensure that if we declare two classes in the same scope, each has a different
		local slot to store the superclasses
		*/
        prs_beginscope(vm);
        prs_addlocal(vm, prs_makesyntoken(vm, "super"));
        prs_definevariable(vm, 0);

        rule_namedvar(vm, className, false);
        prs_emitbyte(vm, OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }
    rule_namedvar(vm, className, false);// helper function to geenrate code that LOADS a variable with a given name to te stack
    prs_consume(vm, TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while(!prs_check(vm, TOKEN_RIGHT_BRACE) && !prs_check(vm, TOKEN_EOF))
    {
        prs_method(vm);
    }
    prs_consume(vm, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    prs_emitbyte(vm, OP_POP);// no longer need the class, vm_pop it

    // close local scope for superclass variable
    if(classCompiler.hasSuperclass)
    {
        prs_endscope(vm);
    }

    currentClass = currentClass->enclosing;// go back to enclosing/main() class
}


void prs_funcdecl(VM* vm)
{
    uint8_t global = prs_parsevariable(vm, "Expect function name.");
    prs_markinitialized(vm);// scoping
    prs_function(vm, TYPE_FUNCTION);
    prs_definevariable(vm, global);
}

void prs_vardecl(VM* vm)
{
    uint8_t global = prs_parsevariable(vm, "Expect variable name.");

    if(prs_matchtoken(vm, TOKEN_EQUAL))
    {
        prs_expression(vm);
    }
    else
    {
        prs_emitbyte(vm, OP_NULL);// not initialized
    }
    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    prs_definevariable(vm, global);// create global variable here; if local, not added to table
}

void prs_exprstmt(VM* vm)
{
    prs_expression(vm);
    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after expression.");
    prs_emitbyte(vm, OP_POP);
}

// if method
void prs_ifstmt(VM* vm)
{
    //	prs_consume(v, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    prs_expression(vm);// compile the expression statment inside; prs_parseprecedence()
    // after compiling expression above conditon value will be left at the top of the stack
    //	prs_consume(vm, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    //prs_consume(vm, TOKEN_THEN, "Missing 'then' keyword after if expression.");

    // gives an operand on how much to offset the ip; how many bytes of code to skip
    // if falsey, simply adjusts the ip by that amount
    // offset to jump to next (potentially else or elf) statment
    // insert to opcode the then branch statment first, then get offset
    int thenJump = prs_emitjump(vm, OP_JUMP_IF_FALSE); /* this gets distance */


    prs_emitbyte(vm, OP_POP);// vm_pop then

    /* use BACKPATCHING
	- emit jump first with a placeholder offset, and get how far to jump
	
	
	*/

    prs_statement(vm);

    // below jump wil SURELY jump; this is skipped if the first prs_emitjump is not false
    int elseJump = prs_emitjump(vm, OP_JUMP);// need to jump at least 'twice' with an else statement
    // if the original statement is  true, then skip the the else statement

    // if then statment is run; vm_pop the expression inside () after if
    prs_patchjump(vm, thenJump); /* this actually jumps */

    prs_emitbyte(vm, OP_POP);// if else statment is run; vm_pop the expression inside () after if
    if(prs_matchtoken(vm, TOKEN_ELSE))
        prs_statement(vm);

    if(prs_matchtoken(vm, TOKEN_ELF))// else if
    {
        // go to statement, then go back to IF
        prs_ifstmt(vm);
    }

    /* this actually jumps */
    // last jump that is executed IF FIRST STATEMENT IS TRUE
    prs_patchjump(vm, elseJump);// for the second jump
}

void prs_switchstmt(VM* vm)
{
    // prs_consume(vm, TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
    if(!prs_check(vm, TOKEN_IDENTIFIER))// check next token
    {
        prs_erroratcurrent(vm, "Expect identifier after switch.");
    }

    // if no error, prs_consume the identifier
    prs_expression(vm);
    prs_consume(vm, TOKEN_LEFT_BRACE, "Expect '{' after switch identifier.");
    prs_consume(vm, TOKEN_CASE, "Expect at least 1 case after switch declaration.");

    /* to store  opcode offsets */
    uint8_t casesCount = -1;
    uint8_t capacity = 0;
    int* casesOffset = ALLOCATE(vm, int, 8);// 8 initial switch cases

    do// while next token is a case, match also advances
    {
        // grow array if needed
        if(capacity < casesCount + 1)
        {
            int oldCapacity = capacity;
            capacity = GROW_CAPACITY(vm, oldCapacity);
            casesOffset = GROW_ARRAY(vm, int, casesOffset, oldCapacity, capacity);
        }

        casesCount++;

        prs_expression(vm);
        prs_consume(vm, TOKEN_COLON, "Expect ':' after case expression.");
        prs_emitbyte(vm, OP_SWITCH_EQUAL);// check if both values are equal

        int caseFalseJump = prs_emitjump(vm, OP_JUMP_IF_FALSE);// jump if false
        //printf("\ncase false jump offset: %d", caseFalseJump);

        // parse the statment
        prs_statement(vm);

        prs_emitbyte(vm, OP_POP);// vm_pop the 'true' from OP_SWITCH_EQUAL
        casesOffset[casesCount] = prs_emitjump(vm, OP_JUMP);
        //printf("\ncase true jump offset: %d", casesOffset[casesCount]);

        // jump to end of case if false
        prs_patchjump(vm, caseFalseJump);
        prs_emitbyte(vm, OP_POP);// vm_pop the 'false' statment from OP_SWITCH_EQUAL
    } while(prs_matchtoken(vm, TOKEN_CASE));

    if(prs_matchtoken(vm, TOKEN_DEFAULT))
    {
        prs_consume(vm, TOKEN_COLON, "Expect ':' default case.");
        prs_statement(vm);// running the default statement
    }
    //prs_consume(vm, TOKEN_DEFAULT, "Default case not provided for switch.");


    // prs_patchjump for each available jump
    for(uint8_t i = 0; i <= casesCount; i++)
    {
        prs_patchjump(vm, casesOffset[i]);
    }

    prs_emitbyte(vm, OP_POP);// vm_pop switch constant
    FREE_ARRAY(vm, int, casesOffset, capacity);

    prs_consume(vm, TOKEN_RIGHT_BRACE, "Expect '}' at the end of switch statement");
}


void prs_returnstmt(VM* vm)
{
    if(current->type == TYPE_SCRIPT)
    {
        prs_error(vm, "Cannot return from top-level code.");
    }
    if(prs_matchtoken(vm, TOKEN_SEMICOLON))
    {
        prs_emitreturn(vm);
    }
    else
    {
        // error in returning from an initializer
        if(current->type == TYPE_INITIALIZER)
        {
            prs_error(vm, "Cannot return a value from an initializer");
        }

        prs_expression(vm);
        prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after return value.");
        prs_emitbyte(vm, OP_RETURN);
    }
}

void prs_forstmt(VM* vm)
{
    prs_beginscope(vm);// for possible variable declarations in clause

    prs_beginloopscope(vm);

    prs_consume(vm, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    // initializer clause
    if(prs_matchtoken(vm, TOKEN_SEMICOLON))
    {
        // no initializer
    }
    else if(prs_matchtoken(vm, TOKEN_VAR))
    {
        prs_vardecl(vm);// for clause scope only
    }
    else
    {
        prs_exprstmt(vm);
    }

    // for for/while loops, loop starts here, with currenChunk()->count
    int loopStart = prs_currentchunk(vm)->count;

    //  the condition clause
    /* CONDITION CLAUSE
	1. If false, vm_pop the recently calculated expression and skip the loop
	2. if true, go to the body; see increment clause below
	*/
    int exitJump = -1;
    if(!prs_matchtoken(vm, TOKEN_SEMICOLON))
    {
        prs_expression(vm);
        prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // jump out of loop if condition is false
        exitJump = prs_emitjump(vm, OP_JUMP_IF_FALSE);
        prs_emitbyte(vm, OP_POP);// still need to figure this out, most likely just deleting 'temporary' constants in the scope
    }

    // the increment clause
    if(!prs_matchtoken(vm, TOKEN_RIGHT_PAREN))// if there is something else before the terminating ')'
    {
        /*	INCEREMENT CLAUSE
		1. from the condition clause, first jump OVER the increment, to the body
		2. in the body, run the body
		3. jump BACK to the increment and run it
		4. from the increment jump BACK to the CONDITION clause, back to the cycle
		*/

        // for continue


        int bodyJump = prs_emitjump(vm, OP_JUMP);// jump the increment clause

        int incrementStart = prs_currentchunk(vm)->count;// starting index for increment

        // set continue jump here, right after the increment statement
        prs_markcontjump(vm);

        prs_expression(vm);// run the for expression
        prs_emitbyte(vm, OP_POP);// vm_pop expression constant
        prs_consume(vm, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        // running the loop
        prs_emitloop(vm, loopStart);// goes back to the start of the CONDITION clause of the for loop
        loopStart = incrementStart;
        prs_patchjump(vm, bodyJump);
    }

    prs_statement(vm);// running the code inside the loop

    prs_emitloop(vm, loopStart);

    // patch the jump in the loop body
    if(exitJump != -1)
    {
        prs_patchjump(vm, exitJump);
        prs_emitbyte(vm, OP_POP);// only vm_pop when THERE EXISTS A CONDITION from the clause
    }

    // patch break jumps, if available
    prs_patchbreakjumps(vm);

    prs_endloopscope(vm);
    prs_endscope(vm);
}

void prs_whilestmt(VM* vm)
{
    int loopStart = prs_currentchunk(vm)->count;// index where the statement to loop starts
    prs_beginloopscope(vm);

    // set jump for potential continue statement
    prs_markcontjump(vm);

    prs_expression(vm);


    int exitJump = prs_emitjump(vm, OP_JUMP_IF_FALSE);// skip stament if condition is false

    prs_emitbyte(vm, OP_POP);// vm_pop the last expression(true or false)

    prs_statement(vm);

    prs_emitloop(vm, loopStart);// method to 'loop' the instruction

    prs_patchjump(vm, exitJump);

    prs_emitbyte(vm, OP_POP);

    // patch break jumps, if available
    prs_patchbreakjumps(vm);

    prs_endloopscope(vm);
}

void prs_breakstmt(VM* vm)
{
    if(current->loopCountTop < 0)
    {
        prs_error(vm, "Break statement must be enclosed in a loop");
        return;
    }

    if(++current->breakJumpCounts[current->loopCountTop] > UINT8_COUNT)
    {
        prs_error(vm, "Too many break statments in one loop");
        return;
    }

    int breakJump = prs_emitjump(vm, OP_JUMP);
    int loopDepth = current->loopCountTop;
    int breakAmount = current->breakJumpCounts[loopDepth];
    current->breakPatchJumps[current->loopCountTop][breakAmount - 1] = breakJump;

    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after break.");
}


void prs_continuestmt(VM* vm)
{
    if(current->loopCountTop < 0)
    {
        prs_error(vm, "Continue statement must be enclosed in a loop");
        return;
    }

    if(current->loopCountTop == current->continueJumpCapacity)
    {
        int oldCapacity = current->continueJumpCapacity;
        current->continueJumpCapacity = GROW_CAPACITY(vm, oldCapacity);
        current->continueJumps = GROW_ARRAY(vm, int, current->continueJumps, oldCapacity, current->continueJumpCapacity);
    }

    prs_emitloop(vm, current->continueJumps[current->loopCountTop]);

    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after continue.");
}

void prs_repeatuntilstmt(VM* vm)
{
    // prs_consume(vm, TOKEN_LEFT_BRACE, "Expect '{' after repeat.");
    int loopStart = prs_currentchunk(vm)->count;
    prs_beginloopscope(vm);
    prs_markcontjump(vm);

    // process the statement
    prs_statement(vm);

    prs_consume(vm, TOKEN_UNTIL, "Expect 'until' after repeat statement.");

    // get true or false
    prs_expression(vm);

    // emit loop if false op code
    prs_emitcondloop(vm, loopStart, false);

    // patch possible break jumps
    prs_patchbreakjumps(vm);

    prs_endloopscope(vm);
    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after until exppression.");
}

void prs_dowhilestmt(VM* vm)
{
    int loopStart = prs_currentchunk(vm)->count;
    prs_beginloopscope(vm);
    prs_markcontjump(vm);

    // process the statement
    prs_statement(vm);

    prs_consume(vm, TOKEN_WHILE, "Expect 'until' after repeat statement.");

    // get true or false
    prs_expression(vm);

    // emit loop if true op code
    prs_emitcondloop(vm, loopStart, true);

    // patch possible break jumps
    prs_patchbreakjumps(vm);

    prs_endloopscope(vm);
    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after until exppression.");
}

void prs_synchronize(VM* vm)
{
    parser.panicMode = false;

    //printf("panic mode");

    // basically turn off the 'error' mode and skips token until something that looks like a statement boundary is found
    // skips tokens indiscriminately until somehing that looks like a statement boundary(eg. semicolon) is found
    while(parser.current.type != TOKEN_EOF)
    {
        if(parser.previous.type == TOKEN_SEMICOLON)
            return;

        switch(parser.current.type)
        {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
                return;
            default:// do nothing
            ;
        }

        prs_advance(vm);
    }
}

void prs_declaration(VM* vm)
{
    if(prs_matchtoken(vm, TOKEN_CLASS))
    {
        prs_classdecl(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_FUN))
    {
        prs_funcdecl(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_VAR))
    {
        prs_vardecl(vm);// declare variable
    }
    else
    {
        prs_statement(vm);
    }
    if(parser.panicMode)
        prs_synchronize(vm);// for errors
}

void prs_statement(VM* vm)// either an expression or a print
{
    if(prs_matchtoken(vm, TOKEN_RETURN))
    {
        prs_returnstmt(vm);// for functions return
    }
    else if(prs_matchtoken(vm, TOKEN_WHILE))
    {
        prs_whilestmt(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_FOR))
    {
        prs_forstmt(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_SWITCH))
    {
        prs_switchstmt(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_BREAK))
    {
        prs_breakstmt(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_CONTINUE))
    {
        prs_continuestmt(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_IF))
    {
        prs_ifstmt(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_REPEAT))
    {
        prs_repeatuntilstmt(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_DO))
    {
        prs_dowhilestmt(vm);
    }
    else if(prs_matchtoken(vm, TOKEN_LEFT_BRACE))// parse initial { token
    {
        prs_beginscope(vm);
        prs_block(vm);
        prs_endscope(vm);
    }
    else
    {
        prs_exprstmt(vm);
    }
}

ObjFunction* prs_compile(VM* vm, const char* source)
{
    scanner_init(source);// start scan/lexing
    Compiler compiler;
    prs_initcompiler(vm, &compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    prs_advance(vm);// vm_call to advance once to 'pump' the g_scanner

    while(!prs_matchtoken(vm, TOKEN_EOF))/// while EOF token is not met
    {
        prs_declaration(vm);
    }
    ObjFunction* function = prs_endcompiler(vm);// ends the expression with a return type
    return parser.hadError ? NULL : function;// if no error return true
}


// marking compiler roots, for garbage collection
void prs_markroots(VM* vm)
{
    Compiler* compiler = current;
    while(compiler != NULL)
    {
        mem_markobject(vm, (Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}

Chunk* prs_currentchunk(VM* vm)
{
    (void)vm;
    return &current->function->chunk;
}

// to handle syntax errors
void prs_errorat(VM* vm, Token* token, const char* message)
{
    (void)vm;
    if(parser.panicMode)
        return;// if an error already exists, no need to run other errors
    parser.panicMode = true;

    fprintf(stderr, "Error at [Line %d]", token->line);

    if(token->type == TOKEN_EOF)
    {
        fprintf(stderr, " at end");
    }
    else if(token->type == TOKEN_ERROR)
    {
        // nothing
    }
    else
    {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

// error from token most recently CONSUMED
void prs_error(VM* vm, const char* message)
{
    prs_errorat(vm, &parser.previous, message);
}


// handling error from token, the most current one being handed, not yet consumed
void prs_erroratcurrent(VM* vm, const char* message)// manually provide the message
{
    prs_errorat(vm, &parser.current, message);// pass in the current parser
}


/* main compile functions */

// pump the compiler, basically go to / 'read' the next token, a SINGLE token
void prs_advance(VM* vm)
{
    parser.previous = parser.current;//  store next parser as current

    for(;;)
    {
        parser.current = scanner_scantoken();// gets next token, stores it for later use(the next scan)

        if(parser.current.type != TOKEN_ERROR)
            break;// if error is not found break

        prs_erroratcurrent(vm, parser.current.start);// start is the location/pointer of the token source code
    }
}


// advance while skipping the given parameter, give none to skip nothing
void prs_advancewhileskipping(VM* vm, TokenType type)
{
    parser.previous = parser.current;//  store next parser as current

    for(;;)
    {
        parser.current = scanner_scantoken();// gets next token, stores it for later use(the next scan)

        if(parser.current.type == type)
            continue;

        if(parser.current.type != TOKEN_ERROR)
            break;// if error is not found break

        prs_erroratcurrent(vm, parser.current.start);// start is the location/pointer of the token source code
    }
}


// SIMILAR to advance but there is a validation for a certain type
// syntax error comes from here, where it is known/expected what the next token will be
void prs_consume(VM* vm, TokenType type, const char* message)
{
    if(parser.current.type == type)// if current token is equal to the token type being compared to
    {
        prs_advance(vm);
        return;
    }

    prs_erroratcurrent(vm, message);// if consumes a different type, error
}

bool prs_check(VM* vm, TokenType type)
{
    (void)vm;
    return parser.current.type == type;// check if current matches given
}


bool prs_matchtoken(VM* vm, TokenType type)
{
    if(!prs_check(vm, type))
        return false;
    prs_advance(vm);
    return true;
}

/* emitting BYTECODE for the VM to understand */
// the chunk_write for the compiler
void prs_emitbyte(VM* vm, uint8_t byte)
{
    chunk_write(vm, prs_currentchunk(vm), byte, parser.previous.line);// sends previous line so runtime errors are associated with that line
}

// write chunk for multiple chunks, used to write an opcode followed by an operand(eg. in constants)
void prs_emitbytes(VM* vm, uint8_t byte1, uint8_t byte2)
{
    prs_emitbyte(vm, byte1);
    prs_emitbyte(vm, byte2);
}

// for looping statements
void prs_emitloop(VM* vm, int loopStart)
{
    prs_emitbyte(vm, OP_LOOP);

    // int below jumps back, + 2 accounting the OP_LOOP and the instruction's own operand
    int offset = prs_currentchunk(vm)->count - loopStart + 2;
    if(offset > UINT16_MAX)
        prs_error(vm, "Loop body too large.");

    prs_emitbyte(vm, (offset >> 8) & 0xff);
    prs_emitbyte(vm, offset & 0xff);
}

void prs_emitcondloop(VM* vm, int loopStart, bool state)
{
    if(state)
        prs_emitbyte(vm, OP_LOOP_IF_TRUE);
    else
        prs_emitbyte(vm, OP_LOOP_IF_FALSE);

    int offset = prs_currentchunk(vm)->count - loopStart + 2;
    if(offset > UINT16_MAX)
        prs_error(vm, "Loop body too large.");

    prs_emitbyte(vm, (offset >> 8) & 0xff);
    prs_emitbyte(vm, offset & 0xff);
}


int prs_emitjump(VM* vm, uint8_t instruction)
{
    /* backpatching */
    prs_emitbyte(vm, instruction);// writes a placeholder operand for jump offset
    prs_emitbyte(vm, 0xff);// hexadecimal number with value of 255
    prs_emitbyte(vm, 0xff);

    // basically, get the difference in bytes before the two 0xff is added
    return prs_currentchunk(vm)->count - 2;
}

//  emit specific return type
void prs_emitreturn(VM* vm)
{
    if(current->type == TYPE_INITIALIZER)// class constructor
    {
        prs_emitbytes(vm, OP_GET_LOCAL, 0);// return the instance
    }
    else
    {
        prs_emitbyte(vm, OP_NULL);// for functions that return nothing
    }

    prs_emitbyte(vm, OP_RETURN);// emit return type at the end of a compiler
}

// to insert into constant table
uint8_t prs_makeconst(VM* vm, Value value)
{
    int constant = chunk_addconstant(vm, prs_currentchunk(vm), value);
    if(constant > UINT8_MAX)
    {
        prs_error(vm, "Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;// return as byte, the byte being the INDEX of the constantin the constats array
}

void prs_emitconst(VM* vm, Value value)// for constant emit the opcode, then the index
{
    prs_emitbytes(vm, OP_CONSTANT, prs_makeconst(vm, value));// add value to constant table
}

void prs_patchjump(VM* vm, int offset)
{
    // - 2 to adjust for the jump offset itself
    int jump = prs_currentchunk(vm)->count - offset - 2;

    if(jump > UINT16_MAX)
    {
        prs_error(vm, "Too much code to jump over.");
    }

    // the prs_patchjump provides the VALUE or amount to JUMP
    prs_currentchunk(vm)->code[offset] = (jump >> 8) & 0xff;// right shift by 8, then bitwise AND with 255(oxff is 111111)
    prs_currentchunk(vm)->code[offset + 1] = jump & 0xff;// only AND
}

// initialize the compiler
void prs_initcompiler(VM* vm, Compiler* compiler, FunctionType type)
{
    compiler->enclosing = current;// the 'outer' compiler
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = obj_mkfunction(vm);
    current = compiler;// current is the global variable pointer for the Compiler struct, point to to the parameter
    // basically assign the global pointer

    // for functions
    if(type != TYPE_SCRIPT)
    {
        current->function->name = obj_copystring(vm, parser.previous.start, parser.previous.length);// function name handled here
    }

    // compiler implicitly claims slot zero for local variables
    Local* local = &current->locals[current->localCount++];
    local->isCaptured = false;

    // for this tags
    if(type != TYPE_FUNCTION)// for none function types, for class methods
    {
        local->name.start = "this";
        local->name.length = 4;
    }
    else// for functions
    {
        local->name.start = "";
        local->name.length = 0;
    }

    // for loop scopes, for break and continue statements
    compiler->loopCountTop = -1;
    compiler->continueJumpCapacity = 4;
    compiler->continueJumps = ALLOCATE(vm, int, 4);

    // use memset to initialize array to 0
    memset(compiler->breakJumpCounts, 0, UINT8_COUNT * sizeof(compiler->breakJumpCounts[0]));
}

ObjFunction* prs_endcompiler(VM* vm)
{
    prs_emitreturn(vm);
    ObjFunction* function = current->function;

    FREE(vm, int, current->continueJumps);


    // for debugging
#ifdef DEBUG_PRINT_CODE
    if(!parser.hadError)
    {
        dbg_disasmchunk(vm, prs_currentchunk(vm), function->name != NULL ? function->name->chars : "<script>");// if name is NULL then it is the Script type(main()
    }
#endif

    current = current->enclosing;// return back to enclosing compiler after function
    return function;// return to free
}

void prs_beginscope(VM* vm)
{
    (void)vm;
    current->scopeDepth++;
}

void prs_endscope(VM* vm)
{
    current->scopeDepth--;

    // remove variables out of scope
    while(current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth)
    {
        /* at the end of a block scope, when the compiler emits code to free the stack slot for the locals, 
		tell which one to hoist to the heap
		*/
        if(current->locals[current->localCount - 1].isCaptured)// if it is captured/used
        {
            prs_emitbyte(vm, OP_CLOSE_UPVALUE);// op code to move the upvalue to the heap
        }
        else
        {
            prs_emitbyte(vm, OP_POP);// if not used anymore/capture simply vm_pop the value off the stack
        }

        current->localCount--;
    }
}

// loop enclosing
void prs_beginloopscope(VM* vm)
{
    (void)vm;
    current->loopCountTop++;
}

void prs_endloopscope(VM* vm)
{
    (void)vm;
    if(current->breakJumpCounts[current->loopCountTop] > 0)
        current->breakJumpCounts[current->loopCountTop] = 0;

    current->loopCountTop--;
}

// mark current chunk for continue jump
void prs_markcontjump(VM* vm)
{
    current->continueJumps[current->loopCountTop] = prs_currentchunk(vm)->count;
}

// patch available break jumps
void prs_patchbreakjumps(VM* vm)
{
    for(int i = 0; i < current->breakJumpCounts[current->loopCountTop]; i++)
    {
        prs_patchjump(vm, current->breakPatchJumps[current->loopCountTop][i]);
    }
}

/* variable declarations */
uint8_t prs_makeconstident(VM* vm, Token* name)
{
    return prs_makeconst(vm, OBJ_VAL(obj_copystring(vm, name->start, name->length)));// add to constant table
}

bool prs_identequal(VM* vm, Token* a, Token* b)
{
    (void)vm;
    if(a->length != b->length)
        return false;
    return memcmp(a->start, b->start, a->length) == 0;
}


int prs_resolvelocal(VM* vm, Compiler* compiler, Token* name)
{
    for(int i = compiler->localCount - 1; i >= 0; i--)// walk through the local variables
    {
        Local* local = &compiler->locals[i];
        if(prs_identequal(vm, name, &local->name))
        {
            if(local->depth == -1)
            {
                prs_error(vm, "Cannot read local variable in its own initializer.");
            }
            return i;// found the var, return the index
        }
    }

    return -1;// not found, name is global variable
}


// add upvalue
int prs_addupvalue(VM* vm, Compiler* compiler, uint8_t index, bool isLocal)
{
    int upvalueCount = compiler->function->upvalueCount;// get current upvalue count

    // check whether the upvalue has already been declared
    for(int i = 0; i < upvalueCount; i++)
    {
        Upvalue* upvalue = &compiler->upvalues[i];// get pointer for each upvalue in the array
        if(upvalue->index == index && upvalue->isLocal == isLocal)
        {
            return i;// if found, return the index of the upvalue in the upvalue array
        }
    }

    if(upvalueCount == UINT8_COUNT)
    {
        prs_error(vm, "Too many closure variables");
        return 0;
    }

    // compiler keeps an array of upvalue structs to track closed-over identifiers
    // indexes in the array match the indexes of ObjClosure at runtime
    // insert to upvalues array
    compiler->upvalues[upvalueCount].isLocal = isLocal;// insert bool status
    compiler->upvalues[upvalueCount].index = index;// insert index
    return compiler->function->upvalueCount++;// increase count and return
}


/*	for closures
- prs_resolveupvalue looks for a local variable declared in any of the surrounding functions
- if it finds one it returns the index for that upvalue variable, otherwise returns -1
*/
int prs_resolveupvalue(VM* vm, Compiler* compiler, Token* name)
{
    if(compiler->enclosing == NULL)
        return -1;// if in main()

    int local = prs_resolvelocal(vm, compiler->enclosing, name);// looks for local value in enclosing function/compiler
    if(local != -1)
    {
        compiler->enclosing->locals[local].isCaptured = true;// mark local is captured/used by and upvalue
        return prs_addupvalue(vm, compiler, (uint8_t)local, true);// create up value
    }

    // recursion to solve nested upvalues
    // recursive vm_call right in the middle
    int upvalue = prs_resolveupvalue(vm, compiler->enclosing, name);// if the enclosing function is main() (NULL), it returns -1
    if(upvalue != -1)
    {
        return prs_addupvalue(vm, compiler, (uint8_t)upvalue, true);
    }


    return -1;
}


void prs_addlocal(VM* vm, Token name)
{
    if(current->localCount == UINT8_COUNT)
    {
        prs_error(vm, "Too many local variables in block.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;// for cases where a variable name is redefined inside another scope, using the variable itself
    local->isCaptured = false;
}

void prs_declarevariable(VM* vm)// for local variables
{
    int i;
    Local* local;
    Token* name;
    // global vars are implicitly declared, and are late bound, not 'initialized' here but in the VM
    if(current->scopeDepth == 0)
        return;

    /* local variable declaration happens below */
    name = &parser.previous;

    // to not allow two variable declarations to have the same name
    // loop only checks to a HIGHER SCOPE; another block overlaping/shadowing is allowed
    // work backwards
    for(i = current->localCount - 1; i >= 0; i--)
    {
        local = &current->locals[i];
        if(local->depth != -1 && local->depth < current->scopeDepth)// if reach beginning of array(highest scope)
        {
            break;
        }
        if(prs_identequal(vm, name, &local->name))
        {
            prs_error(vm, "Variable with this name exists in scope.");
        }
    }

    prs_addlocal(vm, *name);
}

uint8_t prs_parsevariable(VM* vm, const char* errorMessage)
{
    prs_consume(vm, TOKEN_IDENTIFIER, errorMessage);// requires next token to be an identifier

    prs_declarevariable(vm);
    if(current->scopeDepth > 0)
        return 0;// if scopeDepth is not 0, then it is a local not global var
    // return a dummy index
    // at runtime, locals are not looked up by name so no need to insert them to a table


    return prs_makeconstident(vm, &parser.previous);// return index from the constant table
}


void prs_markinitialized(VM* vm)
{
    (void)vm;
    if(current->scopeDepth == 0)
        return;// if global return
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

void prs_definevariable(VM* vm, uint8_t global)
{
    if(current->scopeDepth > 0)
    {
        prs_markinitialized(vm);
        return;
    }

    prs_emitbytes(vm, OP_DEFINE_GLOBAL, global);// opcode for declaration and the constant itself
}


// for function arguments, returns number of arguments
// each argument expression generates code which leaves value on the stack in preparation for the vm_call
uint8_t prs_parsearglist(VM* vm)
{
    uint8_t argCount = 0;
    if(!prs_check(vm, TOKEN_RIGHT_PAREN))// if ) has not been reached
    {
        do
        {
            prs_expression(vm);// collect the arguments

            if(argCount == 255)// cannot have more than 255 arguments as each operand is a single byte(uint8_t)
            {
                prs_error(vm, "Cannot have more than 255 arguments.");
            }

            argCount++;
        } while(prs_matchtoken(vm, TOKEN_COMMA));
    }

    prs_consume(vm, TOKEN_RIGHT_PAREN, "Expect ')' after argument list.");
    return argCount;
}

void rule_and(VM* vm, bool canAssign)
{
    (void)canAssign;
    int endJump = prs_emitjump(vm, OP_JUMP_IF_FALSE);// left hand side is already compiled,
    // and if it is false skip it and go to next

    prs_emitbyte(vm, OP_POP);
    prs_parseprecedence(vm, PREC_AND);
    prs_patchjump(vm, endJump);
}


// for binary, eg. 5 + 4
// or INFIX parser, where the operator is in the middle
// entire left hand expression has been compiled, and the infix operator has been consumed
// rule_binary() handles the rest of the arithmetic operator
void rule_binary(VM* vm, bool canAssign)
{
    (void)canAssign;
    // remember type of operator, already consumed
    TokenType operatorType = parser.previous.type;

    // compile right operand
    ParseRule* rule = prs_getrule(vm, operatorType);// the BIDMAS rule, operands in the right side have HIGHER PRECEDENCE
    // as binary operators are LEFT ASSOCIATIVE
    // recursively vm_call prs_parseprecedence again
    prs_parseprecedence(vm, (Precedence)(rule->precedence + 1));// conert from rule to enum(precedence) type

    switch(operatorType)
    {
            // note how NOT opcode is at the end
            // six binary operators for three instructions only(greater, not, equal)
        case TOKEN_BANG_EQUAL:
            prs_emitbytes(vm, OP_EQUAL, OP_NOT);
            break;// add equal and not to the stack
        case TOKEN_EQUAL_EQUAL:
            prs_emitbyte(vm, OP_EQUAL);
            break;
        case TOKEN_GREATER:
            prs_emitbyte(vm, OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            prs_emitbytes(vm, OP_LESS, OP_NOT);
            break;
        case TOKEN_LESS:
            prs_emitbyte(vm, OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            prs_emitbytes(vm, OP_GREATER, OP_NOT);
            break;

        case TOKEN_PLUS:
            prs_emitbyte(vm, OP_ADD);
            break;
        case TOKEN_MINUS:
            prs_emitbyte(vm, OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            prs_emitbyte(vm, OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            prs_emitbyte(vm, OP_DIVIDE);
            break;
        case TOKEN_MODULO:
            prs_emitbyte(vm, OP_MODULO);
            break;
        default:
            return;// unreachable
    }
}


// for function calls
void rule_parsecall(VM* vm, bool canAssign)
{
    (void)canAssign;
    // again, assumes the function itself(its vm_call name) has been placed on the codestream stack
    uint8_t argCount = prs_parsearglist(vm);// compile arguments using prs_parsearglist
    prs_emitbytes(vm, OP_CALL, argCount);// write on the chunk
}

// class members/fields/properties
void rule_dot(VM* vm, bool canAssign)
{
    prs_consume(vm, TOKEN_IDENTIFIER, "Expect propery name after class instance.");
    uint8_t name = prs_makeconstident(vm, &parser.previous);// already consumed

    if(canAssign && prs_matchtoken(vm, TOKEN_EQUAL))// assignment
    {
        prs_expression(vm);// evalute expression to be set
        prs_emitbytes(vm, OP_SET_PROPERTY, name);
    }
    else if(prs_matchtoken(vm, TOKEN_LEFT_PAREN))// for running class methods, access the method and vm_call it at the same time
    {
        uint8_t argCount = prs_parsearglist(vm);

        /* new OP_INVOKE opcode that takes two operands:
		1. the index of the property name in the constant table
		2. the number of arguments passed in the methods
		*** combines OP_GET_PROPERTY and OP_CALL
		*/
        prs_emitbytes(vm, OP_INVOKE, name);
        prs_emitbyte(vm, argCount);
    }
    else// simply get
    {
        prs_emitbytes(vm, OP_GET_PROPERTY, name);
    }
}

void rule_literal(VM* vm, bool canAssign)
{
    (void)canAssign;
    switch(parser.previous.type)
    {
        case TOKEN_FALSE:
            prs_emitbyte(vm, OP_FALSE);
            break;
        case TOKEN_TRUE:
            prs_emitbyte(vm, OP_TRUE);
            break;
        case TOKEN_NULL:
            prs_emitbyte(vm, OP_NULL);
            break;

        default:// unreachable
            return;
    }
}

// parentheses for rule_grouping
void rule_grouping(VM* vm, bool canAssign)
{
    (void)canAssign;
    // assume initial ( has already been consumed, and recursively vm_call to expression() to compile between the parentheses
    prs_expression(vm);
    prs_consume(vm, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");// expects a right parentheses, if not received then  error
}


/* parsing the tokens */
void rule_number(VM* vm, bool canAssign)
{
    (void)canAssign;
    // strtod below converts from string to double
    // assume that token for the number literal has already been consumed and is stored in previous
    // double strtod(const char* str, char** endptr)
    // endptr is the first non-double character after teh char* str character; if none then null
    /*	The way it works:
	-> in scanner, if a digit exists after a digit, it advances() (skips) the current
	-> hence, we get that the start points to the START of the digit, and using strtod smartly it reaches until the last digit
	*/
    double value = strtod(parser.previous.start, NULL);
    //printf("num %c\n", *parser.previous.start);
    prs_emitconst(vm, NUMBER_VAL(value));
}

void rule_or(VM* vm, bool canAssign)
{
    (void)canAssign;
    // jump if left hand side is true
    int elseJump = prs_emitjump(vm, OP_JUMP_IF_FALSE);// if left is false jump directly to right hand
    int endJump = prs_emitjump(vm, OP_JUMP);// if not skipped(as left is true) jump the right hand

    prs_patchjump(vm, elseJump);
    prs_emitbyte(vm, OP_POP);

    prs_parseprecedence(vm, PREC_OR);
    prs_patchjump(vm, endJump);
}

// 'initialize' the string here
void rule_string(VM* vm, bool canAssign)
{
    (void)canAssign;
    // in a string, eg. "hitagi", the quotation marks are trimmed
    prs_emitconst(vm, OBJ_VAL(obj_copystring(vm, parser.previous.start + 1, parser.previous.length - 2)));
}

// declare/vm_call variables
void rule_namedvar(VM* vm, Token name, bool canAssign)
{
    uint8_t getOp, setOp;
    int arg = prs_resolvelocal(vm, current, &name);// try find a local variable with a given name
    if(arg != -1)
    {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    }
    else if((arg = prs_resolveupvalue(vm, current, &name)) != -1)// for upvalues
    {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    }
    else
    {
        arg = prs_makeconstident(vm, &name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }


    // test case to check whether it is a get(just the name) or a reassignment
    if(canAssign && prs_matchtoken(vm, TOKEN_EQUAL))// if a = follows right after
    {
        prs_expression(vm);
        prs_emitbytes(vm, setOp, (uint8_t)arg);// reassignment/set
    }
    else
    {
        prs_emitbytes(vm, getOp, (uint8_t)arg);// as normal get
        // printf("gest");
    }
}


void rule_variable(VM* vm, bool canAssign)
{
    rule_namedvar(vm, parser.previous, canAssign);
}

// for super classes, token that mimics as if a user types in 'super'
Token prs_makesyntoken(VM* vm, const char* text)
{
    Token token;
    (void)vm;
    token.start = text;
    token.length = (int)strlen(text);// strlen to get char* length
    return token;
}

// for super calls
void rule_super(VM* vm, bool canAssign)
{
    (void)canAssign;
    // if token is not inside a class
    if(currentClass == NULL)
    {
        prs_error(vm, "'super' can only be initialized inside a class.");
    }
    else if(!currentClass->hasSuperclass)// if class has no parent class
    {
        prs_error(vm, "'super' cannot be used on a class with no parent class.");
    }


    prs_consume(vm, TOKEN_DOT, "Expect '.' after 'super'.");
    prs_consume(vm, TOKEN_IDENTIFIER, "Expect parent class method identifier.");
    uint8_t name = prs_makeconstident(vm, &parser.previous);// get identifier index


    /*
	in order to access a superclass method on the CURRENT INSTANCE, runtime needs both the receiver and the superclass
	of the surrounding method's class.
	1. first rule_namedvar vm_call generates code to look up the current receiver and vm_push it to the stack
	2. second rule_namedvar emits code to look up the superclass and vm_push that on top
	*/
    rule_namedvar(vm, prs_makesyntoken(vm, "this"), false);
    if(prs_matchtoken(vm, TOKEN_LEFT_PAREN))// if there is a parameter list, vm_invoke super method
    {
        uint8_t argCount = prs_parsearglist(vm);
        rule_namedvar(vm, prs_makesyntoken(vm, "super"), false);
        prs_emitbytes(vm, OP_SUPER_INVOKE, name);// super vm_invoke opcode
        prs_emitbyte(vm, argCount);
    }
    else
    {
        rule_namedvar(vm, prs_makesyntoken(vm, "super"), false);
        prs_emitbytes(vm, OP_GET_SUPER, name);
    }
}


// for class methods
void rule_this(VM* vm, bool canAssign)
{
    (void)canAssign;
    // if not inside a class
    if(currentClass == NULL)
    {
        prs_error(vm, "Cannot use 'this' outside of class.");
        return;
    }

    rule_variable(vm, false);// always false
}

// unary
void rule_unary(VM* vm, bool canAssign)
{
    (void)canAssign;
    TokenType operatorType = parser.previous.type;// leading - token has already been consumed

    // compile operand
    prs_expression(vm);

    switch(operatorType)
    {
        case TOKEN_BANG:
            prs_emitbyte(vm, OP_NOT);
            break;


            // OP_NEGATE should be emitted last, AFTER the constant itself
            // eg. say 4 - 5; 5 needs to be emitted and added to the chunk->constants first before OP_NEGATE
            /* it is important to take note of the precedence
		e.g -a.b + 3;
		when the unary negation is called, all of a.b + 3 will be consumed in expression(). Hence, a method is needed
		to STOP when + is found, or generally when an operand of LOWER PRECEDENCE is found
		*/
        case TOKEN_MINUS:
            prs_emitbyte(vm, OP_NEGATE);
            break;
        default:
            return;
    }
}

void vm_resetstack(VM* vm)
{
    // point stackStop to the begininng of the empty array
    vm->stackTop = vm->stack;// stack array(vm->stack) is already indirectly declared, hence no need to allocate memory for it
    vm->frameCount = 0;
    vm->openUpvalues = NULL;
}


// IMPORTANT
// variadic function ( ... ), takes a varying number of arguments
void vm_rterror(VM* vm, const char* format, ...)
{
    va_list args;// list from the varying parameter
    va_start(args, format);
    vfprintf(stderr, format, args);// unlike book, not vprintf(stderr, format, args)
    va_end(args);
    fputs("\n", stderr);// fputs; write a string to the stream but not including the null character


    // printing the stack trace for the function
    // print out each function that was still executing when the program died and where the execution was at the point it died
    for(int i = vm->frameCount - 1; i >= 0; i--)
    {
        CallFrame* frame = &vm->frames[i];
        ObjFunction* function = frame->closure->function;
        // - 1 because IP is sitting on the NEXT INSTRUCTION to be executed
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if(function->name == NULL)
        {
            fprintf(stderr, "script\n");
        }
        else
        {
            fprintf(stderr, "%s(%d)\n", function->name->chars, function->arity);
        }
    }


    // tell which line the error occurred
    CallFrame* frame = &vm->frames[vm->frameCount - 1];// pulls from topmost CallFrame on the stack
    size_t instruction = frame->ip - frame->closure->function->chunk.code - 1;// - 1 to deal with the 1 added initially for the main() CallFrame
    int line = frame->closure->function->chunk.lines[instruction];
    fprintf(stderr, "Error in script at [Line %d]\n", line);

    vm_resetstack(vm);
}

void vm_defnative(VM* vm, const char* name, NativeFn function)
{
    vm_push(vm, OBJ_VAL(obj_copystring(vm, name, (int)strlen(name))));// strlen to get char* length
    vm_push(vm, OBJ_VAL(obj_mknative(vm, function)));
    htable_set(vm, &vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
    vm_pop(vm);
    vm_pop(vm);
}

Value cfn_clock(VM* vm, int argc, Value* args)
{
    (void)vm;
    (void)argc;
    (void)args;
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);// returns elapsed time since program was running
}

Value cfn_print(VM* vm, int argc, Value* args)
{
    int i;
    Obj* o;
    ObjString* os;
    (void)vm;
    (void)argc;
    for(i=0; i<argc; i++)
    {
        switch(args[i].type)
        {
            case VAL_BOOL:
                fprintf(stdout, "%s", args[i].as.boolean ? "true" : "false");
                break;
            case VAL_NULL:
                fprintf(stdout, "null");
                break;
            case VAL_NUMBER:
                fprintf(stdout, "%g", args[i].as.number);
                break;
            case VAL_OBJ:
                {
                    o = args[i].as.obj;
                    if(IS_STRING(args[i]))
                    {
                        os = AS_STRING(args[i]);
                        fprintf(stdout, "%.*s", os->length, os->chars);
                    }
                    else
                    {
                        fprintf(stdout, "<obj %d>", o->type);
                    }
                }
                break;
            default:
                {
                    fprintf(stdout, "<val %d>", args[i].type);
                }
                break;
        }
        fflush(stdout);
    }
    return NUMBER_VAL(0);
}

Value cfn_println(VM* vm, int argc, Value* args)
{
    Value r;
    r = cfn_print(vm, argc, args);
    fprintf(stdout, "\n");
    fflush(stdout);
    return r;
}

Value cfn_chr(VM* vm, int argc, Value* args)
{
    char buf[10];
    if(argc > 0)
    {
        if(IS_NUMBER(args[0]))
        {
            buf[0] = args[0].as.number;
            buf[1] = 0;
            return OBJ_VAL(obj_copystring(vm, buf, 1));
        }
        vm_rterror(vm, "expected #1 to be number");
    }
    vm_rterror(vm, "too few arguments");
    return NULL_VAL;
}


void vm_init(VM* vm)
{
    vm_resetstack(vm);// initialiing the Value stack, also initializing the callframe count
    vm->objects = NULL;
    htable_init(vm, &vm->globals);
    htable_init(vm, &vm->strings);

    // initializing gray marked obj stack for garbage collection
    vm->grayCapacity = 0;
    vm->grayCount = 0;
    vm->grayStack = NULL;

    // self adjusting heap to control frequency of GC
    vm->bytesAllocated = 0;
    vm->nextGC = 1024 * 1024;

    // init initalizer string
    vm->initString = NULL;
    vm->initString = obj_copystring(vm, "init", 4);

    vm_defnative(vm, "clock", cfn_clock);
    vm_defnative(vm, "print", cfn_print);
    vm_defnative(vm, "println", cfn_println);
    vm_defnative(vm, "chr", cfn_chr);
}

void vm_free(VM* vm)
{
    vm->initString = NULL;
    mem_freeobjlist(vm);// free all objects, from vm->objects
    htable_free(vm, &vm->globals);
    htable_free(vm, &vm->strings);
}

/* stack operations */
void vm_push(VM* vm, Value value)
{
    *vm->stackTop = value;// * in front of the pointer means the rvalue itself, assign value(parameter) to it
    vm->stackTop++;
}

Value vm_pop(VM* vm)
{
    vm->stackTop--;// first move the stack BACK to get the last element(stackTop points to ONE beyond the last element)
    return *vm->stackTop;
}
/* end of stack operations */

// PEEK from the STACK, AFTER the compiler passes it through
// return a value from top of the stack but does not vm_pop it, distance being how far down
// this is a C kind of accessing arrays/pointers
Value vm_peek(VM* vm, int distance)
{
    return vm->stackTop[-1 - distance];
}

/* for vm_call stacks/functions  */
bool vm_call(VM* vm, ObjClosure* closure, int argCount)
{
    if(argCount != closure->function->arity)// if number of parameters does not match
    {
        vm_rterror(vm, "Expected %d arguments but got %d", closure->function->arity, argCount);
        return false;
    }

    // as CallFrame is an array, to ensure array does not overflow
    if(vm->frameCount == FRAMES_MAX)
    {
        vm_rterror(vm, "Stack overflow.");
        return false;
    }

    // get pointer to next in frame array
    CallFrame* frame = &vm->frames[vm->frameCount++];// initializes callframe to the top of the stack
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;

    // set up slots pointer to give frame its window into the stack
    // ensures everyting lines up
    // slots is the 'starting pointer' for the function cll
    frame->slots = vm->stackTop - argCount - 1;
    return true;
}

bool vm_callvalue(VM* vm, Value callee, int argCount)
{
    if(IS_OBJ(callee))
    {
        switch(OBJ_TYPE(callee))
        {
            case OBJ_BOUND_METHOD:
            {
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);// get ObjBoundMethod from value type(callee)
                vm->stackTop[-argCount - 1] = bound->receiver;// set [-] inside square brackes of top stack pointer to go down the stack
                return vm_call(vm, bound->method, argCount);//	run vm_call to execute
            }
            case OBJ_CLASS:// create class instance
            {
                ObjClass* kelas = AS_CLASS(callee);
                // create new instance here
                vm->stackTop[-argCount - 1] = OBJ_VAL(obj_mkinstance(vm, kelas));// - argcounts as above values are parameters

                // initializer
                Value initializer;
                // if we find one from the table
                if(htable_get(vm, &kelas->methods, vm->initString, &initializer))// have a vm->initString as 'token', ObjString type
                {
                    return vm_call(vm, AS_CLOSURE(initializer), argCount);
                }
                else if(argCount != 0)// if there ARE arguments but the initalizer method cannot be found
                {
                    vm_rterror(vm, "Expected 0  arguments but got %d\n", argCount);
                    return false;
                }

                return true;
            }
            case OBJ_CLOSURE:// ensure type is function
                return vm_call(vm, AS_CLOSURE(callee), argCount);// vm_call to function happens here

            case OBJ_NATIVE:
            {
                NativeFn native = AS_NATIVE(callee);
                Value result = native(vm, argCount, vm->stackTop - argCount);
                vm->stackTop -= argCount + 1;// remove vm_call and arguments from the stack
                vm_push(vm, result);
                return true;
            }
            default:
                break;
        }
    }

    vm_rterror(vm, "Non-function or non-class type is called.");
    return false;
}


bool vm_invokefromclass(VM* vm, ObjClass* kelas, ObjString* name, int argCount)
{
    Value method;
    if(!htable_get(vm, &kelas->methods, name, &method))
    {
        vm_rterror(vm, "Undefined property '%s'.", name->chars);
        return false;
    }

    return vm_call(vm, AS_CLOSURE(method), argCount);
}


// vm_invoke class method, access method + vm_call method
bool vm_invoke(VM* vm, ObjString* name, int argCount)
{
    Value receiver = vm_peek(vm, argCount);// grab the receiver of the stack

    // vm_call method with wrong type, not an objinstance type
    if(!IS_INSTANCE(receiver))
    {
        vm_rterror(vm, "Tried to vm_invoke a method from a non instance object.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    // for fields()
    Value value;
    if(htable_get(vm, &instance->fields, name, &value))
    {
        vm->stackTop[-argCount - 1] = value;
        return vm_callvalue(vm, value, argCount);
    }


    return vm_invokefromclass(vm, instance->kelas, name, argCount);// actual function that searches for method and calls it
}


// bind method and wrap it in a new ObjBoundMethod
bool vm_bindmethod(VM* vm, ObjClass* kelas, ObjString* name)
{
    Value method;
    if(!htable_get(vm, &kelas->methods, name, &method))// get method from table and bind it
    {
        // if method not found
        vm_rterror(vm, "Undefined property %s.", name->chars);
        return false;
    }
    ObjBoundMethod* bound = obj_mkboundmethod(vm, vm_peek(vm, 0), AS_CLOSURE(method));// wrap method in a new ObjBoundMethodd

    vm_pop(vm);// vm_pop the class instance
    vm_push(vm, OBJ_VAL(bound));
    return true;
}


// get corresponding upvalue
ObjUpvalue* vm_captureupvalue(VM* vm, Value* local)
{
    // set up the linked list
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm->openUpvalues;// assign at the start of the list

    // look for an existing upvalue in the list
    /*  LINKED LIST
	1. start at the head of the list, which is the upvalue CLOSET to the TOP OF THE STACK
	2. walk through the list, using a little pointer comparison to iterate past every upvalue pointing
		to slots ABOVE the one we are looking for
	-- upvalue->location (array of the indexes for the locals) is the stack
	
	THREE ways to exit the loop:
	1. local slot stopped is the slot we're looking for
	2. ran ouf ot upvalues to search
	3. found an upvalue whose local slot is BELOW the one we're looking for
	*/
    while(upvalue != NULL && upvalue->location > local)// pointer comparison: only find the ones ABOVE local
    {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if(upvalue != NULL && upvalue->location == local)// if the location/local/indeces match
    {
        return upvalue;// return already created upvalue
    }

    ObjUpvalue* createdUpvalue = obj_mkupvalue(vm, local);
    createdUpvalue->next = upvalue;// insert at the front

    if(prevUpvalue == NULL)// ran out of values to search
    {
        vm->openUpvalues = createdUpvalue;// set pointer to the newly added upvalue
    }
    else// found local slot BELOW the one we are looking for
    {
        prevUpvalue->next = createdUpvalue;// link next slot(the value below) to the newly inserted upvalue
    }

    return createdUpvalue;
}

// closes every upvalue it can find that points to the slot or any above the stack
void vm_closeupvalues(VM* vm, Value* last)// takes pointer to stack slot
{
    while(vm->openUpvalues != NULL && vm->openUpvalues->location >= last)
    {
        ObjUpvalue* upvalue = vm->openUpvalues;// pointer to list of openupvalues
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm->openUpvalues = upvalue->next;
    }
}

// defining method for class type
void vm_defmethod(VM* vm, ObjString* name)
{
    Value method = vm_peek(vm, 0);// method/closure is at the top of the stack
    ObjClass* kelas = AS_CLASS(vm_peek(vm, 1));// class is at the 2nd top
    htable_set(vm, &kelas->methods, name, method);// add to hashtable
    vm_pop(vm);// vm_pop the method
}


// comparison for OP_NOT
bool vm_isfalsey(VM* vm, Value value)
{
    (void)vm;
    // return true if value is the null type or if it is a false bool type
    bool test = IS_NULL(value) || (IS_BOOL(value) && !AS_BOOL(value));

    return test;
}

// string concatenation
void vm_concatenate(VM* vm)
{
    ObjString* second = AS_STRING(vm_peek(vm, 0));// vm_peek, so we do not vm_pop it off if calling a GC is needed
    ObjString* first = AS_STRING(vm_peek(vm, 1));

    int length = first->length + second->length;
    char* chars = ALLOCATE(vm, char, length + 1);// dynamically allocate memory for the char, chars is now a NULL string

    /* NOTE ON C STRINGS, NULL VS EMPTY
	-> null string has no elements, it is an empty charray, ONLY DECLARED
	-> an empty string has the null character '/0'
	*/

    // IMPORTANt -> use memcpy when assinging to a char* pointer
    memcpy(chars, first->chars, first->length);// memcpy function, copy to chars, from first->chars, with second->length number of bits
    memcpy(chars + first->length, second->chars, second->length);// remember to add the first length of bits to chars again, so it will START AFTER the given offset
    chars[length] = '\0';// IMPORTANT-> terminating character for Cstring, if not put will get n2222

    ObjString* result = obj_takestring(vm, chars, length);// declare new ObjString ptr
    vm_pop(vm);// vm_pop the two strings, garbage collection
    vm_pop(vm);
    vm_push(vm, OBJ_VAL(result));
}


/* starting point of the compiler */
InterpretResult vm_interpret(VM* vm, const char* source)
{
    ObjFunction* function = prs_compile(vm, source);
    if(function == NULL)
        return INTERPRET_COMPILE_ERROR;// NULL gets passed from compiler

    vm_push(vm, OBJ_VAL(function));
    ObjClosure* closure = obj_mkclosure(vm, function);
    vm_pop(vm);
    vm_push(vm, OBJ_VAL(closure));
    vm_callvalue(vm, OBJ_VAL(closure), 0);// 0 params for main()
    return vm_run(vm);
}


// run the chunk
// most IMPORTANT part of the interpreter
InterpretResult vm_run(VM* vm)// means the scope of the function is only to this file
{
    CallFrame* frame = &vm->frames[vm->frameCount - 1];


    for(;;)
    {
        // dbg_disasminst needs an byte offset, do pointer math to convert ip back to relative offset
        // from the beginning of the chunk (subtract current ip from the starting ip)
        // IMPORTANT -> only for debugging the VM

#ifdef DEBUG_TRACE_EXECUTION
        // for stack tracing
        printf("		");
        /* note on C POINTERSE
		-> pointing to the array itself means pointing to the start of the array, or the first element of the array
		-> ++/-- means moving through the array (by 1 or - 1)
		-> you can use operands like < > to tell compare how deep are you in the array
		*/

        // prints every existing value in the stack
        for(Value* slot = vm->stack; slot < vm->stackTop; slot++)
        {
            printf("[ ");
            obj_printvalue(vm, *slot);
            printf(" ]");
        }


        dbg_disasminst(vm, &frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code));
#endif
        uint8_t instruction;
        // get result of the byte read, every set of instruction starts with an opcode
        switch(instruction = vmc_readbyte())
        {
            case OP_CONSTANT:
            {
                // function is smart;
                // chunk advances by 1 on first read,
                // then in the vmc_readconst() macro it reads again which advances by 1 and returns the INDEX
                // READ the next line, which is the INDEX of the constant in the constants array
                Value constant = vmc_readconst();
                // vm_push to stack
                vm_push(vm, constant);
                // break from the switch
                break;
            }
            // unary opcode
            case OP_NEGATE:
                if(!IS_NUMBER(vm_peek(vm, 0)))// if next value is not a number
                {
                    //printf("\nnot a number\n"); it actually works
                    vm_rterror(vm, "Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                vm_push(vm, NUMBER_VAL(-AS_NUMBER(vm_pop(vm))));
                break;// negates the last element of the stack

            // literals
            case OP_NULL:
                vm_push(vm, NULL_VAL);
                break;
            case OP_TRUE:
                vm_push(vm, BOOL_VAL(true));
                break;
            case OP_FALSE:
                vm_push(vm, BOOL_VAL(false));
                break;

            // binary opcode
            case OP_ADD:
            {
                if(IS_STRING(vm_peek(vm, 0)) && IS_STRING(vm_peek(vm, 1)))// if last two constants are strings
                {
                    vm_concatenate(vm);
                }
                else if(IS_NUMBER(vm_peek(vm, 0)) && IS_NUMBER(vm_peek(vm, 1)))
                {
                    // in the book, macro is not used and a new algorithm is used directly
                    vmc_binaryop(NUMBER_VAL, +, double);// initialize new Value struct (NUMBER_VAL) here
                }
                else// handle errors dynamically here
                {
                    //printf("operands error");
                    vm_rterror(vm, "Operands are incompatible.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_SUBTRACT:
                vmc_binaryop(NUMBER_VAL, -, double);
                break;
            case OP_MULTIPLY:
                vmc_binaryop(NUMBER_VAL, *, double);
                break;
            case OP_DIVIDE:
                vmc_binaryop(NUMBER_VAL, /, double);
                break;

            case OP_MODULO:
                vmc_binaryop(NUMBER_VAL, %, int);
                break;

            case OP_NOT:
                vm_push(vm, BOOL_VAL(vm_isfalsey(vm, vm_pop(vm))));// again, pops most recent one from the stack, does the operation on it, and pushes it back
                break;

            // for switch eqal
            case OP_SWITCH_EQUAL:
            {
                Value b = vm_pop(vm);// only vm_pop second value
                Value a = vm_peek(vm, 0);// vm_peek topmost, the first value
                vm_push(vm, BOOL_VAL(obj_valequal(vm, a, b)));
                break;
            }

            case OP_EQUAL:// implemenation comparison done here
            {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                vm_push(vm, BOOL_VAL(obj_valequal(vm, a, b)));
                break;
            }
            case OP_GREATER:
                vmc_binaryop(BOOL_VAL, >, double);
                break;
            case OP_LESS:
                vmc_binaryop(BOOL_VAL, <, double);
                break;

            case OP_POP:
                vm_pop(vm);
                break;

            case OP_GET_LOCAL:
            {
                uint8_t slot = vmc_readbyte();
                vm_push(vm, frame->slots[slot]);// pushes the value to the stack where later instructions can read it
                break;
            }

            case OP_SET_LOCAL:
            {
                uint8_t slot = vmc_readbyte();
                // all the local var's VARIABLES are stored inside vm->stack
                frame->slots[slot] = vm_peek(vm, 0);// takes from top of the stack and stores it in the stack slot
                break;
            }

            case OP_DEFINE_GLOBAL:
            {
                ObjString* name = vmc_readstring();// get name from constant table
                htable_set(vm, &vm->globals, name, vm_peek(vm, 0));// take value from the top of the stack
                vm_pop(vm);
                break;
            }

            case OP_GET_GLOBAL:
            {
                ObjString* name = vmc_readstring();// get the name
                Value value;// create new Value
                if(!htable_get(vm, &vm->globals, name, &value))// if key not in hash table
                {
                    vm_rterror(vm, "Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                vm_push(vm, value);
                break;
            }

            case OP_SET_GLOBAL:
            {
                ObjString* name = vmc_readstring();
                if(htable_set(vm, &vm->globals, name, vm_peek(vm, 0)))// if key not in hash table
                {
                    htable_delete(vm, &vm->globals, name);// delete the false name
                    vm_rterror(vm, "Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            // upvalues set/get
            case OP_GET_UPVALUE:
            {
                uint8_t slot = vmc_readbyte();// read index
                vm_push(vm, *frame->closure->upvalues[slot]->location);// vm_push the value to the stack
                break;
            }

            case OP_SET_UPVALUE:
            {
                uint8_t slot = vmc_readbyte();// read index
                *frame->closure->upvalues[slot]->location = vm_peek(vm, 0);// set to the topmost stack
                break;
            }

            case OP_GET_PROPERTY:
            {
                // to make sure only instances are allowed to have fields
                if(!IS_INSTANCE(vm_peek(vm, 0)))
                {
                    vm_rterror(vm, "Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(vm_peek(vm, 0));// get instance from top most stack
                ObjString* name = vmc_readstring();// get identifier name

                Value value;// set up value to add to the stack
                if(htable_get(vm, &instance->fields, name, &value))// get from fields hash table, assign it to instance
                {
                    vm_pop(vm);// vm_pop the instance itself
                    vm_push(vm, value);
                    break;
                }

                if(!vm_bindmethod(vm, instance->kelas, name))// no method as well, error
                {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_SET_PROPERTY:
            {
                if(!IS_INSTANCE(vm_peek(vm, 1)))// if not an instance
                {
                    vm_rterror(vm, "Identifier must be a class instance.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                // not top most, as the top most is reserved for the new value to be set
                ObjInstance* instance = AS_INSTANCE(vm_peek(vm, 1));
                htable_set(vm, &instance->fields, vmc_readstring(), vm_peek(vm, 0));//vm_peek(0) is the new value

                Value value = vm_pop(vm);// vm_pop the already set value
                vm_pop(vm);// vm_pop the property instance itself
                vm_push(vm, value);// vm_push the value back again
                break;
            }


            case OP_CLOSE_UPVALUE:
            {
                vm_closeupvalues(vm, vm->stackTop - 1);// put address to the slot
                vm_pop(vm);// vm_pop from the stack
                break;
            }


            case OP_JUMP:// will always jump
            {
                uint16_t offset = vmc_readshort();
                frame->ip += offset;
                break;
            }

            case OP_JUMP_IF_FALSE:// for initial if, will not jump if expression inside is true
            {
                uint16_t offset = vmc_readshort();// offset already put in the stack
                // actual jump instruction is done here; skip over the instruction pointer
                if(vm_isfalsey(vm, vm_peek(vm, 0)))
                    frame->ip += offset;// if evaluated expression inside if statement is false jump
                break;
            }

            case OP_LOOP:
            {
                uint16_t offset = vmc_readshort();
                frame->ip -= offset;// jumps back
                break;
            }

            case OP_LOOP_IF_FALSE:
            {
                uint16_t offset = vmc_readshort();// offset already put in the stack
                // bool state is at the top of the stack
                // if false loop back
                if(vm_isfalsey(vm, vm_peek(vm, 0)))
                    frame->ip -= offset;
                vm_pop(vm);// vm_pop the true/false
                break;
            }

            case OP_LOOP_IF_TRUE:
            {
                uint16_t offset = vmc_readshort();// offset already put in the stack
                // bool state is at the top of the stack
                // if not false loop back
                if(!vm_isfalsey(vm, vm_peek(vm, 0)))
                    frame->ip -= offset;
                vm_pop(vm);// vm_pop the true/false
                break;
            }

            // a callstack to a funcion has the form of function name, param1, param2...
            // the top level code, or caller, also has the same function name, param1, param2... in the right order
            case OP_CALL:
            {
                int argCount = vmc_readbyte();
                if(!vm_callvalue(vm, vm_peek(vm, argCount), argCount))// vm_call function; pass in the function name istelf[vm_peek(depth)] and the number of arguments
                {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->frameCount - 1];// to update pointer if callFrame is successful, asnew frame is added
                break;
            }

            // closures
            case OP_CLOSURE:
            {
                ObjFunction* function = AS_FUNCTION(vmc_readconst());// load compiled function from table
                ObjClosure* closure = obj_mkclosure(vm, function);
                vm_push(vm, OBJ_VAL(closure));

                // fill upvalue array over in the interpreter when a closure is created
                // to see upvalues in each slot
                for(int i = 0; i < closure->upvalueCount; i++)
                {
                    uint8_t isLocal = vmc_readbyte();// read isLocal bool
                    uint8_t index = vmc_readbyte();// read index for local, if available, in the closure
                    if(isLocal)
                    {
                        closure->upvalues[i] = vm_captureupvalue(vm, frame->slots + index);// get from slots stack
                    }
                    else// if not local(nested upvalue)
                    {
                        closure->upvalues[i] = frame->closure->upvalues[index];// get from current upvalue
                    }
                }

                break;
            }

            case OP_CLASS:
                vm_push(vm, OBJ_VAL(obj_mkclass(vm, vmc_readstring())));// load string for the class' name and vm_push it onto the stack
                break;

            case OP_METHOD:
                vm_defmethod(vm, vmc_readstring());// get name of the method
                break;

            case OP_INVOKE:
            {
                ObjString* method = vmc_readstring();
                int argCount = vmc_readbyte();
                if(!vm_invoke(vm, method, argCount))// new vm_invoke function
                {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->frameCount - 1];
                break;
            }

            case OP_INHERIT:
            {
                Value parent = vm_peek(vm, 1);// parent class from 2nd top of the stack

                // ensure that parent identifier is a class
                if(!IS_CLASS(parent))
                {
                    vm_rterror(vm, "Parent identifier is not a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjClass* child = AS_CLASS(vm_peek(vm, 0));// child class at the top of the stack
                htable_addall(vm, &AS_CLASS(parent)->methods, &child->methods);// add all methods from parent to child table
                vm_pop(vm);// vm_pop the child class
                break;
            }

            case OP_GET_SUPER:
            {
                ObjString* name = vmc_readstring();// get method name/identifier
                ObjClass* parent = AS_CLASS(vm_pop(vm));// class identifier is at the top of the stack
                if(!vm_bindmethod(vm, parent, name))// if binding fails
                {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_SUPER_INVOKE:// super calls optimization
            {
                ObjString* method = vmc_readstring();
                int count = vmc_readbyte();
                ObjClass* parent = AS_CLASS(vm_pop(vm));
                if(!vm_invokefromclass(vm, parent, method, count))
                {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->frameCount - 1];
                break;
            }

            case OP_RETURN:
            {
                Value result = vm_pop(vm);// if function returns a value, value will beon top of the stack

                vm_closeupvalues(vm, frame->slots);// close lingering closed values

                vm->frameCount--;
                if(vm->frameCount == 0)// return from 'main()'/script function
                {
                    vm_pop(vm);// vm_pop main script function from the stack
                    return INTERPRET_OK;
                }

                // for a function
                // discard all the slots the callee was using for its parameters
                vm->stackTop = frame->slots;// basically 're-assign'
                vm_push(vm, result);// vm_push the return value

                frame = &vm->frames[vm->frameCount - 1];// update run function's current frame
                break;
            }
        }
    }

}


// function for loading scripts
void runFile(VM* vm, const char* path)
{
    char* source = readFile(vm, path);// get raw source code from the file
    InterpretResult result = vm_interpret(vm, source);// get enum type result from VM
    free(source);// free the source code

    if(result == INTERPRET_COMPILE_ERROR)
        exit(51);
    if(result == INTERPRET_RUNTIME_ERROR)
        exit(61);
}


int main(int argc, const char* argv[])// used in the command line, argc being the amount of arguments and argv the array
{
    VM vm;
    vm_init(&vm);
    // the FIRST argument will always be the name of the executable being run(e.g node, python in terminal)

    if(argc == 1)// if number of argument is one, run the repl
    {
        repl(&vm);
    }
    else if(argc == 2)// if number of arguments is two, the second one being the file, run the second file
    {
        runFile(&vm, argv[1]);
    }
    else
    {
        fprintf(stderr, "Usage: cfei [path]\n");// fprintf; print on file but not on console, first argument being the file pointer
        // in this case it prints STANDARD ERROR
        exit(64);
    }
    vm_free(&vm);
    return 0;
}
