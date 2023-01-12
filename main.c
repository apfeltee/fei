
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <assert.h>
#include "dnarray.h"

// max frames is fixed
//#define CFG_MAX_VMFRAMES 64
#define CFG_MAX_VMFRAMES (1024)

//#define CFG_MAX_VMSTACK (CFG_MAX_VMFRAMES * UINT8_COUNT)
#define CFG_MAX_VMSTACK (CFG_MAX_VMFRAMES)

// local variables array in compiler, max of 1 byte is 255
#define UINT8_COUNT (UINT8_MAX + 1)

#define CFG_MAX_COMPILERBREAK (255/8)
#define CFG_MAX_COMPILERUPVALS (128)
#define CFG_MAX_COMPILERLOCALS (64)

// growth factor for garbage collection heap
#define GC_HEAP_GROW_FACTOR 2

// the hash table can only be 75% full
#define TABLE_MAX_LOAD 0.75

// track the compiler
#define DEBUG_PRINT_CODE 0

// execution tracing of the VM
#define DEBUG_TRACE_EXECUTION 0


// diagnostic tools for garbage collector
// 'stress' mode; if this is on, GC runs as often as it possibly can
#define DEBUG_STRESS_GC 0
#define DEBUG_LOG_GC 0

// macros for conversions from code type to Value union type
// pass Value struct to the macro
/*	IMPORTANT: macro syntax
#define macroname(parameter) (returntype)
-> here, return a Value type. initializing it inside the macro
-> . means as
IMPORTANT = these macros give a 'tag' to each respective values
*/
#define BOOL_VAL(value) ((Value){ VAL_BOOL, { .valbool = value } })
#define NULL_VAL ((Value){ VAL_NULL, { .valnumber = 0 } })
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, { .valnumber = value } })
#define OBJ_VAL(object) ((Value){ VAL_OBJ, { .valobjptr = (Object*)object } })// pass in as a pointer to the object, receives the actual object

// type comparisons
#define fei_value_isbool(value) ((value).type == VAL_BOOL)
#define fei_value_isnull(value) ((value).type == VAL_NULL)
#define fei_value_isnumber(value) ((value).type == VAL_NUMBER)
#define fei_value_isobj(value) ((value).type == VAL_OBJ)




// macro to allocate memory, usedin obj/heap
// use fei_gcmem_reallocate as malloc here; start from null pointer, old size is 0, and new size is count
#define ALLOCATE(state, typsz, count) fei_gcmem_reallocate(state, NULL, 0, typsz * (count))


// free memory, pass in new size as 0 to free
#define FREE(state, typsz, pointer) fei_gcmem_reallocate(state, pointer, typsz, 0)


// C macros
// calculates a new capacity based on a given current capacity, it should SCALE based on the old one
// this one grows by * 2
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)// capacity becomes 8 for the first time(starts from 0), later it multiplies by 2

// macro to grow array
// make own fei_gcmem_reallocate function
// basically declare our return type here with (type*)
#define GROW_ARRAY(state, typsz, pointer, oldcount, newcount) fei_gcmem_reallocate(state, pointer, typsz * (oldcount), typsz * (newcount))

// no (type*) because function does not return a type
// 0 is the new capacity
// used to free eg. char arrays
#define FREE_ARRAY(state, typsz, pointer, oldcount) fei_gcmem_reallocate(state, pointer, typsz * (oldcount), 0)


enum TokType
{
    // single character
    TOKEN_LEFTPAREN,
    TOKEN_RIGHTPAREN,// ( )
    TOKEN_LEFTBRACE,
    TOKEN_RIGHTBRACE,// { }
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
    TOKEN_LOGICALNOT,
    TOKEN_NOTEQUAL,// !, !=
    TOKEN_ASSIGN,
    TOKEN_EQUAL,
    TOKEN_GREATERTHAN,
    TOKEN_GREATEREQUAL,
    TOKEN_LESSTHAN,
    TOKEN_LESSEQUAL,

    // literals
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,

    // keywords
    TOKEN_KWAND,
    TOKEN_KWCLASS,
    TOKEN_KWELF,
    TOKEN_KWELSE,
    TOKEN_KWFALSE,
    TOKEN_KWFOR,
    TOKEN_KWFUN,
    TOKEN_KWIF,
    TOKEN_KWNULL,
    TOKEN_KWOR,
    TOKEN_KWPRINT,
    TOKEN_KWRETURN,
    TOKEN_KWSUPER,
    TOKEN_KWSWITCH,
    TOKEN_KWDEFAULT,
    TOKEN_KWCASE,
    TOKEN_KWTHIS,
    TOKEN_KWTRUE,
    TOKEN_KWVAR,
    TOKEN_KWWHILE,
    TOKEN_KWBREAK,
    TOKEN_KWCONTINUE,

    // do while, repeat until
    TOKEN_KWDO,
    TOKEN_KWREPEAT,
    TOKEN_KWUNTIL,

    // class inheritance
    TOKEN_KWFROM,

    TOKEN_ERROR,
    TOKEN_EOF
};

// for precedence in unary operations
// ordered from lowest precedence to highest precedence
typedef enum
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
} Precedence;

typedef enum
{
    TYPE_FUNCTION,
    TYPE_SCRIPT,// top level main()
    TYPE_INITIALIZER,// class constructors
    TYPE_METHOD,// class methods
} FuncType;


// in bytecode format, each instruction has a one-byte operation code(opcode)
// the number controls what kind of instruction we're dealing with- add, subtract, etc
// typedef enums are bytes apparently
// these are INSTRUCTIONS
typedef enum
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
    OP_PRINT,
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
} OpCode;// basically a typdef call to an enum
// in C, you cannot have enums called simply by their rvalue 'string' names, use typdef to define them


typedef enum
{
    OBJ_BOUND_METHOD,
    OBJ_INSTANCE,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE
} ObjType;

// rseult that responds from the running VM
typedef enum
{
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} ResultCode;

// type tags for the tagged union
enum ValType
{
    VAL_BOOL,
    VAL_NULL,
    VAL_NUMBER,
    VAL_OBJ,// for bigger instances such as strings, functions, heap-allocated; the payload is a heap pointer
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

typedef enum TokType TokType;
typedef enum ValType ValType;
typedef struct /**/ Value Value;
typedef struct /**/ Object Object;
typedef struct /**/ ObjString ObjString;
typedef struct /**/ ObjFunction ObjFunction;
typedef struct /**/ ObjClass ObjClass;
typedef struct /**/ ObjInstance ObjInstance;
typedef struct /**/ ObjBoundMethod ObjBoundMethod;
typedef struct /**/ ObjUpvalue ObjUpvalue;
typedef struct /**/ ObjClosure ObjClosure;
typedef struct /**/ ObjNative ObjNative;
typedef struct /**/ Local Local;
typedef struct /**/ Upvalue Upvalue;
typedef struct /**/ ASTState ASTState;
typedef struct /**/ GCState GCState;
typedef struct /**/ VMState VMState;
typedef struct /**/ State State;
typedef struct /**/ CallFrame CallFrame;
typedef struct /**/ Token Token;
typedef struct /**/ Scanner Scanner;
typedef struct /**/ Parser Parser;
typedef struct /**/ ParseRule ParseRule;
typedef struct /**/ Compiler Compiler;
typedef struct /**/ ClassCompiler ClassCompiler;
typedef struct /**/ ValArray ValArray;
typedef struct /**/ Chunk Chunk;
typedef struct /**/ TabEntry TabEntry;
typedef struct /**/ Table Table;
typedef struct /**/ Writer Writer;

// simple typdef function type with no arguments and returns nothing
// acts like a "virtual" function , a void function that cam be overidden; actually a void but override it with ParseFn
typedef void (*ParseFn)(State*, bool);


/*  NATIVE FUNCTIONS(file systems, user input etc.)
-> native functions reference a call to native C code insted of bytecode */
typedef Value (*NativeFn)(State*, int, Value*);// rename Value to NativeFn


struct Writer
{
    FILE* filehandle;
    bool filemustclose;
};


/* IMPORTANT 
-> use C unions to OVERLAP in memory for the STRUCT
->  size of the union is its LARGEST FIELD
-> unions are like structs but they only allocate memory for the LARGEST FIELD
*/

struct Value
{
    ValType type;
    union
    {
        bool valbool;
        double valnumber;
        Object* valobjptr;
    } as;
};

struct Token
{
    int length;
    int line;
    TokType type;
    const char* toksrc;
};

struct Scanner
{
    int line;
    size_t length;
    const char* startsrc;
    const char* currentsrc;
};

struct Parser
{
    bool haderror;
    bool panicmode;
    Token currtoken;
    Token prevtoken;
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

    // depth of the variable, corresponding to scoredepth in the struct below
    int depth;

    // track whether the local is captured by a closure or no
    bool iscaptured;
};

struct Upvalue
{
    bool islocalvar;
    // matches the index of the local variable in ObjClosure
    int index;
};

struct Compiler
{
    // wrapping the whole program into one big main() function
    FuncType progfunctype;
    ObjFunction* programfunc;

    // pointer to the 'outer'/enclosing compiler, to return to after function
    Compiler* enclosing;

    // array to store locals, ordered in the order of declarations
    int progloccount;
    Local proglocals[CFG_MAX_COMPILERLOCALS];

    Upvalue upvalues[CFG_MAX_COMPILERUPVALS];

    // number of scopes/blocks surrounding the code
    int scopedepth;

    // for loop breaks and continues, loop enclosing
    int loopcounttop;
    int* continuejumps;

    // only for continue jumpbs
    int continuejumpcapacity;

    // for patching all break statements
    int breakpatchjumps[CFG_MAX_COMPILERBREAK][CFG_MAX_COMPILERBREAK];
    int breakjumpcounts[CFG_MAX_COMPILERBREAK];
};

struct ClassCompiler
{
    // to end scope in superclass declaration
    bool hassuperclass;
    Token name;
    ClassCompiler* enclosing;
};


struct ValArray
{
    int capacity;
    int count;
    Value* values;
};

struct Chunk
{
    int count;
    int capacity;
    uint8_t* code;
    // array of integers that parallels the bytecode/codestream, to get where each location of the bytecode is
    int* lines;
    // store double value literals
    ValArray constants;
};

struct TabEntry
{
    // use ObjString pointer as key
    ObjString* key;
    // the value/data type
    Value value;
};

struct Table
{
    int count;
    int capacity;
    TabEntry* entries;
};

struct Object
{
    // for mark-sweep garbage collection
    bool ismarked;

    ObjType type;

    // linked list or intrusive list, to avoid memory leaks, obj itself as a node
    // traverse the list to find every object that has been allocated on the heap
    Object* next;
};

// for functions and calls
struct ObjFunction
{
    Object obj;

    // store number of parameters
    int arity;

    // to track upvalues
    int upvaluecount;

    // to store the function information
    Chunk chunk;
    ObjString* name;
};

struct ObjUpvalue
{
    Object obj;

    // pointer to value in the enclosing ObjClosure
    Value* location;

    // to store closed upvalue
    Value closed;

    // intrusive/linked list to track sorted openvalues
    // ordered by the stack slot they point to
    ObjUpvalue* next;
};


struct ObjClosure
{
    Object obj;
    ObjFunction* function;

    // for upvalues
    ObjUpvalue** upvalues;
    int upvaluecount;
};

struct ObjNative
{
    Object obj;
    NativeFn function;
};

struct ObjString
{
    Object obj;
    int length;
    char* chars;
    uint32_t hash;
};

struct ObjClass
{
    Object obj;
    ObjString* name;
    Table methods;
};

struct ObjInstance
{
    Object obj;
    ObjClass* classobject;
    Table fields;
};

// struct for class methods
struct ObjBoundMethod
{
    Object obj;
    // wraps receiver and function/method/closure together, receiver is the ObjInstance / lcass type
    Value receiver;
    ObjClosure* method;
};

// the call stack
// keep track where on the stack a function's local begin, where the caller should resume, etc.
// a call frame represents a single ongoing function call
// each time a function is called, create this struct
struct CallFrame
{
    ObjClosure* closure;

    // store ip on where in the VM the function is
    uint8_t* ip;

    // this points into the VM's value stack at the first slot the function can use
    Value* slots;
};

struct GCState
{
    // stack to store gray marked Objects for garbage collection
    int graycapacity;
    int graycount;

    // self-adjusting-g-heap, to control frequency of GC, bytesallocated is the running total
    // size_t is a 32 bit(integer/4bytes), represents size of an object in bytes
    size_t bytesallocated;

    // threhsold that triggers the GC
    size_t nextgc;

    // pointer to the header of the Object itself/node, start of the list
    // nicely used in GARBAGE COLLECTION, where objects are nicely erased in the middle
    Object* objects;
    
    // array of pointers pointing to a particular subgraph
    Object** graystack;
};

struct ASTState
{
    Scanner scanner;
    Parser parser;

    //currentclass
    ClassCompiler* classcompiler;

    //current
    Compiler* compiler;
};

struct VMState
{
    // stores current height of the stack
    int framecount;

    // pointer to the element just PAST the element containing the top value of the stack
    Value* stacktop;

    // for storing global variables
    Table globals;

    // for string interning, to make sure every equal string takes one memory
    Table strings;

    // init string for class constructors
    ObjString* initstring;

    // track all upvalues; points to the first node of the linked list
    ObjUpvalue* openupvalues;


    // since the whole program is one big 'main()' use callstacks
    CallFrame frameobjects[CFG_MAX_VMFRAMES];
    //intptr_t* frameobjects;

    // stack array is 'indirectly' declared inline here
    Value stack[CFG_MAX_VMSTACK];

};

struct State
{
    Writer* outwriter;
    VMState vmstate;
    ASTState aststate;
    GCState gcstate;
};


void *fei_gcmem_reallocate(State *state, void *pointer, size_t oldsize, size_t newsize);
void fei_gcmem_freeobject(State *state, Object *object);
void fei_gcmem_markobject(State *state, Object *object);
void fei_gcmem_markvalue(State *state, Value value);
void fei_gcmem_markarray(State *state, ValArray *array);
void fei_gcmem_markroots(State *state);
void fei_gcmem_blackenobject(State *state, Object *object);
void fei_gcmem_tracerefs(State *state);
void fei_gcmem_sweep(State *state);
void fei_gcmem_collectgarbage(State *state);
void fei_gcmem_freeobjects(State *state);
void fei_valarray_init(State *state, ValArray *array);
void fei_valarray_push(State *state, ValArray *array, Value value);
void fei_valarray_destroy(State *state, ValArray *array);
void fei_value_printvalue(State *state, Value value);
bool fei_value_compare(State *state, Value a, Value b);
Object *fei_object_allocobject(State *state, size_t size, ObjType type);
ObjBoundMethod *fei_object_makeboundmethod(State *state, Value receiver, ObjClosure *method);
ObjClosure *fei_object_makeclosure(State *state, ObjFunction *function);
ObjString *fei_object_allocstring(State *state, char *chars, int length, uint32_t hash);
ObjClass *fei_object_makeclass(State *state, ObjString *name);
ObjInstance *fei_object_makeinstance(State *state, ObjClass *klassobj);
ObjFunction *fei_object_makefunction(State *state);
ObjNative *fei_object_makenativefunc(State *state, NativeFn function);
uint32_t fei_object_hashstring(State *state, const char *key, int length);
ObjString *fei_object_takestring(State *state, char *chars, int length);
ObjString *fei_object_copystring(State *state, const char *chars, int length);
ObjUpvalue *fei_object_makeupvalue(State *state, Value *slot);
void fei_object_printfunc(State *state, ObjFunction *function);
void fei_object_printobject(State *state, Value value);
void fei_table_init(State *state, Table *table);
void fei_table_destroy(State *state, Table *table);
TabEntry *fei_table_findentry(State *state, TabEntry *entries, int capacity, ObjString *key);
bool fei_table_get(State *state, Table *table, ObjString *key, Value *value);
void fei_table_adjustcapacity(State *state, Table *table, int capacity);
bool fei_table_set(State *state, Table *table, ObjString *key, Value value);
bool fei_table_delete(State *state, Table *table, ObjString *key);
void fei_table_mergefrom(State *state, Table *from, Table *to);
ObjString *fei_table_findstring(State *state, Table *table, const char *chars, int length, uint32_t hash);
void fei_table_removeunreachable(State *state, Table *table);
void fei_table_mark(State *state, Table *table);
void fei_chunk_init(State *state, Chunk *chunk);
void fei_chunk_pushbyte(State *state, Chunk *chunk, uint8_t byte, int line);
void fei_chunk_destroy(State *state, Chunk *chunk);
int fei_chunk_pushconst(State *state, Chunk *chunk, Value value);
int fei_dbgutil_printsimpleir(State *state, const char *name, int offset);
int fei_dbgutil_printbyteir(State *state, const char *name, Chunk *chunk, int offset);
int fei_dbgutil_printconstir(State *state, const char *name, Chunk *chunk, int offset);
int fei_dbgutil_printinvokeir(State *state, const char *name, Chunk *chunk, int offset);
int fei_dbgutil_printjumpir(State *state, const char *name, int sign, Chunk *chunk, int offset);
void fei_dbgdisas_chunk(State *state, Chunk *chunk, const char *name);
int fei_dbgdisas_instr(State *state, Chunk *chunk, int offset);
void fei_lexer_initsource(State *state, const char *source, size_t len);
bool fei_lexutil_isalpha(State *state, char c);
bool fei_lexutil_isdigit(State *state, char c);
bool fei_lexer_isatend(State *state);
char fei_lexer_advance(State *state);
bool fei_lexer_match(State *state, char expected);
Token fei_lexer_maketoken(State *state, TokType type);
Token fei_lexer_errortoken(State *state, const char *message);
char fei_lexer_peekcurrent(State *state);
char fei_lexer_peeknext(State *state);
void fei_lexer_skipspace(State *state);
TokType fei_lexer_checkkw(State *state, int start, int length, const char *rest, TokType type);
TokType fei_lexer_scantype(State *state);
Token fei_lexer_scanident(State *state);
Token fei_lexer_scannumber(State *state);
Token fei_lexer_scanstring(State *state);
Token fei_lexer_scantoken(State *state);
Chunk *fei_compiler_currentchunk(State *state);
void fei_compiler_raiseat(State *state, Token *token, const char *message);
void fei_compiler_raiseerror(State *state, const char *message);
void fei_compiler_raisehere(State *state, const char *message);
void fei_compiler_advancenext(State *state);
void fei_compiler_advanceskipping(State *state, TokType type);
void fei_compiler_consume(State *state, TokType type, const char *message);
bool fei_compiler_check(State *state, TokType type);
bool fei_compiler_match(State *state, TokType type);
void fei_compiler_emitbyte(State *state, uint8_t byte);
void fei_compiler_emitbytes(State *state, uint8_t byte1, uint8_t byte2);
void fei_compiler_emitloop(State *state, int loopstart);
void fei_compiler_emitcondloop(State *state, int loopstart, bool condstate);
int fei_compiler_emitjump(State *state, uint8_t instruction);
void fei_compiler_emitreturn(State *state);
uint8_t fei_compiler_makeconst(State *state, Value value);
void fei_compiler_emitconst(State *state, Value value);
void fei_compiler_patchjump(State *state, int offset);
void fei_compiler_init(State *state, Compiler *compiler, FuncType type);
ObjFunction *fei_compiler_endcompiler(State *state);
void fei_compiler_beginscope(State *state);
void fei_compiler_endscope(State *state);
void fei_compiler_beginloopscope(State *state);
void fei_compiler_endloopscope(State *state);
void fei_compiler_markcontinuejump(State *state);
void fei_compiler_patchbreakjumps(State *state);
uint8_t fei_compiler_makeidentconst(State *state, Token *name);
bool fei_compiler_identsequal(State *state, Token *a, Token *b);
int fei_compiler_resolvelocal(State *state, Compiler *compiler, Token *name);
int fei_compiler_addupvalue(State *state, Compiler *compiler, uint8_t index, bool islocal);
int fei_compiler_resolveupvalue(State *state, Compiler *compiler, Token *name);
void fei_compiler_addlocal(State *state, Token name);
void fei_compiler_declvarfromcurrent(State *state);
uint8_t fei_compiler_parsevarfromcurrent(State *state, const char *errormessage);
void fei_compiler_markinit(State *state);
void fei_compiler_defvarindex(State *state, uint8_t global);
uint8_t fei_compiler_parsearglist(State *state);
static void fei_comprule_logicaland(State *state, bool canassign);
static void fei_comprule_binary(State *state, bool canassign);
static void fei_comprule_call(State *state, bool canassign);
static void fei_comprule_dot(State *state, bool canassign);
static void fei_comprule_literal(State *state, bool canassign);
static void fei_comprule_grouping(State *state, bool canassign);
static void fei_comprule_number(State *state, bool canassign);
static void fei_comprule_logicalor(State *state, bool canassign);
static void fei_comprule_string(State *state, bool canassign);
void fei_compiler_declnamedvar(State *state, Token name, bool canassign);
static void fei_comprule_variable(State *state, bool canassign);
Token fei_compiler_makesyntoken(State *state, const char *text);
static void fei_comprule_super(State *state, bool canassign);
static void fei_comprule_this(State *state, bool canassign);
static void fei_comprule_unary(State *state, bool canassign);
void fei_compiler_parseprec(State *state, Precedence precedence);
ParseRule *fei_compiler_getrule(State *state, TokType type);
void fei_compiler_parseexpr(State *state);
void fei_compiler_parseblock(State *state);
void fei_compiler_parsefuncdecl(State *state, FuncType type);
void fei_compiler_parsemethoddecl(State *state);
void fei_compiler_parseclassdecl(State *state);
void fei_compiler_parseclassfuncdecl(State *state);
void fei_compiler_parsevardecl(State *state);
void fei_compiler_parseexprstmt(State *state);
void fei_compiler_parseifstmt(State *state);
void fei_compiler_parseswitchstmt(State *state);
void fei_compiler_parseprintstmt(State *state);
void fei_compiler_parsereturnstmt(State *state);
void fei_compiler_parseforstmt(State *state);
void fei_compiler_parsewhilestmt(State *state);
void fei_compiler_parsebreakstmt(State *state);
void fei_compiler_parsecontinuestmt(State *state);
void fei_compiler_parserepeatuntilstmt(State *state);
void fei_compiler_parsedowhilestmt(State *state);
void fei_compiler_synchronize(State *state);
void fei_compiler_parsedeclaration(State *state);
void fei_compiler_parsestatement(State *state);
ObjFunction *fei_compiler_compilesource(State *state, const char *source, size_t len);
void fei_compiler_markroots(State *state);
void fei_vm_raiseruntimeerror(State *state, const char *format, ...);
void fei_vm_defnative(State *state, const char *name, NativeFn function);
void fei_vm_resetstack(State *state);
State *fei_state_init(void);
void fei_state_destroy(State *state);
void fei_vm_pushvalue(State *state, Value value);
Value fei_vm_popvalue(State *state);
Value fei_vm_peekvalue(State *state, int distance);
bool fei_vm_callclosure(State *state, ObjClosure *closure, int argcount);
bool fei_vm_callvalue(State *state, Value callee, int argcount);
bool fei_class_invokemethod(State *state, ObjClass *klassobj, ObjString *name, int argcount);
bool fei_vm_stackinvoke(State *state, ObjString *name, int argcount);
bool fei_class_bindmethod(State *state, ObjClass *klassobj, ObjString *name);
ObjUpvalue *fei_vm_captureupvalue(State *state, Value *local);
void fei_vm_closeupvalues(State *state, Value *last);
void fei_vm_stackdefmethod(State *state, ObjString *name);
bool fei_value_isfalsey(State *state, Value value);
void fei_vmdo_strconcat(State *state);
ResultCode fei_vm_evalsource(State *state, const char *source, size_t len);
ResultCode fei_vm_exec(State *state);
void repl(State *state);
void runfile(State *state, const char *path);
int main(int argc, const char *argv[]);


// from VALUE STRUCT to RAW  C nicely used in printing
// also for comparisons -> use values ALREADY CONVERTED to Value struct union to raw C
static inline bool fei_value_asbool(Value v)
{
    return v.as.valbool;
}

static inline double fei_value_asnumber(Value v)
{
    return v.as.valnumber;
}

static inline Object* fei_value_asobj(Value v)
{
    return v.as.valobjptr;
}

static inline ObjType OBJ_TYPE(Value v)
{
    return fei_value_asobj(v)->type;
}

static inline ObjBoundMethod* fei_value_asbound_method(Value v)
{
    return (ObjBoundMethod*)fei_value_asobj(v);
};

static inline ObjClass* fei_value_asclass(Value v)
{
    return (ObjClass*)fei_value_asobj(v);
}

static inline ObjInstance* fei_value_asinstance(Value v)
{
    return (ObjInstance*)fei_value_asobj(v);
}

static inline ObjClosure* fei_value_asclosure(Value v)
{
    return (ObjClosure*)fei_value_asobj(v);
}

static inline ObjString* fei_value_asstring(Value v)
{
    return (ObjString*)fei_value_asobj(v);
}

static inline char* fei_value_ascstring(Value v)
{
    return fei_value_asstring(v)->chars;
}

static inline ObjFunction* fei_value_asfunction(Value v)
{
    return (ObjFunction*)fei_value_asobj(v);
}

static inline NativeFn fei_value_asnative(Value v)
{
    return ((ObjNative*)fei_value_asobj(v))->function;
}

static inline bool fei_object_istype(Value value, ObjType type)
{
    return fei_value_isobj(value) && fei_value_asobj(value)->type == type;
}

static inline bool fei_value_isbound_method(Value v)
{
    return fei_object_istype(v, OBJ_BOUND_METHOD);
}

static inline bool fei_value_isclass(Value v)
{
    return fei_object_istype(v, OBJ_CLASS);
}

static inline bool fei_value_isfunction(Value v)
{
    return fei_object_istype(v, OBJ_FUNCTION);
}

static inline bool fei_value_isinstance(Value v)
{
    return fei_object_istype(v, OBJ_INSTANCE);
}

static inline bool fei_value_isnative(Value v)
{
    return fei_object_istype(v, OBJ_NATIVE);
}

static inline bool fei_value_isstring(Value v)
{
    return fei_object_istype(v, OBJ_STRING);
}

static inline bool fei_value_isclosure(Value v)
{
    return fei_object_istype(v, OBJ_CLOSURE);
}


Writer* fei_writer_init(State* state)
{
    Writer* wr;
    wr =  ALLOCATE(state, sizeof(Writer), 1);
    wr->filehandle = NULL;
    wr->filemustclose = false;

    return wr;
}

void fei_writer_destroy(State* state, Writer* wr)
{
    FREE(state, sizeof(Writer), wr);
}

Writer* fei_writer_initfile(State* state, FILE* fh, bool alsoclose)
{
    Writer* wr;
    wr = fei_writer_init(state);
    wr->filehandle = fh;
    wr->filemustclose = alsoclose;
    return wr;
}

void fei_valarray_init(State* state, ValArray* array)
{
    (void)state;
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}

size_t fei_valarray_count(ValArray* arr)
{
    return arr->count;
}

Value fei_valarray_get(State* state, ValArray* arr, int idx)
{
    (void)state;
    return arr->values[idx];
}

void fei_valarray_push(State* state, ValArray* array, Value value)
{
    int oldcap;
    if(array->capacity < array->count + 1)
    {
        oldcap = array->capacity;
        array->capacity = GROW_CAPACITY(oldcap);
        array->values = (Value*)GROW_ARRAY(state, sizeof(Value), array->values, oldcap, array->capacity);
    }
    array->values[array->count] = value;
    array->count++;
}

void fei_valarray_destroy(State* state, ValArray* array)
{
    FREE_ARRAY(state, sizeof(Value), array->values, array->capacity);
    fei_valarray_init(state, array);
}


// actual printing on the virtual machine is done here
void fei_value_printvalue(State* state, Value value)
{
    switch(value.type)
    {
        case VAL_BOOL:
            printf(fei_value_asbool(value) ? "true" : "false");
            break;
        case VAL_NULL:
            printf("null");
            break;
        case VAL_NUMBER:
            printf("%g", fei_value_asnumber(value));
            break;
        case VAL_OBJ:
            fei_object_printobject(state, value);
            break;// print heap allocated value, from object.h
    }
}

// comparison function used in VM run()
// used in ALL types of data(num, string, bools)
bool fei_value_compare(State* state, Value a, Value b)
{
    (void)state;
    if(a.type != b.type)
    {
        return false;
    }
    switch(a.type)
    {
        case VAL_BOOL:
            return fei_value_asbool(a) == fei_value_asbool(b);
        case VAL_NUMBER:
            return fei_value_asnumber(a) == fei_value_asnumber(b);
        case VAL_NULL:
            return true;// true for all nulls
        case VAL_OBJ:
            return fei_value_asobj(a) == fei_value_asobj(b);// already interned, occupies the same address
        default:
            break;
    }
    return false;
}


/* copying the string from the const char* in the source code to the heap 
-> does not reuse string pointers from the source code
*/

Object* fei_object_allocobject(State* state, size_t size, ObjType type)
{
    Object* object = (Object*)fei_gcmem_reallocate(state, NULL, 0, size);// allocate memory for obj
    object->type = type;
    object->ismarked = false;

    // every time an object is allocated, insert to the list
    // insert as the HEAD; the latest one inserted will be at the start
    object->next = state->gcstate.objects;// vm from virtualm.h, with extern
    state->gcstate.objects = object;

#if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
    printf("%p allocate %zd for %d\n", (void*)object, size, type);// %ld prints LONG INT
    // (void*) for 'native pointer type'
#endif


    return object;
}

// new bound method for classes
ObjBoundMethod* fei_object_makeboundmethod(State* state, Value receiver, ObjClosure* method)
{
    ObjBoundMethod* bound = (ObjBoundMethod*)fei_object_allocobject(state, sizeof(ObjBoundMethod), OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}


// create new closure
ObjClosure* fei_object_makeclosure(State* state, ObjFunction* function)
{
    // initialize array of upvalue pointers
    // upvalues carry over
    ObjUpvalue** upvalues = (ObjUpvalue**)ALLOCATE(state, sizeof(ObjUpvalue*), function->upvaluecount);

    for(int i = 0; i < function->upvaluecount; i++)
    {
        upvalues[i] = NULL;// initialize all as null
    }


    ObjClosure* closure = (ObjClosure*)fei_object_allocobject(state, sizeof(ObjClosure), OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvaluecount = function->upvaluecount;
    return closure;
}

ObjString* fei_object_allocstring(State* state, char* chars, int length, uint32_t hash)// pass in hash
{
    ObjString* string = (ObjString*)fei_object_allocobject(state, sizeof(ObjString), OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    fei_vm_pushvalue(state, OBJ_VAL(string));// garbage collection
    //printf("allocate\n");
    fei_table_set(state, &state->vmstate.strings, string, NULL_VAL);// for string interning
    fei_vm_popvalue(state);// garbage collection

    return string;
}

ObjClass* fei_object_makeclass(State* state, ObjString* name)
{
    ObjClass* klassobj = (ObjClass*)fei_object_allocobject(state, sizeof(ObjClass), OBJ_CLASS);
    klassobj->name = name;
    fei_table_init(state, &klassobj->methods);
    return klassobj;
}


// create new class instance
ObjInstance* fei_object_makeinstance(State* state, ObjClass* klassobj)
{
    ObjInstance* instance;
    instance = (ObjInstance*)fei_object_allocobject(state, sizeof(ObjInstance), OBJ_INSTANCE);
    instance->classobject = klassobj;
    fei_table_init(state, &instance->fields);
    return instance;
}


ObjFunction* fei_object_makefunction(State* state)
{
    ObjFunction* function;
    function = (ObjFunction*)fei_object_allocobject(state, sizeof(ObjFunction), OBJ_FUNCTION);
    function->arity = 0;
    function->upvaluecount = 0;
    function->name = NULL;
    fei_chunk_init(state, &function->chunk);
    return function;
}

// new native function
ObjNative* fei_object_makenativefunc(State* state, NativeFn function)
{
    ObjNative* native;
    native = (ObjNative*)fei_object_allocobject(state, sizeof(ObjNative), OBJ_NATIVE);
    native->function = function;
    return native;
}

// hash function, the FNV-1a
uint32_t fei_object_hashstring(State* state, const char* key, int length)
{
    int i;
    uint32_t hash;
    (void)state;
    hash = 2116136261u;
    for(i = 0; i < length; i++)
    {
        // munge the bits from the string key to the hash value; ^= is a bitwise operator
        hash ^= key[i];
        hash *= 16777619;
    }
    return hash;
}


// shorten than fei_object_copystring because owernship of the char* itself is declared in concatenate(), hence no need to declare memory again
ObjString* fei_object_takestring(State* state, char* chars, int length)
{
    uint32_t hash;
    ObjString* interned;
    hash = fei_object_hashstring(state, chars, length);
    interned = fei_table_findstring(state, &state->vmstate.strings, chars, length, hash);
    if(interned != NULL)// if the same string already exists
    {
        FREE_ARRAY(state, sizeof(char), chars, length + 1);// free the memory for use
        return interned;
    }
    return fei_object_allocstring(state, chars, length, hash);
}

// copy string from source code to memory
ObjString* fei_object_copystring(State* state, const char* chars, int length)
{
    uint32_t hash;
    char* heapchars;
    ObjString* interned;
    hash = fei_object_hashstring(state, chars, length);
    interned = fei_table_findstring(state, &state->vmstate.strings, chars, length, hash);
    if(interned != NULL)
    {
        return interned;
    }
    heapchars = (char*)ALLOCATE(state, sizeof(char), length + 1);
    memcpy(heapchars, chars, length);
    heapchars[length] = '\0';
    return fei_object_allocstring(state, heapchars, length, hash);
}


ObjUpvalue* fei_object_makeupvalue(State* state, Value* slot)
{
    ObjUpvalue* upvalue;
    upvalue = (ObjUpvalue*)fei_object_allocobject(state, sizeof(ObjUpvalue), OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->next = NULL;
    upvalue->closed = NULL_VAL;
    return upvalue;
}

void fei_object_printfunc(State* state, ObjFunction* function)
{
    (void)state;
    if(function->name == NULL)
    {
        printf("<script>");
        return;
    }
    printf("fun %s(%d params)", function->name->chars, function->arity);// print name and number of parameters
}

void fei_object_printobject(State* state, Value value)
{
    (void)state;
    // first class objects can be printed; string and functions
    switch(OBJ_TYPE(value))
    {
        case OBJ_BOUND_METHOD:
            fei_object_printfunc(state, fei_value_asbound_method(value)->method->function);
            break;
        case OBJ_CLASS:
            printf("%s", fei_value_asclass(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            printf("%s instance", fei_value_asinstance(value)->classobject->name->chars);
            break;
        case OBJ_CLOSURE:
            fei_object_printfunc(state, fei_value_asclosure(value)->function);
            break;
        case OBJ_FUNCTION:
            fei_object_printfunc(state, fei_value_asfunction(value));
            break;
        case OBJ_NATIVE:
            printf("<native fun>");
            break;
        case OBJ_STRING:
            printf("%s", fei_value_ascstring(value));
            break;
        case OBJ_UPVALUE:
            printf("upvalue");
            break;
        default:
            return;
    }
}

void fei_table_init(State* state, Table* table)
{
    (void)state;
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void fei_table_destroy(State* state, Table* table)
{
    FREE_ARRAY(state, sizeof(TabEntry), table->entries, table->capacity);
    fei_table_init(state, table);
}

TabEntry* fei_table_findentry(State* state, TabEntry* entries, int capacity, ObjString* key)
{
    uint32_t index;
    TabEntry* entry;
    TabEntry* tombstone;
    (void)state;
    // use modulo to map the key's hash to the code index
    index = key->hash % capacity;
    tombstone = NULL;
    for(;;)
    {
        // index is 'inserted' here
        entry = &entries[index];
        if(entry->key == NULL)
        {
            if(fei_value_isnull(entry->value))
            {
                // empty entry
                if(tombstone != NULL)
                {
                    return tombstone;
                }
                return entry;
            }
            else
            {
                if(tombstone == NULL)
                {
                    // can return tombstone bucket as empty and reuse it
                    tombstone = entry;
                }
            }
        }
        if(entry->key == key)// compare them in MEMORY
        {
            return entry;
        }
        index = (index + 1) % capacity;
    }
}

bool fei_table_get(State* state, Table* table, ObjString* key, Value* value)
{
    if(table->count == 0)
        return false;

    TabEntry* entry = fei_table_findentry(state, table->entries, table->capacity, key);
    if(entry->key == NULL)
        return false;

    *value = entry->value;// asign the value parameter the entry value
    return true;
}


void fei_table_adjustcapacity(State* state, Table* table, int capacity)
{
    int i;
    TabEntry* dest;
    TabEntry* entry;
    TabEntry* entries;
    entries = (TabEntry*)ALLOCATE(state, sizeof(TabEntry), capacity);
    for(i = 0; i < capacity; i++)
    {
        entries[i].key = NULL;
        entries[i].value = NULL_VAL;
    }
    // do not copy tombstones over when growing
    // NOTE: entries may end up in different buckets
    // with the same hash as it is divided by the modulo; loop below recalculates everything
    table->count = 0;
    for(i = 0; i < table->capacity; i++)// travers through old array
    {
        entry = &table->entries[i];
        if(entry->key == NULL)
        {
            continue;
        }
        dest = fei_table_findentry(state, entries, capacity, entry->key);// pass in new array
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }
    FREE_ARRAY(state, sizeof(TabEntry), table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

// inserting into the table, return false if collision
bool fei_table_set(State* state, Table* table, ObjString* key, Value value)
{
    bool isnewkey;
    int capacity;
    TabEntry* entry;
    // make sure array is big enough
    if(table->count + 1 > table->capacity * TABLE_MAX_LOAD)
    {
        capacity = GROW_CAPACITY(table->capacity);
        fei_table_adjustcapacity(state, table, capacity);
    }
    entry = fei_table_findentry(state, table->entries, table->capacity, key);
    isnewkey = entry->key == NULL;
    if(isnewkey && fei_value_isnull(entry->value))
    {
        // fei_value_isnull for tombstones; treat them as full objects
        table->count++;
    }
    entry->key = key;
    entry->value = value;
    return isnewkey;
}


bool fei_table_delete(State* state, Table* table, ObjString* key)
{
    TabEntry* entry;
    if(table->count == 0)
    {
        return false;
    }
    // find entry
    entry = fei_table_findentry(state, table->entries, table->capacity, key);
    if(entry->key == NULL)
    {
        return false;
    }
    // place tombstone
    entry->key = NULL;
    entry->value = BOOL_VAL(true);//BOOL_VAL(true) as the tombstone
    return true;
}

void fei_table_mergefrom(State* state, Table* from, Table* to)
{
    int i;
    TabEntry* entry;
    for(i = 0; i < from->capacity; i++)
    {
        entry = &from->entries[i];
        if(entry->key != NULL)
        {
            fei_table_set(state, to, entry->key, entry->value);
        }
    }
}

// used in VM to find the string
ObjString* fei_table_findstring(State* state, Table* table, const char* chars, int length, uint32_t hash)// pass in raw character array
{
    uint32_t index;
    TabEntry* entry;
    (void)state;
    if(table->count == 0)
    {
        return NULL;
    }
    index = hash % table->capacity;
    for(;;)
    {
        entry = &table->entries[index];
        if(entry->key == NULL)
        {
            // stop if found empty non-tombstone entry
            if(fei_value_isnull(entry->value))
            {
                // return null if not tombstone(tombstone value is BOOL_VAL(true))
                return NULL;
            }
        }
        else if((entry->key->length == length) && (entry->key->hash == hash) && (memcmp(entry->key->chars, chars, length) == 0))
        {
            // found the entry
            return entry->key;
        }
        index = (index + 1) % table->capacity;
    }
    return NULL;
}

// removing unreachable pointers, used to remove string interns in garbage collection
void fei_table_removeunreachable(State* state, Table* table)
{
    int i;
    TabEntry* entry;
    for(i = 0; i < table->capacity; i++)
    {
        entry = &table->entries[i];
        if(entry->key != NULL && !entry->key->obj.ismarked)// remove not marked (string) object pointers
        {
            fei_table_delete(state, table, entry->key);
        }
    }
}


// mark global variables, used in VM for garbage collection
void fei_table_mark(State* state, Table* table)
{
    int i;
    TabEntry* entry;
    for(i = 0; i < table->capacity; i++)
    {
        entry = &table->entries[i];
        // need to mark both the STRING KEYS and the actual value/obj itself
        // mark the string key(ObjString type)
        fei_gcmem_markobject(state, (Object*)entry->key);
        // mark the actual avlue
        fei_gcmem_markvalue(state, entry->value);
    }
}

void fei_chunk_init(State* state, Chunk* chunk)
{
    chunk->count = 0;
    chunk->capacity = 0;
    // dynamic array starts off completely empty
    chunk->code = NULL;
    // to store current line of code
    chunk->lines = NULL;
    // initialize constant list
    fei_valarray_init(state, &chunk->constants);
}

void fei_chunk_pushbyte(State* state, Chunk* chunk, uint8_t byte, int line)
{
    int oldcapacity;
    // check if chunk is full
    if(chunk->capacity < chunk->count + 1)
    {
        oldcapacity = chunk->capacity;
        // get size of new capacity
        chunk->capacity = GROW_CAPACITY(oldcapacity);
        // reallocate memory and grow array
        chunk->code = (uint8_t*)GROW_ARRAY(state, sizeof(uint8_t), chunk->code, oldcapacity, chunk->capacity);
        chunk->lines = (int*)GROW_ARRAY(state, sizeof(int), chunk->lines, oldcapacity, chunk->capacity);
    }
    // code is an array, [] is just the index number
    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}


void fei_chunk_destroy(State* state, Chunk* chunk)
{
    // chunk->code is the pointer to the array, capacity is the size
    FREE_ARRAY(state, sizeof(uint8_t), chunk->code, chunk->capacity);
    FREE_ARRAY(state, sizeof(int), chunk->lines, chunk->capacity);
    fei_valarray_destroy(state, &chunk->constants);
    fei_chunk_init(state, chunk);
}

int fei_chunk_pushconst(State* state, Chunk* chunk, Value value)
{
    // garbage collection
    fei_vm_pushvalue(state, value);
    fei_valarray_push(state, &chunk->constants, value);
    // garbage collection
    fei_vm_popvalue(state);
    // return index of the newly added constant
    return fei_valarray_count(&chunk->constants) - 1;
}

int fei_dbgutil_printsimpleir(State* state, const char* name, int offset)
{
    (void)state;
    // print as a string, or char*
    printf("%s\n", name);
    return offset + 1;
}

int fei_dbgutil_printbyteir(State* state, const char* name, Chunk* chunk, int offset)
{
    uint8_t slot;
    (void)state;
    slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}

int fei_dbgutil_printconstir(State* state, const char* name, Chunk* chunk, int offset)
{
    uint8_t constant;
    // pullout the constant index from the subsequent byte in the chunk
    constant = chunk->code[offset + 1];
    // print out name of the opcode, then the constant index
    printf("%-16s %4d '", name, constant);
    //	display the value of the constant,  user defined function
    fei_value_printvalue(state, fei_valarray_get(state, &chunk->constants, constant));
    printf("'\n");
    //OP_RETURN is a single byte, and the other byte is the operand, hence offsets by 2
    return offset + 2;
}

int fei_dbgutil_printinvokeir(State* state, const char* name, Chunk* chunk, int offset)
{
    uint8_t constant;
    uint8_t argcount;
    // get index of the name first
    constant = chunk->code[offset + 1];
    // then get number of arguments
    argcount = chunk->code[offset + 2];
    printf("%-16s (%d args) %4d", name, argcount, constant);
    // print the method
    fei_value_printvalue(state, fei_valarray_get(state, &chunk->constants, constant));
    printf("\n");
    return offset + 3;
}

int fei_dbgutil_printjumpir(State* state, const char* name, int sign, Chunk* chunk, int offset)
{
    uint16_t jump;
    (void)state;
    jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

void fei_dbgdisas_chunk(State* state, Chunk* chunk, const char* name)
{
    int offset;
    // print a little header for debugging
    printf("== %s ==\n", name);
    // for every existing instruction in the chunk
    for(offset = 0; offset < chunk->count;)
    {
        // disassemble individually, offset will be controlled from this function
        offset = fei_dbgdisas_instr(state, chunk, offset);
    }
}

int fei_dbgdisas_instr(State* state, Chunk* chunk, int offset)
{
    int j;
    int index;
    int islocal;
    uint8_t constant;
    uint8_t instruction;
    ObjFunction* function;
    // print byte offset of the given instruction, or the index
    printf("%04d ", offset);
    // show source line each instruction was compiled from
    if(offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1])// show a | for any instruction that comes from the
    //same source as its preceding one
    {
        printf("    | ");
    }
    else
    {
        printf("%4d ", chunk->lines[offset]);
    }
    instruction = chunk->code[offset];// takes one byte, or an element, from the container
    switch(instruction)
    {
        case OP_CONSTANT:
            {
                // pass in chunk to get ValArray element
                return fei_dbgutil_printconstir(state, "OP_CONSTANT", chunk, offset);
            }
            break;
        // literals
        case OP_NULL:
            {
                return fei_dbgutil_printsimpleir(state, "OP_NULL", offset);
            }
            break;
        case OP_TRUE:
            return fei_dbgutil_printsimpleir(state, "OP_TRUE", offset);
        case OP_FALSE:
            return fei_dbgutil_printsimpleir(state, "OP_FALSE", offset);

        case OP_EQUAL:
            return fei_dbgutil_printsimpleir(state, "OP_EQUAL", offset);
        case OP_GREATER:
            return fei_dbgutil_printsimpleir(state, "OP_GREATER", offset);
        case OP_LESS:
            return fei_dbgutil_printsimpleir(state, "OP+LESS", offset);

        // unary
        case OP_NEGATE:
            return fei_dbgutil_printsimpleir(state, "OP_NEGATE", offset);

        // binary
        case OP_ADD:
            return fei_dbgutil_printsimpleir(state, "OP_ADD", offset);
        case OP_SUBTRACT:
            return fei_dbgutil_printsimpleir(state, "OP_MINUS", offset);
        case OP_MULTIPLY:
            return fei_dbgutil_printsimpleir(state, "OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return fei_dbgutil_printsimpleir(state, "OP_DIVIDE", offset);
        case OP_MODULO:
            return fei_dbgutil_printsimpleir(state, "OP_MODULO", offset);

        case OP_NOT:
            return fei_dbgutil_printsimpleir(state, "OP_NOT", offset);

        case OP_POP:
            return fei_dbgutil_printsimpleir(state, "OP_POP", offset);

        // names for local variables do not get carried over, hence only the slot number is shown
        case OP_GET_LOCAL:
            return fei_dbgutil_printbyteir(state, "OP_GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return fei_dbgutil_printbyteir(state, "OP_SET_LOCAL", chunk, offset);

        case OP_GET_UPVALUE:
            return fei_dbgutil_printbyteir(state, "OP_GET_UPVALUE", chunk, offset);
        case OP_SET_UPVALUE:
            return fei_dbgutil_printbyteir(state, "OP_SET_UPVALUE", chunk, offset);
        case OP_GET_PROPERTY:
            return fei_dbgutil_printconstir(state, "OP_GET_PROPERTY", chunk, offset);
        case OP_SET_PROPERTY:
            return fei_dbgutil_printconstir(state, "OP_SET_PROPERTY", chunk, offset);

        case OP_CLOSE_UPVALUE:
            return fei_dbgutil_printsimpleir(state, "OP_CLOSE_VALUE", offset);

        case OP_DEFINE_GLOBAL:
            return fei_dbgutil_printsimpleir(state, "OP_DEFINE_GLOBAL", offset);
        case OP_GET_GLOBAL:
            return fei_dbgutil_printsimpleir(state, "OP_GET_GLOBAL", offset);
        case OP_SET_GLOBAL:
            return fei_dbgutil_printsimpleir(state, "OP_SET_GLOBAL", offset);
        case OP_PRINT:
            return fei_dbgutil_printsimpleir(state, "OP_PRINT", offset);

        case OP_SWITCH_EQUAL:
            return fei_dbgutil_printsimpleir(state, "OP_SWITCH_EQUAL", offset);

        case OP_JUMP:
            return fei_dbgutil_printjumpir(state, "OP_JUMP", 1, chunk, offset);
        case OP_JUMP_IF_FALSE:
            return fei_dbgutil_printjumpir(state, "OP_JUMP_IF_FALSE", 1, chunk, offset);

        case OP_CALL:
            return fei_dbgutil_printbyteir(state, "OP_CALL", chunk, offset);

        case OP_METHOD:
            return fei_dbgutil_printconstir(state, "OP_METHOD", chunk, offset);

        case OP_INVOKE:
            return fei_dbgutil_printinvokeir(state, "OP_INVOKE", chunk, offset);

        case OP_CLOSURE:
            {
                offset++;
                constant = chunk->code[offset++];// index for Value
                printf("%-16s %4d ", "OP_CLOSURE", constant);
                fei_value_printvalue(state, fei_valarray_get(state, &chunk->constants, constant));// accessing the value using the index
                printf("\n");
                function = fei_value_asfunction(fei_valarray_get(state, &chunk->constants, constant));
                for(j = 0; j < function->upvaluecount; j++)// walk through upvalues
                {
                    islocal = chunk->code[offset++];
                    index = chunk->code[offset++];
                    printf("%04d	|	%s %d\n", offset - 2, islocal ? "local" : "upvalue", index);
                }
                return offset;
            }
            break;
        case OP_CLASS:
            return fei_dbgutil_printconstir(state, "OP_CLASS", chunk, offset);

        case OP_INHERIT:
            return fei_dbgutil_printsimpleir(state, "OP_INEHEIRT", offset);


        case OP_GET_SUPER:// class inheritance
            return fei_dbgutil_printconstir(state, "OP_GET_SUPER", chunk, offset);

        case OP_SUPER_INVOKE:
            return fei_dbgutil_printinvokeir(state, "OP_SUPER_INVOKE", chunk, offset);

        case OP_RETURN:
            return fei_dbgutil_printsimpleir(state, "OP_RETURN", offset);// dispatch to a utility function to display it

        case OP_LOOP:
            return fei_dbgutil_printjumpir(state, "OP_LOOP", -1, chunk, offset);

        case OP_LOOP_IF_TRUE:
            return fei_dbgutil_printjumpir(state, "OP_LOOP_IF_TRUE", -1, chunk, offset);

        case OP_LOOP_IF_FALSE:
            return fei_dbgutil_printjumpir(state, "OP_LOOP_IF_FALSE", -1, chunk, offset);

        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}


void fei_lexer_initsource(State* state, const char* source, size_t len)
{
    memset(&state->aststate.scanner, 0, sizeof(Scanner));
    state->aststate.scanner.startsrc = source;
    state->aststate.scanner.currentsrc = source;
    state->aststate.scanner.length = len;
    state->aststate.scanner.line = 1;
}

// to check for identifiers(eg. for, while, print)
bool fei_lexutil_isalpha(State* state, char c)
{
    (void)state;
    return (
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c == '_')
    );
}

bool fei_lexutil_isdigit(State* state, char c)
{
    (void)state;
    // let string comparison handle it
    return (
        (c >= '0') && (c <= '9')
    );
}

// to get EOF symbol -> '\0'
bool fei_lexer_isatend(State* state)
{
    return *state->aststate.scanner.currentsrc == '\0';
}

// goes to next char
char fei_lexer_advance(State* state)
{
    state->aststate.scanner.currentsrc++;// advance to next
    return state->aststate.scanner.currentsrc[-1];// return previous one
}

// logical conditioning to check if 2nd character is embedded to first(e.g two char token)
bool fei_lexer_match(State* state, char expected)
{
    if(fei_lexer_isatend(state))
    {
        return false;
    }
    if(*state->aststate.scanner.currentsrc != expected)
    {
        return false;
    }
    state->aststate.scanner.currentsrc++;
    return true;
}

// make a token, uses the scanner's start and current to capture the lexeme and its size
Token fei_lexer_maketoken(State* state, TokType type)
{
    Token token;
    token.type = type;
    token.toksrc = state->aststate.scanner.startsrc;
    token.length = (int)(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc);
    token.line = state->aststate.scanner.line;
    return token;
}

// similar to fei_lexer_maketoken, but the "lexeme" points to the error message string instead of the scanner's current and start pointers
Token fei_lexer_errortoken(State* state, const char* message)
{
    Token token;
    token.type = TOKEN_ERROR;
    token.toksrc = message;
    token.length = (int)strlen(message);
    token.line = state->aststate.scanner.line;
    return token;
}

// returns current character
char fei_lexer_peekcurrent(State* state)
{
    return *state->aststate.scanner.currentsrc;
}

// returns next character
char fei_lexer_peeknext(State* state)
{
    if(fei_lexer_isatend(state))
    {
        return '\0';
    }
    return state->aststate.scanner.currentsrc[1];
}

// skipping white spaces, tabs, etc.
void fei_lexer_skipspace(State* state)
{
    char c;
    for(;;)
    {
        c = fei_lexer_peekcurrent(state);
        switch(c)
        {
            case ' ':
            case '\r':
            case '\t':
                fei_lexer_advance(state);
                break;

            case '\n':// if a new line is found, also add line number
                state->aststate.scanner.line++;
                fei_lexer_advance(state);
                break;

            // for comments
            case '/':
                if(fei_lexer_peeknext(state) == '/')
                {
                    // comment goes until end of line
                    while(fei_lexer_peekcurrent(state) != '\n' && !fei_lexer_isatend(state))
                        fei_lexer_advance(state);// if not new line or not end, treat as whitespace and advance
                }
                else
                {
                    return;
                }

            default:
                return;
        }
    }
}


// to check for identifiers, if they are keyword or not. rest means the rest of the letter
TokType fei_lexer_checkkw(State* state, int start, int length, const char* rest, TokType type)
{
    /*
    * hard expression here
    * bascially if they are exactly the same, and compares their memory(memcmp)
    * int memcmp(const void *str1, const void *str2, size_t n) -> if it is exactly the same, then it is 0
    */
    if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc == start + length && memcmp(state->aststate.scanner.startsrc + start, rest, length) == 0)
    {
        return type;
    }
    return TOKEN_IDENTIFIER;
}

// the 'trie' to store the set of strings
TokType fei_lexer_scantype(State* state)
{
    /*
    * NP: the commented out bits do NOT work, because they
    * would happily gobble up valid identifiers.
    * (i.e., "do_foo" would also match "do", resulting in an invalid parser state)
    */
    /*
    static struct
    {
        const char* str;
        int type;
    } keywords[] = {
        {"and", TOKEN_KWAND},
        {"assigned", TOKEN_ASSIGN},
        {"equals", TOKEN_ASSIGN},
        {"is", TOKEN_EQUAL},
        {"break", TOKEN_KWBREAK},
        {"class", TOKEN_KWCLASS},
        {"case", TOKEN_KWCASE},
        {"continue", TOKEN_KWCONTINUE},
        {"switch", TOKEN_KWSWITCH},
        {"default", TOKEN_KWDEFAULT},
        {"do", TOKEN_KWDO},
        {"else", TOKEN_KWELSE},
        {"elif", TOKEN_KWELF},
        {"if", TOKEN_KWIF},
        {"for", TOKEN_KWFOR},
        {"while", TOKEN_KWWHILE},
        {"function", TOKEN_KWFUN},
        {"from", TOKEN_KWFROM},
        {"null", TOKEN_KWNULL},
        {"or", TOKEN_KWOR},
        {"return", TOKEN_KWRETURN},
        {"repeat", TOKEN_KWREPEAT},
        {"until", TOKEN_KWUNTIL},
        {"var", TOKEN_KWVAR},
        {NULL, 0},
    };
    int i;
    for(i=0; keywords[i].str != NULL; i++)
    {
        int len = strlen(keywords[i].str);
        if(memcmp(keywords[i].str, state->aststate.scanner.startsrc, len) == 0)
        {
            fprintf(stderr, "start=<%.*s>\n", len, state->aststate.scanner.startsrc);
            return keywords[i].type;
        }
    }
    return TOKEN_IDENTIFIER;
    */

    switch(state->aststate.scanner.startsrc[0])// start of the lexeme
    {
        //case 'a': return fei_lexer_checkkw(state, 1, 2, "nd", TOKEN_KWAND);
        case 'a':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)
                {
                    switch(state->aststate.scanner.startsrc[1])
                    {
                        case 'n':
                            {
                                return fei_lexer_checkkw(state, 2, 1, "d", TOKEN_KWAND);
                            }
                            break;
                        case 's':
                            {
                                return fei_lexer_checkkw(state, 2, 6, "signed", TOKEN_ASSIGN);
                            }
                            break;
                    }
                }
            }
            break;
        case 'b':
            {
                return fei_lexer_checkkw(state, 1, 4, "reak", TOKEN_KWBREAK);
            }
            break;
        case 'c':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)
                {
                    switch(state->aststate.scanner.startsrc[1])
                    {
                        case 'a':
                            {
                                return fei_lexer_checkkw(state, 2, 2, "se", TOKEN_KWCASE);
                            }
                            break;
                        case 'l':
                            {
                                return fei_lexer_checkkw(state, 2, 3, "ass", TOKEN_KWCLASS);
                            }
                            break;
                        case 'o':
                            {
                                return fei_lexer_checkkw(state, 2, 6, "ntinue", TOKEN_KWCONTINUE);
                            }
                            break;
                    }
                }
            }
            break;
        case 'd':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)
                {
                    switch(state->aststate.scanner.startsrc[1])
                    {
                        case 'e':
                            {
                                return fei_lexer_checkkw(state, 2, 5, "fault", TOKEN_KWDEFAULT);
                            }
                            break;
                        case 'o':
                            {
                                return fei_lexer_checkkw(state, 2, 0, "", TOKEN_KWDO);
                            }
                            break;
                    }
                }
            }
            break;
        case 'e':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)
                {
                    switch(state->aststate.scanner.startsrc[1])// check if there is a second letter
                    {
                        case 'l':
                            {
                                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 2)// check if there is a third letter
                                {
                                    switch(state->aststate.scanner.startsrc[2])
                                    {
                                        case 's':
                                            {
                                                return fei_lexer_checkkw(state, 3, 1, "e", TOKEN_KWELSE);
                                            }
                                            break;
                                        case 'f':
                                            {
                                                return fei_lexer_checkkw(state, 3, 0, "", TOKEN_KWELF);// already matched
                                            }
                                            break;
                                    }
                                }
                            }
                            break;
                        case 'q':
                            {
                                return fei_lexer_checkkw(state, 2, 4, "uals", TOKEN_EQUAL);
                            }
                            break;
                    }
                }
            }
            break;
        case 'f':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)// check if there is a second letter
                {
                    switch(state->aststate.scanner.startsrc[1])
                    {
                        case 'a':
                            {
                                return fei_lexer_checkkw(state, 2, 3, "lse", TOKEN_KWFALSE);// starts from 2 not 3, as first letter is already an f
                            }
                            break;
                        case 'o':
                            {
                                return fei_lexer_checkkw(state, 2, 1, "r", TOKEN_KWFOR);
                            }
                            break;
                        case 'r':
                            {
                                return fei_lexer_checkkw(state, 2, 2, "om", TOKEN_KWFROM);
                            }
                            break;
                        case 'n':
                            {
                                return fei_lexer_checkkw(state, 2, 0, "", TOKEN_KWFUN);
                            }
                            break;
                        case 'u':
                            {
                                return fei_lexer_checkkw(state, 2, 6, "nction", TOKEN_KWFUN);
                            }
                            break;
                    }
                }
            }
            break;
        case 'i':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)
                {
                    switch(state->aststate.scanner.startsrc[1])
                    {
                        case 'f':
                            {
                                return fei_lexer_checkkw(state, 2, 0, "", TOKEN_KWIF);
                            }
                            break;
                        case 's':
                            {
                                return fei_lexer_checkkw(state, 2, 0, "", TOKEN_EQUAL);
                            }
                            break;
                    }
                }
            }
            break;
        case 'n':
            {
                return fei_lexer_checkkw(state, 1, 3, "ull", TOKEN_KWNULL);
            }
            break;
        case 'o':
            {
                return fei_lexer_checkkw(state, 1, 1, "r", TOKEN_KWOR);
            }
            break;
        case 'p':
            {
                return fei_lexer_checkkw(state, 1, 7, "rint__keyword", TOKEN_KWPRINT);
            }
            break;
        //case 'r': return fei_lexer_checkkw(state, 1, 5, "eturn", TOKEN_KWRETURN);
        case 'r':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)
                {
                    switch(state->aststate.scanner.startsrc[1])
                    {
                        case 'e':
                            {
                                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 2)
                                {
                                    switch(state->aststate.scanner.startsrc[2])
                                    {
                                        case 't':
                                            {
                                                return fei_lexer_checkkw(state, 3, 3, "urn", TOKEN_KWRETURN);
                                            }
                                            break;
                                        case 'p':
                                            {
                                                return fei_lexer_checkkw(state, 3, 3, "eat", TOKEN_KWREPEAT);
                                            }
                                            break;
                                    }
                                }
                            }
                            break;
                    }
                }
            }
            break;
        case 's':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)// if there is a second letter
                {
                    switch(state->aststate.scanner.startsrc[1])
                    {
                        case 'u':
                            {
                                return fei_lexer_checkkw(state, 2, 3, "per", TOKEN_KWSUPER);
                            }
                            break;
                        case 'w':
                            {
                                return fei_lexer_checkkw(state, 2, 4, "itch", TOKEN_KWSWITCH);
                            }
                            break;
                    }
                }
            }
            break;
        case 't':
            {
                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 1)
                {
                    switch(state->aststate.scanner.startsrc[1])
                    {
                        //case 'h': return fei_lexer_checkkw(state, 2, 2, "is", TOKEN_KWTHIS);
                        case 'h':
                            {
                                if(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc > 2)// check if there is a third letter
                                {
                                    switch(state->aststate.scanner.startsrc[2])
                                    {
                                        case 'i':
                                            {
                                                return fei_lexer_checkkw(state, 3, 1, "s", TOKEN_KWTHIS);// already matched
                                            }
                                            break;
                                    }
                                }
                            }
                            break;
                        case 'r':
                            {
                                return fei_lexer_checkkw(state, 2, 2, "ue", TOKEN_KWTRUE);
                            }
                            break;
                    }
                }
            }
            break;
        case 'u':
            {
                return fei_lexer_checkkw(state, 1, 4, "ntil", TOKEN_KWUNTIL);
            }
            break;
        case 'v':
            {
                return fei_lexer_checkkw(state, 1, 2, "ar", TOKEN_KWVAR);
            }
            break;
        case 'w':
            {
                return fei_lexer_checkkw(state, 1, 4, "hile", TOKEN_KWWHILE);
            }
            break;
    }
    return TOKEN_IDENTIFIER;
}

Token fei_lexer_scanident(State* state)
{
    while(fei_lexutil_isalpha(state, fei_lexer_peekcurrent(state)) || fei_lexutil_isdigit(state, fei_lexer_peekcurrent(state)))
    {
        // skip if still letters or digits
        fei_lexer_advance(state);
    }
    return fei_lexer_maketoken(state, fei_lexer_scantype(state));
}

Token fei_lexer_scannumber(State* state)
{
    while(fei_lexutil_isdigit(state, fei_lexer_peekcurrent(state)))
    {
        // while next is still a digit advance
        fei_lexer_advance(state);
    }
    // look for fractional part
    // if there is a . and next is still digit
    if(fei_lexer_peekcurrent(state) == '.' && fei_lexutil_isdigit(state, fei_lexer_peeknext(state)))
    {
        // consume '.'
        fei_lexer_advance(state);
        while(fei_lexutil_isdigit(state, fei_lexer_peekcurrent(state)))
        {
            fei_lexer_advance(state);
        }
    }
    return fei_lexer_maketoken(state, TOKEN_NUMBER);
}

// for string tokens
Token fei_lexer_scanstring(State* state)
{
    while(fei_lexer_peekcurrent(state) != '"' && !fei_lexer_isatend(state))
    {
        if(fei_lexer_peekcurrent(state) == '\n')
            state->aststate.scanner.line++;// allow strings to go until next line
        fei_lexer_advance(state);// consume characters until the closing quote is reached
    }

    if(fei_lexer_isatend(state))
        return fei_lexer_errortoken(state, "Unterminated string.");

    // closing quote
    fei_lexer_advance(state);
    return fei_lexer_maketoken(state, TOKEN_STRING);

    // convert lexeme to runtime value later
}

// reading the char, and return a token
Token fei_lexer_scantoken(State* state)
{
    fei_lexer_skipspace(state);

    state->aststate.scanner.startsrc = state->aststate.scanner.currentsrc;// reset the scanner to current

    if(fei_lexer_isatend(state))
        return fei_lexer_maketoken(state, TOKEN_EOF);// check if at end

    // if not end of file
    char c = fei_lexer_advance(state);

    if(fei_lexutil_isalpha(state, c))
        return fei_lexer_scanident(state);
    if(fei_lexutil_isdigit(state, c))
        return fei_lexer_scannumber(state);// fei_lexer_scannumber() is a TOKEN_NUMBER


    // lexical grammar for the language
    switch(c)
    {
        // for single characters
        case '(':
            return fei_lexer_maketoken(state, TOKEN_LEFTPAREN);
        case ')':
            return fei_lexer_maketoken(state, TOKEN_RIGHTPAREN);
        case '{':
            return fei_lexer_maketoken(state, TOKEN_LEFTBRACE);
        case '}':
            return fei_lexer_maketoken(state, TOKEN_RIGHTBRACE);
        case ';':
            return fei_lexer_maketoken(state, TOKEN_SEMICOLON);
        case ':':
            return fei_lexer_maketoken(state, TOKEN_COLON);
        case ',':
            return fei_lexer_maketoken(state, TOKEN_COMMA);
        case '.':
            return fei_lexer_maketoken(state, TOKEN_DOT);
        case '-':
            return fei_lexer_maketoken(state, TOKEN_MINUS);
        case '+':
            return fei_lexer_maketoken(state, TOKEN_PLUS);
        case '*':
            return fei_lexer_maketoken(state, TOKEN_STAR);
        case '/':
            return fei_lexer_maketoken(state, TOKEN_SLASH);
        case '%':
            {
                return fei_lexer_maketoken(state, TOKEN_MODULO);
            }
            break;
        // for two characters
        case '!':
            {
                if(fei_lexer_match(state, '='))
                {
                    return fei_lexer_maketoken(state, TOKEN_NOTEQUAL);
                }
                else
                {
                    return fei_lexer_maketoken(state, TOKEN_NOTEQUAL);
                }
            }
            break;
        case '=':
            {
                if(fei_lexer_match(state, '='))
                {
                    return fei_lexer_maketoken(state, TOKEN_EQUAL);
                }
                else
                {
                    return fei_lexer_maketoken(state, TOKEN_ASSIGN);
                }
            }
            break;
        case '>':
            {
                if(fei_lexer_match(state, '='))
                {
                    return fei_lexer_maketoken(state, TOKEN_GREATEREQUAL);
                }
                else
                {
                    return fei_lexer_maketoken(state, TOKEN_GREATERTHAN);
                }
            }
            break;
        case '<':
            {
                if(fei_lexer_match(state, '='))
                {
                    return fei_lexer_maketoken(state, TOKEN_LESSEQUAL);
                }
                else
                {
                    return fei_lexer_maketoken(state, TOKEN_LESSTHAN);
                }
            }
            // literal tokens
        case '"':
            {
                return fei_lexer_scanstring(state);
            }
    }
    return fei_lexer_errortoken(state, "Unexpected character.");
}


static void fei_comprule_logicaland(State* state, bool canassign);
static void fei_comprule_binary(State* state, bool canassign);
static void fei_comprule_call(State* state, bool canassign);
static void fei_comprule_dot(State* state, bool canassign);
static void fei_comprule_literal(State* state, bool canassign);
static void fei_comprule_grouping(State* state, bool canassign);
static void fei_comprule_number(State* state, bool canassign);
static void fei_comprule_logicalor(State* state, bool canassign);
static void fei_comprule_string(State* state, bool canassign);
static void fei_comprule_variable(State* state, bool canassign);
static void fei_comprule_super(State* state, bool canassign);
static void fei_comprule_this(State* state, bool canassign);
static void fei_comprule_unary(State* state, bool canassign);


/* the array of ParseRules 
uses C99 DESIGNATED INITIALIZER syntax
use {struct members} to initialize a struct
[index number] = {struct members}, the index number can be seen clearly
token enums from scanner is reused
*/
ParseRule rules[] = {
    // function calls are like infixes, with high precedence on the left, ( in the middle for arguments, then ) at the end
    [TOKEN_LEFTPAREN] = { fei_comprule_grouping, fei_comprule_call, PREC_CALL },// call for functions
    [TOKEN_RIGHTPAREN] = { NULL, NULL, PREC_NONE },
    [TOKEN_LEFTBRACE] = { NULL, NULL, PREC_NONE },
    [TOKEN_RIGHTBRACE] = { NULL, NULL, PREC_NONE },
    [TOKEN_COMMA] = { NULL, NULL, PREC_NONE },
    [TOKEN_DOT] = { NULL, fei_comprule_dot, PREC_CALL },
    [TOKEN_MINUS] = { fei_comprule_unary, fei_comprule_binary, PREC_TERM },
    [TOKEN_PLUS] = { NULL, fei_comprule_binary, PREC_TERM },
    [TOKEN_SEMICOLON] = { NULL, NULL, PREC_NONE },
    [TOKEN_SLASH] = { NULL, fei_comprule_binary, PREC_FACTOR },
    [TOKEN_STAR] = { NULL, fei_comprule_binary, PREC_FACTOR },
    [TOKEN_MODULO] = { NULL, fei_comprule_binary, PREC_FACTOR },
    [TOKEN_LOGICALNOT] = { fei_comprule_unary, NULL, PREC_NONE },
    [TOKEN_NOTEQUAL] = { NULL, fei_comprule_binary, PREC_EQUALITY },// equality precedence
    [TOKEN_ASSIGN] = { NULL, fei_comprule_binary, PREC_COMPARISON },// comaprison precedence
    [TOKEN_EQUAL] = { NULL, fei_comprule_binary, PREC_COMPARISON },
    [TOKEN_GREATERTHAN] = { NULL, fei_comprule_binary, PREC_COMPARISON },
    [TOKEN_GREATEREQUAL] = { NULL, fei_comprule_binary, PREC_COMPARISON },
    [TOKEN_LESSTHAN] = { NULL, fei_comprule_binary, PREC_COMPARISON },
    [TOKEN_LESSEQUAL] = { NULL, fei_comprule_binary, PREC_COMPARISON },
    [TOKEN_IDENTIFIER] = { fei_comprule_variable, NULL, PREC_NONE },
    [TOKEN_STRING] = { fei_comprule_string, NULL, PREC_NONE },
    [TOKEN_NUMBER] = { fei_comprule_number, NULL, PREC_NONE },
    [TOKEN_KWAND] = { NULL, fei_comprule_logicaland, PREC_AND },
    [TOKEN_KWCLASS] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWELSE] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWFALSE] = { fei_comprule_literal, NULL, PREC_NONE },
    [TOKEN_KWFOR] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWFUN] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWIF] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWSWITCH] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWNULL] = { fei_comprule_literal, NULL, PREC_NONE },
    [TOKEN_KWOR] = { NULL, fei_comprule_logicalor, PREC_OR },
    [TOKEN_KWPRINT] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWRETURN] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWSUPER] = { fei_comprule_super, NULL, PREC_NONE },
    [TOKEN_KWTHIS] = { fei_comprule_this, NULL, PREC_NONE },
    [TOKEN_KWTRUE] = { fei_comprule_literal, NULL, PREC_NONE },
    [TOKEN_KWVAR] = { NULL, NULL, PREC_NONE },
    [TOKEN_KWWHILE] = { NULL, NULL, PREC_NONE },
    [TOKEN_ERROR] = { NULL, NULL, PREC_NONE },
    [TOKEN_EOF] = { NULL, NULL, PREC_NONE },
};


Chunk* fei_compiler_currentchunk(State* state)
{
    return &state->aststate.compiler->programfunc->chunk;
}

// to handle syntax errors
void fei_compiler_raiseat(State* state, Token* token, const char* message)
{
    if(state->aststate.parser.panicmode)
        return;// if an error already exists, no need to run other errors
    state->aststate.parser.panicmode = true;

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
        fprintf(stderr, " at '%.*s'", token->length, token->toksrc);
    }

    fprintf(stderr, ": %s\n", message);
    state->aststate.parser.haderror = true;
}

// error from token most recently CONSUMED
void fei_compiler_raiseerror(State* state, const char* message)
{
    fei_compiler_raiseat(state, &state->aststate.parser.prevtoken, message);
}


// handling error from token, the most current one being handed, not yet consumed
void fei_compiler_raisehere(State* state, const char* message)// manually provide the message
{
    fei_compiler_raiseat(state, &state->aststate.parser.currtoken, message);// pass in the current parser
}


/* main compile functions */

// pump the compiler, basically go to / 'read' the next token, a SINGLE token
void fei_compiler_advancenext(State* state)
{
    state->aststate.parser.prevtoken = state->aststate.parser.currtoken;//  store next parser as current

    for(;;)
    {
        state->aststate.parser.currtoken = fei_lexer_scantoken(state);// gets next token, stores it for later use(the next scan)

        if(state->aststate.parser.currtoken.type != TOKEN_ERROR)
            break;// if error is not found break

        fei_compiler_raisehere(state, state->aststate.parser.currtoken.toksrc);// start is the location/pointer of the token source code
    }
}


// advance while skipping the given parameter, give none to skip nothing
void fei_compiler_advanceskipping(State* state, TokType type)
{
    state->aststate.parser.prevtoken = state->aststate.parser.currtoken;//  store next parser as current

    for(;;)
    {
        state->aststate.parser.currtoken = fei_lexer_scantoken(state);// gets next token, stores it for later use(the next scan)

        if(state->aststate.parser.currtoken.type == type)
            continue;

        if(state->aststate.parser.currtoken.type != TOKEN_ERROR)
            break;// if error is not found break

        fei_compiler_raisehere(state, state->aststate.parser.currtoken.toksrc);// start is the location/pointer of the token source code
    }
}


// SIMILAR to advance but there is a validation for a certain type
// syntax error comes from here, where it is known/expected what the next token will be
void fei_compiler_consume(State* state, TokType type, const char* message)
{
    if(state->aststate.parser.currtoken.type == type)// if current token is equal to the token type being compared to
    {
        fei_compiler_advancenext(state);
        return;
    }

    fei_compiler_raisehere(state, message);// if consumes a different type, error
}

bool fei_compiler_check(State* state, TokType type)
{
    return state->aststate.parser.currtoken.type == type;// check if current matches given
}


bool fei_compiler_match(State* state, TokType type)
{
    if(!fei_compiler_check(state, type))
        return false;
    fei_compiler_advancenext(state);
    return true;
}

/* emitting BYTECODE for the VM to understand */
// the fei_chunk_pushbyte for the compiler
void fei_compiler_emitbyte(State* state, uint8_t byte)
{
    fei_chunk_pushbyte(state, fei_compiler_currentchunk(state), byte, state->aststate.parser.prevtoken.line);// sends previous line so runtime errors are associated with that line
}

// write chunk for multiple chunks, used to write an opcode followed by an operand(eg. in constants)
void fei_compiler_emitbytes(State* state, uint8_t byte1, uint8_t byte2)
{
    fei_compiler_emitbyte(state, byte1);
    fei_compiler_emitbyte(state, byte2);
}

// for looping statements
void fei_compiler_emitloop(State* state, int loopstart)
{
    fei_compiler_emitbyte(state, OP_LOOP);

    // int below jumps back, + 2 accounting the OP_LOOP and the instruction's own operand
    int offset = fei_compiler_currentchunk(state)->count - loopstart + 2;
    if(offset > UINT16_MAX)
        fei_compiler_raiseerror(state, "Loop body too large.");

    fei_compiler_emitbyte(state, (offset >> 8) & 0xff);
    fei_compiler_emitbyte(state, offset & 0xff);
}

void fei_compiler_emitcondloop(State* state, int loopstart, bool condstate)
{
    if(condstate)
        fei_compiler_emitbyte(state, OP_LOOP_IF_TRUE);
    else
        fei_compiler_emitbyte(state, OP_LOOP_IF_FALSE);

    int offset = fei_compiler_currentchunk(state)->count - loopstart + 2;
    if(offset > UINT16_MAX)
        fei_compiler_raiseerror(state, "Loop body too large.");

    fei_compiler_emitbyte(state, (offset >> 8) & 0xff);
    fei_compiler_emitbyte(state, offset & 0xff);
}


int fei_compiler_emitjump(State* state, uint8_t instruction)
{
    /* backpatching */
    fei_compiler_emitbyte(state, instruction);// writes a placeholder operand for jump offset
    fei_compiler_emitbyte(state, 0xff);// hexadecimal number with value of 255
    fei_compiler_emitbyte(state, 0xff);

    // basically, get the difference in bytes before the two 0xff is added
    return fei_compiler_currentchunk(state)->count - 2;
}

//  emit specific return type
void fei_compiler_emitreturn(State* state)
{
    if(state->aststate.compiler->progfunctype == TYPE_INITIALIZER)// class constructor
    {
        fei_compiler_emitbytes(state, OP_GET_LOCAL, 0);// return the instance
    }
    else
    {
        fei_compiler_emitbyte(state, OP_NULL);// for functions that return nothing
    }

    fei_compiler_emitbyte(state, OP_RETURN);// emit return type at the end of a compiler
}

// to insert into constant table
uint8_t fei_compiler_makeconst(State* state, Value value)
{
    int constant = fei_chunk_pushconst(state, fei_compiler_currentchunk(state), value);
    if(constant > UINT8_MAX)
    {
        fei_compiler_raiseerror(state, "Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;// return as byte, the byte being the INDEX of the constantin the constats array
}

void fei_compiler_emitconst(State* state, Value value)// for constant emit the opcode, then the index
{
    fei_compiler_emitbytes(state, OP_CONSTANT, fei_compiler_makeconst(state, value));// add value to constant table
}

void fei_compiler_patchjump(State* state, int offset)
{
    // - 2 to adjust for the jump offset itself
    int jump = fei_compiler_currentchunk(state)->count - offset - 2;

    if(jump > UINT16_MAX)
    {
        fei_compiler_raiseerror(state, "Too much code to jump over.");
    }

    // the fei_compiler_patchjump provides the VALUE or amount to JUMP
    fei_compiler_currentchunk(state)->code[offset] = (jump >> 8) & 0xff;// right shift by 8, then bitwise AND with 255(oxff is 111111)
    fei_compiler_currentchunk(state)->code[offset + 1] = jump & 0xff;// only AND
}

// initialize the compiler
void fei_compiler_init(State* state, Compiler* compiler, FuncType type)
{
    size_t i;
    Local* local;
    compiler->enclosing = state->aststate.compiler;// the 'outer' compiler
    compiler->programfunc = NULL;
    compiler->progfunctype = type;
    compiler->progloccount = 0;
    compiler->scopedepth = 0;
    compiler->programfunc = fei_object_makefunction(state);
    state->aststate.compiler = compiler;
    if(type != TYPE_SCRIPT)
    {
        state->aststate.compiler->programfunc->name = fei_object_copystring(state, state->aststate.parser.prevtoken.toksrc, state->aststate.parser.prevtoken.length);// function name handled here
    }
    for(i=0; i<CFG_MAX_COMPILERLOCALS; i++)
    {
        memset(&state->aststate.compiler->proglocals[i], 0, sizeof(Local));
    }
    // compiler implicitly claims slot zero for local variables
    local = &state->aststate.compiler->proglocals[state->aststate.compiler->progloccount++];
    local->iscaptured = false;
    // for this tags
    if(type != TYPE_FUNCTION)// for none function types, for class methods
    {
        local->name.toksrc = "this";
        local->name.length = 4;
    }
    else// for functions
    {
        local->name.toksrc = "";
        local->name.length = 0;
    }
    // for loop scopes, for break and continue statements
    compiler->loopcounttop = -1;
    compiler->continuejumpcapacity = 4;
    compiler->continuejumps = (int*)ALLOCATE(state, sizeof(int), 4);
    // use memset to initialize array to 0
    memset(compiler->breakjumpcounts, 0, CFG_MAX_COMPILERBREAK * sizeof(compiler->breakjumpcounts[0]));
}

ObjFunction* fei_compiler_endcompiler(State* state)
{
    fei_compiler_emitreturn(state);
    ObjFunction* function = state->aststate.compiler->programfunc;

    FREE(state, sizeof(int), state->aststate.compiler->continuejumps);


    // for debugging
#if defined(DEBUG_PRINT_CODE) && (DEBUG_PRINT_CODE == 1)
    if(!state->aststate.parser.haderror)
    {
        fei_dbgdisas_chunk(state, fei_compiler_currentchunk(state), function->name != NULL ? function->name->chars : "<script>");// if name is NULL then it is the Script type(main()
    }
#endif

    state->aststate.compiler = state->aststate.compiler->enclosing;// return back to enclosing compiler after function
    return function;// return to free
}

void fei_compiler_beginscope(State* state)
{
    state->aststate.compiler->scopedepth++;
}

void fei_compiler_endscope(State* state)
{
    state->aststate.compiler->scopedepth--;

    // remove variables out of scope
    while(state->aststate.compiler->progloccount > 0 && state->aststate.compiler->proglocals[state->aststate.compiler->progloccount - 1].depth > state->aststate.compiler->scopedepth)
    {
        /* at the end of a block scope, when the compiler emits code to free the stack slot for the locals, 
		tell which one to hoist to the heap
		*/
        if(state->aststate.compiler->proglocals[state->aststate.compiler->progloccount - 1].iscaptured)// if it is captured/used
        {
            fei_compiler_emitbyte(state, OP_CLOSE_UPVALUE);// op code to move the upvalue to the heap
        }
        else
        {
            fei_compiler_emitbyte(state, OP_POP);// if not used anymore/capture simply pop the value off the stack
        }

        state->aststate.compiler->progloccount--;
    }
}

// loop enclosing
void fei_compiler_beginloopscope(State* state)
{
    state->aststate.compiler->loopcounttop++;
}

void fei_compiler_endloopscope(State* state)
{
    if(state->aststate.compiler->breakjumpcounts[state->aststate.compiler->loopcounttop] > 0)
        state->aststate.compiler->breakjumpcounts[state->aststate.compiler->loopcounttop] = 0;

    state->aststate.compiler->loopcounttop--;
}

// mark current chunk for continue jump
void fei_compiler_markcontinuejump(State* state)
{
    state->aststate.compiler->continuejumps[state->aststate.compiler->loopcounttop] = fei_compiler_currentchunk(state)->count;
}

// patch available break jumps
void fei_compiler_patchbreakjumps(State* state)
{
    for(int i = 0; i < state->aststate.compiler->breakjumpcounts[state->aststate.compiler->loopcounttop]; i++)
    {
        fei_compiler_patchjump(state, state->aststate.compiler->breakpatchjumps[state->aststate.compiler->loopcounttop][i]);
    }
}

/* variable declarations */
uint8_t fei_compiler_makeidentconst(State* state, Token* name)
{
    return fei_compiler_makeconst(state, OBJ_VAL(fei_object_copystring(state, name->toksrc, name->length)));// add to constant table
}

bool fei_compiler_identsequal(State* state, Token* a, Token* b)
{
    (void)state;
    if(a->length != b->length)
        return false;
    return memcmp(a->toksrc, b->toksrc, a->length) == 0;
}


int fei_compiler_resolvelocal(State* state, Compiler* compiler, Token* name)
{
    for(int i = compiler->progloccount - 1; i >= 0; i--)// walk through the local variables
    {
        Local* local = &compiler->proglocals[i];
        if(fei_compiler_identsequal(state, name, &local->name))
        {
            if(local->depth == -1)
            {
                fei_compiler_raiseerror(state, "Cannot read local variable in its own initializer.");
            }
            return i;// found the var, return the index
        }
    }

    return -1;// not found, name is global variable
}


// add upvalue
int fei_compiler_addupvalue(State* state, Compiler* compiler, uint8_t index, bool islocal)
{
    int upvaluecount = compiler->programfunc->upvaluecount;// get current upvalue count

    // check whether the upvalue has already been declared
    for(int i = 0; i < upvaluecount; i++)
    {
        Upvalue* upvalue = &compiler->upvalues[i];// get pointer for each upvalue in the array
        if(upvalue->index == index && upvalue->islocalvar == islocal)
        {
            return i;// if found, return the index of the upvalue in the upvalue array
        }
    }

    if(upvaluecount == CFG_MAX_COMPILERUPVALS)
    {
        fei_compiler_raiseerror(state, "Too many closure variables");
        return 0;
    }

    // compiler keeps an array of upvalue structs to track closed-over identifiers
    // indexes in the array match the indexes of ObjClosure at runtime
    // insert to upvalues array
    compiler->upvalues[upvaluecount].islocalvar = islocal;// insert bool status
    compiler->upvalues[upvaluecount].index = index;// insert index
    return compiler->programfunc->upvaluecount++;// increase count and return
}


/*	for closures
- fei_compiler_resolveupvalue looks for a local variable declared in any of the surrounding functions
- if it finds one it returns the index for that upvalue variable, otherwise returns -1
*/
int fei_compiler_resolveupvalue(State* state, Compiler* compiler, Token* name)
{
    if(compiler->enclosing == NULL)
        return -1;// if in main()

    int local = fei_compiler_resolvelocal(state, compiler->enclosing, name);// looks for local value in enclosing function/compiler
    if(local != -1)
    {
        compiler->enclosing->proglocals[local].iscaptured = true;// mark local is captured/used by and upvalue
        return fei_compiler_addupvalue(state, compiler, (uint8_t)local, true);// create up value
    }

    // recursion to solve nested upvalues
    // recursive call right in the middle
    int upvalue = fei_compiler_resolveupvalue(state, compiler->enclosing, name);// if the enclosing function is main() (NULL), it returns -1
    if(upvalue != -1)
    {
        return fei_compiler_addupvalue(state, compiler, (uint8_t)upvalue, true);
    }


    return -1;
}


void fei_compiler_addlocal(State* state, Token name)
{
    if(state->aststate.compiler->progloccount == CFG_MAX_COMPILERLOCALS)
    {
        fei_compiler_raiseerror(state, "Too many local variables in block.");
        return;
    }

    Local* local = &state->aststate.compiler->proglocals[state->aststate.compiler->progloccount++];
    local->name = name;
    local->depth = -1;// for cases where a variable name is redefined inside another scope, using the variable itself
    local->iscaptured = false;
}

void fei_compiler_declvarfromcurrent(State* state)// for local variables
{
    // global vars are implicitly declared, and are late bound, not 'initialized' here but in the VM
    if(state->aststate.compiler->scopedepth == 0)
        return;


    /* local variable declaration happens below */
    Token* name = &state->aststate.parser.prevtoken;

    // to not allow two variable declarations to have the same name
    // loop only checks to a HIGHER SCOPE; another block overlaping/shadowing is allowed
    // work backwards
    for(int i = state->aststate.compiler->progloccount - 1; i >= 0; i--)
    {
        Local* local = &state->aststate.compiler->proglocals[i];
        if(local->depth != -1 && local->depth < state->aststate.compiler->scopedepth)// if reach beginning of array(highest scope)
        {
            break;
        }

        if(fei_compiler_identsequal(state, name, &local->name))
        {
            fei_compiler_raiseerror(state, "Variable with this name exists in scope.");
        }
    }

    fei_compiler_addlocal(state, *name);
}

uint8_t fei_compiler_parsevarfromcurrent(State* state, const char* errormessage)
{
    fei_compiler_consume(state, TOKEN_IDENTIFIER, errormessage);// requires next token to be an identifier

    fei_compiler_declvarfromcurrent(state);
    if(state->aststate.compiler->scopedepth > 0)
        return 0;// if scopedepth is not 0, then it is a local not global var
    // return a dummy index
    // at runtime, locals are not looked up by name so no need to insert them to a table


    return fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);// return index from the constant table
}


void fei_compiler_markinit(State* state)
{
    if(state->aststate.compiler->scopedepth == 0)
        return;// if global return
    state->aststate.compiler->proglocals[state->aststate.compiler->progloccount - 1].depth = state->aststate.compiler->scopedepth;
}

void fei_compiler_defvarindex(State* state, uint8_t global)
{
    if(state->aststate.compiler->scopedepth > 0)
    {
        fei_compiler_markinit(state);
        return;
    }

    fei_compiler_emitbytes(state, OP_DEFINE_GLOBAL, global);// opcode for declaration and the constant itself
}


// for function arguments, returns number of arguments
// each argument expression generates code which leaves value on the stack in preparation for the call
uint8_t fei_compiler_parsearglist(State* state)
{
    uint8_t argcount = 0;
    if(!fei_compiler_check(state, TOKEN_RIGHTPAREN))// if ) has not been reached
    {
        do
        {
            fei_compiler_parseexpr(state);// collect the arguments

            if(argcount == 255)// cannot have more than 255 arguments as each operand is a single byte(uint8_t)
            {
                fei_compiler_raiseerror(state, "Cannot have more than 255 arguments.");
            }

            argcount++;
        } while(fei_compiler_match(state, TOKEN_COMMA));
    }

    fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after argument list.");
    return argcount;
}

static void fei_comprule_logicaland(State* state, bool canassign)
{
    (void)canassign;
    int endjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);// left hand side is already compiled,
    // and if it is false skip it and go to next

    fei_compiler_emitbyte(state, OP_POP);
    fei_compiler_parseprec(state, PREC_AND);


    fei_compiler_patchjump(state, endjump);
}


// for binary, eg. 5 + 4
// or INFIX parser, where the operator is in the middle
// entire left hand expression has been compiled, and the infix operator has been consumed
// fei_comprule_binary() handles the rest of the arithmetic operator
static void fei_comprule_binary(State* state, bool canassign)
{
    (void)canassign;
    // remember type of operator, already consumed
    TokType operatortype = state->aststate.parser.prevtoken.type;

    // compile right operand
    ParseRule* rule = fei_compiler_getrule(state, operatortype);// the BIDMAS rule, operands in the right side have HIGHER PRECEDENCE
    // as binary operators are LEFT ASSOCIATIVE
    // recursively call fei_compiler_parseprec again
    fei_compiler_parseprec(state, (Precedence)(rule->precedence + 1));// conert from rule to enum(precedence) type

    switch(operatortype)
    {
            // note how NOT opcode is at the end
            // six binary operators for three instructions only(greater, not, equal)
        case TOKEN_NOTEQUAL:
            fei_compiler_emitbytes(state, OP_EQUAL, OP_NOT);
            break;// add equal and not to the stack
        case TOKEN_EQUAL:
            fei_compiler_emitbyte(state, OP_EQUAL);
            break;
        case TOKEN_GREATERTHAN:
            fei_compiler_emitbyte(state, OP_GREATER);
            break;
        case TOKEN_GREATEREQUAL:
            fei_compiler_emitbytes(state, OP_LESS, OP_NOT);
            break;
        case TOKEN_LESSTHAN:
            fei_compiler_emitbyte(state, OP_LESS);
            break;
        case TOKEN_LESSEQUAL:
            fei_compiler_emitbytes(state, OP_GREATER, OP_NOT);
            break;

        case TOKEN_PLUS:
            fei_compiler_emitbyte(state, OP_ADD);
            break;
        case TOKEN_MINUS:
            fei_compiler_emitbyte(state, OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            fei_compiler_emitbyte(state, OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            fei_compiler_emitbyte(state, OP_DIVIDE);
            break;
        case TOKEN_MODULO:
            fei_compiler_emitbyte(state, OP_MODULO);
            break;
        default:
            return;// unreachable
    }
}


// for function calls
static void fei_comprule_call(State* state, bool canassign)
{
    (void)canassign;
    // again, assumes the function itself(its call name) has been placed on the codestream stack
    uint8_t argcount = fei_compiler_parsearglist(state);// compile arguments using fei_compiler_parsearglist
    fei_compiler_emitbytes(state, OP_CALL, argcount);// write on the chunk
}

// class members/fields/properties
static void fei_comprule_dot(State* state, bool canassign)
{
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect propery name after class instance.");
    uint8_t name = fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);// already consumed

    if(canassign && fei_compiler_match(state, TOKEN_ASSIGN))// assignment
    {
        fei_compiler_parseexpr(state);// evalute expression to be set
        fei_compiler_emitbytes(state, OP_SET_PROPERTY, name);
    }
    else if(fei_compiler_match(state, TOKEN_LEFTPAREN))// for running class methods, access the method and call it at the same time
    {
        uint8_t argcount = fei_compiler_parsearglist(state);

        /* new OP_INVOKE opcode that takes two operands:
		1. the index of the property name in the constant table
		2. the number of arguments passed in the methods
		*** combines OP_GET_PROPERTY and OP_CALL
		*/
        fei_compiler_emitbytes(state, OP_INVOKE, name);
        fei_compiler_emitbyte(state, argcount);
    }
    else// simply get
    {
        fei_compiler_emitbytes(state, OP_GET_PROPERTY, name);
    }
}

static void fei_comprule_literal(State* state, bool canassign)
{
    (void)canassign;
    switch(state->aststate.parser.prevtoken.type)
    {
        case TOKEN_KWFALSE:
            fei_compiler_emitbyte(state, OP_FALSE);
            break;
        case TOKEN_KWTRUE:
            fei_compiler_emitbyte(state, OP_TRUE);
            break;
        case TOKEN_KWNULL:
            fei_compiler_emitbyte(state, OP_NULL);
            break;

        default:// unreachable
            return;
    }
}

// parentheses for grouping
static void fei_comprule_grouping(State* state, bool canassign)
{
    (void)canassign;
    // assume initial ( has already been consumed, and recursively call to fei_compiler_parseexpr() to compile between the parentheses
    fei_compiler_parseexpr(state);
    fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after expression.");// expects a right parentheses, if not received then  error
}


/* parsing the tokens */
static void fei_comprule_number(State* state, bool canassign)
{
    (void)canassign;
    // strtod below converts from string to double
    // assume that token for the number literal has already been consumed and is stored in previous
    // double strtod(const char* str, char** endptr)
    // endptr is the first non-double character after teh char* str character; if none then null
    /*	The way it works:
	-> in scanner, if a digit exists after a digit, it advances() (skips) the current
	-> hence, we get that the start points to the START of the digit, and using strtod smartly it reaches until the last digit
	*/
    double value = strtod(state->aststate.parser.prevtoken.toksrc, NULL);
    //printf("num %c\n", *state->aststate.parser.prevtoken.toksrc);
    fei_compiler_emitconst(state, NUMBER_VAL(value));
}

static void fei_comprule_logicalor(State* state, bool canassign)
{
    (void)canassign;
    // jump if left hand side is true
    int elsejump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);// if left is false jump directly to right hand
    int endjump = fei_compiler_emitjump(state, OP_JUMP);// if not skipped(as left is true) jump the right hand

    fei_compiler_patchjump(state, elsejump);
    fei_compiler_emitbyte(state, OP_POP);

    fei_compiler_parseprec(state, PREC_OR);
    fei_compiler_patchjump(state, endjump);
}

// 'initialize' the string here
static void fei_comprule_string(State* state, bool canassign)
{
    (void)canassign;
    // in a string, eg. "hitagi", the quotation marks are trimmed
    fei_compiler_emitconst(state, OBJ_VAL(fei_object_copystring(state, state->aststate.parser.prevtoken.toksrc + 1, state->aststate.parser.prevtoken.length - 2)));
}

// declare/call variables
void fei_compiler_declnamedvar(State* state, Token name, bool canassign)
{
    (void)canassign;
    uint8_t getop, setop;
    int arg = fei_compiler_resolvelocal(state, state->aststate.compiler, &name);// try find a local variable with a given name
    if(arg != -1)
    {
        getop = OP_GET_LOCAL;
        setop = OP_SET_LOCAL;
    }
    else if((arg = fei_compiler_resolveupvalue(state, state->aststate.compiler, &name)) != -1)// for upvalues
    {
        getop = OP_GET_UPVALUE;
        setop = OP_SET_UPVALUE;
    }
    else
    {
        arg = fei_compiler_makeidentconst(state, &name);
        getop = OP_GET_GLOBAL;
        setop = OP_SET_GLOBAL;
    }


    // test case to check whether it is a get(just the name) or a reassignment
    if(canassign && fei_compiler_match(state, TOKEN_ASSIGN))// if a = follows right after
    {
        fei_compiler_parseexpr(state);
        fei_compiler_emitbytes(state, setop, (uint8_t)arg);// reassignment/set
    }
    else
    {
        fei_compiler_emitbytes(state, getop, (uint8_t)arg);// as normal get
        // printf("gest");
    }
}


static void fei_comprule_variable(State* state, bool canassign)
{
    (void)canassign;
    fei_compiler_declnamedvar(state, state->aststate.parser.prevtoken, canassign);
}

// for super classes, token that mimics as if a user types in 'super'
Token fei_compiler_makesyntoken(State* state, const char* text)
{
    Token token;
    (void)state;
    token.toksrc = text;
    token.length = (int)strlen(text);// strlen to get char* length
    return token;
}

// for super calls
static void fei_comprule_super(State* state, bool canassign)
{
    (void)canassign;
    // if token is not inside a class
    if(state->aststate.classcompiler == NULL)
    {
        fei_compiler_raiseerror(state, "'super' can only be initialized inside a class.");
    }
    else if(!state->aststate.classcompiler->hassuperclass)// if class has no parent class
    {
        fei_compiler_raiseerror(state, "'super' cannot be used on a class with no parent class.");
    }


    fei_compiler_consume(state, TOKEN_DOT, "Expect '.' after 'super'.");
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect parent class method identifier.");
    uint8_t name = fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);// get identifier index


    /*
	in order to access a superclass method on the CURRENT INSTANCE, runtime needs both the receiver and the superclass
	of the surrounding method's class.
	1. first fei_compiler_declnamedvar call generates code to look up the current receiver and push it to the stack
	2. second fei_compiler_declnamedvar emits code to look up the superclass and push that on top
	*/
    fei_compiler_declnamedvar(state, fei_compiler_makesyntoken(state, "this"), false);
    if(fei_compiler_match(state, TOKEN_LEFTPAREN))// if there is a parameter list, invoke super method
    {
        uint8_t argcount = fei_compiler_parsearglist(state);
        fei_compiler_declnamedvar(state, fei_compiler_makesyntoken(state, "super"), false);
        fei_compiler_emitbytes(state, OP_SUPER_INVOKE, name);// super invoke opcode
        fei_compiler_emitbyte(state, argcount);
    }
    else
    {
        fei_compiler_declnamedvar(state, fei_compiler_makesyntoken(state, "super"), false);
        fei_compiler_emitbytes(state, OP_GET_SUPER, name);
    }
}


// for class methods
static void fei_comprule_this(State* state, bool canassign)
{
    (void)canassign;
    // if not inside a class
    if(state->aststate.classcompiler == NULL)
    {
        fei_compiler_raiseerror(state, "Cannot use 'this' outside of class.");
        return;
    }

    fei_comprule_variable(state, false);// always false
}


static void fei_comprule_unary(State* state, bool canassign)
{
    (void)canassign;
    TokType operatortype = state->aststate.parser.prevtoken.type;// leading - token has already been consumed

    // compile operand
    fei_compiler_parseexpr(state);

    switch(operatortype)
    {
        case TOKEN_LOGICALNOT:
            fei_compiler_emitbyte(state, OP_NOT);
            break;


            // OP_NEGATE should be emitted last, AFTER the constant itself
            // eg. say 4 - 5; 5 needs to be emitted and added to the chunk->constants first before OP_NEGATE
            /* it is important to take note of the precedence
		e.g -a.b + 3;
		when the unary negation is called, all of a.b + 3 will be consumed in fei_compiler_parseexpr(). Hence, a method is needed
		to STOP when + is found, or generally when an operand of LOWER PRECEDENCE is found
		*/
        case TOKEN_MINUS:
            fei_compiler_emitbyte(state, OP_NEGATE);
            break;
        default:
            return;
    }
}


// for inserting where the unary operator should lie
// starts at current token and parses any expression at the given precedence level or higher
// for example, if fei_compiler_parseprec(PREC_COMPARISON) is called, it will parse unaries, terms, and factors
// ubt not or, and or assignment operators as they are lower. Basically parse anything that is ABOVE the given precedence
void fei_compiler_parseprec(State* state, Precedence precedence)
{
    /*	PREFIX FIRST
	look up for a prefix token, and the FIRSt token is ALWAYS going to be a prefix
	*/
    fei_compiler_advancenext(state);// again, go next first then use previous type as the 'current' token
    // the way the compiler is designed is that it has to always have a prefix
    ParseFn prefixrule = fei_compiler_getrule(state, state->aststate.parser.prevtoken.type)->prefix;

    if(prefixrule == NULL)
    {
        fei_compiler_raiseerror(state, "Expect expression.");
        return;
    }

    //

    bool canassign = precedence <= PREC_ASSIGNMENT;// for assignment precedence
    prefixrule(state, canassign);// call the prefix function, may consume a lot of tokens


    /* after prefix expression is done, look for infix expression
	IMPORTANT: infix only runs if given precedence is LOWER than the operator for the infix
	or more nicely if NEXT/INFIX PRECEDENCE IS HIGHER THAN PREC ASSIGNMENT(parameter above_
	*/


    while(precedence <= fei_compiler_getrule(state, state->aststate.parser.currtoken.type)->precedence)
    {
        fei_compiler_advancenext(state);
        ParseFn infixrule = fei_compiler_getrule(state, state->aststate.parser.prevtoken.type)->infix;

        infixrule(state, canassign);
    }

    //consume(TOKEN_KWAND, "consume and failed");

    if(canassign && fei_compiler_match(state, TOKEN_ASSIGN))// if = is not consumed as part of the expression, nothing will , hence an error
    {
        fei_compiler_raiseerror(state, "Invalid Assignment target.");
    }
}

// get pointer to ParseRule struct according to type parameter
ParseRule* fei_compiler_getrule(State* state, TokType type)
{
    (void)state;
    return &rules[type];
}

void fei_compiler_parseexpr(State* state)// a single 'statement' or line
{
    fei_compiler_parseprec(state, PREC_ASSIGNMENT);// as assignment is the 2nd lowest, parses evrything
}

void fei_compiler_parseblock(State* state)
{
    while(!fei_compiler_check(state, TOKEN_RIGHTBRACE) && !fei_compiler_check(state, TOKEN_EOF))// parse until EOF or right brace is 'peeked'
    {
        fei_compiler_parsedeclaration(state);// compile rest of block, keeps on parsing until right brace or EOF is 'peeked'
    }

    fei_compiler_consume(state, TOKEN_RIGHTBRACE, "Expect '}' after block.");
}


/* functions */
void fei_compiler_parsefuncdecl(State* state, FuncType type)
{
    // create separate Compiler for each function
    Compiler compiler;
    fei_compiler_init(state, &compiler, type);
    fei_compiler_beginscope(state);

    // compile parameters
    fei_compiler_consume(state, TOKEN_LEFTPAREN, "Expect '(' after function name.");

    if(!fei_compiler_check(state, TOKEN_RIGHTPAREN))// if end ) has not been reached
    {
        do
        {
            state->aststate.compiler->programfunc->arity++;// add number of parameters
            if(state->aststate.compiler->programfunc->arity > 255)
            {
                fei_compiler_raisehere(state, "Cannot have more than 255 parameters.");
            }

            uint8_t paramconstant = fei_compiler_parsevarfromcurrent(state, "Expect variable name.");// get name
            fei_compiler_defvarindex(state, paramconstant);// scope handled here already
        } while(fei_compiler_match(state, TOKEN_COMMA));
    }

    fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after parameter list.");

    // body
    fei_compiler_consume(state, TOKEN_LEFTBRACE, "Expect '{' before function body.");
    fei_compiler_parseblock(state);

    // create function object
    ObjFunction* function = fei_compiler_endcompiler(state);// ends the current compiler
    // compilers are treated like a stack; if current one is ended, like above, return to the previous one

    // fei_compiler_emitbytes(state, OP_CONSTANT, fei_compiler_makeconst(state, OBJ_VAL(function)));
    fei_compiler_emitbytes(state, OP_CLOSURE, fei_compiler_makeconst(state, OBJ_VAL(function)));

    /*	by the time the compiler reaches the end of a function declaration,
	every variable reference hass been resolved as either local, upvalue or global.
	each upvalue may return a local var or another upvalue

	-> for each upvalue there are two single-byte operands
	-> if first byte is one, then it captures a local variable in the enclosing function
	-> if first byte is 0, it captures the function's upvalues
	*/

    for(int i = 0; i < function->upvaluecount; i++)
    {
        fei_compiler_emitbyte(state, compiler.upvalues[i].islocalvar ? 1 : 0);
        fei_compiler_emitbyte(state, compiler.upvalues[i].index);// emit index
    }
}

// create method for class type
void fei_compiler_parsemethoddecl(State* state)
{
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);// get method name

    // method body
    FuncType type = TYPE_METHOD;

    // if initializer
    if(state->aststate.parser.prevtoken.length == 4 && memcmp(state->aststate.parser.prevtoken.toksrc, "init", 4) == 0)
    {
        type = TYPE_INITIALIZER;
    }

    fei_compiler_parsefuncdecl(state, type);// process the function

    fei_compiler_emitbytes(state, OP_METHOD, constant);
}


void fei_compiler_parseclassdecl(State* state)
{
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect class name.");
    Token classname = state->aststate.parser.prevtoken;// get class name
    uint8_t nameconstant = fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);// add to constant table as a string, return its index
    fei_compiler_declvarfromcurrent(state);// declare that name variable

    fei_compiler_emitbytes(state, OP_CLASS, nameconstant);// takes opcode and takes the constant table index
    fei_compiler_defvarindex(state, nameconstant);// add it to the global hasht; we must DEFINE AFTER DECLARE to use it

    // handle class enclosing for 'this'
    ClassCompiler classcompiler;
    classcompiler.name = state->aststate.parser.prevtoken;
    classcompiler.hassuperclass = false;
    classcompiler.enclosing = state->aststate.classcompiler;
    state->aststate.classcompiler = &classcompiler;// set new class as current

    // class inheritance
    if(fei_compiler_match(state, TOKEN_KWFROM))
    {
        fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect parent class name.");
        fei_comprule_variable(state, false);// get the class variable, looks up the parent class by name and push it to the stack

        // check that the class names must be different
        if(fei_compiler_identsequal(state, &classname, &state->aststate.parser.prevtoken))
        {
            fei_compiler_raiseerror(state, "Cannot inherit class from itself");
        }

        /* super classes
		- create new lexical scope to ensure that if we declare two classes in the same scope, each has a different
		local slot to store the superclasses
		*/
        fei_compiler_beginscope(state);
        fei_compiler_addlocal(state, fei_compiler_makesyntoken(state, "super"));
        fei_compiler_defvarindex(state, 0);

        fei_compiler_declnamedvar(state, classname, false);
        fei_compiler_emitbyte(state, OP_INHERIT);
        classcompiler.hassuperclass = true;
    }


    fei_compiler_declnamedvar(state, classname, false);// helper function to geenrate code that LOADS a variable with a given name to te stack

    fei_compiler_consume(state, TOKEN_LEFTBRACE, "Expect '{' before class body.");
    while(!fei_compiler_check(state, TOKEN_RIGHTBRACE) && !fei_compiler_check(state, TOKEN_EOF))
    {
        fei_compiler_parsemethoddecl(state);
    }

    fei_compiler_consume(state, TOKEN_RIGHTBRACE, "Expect '}' after class body.");
    fei_compiler_emitbyte(state, OP_POP);// no longer need the class, pop it

    // close local scope for superclass variable
    if(classcompiler.hassuperclass)
    {
        fei_compiler_endscope(state);
    }

    state->aststate.classcompiler = state->aststate.classcompiler->enclosing;// go back to enclosing/main() class
}


void fei_compiler_parseclassfuncdecl(State* state)
{
    uint8_t global = fei_compiler_parsevarfromcurrent(state, "Expect function name.");
    fei_compiler_markinit(state);// scoping
    fei_compiler_parsefuncdecl(state, TYPE_FUNCTION);
    fei_compiler_defvarindex(state, global);
}

void fei_compiler_parsevardecl(State* state)
{
    uint8_t global = fei_compiler_parsevarfromcurrent(state, "Expect variable name.");

    if(fei_compiler_match(state, TOKEN_ASSIGN))
    {
        fei_compiler_parseexpr(state);
    }
    else
    {
        fei_compiler_emitbyte(state, OP_NULL);// not initialized
    }
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "expect ';'");
    }
    fei_compiler_defvarindex(state, global);// create global variable here; if local, not added to table
}

void fei_compiler_parseexprstmt(State* state)
{
    fei_compiler_parseexpr(state);
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after expression.");
    }
    fei_compiler_emitbyte(state, OP_POP);
}

// if method
void fei_compiler_parseifstmt(State* state)
{
    //	fei_compiler_consume(state, TOKEN_LEFTPAREN, "Expect '(' after 'if'.");
    fei_compiler_parseexpr(state);// compile the expression statment inside; fei_compiler_parseprec()
    // after compiling expression above conditon value will be left at the top of the stack
    //	fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after condition.");


    // gives an operand on how much to offset the ip; how many bytes of code to skip
    // if falsey, simply adjusts the ip by that amount
    // offset to jump to next (potentially else or elf) statment
    // insert to opcode the then branch statment first, then get offset
    int thenjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE); /* this gets distance */


    fei_compiler_emitbyte(state, OP_POP);// pop then

    /* use BACKPATCHING
	- emit jump first with a placeholder offset, and get how far to jump
	
	
	*/

    fei_compiler_parsestatement(state);

    // below jump wil SURELY jump; this is skipped if the first fei_compiler_emitjump is not false
    int elsejump = fei_compiler_emitjump(state, OP_JUMP);// need to jump at least 'twice' with an else statement
    // if the original statement is  true, then skip the the else statement

    // if then statment is run; pop the expression inside () after if
    fei_compiler_patchjump(state, thenjump); /* this actually jumps */

    fei_compiler_emitbyte(state, OP_POP);// if else statment is run; pop the expression inside () after if
    if(fei_compiler_match(state, TOKEN_KWELSE))
        fei_compiler_parsestatement(state);

    if(fei_compiler_match(state, TOKEN_KWELF))// else if
    {
        // go to statement, then go back to IF
        fei_compiler_parseifstmt(state);
    }

    /* this actually jumps */
    // last jump that is executed IF FIRST STATEMENT IS TRUE
    fei_compiler_patchjump(state, elsejump);// for the second jump
}

void fei_compiler_parseswitchstmt(State* state)
{
    // fei_compiler_consume(state, TOKEN_LEFTPAREN, "Expect '(' after 'switch'.");
    if(!fei_compiler_check(state, TOKEN_IDENTIFIER))// check next token
    {
        fei_compiler_raisehere(state, "Expect identifier after switch.");
    }

    // if no error, consume the identifier
    fei_compiler_parseexpr(state);
    fei_compiler_consume(state, TOKEN_LEFTBRACE, "Expect '{' after switch identifier.");
    fei_compiler_consume(state, TOKEN_KWCASE, "Expect at least 1 case after switch declaration.");

    /* to store  opcode offsets */
    uint8_t casescount = -1;
    uint8_t capacity = 0;
    int* casesoffset = (int*)ALLOCATE(state, sizeof(int), 8);// 8 initial switch cases

    do// while next token is a case, match also advances
    {
        // grow array if needed
        if(capacity < casescount + 1)
        {
            int oldcapacity = capacity;
            capacity = GROW_CAPACITY(oldcapacity);
            casesoffset = (int*)GROW_ARRAY(state, sizeof(int), casesoffset, oldcapacity, capacity);
        }

        casescount++;

        fei_compiler_parseexpr(state);
        fei_compiler_consume(state, TOKEN_COLON, "Expect ':' after case expression.");
        fei_compiler_emitbyte(state, OP_SWITCH_EQUAL);// check if both values are equal

        int casefalsejump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);// jump if false
        //printf("\ncase false jump offset: %d", casefalsejump);

        // parse the statment
        fei_compiler_parsestatement(state);

        fei_compiler_emitbyte(state, OP_POP);// pop the 'true' from OP_SWITCH_EQUAL
        casesoffset[casescount] = fei_compiler_emitjump(state, OP_JUMP);
        //printf("\ncase true jump offset: %d", casesoffset[casescount]);

        // jump to end of case if false
        fei_compiler_patchjump(state, casefalsejump);
        fei_compiler_emitbyte(state, OP_POP);// pop the 'false' statment from OP_SWITCH_EQUAL
    } while(fei_compiler_match(state, TOKEN_KWCASE));

    if(fei_compiler_match(state, TOKEN_KWDEFAULT))
    {
        fei_compiler_consume(state, TOKEN_COLON, "Expect ':' default case.");
        fei_compiler_parsestatement(state);// running the default statement
    }
    //fei_compiler_consume(state, TOKEN_KWDEFAULT, "Default case not provided for switch.");


    // fei_compiler_patchjump for each available jump
    for(uint8_t i = 0; i <= casescount; i++)
    {
        fei_compiler_patchjump(state, casesoffset[i]);
    }

    fei_compiler_emitbyte(state, OP_POP);// pop switch constant
    FREE_ARRAY(state, sizeof(int), casesoffset, capacity);

    fei_compiler_consume(state, TOKEN_RIGHTBRACE, "Expect '}' at the end of switch statement");
}


void fei_compiler_parseprintstmt(State* state)
{
    fei_compiler_parseexpr(state);// this is the function that actually processes the experssion
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after value.");// try consume ;, if fails show message
    }
    fei_compiler_emitbyte(state, OP_PRINT);
}

void fei_compiler_parsereturnstmt(State* state)
{
    if(state->aststate.compiler->progfunctype == TYPE_SCRIPT)
    {
        fei_compiler_raiseerror(state, "Cannot return from top-level code.");
    }
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        fei_compiler_emitreturn(state);
    }
    else
    {
        // error in returning from an initializer
        if(state->aststate.compiler->progfunctype == TYPE_INITIALIZER)
        {
            fei_compiler_raiseerror(state, "Cannot return a value from an initializer");
        }

        fei_compiler_parseexpr(state);
        fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after return value.");
        fei_compiler_emitbyte(state, OP_RETURN);
    }
}

void fei_compiler_parseforstmt(State* state)
{
    fei_compiler_beginscope(state);// for possible variable declarations in clause

    fei_compiler_beginloopscope(state);

    fei_compiler_consume(state, TOKEN_LEFTPAREN, "Expect '(' after 'for'.");

    // initializer clause
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        // no initializer
    }
    else if(fei_compiler_match(state, TOKEN_KWVAR))
    {
        fei_compiler_parsevardecl(state);// for clause scope only
    }
    else
    {
        fei_compiler_parseexprstmt(state);
    }

    // for for/while loops, loop starts here, with currenchunk()->count
    int loopstart = fei_compiler_currentchunk(state)->count;

    //  the condition clause
    /* CONDITION CLAUSE
	1. If false, pop the recently calculated expression and skip the loop
	2. if true, go to the body; see increment clause below
	*/
    int exitjump = -1;
    if(!fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        fei_compiler_parseexpr(state);
        fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // jump out of loop if condition is false
        exitjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);
        fei_compiler_emitbyte(state, OP_POP);// still need to figure this out, most likely just deleting 'temporary' constants in the scope
    }

    // the increment clause
    if(!fei_compiler_match(state, TOKEN_RIGHTPAREN))// if there is something else before the terminating ')'
    {
        /*	INCEREMENT CLAUSE
		1. from the condition clause, first jump OVER the increment, to the body
		2. in the body, run the body
		3. jump BACK to the increment and run it
		4. from the increment jump BACK to the CONDITION clause, back to the cycle
		*/

        // for continue


        int bodyjump = fei_compiler_emitjump(state, OP_JUMP);// jump the increment clause

        int incrementstart = fei_compiler_currentchunk(state)->count;// starting index for increment

        // set continue jump here, right after the increment statement
        fei_compiler_markcontinuejump(state);

        fei_compiler_parseexpr(state);// run the for expression
        fei_compiler_emitbyte(state, OP_POP);// pop expression constant
        fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after for clauses.");

        // running the loop
        fei_compiler_emitloop(state, loopstart);// goes back to the start of the CONDITION clause of the for loop
        loopstart = incrementstart;
        fei_compiler_patchjump(state, bodyjump);
    }

    fei_compiler_parsestatement(state);// running the code inside the loop

    fei_compiler_emitloop(state, loopstart);

    // patch the jump in the loop body
    if(exitjump != -1)
    {
        fei_compiler_patchjump(state, exitjump);
        fei_compiler_emitbyte(state, OP_POP);// only pop when THERE EXISTS A CONDITION from the clause
    }

    // patch break jumps, if available
    fei_compiler_patchbreakjumps(state);

    fei_compiler_endloopscope(state);
    fei_compiler_endscope(state);
}

void fei_compiler_parsewhilestmt(State* state)
{
    int loopstart = fei_compiler_currentchunk(state)->count;// index where the statement to loop starts
    fei_compiler_beginloopscope(state);

    // set jump for potential continue statement
    fei_compiler_markcontinuejump(state);

    fei_compiler_parseexpr(state);


    int exitjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);// skip stament if condition is false

    fei_compiler_emitbyte(state, OP_POP);// pop the last expression(true or false)

    fei_compiler_parsestatement(state);

    fei_compiler_emitloop(state, loopstart);// method to 'loop' the instruction

    fei_compiler_patchjump(state, exitjump);

    fei_compiler_emitbyte(state, OP_POP);

    // patch break jumps, if available
    fei_compiler_patchbreakjumps(state);

    fei_compiler_endloopscope(state);
}

void fei_compiler_parsebreakstmt(State* state)
{
    if(state->aststate.compiler->loopcounttop < 0)
    {
        fei_compiler_raiseerror(state, "Break statement must be enclosed in a loop");
        return;
    }

    if(++state->aststate.compiler->breakjumpcounts[state->aststate.compiler->loopcounttop] > CFG_MAX_COMPILERBREAK)
    {
        fei_compiler_raiseerror(state, "Too many break statments in one loop");
        return;
    }

    int breakjump = fei_compiler_emitjump(state, OP_JUMP);
    int loopdepth = state->aststate.compiler->loopcounttop;
    int breakamount = state->aststate.compiler->breakjumpcounts[loopdepth];
    state->aststate.compiler->breakpatchjumps[state->aststate.compiler->loopcounttop][breakamount - 1] = breakjump;
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after break.");
    }
}


void fei_compiler_parsecontinuestmt(State* state)
{
    if(state->aststate.compiler->loopcounttop < 0)
    {
        fei_compiler_raiseerror(state, "Continue statement must be enclosed in a loop");
        return;
    }

    if(state->aststate.compiler->loopcounttop == state->aststate.compiler->continuejumpcapacity)
    {
        int oldcapacity = state->aststate.compiler->continuejumpcapacity;
        state->aststate.compiler->continuejumpcapacity = GROW_CAPACITY(oldcapacity);
        state->aststate.compiler->continuejumps = (int*)GROW_ARRAY(state, sizeof(int), state->aststate.compiler->continuejumps, oldcapacity, state->aststate.compiler->continuejumpcapacity);
    }

    fei_compiler_emitloop(state, state->aststate.compiler->continuejumps[state->aststate.compiler->loopcounttop]);
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after continue.");
    }
}

void fei_compiler_parserepeatuntilstmt(State* state)
{
    // fei_compiler_consume(state, TOKEN_LEFTBRACE, "Expect '{' after repeat.");
    int loopstart = fei_compiler_currentchunk(state)->count;
    fei_compiler_beginloopscope(state);
    fei_compiler_markcontinuejump(state);

    // process the statement
    fei_compiler_parsestatement(state);

    fei_compiler_consume(state, TOKEN_KWUNTIL, "Expect 'until' after repeat statement.");

    // get true or false
    fei_compiler_parseexpr(state);

    // emit loop if false op code
    fei_compiler_emitcondloop(state, loopstart, false);

    // patch possible break jumps
    fei_compiler_patchbreakjumps(state);

    fei_compiler_endloopscope(state);
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after until exppression.");
    }
}

void fei_compiler_parsedowhilestmt(State* state)
{
    int loopstart = fei_compiler_currentchunk(state)->count;
    fei_compiler_beginloopscope(state);
    fei_compiler_markcontinuejump(state);

    // process the statement
    fei_compiler_parsestatement(state);

    fei_compiler_consume(state, TOKEN_KWWHILE, "Expect 'until' after repeat statement.");

    // get true or false
    fei_compiler_parseexpr(state);

    // emit loop if true op code
    fei_compiler_emitcondloop(state, loopstart, true);

    // patch possible break jumps
    fei_compiler_patchbreakjumps(state);

    fei_compiler_endloopscope(state);
    fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after until exppression.");
}

void fei_compiler_synchronize(State* state)
{
    state->aststate.parser.panicmode = false;

    //printf("panic mode");

    // basically turn off the 'error' mode and skips token until something that looks like a statement boundary is found
    // skips tokens indiscriminately until somehing that looks like a statement boundary(eg. semicolon) is found
    while(state->aststate.parser.currtoken.type != TOKEN_EOF)
    {
        if(state->aststate.parser.prevtoken.type == TOKEN_SEMICOLON)
            return;

        switch(state->aststate.parser.currtoken.type)
        {
            case TOKEN_KWCLASS:
            case TOKEN_KWFUN:
            case TOKEN_KWVAR:
            case TOKEN_KWFOR:
            case TOKEN_KWIF:
            case TOKEN_KWWHILE:
            case TOKEN_KWPRINT:
            case TOKEN_KWRETURN:
                return;
            default:// do nothing
            ;
        }

        fei_compiler_advancenext(state);
    }
}

void fei_compiler_parsedeclaration(State* state)
{
    if(fei_compiler_match(state, TOKEN_KWCLASS))
    {
        fei_compiler_parseclassdecl(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWFUN))
    {
        fei_compiler_parseclassfuncdecl(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWVAR))
    {
        fei_compiler_parsevardecl(state);// declare variable
    }
    else
    {
        fei_compiler_parsestatement(state);
    }
    if(state->aststate.parser.panicmode)
        fei_compiler_synchronize(state);// for errors
}

void fei_compiler_parsestatement(State* state)// either an expression or a print
{
    if(fei_compiler_match(state, TOKEN_KWPRINT))
    {
        fei_compiler_parseprintstmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWRETURN))
    {
        fei_compiler_parsereturnstmt(state);// for functions return
    }
    else if(fei_compiler_match(state, TOKEN_KWWHILE))
    {
        fei_compiler_parsewhilestmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWFOR))
    {
        fei_compiler_parseforstmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWSWITCH))
    {
        fei_compiler_parseswitchstmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWBREAK))
    {
        fei_compiler_parsebreakstmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWCONTINUE))
    {
        fei_compiler_parsecontinuestmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWIF))
    {
        fei_compiler_parseifstmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWREPEAT))
    {
        fei_compiler_parserepeatuntilstmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWDO))
    {
        fei_compiler_parsedowhilestmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_LEFTBRACE))// parse initial { token
    {
        fei_compiler_beginscope(state);
        fei_compiler_parseblock(state);
        fei_compiler_endscope(state);
    }
    else
    {
        fei_compiler_parseexprstmt(state);
    }
}

ObjFunction* fei_compiler_compilesource(State* state, const char* source, size_t len)
{
    Compiler compiler;
    ObjFunction* function;
    fei_lexer_initsource(state, source, len);// start scan/lexing
    memset(&compiler, 0, sizeof(compiler));
    fei_compiler_init(state, &compiler, TYPE_SCRIPT);
    state->aststate.parser.haderror = false;
    state->aststate.parser.panicmode = false;
    fei_compiler_advancenext(state);// call to advance once to 'pump' the scanner
    while(!fei_compiler_match(state, TOKEN_EOF))/// while EOF token is not met
    {
        fei_compiler_parsedeclaration(state);
    }
    function = fei_compiler_endcompiler(state);// ends the expression with a return type
    return state->aststate.parser.haderror ? NULL : function;// if no error return true
}


// marking compiler roots, for garbage collection
void fei_compiler_markroots(State* state)
{
    Compiler* compiler = state->aststate.compiler;
    while(compiler != NULL)
    {
        fei_gcmem_markobject(state, (Object*)compiler->programfunc);
        compiler = compiler->enclosing;
    }
}


static Value cfn_clock(State* state, int argcount, Value* args)
{
    (void)state;
    (void)argcount;
    (void)args;
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);// returns elapsed time since program was running
}

static Value cfn_print(State* state, int argc, Value* args)
{
    int i;
    Object* o;
    ObjString* os;
    (void)state;
    for(i = 0; i < argc; i++)
    {
        switch(args[i].type)
        {
            case VAL_BOOL:
                fprintf(stdout, "%s", args[i].as.valbool ? "true" : "false");
                break;
            case VAL_NULL:
                fprintf(stdout, "null");
                break;
            case VAL_NUMBER:
                fprintf(stdout, "%g", args[i].as.valnumber);
                break;
            case VAL_OBJ:
            {
                o = args[i].as.valobjptr;
                if(fei_value_isstring(args[i]))
                {
                    os = fei_value_asstring(args[i]);
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

static Value cfn_println(State* state, int argc, Value* args)
{
    Value r;
    r = cfn_print(state, argc, args);
    fprintf(stdout, "\n");
    fflush(stdout);
    return r;
}


static Value cfn_chr(State* state, int argc, Value* args)
{
    char ch;
    (void)state;
    (void)argc;
    if(!fei_value_isnumber(args[0]))
    {
        fei_vm_raiseruntimeerror(state, "chr() expects a number");
        return NULL_VAL;
    }
    ch = fei_value_asnumber(args[0]);
    return OBJ_VAL(fei_object_copystring(state, &ch, 1));
}


CallFrame* fei_vm_frameget(State* state, int idx)
{
    /*
    size_t cnt;
    cnt = da_count(state->vmstate.frameobjects);
    if(idx > cnt)
    {
        CallFrame fr;
        da_push(state, state->vmstate.frameobjects, &fr);
    }
    */
    return &state->vmstate.frameobjects[idx];
}


// A void pointer is a pointer that has no associated data type with it.
// A void pointer can hold address of any type and can be typcasted to any type.
void* fei_gcmem_reallocate(State* state, void* pointer, size_t oldsize, size_t newsize)
{
    void* result;
    // self adjusting heap for garbage collection
    state->gcstate.bytesallocated += newsize - oldsize;

    // when allocating NEW memory, not when freeing as collecgarbage will cal void* reallocate itself
    if(newsize > oldsize)
    {
        #if defined(DEBUG_STRESS_GC) && (DEBUG_STRESS_GC == 1)
            fei_gcmem_collectgarbage(state);
        #endif
        // run collecter if bytesallocated is above threshold
        if(state->gcstate.bytesallocated > state->gcstate.nextgc)
        {
            fei_gcmem_collectgarbage(state);
        }
    }
    if(newsize == 0)
    {
        free(pointer);
        return NULL;
    }
    // C realloc
    result = realloc(pointer, newsize);
    // if there is not enought memory, realloc will return null
    if(result == NULL)
    {
        fprintf(stderr, "internal error: failed to realloc to %d bytes!\n", (int)newsize);
        exit(1);
    }
    return result;
}


// you can pass in a'lower' struct pointer, in this case Object*, and get the higher level which is ObjFunction
void fei_gcmem_freeobject(State* state, Object* object)// to handle different types
{
    ObjClass* klassobj;
    ObjInstance* instance;
    ObjClosure* closure;
    ObjFunction* function;
    ObjString* string;

    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("%p free type %d\n", (void*)object, object->type);
    #endif
    switch(object->type)
    {
        case OBJ_BOUND_METHOD:
            {
                FREE(state, sizeof(ObjBoundMethod), object);
            }
            break;
        case OBJ_CLASS:
            {
                klassobj = (ObjClass*)object;
                fei_table_destroy(state, &klassobj->methods);
                FREE(state, sizeof(ObjClass), object);
            }
            break;
        case OBJ_INSTANCE:
            {
                instance = (ObjInstance*)object;
                fei_table_destroy(state, &instance->fields);
                FREE(state, sizeof(ObjInstance), object);
            }
            break;
        case OBJ_CLOSURE:
            {
                // free upvalues
                closure = (ObjClosure*)object;
                FREE_ARRAY(state, sizeof(ObjUpvalue*), closure->upvalues, closure->upvaluecount);
                // only free the closure, not the function itself
                FREE(state, sizeof(ObjClosure), object);
            }
            break;
        case OBJ_FUNCTION:
            {
                function = (ObjFunction*)object;
                fei_chunk_destroy(state, &function->chunk);
                FREE(state, sizeof(ObjFunction), object);
            }
            break;
        case OBJ_NATIVE:
            {
                FREE(state, sizeof(ObjNative), object);
            }
            break;
        case OBJ_STRING:
            {
                string = (ObjString*)object;
                FREE_ARRAY(state, sizeof(char), string->chars, string->length + 1);
                FREE(state, sizeof(ObjString), object);
            }
            break;
        case OBJ_UPVALUE:
            {
                FREE(state, sizeof(ObjUpvalue), object);
            }
            break;
    }
}

/*		garbage collection		 */

void fei_gcmem_markobject(State* state, Object* object)
{
    if(object == NULL)
    {
        return;
    }
    if(object->ismarked)
    {
        return;
    }
    object->ismarked = true;
    // create a worklist of grayobjects to traverse later, use a stack to implement it
    // if need more space, allocate
    if(state->gcstate.graycapacity < state->gcstate.graycount + 1)
    {
        state->gcstate.graycapacity = GROW_CAPACITY(state->gcstate.graycapacity);
        state->gcstate.graystack = realloc(state->gcstate.graystack, sizeof(Object*) * state->gcstate.graycapacity);// use native realloc here
    }
    if(state->gcstate.graystack == NULL)
    {
        fprintf(stderr, "internal error: failed to allocate memory for gray stack\n");
        exit(1);
    }
    // add the 'gray' object to the working list
    state->gcstate.graystack[state->gcstate.graycount++] = object;
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("%p marked ", (void*)object);
        fei_value_printvalue(state, OBJ_VAL(object));// you cant print first class objects, like how you would print in the actual repl
        printf("\n");
    #endif
}

void fei_gcmem_markvalue(State* state, Value value)
{
    if(!fei_value_isobj(value))
        return;// if value is not first class Objtype return
    fei_gcmem_markobject(state, fei_value_asobj(value));
}


// marking array of values/constants of a function, used in fei_gcmem_blackenobject, case OBJ_FUNCTION
void fei_gcmem_markarray(State* state, ValArray* array)
{
    size_t i;
    for(i = 0; i < fei_valarray_count(array); i++)
    {
        fei_gcmem_markvalue(state, fei_valarray_get(state, array, i));// mark each Value in the array
    }
}

void fei_gcmem_markroots(State* state)
{
    int i;
    Value* slot;
    ObjUpvalue* upvalue;
    // assiging a pointer to a full array means assigning the pointer to the FIRST element of that array
    for(slot = state->vmstate.stack; slot < state->vmstate.stacktop; slot++)// walk through all values/slots in the Value* array
    {
        fei_gcmem_markvalue(state, *slot);
    }
    // mark closures
    for(i = 0; i < state->vmstate.framecount; i++)
    {
        fei_gcmem_markobject(state, (Object*)fei_vm_frameget(state, i)->closure);
    }
    // mark upvalues, walk through the linked list of upvalues
    for(upvalue = state->vmstate.openupvalues; upvalue != NULL; upvalue = upvalue->next)
    {
        fei_gcmem_markobject(state, (Object*)upvalue);
    }
    // mark global variables, belongs in the VM/hashtable
    fei_table_mark(state, &state->vmstate.globals);
    // compiler also grabs memory
    fei_compiler_markroots(state);
    fei_gcmem_markobject(state, (Object*)state->vmstate.initstring);
}

// actual tracing of each gray object and marking it black
void fei_gcmem_blackenobject(State* state, Object* object)
{
    int i;
    ObjBoundMethod* bound;
    ObjClass* klassobj;
    ObjInstance* instance;
    ObjFunction* function;
    ObjClosure* closure;
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("%p blackened ", (void*)object);
        fei_value_printvalue(state, OBJ_VAL(object));
        printf("\n");
    #endif
    switch(object->type)
    {
        case OBJ_BOUND_METHOD:
            {
                bound = (ObjBoundMethod*)object;
                fei_gcmem_markvalue(state, bound->receiver);
                fei_gcmem_markobject(state, (Object*)bound->method);
            }
            break;
        case OBJ_UPVALUE:
            {
                fei_gcmem_markvalue(state, ((ObjUpvalue*)object)->closed);
            }
            break;
        case OBJ_FUNCTION:
            {
                // mark the name and its value array of constants
                // you can get the coressponding 'higher' object type from a lower derivation struct in C using (higher*)lower
                function = (ObjFunction*)object;
                fei_gcmem_markobject(state, (Object*)function->name);// mark its name, an ObjString type
                fei_gcmem_markarray(state, &function->chunk.constants);// mark value array of chunk constants, pass it in AS A POINTER using &
            }
            break;
        case OBJ_CLOSURE:
            {
                // mark the function and all of the closure's upvalues
                closure = (ObjClosure*)object;
                fei_gcmem_markobject(state, (Object*)closure->function);
                for(i = 0; i < closure->upvaluecount; i++)
                {
                    fei_gcmem_markobject(state, (Object*)closure->upvalues[i]);
                }
            }
            break;
        case OBJ_CLASS:
            {
                klassobj = (ObjClass*)object;
                fei_gcmem_markobject(state, (Object*)klassobj->name);
                fei_table_mark(state, &klassobj->methods);
            }
            break;
        case OBJ_INSTANCE:
            {
                instance = (ObjInstance*)object;
                fei_gcmem_markobject(state, (Object*)instance->classobject);
                fei_table_mark(state, &instance->fields);
            }
            break;
        // these two objects contain NO OUTGOING REFERENCES there is nothing to traverse
        case OBJ_NATIVE:
        case OBJ_STRING:
            {
            }
            break;
    }
}


// traversing the gray stack work list
void fei_gcmem_tracerefs(State* state)
{
    while(state->gcstate.graycount > 0)
    {
        // pop Object* (pointer) from the stack
        // note how -- is the prefix; subtract first then use it as an index
        // --state->gcstate.graycount already decreases its count, hence everything is already 'popped'
        Object* object = state->gcstate.graystack[--state->gcstate.graycount];
        fei_gcmem_blackenobject(state, object);
    }
}


// sweeping all unreachable values
void fei_gcmem_sweep(State* state)
{
    Object* previous = NULL;
    Object* object = state->gcstate.objects;// linked intrusive list of Objects in the VM

    while(object != NULL)
    {
        if(object->ismarked)// object marked, do not free
        {
            object->ismarked = false;// reset the marking to 'white'
            previous = object;
            object = object->next;
        }
        else// free the unreachable object
        {
            Object* unreached = object;
            object = object->next;

            if(previous != NULL)// link to previous object if previous not null
            {
                previous->next = object;
            }
            else// if not set the next as the start of the list
            {
                state->gcstate.objects = object;
            }

            fei_gcmem_freeobject(state, unreached);// method that actually frees the object
        }
    }
}

void fei_gcmem_collectgarbage(State* state)
{
#if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
    printf("--Garbage Collection Begin\n");
    size_t before = state->gcstate.bytesallocated;
#endif

    fei_gcmem_markroots(state);// function to start traversing the graph, from the root and marking them
    fei_gcmem_tracerefs(state);// tracing each gray marked object

    // removing intern strings, BEFORE the sweep so the pointers can still access its memory
    // function defined in hahst.c
    fei_table_removeunreachable(state, &state->vmstate.strings);

    fei_gcmem_sweep(state);// free all unreachable roots

    // adjust size of threshold
    state->gcstate.nextgc = state->gcstate.bytesallocated * GC_HEAP_GROW_FACTOR;

#if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
    printf("--Garbage Collection End\n");
    printf("	collected %zd bytes (from %zd to %zd) next at %zd\n", before - state->gcstate.bytesallocated, before, state->gcstate.bytesallocated, state->gcstate.nextgc);
#endif
}


/*		end of garbage collection		 */


void fei_gcmem_freeobjects(State* state)// free from VM
{
    Object* object = state->gcstate.objects;
    // free from the whole list
    while(object != NULL)
    {
        Object* next = object->next;
        fei_gcmem_freeobject(state, object);
        object = next;
    }

    free(state->gcstate.graystack);// free gray marked obj stack used for garbage collection
}


// IMPORTANT
// variadic function ( ... ), takes a varying number of arguments
void fei_vm_raiseruntimeerror(State* state, const char* format, ...)
{
    va_list args;// list from the varying parameter
    va_start(args, format);
    vprintf(format, args);// unlike book, not vprintf(stderr, format, args)
    va_end(args);
    fputs("\n", stderr);// fputs; write a string to the stream but not including the null character


    // printing the stack trace for the function
    // print out each function that was still executing when the program died and where the execution was at the point it died
    for(int i = state->vmstate.framecount - 1; i >= 0; i--)
    {
        CallFrame* frame = fei_vm_frameget(state, i);
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
    // pulls from topmost CallFrame on the stack
    CallFrame* frame = fei_vm_frameget(state, state->vmstate.framecount - 1);
    // - 1 to deal with the 1 added initially for the main() CallFrame
    size_t instruction = frame->ip - frame->closure->function->chunk.code - 1;
    int line = frame->closure->function->chunk.lines[instruction];
    fprintf(stderr, "Error in script at [Line %d]\n", line);

    fei_vm_resetstack(state);
}

void fei_vm_defnative(State* state, const char* name, NativeFn function)
{
    fei_vm_pushvalue(state, OBJ_VAL(fei_object_copystring(state, name, (int)strlen(name))));// strlen to get char* length
    fei_vm_pushvalue(state, OBJ_VAL(fei_object_makenativefunc(state, function)));
    fei_table_set(state, &state->vmstate.globals, fei_value_asstring(state->vmstate.stack[0]), state->vmstate.stack[1]);
    fei_vm_popvalue(state);
    fei_vm_popvalue(state);
}


void fei_vm_resetstack(State* state)
{
    // point stackstop to the begininng of the empty array
    state->vmstate.stacktop = state->vmstate.stack;// stack array(state->vmstate.stack) is already indirectly declared, hence no need to allocate memory for it
    state->vmstate.framecount = 0;
    state->vmstate.openupvalues = NULL;
}

State* fei_state_init()
{
    State* state;
    state = (State*)malloc(sizeof(State));
    memset(state, 0, sizeof(State));
    state->aststate.compiler = NULL;
    state->aststate.classcompiler = NULL;
    fei_vm_resetstack(state);// initialiing the Value stack, also initializing the callframe count
    {
        state->gcstate.objects = NULL;
        // initializing gray marked obj stack for garbage collection
        state->gcstate.graycapacity = 0;
        state->gcstate.graycount = 0;
        state->gcstate.graystack = NULL;

        // self adjusting heap to control frequency of GC
        state->gcstate.bytesallocated = 0;
        state->gcstate.nextgc = 1024 * 1024;
    }
    fei_table_init(state, &state->vmstate.globals);
    fei_table_init(state, &state->vmstate.strings);
    {
        // init initalizer string
        state->vmstate.initstring = NULL;
        state->vmstate.initstring = fei_object_copystring(state, "init", 4);
        //state->vmstate.frameobjects = da_make(state, state->vmstate.frameobjects, 0, sizeof(CallFrame));
    }
    {
        fei_vm_defnative(state, "clock", cfn_clock);
        fei_vm_defnative(state, "chr", cfn_chr);
        fei_vm_defnative(state, "print", cfn_print);
        fei_vm_defnative(state, "println", cfn_println);
    }
    state->outwriter = fei_writer_initfile(state, stdout, false);
    return state;
}

void fei_state_destroy(State* state)
{
    state->vmstate.initstring = NULL;
    fei_gcmem_freeobjects(state);// free all objects, from state->gcstate.objects
    fei_table_destroy(state, &state->vmstate.globals);
    fei_table_destroy(state, &state->vmstate.strings);
    fei_writer_destroy(state, state->outwriter);
    free(state);
}

/* stack operations */
void fei_vm_pushvalue(State* state, Value value)
{
    *state->vmstate.stacktop = value;// * in front of the pointer means the rvalue itself, assign value(parameter) to it
    state->vmstate.stacktop++;
}

Value fei_vm_popvalue(State* state)
{
    state->vmstate.stacktop--;// first move the stack BACK to get the last element(stacktop points to ONE beyond the last element)
    return *state->vmstate.stacktop;
}
/* end of stack operations */

// PEEK from the STACK, AFTER the compiler passes it through
// return a value from top of the stack but does not pop it, distance being how far down
// this is a C kind of accessing arrays/pointers
Value fei_vm_peekvalue(State* state, int distance)
{
    return state->vmstate.stacktop[-1 - distance];
}

/* for call stacks/functions  */
bool fei_vm_callclosure(State* state, ObjClosure* closure, int argcount)
{
    if(argcount != closure->function->arity)// if number of parameters does not match
    {
        fei_vm_raiseruntimeerror(state, "Expected %d arguments but got %d", closure->function->arity, argcount);
        return false;
    }

    // as CallFrame is an array, to ensure array does not overflow
    if(state->vmstate.framecount == CFG_MAX_VMFRAMES)
    {
        fprintf(stderr, "internal error: stack overflow!\n");
        fei_vm_raiseruntimeerror(state, "Stack overflow.");
        return false;
    }

    // get pointer to next in frame array
    // initializes callframe to the top of the stack
    CallFrame* frame = fei_vm_frameget(state, state->vmstate.framecount++);
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;

    // set up slots pointer to give frame its window into the stack
    // ensures everyting lines up
    // slots is the 'starting pointer' for the function cll
    frame->slots = state->vmstate.stacktop - argcount - 1;
    return true;
}

bool fei_vm_callvalue(State* state, Value callee, int argcount)
{
    if(fei_value_isobj(callee))
    {
        switch(OBJ_TYPE(callee))
        {
            case OBJ_BOUND_METHOD:
            {
                ObjBoundMethod* bound = fei_value_asbound_method(callee);// get ObjBoundMethod from value type(callee)
                state->vmstate.stacktop[-argcount - 1] = bound->receiver;// set [-] inside square brackes of top stack pointer to go down the stack
                return fei_vm_callclosure(state, bound->method, argcount);//	run call to execute
            }
            case OBJ_CLASS:// create class instance
            {
                ObjClass* klass = fei_value_asclass(callee);
                // create new instance here
                state->vmstate.stacktop[-argcount - 1] = OBJ_VAL(fei_object_makeinstance(state, klass));// - argcounts as above values are parameters

                // initializer
                Value initializer;
                // if we find one from the table
                if(fei_table_get(state, &klass->methods, state->vmstate.initstring, &initializer))// have a state->vmstate.initstring as 'token', ObjString type
                {
                    return fei_vm_callclosure(state, fei_value_asclosure(initializer), argcount);
                }
                else if(argcount != 0)// if there ARE arguments but the initalizer method cannot be found
                {
                    fei_vm_raiseruntimeerror(state, "Expected 0  arguments but got %d\n", argcount);
                    return false;
                }

                return true;
            }
            case OBJ_CLOSURE:// ensure type is function
                {
                    // call to function happens here
                    return fei_vm_callclosure(state, fei_value_asclosure(callee), argcount);
                }
                break;
            case OBJ_NATIVE:
                {
                    NativeFn natfn = fei_value_asnative(callee);
                    Value result = natfn(state, argcount, state->vmstate.stacktop - argcount);
                    // remove call and arguments from the stack
                    state->vmstate.stacktop -= argcount + 1;
                    fei_vm_pushvalue(state, result);
                    return true;
                }
                break;
            default:
                break;
        }
    }

    fei_vm_raiseruntimeerror(state, "Non-function or non-class type is called.");
    return false;
}


bool fei_class_invokemethod(State* state, ObjClass* klassobj, ObjString* name, int argcount)
{
    Value method;
    if(!fei_table_get(state, &klassobj->methods, name, &method))
    {
        fei_vm_raiseruntimeerror(state, "Undefined property '%s'.", name->chars);
        return false;
    }

    return fei_vm_callclosure(state, fei_value_asclosure(method), argcount);
}


// invoke class method, access method + call method
bool fei_vm_stackinvoke(State* state, ObjString* name, int argcount)
{
    Value receiver = fei_vm_peekvalue(state, argcount);// grab the receiver of the stack

    // call method with wrong type, not an objinstance type
    if(!fei_value_isinstance(receiver))
    {
        fei_vm_raiseruntimeerror(state, "Tried to invoke a method from a non instance object.");
        return false;
    }

    ObjInstance* instance = fei_value_asinstance(receiver);

    // for fields()
    Value value;
    if(fei_table_get(state, &instance->fields, name, &value))
    {
        state->vmstate.stacktop[-argcount - 1] = value;
        return fei_vm_callvalue(state, value, argcount);
    }


    return fei_class_invokemethod(state, instance->classobject, name, argcount);// actual function that searches for method and calls it
}


// bind method and wrap it in a new ObjBoundMethod
bool fei_class_bindmethod(State* state, ObjClass* klassobj, ObjString* name)
{
    Value method;
    if(!fei_table_get(state, &klassobj->methods, name, &method))// get method from table and bind it
    {
        // if method not found
        fei_vm_raiseruntimeerror(state, "Undefined property %s.", name->chars);
        return false;
    }
    ObjBoundMethod* bound = fei_object_makeboundmethod(state, fei_vm_peekvalue(state, 0), fei_value_asclosure(method));// wrap method in a new ObjBoundMethodd

    fei_vm_popvalue(state);// pop the class instance
    fei_vm_pushvalue(state, OBJ_VAL(bound));
    return true;
}


// get corresponding upvalue
ObjUpvalue* fei_vm_captureupvalue(State* state, Value* local)
{
    // set up the linked list
    ObjUpvalue* upvalue;
    ObjUpvalue* prevupvalue;
    ObjUpvalue* createdupvalue;
    prevupvalue = NULL;

    // assign at the start of the list
    upvalue = state->vmstate.openupvalues;

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
        prevupvalue = upvalue;
        upvalue = upvalue->next;
    }
    if(upvalue != NULL && upvalue->location == local)// if the location/local/indeces match
    {
        // return already created upvalue
        return upvalue;
    }
    createdupvalue = fei_object_makeupvalue(state, local);
    // insert at the front
    createdupvalue->next = upvalue;
    // ran out of values to search
    if(prevupvalue == NULL)
    {
        // set pointer to the newly added upvalue
        state->vmstate.openupvalues = createdupvalue;
    }
    // found local slot BELOW the one we are looking for
    else
    {
        // link next slot(the value below) to the newly inserted upvalue
        prevupvalue->next = createdupvalue;
    }

    return createdupvalue;
}

// closes every upvalue it can find that points to the slot or any above the stack
void fei_vm_closeupvalues(State* state, Value* last)// takes pointer to stack slot
{
    while(state->vmstate.openupvalues != NULL && state->vmstate.openupvalues->location >= last)
    {
        ObjUpvalue* upvalue = state->vmstate.openupvalues;// pointer to list of openupvalues
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        state->vmstate.openupvalues = upvalue->next;
    }
}

// defining method for class type
void fei_vm_stackdefmethod(State* state, ObjString* name)
{
    Value method = fei_vm_peekvalue(state, 0);// method/closure is at the top of the stack
    ObjClass* klassobj = fei_value_asclass(fei_vm_peekvalue(state, 1));// class is at the 2nd top
    fei_table_set(state, &klassobj->methods, name, method);// add to hashtable
    fei_vm_popvalue(state);// pop the method
}


// comparison for OP_NOT
bool fei_value_isfalsey(State* state, Value value)
{
    bool test;
    (void)state;
    // return true if value is the null type or if it is a false bool type
    test = (
        fei_value_isnull(value) ||
        (fei_value_isbool(value) && !fei_value_asbool(value))
    );
    return test;
}

// string concatenation
void fei_vmdo_strconcat(State* state)
{
    int length;
    char* chars;
    ObjString* first;
    ObjString* second;
    ObjString* result;
    // peek, so we do not pop it off if calling a GC is needed
    second = fei_value_asstring(fei_vm_peekvalue(state, 0));
    first = fei_value_asstring(fei_vm_peekvalue(state, 1));
    length = first->length + second->length;
    // dynamically allocate memory for the char, chars is now a NULL string
    chars = (char*)ALLOCATE(state, sizeof(char), length + 1);
    // IMPORTANt -> use memcpy when assinging to a char* pointer
    // memcpy function, copy to chars, from first->chars, with second->length number of bits
    memcpy(chars, first->chars, first->length);
    // remember to add the first length of bits to chars again, so it will START AFTER the given offset
    memcpy(chars + first->length, second->chars, second->length);
    chars[length] = '\0';
    result = fei_object_takestring(state, chars, length);
    // pop the two strings, garbage collection
    fei_vm_popvalue(state);
    fei_vm_popvalue(state);
    fei_vm_pushvalue(state, OBJ_VAL(result));
}


/* starting point of the compiler */
ResultCode fei_vm_evalsource(State* state, const char* source, size_t len)
{
    ObjClosure* closure;
    ObjFunction* function;
    function = fei_compiler_compilesource(state, source, len);
    if(function == NULL)
    {
        return INTERPRET_COMPILE_ERROR;// NULL gets passed from compiler
    }
    fei_vm_pushvalue(state, OBJ_VAL(function));
    closure = fei_object_makeclosure(state, function);
    fei_vm_popvalue(state);
    fei_vm_pushvalue(state, OBJ_VAL(closure));
    // 0 params for main()
    fei_vm_callvalue(state, OBJ_VAL(closure), 0);
    return fei_vm_exec(state);
}


    /* info on the macros below
Below macros are FUNCTIONSt that take ZERO arguments, and what is inside () is their return value
READ_BYTE:	
	macro to ACCESS the BYTE(uin8_t) from the POINTER(ip), and increment it
	reads byte currently pointed at ip, then advances the instruction pointer
READ_CONSTANT:
	return constants.values element, from READ_BYTE(), which points exactly to the NEXT index
READ STRING:
	return as object string, read directly from the vm(oip)
*/

static inline uint8_t READ_BYTE(CallFrame* frame)
{
    return (*frame->ip++);
}

static inline Value READ_CONSTANT(State* state, CallFrame* frame)
{
    return fei_valarray_get(state, &frame->closure->function->chunk.constants, READ_BYTE(frame));
}

static inline ObjString* READ_STRING(State* state, CallFrame* frame)
{
    return fei_value_asstring(READ_CONSTANT(state, frame));
}

// for patch jumps
// yanks next two bytes from the chunk(used to calculate the offset earlier) and return a 16-bit integer out of it
// use bitwise OR
static inline uint16_t READ_SHORT(CallFrame* frame)
{
    frame->ip += 2;
    return (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]);
}

// MACRO for binary operations
// take two last constants, and push ONE final value doing the operations on both of them
// this macro needs to expand to a series of statements, read a-virtual-machine for more info, this is a macro trick or a SCOPE BLOCK
// pass in an OPERAOTR as a MACRO
// valuetype is a Value struct
// first check that both operands are numbers
#define BINARY_OP(valuetype, op, downcasttype)                                 \
    do                                                                         \
    {                                                                          \
        if(!fei_value_isnumber(fei_vm_peekvalue(state, 0)) || !fei_value_isnumber(fei_vm_peekvalue(state, 1))) \
        {                                                                      \
            fei_vm_raiseruntimeerror(state, "Operands must be numbers.");             \
            return INTERPRET_RUNTIME_ERROR;                                    \
        }                                                                      \
        downcasttype b = (downcasttype)fei_value_asnumber(fei_vm_popvalue(state));           \
        downcasttype a = (downcasttype)fei_value_asnumber(fei_vm_popvalue(state));           \
        fei_vm_pushvalue(state, valuetype(a op b));                                   \
    } while(false)



// run the chunk
// most IMPORTANT part of the interpreter
ResultCode fei_vm_exec(State* state)// static means the scope of the function is only to this file
{
    uint8_t instruction;
    CallFrame* frame;
    frame = fei_vm_frameget(state, state->vmstate.framecount - 1);

    for(;;)
    {
        // fei_dbgdisas_instr needs an byte offset, do pointer math to convert ip back to relative offset
        // from the beginning of the chunk (subtract current ip from the starting ip)
        // IMPORTANT -> only for debugging the VM

#if defined(DEBUG_TRACE_EXECUTION) && (DEBUG_TRACE_EXECUTION == 1)
        // for stack tracing
        printf("		");
        /* note on C POINTERSE
        -> pointing to the array itself means pointing to the start of the array, or the first element of the array
        -> ++/-- means moving through the array (by 1 or - 1)
        -> you can use operands like < > to tell compare how deep are you in the array
        */
        // prints every existing value in the stack
        for(Value* slot = state->vmstate.stack; slot < state->vmstate.stacktop; slot++)
        {
            printf("[ ");
            fei_value_printvalue(state, *slot);
            printf(" ]");
        }
        fei_dbgdisas_instr(state, &frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code));
#endif
        switch(instruction = READ_BYTE(frame))// get result of the byte read, every set of instruction starts with an opcode
        {
            case OP_CONSTANT:
            {
                // function is smart; chunk advances by 1 on first read, then in the READ_CONSTANT() macro it reads again which advances by 1 and returns the INDEX
                Value constant = READ_CONSTANT(state, frame);// READ the next line, which is the INDEX of the constant in the constants array
                fei_vm_pushvalue(state, constant);// push to stack
                break;// break from the switch
            }
            // unary opcode
            case OP_NEGATE:
                if(!fei_value_isnumber(fei_vm_peekvalue(state, 0)))// if next value is not a number
                {
                    //printf("\nnot a number\n"); it actually works
                    fei_vm_raiseruntimeerror(state, "Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                fei_vm_pushvalue(state, NUMBER_VAL(-fei_value_asnumber(fei_vm_popvalue(state))));
                break;// negates the last element of the stack

            // literals
            case OP_NULL:
                fei_vm_pushvalue(state, NULL_VAL);
                break;
            case OP_TRUE:
                fei_vm_pushvalue(state, BOOL_VAL(true));
                break;
            case OP_FALSE:
                fei_vm_pushvalue(state, BOOL_VAL(false));
                break;

            // binary opcode
            case OP_ADD:
            {
                if(fei_value_isstring(fei_vm_peekvalue(state, 0)) && fei_value_isstring(fei_vm_peekvalue(state, 1)))// if last two constants are strings
                {
                    fei_vmdo_strconcat(state);
                }
                else if(fei_value_isnumber(fei_vm_peekvalue(state, 0)) && fei_value_isnumber(fei_vm_peekvalue(state, 1)))
                {
                    // in the book, macro is not used and a new algorithm is used directly
                    BINARY_OP(NUMBER_VAL, +, double);// initialize new Value struct (NUMBER_VAL) here
                }
                else// handle errors dynamically here
                {
                    //printf("operands error");
                    fei_vm_raiseruntimeerror(state, "Operands are incompatible.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_SUBTRACT:
                BINARY_OP(NUMBER_VAL, -, double);
                break;
            case OP_MULTIPLY:
                BINARY_OP(NUMBER_VAL, *, double);
                break;
            case OP_DIVIDE:
                BINARY_OP(NUMBER_VAL, /, double);
                break;

            case OP_MODULO:
                BINARY_OP(NUMBER_VAL, %, int);
                break;

            case OP_NOT:
                fei_vm_pushvalue(state, BOOL_VAL(fei_value_isfalsey(state, fei_vm_popvalue(state))));// again, pops most recent one from the stack, does the operation on it, and pushes it back
                break;

            // for switch eqal
            case OP_SWITCH_EQUAL:
            {
                Value b = fei_vm_popvalue(state);// only pop second value
                Value a = fei_vm_peekvalue(state, 0);// peek topmost, the first value
                fei_vm_pushvalue(state, BOOL_VAL(fei_value_compare(state, a, b)));
                break;
            }

            case OP_EQUAL:// implemenation comparison done here
            {
                Value b = fei_vm_popvalue(state);
                Value a = fei_vm_popvalue(state);
                fei_vm_pushvalue(state, BOOL_VAL(fei_value_compare(state, a, b)));
                break;
            }
            case OP_GREATER:
                BINARY_OP(BOOL_VAL, >, double);
                break;
            case OP_LESS:
                BINARY_OP(BOOL_VAL, <, double);
                break;


            case OP_PRINT:
            {
                // ACTUAL PRINTING IS DONE HERE
                fei_value_printvalue(state, fei_vm_popvalue(state));// pop the stack and print the value, getting it from value.c
                printf("\n");
                break;
            }

            case OP_POP:
                fei_vm_popvalue(state);
                break;

            case OP_GET_LOCAL:
            {
                uint8_t slot = READ_BYTE(frame);
                fei_vm_pushvalue(state, frame->slots[slot]);// pushes the value to the stack where later instructions can read it
                break;
            }

            case OP_SET_LOCAL:
            {
                uint8_t slot = READ_BYTE(frame);
                // all the local var's VARIABLES are stored inside state->vmstate.stack
                frame->slots[slot] = fei_vm_peekvalue(state, 0);// takes from top of the stack and stores it in the stack slot
                break;
            }

            case OP_DEFINE_GLOBAL:
            {
                ObjString* name = READ_STRING(state, frame);// get name from constant table
                fei_table_set(state, &state->vmstate.globals, name, fei_vm_peekvalue(state, 0));// take value from the top of the stack
                fei_vm_popvalue(state);
                break;
            }

            case OP_GET_GLOBAL:
            {
                ObjString* name = READ_STRING(state, frame);// get the name
                Value value;// create new Value
                if(!fei_table_get(state, &state->vmstate.globals, name, &value))// if key not in hash table
                {
                    fei_vm_raiseruntimeerror(state, "Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                fei_vm_pushvalue(state, value);
                break;
            }

            case OP_SET_GLOBAL:
            {
                ObjString* name = READ_STRING(state, frame);
                if(fei_table_set(state, &state->vmstate.globals, name, fei_vm_peekvalue(state, 0)))// if key not in hash table
                {
                    fei_table_delete(state, &state->vmstate.globals, name);// delete the false name
                    fei_vm_raiseruntimeerror(state, "Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            // upvalues set/get
            case OP_GET_UPVALUE:
                {
                    // read index
                    uint8_t slot = READ_BYTE(frame);
                    // push the value to the stack
                    fei_vm_pushvalue(state, *frame->closure->upvalues[slot]->location);
                }
                break;
            case OP_SET_UPVALUE:
                {
                    // read index
                    uint8_t slot = READ_BYTE(frame);
                    // set to the topmost stack
                    *frame->closure->upvalues[slot]->location = fei_vm_peekvalue(state, 0);
                }
                break;

            case OP_GET_PROPERTY:
                {
                    Value value;
                    // to make sure only instances are allowed to have fields
                    if(!fei_value_isinstance(fei_vm_peekvalue(state, 0)))
                    {
                        fei_vm_raiseruntimeerror(state, "Only instances have properties.");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    // get instance from top most stack
                    ObjInstance* instance = fei_value_asinstance(fei_vm_peekvalue(state, 0));
                    // get identifier name
                    ObjString* name = READ_STRING(state, frame);
                    if(fei_table_get(state, &instance->fields, name, &value))// get from fields hash table, assign it to instance
                    {
                        fei_vm_popvalue(state);// pop the instance itself
                        fei_vm_pushvalue(state, value);
                        break;
                    }
                    // no method as well, error
                    if(!fei_class_bindmethod(state, instance->classobject, name))
                    {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                }
                break;

            case OP_SET_PROPERTY:
                {
                    if(!fei_value_isinstance(fei_vm_peekvalue(state, 1)))// if not an instance
                    {
                        fei_vm_raiseruntimeerror(state, "Identifier must be a class instance.");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    // not top most, as the top most is reserved for the new value to be set
                    ObjInstance* instance = fei_value_asinstance(fei_vm_peekvalue(state, 1));
                    fei_table_set(state, &instance->fields, READ_STRING(state, frame), fei_vm_peekvalue(state, 0));//peek(0) is the new value
                    Value value = fei_vm_popvalue(state);// pop the already set value
                    fei_vm_popvalue(state);// pop the property instance itself
                    fei_vm_pushvalue(state, value);// push the value back again
                }
                break;


            case OP_CLOSE_UPVALUE:
                {
                    fei_vm_closeupvalues(state, state->vmstate.stacktop - 1);// put address to the slot
                    fei_vm_popvalue(state);// pop from the stack
                }
                break;
            case OP_JUMP:// will always jump
                {
                    uint16_t offset = READ_SHORT(frame);
                    frame->ip += offset;
                }
                break;
            case OP_JUMP_IF_FALSE:// for initial if, will not jump if expression inside is true
                {
                    // offset already put in the stack
                    uint16_t offset = READ_SHORT(frame);
                    // actual jump instruction is done here; skip over the instruction pointer
                    if(fei_value_isfalsey(state, fei_vm_peekvalue(state, 0)))
                    {
                        // if evaluated expression inside if statement is false jump
                        frame->ip += offset;
                    }
                }
                break;
            case OP_LOOP:
                {
                    uint16_t offset = READ_SHORT(frame);
                    frame->ip -= offset;// jumps back
                }
                break;
            case OP_LOOP_IF_FALSE:
                {
                    // offset already put in the stack
                    uint16_t offset = READ_SHORT(frame);
                    // bool state is at the top of the stack
                    // if false loop back
                    if(fei_value_isfalsey(state, fei_vm_peekvalue(state, 0)))
                    {
                        frame->ip -= offset;
                    }
                    // pop the true/false
                    fei_vm_popvalue(state);
                }
                break;

            case OP_LOOP_IF_TRUE:
                {
                    // offset already put in the stack
                    uint16_t offset = READ_SHORT(frame);
                    // bool state is at the top of the stack
                    // if not false loop back
                    if(!fei_value_isfalsey(state, fei_vm_peekvalue(state, 0)))
                    {
                        frame->ip -= offset;
                    }
                    fei_vm_popvalue(state);// pop the true/false
                }
                break;
            // a callstack to a funcion has the form of function name, param1, param2...
            // the top level code, or caller, also has the same function name, param1, param2... in the right order
            case OP_CALL:
                {
                    int argcount = READ_BYTE(frame);
                    // call function; pass in the function name istelf[peek(depth)] and the number of arguments
                    if(!fei_vm_callvalue(state, fei_vm_peekvalue(state, argcount), argcount))
                    {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    // to update pointer if callframe is successful, asnew frame is added
                    frame = fei_vm_frameget(state, state->vmstate.framecount - 1);
                }
                break;
            // closures
            case OP_CLOSURE:
                {
                    // load compiled function from table
                    ObjFunction* function = fei_value_asfunction(READ_CONSTANT(state, frame));
                    ObjClosure* closure = fei_object_makeclosure(state, function);
                    fei_vm_pushvalue(state, OBJ_VAL(closure));
                    // fill upvalue array over in the interpreter when a closure is created
                    // to see upvalues in each slot
                    for(int i = 0; i < closure->upvaluecount; i++)
                    {
                        // read islocal bool
                        uint8_t islocal = READ_BYTE(frame);
                        // read index for local, if available, in the closure
                        uint8_t index = READ_BYTE(frame);
                        if(islocal)
                        {
                            // get from slots stack
                            closure->upvalues[i] = fei_vm_captureupvalue(state, frame->slots + index);
                        }
                        // if not local(nested upvalue)
                        else
                        {
                            // get from current upvalue
                            closure->upvalues[i] = frame->closure->upvalues[index];
                        }
                    }
                }
                break;
            case OP_CLASS:
                {
                    // load string for the class' name and push it onto the stack
                    fei_vm_pushvalue(state, OBJ_VAL(fei_object_makeclass(state, READ_STRING(state, frame))));
                }
                break;
            case OP_METHOD:
                {
                    // get name of the method
                    fei_vm_stackdefmethod(state, READ_STRING(state, frame));
                }
                break;
            case OP_INVOKE:
                {
                    ObjString* method = READ_STRING(state, frame);
                    int argcount = READ_BYTE(frame);
                    // new invoke function
                    if(!fei_vm_stackinvoke(state, method, argcount))
                    {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    frame = fei_vm_frameget(state, state->vmstate.framecount - 1);
                }
                break;
            case OP_INHERIT:
                {
                    // parent class from 2nd top of the stack
                    // ensure that parent identifier is a class
                    Value parent = fei_vm_peekvalue(state, 1);
                    if(!fei_value_isclass(parent))
                    {
                        fei_vm_raiseruntimeerror(state, "Parent identifier is not a class.");
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    // child class at the top of the stack
                    ObjClass* child = fei_value_asclass(fei_vm_peekvalue(state, 0));
                    // add all methods from parent to child table
                    fei_table_mergefrom(state, &fei_value_asclass(parent)->methods, &child->methods);
                    // pop the child class
                    fei_vm_popvalue(state);
                }
                break;
            case OP_GET_SUPER:
                {
                    // get method name/identifier
                    ObjString* name = READ_STRING(state, frame);
                    // class identifier is at the top of the stack
                    ObjClass* parent = fei_value_asclass(fei_vm_popvalue(state));
                    // if binding fails
                    if(!fei_class_bindmethod(state, parent, name))
                    {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                }
                break;
            case OP_SUPER_INVOKE:
                {
                    ObjString* method = READ_STRING(state, frame);
                    int count = READ_BYTE(frame);
                    ObjClass* parent = fei_value_asclass(fei_vm_popvalue(state));
                    if(!fei_class_invokemethod(state, parent, method, count))
                    {
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    frame = fei_vm_frameget(state, state->vmstate.framecount - 1);
                }
                break;
            case OP_RETURN:
                {
                    // if function returns a value, value will beon top of the stack
                    Value result = fei_vm_popvalue(state);
                    // close lingering closed values
                    fei_vm_closeupvalues(state, frame->slots);
                    state->vmstate.framecount--;
                    // return from 'main()'/script function
                    if(state->vmstate.framecount == 0)
                    {
                        // pop main script function from the stack
                        fei_vm_popvalue(state);
                        return INTERPRET_OK;
                    }
                    // for a function
                    // discard all the slots the callee was using for its parameters
                    // basically 're-assign'
                    state->vmstate.stacktop = frame->slots;
                    // push the return value
                    fei_vm_pushvalue(state, result);
                    // update run function's current frame
                    frame = fei_vm_frameget(state, state->vmstate.framecount - 1);
                }
                break;

        }
    }


#undef BINARY_OP
}


char* readhandle(FILE* hnd, size_t* dlen)
{
    long rawtold;
    /*
    * the value returned by ftell() may not necessarily be the same as
    * the amount that can be read.
    * since we only ever read a maximum of $toldlen, there will
    * be no memory trashing.
    */
    size_t toldlen;
    size_t actuallen;
    char* buf;
    if(fseek(hnd, 0, SEEK_END) == -1)
    {
        return NULL;
    }
    if((rawtold = ftell(hnd)) == -1)
    {
        return NULL;
    }
    toldlen = rawtold;
    if(fseek(hnd, 0, SEEK_SET) == -1)
    {
        return NULL;
    }
    buf = (char*)malloc(toldlen + 1);
    memset(buf, 0, toldlen+1);
    if(buf != NULL)
    {
        actuallen = fread(buf, sizeof(char), toldlen, hnd);
        /*
        // optionally, read remainder:
        size_t tmplen;
        if(actuallen < toldlen)
        {
            tmplen = actuallen;
            actuallen += fread(buf+tmplen, sizeof(char), actuallen-toldlen, hnd);
            ...
        }
        // unlikely to be necessary, so not implemented.
        */
        if(dlen != NULL)
        {
            *dlen = actuallen;
        }
        return buf;
    }
    return NULL;
}

char* readfile(const char* filename, size_t* dlen)
{
    char* b;
    FILE* fh;
    if((fh = fopen(filename, "rb")) == NULL)
    {
        return NULL;
    }
    b = readhandle(fh, dlen);
    fclose(fh);
    return b;
}


// for REPL, the print eval read loop
void repl(State* state)
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

        fei_vm_evalsource(state, line, strlen(line));
    }
}


// function for loading scripts
void runfile(State* state, const char* path)
{
    size_t len;
    char* source;
    source = readfile(path, &len);// get raw source code from the file
    ResultCode result = fei_vm_evalsource(state, source, len);// get enum type result from VM
    free(source);// free the source code

    if(result == INTERPRET_COMPILE_ERROR)
        exit(51);
    if(result == INTERPRET_RUNTIME_ERROR)
        exit(61);
}

#define ptyp(t) \
    fprintf(stderr, "%d\tsizeof(%s)\n", (int)sizeof(t), #t)

int main(int argc, const char* argv[])// used in the command line, argc being the amount of arguments and argv the array
{
    State* state;

    state = fei_state_init();

    {
ptyp(Value);
ptyp(Object);
ptyp(ObjString);
ptyp(ObjFunction);
ptyp(ObjClass);
ptyp(ObjInstance);
ptyp(ObjBoundMethod);
ptyp(ObjUpvalue);
ptyp(ObjClosure);
ptyp(ObjNative);
ptyp(Local);
ptyp(Upvalue);
ptyp(ASTState);
ptyp(GCState);
ptyp(VMState);
ptyp(State);
ptyp(CallFrame);
ptyp(Token);
ptyp(Scanner);
ptyp(Parser);
ptyp(ParseRule);
ptyp(Compiler);
ptyp(ClassCompiler);
ptyp(ValArray);
ptyp(Chunk);
ptyp(TabEntry);
ptyp(Table);

    }

    // the FIRST argument will always be the name of the executable being run(e.g node, python in terminal)

    if(argc == 1)// if number of argument is one, run the repl
    {
        repl(state);
    }
    else if(argc == 2)// if number of arguments is two, the second one being the file, run the second file
    {
        runfile(state, argv[1]);
    }
    else
    {
        fprintf(stderr, "Usage: cfei [path]\n");// fprintf; print on file but not on console, first argument being the file pointer
        // in this case it prints STANDARD ERROR
        exit(64);
    }

    fei_state_destroy(state);
    return 0;
}
