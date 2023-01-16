
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

#ifndef __TINYC__
    #if __has_include(<readline/readline.h>)
        #include <readline/readline.h>
        #include <readline/history.h>
        #define FEI_HAVE_READLINE
    #endif
#endif

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
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, { .valnumber = (double)(value) } })
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
    STATUS_OK,
    STATUS_SYNTAXERROR,
    STATUS_RTERROR
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
    //Local* proglocals;

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
    int level;
    int stackpos;

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


#if !defined(DEBUG_PRINTTYPES)
    #define DEBUG_PRINTTYPES 0
#endif
#if !defined(USE_DYNVALUE)
    #define USE_DYNVALUE 0
#endif


struct VMState
{

    int stackindex;

    // pointer to the element just PAST the element containing the top value of the stack
    // == stackvalues[size(stackvalues) - 2] 
    #if defined(USE_DYNVALUE) && (USE_DYNVALUE == 1)
        Value* stackvalues;
        int64_t stacktop;
    #else
        // stack array is 'indirectly' declared inline here
        Value stackvalues[CFG_MAX_VMSTACK];

        Value* stacktop;
    #endif

    // for storing global variables
    Table globals;

    // for string interning, to make sure every equal string takes one memory
    Table strings;

    // init string for class constructors
    ObjString* initstring;

    // track all upvalues; points to the first node of the linked list
    ObjUpvalue* openupvalues;

    CallFrame* topframe;

    // stores current height of the stack
    int framecount;

    // since the whole program is one big 'main()' use callstacks
    //CallFrame frameobjects[CFG_MAX_VMFRAMES];
    CallFrame** frameobjects;


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
void fei_value_printvalue(State *state, FILE* fh, Value value, bool withquot);
void fei_value_printfunc(State *state, FILE* fh, ObjFunction *function);
void fei_value_printobject(State *state, FILE* fh, Value value, bool withquot);
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

void fei_table_initcapacity(State* state, Table* table, int cap);
void fei_table_initempty(State *state, Table *table);
void fei_table_initnull(State *state, Table *table);
void fei_table_destroy(State *state, Table *table);
TabEntry *fei_table_findentry(State *state, int count, TabEntry *entries, int capacity, ObjString *key);
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

bool fei_class_invokemethod(State *state, ObjClass *klassobj, ObjString *name, int argcount);
bool fei_class_bindmethod(State *state, ObjClass *klassobj, ObjString *name);

CallFrame* fei_vm_frameget(State* state, int idx); 
void fei_vm_stackpush(State *state, Value value);
Value fei_vm_stackpop(State *state);
Value fei_vm_stackpeek(State* state, int distance);

bool fei_vm_callclosure(State *state, ObjClosure *closure, int argcount);
bool fei_vm_callvalue(State *state, Value callee, int argcount);
bool fei_vm_classinvokefromstack(State *state, ObjString *name, int argcount);
ObjUpvalue *fei_vm_captureupvalue(State *state, Value *local);
void fei_vm_closeupvalues(State *state, Value *last);
void fei_vm_classdefmethodfromstack(State *state, ObjString *name);
bool fei_value_isfalsey(State *state, Value value);
void fei_vmdo_strconcat(State *state);
ResultCode fei_vm_evalsource(State *state, const char *source, size_t len);
ResultCode fei_vm_exec(State *state);
void repl(State *state);
void runfile(State *state, const char *path);


// from VALUE STRUCT to RAW  C nicely used in printing
// also for comparisons -> use values ALREADY CONVERTED to Value struct union to raw C
bool fei_value_asbool(Value v)
{
    return v.as.valbool;
}

double fei_value_asnumber(Value v)
{
    return v.as.valnumber;
}

Object* fei_value_asobj(Value v)
{
    return v.as.valobjptr;
}

ObjType OBJ_TYPE(Value v)
{
    return fei_value_asobj(v)->type;
}

ObjBoundMethod* fei_value_asbound_method(Value v)
{
    return (ObjBoundMethod*)fei_value_asobj(v);
};

ObjClass* fei_value_asclass(Value v)
{
    return (ObjClass*)fei_value_asobj(v);
}

ObjInstance* fei_value_asinstance(Value v)
{
    return (ObjInstance*)fei_value_asobj(v);
}

ObjClosure* fei_value_asclosure(Value v)
{
    return (ObjClosure*)fei_value_asobj(v);
}

ObjString* fei_value_asstring(Value v)
{
    return (ObjString*)fei_value_asobj(v);
}

char* fei_value_ascstring(Value v)
{
    return fei_value_asstring(v)->chars;
}

ObjFunction* fei_value_asfunction(Value v)
{
    return (ObjFunction*)fei_value_asobj(v);
}

NativeFn fei_value_asnative(Value v)
{
    return ((ObjNative*)fei_value_asobj(v))->function;
}

bool fei_object_istype(Value value, ObjType type)
{
    return fei_value_isobj(value) && fei_value_asobj(value)->type == type;
}

bool fei_value_isbound_method(Value v)
{
    return fei_object_istype(v, OBJ_BOUND_METHOD);
}

bool fei_value_isclass(Value v)
{
    return fei_object_istype(v, OBJ_CLASS);
}

bool fei_value_isfunction(Value v)
{
    return fei_object_istype(v, OBJ_FUNCTION);
}

bool fei_value_isinstance(Value v)
{
    return fei_object_istype(v, OBJ_INSTANCE);
}

bool fei_value_isnative(Value v)
{
    return fei_object_istype(v, OBJ_NATIVE);
}

bool fei_value_isstring(Value v)
{
    return fei_object_istype(v, OBJ_STRING);
}

bool fei_value_isclosure(Value v)
{
    return fei_object_istype(v, OBJ_CLOSURE);
}

Writer* fei_writer_init(State* state)
{
    Writer* wr;
    wr = (Writer*)ALLOCATE(state, sizeof(Writer), 1);
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

void fei_writer_appendstringlen(Writer* wr, const char* str, size_t len)
{
    if(wr->filehandle != NULL)
    {
        fwrite(str, len, sizeof(char), wr->filehandle);
    }
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
            {
                return fei_value_asbool(a) == fei_value_asbool(b);
            }
            break;
        case VAL_NUMBER:
            {
                return fei_value_asnumber(a) == fei_value_asnumber(b);
            }
            break;
        case VAL_NULL:
            {
                // true for all nulls
                return true;
            }
            break;
        case VAL_OBJ:
            {
                // already interned, occupies the same address
                return fei_value_asobj(a) == fei_value_asobj(b);
            }
            break;
        default:
            {
            }
            break;
    }
    return false;
}

Object* fei_object_allocobject(State* state, size_t size, ObjType type)
{
    Object* object;
    object = (Object*)fei_gcmem_reallocate(state, NULL, 0, size);
    object->type = type;
    object->ismarked = false;
    // every time an object is allocated, insert to the list
    // insert as the HEAD; the latest one inserted will be at the start
    object->next = state->gcstate.objects;// vm from virtualm.h, with extern
    state->gcstate.objects = object;
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("%p allocate %zd for %d\n", (void*)object, size, type);
    #endif

    return object;
}

// new bound method for classes
ObjBoundMethod* fei_object_makeboundmethod(State* state, Value receiver, ObjClosure* method)
{
    ObjBoundMethod* bound;
    bound = (ObjBoundMethod*)fei_object_allocobject(state, sizeof(ObjBoundMethod), OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

// create new closure
ObjClosure* fei_object_makeclosure(State* state, ObjFunction* function)
{
    int i;
    ObjClosure* closure;
    ObjUpvalue** upvalues;
    // initialize array of upvalue pointers
    // upvalues carry over
    upvalues = (ObjUpvalue**)ALLOCATE(state, sizeof(ObjUpvalue*), function->upvaluecount);
    for(i = 0; i < function->upvaluecount; i++)
    {
        upvalues[i] = NULL;
    }
    closure = (ObjClosure*)fei_object_allocobject(state, sizeof(ObjClosure), OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvaluecount = function->upvaluecount;
    return closure;
}

ObjString* fei_object_allocstring(State* state, char* chars, int length, uint32_t hash)// pass in hash
{
    ObjString* string;
    string = (ObjString*)fei_object_allocobject(state, sizeof(ObjString), OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    // garbage collection
    fei_vm_stackpush(state, OBJ_VAL(string));
    //printf("allocate\n");
    // for string interning
    fei_table_set(state, &state->vmstate.strings, string, NULL_VAL);
    // garbage collection
    fei_vm_stackpop(state);
    return string;
}

ObjClass* fei_object_makeclass(State* state, ObjString* name)
{
    ObjClass* klassobj = (ObjClass*)fei_object_allocobject(state, sizeof(ObjClass), OBJ_CLASS);
    klassobj->name = name;
    fei_table_initempty(state, &klassobj->methods);
    //fei_table_initcapacity(state, &klassobj->methods, 4);
    return klassobj;
}

// create new class instance
ObjInstance* fei_object_makeinstance(State* state, ObjClass* klassobj)
{
    ObjInstance* instance;
    instance = (ObjInstance*)fei_object_allocobject(state, sizeof(ObjInstance), OBJ_INSTANCE);
    instance->classobject = klassobj;
    fei_table_initempty(state, &instance->fields);
    //fei_table_initcapacity(state, &instance->fields, 4);
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
    if(interned != NULL)
    {
        FREE_ARRAY(state, sizeof(char), chars, length + 1);
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

void fei_value_printfunc(State* state, FILE* fh, ObjFunction* function)
{
    (void)state;
    if(function->name == NULL)
    {
        fprintf(fh, "<script>");
        return;
    }
    fprintf(fh, "fun %s(%d params)", function->name->chars, function->arity);// print name and number of parameters
}

// actual printing on the virtual machine is done here
void fei_value_printvalue(State* state, FILE* fh, Value value, bool withquot)
{
    switch(value.type)
    {
        case VAL_BOOL:
            {
                fprintf(fh, fei_value_asbool(value) ? "true" : "false");
            }
            break;
        case VAL_NULL:
            {
                fprintf(fh, "null");
            }
            break;
        case VAL_NUMBER:
            {
                fprintf(fh, "%g", fei_value_asnumber(value));
            }
            break;
        case VAL_OBJ:
            {
                fei_value_printobject(state, fh, value, withquot);
            }
            break;
    }
}

void fei_value_printobject(State* state, FILE* fh, Value value, bool withquot)
{
    (void)state;
    // first class objects can be printed; string and functions
    switch(OBJ_TYPE(value))
    {
        case OBJ_BOUND_METHOD:
            fei_value_printfunc(state, fh, fei_value_asbound_method(value)->method->function);
            break;
        case OBJ_CLASS:
            fprintf(fh, "%s", fei_value_asclass(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            fprintf(fh, "%s instance", fei_value_asinstance(value)->classobject->name->chars);
            break;
        case OBJ_CLOSURE:
            fei_value_printfunc(state, fh, fei_value_asclosure(value)->function);
            break;
        case OBJ_FUNCTION:
            fei_value_printfunc(state, fh, fei_value_asfunction(value));
            break;
        case OBJ_NATIVE:
            fprintf(fh, "<native fun>");
            break;
        case OBJ_STRING:
            fprintf(fh, "%s", fei_value_ascstring(value));
            break;
        case OBJ_UPVALUE:
            fprintf(fh, "upvalue");
            break;
        default:
            return;
    }
}

void fei_table_initcapacity(State* state, Table* table, int cap)
{
    (void)state;
    fei_table_initnull(state, table);
    if(cap > 0)
    {
        fei_table_adjustcapacity(state, table, cap);
    }
}

void fei_table_initempty(State* state, Table* table)
{
    fei_table_initcapacity(state, table, 0);
}

void fei_table_initnull(State* state, Table* table)
{
    (void)state;
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void fei_table_destroy(State* state, Table* table)
{
    FREE_ARRAY(state, sizeof(TabEntry), table->entries, table->capacity);
    fei_table_initnull(state, table);
}

TabEntry* fei_table_findentry(State* state, int count, TabEntry* entries, int capacity, ObjString* key)
{
    uint32_t index;
    TabEntry* entry;
    TabEntry* tombstone;
    (void)state;
    (void)count;
    // use modulo to map the key's hash to the code index
    index = key->hash % capacity;
    tombstone = NULL;
    while(true)
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
    return NULL;
}

bool fei_table_get(State* state, Table* table, ObjString* key, Value* value)
{
    TabEntry* entry;
    if(table->count == 0)
    {
        return false;
    }
    entry = fei_table_findentry(state, table->count, table->entries, table->capacity, key);
    if(entry == NULL)
    {
        return false;
    }
    if(entry->key == NULL)
    {
        return false;
    }
    *value = entry->value;
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
    //fprintf(stderr, "fei_table_adjustcapacity(%d): table->count=%d table->entries=%p\n", capacity, table->count, table->entries);
    if((table->count > 0) && (table->entries != NULL))
    {
        table->count = 0;
        // travers through old array
        for(i = 0; i < table->capacity; i++)
        {
            entry = &table->entries[i];
            if(entry->key == NULL)
            {
                continue;
            }
            // pass in new array
            dest = fei_table_findentry(state, table->count, entries, capacity, entry->key);
            dest->key = entry->key;
            dest->value = entry->value;
            table->count++;
        }
        FREE_ARRAY(state, sizeof(TabEntry), table->entries, table->capacity);
    }
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
    if((table->count + 1) > (table->capacity * TABLE_MAX_LOAD))
    {
        capacity = (GROW_CAPACITY(table->capacity));
        fei_table_adjustcapacity(state, table, capacity);
    }
    entry = fei_table_findentry(state, table->count, table->entries, table->capacity, key);
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
    entry = fei_table_findentry(state, table->count, table->entries, table->capacity, key);
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
    fei_vm_stackpush(state, value);
    fei_valarray_push(state, &chunk->constants, value);
    // garbage collection
    fei_vm_stackpop(state);
    // return index of the newly added constant
    return fei_valarray_count(&chunk->constants) - 1;
}

int fei_dbgutil_printsimpleir(State* state, const char* name, int offset)
{
    (void)state;
    // print as a string, or char*
    fprintf(stderr, "%s\n", name);
    return offset + 1;
}

int fei_dbgutil_printbyteir(State* state, const char* name, Chunk* chunk, int offset)
{
    uint8_t slot;
    (void)state;
    slot = chunk->code[offset + 1];
    fprintf(stderr, "%-16s %4d\n", name, slot);
    return offset + 2;
}

int fei_dbgutil_printconstir(State* state, const char* name, Chunk* chunk, int offset)
{
    uint8_t constant;
    // pullout the constant index from the subsequent byte in the chunk
    constant = chunk->code[offset + 1];
    // print out name of the opcode, then the constant index
    fprintf(stderr, "%-16s %4d '", name, constant);
    //	display the value of the constant,  user defined function
    fei_value_printvalue(state, stderr, fei_valarray_get(state, &chunk->constants, constant), true);
    fprintf(stderr, "'\n");
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
    fprintf(stderr, "%-16s (%d args) %4d", name, argcount, constant);
    // print the method
    fei_value_printvalue(state, stderr, fei_valarray_get(state, &chunk->constants, constant), true);
    fprintf(stderr, "\n");
    return offset + 3;
}

int fei_dbgutil_printjumpir(State* state, const char* name, int sign, Chunk* chunk, int offset)
{
    uint16_t jump;
    (void)state;
    jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    fprintf(stderr, "%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

void fei_dbgdisas_chunk(State* state, Chunk* chunk, const char* name)
{
    int offset;
    // print a little header for debugging
    fprintf(stderr, "== %s ==\n", name);
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
    fprintf(stderr, "%04d ", offset);
    // show source line each instruction was compiled from
    if(offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1])// show a | for any instruction that comes from the
    //same source as its preceding one
    {
        fprintf(stderr, "    | ");
    }
    else
    {
        fprintf(stderr, "%4d ", chunk->lines[offset]);
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
                fprintf(stderr, "%-16s %4d ", "OP_CLOSURE", constant);
                fei_value_printvalue(state, stderr, fei_valarray_get(state, &chunk->constants, constant), true);// accessing the value using the index
                fprintf(stderr, "\n");
                function = fei_value_asfunction(fei_valarray_get(state, &chunk->constants, constant));
                for(j = 0; j < function->upvaluecount; j++)// walk through upvalues
                {
                    islocal = chunk->code[offset++];
                    index = chunk->code[offset++];
                    fprintf(stderr, "%04d	|	%s %d\n", offset - 2, islocal ? "local" : "upvalue", index);
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
            {
                return fei_dbgutil_printinvokeir(state, "OP_SUPER_INVOKE", chunk, offset);
            }
            break;
        case OP_RETURN:
            {
                return fei_dbgutil_printsimpleir(state, "OP_RETURN", offset);// dispatch to a utility function to display it
            }
            break;
        case OP_LOOP:
            {
                return fei_dbgutil_printjumpir(state, "OP_LOOP", -1, chunk, offset);
            }
            break;
        case OP_LOOP_IF_TRUE:
            {
                return fei_dbgutil_printjumpir(state, "OP_LOOP_IF_TRUE", -1, chunk, offset);
            }
            break;
        case OP_LOOP_IF_FALSE:
            {
                return fei_dbgutil_printjumpir(state, "OP_LOOP_IF_FALSE", -1, chunk, offset);
            }
            break;
        default:
            {
                fprintf(stderr, "Unknown opcode %d\n", instruction);
                return offset + 1;
            }
            break;
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
    // advance to next
    state->aststate.scanner.currentsrc++;
    // return previous one
    return state->aststate.scanner.currentsrc[-1];
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
    while(true)
    {
        c = fei_lexer_peekcurrent(state);
        switch(c)
        {
            case ' ':
            case '\r':
            case '\t':
                {
                    fei_lexer_advance(state);
                }
                break;
            // if a new line is found, also add line number
            case '\n':
                {
                    state->aststate.scanner.line++;
                    fei_lexer_advance(state);
                }
                break;
            // for comments
            case '/':
                {
                    if(fei_lexer_peeknext(state) == '/')
                    {
                        // comment goes until end of line
                        while(fei_lexer_peekcurrent(state) != '\n' && !fei_lexer_isatend(state))
                        {
                            // if not new line or not end, treat as whitespace and advance
                            fei_lexer_advance(state);
                        }
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
TokType fei_lexer_checkkw(State* state, int start, int length, const char* rest, TokType type)
{
    Scanner* scn;
    scn = &state->aststate.scanner;
    /*
    * hard expression here
    * bascially if they are exactly the same, and compares their memory(memcmp)
    * int memcmp(const void *str1, const void *str2, size_t n) -> if it is exactly the same, then it is 0
    */
    if(scn->currentsrc - scn->startsrc == start + length && memcmp(scn->startsrc + start, rest, length) == 0)
    {
        return type;
    }
    return TOKEN_IDENTIFIER;
}

// the 'trie' to store the set of strings
TokType fei_lexer_scantype(State* state)
{
    Scanner* scn;
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
    scn = &state->aststate.scanner;
    // start of the lexeme
    switch(scn->startsrc[0])
    {
        //case 'a': return fei_lexer_checkkw(state, 1, 2, "nd", TOKEN_KWAND);
        case 'a':
            {
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    switch(scn->startsrc[1])
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
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    switch(scn->startsrc[1])
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
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    switch(scn->startsrc[1])
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
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    // check if there is a second letter
                    switch(scn->startsrc[1])
                    {
                        case 'l':
                            {
                                // check if there is a third letter
                                if(scn->currentsrc - scn->startsrc > 2)
                                {
                                    switch(scn->startsrc[2])
                                    {
                                        case 's':
                                            {
                                                return fei_lexer_checkkw(state, 3, 1, "e", TOKEN_KWELSE);
                                            }
                                            break;
                                        case 'f':
                                            {
                                                // already matched
                                                return fei_lexer_checkkw(state, 3, 0, "", TOKEN_KWELF);
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
                // check if there is a second letter
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    switch(scn->startsrc[1])
                    {
                        case 'a':
                            {
                                // starts from 2 not 3, as first letter is already an f
                                return fei_lexer_checkkw(state, 2, 3, "lse", TOKEN_KWFALSE);
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
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    switch(scn->startsrc[1])
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
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    switch(scn->startsrc[1])
                    {
                        case 'e':
                            {
                                if(scn->currentsrc - scn->startsrc > 2)
                                {
                                    switch(scn->startsrc[2])
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
                // if there is a second letter
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    switch(scn->startsrc[1])
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
                if(scn->currentsrc - scn->startsrc > 1)
                {
                    switch(scn->startsrc[1])
                    {
                        //case 'h': return fei_lexer_checkkw(state, 2, 2, "is", TOKEN_KWTHIS);
                        case 'h':
                            {
                                // check if there is a third letter
                                if(scn->currentsrc - scn->startsrc > 2)
                                {
                                    switch(scn->startsrc[2])
                                    {
                                        case 'i':
                                            {
                                                // already matched
                                                return fei_lexer_checkkw(state, 3, 1, "s", TOKEN_KWTHIS);
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
        {
            // allow strings to go until next line
            state->aststate.scanner.line++;
        }
        // consume characters until the closing quote is reached
        fei_lexer_advance(state);
    }
    if(fei_lexer_isatend(state))
    {
        return fei_lexer_errortoken(state, "Unterminated string.");
    }
    // closing quote
    fei_lexer_advance(state);
    return fei_lexer_maketoken(state, TOKEN_STRING);
    // convert lexeme to runtime value later
}

// reading the char, and return a token
Token fei_lexer_scantoken(State* state)
{
    char c;
    fei_lexer_skipspace(state);
    // reset the scanner to current
    state->aststate.scanner.startsrc = state->aststate.scanner.currentsrc;
    if(fei_lexer_isatend(state))
    {
        // check if at end
        return fei_lexer_maketoken(state, TOKEN_EOF);
    }
    // if not end of file
    c = fei_lexer_advance(state);
    if(fei_lexutil_isalpha(state, c))
    {
        return fei_lexer_scanident(state);
    }
    // fei_lexer_scannumber() is a TOKEN_NUMBER
    if(fei_lexutil_isdigit(state, c))
    {
        return fei_lexer_scannumber(state);
    }
    // lexical grammar for the language
    switch(c)
    {
        // for single characters
        case '(':
            {
                return fei_lexer_maketoken(state, TOKEN_LEFTPAREN);
            }
            break;
        case ')':
            {
                return fei_lexer_maketoken(state, TOKEN_RIGHTPAREN);
            }
            break;
        case '{':
            {
                return fei_lexer_maketoken(state, TOKEN_LEFTBRACE);
            }
            break;
        case '}':
            {
                return fei_lexer_maketoken(state, TOKEN_RIGHTBRACE);
            }
            break;
        case ';':
            {
                return fei_lexer_maketoken(state, TOKEN_SEMICOLON);
            }
            break;
        case ':':
            {
                return fei_lexer_maketoken(state, TOKEN_COLON);
            }
            break;
        case ',':
            {
                return fei_lexer_maketoken(state, TOKEN_COMMA);
            }
            break;
        case '.':
            {
                return fei_lexer_maketoken(state, TOKEN_DOT);
            }
            break;
        case '-':
            {
                return fei_lexer_maketoken(state, TOKEN_MINUS);
            }
            break;
        case '+':
            {
                return fei_lexer_maketoken(state, TOKEN_PLUS);
            }
            break;
        case '*':
            {
                return fei_lexer_maketoken(state, TOKEN_STAR);
            }
            break;
        case '/':
            {
                return fei_lexer_maketoken(state, TOKEN_SLASH);
            }
            break;
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

Chunk* fei_compiler_currentchunk(State* state)
{
    return &state->aststate.compiler->programfunc->chunk;
}

// to handle syntax errors
void fei_compiler_raiseat(State* state, Token* token, const char* message)
{
    // if an error already exists, no need to run other errors
    if(state->aststate.parser.panicmode)
    {
        return;
    }
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
void fei_compiler_raisehere(State* state, const char* message)
{
    // pass in the current parser
    fei_compiler_raiseat(state, &state->aststate.parser.currtoken, message);
}


/* main compile functions */

// pump the compiler, basically go to / 'read' the next token, a SINGLE token
void fei_compiler_advancenext(State* state)
{
    // store next parser as current
    state->aststate.parser.prevtoken = state->aststate.parser.currtoken;
    while(true)
    {
        // gets next token, stores it for later use(the next scan)
        state->aststate.parser.currtoken = fei_lexer_scantoken(state);
        // if error is not found break
        if(state->aststate.parser.currtoken.type != TOKEN_ERROR)
        {
            break;
        }
        // start is the location/pointer of the token source code
        fei_compiler_raisehere(state, state->aststate.parser.currtoken.toksrc);
    }
}

// advance while skipping the given parameter, give none to skip nothing
void fei_compiler_advanceskipping(State* state, TokType type)
{
    // store next parser as current
    state->aststate.parser.prevtoken = state->aststate.parser.currtoken;
    while(true)
    {
        // gets next token, stores it for later use(the next scan)
        state->aststate.parser.currtoken = fei_lexer_scantoken(state);
        if(state->aststate.parser.currtoken.type == type)
        {
            continue;
        }
        // if error is not found break
        if(state->aststate.parser.currtoken.type != TOKEN_ERROR)
        {
            break;
        }
        // start is the location/pointer of the token source code
        fei_compiler_raisehere(state, state->aststate.parser.currtoken.toksrc);
    }
}


// SIMILAR to advance but there is a validation for a certain type
// syntax error comes from here, where it is known/expected what the next token will be
void fei_compiler_consume(State* state, TokType type, const char* message)
{
    // if current token is equal to the token type being compared to
    if(state->aststate.parser.currtoken.type == type)
    {
        fei_compiler_advancenext(state);
        return;
    }
    // if consumes a different type, error
    fei_compiler_raisehere(state, message);
}

bool fei_compiler_check(State* state, TokType type)
{
    // check if current matches given
    return state->aststate.parser.currtoken.type == type;
}

bool fei_compiler_match(State* state, TokType type)
{
    if(!fei_compiler_check(state, type))
    {
        return false;
    }
    fei_compiler_advancenext(state);
    return true;
}

/* emitting BYTECODE for the VM to understand */
// the fei_chunk_pushbyte for the compiler
void fei_compiler_emitbyte(State* state, uint8_t byte)
{
    // sends previous line so runtime errors are associated with that line
    fei_chunk_pushbyte(state, fei_compiler_currentchunk(state), byte, state->aststate.parser.prevtoken.line);
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
    int offset;
    fei_compiler_emitbyte(state, OP_LOOP);
    // int below jumps back, + 2 accounting the OP_LOOP and the instruction's own operand
    offset = fei_compiler_currentchunk(state)->count - loopstart + 2;
    if(offset > UINT16_MAX)
    {
        fei_compiler_raiseerror(state, "loop body contains too many instructions");
    }
    fei_compiler_emitbyte(state, (offset >> 8) & 0xff);
    fei_compiler_emitbyte(state, offset & 0xff);
}

void fei_compiler_emitcondloop(State* state, int loopstart, bool condstate)
{
    int offset;
    if(condstate)
    {
        fei_compiler_emitbyte(state, OP_LOOP_IF_TRUE);
    }
    else
    {
        fei_compiler_emitbyte(state, OP_LOOP_IF_FALSE);
    }
    offset = fei_compiler_currentchunk(state)->count - loopstart + 2;
    if(offset > UINT16_MAX)
    {
        fei_compiler_raiseerror(state, "loop body contains too many instructions");
    }
    fei_compiler_emitbyte(state, (offset >> 8) & 0xff);
    fei_compiler_emitbyte(state, offset & 0xff);
}

int fei_compiler_emitjump(State* state, uint8_t instruction)
{
    /* backpatching */
    // writes a placeholder operand for jump offset
    fei_compiler_emitbyte(state, instruction);
    // hexadecimal number with value of 255
    fei_compiler_emitbyte(state, 0xff);
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
        // for functions that return nothing
        fei_compiler_emitbyte(state, OP_NULL);
    }
    // emit return type at the end of a compiler
    fei_compiler_emitbyte(state, OP_RETURN);
}

// to insert into constant table
uint8_t fei_compiler_makeconst(State* state, Value value)
{
    int constant = fei_chunk_pushconst(state, fei_compiler_currentchunk(state), value);
    if(constant > UINT8_MAX)
    {
        fei_compiler_raiseerror(state, "chunk contains too many constants");
        return 0;
    }
    // return as byte, the byte being the INDEX of the constantin the constats array
    return (uint8_t)constant;
}

// for constant emit the opcode, then the index
void fei_compiler_emitconst(State* state, Value value)
{
    // add value to constant table
    fei_compiler_emitbytes(state, OP_CONSTANT, fei_compiler_makeconst(state, value));
}

void fei_compiler_patchjump(State* state, int offset)
{
    // - 2 to adjust for the jump offset itself
    int jump;
    jump = fei_compiler_currentchunk(state)->count - offset - 2;
    if(jump > UINT16_MAX)
    {
        fei_compiler_raiseerror(state, "too much code to jump over");
    }
    // the fei_compiler_patchjump provides the VALUE or amount to JUMP
    // right shift by 8, then bitwise AND with 255(oxff is 111111)
    fei_compiler_currentchunk(state)->code[offset] = (jump >> 8) & 0xff;
    // only AND
    fei_compiler_currentchunk(state)->code[offset + 1] = jump & 0xff;
}

// initialize the compiler
void fei_compiler_init(State* state, Compiler* compiler, FuncType type)
{
    size_t i;
    Local stacklocal;
    Local* local;
    Compiler* astcc;
    // the 'outer' compiler
    compiler->enclosing = state->aststate.compiler;
    compiler->programfunc = NULL;
    compiler->progfunctype = type;
    compiler->progloccount = 0;
    compiler->scopedepth = 0;
    compiler->programfunc = fei_object_makefunction(state);
    state->aststate.compiler = compiler;
    astcc = state->aststate.compiler;
    if(type != TYPE_SCRIPT)
    {
        // function name handled here
        astcc->programfunc->name = fei_object_copystring(state, state->aststate.parser.prevtoken.toksrc, state->aststate.parser.prevtoken.length);
    }
    //astcc->proglocals = da_make(state, astcc->proglocals, 0, sizeof(Local));
    for(i=0; i<CFG_MAX_COMPILERLOCALS; i++)
    {
        memset(&astcc->proglocals[i], 0, sizeof(Local));
    }
    
    // compiler implicitly claims slot zero for local variables
    local = &astcc->proglocals[astcc->progloccount++];
    //local = &stacklocal;
    //da_push(state, astcc->proglocals, &stacklocal);
    local->iscaptured = false;
    // for this tags
    // for none function types, for class methods
    if(type != TYPE_FUNCTION)
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
    ObjFunction* function;
    Compiler* astcc;
    astcc = state->aststate.compiler;
    fei_compiler_emitreturn(state);
    function = astcc->programfunc;
    FREE(state, sizeof(int), astcc->continuejumps);
    // for debugging
#if defined(DEBUG_PRINT_CODE) && (DEBUG_PRINT_CODE == 1)
    if(!state->aststate.parser.haderror)
    {
        // if name is NULL then it is the Script type(main()
        fei_dbgdisas_chunk(state, fei_compiler_currentchunk(state), function->name != NULL ? function->name->chars : "<script>");
    }
#endif
    // return back to enclosing compiler after function
    state->aststate.compiler = astcc->enclosing;
    // return to free
    return function;
}

void fei_compiler_beginscope(State* state)
{
    state->aststate.compiler->scopedepth++;
}

void fei_compiler_endscope(State* state)
{
    Compiler* cc;
    cc = state->aststate.compiler;
    cc->scopedepth--;
    // remove variables out of scope
    while(cc->progloccount > 0 && cc->proglocals[cc->progloccount - 1].depth > cc->scopedepth)
    {
        /*
        * at the end of a block scope, when the compiler emits code to free the stack slot for the locals, 
        * tell which one to hoist to the heap
        */
        // if it is captured/used
        if(cc->proglocals[cc->progloccount - 1].iscaptured)
        {
            // op code to move the upvalue to the heap
            fei_compiler_emitbyte(state, OP_CLOSE_UPVALUE);
        }
        else
        {
            // if not used anymore/capture simply pop the value off the stack
            fei_compiler_emitbyte(state, OP_POP);
        }
        cc->progloccount--;
    }
}

// loop enclosing
void fei_compiler_beginloopscope(State* state)
{
    state->aststate.compiler->loopcounttop++;
}

void fei_compiler_endloopscope(State* state)
{
    Compiler* astcc;
    astcc = state->aststate.compiler;
    if(astcc->breakjumpcounts[astcc->loopcounttop] > 0)
    {
        astcc->breakjumpcounts[astcc->loopcounttop] = 0;
    }
    astcc->loopcounttop--;
}

// mark current chunk for continue jump
void fei_compiler_markcontinuejump(State* state)
{
    Compiler* astcc;
    astcc = state->aststate.compiler;
    astcc->continuejumps[astcc->loopcounttop] = fei_compiler_currentchunk(state)->count;
}

// patch available break jumps
void fei_compiler_patchbreakjumps(State* state)
{
    int i;
    Compiler* astcc;
    astcc = state->aststate.compiler;
    for(i = 0; i < astcc->breakjumpcounts[astcc->loopcounttop]; i++)
    {
        fei_compiler_patchjump(state, astcc->breakpatchjumps[astcc->loopcounttop][i]);
    }
}

/* variable declarations */
uint8_t fei_compiler_makeidentconst(State* state, Token* name)
{
    // add to constant table
    return fei_compiler_makeconst(state, OBJ_VAL(fei_object_copystring(state, name->toksrc, name->length)));
}

bool fei_compiler_identsequal(State* state, Token* a, Token* b)
{
    (void)state;
    if(a->length != b->length)
    {
        return false;
    }
    return memcmp(a->toksrc, b->toksrc, a->length) == 0;
}


int fei_compiler_resolvelocal(State* state, Compiler* compiler, Token* name)
{
    int i;
    Local* local;
    Local stacklocal;
    // walk through the local variables
    for(i = compiler->progloccount - 1; i >= 0; i--)
    {
        /*
        if(i >= da_count(compiler->proglocals))
        {
            local = &stacklocal;
            memset(local, 0, sizeof(Local));
            da_push(state, compiler->proglocals, &stacklocal);
        }
        else
        */
        {
            local = &compiler->proglocals[i];
        }
        if(fei_compiler_identsequal(state, name, &local->name))
        {
            if(local->depth == -1)
            {
                fei_compiler_raiseerror(state, "cannot read local variable in its own initializer");
            }
            // found the var, return the index
            return i;
        }
    }
    // not found, name is global variable
    return -1;
}

// add upvalue
int fei_compiler_addupvalue(State* state, Compiler* compiler, uint8_t index, bool islocal)
{
    int i;
    int upvaluecount;
    Upvalue* upvalue;
    // get current upvalue count
    upvaluecount = compiler->programfunc->upvaluecount;
    // check whether the upvalue has already been declared
    for(i = 0; i < upvaluecount; i++)
    {
        // get pointer for each upvalue in the array
        upvalue = &compiler->upvalues[i];
        if(upvalue->index == index && upvalue->islocalvar == islocal)
        {
            // if found, return the index of the upvalue in the upvalue array
            return i;
        }
    }
    if(upvaluecount == CFG_MAX_COMPILERUPVALS)
    {
        fei_compiler_raiseerror(state, "too many closure variables");
        return 0;
    }
    // compiler keeps an array of upvalue structs to track closed-over identifiers
    // indexes in the array match the indexes of ObjClosure at runtime
    // insert to upvalues array
    // insert bool status
    compiler->upvalues[upvaluecount].islocalvar = islocal;
    // insert index
    compiler->upvalues[upvaluecount].index = index;
    // increase count and return
    return compiler->programfunc->upvaluecount++;
}

/*
* for closures
- fei_compiler_resolveupvalue looks for a local variable declared in any of the surrounding functions
- if it finds one it returns the index for that upvalue variable, otherwise returns -1
*/
int fei_compiler_resolveupvalue(State* state, Compiler* compiler, Token* name)
{
    int local;
    int upvalue;
    if(compiler->enclosing == NULL)
    {
        // if in main()
        return -1;
    }
    // looks for local value in enclosing function/compiler
    local = fei_compiler_resolvelocal(state, compiler->enclosing, name);
    if(local != -1)
    {
        // mark local is captured/used by and upvalue
        compiler->enclosing->proglocals[local].iscaptured = true;
        // create up value
        return fei_compiler_addupvalue(state, compiler, (uint8_t)local, true);
    }
    // recursion to solve nested upvalues
    // recursive call right in the middle
    // if the enclosing function is main() (NULL), it returns -1
    upvalue = fei_compiler_resolveupvalue(state, compiler->enclosing, name);
    if(upvalue != -1)
    {
        return fei_compiler_addupvalue(state, compiler, (uint8_t)upvalue, true);
    }
    return -1;
}

void fei_compiler_addlocal(State* state, Token name)
{
    Local* local;
    Compiler* astcc;
    astcc = state->aststate.compiler;
    if(astcc->progloccount == CFG_MAX_COMPILERLOCALS)
    {
        fei_compiler_raiseerror(state, "too many local variables in block");
        return;
    }
    local = &astcc->proglocals[astcc->progloccount++];
    local->name = name;
    // for cases where a variable name is redefined inside another scope, using the variable itself
    local->depth = -1;
    local->iscaptured = false;
}

void fei_compiler_declvarfromcurrent(State* state)// for local variables
{
    int i;
    Local* local;
    Token* name;
    Compiler* astcc;
    astcc = state->aststate.compiler;
    // global vars are implicitly declared, and are late bound, not 'initialized' here but in the VM
    if(astcc->scopedepth == 0)
    {
        return;
    }
    /* local variable declaration happens below */
    name = &state->aststate.parser.prevtoken;
    // to not allow two variable declarations to have the same name
    // loop only checks to a HIGHER SCOPE; another block overlaping/shadowing is allowed
    // work backwards
    for(i = astcc->progloccount - 1; i >= 0; i--)
    {
        local = &astcc->proglocals[i];
        // if reach beginning of array(highest scope)
        if(local->depth != -1 && local->depth < astcc->scopedepth)
        {
            break;
        }
        if(fei_compiler_identsequal(state, name, &local->name))
        {
            fei_compiler_raiseerror(state, "variable with this name exists in scope");
        }
    }
    fei_compiler_addlocal(state, *name);
}

uint8_t fei_compiler_parsevarfromcurrent(State* state, const char* errormessage)
{
    // requires next token to be an identifier
    fei_compiler_consume(state, TOKEN_IDENTIFIER, errormessage);
    fei_compiler_declvarfromcurrent(state);
    if(state->aststate.compiler->scopedepth > 0)
    {
        // if scopedepth is not 0, then it is a local not global var
        return 0;
    }
    // return a dummy index
    // at runtime, locals are not looked up by name so no need to insert them to a table
    // return index from the constant table
    return fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);
}

void fei_compiler_markinit(State* state)
{
    Compiler* astcc;
    astcc = state->aststate.compiler;
    if(astcc->scopedepth == 0)
    {
        // if global return
        return;
    }
    astcc->proglocals[astcc->progloccount - 1].depth = astcc->scopedepth;
}

void fei_compiler_defvarindex(State* state, uint8_t global)
{
    if(state->aststate.compiler->scopedepth > 0)
    {
        fei_compiler_markinit(state);
        return;
    }
    // opcode for declaration and the constant itself
    fei_compiler_emitbytes(state, OP_DEFINE_GLOBAL, global);
}

// for function arguments, returns number of arguments
// each argument expression generates code which leaves value on the stack in preparation for the call
uint8_t fei_compiler_parsearglist(State* state)
{
    uint8_t argcount;
    argcount = 0;
    // if ) has not been reached
    if(!fei_compiler_check(state, TOKEN_RIGHTPAREN))
    {
        do
        {
            // collect the arguments
            fei_compiler_parseexpr(state);
            // cannot have more than 255 arguments as each operand is a single byte(uint8_t)
            if(argcount == 255)
            {
                fei_compiler_raiseerror(state, "cannot have more than 255 arguments");
            }
            argcount++;
        } while(fei_compiler_match(state, TOKEN_COMMA));
    }
    fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after argument list.");
    return argcount;
}

static void fei_comprule_logicaland(State* state, bool canassign)
{
    int endjump;
    (void)canassign;
    // left hand side is already compiled,
    // and if it is false skip it and go to next
    endjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);
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
    ParseRule* rule;
    TokType operatortype;
    (void)canassign;
    // remember type of operator, already consumed
    operatortype = state->aststate.parser.prevtoken.type;
    // compile right operand
    // the BIDMAS rule, operands in the right side have HIGHER PRECEDENCE
    rule = fei_compiler_getrule(state, operatortype);
    // as binary operators are LEFT ASSOCIATIVE
    // recursively call fei_compiler_parseprec again
    // conert from rule to enum(precedence) type
    fei_compiler_parseprec(state, (Precedence)(rule->precedence + 1));
    switch(operatortype)
    {
        // note how NOT opcode is at the end
        // six binary operators for three instructions only(greater, not, equal)
        case TOKEN_NOTEQUAL:
            {
                // add equal and not to the stack
                fei_compiler_emitbytes(state, OP_EQUAL, OP_NOT);
            }
            break;
        case TOKEN_EQUAL:
            {
                fei_compiler_emitbyte(state, OP_EQUAL);
            }
            break;
        case TOKEN_GREATERTHAN:
            {
                fei_compiler_emitbyte(state, OP_GREATER);
            }
            break;
        case TOKEN_GREATEREQUAL:
            {
                fei_compiler_emitbytes(state, OP_LESS, OP_NOT);
            }
            break;
        case TOKEN_LESSTHAN:
            {
                fei_compiler_emitbyte(state, OP_LESS);
            }
            break;
        case TOKEN_LESSEQUAL:
            {
                fei_compiler_emitbytes(state, OP_GREATER, OP_NOT);
            }
            break;
        case TOKEN_PLUS:
            {
                fei_compiler_emitbyte(state, OP_ADD);
            }
            break;
        case TOKEN_MINUS:
            {
                fei_compiler_emitbyte(state, OP_SUBTRACT);
            }
            break;
        case TOKEN_STAR:
            {
                fei_compiler_emitbyte(state, OP_MULTIPLY);
            }
            break;
        case TOKEN_SLASH:
            {
                fei_compiler_emitbyte(state, OP_DIVIDE);
            }
            break;
        case TOKEN_MODULO:
            {
                fei_compiler_emitbyte(state, OP_MODULO);
            }
            break;
            // unreachable
        default:
            {
                return;
            }
            break;
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
    uint8_t name;
    uint8_t argcount;
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect propery name after class instance.");
    // already consumed
    name = fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);
    // assignment
    if(canassign && fei_compiler_match(state, TOKEN_ASSIGN))
    {
        // evalute expression to be set
        fei_compiler_parseexpr(state);
        fei_compiler_emitbytes(state, OP_SET_PROPERTY, name);
    }
    // for running class methods, access the method and call it at the same time
    else if(fei_compiler_match(state, TOKEN_LEFTPAREN))
    {
        argcount = fei_compiler_parsearglist(state);
        /*
        * new OP_INVOKE opcode that takes two operands:
        * 1. the index of the property name in the constant table
        * 2. the number of arguments passed in the methods
        *** combines OP_GET_PROPERTY and OP_CALL
        */
        fei_compiler_emitbytes(state, OP_INVOKE, name);
        fei_compiler_emitbyte(state, argcount);
    }
    // simply get
    else
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
            {
                fei_compiler_emitbyte(state, OP_FALSE);
            }
            break;
        case TOKEN_KWTRUE:
            {
                fei_compiler_emitbyte(state, OP_TRUE);
            }
            break;
        case TOKEN_KWNULL:
            {
                fei_compiler_emitbyte(state, OP_NULL);
            }
            break;
        // unreachable
        default:
            {
                return;
            }
            break;
    }
}

// parentheses for grouping
static void fei_comprule_grouping(State* state, bool canassign)
{
    (void)canassign;
    // assume initial ( has already been consumed,
    // and recursively call to fei_compiler_parseexpr() to compile between the parentheses
    fei_compiler_parseexpr(state);
    // expects a right parentheses, if not received then  error
    fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after expression.");
}

/* parsing the tokens */
static void fei_comprule_number(State* state, bool canassign)
{
    double value;
    (void)canassign;
    // strtod below converts from string to double
    // assume that token for the number literal has already been consumed and is stored in previous
    // double strtod(const char* str, char** endptr)
    // endptr is the first non-double character after teh char* str character; if none then null
    /*	The way it works:
    -> in scanner, if a digit exists after a digit, it advances() (skips) the current
    -> hence, we get that the start points to the START of the digit, and using strtod smartly it reaches until the last digit
    */
    value = strtod(state->aststate.parser.prevtoken.toksrc, NULL);
    //printf("num %c\n", *state->aststate.parser.prevtoken.toksrc);
    fei_compiler_emitconst(state, NUMBER_VAL(value));
}

static void fei_comprule_logicalor(State* state, bool canassign)
{
    int endjump;
    int elsejump;
    (void)canassign;
    // jump if left hand side is true
    // if left is false jump directly to right hand
    elsejump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);
    // if not skipped(as left is true) jump the right hand
    endjump = fei_compiler_emitjump(state, OP_JUMP);
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
    fei_compiler_emitconst(state, OBJ_VAL(fei_object_copystring(state,
        state->aststate.parser.prevtoken.toksrc + 1,
        state->aststate.parser.prevtoken.length - 2)
    ));
}

// declare/call variables
void fei_compiler_declnamedvar(State* state, Token name, bool canassign)
{
    int arg;
    uint8_t getop;
    uint8_t setop;
    (void)canassign;
    // try find a local variable with a given name
    arg = fei_compiler_resolvelocal(state, state->aststate.compiler, &name);
    if(arg != -1)
    {
        getop = OP_GET_LOCAL;
        setop = OP_SET_LOCAL;
    }
    // for upvalues
    else if((arg = fei_compiler_resolveupvalue(state, state->aststate.compiler, &name)) != -1)
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
    // if a = follows right after
    if(canassign && fei_compiler_match(state, TOKEN_ASSIGN))
    {
        fei_compiler_parseexpr(state);
        // reassignment/set
        fei_compiler_emitbytes(state, setop, (uint8_t)arg);
    }
    else
    {
        // as normal get
        fei_compiler_emitbytes(state, getop, (uint8_t)arg);
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
    token.length = (int)strlen(text);
    return token;
}

// for super calls
static void fei_comprule_super(State* state, bool canassign)
{
    uint8_t name;
    uint8_t argcount;
    (void)canassign;
    // if token is not inside a class
    if(state->aststate.classcompiler == NULL)
    {
        fei_compiler_raiseerror(state, "'super' can only be initialized inside a class.");
    }
    // if class has no parent class
    else if(!state->aststate.classcompiler->hassuperclass)
    {
        fei_compiler_raiseerror(state, "'super' cannot be used on a class with no parent class.");
    }
    fei_compiler_consume(state, TOKEN_DOT, "Expect '.' after 'super'.");
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect parent class method identifier.");
    // get identifier index
    name = fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);
    /*
    * in order to access a superclass method on the CURRENT INSTANCE,
    * runtime needs both the receiver and the superclass of the surrounding method's class.
    * 1. first fei_compiler_declnamedvar call generates code to look up the current receiver and push it to the stack
    * 2. second fei_compiler_declnamedvar emits code to look up the superclass and push that on top
    */
    fei_compiler_declnamedvar(state, fei_compiler_makesyntoken(state, "this"), false);
    // if there is a parameter list, invoke super method
    if(fei_compiler_match(state, TOKEN_LEFTPAREN))
    {
        argcount = fei_compiler_parsearglist(state);
        fei_compiler_declnamedvar(state, fei_compiler_makesyntoken(state, "super"), false);
        // super invoke opcode
        fei_compiler_emitbytes(state, OP_SUPER_INVOKE, name);
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
    // always false
    fei_comprule_variable(state, false);
}

static void fei_comprule_unary(State* state, bool canassign)
{
    TokType operatortype;
    (void)canassign;
    // leading - token has already been consumed
    operatortype = state->aststate.parser.prevtoken.type;
    // compile operand
    fei_compiler_parseexpr(state);
    switch(operatortype)
    {
        case TOKEN_LOGICALNOT:
            {
                fei_compiler_emitbyte(state, OP_NOT);
            }
            break;
            // OP_NEGATE should be emitted last, AFTER the constant itself
            // eg. say 4 - 5; 5 needs to be emitted and added to the chunk->constants first before OP_NEGATE
            /*
            * it is important to take note of the precedence
            * e.g -a.b + 3;
            * when the unary negation is called, all of a.b + 3 will be consumed in fei_compiler_parseexpr(). Hence, a method is needed
            * to STOP when + is found, or generally when an operand of LOWER PRECEDENCE is found
            */
        case TOKEN_MINUS:
            {
                fei_compiler_emitbyte(state, OP_NEGATE);
            }
            break;
        default:
            {
                return;
            }
            break;
    }
}


// for inserting where the unary operator should lie
// starts at current token and parses any expression at the given precedence level or higher
// for example, if fei_compiler_parseprec(PREC_COMPARISON) is called, it will parse unaries, terms, and factors
// ubt not or, and or assignment operators as they are lower. Basically parse anything that is ABOVE the given precedence
void fei_compiler_parseprec(State* state, Precedence precedence)
{
    bool canassign;
    ParseFn prefixrule;
    ParseFn infixrule;
    /*
    * PREFIX FIRST
    * look up for a prefix token, and the FIRSt token is ALWAYS going to be a prefix
    */
    fei_compiler_advancenext(state);// again, go next first then use previous type as the 'current' token
    // the way the compiler is designed is that it has to always have a prefix
    prefixrule = fei_compiler_getrule(state, state->aststate.parser.prevtoken.type)->prefix;
    if(prefixrule == NULL)
    {
        fei_compiler_raiseerror(state, "Expect expression.");
        return;
    }
    canassign = precedence <= PREC_ASSIGNMENT;// for assignment precedence
    prefixrule(state, canassign);// call the prefix function, may consume a lot of tokens
    /*
    * after prefix expression is done, look for infix expression
    * IMPORTANT: infix only runs if given precedence is LOWER than the operator for the infix
    * or more nicely if NEXT/INFIX PRECEDENCE IS HIGHER THAN PREC ASSIGNMENT(parameter above_
    */
    while(precedence <= fei_compiler_getrule(state, state->aststate.parser.currtoken.type)->precedence)
    {
        fei_compiler_advancenext(state);
        infixrule = fei_compiler_getrule(state, state->aststate.parser.prevtoken.type)->infix;
        infixrule(state, canassign);
    }
    //consume(TOKEN_KWAND, "consume and failed");
    // if = is not consumed as part of the expression, nothing will , hence an error
    if(canassign && fei_compiler_match(state, TOKEN_ASSIGN))
    {
        fei_compiler_raiseerror(state, "Invalid Assignment target.");
    }
}

// get pointer to ParseRule struct according to type parameter
ParseRule* fei_compiler_getrule(State* state, TokType type)
{
    static ParseRule rule;
    (void)state;
    switch(type)
    {
        case TOKEN_LEFTPAREN:
            rule = (ParseRule){ fei_comprule_grouping, fei_comprule_call, PREC_CALL };
            break;
        case TOKEN_RIGHTPAREN:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_LEFTBRACE:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_RIGHTBRACE:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_COMMA:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_DOT:
            rule = (ParseRule){ NULL, fei_comprule_dot, PREC_CALL };
            break;
        case TOKEN_MINUS:
            rule = (ParseRule){ fei_comprule_unary, fei_comprule_binary, PREC_TERM };
            break;
        case TOKEN_PLUS:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_TERM };
            break;
        case TOKEN_SEMICOLON:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_SLASH:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_FACTOR };
            break;
        case TOKEN_STAR:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_FACTOR };
            break;
        case TOKEN_MODULO:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_FACTOR };
            break;
        case TOKEN_LOGICALNOT:
            rule = (ParseRule){ fei_comprule_unary, NULL, PREC_NONE };
            break;
        case TOKEN_NOTEQUAL:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_EQUALITY };
            break;// equality precedence
        case TOKEN_ASSIGN:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;// comaprison precedence
        case TOKEN_EQUAL:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_GREATERTHAN:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_GREATEREQUAL:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_LESSTHAN:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_LESSEQUAL:
            rule = (ParseRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_IDENTIFIER:
            rule = (ParseRule){ fei_comprule_variable, NULL, PREC_NONE };
            break;
        case TOKEN_STRING:
            rule = (ParseRule){ fei_comprule_string, NULL, PREC_NONE };
            break;
        case TOKEN_NUMBER:
            rule = (ParseRule){ fei_comprule_number, NULL, PREC_NONE };
            break;
        case TOKEN_KWAND:
            rule = (ParseRule){ NULL, fei_comprule_logicaland, PREC_AND };
            break;
        case TOKEN_KWCLASS:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWELSE:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWFALSE:
            rule = (ParseRule){ fei_comprule_literal, NULL, PREC_NONE };
            break;
        case TOKEN_KWFOR:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWFUN:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWIF:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWSWITCH:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWNULL:
            rule = (ParseRule){ fei_comprule_literal, NULL, PREC_NONE };
            break;
        case TOKEN_KWOR:
            rule = (ParseRule){ NULL, fei_comprule_logicalor, PREC_OR };
            break;
        case TOKEN_KWPRINT:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWRETURN:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWSUPER:
            rule = (ParseRule){ fei_comprule_super, NULL, PREC_NONE };
            break;
        case TOKEN_KWTHIS:
            rule = (ParseRule){ fei_comprule_this, NULL, PREC_NONE };
            break;
        case TOKEN_KWTRUE:
            rule = (ParseRule){ fei_comprule_literal, NULL, PREC_NONE };
            break;
        case TOKEN_KWVAR:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWWHILE:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_ERROR:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_EOF:
            rule = (ParseRule){ NULL, NULL, PREC_NONE };
            break;
        default:
            {
                return NULL;
            };
    }
    return &rule;
}

void fei_compiler_parseexpr(State* state)// a single 'statement' or line
{
    fei_compiler_parseprec(state, PREC_ASSIGNMENT);// as assignment is the 2nd lowest, parses evrything
}

void fei_compiler_parseblock(State* state)
{
    // parse until EOF or right brace is 'peeked'
    while(!fei_compiler_check(state, TOKEN_RIGHTBRACE) && !fei_compiler_check(state, TOKEN_EOF))
    {
        // compile rest of block, keeps on parsing until right brace or EOF is 'peeked'
        fei_compiler_parsedeclaration(state);
    }
    fei_compiler_consume(state, TOKEN_RIGHTBRACE, "Expect '}' after block.");
}

/* functions */
void fei_compiler_parsefuncdecl(State* state, FuncType type)
{
    int i;
    uint8_t paramconstant;
    Compiler compiler;
    ObjFunction* function;
    // create separate Compiler for each function
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
            paramconstant = fei_compiler_parsevarfromcurrent(state, "Expect variable name.");// get name
            fei_compiler_defvarindex(state, paramconstant);// scope handled here already
        } while(fei_compiler_match(state, TOKEN_COMMA));
    }
    fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after parameter list.");
    // body
    fei_compiler_consume(state, TOKEN_LEFTBRACE, "Expect '{' before function body.");
    fei_compiler_parseblock(state);
    // create function object
    function = fei_compiler_endcompiler(state);// ends the current compiler
    // compilers are treated like a stack; if current one is ended, like above, return to the previous one
    // fei_compiler_emitbytes(state, OP_CONSTANT, fei_compiler_makeconst(state, OBJ_VAL(function)));
    fei_compiler_emitbytes(state, OP_CLOSURE, fei_compiler_makeconst(state, OBJ_VAL(function)));
    /*
    * by the time the compiler reaches the end of a function declaration,
    * every variable reference hass been resolved as either local, upvalue or global.
    * each upvalue may return a local var or another upvalue
    * -> for each upvalue there are two single-byte operands
    * -> if first byte is one, then it captures a local variable in the enclosing function
    * -> if first byte is 0, it captures the function's upvalues
    */
    for(i = 0; i < function->upvaluecount; i++)
    {
        fei_compiler_emitbyte(state, compiler.upvalues[i].islocalvar ? 1 : 0);
        fei_compiler_emitbyte(state, compiler.upvalues[i].index);// emit index
    }
}

// create method for class type
void fei_compiler_parsemethoddecl(State* state)
{
    uint8_t constant;
    FuncType type;
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect method name.");
    // get method name
    constant = fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);
    // method body
    type = TYPE_METHOD;
    // if initializer
    if(state->aststate.parser.prevtoken.length == 4 && memcmp(state->aststate.parser.prevtoken.toksrc, "init", 4) == 0)
    {
        type = TYPE_INITIALIZER;
    }
    // process the function
    fei_compiler_parsefuncdecl(state, type);
    fei_compiler_emitbytes(state, OP_METHOD, constant);
}


void fei_compiler_parseclassdecl(State* state)
{
    uint8_t nameconstant;
    Token classname;
    ClassCompiler classcompiler;
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect class name.");
    // get class name
    classname = state->aststate.parser.prevtoken;
    // add to constant table as a string, return its index
    nameconstant = fei_compiler_makeidentconst(state, &state->aststate.parser.prevtoken);
    // declare that name variable
    fei_compiler_declvarfromcurrent(state);

    // takes opcode and takes the constant table index
    fei_compiler_emitbytes(state, OP_CLASS, nameconstant);
    // add it to the global hasht; we must DEFINE AFTER DECLARE to use it
    fei_compiler_defvarindex(state, nameconstant);
    // handle class enclosing for 'this'
    classcompiler.name = state->aststate.parser.prevtoken;
    classcompiler.hassuperclass = false;
    classcompiler.enclosing = state->aststate.classcompiler;
    // set new class as current
    state->aststate.classcompiler = &classcompiler;
    // class inheritance
    if(fei_compiler_match(state, TOKEN_KWFROM))
    {
        fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect parent class name.");
        // get the class variable, looks up the parent class by name and push it to the stack
        fei_comprule_variable(state, false);
        // check that the class names must be different
        if(fei_compiler_identsequal(state, &classname, &state->aststate.parser.prevtoken))
        {
            fei_compiler_raiseerror(state, "Cannot inherit class from itself");
        }
        /*
        * super classes
        * - create new lexical scope to ensure that if we declare two classes in the same scope,
        * each has a different local slot to store the superclasses
		*/
        fei_compiler_beginscope(state);
        fei_compiler_addlocal(state, fei_compiler_makesyntoken(state, "super"));
        fei_compiler_defvarindex(state, 0);
        fei_compiler_declnamedvar(state, classname, false);
        fei_compiler_emitbyte(state, OP_INHERIT);
        classcompiler.hassuperclass = true;
    }
    // helper function to geenrate code that LOADS a variable with a given name to te stack
    fei_compiler_declnamedvar(state, classname, false);
    fei_compiler_consume(state, TOKEN_LEFTBRACE, "Expect '{' before class body.");
    while(!fei_compiler_check(state, TOKEN_RIGHTBRACE) && !fei_compiler_check(state, TOKEN_EOF))
    {
        fei_compiler_parsemethoddecl(state);
    }
    fei_compiler_consume(state, TOKEN_RIGHTBRACE, "Expect '}' after class body.");
    // no longer need the class, pop it
    fei_compiler_emitbyte(state, OP_POP);
    // close local scope for superclass variable
    if(classcompiler.hassuperclass)
    {
        fei_compiler_endscope(state);
    }

    state->aststate.classcompiler = state->aststate.classcompiler->enclosing;// go back to enclosing/main() class
}

void fei_compiler_parseclassfuncdecl(State* state)
{
    uint8_t global;
    global = fei_compiler_parsevarfromcurrent(state, "Expect function name.");
    fei_compiler_markinit(state);// scoping
    fei_compiler_parsefuncdecl(state, TYPE_FUNCTION);
    fei_compiler_defvarindex(state, global);
}

void fei_compiler_parsevardecl(State* state)
{
    uint8_t global;
    global = fei_compiler_parsevarfromcurrent(state, "Expect variable name.");
    if(fei_compiler_match(state, TOKEN_ASSIGN))
    {
        fei_compiler_parseexpr(state);
    }
    else
    {
        // not initialized
        fei_compiler_emitbyte(state, OP_NULL);
    }
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "expect ';'");
    }
    // create global variable here; if local, not added to table
    fei_compiler_defvarindex(state, global);
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
    int elsejump;
    int thenjump;
    //fei_compiler_consume(state, TOKEN_LEFTPAREN, "Expect '(' after 'if'.");
    fei_compiler_parseexpr(state);// compile the expression statment inside; fei_compiler_parseprec()
    // after compiling expression above conditon value will be left at the top of the stack
    //fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after condition.");
    // gives an operand on how much to offset the ip; how many bytes of code to skip
    // if falsey, simply adjusts the ip by that amount
    // offset to jump to next (potentially else or elf) statment
    // insert to opcode the then branch statment first, then get offset
    /* this gets distance */
    thenjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);
    fei_compiler_emitbyte(state, OP_POP);
    /* use BACKPATCHING: emit jump first with a placeholder offset, and get how far to jump */
    fei_compiler_parsestatement(state);
    // below jump wil SURELY jump; this is skipped if the first fei_compiler_emitjump is not false
    elsejump = fei_compiler_emitjump(state, OP_JUMP);// need to jump at least 'twice' with an else statement
    // if the original statement is  true, then skip the the else statement
    // if then statment is run; pop the expression inside () after if
    /* this actually jumps */
    fei_compiler_patchjump(state, thenjump);
    // if else statment is run; pop the expression inside () after if
    fei_compiler_emitbyte(state, OP_POP);
    if(fei_compiler_match(state, TOKEN_KWELSE))
    {
        fei_compiler_parsestatement(state);
    }
    // else if
    if(fei_compiler_match(state, TOKEN_KWELF))
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
    uint8_t i;
    uint8_t casescount;
    uint8_t capacity;
    int* casesoffset;
    int oldcapacity;
    int casefalsejump;
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
    casescount = -1;
    capacity = 0;
    // 8 initial switch cases
    casesoffset = (int*)ALLOCATE(state, sizeof(int), 8);
    // while next token is a case, match also advances
    do
    {
        // grow array if needed
        if(capacity < casescount + 1)
        {
            oldcapacity = capacity;
            capacity = GROW_CAPACITY(oldcapacity);
            casesoffset = (int*)GROW_ARRAY(state, sizeof(int), casesoffset, oldcapacity, capacity);
        }
        casescount++;
        fei_compiler_parseexpr(state);
        fei_compiler_consume(state, TOKEN_COLON, "Expect ':' after case expression.");
        fei_compiler_emitbyte(state, OP_SWITCH_EQUAL);// check if both values are equal
        casefalsejump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);// jump if false
        //printf("\ncase false jump offset: %d", casefalsejump);
        // parse the statment
        fei_compiler_parsestatement(state);
        fei_compiler_emitbyte(state, OP_POP);// pop the 'true' from OP_SWITCH_EQUAL
        casesoffset[casescount] = fei_compiler_emitjump(state, OP_JUMP);
        //printf("\ncase true jump offset: %d", casesoffset[casescount]);
        // jump to end of case if false
        fei_compiler_patchjump(state, casefalsejump);
        // pop the 'false' statment from OP_SWITCH_EQUAL
        fei_compiler_emitbyte(state, OP_POP);
    } while(fei_compiler_match(state, TOKEN_KWCASE));
    if(fei_compiler_match(state, TOKEN_KWDEFAULT))
    {
        fei_compiler_consume(state, TOKEN_COLON, "Expect ':' default case.");
        // running the default statement
        fei_compiler_parsestatement(state);
    }
    //fei_compiler_consume(state, TOKEN_KWDEFAULT, "Default case not provided for switch.");
    // fei_compiler_patchjump for each available jump
    for(i = 0; i <= casescount; i++)
    {
        fei_compiler_patchjump(state, casesoffset[i]);
    }
    fei_compiler_emitbyte(state, OP_POP);// pop switch constant
    FREE_ARRAY(state, sizeof(int), casesoffset, capacity);
    fei_compiler_consume(state, TOKEN_RIGHTBRACE, "Expect '}' at the end of switch statement");
}


void fei_compiler_parseprintstmt(State* state)
{
    // this is the function that actually processes the experssion
    fei_compiler_parseexpr(state);
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
    int exitjump;
    int bodyjump;
    int loopstart;
    int incrementstart;
    // for possible variable declarations in clause
    fei_compiler_beginscope(state);
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
    loopstart = fei_compiler_currentchunk(state)->count;
    /*
    * CONDITION CLAUSE
    * 1. If false, pop the recently calculated expression and skip the loop
    * 2. if true, go to the body; see increment clause below
    */
    exitjump = -1;
    if(!fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        fei_compiler_parseexpr(state);
        fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after loop condition.");
        // jump out of loop if condition is false
        exitjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);
        // still need to figure this out, most likely just deleting 'temporary' constants in the scope
        fei_compiler_emitbyte(state, OP_POP);
    }
    // the increment clause
    // if there is something else before the terminating ')'
    if(!fei_compiler_match(state, TOKEN_RIGHTPAREN))
    {
        /*
        * INCEREMENT CLAUSE
        * 1. from the condition clause, first jump OVER the increment, to the body
        * 2. in the body, run the body
        * 3. jump BACK to the increment and run it
        * 4. from the increment jump BACK to the CONDITION clause, back to the cycle
        */
        // for continue
        // jump the increment clause
        bodyjump = fei_compiler_emitjump(state, OP_JUMP);
        // starting index for increment
        incrementstart = fei_compiler_currentchunk(state)->count;
        // set continue jump here, right after the increment statement
        fei_compiler_markcontinuejump(state);
        // run the for expression
        fei_compiler_parseexpr(state);
        // pop expression constant
        fei_compiler_emitbyte(state, OP_POP);
        fei_compiler_consume(state, TOKEN_RIGHTPAREN, "Expect ')' after for clauses.");
        // running the loop
        // goes back to the start of the CONDITION clause of the for loop
        fei_compiler_emitloop(state, loopstart);
        loopstart = incrementstart;
        fei_compiler_patchjump(state, bodyjump);
    }
    fei_compiler_parsestatement(state);// running the code inside the loop
    fei_compiler_emitloop(state, loopstart);
    // patch the jump in the loop body
    if(exitjump != -1)
    {
        fei_compiler_patchjump(state, exitjump);
        // only pop when THERE EXISTS A CONDITION from the clause
        fei_compiler_emitbyte(state, OP_POP);
    }
    // patch break jumps, if available
    fei_compiler_patchbreakjumps(state);
    fei_compiler_endloopscope(state);
    fei_compiler_endscope(state);
}

void fei_compiler_parsewhilestmt(State* state)
{
    int exitjump;
    int loopstart;
    // index where the statement to loop starts
    loopstart = fei_compiler_currentchunk(state)->count;
    fei_compiler_beginloopscope(state);
    // set jump for potential continue statement
    fei_compiler_markcontinuejump(state);
    fei_compiler_parseexpr(state);
    // skip stament if condition is false
    exitjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);
    // pop the last expression(true or false)
    fei_compiler_emitbyte(state, OP_POP);
    fei_compiler_parsestatement(state);
    // method to 'loop' the instruction
    fei_compiler_emitloop(state, loopstart);
    fei_compiler_patchjump(state, exitjump);
    fei_compiler_emitbyte(state, OP_POP);
    // patch break jumps, if available
    fei_compiler_patchbreakjumps(state);
    fei_compiler_endloopscope(state);
}

void fei_compiler_parsebreakstmt(State* state)
{
    int breakjump;
    int loopdepth;
    int breakamount;
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
    breakjump = fei_compiler_emitjump(state, OP_JUMP);
    loopdepth = state->aststate.compiler->loopcounttop;
    breakamount = state->aststate.compiler->breakjumpcounts[loopdepth];
    state->aststate.compiler->breakpatchjumps[state->aststate.compiler->loopcounttop][breakamount - 1] = breakjump;
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after break.");
    }
}

void fei_compiler_parsecontinuestmt(State* state)
{
    int oldcapacity;
    if(state->aststate.compiler->loopcounttop < 0)
    {
        fei_compiler_raiseerror(state, "Continue statement must be enclosed in a loop");
        return;
    }
    if(state->aststate.compiler->loopcounttop == state->aststate.compiler->continuejumpcapacity)
    {
        oldcapacity = state->aststate.compiler->continuejumpcapacity;
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
    int loopstart;
    // fei_compiler_consume(state, TOKEN_LEFTBRACE, "Expect '{' after repeat.");
    loopstart = fei_compiler_currentchunk(state)->count;
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
        {
            return;
        }
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
                {
                    return;
                }
                break;
            default:
                {
                    // do nothing
                }
                break;
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
        // declare variable
        fei_compiler_parsevardecl(state);
    }
    else
    {
        fei_compiler_parsestatement(state);
    }
    if(state->aststate.parser.panicmode)
    {
        // for errors
        fei_compiler_synchronize(state);
    }
}

void fei_compiler_parsestatement(State* state)// either an expression or a print
{
    if(fei_compiler_match(state, TOKEN_KWPRINT))
    {
        fei_compiler_parseprintstmt(state);
    }
    else if(fei_compiler_match(state, TOKEN_KWRETURN))
    {
        // for functions return
        fei_compiler_parsereturnstmt(state);
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
    // parse initial { token
    else if(fei_compiler_match(state, TOKEN_LEFTBRACE))
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
    // start scan/lexing
    fei_lexer_initsource(state, source, len);
    memset(&compiler, 0, sizeof(compiler));
    fei_compiler_init(state, &compiler, TYPE_SCRIPT);
    state->aststate.parser.haderror = false;
    state->aststate.parser.panicmode = false;
    // call to advance once to 'pump' the scanner
    fei_compiler_advancenext(state);
    while(!fei_compiler_match(state, TOKEN_EOF))
    {
        fei_compiler_parsedeclaration(state);
    }
    // ends the expression with a return type
    function = fei_compiler_endcompiler(state);
    // if no error return true
    if(state->aststate.parser.haderror)
    {
        return NULL;
    }
    return function;
}


// marking compiler roots, for garbage collection
void fei_compiler_markroots(State* state)
{
    Compiler* compiler;
    compiler = state->aststate.compiler;
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
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
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



// A void pointer is a pointer that has no associated data type with it.
// A void pointer can hold address of any type and can be typcasted to any type.
void* fei_gcmem_reallocate(State* state, void* pointer, size_t oldsize, size_t newsize)
{
    void* memdata;
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
    memdata = NULL;
    if(pointer == NULL)
    {
        memdata = malloc(newsize);
    }
    else
    {
        memdata = realloc(pointer, newsize);
    }
    if(memdata == NULL)
    {
        fprintf(stderr, "internal error: failed to realloc to %d bytes!\n", (int)newsize);
        exit(1);
    }
    if(pointer == NULL)
    {
        memset(memdata, 0, newsize);
    }
    return memdata;
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
        // use native realloc here
        state->gcstate.graystack = (Object**)realloc(state->gcstate.graystack, sizeof(Object*) * state->gcstate.graycapacity);
    }
    if(state->gcstate.graystack == NULL)
    {
        fprintf(stderr, "internal error: failed to allocate memory for gray stack\n");
        exit(1);
    }
    // add the 'gray' object to the working list
    state->gcstate.graystack[state->gcstate.graycount++] = object;
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        fprintf(stderr, "%p marked ", (void*)object);
        // you cant print first class objects, like how you would print in the actual repl
        fei_value_printvalue(state, stderr, OBJ_VAL(object), true);
        fprintf(stderr, "\n");
    #endif
}

void fei_gcmem_markvalue(State* state, Value value)
{
    // if value is not first class Objtype return
    if(!fei_value_isobj(value))
    {
        return;
    }
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
    for(slot = state->vmstate.stackvalues; slot < state->vmstate.stacktop; slot++)// walk through all values/slots in the Value* array
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
        fprintf(stderr, "%p blackened ", (void*)object);
        fei_value_printvalue(state, stderr, OBJ_VAL(object), true);
        fprintf(stderr, "\n");
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
    Object* object;
    while(state->gcstate.graycount > 0)
    {
        // pop Object* (pointer) from the stack
        // note how -- is the prefix; subtract first then use it as an index
        // --state->gcstate.graycount already decreases its count, hence everything is already 'popped'
        object = state->gcstate.graystack[--state->gcstate.graycount];
        fei_gcmem_blackenobject(state, object);
    }
}

// sweeping all unreachable values
void fei_gcmem_sweep(State* state)
{
    Object* object;
    Object* previous;
    Object* unreached;
    previous = NULL;
    // linked intrusive list of Objects in the VM
    object = state->gcstate.objects;
    while(object != NULL)
    {
        // object marked, do not free
        if(object->ismarked)
        {
            // reset the marking to 'white'
            object->ismarked = false;
            previous = object;
            object = object->next;
        }
        // free the unreachable object
        else
        {
            unreached = object;
            object = object->next;
            // link to previous object if previous not null
            if(previous != NULL)
            {
                previous->next = object;
            }
            // if not set the next as the start of the list
            else
            {
                state->gcstate.objects = object;
            }
            // method that actually frees the object
            fei_gcmem_freeobject(state, unreached);
        }
    }
}

void fei_gcmem_collectgarbage(State* state)
{
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("--Garbage Collection Begin\n");
        size_t before = state->gcstate.bytesallocated;
    #endif
    // function to start traversing the graph, from the root and marking them
    fei_gcmem_markroots(state);
    // tracing each gray marked object
    fei_gcmem_tracerefs(state);
    // removing intern strings, BEFORE the sweep so the pointers can still access its memory
    // function defined in hahst.c
    fei_table_removeunreachable(state, &state->vmstate.strings);
    // free all unreachable roots
    fei_gcmem_sweep(state);
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
    Object* next;
    Object* object;
    object = state->gcstate.objects;
    // free from the whole list
    while(object != NULL)
    {
        next = object->next;
        fei_gcmem_freeobject(state, object);
        object = next;
    }
    free(state->gcstate.graystack);// free gray marked obj stack used for garbage collection
}

// IMPORTANT
// variadic function ( ... ), takes a varying number of arguments
void fei_vm_raiseruntimeerror(State* state, const char* format, ...)
{
    int i;
    int line;
    size_t instruction;
    va_list args;
    CallFrame* frame;
    ObjFunction* function;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
    // printing the stack trace for the function
    // print out each function that was still executing when the program died and where the execution was at the point it died
    for(i = state->vmstate.framecount - 1; i >= 0; i--)
    {
        frame = fei_vm_frameget(state, i);
        function = frame->closure->function;
        // - 1 because IP is sitting on the NEXT INSTRUCTION to be executed
        instruction = frame->ip - function->chunk.code - 1;
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
    frame = fei_vm_frameget(state, state->vmstate.framecount - 1);
    // - 1 to deal with the 1 added initially for the main() CallFrame
    instruction = frame->ip - frame->closure->function->chunk.code - 1;
    line = frame->closure->function->chunk.lines[instruction];
    fprintf(stderr, "Error in script at [Line %d]\n", line);
    fei_vm_resetstack(state);
}

void fei_vm_defnative(State* state, const char* name, NativeFn function)
{
    fei_vm_stackpush(state, OBJ_VAL(fei_object_copystring(state, name, (int)strlen(name))));
    fei_vm_stackpush(state, OBJ_VAL(fei_object_makenativefunc(state, function)));
    fei_table_set(state, &state->vmstate.globals, fei_value_asstring(state->vmstate.stackvalues[0]), state->vmstate.stackvalues[1]);
    fei_vm_stackpop(state);
    fei_vm_stackpop(state);
}

void fei_vm_resetstack(State* state)
{
    // point stackstop to the begininng of the empty array
    // stack array(state->vmstate.stackvalues) is already indirectly declared, hence no need to allocate memory for it
    state->vmstate.stacktop = state->vmstate.stackvalues;
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
    // initialiing the Value stack, also initializing the callframe count
    fei_vm_resetstack(state);
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
    fei_table_initcapacity(state, &state->vmstate.globals, 32);
    fei_table_initcapacity(state, &state->vmstate.strings, 32);
    {
        // init initalizer string
        state->vmstate.initstring = NULL;
        state->vmstate.initstring = fei_object_copystring(state, "init", 4);
        state->vmstate.frameobjects = da_make(state->vmstate.frameobjects, 24, sizeof(CallFrame));
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
    int i;
    state->vmstate.initstring = NULL;
    // free all objects, from state->gcstate.objects
    fei_gcmem_freeobjects(state);
    fei_table_destroy(state, &state->vmstate.globals);
    fei_table_destroy(state, &state->vmstate.strings);
    fei_writer_destroy(state, state->outwriter);
    for(i=0; i<da_count(state->vmstate.frameobjects); i++)
    {
        free(state->vmstate.frameobjects[i]);
    }
    da_destroy(state->vmstate.frameobjects);
    free(state);
}

bool fei_class_invokemethod(State* state, ObjClass* klassobj, ObjString* name, int argcount)
{
    Value method;
    if(!fei_table_get(state, &klassobj->methods, name, &method))
    {
        fei_vm_raiseruntimeerror(state, "cannot invoke undefined property '%s'", name->chars);
        return false;
    }
    return fei_vm_callclosure(state, fei_value_asclosure(method), argcount);
}

// bind method and wrap it in a new ObjBoundMethod
bool fei_class_bindmethod(State* state, ObjClass* klassobj, ObjString* name)
{
    Value method;
    Value peeked;
    ObjBoundMethod* bound;
    // get method from table and bind it
    if(!fei_table_get(state, &klassobj->methods, name, &method))
    {
        // if method not found
        fei_vm_raiseruntimeerror(state, "cannot bind undefined property '%s'", name->chars);
        return false;
    }
    peeked = fei_vm_stackpeek(state, 0);
    // wrap method in a new ObjBoundMethodd
    bound = fei_object_makeboundmethod(state, peeked, fei_value_asclosure(method));
    // pop the class instance
    fei_vm_stackpop(state);
    fei_vm_stackpush(state, OBJ_VAL(bound));
    return true;
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

/* starting point of the compiler */
ResultCode fei_vm_evalsource(State* state, const char* source, size_t len)
{
    ObjClosure* closure;
    ObjFunction* function;
    function = fei_compiler_compilesource(state, source, len);
    if(function == NULL)
    {
        return STATUS_SYNTAXERROR;
    }
    fei_vm_stackpush(state, OBJ_VAL(function));
    closure = fei_object_makeclosure(state, function);
    fei_vm_stackpop(state);
    fei_vm_stackpush(state, OBJ_VAL(closure));
    // 0 params for main()
    fei_vm_callvalue(state, OBJ_VAL(closure), 0);
    return fei_vm_exec(state);
}


CallFrame* fei_vm_frameget(State* state, int idx)
{
    long cnt;
    long cap;
    bool mustalloc;
    CallFrame* fr;
    CallFrame** frobs;
    frobs = state->vmstate.frameobjects;
    cnt = da_count(frobs);
    cap = da_capacity(frobs);
    //fprintf(stderr, "fei_vm_frameget: idx=%d cnt=%d cap=%d\n", idx, cnt);
    mustalloc = (
        (
            (idx >= cnt) ||
            (cnt == 0)
        )
        #if 0
        &&
        (
            (cap > 0) &&
            (idx < cap)
        )
        #endif
    );
    if(mustalloc)
    {
        fr = (CallFrame*)malloc(sizeof(CallFrame));
        memset(fr, 0, sizeof(CallFrame));
        da_push(state->vmstate.frameobjects, sizeof(CallFrame), fr);
    }
    fr = state->vmstate.frameobjects[idx];
    fr->level = idx;
    fr->stackpos = state->vmstate.stackindex;
    return fr;
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

#define ALSO_PRINTTOP 0

void dumpstack(State* state, const char* fmt, ...)
{
    int i;
    va_list va;
    va_start(va, fmt);
    fprintf(stderr, "in ");
    vfprintf(stderr, fmt, va);
    va_end(va);
    fprintf(stderr, ": stackindex=%d:\n", state->vmstate.stackindex);
    for(i=0; i!=da_count(state->vmstate.stackvalues); i++)
    {
        fprintf(stderr, "  stack(values/top)[%d] = ", i);
        #if defined(ALSO_PRINTTOP) && (ALSO_PRINTTOP == 1)
        fprintf(stderr, "{");
        #endif
        fei_value_printvalue(state, stderr, state->vmstate.stackvalues[i], true);
        #if defined(ALSO_PRINTTOP) && (ALSO_PRINTTOP == 1)
            fprintf(stderr, ", ");
            fei_value_printvalue(state, stderr, state->vmstate.stacktop[i], true);
            fprintf(stderr, "}");
        #endif
        fprintf(stderr, "\n");
    }
}

/*
    // pointer to the element just PAST the element containing the top value of the stack
    // == stackvalues[size(stackvalues) - 2]
    Value* stacktop;
*/

/*
void push(const Value& value)
{
    m_stackidx++;
    m_stacktop++;
    m_stack.reserve(m_stack.size() + 1);
    m_stack[m_stacktop - 1] = value;
}
*/
static inline void fei_vm_stackpush_inline(State* state, Value value)
{
    state->vmstate.stackindex++;
    // * in front of the pointer means the rvalue itself, assign value(parameter) to it
    #if defined(USE_DYNVALUE) && (USE_DYNVALUE == 1)
        int growsz;
        int setidx;
        growsz = state->vmstate.stackindex*2;
        setidx = state->vmstate.stacktop - 1;
        //setidx = da_count(state->vmstate.stackvalues) - 1;
        if(setidx <= -1)
        {
            setidx = 0;
        }
        fprintf(stderr, "stackpush:growsz=%d, setidx=%d\n", growsz, setidx);
        state->vmstate.stacktop++;
        
        //da_grow(state->vmstate.stackvalues, growsz, sizeof(Value));
        da_need_to_grow_internal(state->vmstate.stackvalues, growsz);
        da_push(state->vmstate.stackvalues, sizeof(Value), value);
        state->vmstate.stackvalues[setidx] = value;
    #else
        *state->vmstate.stacktop = value;
        state->vmstate.stacktop++;
    #endif
}

/*
Value pop()
{
    m_stackidx--;
    if(m_stackidx < -1)
    {
        m_stackidx = 0;
    }
    Value top = m_stack[m_stackidx-0];
    m_stacktop--;
    m_stack.pop_back();
    return top;
}
*/
static inline Value fei_vm_stackpop_inline(State* state)
{
    state->vmstate.stackindex--;
    if(state->vmstate.stackindex < -1)
    {
        state->vmstate.stackindex = 0;
    }
    //dumpstack(state, "stackpop");
    // first move the stack BACK to get the last element(stacktop points to ONE beyond the last element)
    #if defined(USE_DYNVALUE) && (USE_DYNVALUE == 1)
        Value retme;
        retme = state->vmstate.stackvalues[state->vmstate.stackindex - 0];
        state->vmstate.stacktop--;
        da_pop(state->vmstate.stackvalues, sizeof(Value));
        return retme;
    #else
        state->vmstate.stacktop--;
        return *state->vmstate.stacktop;
    #endif
}

/*
Value& peek(int distance = 0)
{
    assert(distance >= 0);
    dumpstack(Util::Join("peek(", distance, ", (m_stacktop-distance)=", (m_stacktop-distance), ")"));
    //return *(m_stacktop - distance - 1);
    //return m_stack[distance - 0];
    return m_stack[m_stacktop - distance];
}
*/
static inline Value fei_vm_stackpeek_inline(State* state, int distance)
{
    #if defined(USE_DYNVALUE) && (USE_DYNVALUE == 1)
        int comp;
        comp = state->vmstate.stacktop - distance - 2;
        //comp = da_count(state->vmstate.stackvalues) - 3;
        dumpstack(state, "stackpeek(distance=%d, comp=%d)", distance, comp);

        //return state->vmstate.stackvalues[state->vmstate.stacktop - distance];
        /*
        if(distance==0)
        {
            distance+=1;
        }
        */
        return state->vmstate.stackvalues[comp];
    #else
        return state->vmstate.stacktop[-1 - distance];
    #endif
}


void fei_vm_stackpush(State* state, Value value)
{
    fei_vm_stackpush_inline(state, value);
}

Value fei_vm_stackpop(State* state)
{
    return fei_vm_stackpop_inline(state);
}

Value fei_vm_stackpeek(State* state, int distance)
{
    return fei_vm_stackpeek_inline(state, distance);
}

Value fei_vm_stackget(State* state, int idx)
{
    return state->vmstate.stackvalues[idx];
}


/* for call stacks/functions  */
bool fei_vm_callclosure(State* state, ObjClosure* closure, int argcount)
{
    CallFrame* frame;
    // if number of parameters does not match
    if(argcount != closure->function->arity)
    {
        fei_vm_raiseruntimeerror(state, "expected %d arguments but got %d", closure->function->arity, argcount);
        return false;
    }
    // as CallFrame is an array, to ensure array does not overflow
    if(state->vmstate.framecount == CFG_MAX_VMFRAMES)
    {
        fprintf(stderr, "internal error: stack overflow!\n");
        fei_vm_raiseruntimeerror(state, "stack overflow");
        return false;
    }
    // get pointer to next in frame array
    // initializes callframe to the top of the stack
    frame = fei_vm_frameget(state, state->vmstate.framecount++);
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
    NativeFn natfn;
    Value result;
    Value initializer;
    ObjClass* klass;
    ObjBoundMethod* bound;
    if(fei_value_isobj(callee))
    {
        switch(OBJ_TYPE(callee))
        {
            case OBJ_BOUND_METHOD:
                {
                    // get ObjBoundMethod from value type(callee)
                    bound = fei_value_asbound_method(callee);
                    state->vmstate.stacktop[-argcount - 1] = bound->receiver;
                    // run call to execute
                    return fei_vm_callclosure(state, bound->method, argcount);
                }
                break;
            // create class instance
            case OBJ_CLASS:
                {
                    klass = fei_value_asclass(callee);
                    // create new instance here
                    state->vmstate.stacktop[-argcount - 1] = OBJ_VAL(fei_object_makeinstance(state, klass));
                    // if we find one from the table
                    if(fei_table_get(state, &klass->methods, state->vmstate.initstring, &initializer))
                    {
                        return fei_vm_callclosure(state, fei_value_asclosure(initializer), argcount);
                    }
                    // if there ARE arguments but the initalizer method cannot be found
                    else if(argcount != 0)
                    {
                        fei_vm_raiseruntimeerror(state, "class has not initializer, but still received %d arguments", argcount);
                        return false;
                    }
                    return true;
                }
                break;
            case OBJ_CLOSURE:
                {
                    // call to function happens here
                    return fei_vm_callclosure(state, fei_value_asclosure(callee), argcount);
                }
                break;
            case OBJ_NATIVE:
                {
                    natfn = fei_value_asnative(callee);
                    result = natfn(state, argcount, state->vmstate.stacktop - argcount);
                    // remove call and arguments from the stack
                    state->vmstate.stacktop -= argcount + 1;
                    fei_vm_stackpush(state, result);
                    return true;
                }
                break;
            default:
                {
                }
                break;
        }
    }
    fei_vm_raiseruntimeerror(state, "only functions or classes can be called");
    return false;
}


// invoke class method, access method + call method
bool fei_vm_classinvokefromstack(State* state, ObjString* name, int argcount)
{
    Value value;
    Value receiver;
    ObjInstance* instance;
    // grab the receiver of the stack
    receiver = fei_vm_stackpeek_inline(state, argcount);
    // call method with wrong type, not an objinstance type
    if(!fei_value_isinstance(receiver))
    {
        fei_vm_raiseruntimeerror(state, "cannot invoke method on something not an instance");
        return false;
    }
    instance = fei_value_asinstance(receiver);
    // for fields()
    if(fei_table_get(state, &instance->fields, name, &value))
    {
        state->vmstate.stacktop[-argcount - 1] = value;
        return fei_vm_callvalue(state, value, argcount);
    }
    // actual function that searches for method and calls it
    return fei_class_invokemethod(state, instance->classobject, name, argcount);
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
    // pointer comparison: only find the ones ABOVE local
    while(upvalue != NULL && upvalue->location > local)
    {
        prevupvalue = upvalue;
        upvalue = upvalue->next;
    }
    // if the location/local/indeces match
    if(upvalue != NULL && upvalue->location == local)
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
    ObjUpvalue* upvalue;
    while(state->vmstate.openupvalues != NULL && state->vmstate.openupvalues->location >= last)
    {
        // pointer to list of openupvalues
        upvalue = state->vmstate.openupvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        state->vmstate.openupvalues = upvalue->next;
    }
}

// defining method for class type
void fei_vm_classdefmethodfromstack(State* state, ObjString* name)
{
    Value method;
    ObjClass* klassobj;
    method = fei_vm_stackpeek_inline(state, 0);// method/closure is at the top of the stack
    klassobj = fei_value_asclass(fei_vm_stackpeek_inline(state, 1));// class is at the 2nd top
    fei_table_set(state, &klassobj->methods, name, method);// add to hashtable
    fei_vm_stackpop_inline(state);// pop the method
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
    second = fei_value_asstring(fei_vm_stackpeek_inline(state, 0));
    first = fei_value_asstring(fei_vm_stackpeek_inline(state, 1));
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
    fei_vm_stackpop_inline(state);
    fei_vm_stackpop_inline(state);
    fei_vm_stackpush(state, OBJ_VAL(result));
}


bool fei_vmdo_return(State* state)
{
    // if function returns a value, value will beon top of the stack
    Value result = fei_vm_stackpop_inline(state);
    // close lingering closed values
    fei_vm_closeupvalues(state, state->vmstate.topframe->slots);
    state->vmstate.framecount--;
    // return from 'main()'/script function
    if(state->vmstate.framecount == 0)
    {
        // pop main script function from the stack
        fei_vm_stackpop_inline(state);
        return false;
    }
    // for a function
    // discard all the slots the callee was using for its parameters
    // basically 're-assign'
    state->vmstate.stacktop = state->vmstate.topframe->slots;
    // push the return value
    fei_vm_stackpush(state, result);
    // update run function's current frame
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    return true;
}

bool fei_vmdo_superinvoke(State* state)
{
    int count;
    ObjClass* parent;
    ObjString* method;
    method = READ_STRING(state, state->vmstate.topframe);
    count = READ_BYTE(state->vmstate.topframe);
    parent = fei_value_asclass(fei_vm_stackpop_inline(state));
    if(!fei_class_invokemethod(state, parent, method, count))
    {
        return false;
    }
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    return true;
}

bool fei_vmdo_getsuper(State* state)
{
    ObjString* name;
    ObjClass* parent;
    // get method name/identifier
    name = READ_STRING(state, state->vmstate.topframe);
    // class identifier is at the top of the stack
    parent = fei_value_asclass(fei_vm_stackpop_inline(state));
    // if binding fails
    if(!fei_class_bindmethod(state, parent, name))
    {
        return false;
    }
    return true;
}

bool fei_vmdo_inherit(State* state)
{
    Value parent;
    ObjClass* child;
    // parent class from 2nd top of the stack
    // ensure that parent identifier is a class
    parent = fei_vm_stackpeek_inline(state, 1);
    if(!fei_value_isclass(parent))
    {
        fei_vm_raiseruntimeerror(state, "parent identifier is not a class");
        return false;
    }
    // child class at the top of the stack
    child = fei_value_asclass(fei_vm_stackpeek_inline(state, 0));
    // add all methods from parent to child table
    fei_table_mergefrom(state, &fei_value_asclass(parent)->methods, &child->methods);
    // pop the child class
    fei_vm_stackpop_inline(state);
    return true;
}

bool fei_vmdo_invokemethod(State* state)
{
    int argcount;
    ObjString* method;
    method = READ_STRING(state, state->vmstate.topframe);
    argcount = READ_BYTE(state->vmstate.topframe);
    // new invoke function
    if(!fei_vm_classinvokefromstack(state, method, argcount))
    {
        return false;
    }
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    return true;
}

bool fei_vmdo_makeclosure(State* state)
{
    int i;
    uint8_t index;
    uint8_t islocal;
    ObjClosure* closure;
    ObjFunction* function;
    // load compiled function from table
    function = fei_value_asfunction(READ_CONSTANT(state, state->vmstate.topframe));
    closure = fei_object_makeclosure(state, function);
    fei_vm_stackpush(state, OBJ_VAL(closure));
    // fill upvalue array over in the interpreter when a closure is created
    // to see upvalues in each slot
    for(i = 0; i < closure->upvaluecount; i++)
    {
        // read islocal bool
        islocal = READ_BYTE(state->vmstate.topframe);
        // read index for local, if available, in the closure
        index = READ_BYTE(state->vmstate.topframe);
        if(islocal)
        {
            // get from slots stack
            closure->upvalues[i] = fei_vm_captureupvalue(state, state->vmstate.topframe->slots + index);
        }
        // if not local(nested upvalue)
        else
        {
            // get from current upvalue
            closure->upvalues[i] = state->vmstate.topframe->closure->upvalues[index];
        }
    }
    return true;
}

bool fei_vmdo_setproperty(State* state)
{
    ObjInstance* instance;
    if(!fei_value_isinstance(fei_vm_stackpeek_inline(state, 1)))// if not an instance
    {
        fei_vm_raiseruntimeerror(state, "setproperty() where parent symbol is not a class instance");
        return false;
    }
    // not top most, as the top most is reserved for the new value to be set
    instance = fei_value_asinstance(fei_vm_stackpeek_inline(state, 1));
    //peek(0) is the new value
    fei_table_set(state, &instance->fields, READ_STRING(state, state->vmstate.topframe), fei_vm_stackpeek_inline(state, 0));
    // pop the already set value
    Value value = fei_vm_stackpop_inline(state);
    // pop the property instance itself
    fei_vm_stackpop_inline(state);
    // push the value back again
    fei_vm_stackpush(state, value);
    return true;
}

bool fei_vmdo_getproperty(State* state)
{
    Value value;
    ObjString* name;
    ObjInstance* instance;
    // to make sure only instances are allowed to have fields
    if(!fei_value_isinstance(fei_vm_stackpeek_inline(state, 0)))
    {
        fei_vm_raiseruntimeerror(state, "only instances have properties");
        return false;
    }
    // get instance from top most stack
    instance = fei_value_asinstance(fei_vm_stackpeek_inline(state, 0));
    // get identifier name
    name = READ_STRING(state, state->vmstate.topframe);
    // get from fields hash table, assign it to instance
    if(fei_table_get(state, &instance->fields, name, &value))
    {
        // pop the instance itself
        fei_vm_stackpop_inline(state);
        fei_vm_stackpush(state, value);
        return true;
    }
    // no method as well, error
    if(!fei_class_bindmethod(state, instance->classobject, name))
    {
        return false;
    }
    return true;
}

bool fei_vmdo_unary(State* state, uint8_t instruc)
{
    double dnum;
    double res;
    Value poked;
    Value popped;
    poked = fei_vm_stackpeek_inline(state, 0);
    if(!fei_value_isnumber(poked))
    {
        fei_vm_raiseruntimeerror(state, "operand must be a number");
        return false;
    }
    popped = fei_vm_stackpop_inline(state);
    dnum = fei_value_asnumber(popped);
    switch(instruc)
    {
        case OP_NEGATE:
            {
                res = -dnum;
            }
            break;
        default:
            {
                fei_vm_raiseruntimeerror(state, "invalid instruction '%d' for vmdo_unary", instruc);
                return false;
            }
            break;
    }
    fei_vm_stackpush(state, NUMBER_VAL(res));
    return true;
}

bool fei_vmdo_binary(State* state, uint8_t instruc)
{
    Value valright;
    Value valleft;
    Value pokeright;
    Value pokeleft;
    double fvleft;
    double fvright;
    int nvleft;
    int nvright;
    pokeright = fei_vm_stackpeek_inline(state, 0);
    pokeleft = fei_vm_stackpeek_inline(state, 1);
    if((instruc == OP_ADD) && (fei_value_isstring(pokeright) && fei_value_isstring(pokeleft)))
    {
        fei_vmdo_strconcat(state);
        return true;
    }
    if(fei_value_isnumber(pokeright) && fei_value_isnumber(pokeleft))
    {
        valright = fei_vm_stackpop_inline(state);
        valleft = fei_vm_stackpop_inline(state);
        // do NOT turn these into macros, since some of can be optimized further.
        // macros would make that much more difficult.
        switch(instruc)
        {
            case OP_ADD:
                {
                    fvright = (double)fei_value_asnumber(valright);
                    fvleft = (double)fei_value_asnumber(valleft);
                    fei_vm_stackpush(state, NUMBER_VAL(fvleft + fvright));
                }
                break;
            case OP_SUBTRACT:
                {
                    fvright = (double)fei_value_asnumber(valright);
                    fvleft = (double)fei_value_asnumber(valleft);
                    fei_vm_stackpush(state, NUMBER_VAL(fvleft - fvright));
                }
                break;
            case OP_MULTIPLY:
                {
                    fvright = (double)fei_value_asnumber(valright);
                    fvleft = (double)fei_value_asnumber(valleft);
                    fei_vm_stackpush(state, NUMBER_VAL(fvleft * fvright));
                }
                break;
            case OP_DIVIDE:
                {
                    fvright = (double)fei_value_asnumber(valright);
                    fvleft = (double)fei_value_asnumber(valleft);
                    fei_vm_stackpush(state, NUMBER_VAL(fvleft / fvright));
                }
                break;
            case OP_MODULO:
                {
                    nvright = (int)fei_value_asnumber(valright);
                    nvleft = (int)fei_value_asnumber(valleft);
                    fei_vm_stackpush(state, NUMBER_VAL(nvleft % nvright));
                }
                break;
            case OP_GREATER:
                {
                    fvright = (double)fei_value_asnumber(valright);
                    fvleft = (double)fei_value_asnumber(valleft);
                    fei_vm_stackpush(state, BOOL_VAL(fvleft > fvright));
                }
                break;
            case OP_LESS:
                {
                    fvright = (double)fei_value_asnumber(valright);
                    fvleft = (double)fei_value_asnumber(valleft);
                    fei_vm_stackpush(state, BOOL_VAL(fvleft < fvright));
                }
                break;
            default:
                {
                    fei_vm_raiseruntimeerror(state, "invalid instruction '%d' for vmdo_binary", instruc);
                    return false;
                }
        }
    }
    else
    {
        fei_vm_raiseruntimeerror(state, "operands are incompatible");
        return false;
    }
    return true;
}

bool fei_vmdo_call(State* state)
{
    int argcount;
    argcount = READ_BYTE(state->vmstate.topframe);
    // call function; pass in the function name istelf[peek(depth)] and the number of arguments
    if(!fei_vm_callvalue(state, fei_vm_stackpeek_inline(state, argcount), argcount))
    {
        return false;
    }
    // to update pointer if callframe is successful, asnew frame is added
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    return true;
}

bool fei_vmdo_switchequal(State* state)
{
    Value a;
    Value b;
    // only pop second value
    b = fei_vm_stackpop_inline(state);
    // peek topmost, the first value
    a = fei_vm_stackpeek_inline(state, 0);
    fei_vm_stackpush(state, BOOL_VAL(fei_value_compare(state, a, b)));
    return true;
}

bool fei_vmdo_compare(State* state)
{
    Value a;
    Value b;
    b = fei_vm_stackpop_inline(state);
    a = fei_vm_stackpop_inline(state);
    fei_vm_stackpush(state, BOOL_VAL(fei_value_compare(state, a, b)));
    return true;
}

bool fei_vmdo_logicalnot(State* state)
{
    bool isfalsey;
    Value popped;
    popped = fei_vm_stackpop_inline(state);
    isfalsey = fei_value_isfalsey(state, popped);
    fei_vm_stackpush(state, BOOL_VAL(isfalsey));
    return true;
}

bool fei_vmdo_defineglobal(State* state)
{
    ObjString* name;
    // get name from constant table
    name = READ_STRING(state, state->vmstate.topframe);
    // take value from the top of the stack
    fei_table_set(state, &state->vmstate.globals, name, fei_vm_stackpeek_inline(state, 0));
    fei_vm_stackpop_inline(state);
    return true;
}

bool fei_vmdo_setglobal(State* state)
{
    ObjString* name;
    name = READ_STRING(state, state->vmstate.topframe);
    // if key not in hash table
    if(fei_table_set(state, &state->vmstate.globals, name, fei_vm_stackpeek_inline(state, 0)))
    {
        //fei_table_delete(state, &state->vmstate.globals, name);// delete the false name
        //fei_vm_raiseruntimeerror(state, "undefined variable '%s'", name->chars);
        //return STATUS_RTERROR;
    }
    return true;
}

bool fei_vmdo_getglobal(State* state)
{
    Value value;
    ObjString* name;
    // get the name
    name = READ_STRING(state, state->vmstate.topframe);
    // if key not in hash table
    if(!fei_table_get(state, &state->vmstate.globals, name, &value))
    {
        fei_vm_raiseruntimeerror(state, "undefined variable '%s'", name->chars);
        return false;
    }
    fei_vm_stackpush(state, value);
    return true;
}

bool fei_vmdo_setlocal(State* state)
{
    uint8_t slot;
    slot = READ_BYTE(state->vmstate.topframe);
    // all the local var's VARIABLES are stored inside state->vmstate.stackvalues
    // takes from top of the stack and stores it in the stack slot
    state->vmstate.topframe->slots[slot] = fei_vm_stackpeek_inline(state, 0);
    return true;
}

bool fei_vmdo_getlocal(State* state)
{
    uint8_t slot;
    slot = READ_BYTE(state->vmstate.topframe);
    // pushes the value to the stack where later instructions can read it
    fei_vm_stackpush(state, state->vmstate.topframe->slots[slot]);
    return true;
}

bool fei_vmdo_getconstant(State* state)
{
    Value constant;
    // READ the next line, which is the INDEX of the constant in the constants array
    constant = READ_CONSTANT(state, state->vmstate.topframe);
    fei_vm_stackpush(state, constant);
    return true;
}

bool fei_vmdo_setupvalue(State* state)
{
    uint8_t slot;
    // read index
    slot = READ_BYTE(state->vmstate.topframe);
    // set to the topmost stack
    *(state->vmstate.topframe->closure->upvalues[slot]->location) = fei_vm_stackpeek_inline(state, 0);
    return true;
}

bool fei_vmdo_getupvalue(State* state)
{
    uint8_t slot;
    // read index
    slot = READ_BYTE(state->vmstate.topframe);
    // push the value to the stack
    fei_vm_stackpush(state, *(state->vmstate.topframe->closure->upvalues[slot]->location));
    return true;
}

bool fei_vmdo_closeupvalue(State* state)
{
    // put address to the slot
    fei_vm_closeupvalues(state, state->vmstate.stacktop - 1);
    // pop from the stack
    fei_vm_stackpop_inline(state);
    return true;
}

bool fei_vmdo_jumpalways(State* state)
{
    uint16_t offset;
    (void)state;
    offset = READ_SHORT(state->vmstate.topframe);
    state->vmstate.topframe->ip += offset;
    return true;
}

bool fei_vmdo_jumpiffalse(State* state)
{
    uint16_t offset;
    // offset already put in the stack
    offset = READ_SHORT(state->vmstate.topframe);
    // actual jump instruction is done here; skip over the instruction pointer
    if(fei_value_isfalsey(state, fei_vm_stackpeek_inline(state, 0)))
    {
        // if evaluated expression inside if statement is false jump
        state->vmstate.topframe->ip += offset;
    }
    return true;
}

bool fei_vmdo_loop(State* state)
{
    uint16_t offset;
    (void)state;
    offset = READ_SHORT(state->vmstate.topframe);
    // jumps back
    state->vmstate.topframe->ip -= offset;
    return true;
}

bool fei_vmdo_loopiffalse(State* state)
{
    uint16_t offset;
    // offset already put in the stack
    offset = READ_SHORT(state->vmstate.topframe);
    // bool state is at the top of the stack
    // if false loop back
    if(fei_value_isfalsey(state, fei_vm_stackpeek_inline(state, 0)))
    {
        state->vmstate.topframe->ip -= offset;
    }
    // pop the true/false
    fei_vm_stackpop_inline(state);
    return true;
}

bool fei_vmdo_loopiftrue(State* state)
{
    uint16_t offset;
    (void)state;
    // offset already put in the stack
    offset = READ_SHORT(state->vmstate.topframe);
    // bool state is at the top of the stack
    // if not false loop back
    if(!fei_value_isfalsey(state, fei_vm_stackpeek_inline(state, 0)))
    {
        state->vmstate.topframe->ip -= offset;
    }
    // pop the true/false
    fei_vm_stackpop_inline(state);
    return true;
}

typedef bool(*VMPrimitive)(State*);

#define exec_vmprim(fn) \
    { \
        if(!(fn)(state)) \
        { \
            return STATUS_RTERROR; \
        } \
    }

// run the chunk
ResultCode fei_vm_exec(State* state)
{
    uint8_t instruction;
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    while(true)
    {
        // fei_dbgdisas_instr needs an byte offset, do pointer math to convert ip back to relative offset
        // from the beginning of the chunk (subtract current ip from the starting ip)
        // IMPORTANT -> only for debugging the VM
        #if defined(DEBUG_TRACE_EXECUTION) && (DEBUG_TRACE_EXECUTION == 1)
            // for stack tracing
            printf("      ");
            /* note on C POINTERSE
            -> pointing to the array itself means pointing to the start of the array, or the first element of the array
            -> ++/-- means moving through the array (by 1 or - 1)
            -> you can use operands like < > to tell compare how deep are you in the array
            */
            // prints every existing value in the stack
            for(Value* slot = state->vmstate.stackvalues; slot < state->vmstate.stacktop; slot++)
            {
                fprintf(stderr, "[ ");
                fei_value_printvalue(state, stderr, *slot, true);
                fprintf(stderr, " ]");
            }
            fei_dbgdisas_instr(state, &state->vmstate.topframe->closure->function->chunk, (int)(state->vmstate.topframe->ip - state->vmstate.topframe->closure->function->chunk.code));
        #endif
        // get result of the byte read, every set of instruction starts with an opcode
        switch(instruction = READ_BYTE(state->vmstate.topframe))
        {
            case OP_CONSTANT:
                {
                    exec_vmprim(fei_vmdo_getconstant);
                }
                break;
            // unary opcode
            case OP_NEGATE:
                {
                    if(!fei_vmdo_unary(state, instruction))
                    {
                        return STATUS_RTERROR;
                    }
                }
                break;
            // literals
            case OP_NULL:
                {
                    fei_vm_stackpush(state, NULL_VAL);
                }
                break;
            case OP_TRUE:
                {
                    fei_vm_stackpush(state, BOOL_VAL(true));
                }
                break;
            case OP_FALSE:
                {
                    fei_vm_stackpush(state, BOOL_VAL(false));
                }
                break;
            case OP_ADD:
            case OP_SUBTRACT:
            case OP_MULTIPLY:
            case OP_DIVIDE:
            case OP_MODULO:
            case OP_LESS:
            case OP_GREATER:
                {
                    if(!fei_vmdo_binary(state, instruction))
                    {
                        return STATUS_RTERROR;
                    }
                }
                break;
            case OP_NOT:
                {
                    exec_vmprim(fei_vmdo_logicalnot);
                }
                break;
            case OP_SWITCH_EQUAL:
                {
                    exec_vmprim(fei_vmdo_switchequal);
                }
                break;
            case OP_EQUAL:
                {
                    exec_vmprim(fei_vmdo_compare);
                }
                break;
            case OP_PRINT:
                {
                    fei_value_printvalue(state, stdout, fei_vm_stackpop_inline(state), false);
                    printf("\n");
                }
                break;
            case OP_POP:
                {
                    fei_vm_stackpop_inline(state);
                }
                break;
            case OP_GET_LOCAL:
                {
                    exec_vmprim(fei_vmdo_getlocal);
                }
                break;
            case OP_SET_LOCAL:
                {
                    exec_vmprim(fei_vmdo_setlocal);
                }
                break;
            case OP_DEFINE_GLOBAL:
                {
                    exec_vmprim(fei_vmdo_defineglobal);
                }
                break;
            case OP_GET_GLOBAL:
                {
                    exec_vmprim(fei_vmdo_getglobal);
                }
                break;
            case OP_SET_GLOBAL:
                {
                    exec_vmprim(fei_vmdo_setglobal);
                }
                break;
            // upvalues set/get
            case OP_GET_UPVALUE:
                {
                    exec_vmprim(fei_vmdo_getupvalue);
                }
                break;
            case OP_SET_UPVALUE:
                {
                    exec_vmprim(fei_vmdo_setupvalue);
                }
                break;
            case OP_GET_PROPERTY:
                {
                    exec_vmprim(fei_vmdo_getproperty);
                }
                break;
            case OP_SET_PROPERTY:
                {
                    exec_vmprim(fei_vmdo_setproperty);
                }
                break;
            case OP_CLOSE_UPVALUE:
                {
                    exec_vmprim(fei_vmdo_closeupvalue);
                }
                break;
            // will always jump
            case OP_JUMP:
                {
                    exec_vmprim(fei_vmdo_jumpalways);
                }
                break;
            case OP_JUMP_IF_FALSE:// for initial if, will not jump if expression inside is true
                {
                    exec_vmprim(fei_vmdo_jumpiffalse);
                }
                break;
            case OP_LOOP:
                {
                    exec_vmprim(fei_vmdo_loop);
                }
                break;
            case OP_LOOP_IF_FALSE:
                {
                    exec_vmprim(fei_vmdo_loopiffalse);
                }
                break;
            case OP_LOOP_IF_TRUE:
                {
                    exec_vmprim(fei_vmdo_loopiftrue);
                }
                break;
            // a callstack to a funcion has the form of function name, param1, param2...
            // the top level code, or caller, also has the same function name, param1, param2... in the right order
            case OP_CALL:
                {
                    exec_vmprim(fei_vmdo_call);
                }
                break;
            // closures
            case OP_CLOSURE:
                {
                    exec_vmprim(fei_vmdo_makeclosure);
                }
                break;
            case OP_CLASS:
                {
                    // load string for the class' name and push it onto the stack
                    fei_vm_stackpush(state, OBJ_VAL(fei_object_makeclass(state, READ_STRING(state, state->vmstate.topframe))));
                }
                break;
            case OP_METHOD:
                {
                    // get name of the method
                    fei_vm_classdefmethodfromstack(state, READ_STRING(state, state->vmstate.topframe));
                }
                break;
            case OP_INVOKE:
                {
                    exec_vmprim(fei_vmdo_invokemethod);
                }
                break;
            case OP_INHERIT:
                {
                    exec_vmprim(fei_vmdo_inherit);
                }
                break;
            case OP_GET_SUPER:
                {
                    exec_vmprim(fei_vmdo_getsuper);
                }
                break;
            case OP_SUPER_INVOKE:
                {
                    exec_vmprim(fei_vmdo_superinvoke);
                }
                break;
            case OP_RETURN:
                {
                    if(fei_vmdo_return(state) == false)
                    {
                        return STATUS_OK;                        
                    }
                }
                break;
        }
    }
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

#if defined(FEI_HAVE_READLINE)
void repl(State* state)
{
    char* line;
    while(true)
    {
        line = readline(">> ");
        if(line == NULL)
        {
            break;
        }
        fei_vm_evalsource(state, line, strlen(line));
        add_history(line);
    }
}
#endif

void runfile(State* state, const char* path)
{
    size_t len;
    char* source;
    source = readfile(path, &len);
    ResultCode result = fei_vm_evalsource(state, source, len);
    free(source);
    if(result == STATUS_SYNTAXERROR)
    {
        exit(51);
    }
    if(result == STATUS_RTERROR)
    {
        exit(61);
    }
}

enum
{
    MAX_RESTARGS = 1024,
    MAX_OPTS = 1024,
};

typedef struct Flag_t Flag_t;
typedef struct FlagContext_t FlagContext_t;
typedef struct Options_t Options_t;


struct Flag_t
{
    char flag;
    char* value;
};

struct FlagContext_t
{
    int nargc;
    int fcnt;
    int poscnt;
    char* positional[MAX_RESTARGS + 1];
    Flag_t flags[MAX_OPTS + 1];
};

struct Options_t
{
    char* debugmode;
    char* codeline;
};


#define ptyp(t) \
    fprintf(stderr, "%d\tsizeof(%s)\n", (int)sizeof(t), #t)

static bool populate_flags(int argc, int begin, char** argv, const char* expectvalue, FlagContext_t* fx)
{
    int i;
    int nextch;
    int psidx;
    int flidx;
    char* arg;
    char* nextarg;
    psidx = 0;
    flidx = 0;
    fx->fcnt = 0;
    fx->poscnt = 0;
    for(i=begin; i<argc; i++)
    {
        arg = argv[i];
        nextarg = NULL;
        if((i+1) < argc)
        {
            nextarg = argv[i+1];
        }
        if(arg[0] == '-')
        {
            fx->flags[flidx].flag = arg[1];
            fx->flags[flidx].value = NULL;
            if(strchr(expectvalue, arg[1]) != NULL)
            {
                nextch = arg[2];
                /* -e "somecode(...)" */
                /* -e is followed by text: -e"somecode(...)" */
                if(nextch != 0)
                {
                    fx->flags[flidx].value = arg + 2;
                }
                else if(nextarg != NULL)
                {
                    if(nextarg[0] != '-')
                    {
                        fx->flags[flidx].value = nextarg;
                        i++;
                    }
                }
                else
                {
                    fx->flags[flidx].value = NULL;
                }
            }
            flidx++;
        }
        else
        {
            fx->positional[psidx] = arg;
            psidx++;
        }
    }
    fx->fcnt = flidx;
    fx->poscnt = psidx;
    fx->nargc = i;
    return true;
}

static void show_help()
{
    printf("lit [options] [files]\n");
    printf("    -o --output [file]  Instead of running the file the compiled bytecode will be saved.\n");
    printf(" -O[name] [string] Enables given optimization. For the list of aviable optimizations run with -Ohelp\n");
    printf(" -D[name]  Defines given symbol.\n");
    printf(" -e --eval [string] Runs the given code string.\n");
    printf(" -p --pass [args] Passes the rest of the arguments to the script.\n");
    printf(" -i --interactive Starts an interactive shell.\n");
    printf(" -d --dump  Dumps all the bytecode chunks from the given file.\n");
    printf(" -t --time  Measures and prints the compilation timings.\n");
    printf(" -h --help  I wonder, what this option does.\n");
    printf(" If no code to run is provided, lit will try to run either main.lbc or main.lit and, if fails, default to an interactive shell will start.\n");
}

static bool parse_options(Options_t* opts, Flag_t* flags, int fcnt)
{
    int i;
    opts->codeline = NULL;
    opts->debugmode = NULL;
    for(i=0; i<fcnt; i++)
    {
        switch(flags[i].flag)
        {
            case 'h':
                {
                    show_help();
                    return false;
                }
                break;
            case 'e':
                {
                    if(flags[i].value == NULL)
                    {
                        fprintf(stderr, "flag '-e' expects a string\n");
                        return false;
                    }
                    opts->codeline = flags[i].value;
                }
                break;
            case 'd':
                {
                    if(flags[i].value == NULL)
                    {
                        fprintf(stderr, "flag '-d' expects a value. run '-h' for possible values\n");
                        return false;
                    }
                    opts->debugmode = flags[i].value;
                }
                break;
            default:
                break;
        }
    }
    return true;
}


int main(int argc, char* argv[])
{
    bool cmdfailed;
    const char* dm;
    const char* filename;
    State* state;
    FlagContext_t fx;
    Options_t opts;
    cmdfailed = false;
    state = fei_state_init();
    populate_flags(argc, 1, argv, "ed", &fx);
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

    if(!parse_options(&opts, fx.flags, fx.fcnt))
    {
        cmdfailed = true;
    }
    else
    {
        if(opts.debugmode != NULL)
        {
            dm = opts.debugmode;
            /*
            if(strcmp(dm, "bc") == 0)
            {
                state->config.dumpbytecode = true;
            }
            else if(strcmp(dm, "ast") == 0)
            {
                state->config.dumpast = true;
            }
            else
            */
            {
                fprintf(stderr, "unrecognized dump mode '%s'\n", dm);
                cmdfailed = true;
            }
        }
    }
    if(!cmdfailed)
    {

        if((fx.poscnt > 0) || (opts.codeline != NULL))
        {
            if(opts.codeline)
            {
                fei_vm_evalsource(state, opts.codeline, strlen(opts.codeline));
            }
            else
            {
                filename = fx.positional[0];
                runfile(state, filename);
            }
        }
        else
        {
            #if defined(LIT_HAVE_READLINE)
                repl(state);
            #else
                fprintf(stderr, "no repl support compiled in\n");
            #endif
        }
    }
    fei_state_destroy(state);
    return 0;
}
