
#pragma once

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

/*
#ifndef __TINYC__
    #if __has_include(<readline/readline.h>)
        #include <readline/readline.h>
        #include <readline/history.h>
        #define FEI_HAVE_READLINE
    #endif
#endif
*/

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

#define fei_value_makeobject(object) fei_value_makeobject_actual((void*)(object))

// pass in as a pointer to the object, receives the actual object

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

#define fei_value_asnumber(v) \
    ( \
        ((v).isfixednumber) ? \
        ((v).as.valfixednum) : \
        ((v).as.valfloatnum) \
    )

enum FeiAstTokType
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


/*
* types of the most primitive types, stuff that can be stored directly as a FeiValue on the stack.
*/
enum FeiValType
{
    VAL_UNDEF = 0,
    VAL_BOOL = 1,
    VAL_NULL = 2,
    VAL_NUMBER = 3,
    // for bigger instances such as strings, functions, heap-allocated; the payload is a heap pointer
    VAL_OBJ = 4,
};

/*
* types of heap allocated objects.
* important: enum values must be in continuation of FeiValType, STARTING at 5.
* to add a new type, either re-order the values, or just append a new one.
*/
enum FeiObjType
{
    OBJ_BOUND_METHOD = 5,
    OBJ_INSTANCE = 6,
    OBJ_CLASS = 7,
    OBJ_CLOSURE = 8,
    OBJ_FUNCTION = 9,
    OBJ_NATIVE = 10,
    OBJ_STRING = 11,
    OBJ_UPVALUE = 12
};

/*
* status code returned by the VM.
*/
enum ResultCode
{
    STATUS_OK,
    STATUS_SYNTAXERROR,
    STATUS_RTERROR
};

typedef enum /**/ FeiObjType FeiObjType;
typedef enum /**/ FeiAstTokType FeiAstTokType;
typedef enum /**/ FeiValType FeiValType;
typedef enum /**/ ResultCode ResultCode;
typedef struct /**/ FeiValue FeiValue;
typedef struct /**/ FeiObject FeiObject;
typedef struct /**/ ObjString ObjString;
typedef struct /**/ ObjFunction ObjFunction;
typedef struct /**/ ObjClass ObjClass;
typedef struct /**/ ObjInstance ObjInstance;
typedef struct /**/ ObjBoundMethod ObjBoundMethod;
typedef struct /**/ ObjUpvalue ObjUpvalue;
typedef struct /**/ ObjClosure ObjClosure;
typedef struct /**/ ObjNative ObjNative;
typedef struct /**/ FeiAstLocal FeiAstLocal;
typedef struct /**/ FeiAstUpvalue FeiAstUpvalue;
typedef struct /**/ ASTState ASTState;
typedef struct /**/ GCState GCState;
typedef struct /**/ VMState VMState;
typedef struct /**/ FeiState FeiState;
typedef struct /**/ FeiVMFrame FeiVMFrame;
typedef struct /**/ FeiAstToken FeiAstToken;
typedef struct /**/ FeiAstLexer FeiAstLexer;
typedef struct /**/ FeiAstParser FeiAstParser;
typedef struct /**/ FeiAstRule FeiAstRule;
typedef struct /**/ FeiAstCompiler FeiAstCompiler;
typedef struct /**/ ClassCompiler ClassCompiler;
typedef struct /**/ ValArray ValArray;
typedef struct /**/ FeiBytecodeList FeiBytecodeList;
typedef struct /**/ TabEntry TabEntry;
typedef struct /**/ Table Table;
typedef struct /**/ Writer Writer;

/*
* callback function type used in the parser.
*/
typedef void (*ParseFn)(FeiState*, bool);


/*
* callback function type for builtin functions, object functions, etc.
* first argument is the overall state (obviously),
* second argument is an instance *if* this function belongs to some kind of object,
* third argument is the number of arguments passed,
* fourth argument is the C array of argument values.
*/
typedef FeiValue (*NativeFn)(FeiState*, FeiValue, int, FeiValue*);

/*
* used to abstract writing things to output.
* as of now, supports IO only.
*/
struct Writer
{
    FILE* filehandle;
    bool filemustclose;
};

/*
* the most basic, primitive representation of a value.
* be careful if and when augmenting this type: as of now, FeiValue is 16 bytes.
* this isn't a requirement anywhere, but keeping it small also keeps it fast. 
*/
struct FeiValue
{
    FeiValType type;
    bool isfixednumber;
    union
    {
        bool valbool;
        int64_t valfixednum;
        double valfloatnum;
        FeiObject* valobjptr;
    } as;
};

/*
* a heap allocated object.
* it doubles as a linked list for associated child objects.
*/
struct FeiObject
{
    // for mark-sweep garbage collection
    bool ismarked;

    FeiObjType type;

    // linked list or intrusive list, to avoid memory leaks, obj itself as a node
    // traverse the list to find every object that has been allocated on the heap
    FeiObject* next;
};

/*
* a single parsed Token.
*/
struct FeiAstToken
{
    int length;
    int line;
    FeiAstTokType type;
    const char* toksrc;
};

/*
* the source scanner, which tokenizes input.
*/
struct FeiAstLexer
{
    int line;
    size_t length;
    const char* startsrc;
    const char* currentsrc;
};

/*
* the parser, which parses tokenized input into an abstract syntax tree.
*/
struct FeiAstParser
{
    bool haderror;
    bool panicmode;
    FeiAstToken currtoken;
    FeiAstToken prevtoken;
};


/*
* a parse rule definition;
* $prefix is a function that parses a token after an operator;
* $infix is a function that parses token following an operator.
*/
struct FeiAstRule
{
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
};

/*
* holds information about a local name in the AST.
*/
struct FeiAstLocal
{
    FeiAstToken name;

    // depth of the variable, corresponding to scoredepth in the struct below
    int depth;

    // track whether the local is captured by a closure or no
    bool iscaptured;
};

struct FeiAstUpvalue
{
    bool islocalvar;
    // matches the index of the local variable in ObjClosure
    int index;
};

/*
* compiles an abstract syntax tree into a stream of bytecode.
*/
struct FeiAstCompiler
{
    // wrapping the whole program into one big main() function
    FuncType progfunctype;
    ObjFunction* programfunc;

    // pointer to the 'outer'/enclosing compiler, to return to after function
    FeiAstCompiler* enclosing;

    /*
    * array to store locals, ordered in the order of declarations
    * TODO: should be dynamic
    */
    int progloccount;
    FeiAstLocal proglocals[CFG_MAX_COMPILERLOCALS];

    /*
    * TODO: should be dynamic
    */
    FeiAstUpvalue upvalues[CFG_MAX_COMPILERUPVALS];

    // number of scopes/blocks surrounding the code
    int scopedepth;

    // for loop breaks and continues, loop enclosing
    int loopcounttop;
    int* continuejumps;

    // only for continue jumpbs
    int continuejumpcapacity;

    /*
    * for patching all break statements
    * TODO: should be dynamic
    */
    int breakpatchjumps[CFG_MAX_COMPILERBREAK][CFG_MAX_COMPILERBREAK];
    int breakjumpcounts[CFG_MAX_COMPILERBREAK];
};

/*
* aid in compiling a class definition.
*/
struct ClassCompiler
{
    // to end scope in superclass declaration
    bool hassuperclass;
    FeiAstToken name;
    ClassCompiler* enclosing;
};

/*
* a dynamic array holding FeiValue items.
*/
struct ValArray
{
    int capacity;
    int count;
    FeiValue* values;
};

/*
a dynamic array holding bytecode.
*/
struct FeiBytecodeList
{
    int count;
    int capacity;
    uint8_t* code;
    // array of integers that parallels the bytecode/codestream, to get where each location of the bytecode is
    int* lines;
    // store double value literals
    ValArray constants;
};

/*
* an entry in a Table.
*/
struct TabEntry
{
    /* the key (obviously) */
    ObjString* key;
    /* and the value it is assigned to */
    FeiValue value;
};

struct Table
{
    int count;
    int capacity;
    TabEntry* entries;
};


// for functions and calls
struct ObjFunction
{
    FeiObject obj;

    // store number of parameters
    int arity;

    // to track upvalues
    int upvaluecount;

    // to store the function information
    FeiBytecodeList chunk;
    ObjString* name;
};

struct ObjUpvalue
{
    FeiObject obj;

    // pointer to value in the enclosing ObjClosure
    FeiValue* location;

    // to store closed upvalue
    FeiValue closed;

    // intrusive/linked list to track sorted openvalues
    // ordered by the stack slot they point to
    ObjUpvalue* next;
};


struct ObjClosure
{
    FeiObject obj;
    ObjFunction* function;

    // for upvalues
    ObjUpvalue** upvalues;
    int upvaluecount;
};

struct ObjNative
{
    FeiObject obj;
    NativeFn function;
};

struct ObjString
{
    FeiObject obj;
    int length;
    char* chars;
    uint32_t hash;
    int capacity;
};

struct ObjClass
{
    FeiObject obj;
    ObjString* name;
    Table methods;
    Table fieldlike;
};

struct ObjInstance
{
    FeiObject obj;
    ObjClass* classobject;
    Table fields;
};

// struct for class methods
struct ObjBoundMethod
{
    FeiObject obj;
    // wraps receiver and function/method/closure together, receiver is the ObjInstance / lcass type
    FeiValue receiver;
    ObjClosure* method;
};

// the call stack
// keep track where on the stack a function's local begin, where the caller should resume, etc.
// a call frame represents a single ongoing function call
// each time a function is called, create this struct
struct FeiVMFrame
{
    int level;
    int stackpos;
    FeiValue* stackptr;

    ObjClosure* closure;

    // store ip on where in the VM the function is
    uint8_t* ip;

    // this points into the VM's value stack at the first slot the function can use
    FeiValue* slots;
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

    // pointer to the header of the FeiObject itself/node, start of the list
    // nicely used in GARBAGE COLLECTION, where objects are nicely erased in the middle
    FeiObject* objects;
    
    // array of pointers pointing to a particular subgraph
    FeiObject** graystack;
};

struct ASTState
{
    FeiAstLexer scanner;
    FeiAstParser parser;

    //currentclass
    ClassCompiler* classcompiler;

    //current
    FeiAstCompiler* compiler;
};


#if !defined(DEBUG_PRINTTYPES)
    #define DEBUG_PRINTTYPES 0
#endif

struct VMState
{
    int stackindex;

    /*
    * pointer to the element just PAST the element containing the top value of the stack
    * == stackvalues[size(stackvalues) - 2] 
    * stack array is 'indirectly' declared inline here
    * ----
    * NB. i've tried to make stackvalues dynamic, but ancient, arcane pointer arithmetic
    * makes this either
    *    a) very broken
    *    b) very inefficient
    *    c) very memory intensive
    * pick your poison. for now, just leave as-is.
    * if, however, you feel like changing it:
    * stacktop gets assigned from 'slots' of the current FeiVMFrame.
    * no idea what exactly that does, tbh.
    * have fun.
    */
    FeiValue stackvalues[CFG_MAX_VMSTACK];
    FeiValue* stacktop;

    // for storing global variables
    Table globals;

    // for string interning, to make sure every equal string takes one memory
    Table strings;

    // init string for class constructors
    ObjString* initstring;

    // track all upvalues; points to the first node of the linked list
    ObjUpvalue* openupvalues;

    FeiVMFrame* topframe;

    // stores current height of the stack
    int framecount;

    // since the whole program is one big 'main()' use callstacks
    FeiVMFrame** frameobjects;
};



struct FeiState
{
    /* a Writer instance that writes to standard output */
    Writer* iowriter_stdout;

    /* a Writer instance that writes to standard error */
    Writer* iowriter_stderr;

    ObjClass* objstringclass;
    ObjClass* objnumberclass;

    /* the virtualmachine state */
    VMState vmstate;

    /* the parser state */
    ASTState aststate;

    /* the memory (garbage collector, et al) state */
    GCState gcstate;

    struct
    {
        int64_t cntstring;
        int64_t cntfunction;
        int64_t cntclass;
        int64_t cntinstance;
        int64_t cntbound;
        int64_t cntupval;
        int64_t cntclosure;
        int64_t cntnative;
    } ocount;
};

/* array.c */
void fei_valarray_init(FeiState *state, ValArray *array);
size_t fei_valarray_count(ValArray *arr);
FeiValue fei_valarray_get(FeiState *state, ValArray *arr, int idx);
void fei_valarray_push(FeiState *state, ValArray *array, FeiValue value);
void fei_valarray_destroy(FeiState *state, ValArray *array);
/* class.c */
bool fei_class_invokemethod(FeiState *state, ObjClass *klassobj, ObjString *name, int argcount);
bool fei_class_bindmethod(FeiState *state, ObjClass *klassobj, ObjString *name, FeiValue val, bool isfield, bool force);
bool fei_class_setmethod(FeiState *state, ObjClass *klass, ObjString *name, FeiValue method);
void fei_class_inherit(FeiState *state, ObjClass *base, ObjClass *inheritme);
bool fei_class_defmethod(FeiState *state, ObjClass *klassobj, const char *strname, NativeFn fn, bool isfield);
ObjClass *fei_object_makeclass(FeiState *state, ObjString *name);
ObjClass *fei_object_makeclass_str(FeiState *state, const char *name);
ObjInstance *fei_object_makeinstance(FeiState *state, ObjClass *klassobj);
/* compiler.c */
void fei_chunk_init(FeiState *state, FeiBytecodeList *chunk);
void fei_chunk_pushbyte(FeiState *state, FeiBytecodeList *chunk, uint8_t byte, int line);
void fei_chunk_destroy(FeiState *state, FeiBytecodeList *chunk);
int fei_chunk_pushconst(FeiState *state, FeiBytecodeList *chunk, FeiValue value);
void fei_lexer_initsource(FeiState *state, const char *source, size_t len);
bool fei_lexutil_isalpha(FeiState *state, char c);
bool fei_lexutil_isdigit(FeiState *state, char c);
bool fei_lexer_isatend(FeiState *state);
char fei_lexer_advance(FeiState *state);
bool fei_lexer_match(FeiState *state, char expected);
FeiAstToken fei_lexer_maketoken(FeiState *state, FeiAstTokType type);
FeiAstToken fei_lexer_errortoken(FeiState *state, const char *message);
char fei_lexer_peekcurrent(FeiState *state);
char fei_lexer_peeknext(FeiState *state);
void fei_lexer_skipspace(FeiState *state);
FeiAstTokType fei_lexer_checkkw(FeiState *state, int start, int length, const char *rest, FeiAstTokType type);
FeiAstTokType fei_lexer_scantype(FeiState *state);
FeiAstToken fei_lexer_scanident(FeiState *state);
FeiAstToken fei_lexer_scannumber(FeiState *state);
FeiAstToken fei_lexer_scanstring(FeiState *state);
FeiAstToken fei_lexer_scantoken(FeiState *state);
FeiBytecodeList *fei_compiler_currentchunk(FeiState *state);
void fei_compiler_raiseat(FeiState *state, FeiAstToken *token, const char *message);
void fei_compiler_raiseerror(FeiState *state, const char *message);
void fei_compiler_raisehere(FeiState *state, const char *message);
void fei_compiler_advancenext(FeiState *state);
void fei_compiler_advanceskipping(FeiState *state, FeiAstTokType type);
void fei_compiler_consume(FeiState *state, FeiAstTokType type, const char *message);
bool fei_compiler_check(FeiState *state, FeiAstTokType type);
bool fei_compiler_match(FeiState *state, FeiAstTokType type);
void fei_compiler_emitbyte(FeiState *state, uint8_t byte);
void fei_compiler_emitbytes(FeiState *state, uint8_t byte1, uint8_t byte2);
void fei_compiler_emitloop(FeiState *state, int loopstart);
void fei_compiler_emitcondloop(FeiState *state, int loopstart, bool condstate);
int fei_compiler_emitjump(FeiState *state, uint8_t instruction);
void fei_compiler_emitreturn(FeiState *state);
uint8_t fei_compiler_makeconst(FeiState *state, FeiValue value);
void fei_compiler_emitconst(FeiState *state, FeiValue value);
void fei_compiler_patchjump(FeiState *state, int offset);
void fei_compiler_init(FeiState *state, FeiAstCompiler *compiler, FuncType type);
ObjFunction *fei_compiler_endcompiler(FeiState *state);
void fei_compiler_beginscope(FeiState *state);
void fei_compiler_endscope(FeiState *state);
void fei_compiler_beginloopscope(FeiState *state);
void fei_compiler_endloopscope(FeiState *state);
void fei_compiler_markcontinuejump(FeiState *state);
void fei_compiler_patchbreakjumps(FeiState *state);
uint8_t fei_compiler_makeidentconst(FeiState *state, FeiAstToken *name);
bool fei_compiler_identsequal(FeiState *state, FeiAstToken *a, FeiAstToken *b);
int fei_compiler_resolvelocal(FeiState *state, FeiAstCompiler *compiler, FeiAstToken *name);
int fei_compiler_addupvalue(FeiState *state, FeiAstCompiler *compiler, uint8_t index, bool islocal);
int fei_compiler_resolveupvalue(FeiState *state, FeiAstCompiler *compiler, FeiAstToken *name);
void fei_compiler_addlocal(FeiState *state, FeiAstToken name);
void fei_compiler_declvarfromcurrent(FeiState *state);
uint8_t fei_compiler_parsevarfromcurrent(FeiState *state, const char *errormessage);
void fei_compiler_markinit(FeiState *state);
void fei_compiler_defvarindex(FeiState *state, uint8_t global);
uint8_t fei_compiler_parsearglist(FeiState *state);
void fei_compiler_declnamedvar(FeiState *state, FeiAstToken name, bool canassign);
FeiAstToken fei_compiler_makesyntoken(FeiState *state, const char *text);
void fei_compiler_parseprec(FeiState *state, Precedence precedence);
FeiAstRule *fei_compiler_getrule(FeiState *state, FeiAstTokType type);
void fei_compiler_parseexpr(FeiState *state);
void fei_compiler_parseblock(FeiState *state);
void fei_compiler_parsefuncdecl(FeiState *state, FuncType type);
void fei_compiler_parsemethoddecl(FeiState *state);
void fei_compiler_parseclassdecl(FeiState *state);
void fei_compiler_parseclassfuncdecl(FeiState *state);
void fei_compiler_parsevardecl(FeiState *state);
void fei_compiler_parseexprstmt(FeiState *state);
void fei_compiler_parseifstmt(FeiState *state);
void fei_compiler_parseswitchstmt(FeiState *state);
void fei_compiler_parseprintstmt(FeiState *state);
void fei_compiler_parsereturnstmt(FeiState *state);
void fei_compiler_parseforstmt(FeiState *state);
void fei_compiler_parsewhilestmt(FeiState *state);
void fei_compiler_parsebreakstmt(FeiState *state);
void fei_compiler_parsecontinuestmt(FeiState *state);
void fei_compiler_parserepeatuntilstmt(FeiState *state);
void fei_compiler_parsedowhilestmt(FeiState *state);
void fei_compiler_synchronize(FeiState *state);
void fei_compiler_parsedeclaration(FeiState *state);
void fei_compiler_parsestatement(FeiState *state);
ObjFunction *fei_compiler_compilesource(FeiState *state, const char *source, size_t len);
void fei_compiler_markroots(FeiState *state);
/* corelib.c */
void fei_state_setupglobals(FeiState *state);
void fei_state_setupstring(FeiState *state);
void fei_state_setupnumber(FeiState *state);
/* debug.c */
int fei_dbgutil_printsimpleir(FeiState *state, const char *name, int offset);
int fei_dbgutil_printbyteir(FeiState *state, const char *name, FeiBytecodeList *chunk, int offset);
int fei_dbgutil_printconstir(FeiState *state, const char *name, FeiBytecodeList *chunk, int offset);
int fei_dbgutil_printinvokeir(FeiState *state, const char *name, FeiBytecodeList *chunk, int offset);
int fei_dbgutil_printjumpir(FeiState *state, const char *name, int sign, FeiBytecodeList *chunk, int offset);
void fei_dbgdisas_chunk(FeiState *state, FeiBytecodeList *chunk, const char *name);
int fei_dbgdisas_instr(FeiState *state, FeiBytecodeList *chunk, int offset);
/* main.c */
char *readhandle(FILE *hnd, size_t *dlen);
char *readfile(const char *filename, size_t *dlen);
void runfile(FeiState *state, const char *path);
int main(int argc, char *argv[]);
/* mem.c */
void *fei_gcmem_reallocate(FeiState *state, void *pointer, size_t oldsize, size_t newsize);
void fei_gcmem_freeobject(FeiState *state, FeiObject *object);
void fei_gcmem_markobject(FeiState *state, FeiObject *object);
void fei_gcmem_markvalue(FeiState *state, FeiValue value);
void fei_gcmem_markarray(FeiState *state, ValArray *array);
void fei_gcmem_markroots(FeiState *state);
void fei_gcmem_blackenobject(FeiState *state, FeiObject *object);
void fei_gcmem_tracerefs(FeiState *state);
void fei_gcmem_sweep(FeiState *state);
void fei_gcmem_collectgarbage(FeiState *state);
void fei_gcmem_freeobjects(FeiState *state);
/* object.c */
FeiObject *fei_object_allocobject(FeiState *state, size_t size, FeiObjType type);
ObjBoundMethod *fei_object_makeboundmethod(FeiState *state, FeiValue receiver, FeiObject *method);
ObjClosure *fei_object_makeclosure(FeiState *state, ObjFunction *function);
ObjFunction *fei_object_makefunction(FeiState *state);
ObjNative *fei_object_makenativefunc(FeiState *state, NativeFn function);
ObjUpvalue *fei_object_makeupvalue(FeiState *state, FeiValue *slot);
/* state.c */
void fei_vm_raiseruntimeerror(FeiState *state, const char *format, ...);
void fei_vm_defnative(FeiState *state, const char *name, NativeFn function);
void fei_vm_resetstack(FeiState *state);
FeiState *fei_state_init(void);
void fei_state_destroy(FeiState *state);
ResultCode fei_vm_evalsource(FeiState *state, const char *source, size_t len);
/* string.c */
uint32_t fei_string_gethash(FeiState *state, const char *key, int length);
ObjString *fei_string_make(FeiState *state, int length);
bool fei_string_destroy(FeiState *state, ObjString *str);
bool fei_string_appendobj(FeiState *state, ObjString *dest, const ObjString *str2);
bool fei_string_append(FeiState *state, ObjString *dest, const char *strdata, int length);
ObjString *fei_object_allocstring(FeiState *state, const char *chars, int length, uint32_t hash);
ObjString *fei_string_take(FeiState *state, char *chars, int length);
ObjString *fei_string_copy(FeiState *state, const char *chars, int length);
/* table.c */
void fei_table_initcapacity(FeiState *state, Table *table, int cap);
void fei_table_initempty(FeiState *state, Table *table);
void fei_table_initnull(FeiState *state, Table *table);
void fei_table_destroy(FeiState *state, Table *table);
TabEntry *fei_table_findentry(FeiState *state, int count, TabEntry *entries, int capacity, ObjString *key);
bool fei_table_get(FeiState *state, Table *table, ObjString *key, FeiValue *value);
void fei_table_adjustcapacity(FeiState *state, Table *table, int capacity);
bool fei_table_set(FeiState *state, Table *table, ObjString *key, FeiValue value);
bool fei_table_delete(FeiState *state, Table *table, ObjString *key);
void fei_table_mergefrom(FeiState *state, Table *from, Table *to);
ObjString *fei_table_findstring(FeiState *state, Table *table, const char *chars, int length, uint32_t hash);
void fei_table_removeunreachable(FeiState *state, Table *table);
void fei_table_mark(FeiState *state, Table *table);
/* tostring.c */
void fei_value_printfunc(FeiState *state, Writer *wr, ObjFunction *function);
void fei_value_printstring(FeiState *state, Writer *wr, ObjString *ostr, bool withquot);
void fei_value_printvalue(FeiState *state, Writer *wr, FeiValue value, bool withquot);
void fei_value_printobject(FeiState *state, Writer *wr, FeiValue value, bool withquot);
/* value.c */

bool fei_value_asbool(FeiValue v);


int64_t fei_value_asfixednumber(FeiValue v);
double fei_value_asfloatnumber(FeiValue v);

FeiObject *fei_value_asobj(FeiValue v);
FeiObjType OBJ_TYPE(FeiValue v);
bool fei_object_istype(FeiValue value, FeiObjType type);
bool fei_value_isboundmethod(FeiValue v);
bool fei_value_isclass(FeiValue v);
bool fei_value_isfunction(FeiValue v);
bool fei_value_isinstance(FeiValue v);
bool fei_value_isnative(FeiValue v);
bool fei_value_isstring(FeiValue v);
bool fei_value_isclosure(FeiValue v);
bool fei_value_isobject(FeiValue v);
bool fei_value_isfalsey(FeiState *state, FeiValue value);
int fei_value_gettype(FeiValue v);
ObjBoundMethod *fei_value_asbound_method(FeiValue v);
ObjClass *fei_value_asclass(FeiValue v);
ObjInstance *fei_value_asinstance(FeiValue v);
ObjClosure *fei_value_asclosure(FeiValue v);
ObjString *fei_value_asstring(FeiValue v);
char *fei_value_ascstring(FeiValue v);
ObjFunction *fei_value_asfunction(FeiValue v);
NativeFn fei_value_asnative(FeiValue v);
bool fei_value_compare(FeiState *state, FeiValue a, FeiValue b);
/* vm.c */
FeiVMFrame *fei_vm_frameget(FeiState *state, int idx);
void dumpstack(FeiState *state, const char *fmt, ...);
void fei_vm_stackpush(FeiState *state, FeiValue value);
FeiValue fei_vm_stackpop(FeiState *state);
FeiValue fei_vm_stackpeek(FeiState *state, int distance);
bool fei_vm_callclosure(FeiState *state, ObjClosure *closure, int argcount);
bool fei_vm_callvalue(FeiState *state, FeiValue instval, FeiValue callee, int argcount);
ObjUpvalue *fei_vm_captureupvalue(FeiState *state, FeiValue *local);
void fei_vm_closeupvalues(FeiState *state, FeiValue *last);
bool fei_instance_setproperty(FeiState *state, ObjInstance *instance, ObjString *name, FeiValue val);
bool fei_instance_getproperty(FeiState *state, ObjInstance *instance, ObjString *name, FeiValue *dest);
void fei_vm_classdefmethodfromstack(FeiState *state, ObjString *name);
void fei_vmdo_strconcat(FeiState *state);
bool fei_vmdo_return(FeiState *state);
bool fei_vmdo_superinvoke(FeiState *state);
bool fei_vmdo_getsuper(FeiState *state);
bool fei_vmdo_inherit(FeiState *state);
bool fei_vmdo_invokemethod(FeiState *state);
bool fei_vmdo_makeclosure(FeiState *state);
bool fei_vmdo_setproperty(FeiState *state);
bool fei_vmdo_call(FeiState *state);
ObjClass *fei_vm_getclassfor(FeiState *state, int typ);
bool fei_vm_otherproperty(FeiState *state, ObjString *name, int typ, bool asfield);
bool fei_vm_classinvoke(FeiState *state, FeiValue receiver, ObjString *name, int argcount);
bool fei_vm_classinvokefromstack(FeiState *state, ObjString *name, int argcount);
bool fei_vmdo_getproperty(FeiState *state);
bool fei_vmdo_unary(FeiState *state, uint8_t instruc);
bool fei_vmdo_binary(FeiState *state, uint8_t instruc);
bool fei_vmdo_switchequal(FeiState *state);
bool fei_vmdo_compare(FeiState *state);
bool fei_vmdo_logicalnot(FeiState *state);
bool fei_vmdo_defineglobal(FeiState *state);
bool fei_vmdo_setglobal(FeiState *state);
bool fei_vmdo_getglobal(FeiState *state);
bool fei_vmdo_setlocal(FeiState *state);
bool fei_vmdo_getlocal(FeiState *state);
bool fei_vmdo_getconstant(FeiState *state);
bool fei_vmdo_setupvalue(FeiState *state);
bool fei_vmdo_getupvalue(FeiState *state);
bool fei_vmdo_closeupvalue(FeiState *state);
bool fei_vmdo_jumpalways(FeiState *state);
bool fei_vmdo_jumpiffalse(FeiState *state);
bool fei_vmdo_loop(FeiState *state);
bool fei_vmdo_loopiffalse(FeiState *state);
bool fei_vmdo_loopiftrue(FeiState *state);
ResultCode fei_vm_exec(FeiState *state);
/* writer.c */
Writer *fei_writer_init(FeiState *state);
void fei_writer_destroy(FeiState *state, Writer *wr);
Writer *fei_writer_initfile(FeiState *state, FILE *fh, bool alsoclose);
void fei_writer_appendstringlen(Writer *wr, const char *str, size_t len);
void fei_writer_appendchar(Writer *wr, int c);
void fei_writer_appendfmtva(Writer *wr, const char *fmt, va_list va);
void fei_writer_appendfmt(Writer *wr, const char *fmt, ...);


static inline FeiValue fei_value_makebool(bool b)
{
    FeiValue v;
    v.type = VAL_BOOL;
    v.as.valbool = b;
    return v;
}

static inline FeiValue fei_value_makenull()
{
    FeiValue v;
    v.type = VAL_NULL;
    v.as.valfloatnum = 0;
    return v;
}

static inline FeiValue fei_value_makefloatnumber(double dn)
{
    FeiValue v;
    v.type = VAL_NUMBER;
    v.isfixednumber = false;
    v.as.valfloatnum = dn;
    return v;
}


static inline FeiValue fei_value_makefixednumber(int64_t dn)
{
    FeiValue v;
    v.type = VAL_NUMBER;
    v.isfixednumber = true;
    v.as.valfixednum = dn;
    return v;
}


static inline FeiValue fei_value_makeobject_actual(void* obj)
{
    FeiValue v;
    v.type = VAL_OBJ;
    v.as.valobjptr = (FeiObject*)obj;
    return v;
}
