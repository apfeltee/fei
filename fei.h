
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

// diagnostic tools for garbage collector
// 'stress' mode; if this is on, GC runs as often as it possibly can
#define DEBUG_STRESS_GC 0
#define DEBUG_LOG_GC 0

#define fei_value_makeobject(state, object) fei_value_makeobject_actual(state, (void*)(object))

// pass in as a pointer to the object, receives the actual object

// type comparisons
#define fei_value_isbool(value) ((value).type == VAL_BOOL)
#define fei_value_isnull(value) ((value).type == VAL_NULL)
#define fei_value_isnumber(value) ((value).type == VAL_NUMBER)

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
        (fei_value_asfixednumber(v)) : \
        (fei_value_asfloatnumber(v)) \
    )

enum FeiAstTokType
{
    // single character
    TOKEN_OPENPAREN,
    TOKEN_CLOSEPAREN,// ( )
    TOKEN_LEFTBRACE,
    TOKEN_RIGHTBRACE,// { }
    TOKEN_OPENBRACKET,
    TOKEN_CLOSEBRACKET,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    TOKEN_MODULO,
    TOKEN_SHIFTLEFT,
    TOKEN_SHIFTRIGHT,
    TOKEN_BITXOR,
    TOKEN_BITNOT,
    TOKEN_BITOR,
    TOKEN_BITAND,

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
enum FeiAstPrecedence
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

enum FeiFuncType
{
    TYPE_FUNCTION,
    TYPE_SCRIPT,// top level main()
    TYPE_INITIALIZER,// class constructors
    TYPE_METHOD,// class methods
};


enum FeiOpCode
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
    OP_SHIFTLEFT,
    OP_SHIFTRIGHT,
    OP_BITNOT,
    OP_BITXOR,
    OP_BITOR,
    OP_BITAND,

    // logical, unary
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,

    OP_GETINDEX,
    OP_SETINDEX,

    OP_MAKEARRAY,

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
};

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
    OBJ_UPVALUE = 12,
    OBJ_ARRAY = 13,
};

/*
* status code returned by the VM.
*/
enum FeiResultCode
{
    STATUS_OK,
    STATUS_SYNTAXERROR,
    STATUS_RTERROR
};

typedef enum /**/ FeiFuncType FeiFuncType;
typedef enum /**/ FeiOpCode FeiOpCode;
typedef enum /**/ FeiAstPrecedence FeiAstPrecedence;
typedef enum /**/ FeiObjType FeiObjType;
typedef enum /**/ FeiAstTokType FeiAstTokType;
typedef enum /**/ FeiValType FeiValType;
typedef enum /**/ FeiResultCode FeiResultCode;
typedef struct /**/ FeiValue FeiValue;
typedef struct /**/ FeiObject FeiObject;
typedef struct /**/ FeiString FeiString;
typedef struct /**/ FeiObjFunction FeiObjFunction;
typedef struct /**/ FeiClass FeiClass;
typedef struct /**/ FeiInstance FeiInstance;
typedef struct /**/ FeiObjBoundMethod FeiObjBoundMethod;
typedef struct /**/ FeiObjUpvalue FeiObjUpvalue;
typedef struct /**/ FeiObjClosure FeiObjClosure;
typedef struct /**/ FeiObjNative FeiObjNative;
typedef struct /**/ FeiArray FeiArray;
typedef struct /**/ FeiAstLocal FeiAstLocal;
typedef struct /**/ FeiAstUpvalue FeiAstUpvalue;
typedef struct /**/ FeiASTState FeiASTState;
typedef struct /**/ FeiGCState FeiGCState;
typedef struct /**/ FeiVMState FeiVMState;
typedef struct /**/ FeiState FeiState;
typedef struct /**/ FeiVMFrame FeiVMFrame;
typedef struct /**/ FeiAstToken FeiAstToken;
typedef struct /**/ FeiAstLexer FeiAstLexer;
typedef struct /**/ FeiAstParser FeiAstParser;
typedef struct /**/ FeiAstRule FeiAstRule;
typedef struct /**/ FeiAstCompiler FeiAstCompiler;
typedef struct /**/ FeiAstClassCompiler FeiAstClassCompiler;
typedef struct /**/ FeiValArray FeiValArray;
typedef struct /**/ FeiBytecodeList FeiBytecodeList;
typedef struct /**/ FeiValTabEntry FeiValTabEntry;
typedef struct /**/ FeiValTable FeiValTable;
typedef struct /**/ FeiWriter FeiWriter;
typedef struct /**/ FeiPrimitive FeiPrimitive;
typedef struct /**/ FeiConfig FeiConfig;


/*
* callback function type for builtin functions, object functions, etc.
* first argument is the overall state (obviously),
* second argument is an instance *if* this function belongs to some kind of object,
* third argument is the number of arguments passed,
* fourth argument is the C array of argument values.
*/
typedef FeiValue (*FeiNativeFn)(FeiState*, FeiValue, int, FeiValue*);

/*
* used to abstract writing things to output.
* as of now, supports IO only.
*/
struct FeiWriter
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
    // matches the index of the local variable in FeiObjClosure
    int index;
};

/*
* compiles an abstract syntax tree into a stream of bytecode.
*/
struct FeiAstCompiler
{
    // wrapping the whole program into one big main() function
    FeiFuncType progfunctype;
    FeiObjFunction* programfunc;

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
struct FeiAstClassCompiler
{
    // to end scope in superclass declaration
    bool hassuperclass;
    FeiAstToken name;
    FeiAstClassCompiler* enclosing;
};

/*
* a dynamic array holding FeiValue items.
*/
struct FeiValArray
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
    FeiValArray constants;
};

/*
* an entry in a FeiValTable.
*/
struct FeiValTabEntry
{
    /* the key (obviously) */
    FeiString* key;
    /* and the value it is assigned to */
    FeiValue value;
};

struct FeiValTable
{
    int count;
    int capacity;
    FeiValTabEntry* entries;
};


// for functions and calls
struct FeiObjFunction
{
    FeiObject obj;

    // store number of parameters
    int arity;

    // to track upvalues
    int upvaluecount;

    // to store the function information
    FeiBytecodeList chunk;
    FeiString* name;
};

struct FeiObjUpvalue
{
    FeiObject obj;

    // pointer to value in the enclosing FeiObjClosure
    FeiValue* location;

    // to store closed upvalue
    FeiValue closed;

    // intrusive/linked list to track sorted openvalues
    // ordered by the stack slot they point to
    FeiObjUpvalue* next;
};


struct FeiObjClosure
{
    FeiObject obj;
    FeiObjFunction* function;

    // for upvalues
    FeiObjUpvalue** upvalues;
    int upvaluecount;
};

struct FeiObjNative
{
    FeiObject obj;
    FeiNativeFn function;
};

struct FeiString
{
    FeiObject obj;
    int length;
    char* chars;
    uint32_t hash;
    int capacity;
};

struct FeiClass
{
    FeiObject obj;
    FeiString* name;
    FeiValTable methods;
};

struct FeiInstance
{
    FeiObject obj;
    FeiClass* classobject;
    FeiValTable fields;
};

// struct for class methods
struct FeiObjBoundMethod
{
    FeiObject obj;
    // wraps receiver and function/method/closure together, receiver is the FeiInstance / lcass type
    FeiValue receiver;
    FeiObjClosure* method;
};

struct FeiArray
{
    FeiObject obj;
    FeiValArray items;
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

    FeiObjClosure* closure;

    // store ip on where in the VM the function is
    uint8_t* ip;

    // this points into the VM's value stack at the first slot the function can use
    FeiValue* slots;
};

struct FeiGCState
{
    // stack to store gray marked FeiObjects for garbage collection
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

struct FeiASTState
{
    FeiAstLexer scanner;
    FeiAstParser parser;

    //currentclass
    FeiAstClassCompiler* classcompiler;

    //current
    FeiAstCompiler* compiler;
};

struct FeiVMState
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
    FeiValTable globals;

    // for string interning, to make sure every equal string takes one memory
    FeiValTable strings;

    // init string for class constructors
    FeiString* initstring;

    // track all upvalues; points to the first node of the linked list
    FeiObjUpvalue* openupvalues;

    FeiVMFrame* topframe;

    // stores current height of the stack
    int framecount;

    // since the whole program is one big 'main()' use callstacks
    FeiVMFrame** frameobjects;
};

struct FeiConfig
{
    // execution tracing of the VM
    bool traceinstructions;

    bool tracestackvalues;

    bool printcreatedobjcount;
};

/*
* wraps class + instance of a builtin, primitive type,
* such as string, number, function, etc.
* right now, methods are stored in classobj, making instobj pointless,
* but that will be changed. eventually.
*/
struct FeiPrimitive
{
    FeiClass* classobj;
    FeiInstance* instobj;
};

struct FeiState
{
    FeiConfig config;

    /* a FeiWriter instance that writes to standard output */
    FeiWriter* iowriter_stdout;

    /* a FeiWriter instance that writes to standard error */
    FeiWriter* iowriter_stderr;


    FeiPrimitive objstring;
    FeiPrimitive objnumber;
    FeiPrimitive objarray;

    /* the virtualmachine state */
    FeiVMState vmstate;

    /* the parser state */
    FeiASTState aststate;

    /* the memory (garbage collector, et al) state */
    FeiGCState gcstate;

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
        int64_t cntarray;
        int64_t cntnumfixed;
        int64_t cntnumfloat;
    } ocount;
};

/* array.c */
void fei_valarray_init(FeiState *state, FeiValArray *array);
size_t fei_valarray_count(FeiValArray *arr);
FeiValue fei_valarray_get(FeiState *state, FeiValArray *arr, int idx);
void fei_valarray_push(FeiState *state, FeiValArray *array, FeiValue value);
FeiValue fei_valarray_pop(FeiState *state, FeiValArray *array);
void fei_valarray_destroy(FeiState *state, FeiValArray *array);
FeiArray *fei_object_makearray(FeiState *state);
size_t fei_array_count(FeiArray *arr);
bool fei_array_push(FeiState *state, FeiArray *arr, FeiValue val);
FeiValue fei_array_get(FeiState *state, FeiArray *arr, size_t idx);
FeiValue fei_array_pop(FeiState *state, FeiArray *arr);
bool fei_array_destroy(FeiState *state, FeiArray *arr);



/* class.c */
bool fei_class_invokemethod(FeiState *state, FeiClass *klassobj, FeiString *name, int argcount);
bool fei_class_bindmethod(FeiState *state, FeiClass *klassobj, FeiString *name, FeiValue val, bool isfield, bool force);
bool fei_class_setmethod(FeiState *state, FeiClass *klass, FeiString *name, FeiValue method);
void fei_class_inherit(FeiState *state, FeiClass *base, FeiClass *inheritme);
bool fei_class_defmethod(FeiState *state, FeiClass *klassobj, const char *strname, FeiNativeFn fn, bool isfield);
FeiClass *fei_object_makeclass(FeiState *state, FeiString *name);
FeiClass *fei_object_makeclass_str(FeiState *state, const char *name);
FeiInstance *fei_object_makeinstance(FeiState *state, FeiClass *klassobj);

/* compiler.c */
void fei_chunk_init(FeiState *state, FeiBytecodeList *chunk);
void fei_chunk_pushbyte(FeiState *state, FeiBytecodeList *chunk, uint8_t byte, int line);
void fei_chunk_destroy(FeiState *state, FeiBytecodeList *chunk);
int fei_chunk_pushconst(FeiState *state, FeiBytecodeList *chunk, FeiValue value);
void fei_lexer_initsource(FeiState *state, const char *source, size_t len);
bool fei_lexutil_isalpha(FeiState *state, char c);
bool fei_lexutil_isdigit(FeiState *state, char c);
const char *fei_lexer_tokenname(int t);
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
void fei_compiler_raiseatv(FeiState *state, FeiAstToken *token, const char *message, va_list va);
void fei_compiler_raiseat(FeiState *state, FeiAstToken *token, const char *message, ...);
void fei_compiler_raiseerrorv(FeiState *state, const char *fmt, va_list va);
void fei_compiler_raiseerror(FeiState *state, const char *fmt, ...);
void fei_compiler_raiseherev(FeiState *state, const char *fmt, va_list va);
void fei_compiler_raisehere(FeiState *state, const char *fmt, ...);
void fei_compiler_advancenext(FeiState *state);
void fei_compiler_advanceskipping(FeiState *state, FeiAstTokType type);
void fei_compiler_consumev(FeiState *state, FeiAstTokType type, const char* fmt, va_list va);
void fei_compiler_consume(FeiState *state, FeiAstTokType type, const char* fmt, ...);
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
void fei_compiler_init(FeiState *state, FeiAstCompiler *compiler, FeiFuncType type);
FeiObjFunction *fei_compiler_endcompiler(FeiState *state);
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
uint8_t fei_compiler_parsearglist(FeiState* state, const char* contextname, FeiAstTokType tokbegin, FeiAstTokType tokend);
void fei_compiler_declnamedvar(FeiState *state, FeiAstToken name, bool canassign);
FeiAstToken fei_compiler_makesyntoken(FeiState *state, const char *text);
void fei_compiler_parseprec(FeiState *state, FeiAstPrecedence precedence);
FeiAstRule *fei_compiler_getrule(FeiState *state, FeiAstTokType type);
void fei_compiler_parseexpr(FeiState *state);
void fei_compiler_parseblock(FeiState *state);
void fei_compiler_parsefuncdecl(FeiState *state, FeiFuncType type);
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
FeiObjFunction *fei_compiler_compilesource(FeiState *state, const char *source, size_t len);
void fei_compiler_markroots(FeiState *state);

/* corelib.c */
void fei_state_setupglobals(FeiState *state);
void fei_state_setupstring(FeiState *state);
void fei_state_setupnumber(FeiState *state);
void fei_state_setuparray(FeiState* state);

/* debug.c */
int fei_dbgutil_printsimpleir(FeiState *state, const char *name, int offset);
int fei_dbgutil_printbyteir(FeiState *state, const char *name, FeiBytecodeList *chunk, int offset);
int fei_dbgutil_printconstir(FeiState *state, const char *name, FeiBytecodeList *chunk, int offset);
int fei_dbgutil_printinvokeir(FeiState *state, const char *name, FeiBytecodeList *chunk, int offset);
int fei_dbgutil_printjumpir(FeiState *state, const char *name, int sign, FeiBytecodeList *chunk, int offset);
void fei_dbgdisas_chunk(FeiState *state, FeiBytecodeList *chunk, const char *name);
int fei_dbgdisas_instr(FeiState *state, FeiBytecodeList *chunk, int offset);


/* mem.c */
void *fei_gcmem_reallocate(FeiState *state, void *pointer, size_t oldsize, size_t newsize);
void fei_gcmem_freeobject(FeiState *state, FeiObject *object);
void fei_gcmem_markobject(FeiState *state, FeiObject *object);
void fei_gcmem_markvalue(FeiState *state, FeiValue value);
void fei_gcmem_markarray(FeiState *state, FeiValArray *array);
void fei_gcmem_markroots(FeiState *state);
void fei_gcmem_blackenobject(FeiState *state, FeiObject *object);
void fei_gcmem_tracerefs(FeiState *state);
void fei_gcmem_sweep(FeiState *state);
void fei_gcmem_collectgarbage(FeiState *state);
void fei_gcmem_freeobjects(FeiState *state);

/* object.c */
FeiObject *fei_object_allocobject(FeiState *state, size_t size, FeiObjType type);
FeiObjBoundMethod *fei_object_makeboundmethod(FeiState *state, FeiValue receiver, FeiObject *method);
FeiObjClosure *fei_object_makeclosure(FeiState *state, FeiObjFunction *function);
FeiObjFunction *fei_object_makefunction(FeiState *state);
FeiObjNative *fei_object_makenativefunc(FeiState *state, FeiNativeFn function);
FeiObjUpvalue *fei_object_makeupvalue(FeiState *state, FeiValue *slot);


/* state.c */
void fei_vm_raiseruntimeerror(FeiState *state, const char *format, ...);
void fei_vm_defnative(FeiState *state, const char *name, FeiNativeFn function);
void fei_vm_resetstack(FeiState *state);
FeiState *fei_state_init(void);
void fei_state_destroy(FeiState *state);
FeiResultCode fei_vm_evalsource(FeiState *state, const char *source, size_t len);
/* string.c */
uint32_t fei_string_gethash(FeiState *state, const char *key, int length);
FeiString *fei_string_make(FeiState *state, int length);
bool fei_string_destroy(FeiState *state, FeiString *str);
bool fei_string_appendobj(FeiState *state, FeiString *dest, const FeiString *str2);
bool fei_string_append(FeiState *state, FeiString *dest, const char *strdata, int length);
FeiString *fei_object_allocstring(FeiState *state, const char *chars, int length, uint32_t hash);
FeiString *fei_string_take(FeiState *state, char *chars, int length);
FeiString *fei_string_copy(FeiState *state, const char *chars, int length);
/* table.c */
void fei_table_initcapacity(FeiState *state, FeiValTable *table, int cap);
void fei_table_initempty(FeiState *state, FeiValTable *table);
void fei_table_initnull(FeiState *state, FeiValTable *table);
void fei_table_destroy(FeiState *state, FeiValTable *table);
FeiValTabEntry *fei_table_findentry(FeiState *state, int count, FeiValTabEntry *entries, int capacity, FeiString *key);
bool fei_table_get(FeiState *state, FeiValTable *table, FeiString *key, FeiValue *value);
void fei_table_adjustcapacity(FeiState *state, FeiValTable *table, int capacity);
bool fei_table_set(FeiState *state, FeiValTable *table, FeiString *key, FeiValue value);
bool fei_table_delete(FeiState *state, FeiValTable *table, FeiString *key);
void fei_table_mergefrom(FeiState *state, FeiValTable *from, FeiValTable *to);
FeiString *fei_table_findstring(FeiState *state, FeiValTable *table, const char *chars, int length, uint32_t hash);
void fei_table_removeunreachable(FeiState *state, FeiValTable *table);
void fei_table_mark(FeiState *state, FeiValTable *table);
/* tostring.c */
void fei_value_printfunc(FeiState *state, FeiWriter *wr, FeiObjFunction *function);
void fei_value_printstring(FeiState *state, FeiWriter *wr, FeiString *ostr, bool withquot);
void fei_value_printvalue(FeiState *state, FeiWriter *wr, FeiValue value, bool withquot);
void fei_value_printobject(FeiState *state, FeiWriter *wr, FeiValue value, bool withquot);

/* value.c */
const char* fei_value_typename(FeiValue v);
const char* fei_object_typename(FeiObject* v);
bool fei_value_compare(FeiState *state, FeiValue a, FeiValue b);

/* vm.c */
FeiVMFrame *fei_vm_frameget(FeiState *state, int idx);
void dumpstack(FeiState *state, const char *fmt, ...);
void fei_vm_dumpval(FeiState* state, FeiValue val, const char* fmt, ...);
void fei_vm_stackpush(FeiState *state, FeiValue value);
FeiValue fei_vm_stackpop(FeiState *state);
FeiValue fei_vm_stackpeek(FeiState *state, int distance);
bool fei_vm_callclosure(FeiState *state, FeiObjClosure *closure, int argcount);
bool fei_vm_callvalue(FeiState *state, FeiValue instval, FeiValue callee, int argcount);
FeiObjUpvalue *fei_vm_captureupvalue(FeiState *state, FeiValue *local);
void fei_vm_closeupvalues(FeiState *state, FeiValue *last);
bool fei_instance_setproperty(FeiState *state, FeiInstance *instance, FeiString *name, FeiValue val);
bool fei_instance_getproperty(FeiState *state, FeiInstance *instance, FeiString *name, FeiValue *dest);
void fei_vm_classdefmethodfromstack(FeiState *state, FeiString *name);
bool fei_vmdo_strconcat(FeiState *state);
bool fei_vmdo_return(FeiState *state);
bool fei_vmdo_superinvoke(FeiState *state);
bool fei_vmdo_getsuper(FeiState *state);
bool fei_vmdo_inherit(FeiState *state);
bool fei_vmdo_invokemethod(FeiState *state);
bool fei_vmdo_makeclosure(FeiState *state);
bool fei_vmdo_setproperty(FeiState *state);
bool fei_vmdo_call(FeiState *state);
FeiInstance *fei_vm_getinstancefor(FeiState *state, int typ);
bool fei_vm_otherproperty(FeiState *state, FeiString *name, int typ, bool asfield);
bool fei_vm_classinvoke(FeiState *state, FeiValue receiver, FeiString *name, int argcount);
bool fei_vm_classinvokefromstack(FeiState *state, FeiString *name, int argcount);
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
FeiResultCode fei_vm_exec(FeiState *state);
/* writer.c */
FeiWriter *fei_writer_init(FeiState *state);
void fei_writer_destroy(FeiState *state, FeiWriter *wr);
FeiWriter *fei_writer_initfile(FeiState *state, FILE *fh, bool alsoclose);
void fei_writer_appendstringlen(FeiWriter *wr, const char *str, size_t len);
void fei_writer_appendchar(FeiWriter *wr, int c);
void fei_writer_appendfmtva(FeiWriter *wr, const char *fmt, va_list va);
void fei_writer_appendfmt(FeiWriter *wr, const char *fmt, ...);
void fei_writer_appendquotedstring(FeiWriter* wr, const char* str, size_t len, bool withquot);

static inline FeiValue fei_value_makebool(FeiState* state, bool b)
{
    FeiValue v;
    (void)state;
    v.type = VAL_BOOL;
    v.as.valbool = b;
    return v;
}

static inline FeiValue fei_value_makenull(FeiState* state)
{
    FeiValue v;
    (void)state;
    v.type = VAL_NULL;
    v.isfixednumber = true; 
    v.as.valfixednum = 0;
    return v;
}

static inline FeiValue fei_value_makefloatnumber(FeiState* state, double dn)
{
    FeiValue v;
    state->ocount.cntnumfloat++;
    v.type = VAL_NUMBER;
    v.isfixednumber = false;
    v.as.valfloatnum = dn;
    return v;
}


static inline FeiValue fei_value_makefixednumber(FeiState* state, int64_t dn)
{
    FeiValue v;
    state->ocount.cntnumfixed++;
    v.type = VAL_NUMBER;
    v.isfixednumber = true;
    v.as.valfixednum = dn;
    return v;
}


static inline FeiValue fei_value_makeobject_actual(FeiState* state, void* obj)
{
    FeiValue v;
    (void)state;
    v.type = VAL_OBJ;
    v.as.valobjptr = (FeiObject*)obj;
    return v;
}


static inline bool fei_value_asbool(FeiValue v)
{
    return v.as.valbool;
}

static inline double fei_value_asfloatnumber(FeiValue v)
{
    if(v.type != VAL_NUMBER)
    {
        return 0;
    }
    #if 1
    if(v.isfixednumber)
    {
        return v.as.valfixednum;
    }
    #endif
    return v.as.valfloatnum;
}

static inline int64_t fei_value_asfixednumber(FeiValue v)
{
    if(v.type != VAL_NUMBER)
    {
        return 0;
    }
    #if 1
    if(!v.isfixednumber)
    {
        #if 0
        if(isnan(v.as.valfloatnum))
        {
            return -1;
        }
        #endif
        return v.as.valfloatnum;
    }
    #endif
    return v.as.valfixednum;
}

static inline FeiObject* fei_value_asobject(FeiValue v)
{
    return v.as.valobjptr;
}

static inline FeiObjType fei_value_objtype(FeiValue v)
{
    return fei_value_asobject(v)->type;
}

static inline bool fei_value_isobject(FeiValue v)
{
    return v.type == VAL_OBJ;
}

static inline bool fei_object_istype(FeiValue value, FeiObjType type)
{
    return fei_value_isobject(value) && fei_value_asobject(value)->type == type;
}

static inline bool fei_value_isboundmethod(FeiValue v)
{
    return fei_object_istype(v, OBJ_BOUND_METHOD);
}

static inline bool fei_value_isclass(FeiValue v)
{
    return fei_object_istype(v, OBJ_CLASS);
}

static inline bool fei_value_isfunction(FeiValue v)
{
    return fei_object_istype(v, OBJ_FUNCTION);
}

static inline bool fei_value_isinstance(FeiValue v)
{
    return (
        fei_object_istype(v, OBJ_INSTANCE)
    );
}

static inline bool fei_value_isnative(FeiValue v)
{
    return fei_object_istype(v, OBJ_NATIVE);
}

static inline bool fei_value_isstring(FeiValue v)
{
    return fei_object_istype(v, OBJ_STRING);
}

static inline bool fei_value_isclosure(FeiValue v)
{
    return fei_object_istype(v, OBJ_CLOSURE);
}


static inline bool fei_value_isarray(FeiValue v)
{
    return fei_object_istype(v, OBJ_ARRAY);
}

static inline bool fei_value_numberisnull(FeiState* state, FeiValue val)
{
    (void)state;
    if(val.type == VAL_NUMBER)
    {
        if(val.isfixednumber)
        {
            return fei_value_asfixednumber(val) == 0;
        }
        return fei_value_asfloatnumber(val) == 0.0;
    }
    return false;
}

static inline bool fei_value_isfalsey(FeiState* state, FeiValue value)
{
    (void)state;
    if(fei_value_isnull(value))
    {
        return true;
    }
    if(fei_value_numberisnull(state, value))
    {
        return true;
    }
    if(fei_value_isbool(value))
    {
        return !fei_value_asbool(value);
    }
    return false;
}

static inline int fei_value_gettype(FeiValue v)
{
    if(fei_value_isobject(v))
    {
        return v.as.valobjptr->type;
    }
    return v.type;
}

static inline FeiArray* fei_value_asarray(FeiValue v)
{
    return (FeiArray*)fei_value_asobject(v);
}

static inline FeiObjBoundMethod* fei_value_asbound_method(FeiValue v)
{
    return (FeiObjBoundMethod*)fei_value_asobject(v);
}

static inline FeiClass* fei_value_asclass(FeiValue v)
{
    return (FeiClass*)fei_value_asobject(v);
}

static inline FeiInstance* fei_value_asinstance(FeiValue v)
{
    return (FeiInstance*)fei_value_asobject(v);
}

static inline FeiObjClosure* fei_value_asclosure(FeiValue v)
{
    return (FeiObjClosure*)fei_value_asobject(v);
}

static inline FeiString* fei_value_asstring(FeiValue v)
{
    return (FeiString*)fei_value_asobject(v);
}

static inline char* fei_value_ascstring(FeiValue v)
{
    return fei_value_asstring(v)->chars;
}

static inline FeiObjFunction* fei_value_asfunction(FeiValue v)
{
    return (FeiObjFunction*)fei_value_asobject(v);
}

static inline FeiNativeFn fei_value_asnative(FeiValue v)
{
    return ((FeiObjNative*)fei_value_asobject(v))->function;
}
