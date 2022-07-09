

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
#define ALLOCATE_OBJ(type, objecttype) (type*)mem_allocobject(vm, sizeof(type), objecttype)

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
#define obj_isbool(value) ((value).type == VAL_BOOL)
#define obj_isnull(value) ((value).type == VAL_NULL)
#define obj_isnumber(value) ((value).type == VAL_NUMBER)
#define obj_isobject(value) ((value).type == VAL_OBJ)


// from VALUE STRUCT to RAW  C nicely used in printing
// also for comparisons -> use values ALREADY CONVERTED to Value struct union to raw C
#define obj_asbool(value) ((value).as.boolean)
#define obj_asnumber(value) ((value).as.number)
#define obj_asobject(value) ((value).as.obj)


// macros for conversions from code type to struct Value union type
// pass Value struct to the macro
/*	IMPORTANT: macro syntax
#define macroname(parameter) (returntype)
-> here, return a Value type. initializing it inside the macro
-> . means as
IMPORTANT = these macros give a 'tag' to each respective values
*/
#define obj_mkbool(value) ((Value){ VAL_BOOL, { .boolean = value } })
#define obj_nullval ((Value){ VAL_NULL, { .number = 0 } })
#define obj_mknumber(value) ((Value){ VAL_NUMBER, { .number = value } })
// pass in as a pointer to the object, receives the actual object
#define obj_mkobject(object) ((Value){ VAL_OBJ, { .obj = (Obj*)object } })


#define obj_type(value) (obj_asobject(value)->type)

// macros for checking(bool) whether an object is a certain type
#define obj_isboundmethod(value) obj_istype(value, OBJ_BOUND_METHOD)
#define obj_isclass(value) obj_istype(value, OBJ_CLASS)
#define obj_isfunction(value) obj_istype(value, OBJ_FUNCTION)
#define obj_isinstance(value) obj_istype(value, OBJ_INSTANCE)
#define obj_isnative(value) obj_istype(value, OBJ_NATIVE)
// takes in raw Value, not raw Obj*
#define obj_isstring(value) obj_istype(value, OBJ_STRING)
#define obj_isclosure(value) obj_istype(value, OBJ_CLOSURE)

// macros to tell that it is safe when creating a tag, by returning the requested type
// take a Value that is expected to conatin a pointer to the heap, first returns pointer second the charray itself
// used to cast as an ObjType pointer, from a Value type
#define obj_asboundmethod(value) ((ObjBoundMethod*)obj_asobject(value))
#define obj_asclass(value) ((ObjClass*)obj_asobject(value))
#define obj_asinstance(value) ((ObjInstance*)obj_asobject(value))
#define obj_asclosure(value) ((ObjClosure*)obj_asobject(value))
#define obj_asstring(value) ((ObjString*)obj_asobject(value))
#define obj_asfunction(value) ((ObjFunction*)obj_asobject(value))
#define obj_asnative(value) (((ObjNative*)obj_asobject(value))->function)


// macro to allocate memory, usedin obj/heap
// use mem_realloc as malloc here; start from null pointer, old size is 0, and new size is count
#define memwrap_allocate(vm, type, count) (type*)mem_realloc(vm, NULL, 0, sizeof(type) * (count))


// free memory, pass in new size as 0 to free
#define memwrap_free(vm, type, pointer) mem_realloc(vm, pointer, sizeof(type), 0)


// C macros
// calculates a new capacity based on a given current capacity, it should SCALE based on the old one
// this one grows by * 2
// capacity becomes 8 for the first time(starts from 0), later it multiplies by 2
#define memwrap_growcap(vm, capacity) ((capacity) < 8 ? 8 : (capacity)*2)

// macro to grow array
// make own mem_realloc function
// basically declare our return type here with (type*)
#define memwrap_growarray(vm, type, pointer, oldcount, newcount) (type*)mem_realloc(vm, pointer, sizeof(type) * (oldcount), sizeof(type) * (newcount))

// no (type*) because function does not return a type
// 0 is the new capacity
// used to free eg. char arrays
#define memwrap_freearray(vm, type, pointer, oldcount) mem_realloc(vm, pointer, sizeof(type) * (oldcount), 0)

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
    obj_asstring(vmc_readconst())

// for patch jumps
// yanks next two bytes from the chunk(used to calculate the offset earlier) and return a 16-bit integer out of it
// use bitwise OR
#define vmc_readshort() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

// MACRO for binary operations
// take two last constants, and vm_push ONE final value doing the operations on both of them
// this macro needs to expand to a series of statements, read a-virtual-machine for more info, this is a macro trick or a SCOPE BLOCK
// pass in an OPERAOTR as a MACRO
// valuetype is a Value struct
// first check that both operands are numbers
#define vmc_binaryop(valuetype, op, downcasttype)           \
    do                                                   \
    {                                                    \
        if(!obj_isnumber(vm_peek(vm, 0)) || !obj_isnumber(vm_peek(vm, 1)))   \
        {                                                \
            vm_rterror(vm, "Operands must be numbers.");   \
            return STATUS_RUNTIMEFAIL;              \
        }                                                \
        downcasttype b = (downcasttype)obj_asnumber(vm_pop(vm)); \
        downcasttype a = (downcasttype)obj_asnumber(vm_pop(vm)); \
        vm_push(vm, valuetype(a op b));                         \
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
    // top level main()
    TYPE_SCRIPT,
    // class constructors
    TYPE_INITIALIZER,
    // class methods
    TYPE_METHOD,
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
    // for bigger instances such as strings, functions, heap-allocated; the payload is a heap pointer
    VAL_OBJ,
};
// in bytecode format, each instruction has a one-byte operation code(opcode)
// the number controls what kind of instruction we're dealing with- add, subtract, etc
// typedef enums are bytes apparently
// these are INSTRUCTIONS

enum OpCode
{
    // chunk needs to know when to produce constants and print them in the right order
    // they have operands, to eg. identify which variable to load
    // OP_CONSTANT take up 2 bytes, one is the opcode itself and the other the constant index
    OP_CONSTANT,

    OP_NULL,
    OP_TRUE,
    OP_FALSE,

    // unary operators
    OP_NEGATE,// operand to negate, utilized in virtual machine

    // literals/declarations
    OP_POP,// basically pops a value off the stack and forgets it, used for expression statements
    OP_GETLOCAL,
    OP_SETLOCAL,
    OP_DEFINEGLOBAL,
    OP_GETGLOBAL,
    OP_SETGLOBAL,
    OP_GET_UPVALUE,
    OP_SETUPVALUE,
    OP_GETPROPERTY,
    OP_SETPROPERTY,

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

    OP_SWITCHEQUAL,
    OP_CLOSEUPVALUE,

    OP_JUMP,
    OP_JUMPIFFALSE,// takes a 16-bit operand
    OP_CALL,

    OP_LOOP,
    OP_LOOPIFFALSE,// repeat until
    OP_LOOPIFTRUE,// do while

    OP_CLOSURE,
    OP_CLASS,
    OP_METHOD,
    OP_INVOKE,

    OP_INHERIT,// class inheritance
    OP_GETSUPER,// for superclasses
    OP_SUPERINVOKE,

    OP_RETURN,// means return from current function
};

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
    STATUS_OK,
    STATUS_COMPILEFAIL,
    STATUS_RUNTIMEFAIL
};

enum TokenType
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
    TOKEN_BANG,
    TOKEN_BANGEQUAL,// !, !=
    TOKEN_EQUAL,
    TOKEN_EQUALEQUAL,
    TOKEN_GREATERTHAN,
    TOKEN_GREATEREQUAL,
    TOKEN_LESSTHAN,
    TOKEN_LESSEQUAL,

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
typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef struct Token Token;
typedef struct CallFrame CallFrame;
typedef struct ObjBoundMethod ObjBoundMethod;
typedef struct ObjInstance ObjInstance;
typedef struct ObjClass ObjClass;
typedef struct ObjNative ObjNative;
typedef struct ObjClosure ObjClosure;
typedef struct ObjUpvalue ObjUpvalue;
typedef struct ObjFunction ObjFunction;
typedef struct Table Table;
typedef struct Entry Entry;
typedef struct Chunk Chunk;
typedef struct ValueArray ValueArray;
typedef struct Value Value;
typedef struct VM VM;

typedef Value (*NativeFn)(VM*, int, Value*);

// acts like a "virtual" function , a void function that cam be overidden; actually a void but override it with ParseFn
typedef void (*ParseFn)(VM* vm, bool canassign);

/* IMPORTANT 
-> use C unions to OVERLAP in memory for the STRUCT
->  size of the union is its LARGEST FIELD
-> unions are like structs but they only allocate memory for the LARGEST FIELD
*/
struct Value
{
    ValueType type;
    union
    {
        bool boolean;
        double number;
        // pointer to the heap, the payload for bigger types of data
        Obj* obj;
    } as;
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
    // current size
    int count;
    // max array size
    int capacity;
    // 1 byte unsigned int, to store the CODESTREAM
    uint8_t* code;
    // array of integers that parallels the bytecode/codestream, to get where each location of the bytecode is
    int* lines;
    // store double value literals
    ValueArray constants;
};


struct Entry
{
    // use ObjString pointer as key
    ObjString* key;
    // the value/data type
    Value value;
};

// the table, an array of entries

struct Table
{
    int count;
    int capacity;
    Entry* entries;
};


struct Obj
{
    ObjType type;
    // linked list or intrusive list, to avoid memory leaks, obj itself as a node
    
    // traverse the list to find every object that has been allocated on the heap
    struct Obj* next;
    // for mark-sweep garbage collection
    bool ismarked;
};

// for functions and calls

struct ObjFunction
{
    Obj obj;
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
    Obj obj;
    // pointer to value in the enclosing ObjClosure
    Value* location;

    // to store closed upvalue
    Value closed;

    // intrusive/linked list to track sorted openvalues
    // ordered by the stack slot they point to
    struct ObjUpvalue* next;
};

// for closures

struct ObjClosure
{
    // points to an ObjFunction and Obj header
    // Obj header
    Obj obj;
    ObjFunction* function;

    // for upvalues
    // array of upvalue pointers
    ObjUpvalue** upvalues;
    int upvaluecount;
};


/*  NATIVE FUNCTIONS(file systems, user input etc.)
-> native functions reference a vm_call to native C code insted of bytecode */

struct ObjNative
{
    Obj obj;
    NativeFn function;
};


struct ObjString
{
    Obj obj;
    int length;
    char* chars;
    // for hash table, for cache(temporary storage area); each ObjString has a hash code for itself
    uint32_t hash;
};


// class object type

struct ObjClass
{
    Obj obj;
    // not needed for uer's program, but helps the dev in debugging
    ObjString* name;
    // hash table for storing methods
    Table methods;
};


struct ObjInstance
{
    // inherits from object, the "object" tag
    Obj obj;
    // pointer to class types
    ObjClass* classobject;
    // use a hash table to store fields
    Table fields;
};


// struct for class methods
struct ObjBoundMethod
{
    Obj obj;
    // wraps receiver and function/method/closure together, receiver is the ObjInstance / lcass type
    Value receiver;
    ObjClosure* method;
};


// the vm_call stack
// keep track where on the stack a function's local begin, where the caller should resume, etc.
// a vm_call frame represents a single ongoing function vm_call
// each time a function is called, create this struct
struct CallFrame
{
    ObjClosure* closure;
    // store ip on where in the VM the function is
    uint8_t* ip;
    // this points into the VM's value stack at the first slot the function can use
    Value* slots;
};
struct VM
{
    // since the whole program is one big 'main()' use callstacks
    CallFrame frames[FRAMES_MAX];
    // stores current height of the stack
    int framecount;

    // stack array is 'indirectly' declared inline here
    Value stack[STACK_MAX];
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

    // pointer to the header of the Obj itself/node, start of the list
    // nicely used in GARBAGE COLLECTION, where objects are nicely erased in the middle
    Obj* objects;

    // stack to store gray marked Objects for garbage collection
    int graycap;
    int graycount;
    // array of pointers pointing to a particular subgraph
    Obj** graystack;

    // self-adjusting-g-heap, to control frequency of GC, totalalloc is the running total
    size_t totalalloc;
    // threhsold that triggers the GC
    size_t nextgc;
};


struct Token
{
    // identifier to type of token, eg. number, + operator, identifier
    TokenType type;
    const char* start;
    int length;
    int line;
};

struct Scanner
{
    // marks the beginning of the current lexeme('word', you can say_
    const char* start;
    // points to the character being looked at
    const char* current;
    // int to tell the current line being looked at
    int line;
};

struct Parser
{
    Token current;
    Token previous;
    // flag to tell whether the code has a syntax error or no
    bool haderror;
    // flag for error cascades/multiple errors so the parser does not get confused, only returns the first
    bool panicmode;
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
    bool islocal;
    // matches the index of the local variable in ObjClosure
    int index;
};

// stack for local variables
struct Compiler
{
    // pointer to the 'outer'/enclosing compiler, to return to after function
    Compiler* enclosing;

    // wrapping the whole program into one big main() function
    ObjFunction* function;
    FunctionType type;

    // array to store locals, ordered in the order of declarations
    Local locals[UINT8_COUNT];
    // tracks amount of locals in a scope
    int localcount;
    Upvalue upvalues[UINT8_COUNT];
    // number of scopes/blocks surrounding the code
    int scopedepth;

    // for loop breaks and continues, loop enclosing
    int loopcounttop;
    int* continuejumps;
    // only for continue jumpbs
    int continuejumpcap;

    // for patching all break statements
    int breakpatchjumps[UINT8_COUNT][UINT8_COUNT];
    int breakjumpcounts[UINT8_COUNT];

};

// for 'this' tokens, a main() for class
struct ClassCompiler
{
    ClassCompiler* enclosing;
    Token name;
    // to end scope in superclass declaration
    bool hassuper;
};

Scanner g_currentscanner;
Parser g_currentparser;
ClassCompiler* g_currentclass = NULL;
Compiler* g_currentcompiler = NULL;

/* main.c */

/* main.c */
void repl(VM *vm);
char *readfile(VM *vm, const char *path);
void htable_init(VM *vm, Table *table);
void htable_free(VM *vm, Table *table);
Entry *htable_findentry(Entry *entries, int capacity, ObjString *key);
bool htable_get(VM *vm, Table *table, ObjString *key, Value *value);
void htable_adjustcap(VM *vm, Table *table, int capacity);
bool htable_set(VM *vm, Table *table, ObjString *key, Value value);
bool htable_delete(VM *vm, Table *table, ObjString *key);
void htable_addall(VM *vm, Table *from, Table *to);
ObjString *htable_findstring(VM *vm, Table *table, const char *chars, int length, uint32_t hash);
void htable_removewhite(VM *vm, Table *table);
void htable_mark(VM *vm, Table *table);
void chunk_init(VM *vm, Chunk *chunk);
void chunk_write(VM *vm, Chunk *chunk, uint8_t byte, int line);
void chunk_free(VM *vm, Chunk *chunk);
int chunk_addconstant(VM *vm, Chunk *chunk, Value value);
void scanner_init(const char *source);
bool scanner_isalpha(char c);
bool scanner_isdigit(char c);
bool scanner_isatend();
char scanner_advance();
bool scanner_match(char expected);
Token scanner_maketoken(TokenType type);
Token scanner_errortoken(const char *message);
char scanner_peek();
char scanner_peeknext();
void scanner_skipspace();
TokenType scanner_checkkeyword(int start, int length, const char *rest, TokenType type);
TokenType scanner_parseidenttype();
Token scanner_parseident();
Token scanner_parsenumber();
Token scanner_parsestring();
Token scanner_scantoken();
void prs_parseprecedence(VM *vm, Precedence precedence);
ParseRule *prs_getrule(VM *vm, TokenType type);
void prs_expression(VM *vm);
void prs_block(VM *vm);
void prs_function(VM *vm, FunctionType type);
void prs_parsemethod(VM *vm);
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
bool prs_check(VM *vm, TokenType type);
bool prs_matchtoken(VM *vm, TokenType type);
void prs_emitbyte(VM *vm, uint8_t byte);
void prs_emitbytes(VM *vm, uint8_t byte1, uint8_t byte2);
void prs_emitloop(VM *vm, int loopstart);
void prs_emitcondloop(VM *vm, int loopstart, bool state);
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
bool prs_identequal(VM *vm, Token *a, Token *b);
int prs_resolvelocal(VM *vm, Compiler *compiler, Token *name);
int prs_addupvalue(VM *vm, Compiler *compiler, uint8_t index, bool isl);
int prs_resolveupvalue(VM *vm, Compiler *compiler, Token *name);
void prs_addlocal(VM *vm, Token name);
void prs_declarevariable(VM *vm);
uint8_t prs_parsevariable(VM *vm, const char *errormessage);
void prs_markinitialized(VM *vm);
void prs_definevariable(VM *vm, uint8_t global);
uint8_t prs_parsearglist(VM *vm);
void rule_and(VM *vm, bool canassign);
void rule_binary(VM *vm, bool canassign);
void rule_parsecall(VM *vm, bool canassign);
void rule_dot(VM *vm, bool canassign);
void rule_literal(VM *vm, bool canassign);
void rule_grouping(VM *vm, bool canassign);
void rule_number(VM *vm, bool canassign);
void rule_or(VM *vm, bool canassign);
void rule_string(VM *vm, bool canassign);
void rule_namedvar(VM *vm, Token name, bool canassign);
void rule_variable(VM *vm, bool canassign);
Token prs_makesyntoken(VM *vm, const char *text);
void rule_super(VM *vm, bool canassign);
void rule_this(VM *vm, bool canassign);
void rule_unary(VM *vm, bool canassign);
void runfile(VM *vm, const char *path);
int main(int argc, const char *argv[]);
int dbg_print_simpleinst(VM *vm, const char *name, int offset);
int dbg_print_byteinst(VM *vm, const char *name, Chunk *chunk, int offset);
int dbg_print_constinst(VM *vm, const char *name, Chunk *chunk, int offset);
int dbg_print_invokeinst(VM *vm, const char *name, Chunk *chunk, int offset);
int dbg_print_jumpinst(VM *vm, const char *name, int sign, Chunk *chunk, int offset);
void dbg_disasmchunk(VM *vm, Chunk *chunk, const char *name);
int dbg_disasminst(VM *vm, Chunk *chunk, int offset);
void *mem_realloc(VM *vm, void *pointer, size_t oldsize, size_t newsize);
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
bool obj_istype(Value value, ObjType type);
ObjBoundMethod *obj_mkboundmethod(VM *vm, Value receiver, ObjClosure *method);
ObjClosure *obj_mkclosure(VM *vm, ObjFunction *function);
ObjString *obj_mkstring(VM *vm, char *chars, int length, uint32_t hash);
ObjClass *obj_mkclass(VM *vm, ObjString *name);
ObjInstance *obj_mkinstance(VM *vm, ObjClass *klass);
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
bool obj_valequal(VM *vm, Value a, Value b);
void vm_resetstack(VM *vm);
void vm_rterror(VM *vm, const char *format, ...);
void vm_defnative(VM *vm, const char *name, NativeFn function);
Value cfn_clock(VM *vm, int argc, Value *args);
Value cfn_print(VM *vm, int argc, Value *args);
Value cfn_println(VM *vm, int argc, Value *args);
Value cfn_chr(VM *vm, int argc, Value *args);
void vm_init(VM *vm);
void vm_free(VM *vm);
void vm_push(VM *vm, Value value);
Value vm_pop(VM *vm);
Value vm_peek(VM *vm, int distance);
bool vm_call(VM *vm, ObjClosure *closure, int argc);
bool vm_callvalue(VM *vm, Value callee, int argc);
bool vm_invokefromclass(VM *vm, ObjClass *klass, ObjString *name, int argc);
bool vm_invoke(VM *vm, ObjString *name, int argc);
bool vm_bindmethod(VM *vm, ObjClass *klass, ObjString *name);
ObjUpvalue *vm_captureupvalue(VM *vm, Value *local);
void vm_closeupvalues(VM *vm, Value *last);
void vm_defmethod(VM *vm, ObjString *name);
bool vm_isfalsey(VM *vm, Value value);
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
    // vm_call for functions
    [TOKEN_LEFTPAREN] = { rule_grouping, rule_parsecall, PREC_CALL },
    [TOKEN_RIGHTPAREN] = { NULL, NULL, PREC_NONE },
    [TOKEN_LEFTBRACE] = { NULL, NULL, PREC_NONE },
    [TOKEN_RIGHTBRACE] = { NULL, NULL, PREC_NONE },
    [TOKEN_COMMA] = { NULL, NULL, PREC_NONE },
    [TOKEN_DOT] = { NULL, rule_dot, PREC_CALL },
    [TOKEN_MINUS] = { rule_unary, rule_binary, PREC_TERM },
    [TOKEN_PLUS] = { NULL, rule_binary, PREC_TERM },
    [TOKEN_SEMICOLON] = { NULL, NULL, PREC_NONE },
    [TOKEN_SLASH] = { NULL, rule_binary, PREC_FACTOR },
    [TOKEN_STAR] = { NULL, rule_binary, PREC_FACTOR },
    [TOKEN_MODULO] = { NULL, rule_binary, PREC_FACTOR },
    [TOKEN_BANG] = { rule_unary, NULL, PREC_NONE },
    // equality precedence
    [TOKEN_BANGEQUAL] = { NULL, rule_binary, PREC_EQUALITY },
    // comaprison precedence
    [TOKEN_EQUAL] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_EQUALEQUAL] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_GREATERTHAN] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_GREATEREQUAL] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_LESSTHAN] = { NULL, rule_binary, PREC_COMPARISON },
    [TOKEN_LESSEQUAL] = { NULL, rule_binary, PREC_COMPARISON },
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

void repl(VM* vm)
{
    char line[1024];
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

char* readfile(VM* vm, const char* path)
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
    if(file == NULL)
    {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }
    fseek(file, 0L, SEEK_END);
    size_t fsz = ftell(file);
    rewind(file);
    char* buffer = (char*)malloc(fsz + 1);
    if(buffer == NULL)
    {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }

    size_t bytesread = fread(buffer, sizeof(char), fsz, file);
    /* notes on fread in C
	size_t fread(void * buffer, size_t size, size_t count, FILE * stream)
	buffer =  a pointer to the block of memoery with a least (size*count) byte
	size = size of the result type
	count = number of elements/ file size
	*/

    if(bytesread < fsz)
    {
        fprintf(stderr, "Could not read \"%s\".\n", path);
        exit(74);
    }

    buffer[bytesread] = '\0';

    fclose(file);
    return buffer;
}

void* mem_realloc(VM* vm, void* pointer, size_t oldsize, size_t newsize)
{
    // self adjusting heap for garbage collection
    vm->totalalloc += newsize - oldsize;
    // when allocating NEW memory, not when freeing as collectgarbage will call mem_realloc itself
    if(newsize > oldsize)
    {
#ifdef DEBUG_STRESS_GC
        mem_collectgarbage(vm);
#endif

        // run collecter if totalalloc is above threshold
        if(vm->totalalloc > vm->nextgc)
        {
            mem_collectgarbage(vm);
        }
    }

    if(newsize == 0)
    {
        free(pointer);
        return NULL;
    }
    void* result = realloc(pointer, newsize);
    if(result == NULL)
    {
        fprintf(stderr, "realloc(..., %d) returned NULL!\n", newsize);
        exit(1);
    }
    return result;
}


Obj* mem_allocobject(VM* vm, size_t size, ObjType type)
{
    Obj* object = (Obj*)mem_realloc(vm, NULL, 0, size);
    object->type = type;
    object->ismarked = false;

    // every time an object is allocated, insert to the list
    // insert as the HEAD; the latest one inserted will be at the start
    object->next = vm->objects;
    vm->objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zd for %d\n", (void*)object, size, type);
#endif
    return object;
}


// you can pass in a'lower' struct pointer, in this case Obj*, and get the higher level which is ObjFunction
void mem_freeobject(VM* vm, Obj* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch(object->type)
    {
        case OBJ_BOUND_METHOD:
            {
                memwrap_free(vm, ObjBoundMethod, object);
            }
            break;

        case OBJ_CLASS:
            {
                ObjClass* klass = (ObjClass*)object;
                htable_free(vm, &klass->methods);
                memwrap_free(vm, ObjClass, object);
            }
            break;

        case OBJ_INSTANCE:
            {
                ObjInstance* instance = (ObjInstance*)object;
                htable_free(vm, &instance->fields);
                memwrap_free(vm, ObjInstance, object);
            }
            break;
        case OBJ_CLOSURE:
        {
            // free upvalues
            ObjClosure* closure = (ObjClosure*)object;
            memwrap_freearray(vm, ObjUpvalue*, closure->upvalues, closure->upvaluecount);
            // only free the closure, not the function itself
            memwrap_free(vm, ObjClosure, object);
            break;
        }
        // return bits(chunk) borrowed to the operating system
        case OBJ_FUNCTION:
        {
            ObjFunction* function = (ObjFunction*)object;
            chunk_free(vm, &function->chunk);
            memwrap_free(vm, ObjFunction, object);
            break;
        }
        case OBJ_NATIVE:
        {
            memwrap_free(vm, ObjNative, object);
            break;
        }
        case OBJ_STRING:
        {
            ObjString* string = (ObjString*)object;
            memwrap_freearray(vm, char, string->chars, string->length + 1);
            memwrap_free(vm, ObjString, object);
            break;
        }
        case OBJ_UPVALUE:
        {
            memwrap_free(vm, ObjUpvalue, object);
            break;
        }
    }
}

/*		garbage collection		 */

void mem_markobject(VM* vm, Obj* object)
{
    // in some places the pointer is empty
    if(object == NULL)
    {
        return;
    }
    // object is already marked
    if(object->ismarked)
    {
        return;
    }
    object->ismarked = true;

    // create a worklist of grayobjects to traverse later, use a stack to implement it
    if(vm->graycap < vm->graycount + 1)
    {
        vm->graycap = memwrap_growcap(vm, vm->graycap);
        // use native realloc here
        vm->graystack = realloc(vm->graystack, sizeof(Obj*) * vm->graycap);
    }

    // if fail to allocate memory for the gray stack
    if(vm->graystack == NULL)
    {
        fprintf(stderr, "failed to allocate graystack!\n");
        exit(1);
    }
    // add the 'gray' object to the working list
    vm->graystack[vm->graycount++] = object;

#ifdef DEBUG_LOG_GC
    printf("%p marked ", (void*)object);
    // you cant print first class objects, like how you would print in the actual repl
    obj_printvalue(vm, obj_mkobject(object));
    printf("\n");
#endif
}

void mem_markvalue(VM* vm, Value value)
{
    // if value is not first class Objtype return
    if(!obj_isobject(value))
        return;
    mem_markobject(vm, obj_asobject(value));
}


// marking array of values/constants of a function, used in mem_blackenobject, case OBJ_FUNCTION
void mem_markarray(VM* vm, ValueArray* array)
{
    for(int i = 0; i < array->count; i++)
    {
        // mark each Value in the array
        mem_markvalue(vm, array->values[i]);
    }
}


void mem_markroots(VM* vm)
{
    // assiging a pointer to a full array means assigning the pointer to the FIRST element of that array
    // walk through all values/slots in the Value* array
    for(Value* slot = vm->stack; slot < vm->stacktop; slot++)
    {
        mem_markvalue(vm, *slot);
    }
    // mark closures
    for(int i = 0; i < vm->framecount; i++)
    {
        mem_markobject(vm, (Obj*)vm->frames[i].closure);
    }

    // mark upvalues, walk through the linked list of upvalues
    for(ObjUpvalue* upvalue = vm->openupvalues; upvalue != NULL; upvalue = upvalue->next)
    {
        mem_markobject(vm, (Obj*)upvalue);
    }
    // mark global variables, belongs in the VM/hashtable
    htable_mark(vm, &vm->globals);

    // compiler also grabs memory; special function only for 'backend' processes
    prs_markroots(vm);
    // mark objstring for init
    mem_markobject(vm, (Obj*)vm->initstring);
}

// actual tracing of each gray object and marking it black
void mem_blackenobject(VM* vm, Obj* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p blackened ", (void*)object);
    obj_printvalue(vm, obj_mkobject(object));
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

        case OBJ_UPVALUE:
            // simply mark the closed value
            mem_markvalue(vm, ((ObjUpvalue*)object)->closed);
            break;

        // mark the name and its value array of constants
        case OBJ_FUNCTION:
        {
            // you can get the coressponding 'higher' object type from a lower derivation struct in C using (higher*)lower
            ObjFunction* function = (ObjFunction*)object;
            // mark its name, an ObjString type
            mem_markobject(vm, (Obj*)function->name);
            // mark value array of chunk constants, pass it in AS A POINTER using &
            mem_markarray(vm, &function->chunk.constants);
            break;
        }

        // mark the function and all of the closure's upvalues
        case OBJ_CLOSURE:
        {
            ObjClosure* closure = (ObjClosure*)object;
            mem_markobject(vm, (Obj*)closure->function);
            for(int i = 0; i < closure->upvaluecount; i++)
            {
                mem_markobject(vm, (Obj*)closure->upvalues[i]);
            }
            break;
        }

        case OBJ_CLASS:
        {
            ObjClass* klass = (ObjClass*)object;
            mem_markobject(vm, (Obj*)klass->name);
            htable_mark(vm, &klass->methods);
            break;
        }

        case OBJ_INSTANCE:
        {
            ObjInstance* instance = (ObjInstance*)object;
            mem_markobject(vm, (Obj*)instance->classobject);
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
    while(vm->graycount > 0)
    {
        // vm_pop Obj* (pointer) from the stack
        // note how -- is the prefix; subtract first then use it as an index
        // --vm->graycount already decreases its count, hence everything is already 'popped'
        Obj* object = vm->graystack[--vm->graycount];
        mem_blackenobject(vm, object);
    }
}


// sweeping all unreachable values
void mem_sweep(VM* vm)
{
    Obj* previous = NULL;
    // linked intrusive list of Objects in the VM
    Obj* object = vm->objects;

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
            Obj* unreached = object;
            object = object->next;
            // link to previous object if previous not null
            if(previous != NULL)
            {
                previous->next = object;
            }
            // if not set the next as the start of the list
            else
            {
                vm->objects = object;
            }
            // method that actually frees the object
            mem_freeobject(vm, unreached);
        }
    }
}

void mem_collectgarbage(VM* vm)
{
#ifdef DEBUG_LOG_GC
    printf("--Garbage Collection Begin\n");
    size_t before = vm->totalalloc;
#endif
    // function to start traversing the graph, from the root and marking them
    mem_markroots(vm);
    // tracing each gray marked object
    mem_tracerefs(vm);

    // removing intern strings, BEFORE the sweep so the pointers can still access its memory
    // function defined in hahst.c
    htable_removewhite(vm, &vm->strings);
    // free all unreachable roots
    mem_sweep(vm);

    // adjust size of threshold
    vm->nextgc = vm->totalalloc * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("--Garbage Collection End\n");
    printf("	collected %zd bytes (from %zd to %zd) next at %zd\n", before - vm->totalalloc, before, vm->totalalloc, vm->nextgc);
#endif
}

void mem_freeobjlist(VM* vm)
{
    Obj* object = vm->objects;
    // free from the whole list
    while(object != NULL)
    {
        Obj* next = object->next;
        mem_freeobject(vm, object);
        object = next;
    }
    // free gray marked obj stack used for garbage collection
    free(vm->graystack);
}


int dbg_print_simpleinst(VM* vm, const char* name, int offset)
{
    (void)vm;
    printf("%s\n", name);
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
    // pullout the constant index from the subsequent byte in the chunk
    uint8_t constant = chunk->code[offset + 1];
    // print out name of the opcode, then the constant index
    printf("%-16s %4d '", name, constant);
    //	display the value of the constant,  user defined function
    obj_printvalue(vm, chunk->constants.values[constant]);
    printf("'\n");
    //OP_RETURN is a single byte, and the other byte is the operand, hence offsets by 2
    return offset + 2;
}

int dbg_print_invokeinst(VM* vm, const char* name, Chunk* chunk, int offset)
{
    (void)vm;
    // get index of the name first
    uint8_t constant = chunk->code[offset + 1];
    // then get number of arguments
    uint8_t argc = chunk->code[offset + 2];
    printf("%-16s (%d args) %4d", name, argc, constant);
    // print the method
    obj_printvalue(vm, chunk->constants.values[constant]);
    printf("\n");
    return offset + 3;
}

int dbg_print_jumpinst(VM* vm, const char* name, int sign, Chunk* chunk, int offset)
{
    (void)vm;
    // get jump
    uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

void dbg_disasmchunk(VM* vm, Chunk* chunk, const char* name)
{
    // print a little header for debugging
    printf("== %s ==\n", name);

    // for every existing instruction in the chunk
    for(int offset = 0; offset < chunk->count;)
    {
        // disassemble individually, offset will be controlled from this function
        offset = dbg_disasminst(vm, chunk, offset);
    }
}

int dbg_disasminst(VM* vm, Chunk* chunk, int offset)
{
    // print byte offset of the given instruction, or the index
    printf("%04d ", offset);

    // show source line each instruction was compiled from
    // show a | for any instruction that comes from the
    //same source as its preceding one
    if(offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1])
    {
        printf("	| ");
    }
    else
    {
        printf("%4d ", chunk->lines[offset]);
    }
    // takes one byte, or an element, from the container
    uint8_t instruction = chunk->code[offset];
    switch(instruction)
    {
        // pass in chunk to get ValueArray element
        case OP_CONSTANT:
            return dbg_print_constinst(vm, "OP_CONSTANT", chunk, offset);

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
        case OP_GETLOCAL:
            return dbg_print_byteinst(vm, "OP_GETLOCAL", chunk, offset);
        case OP_SETLOCAL:
            return dbg_print_byteinst(vm, "OP_SETLOCAL", chunk, offset);

        case OP_GET_UPVALUE:
            return dbg_print_byteinst(vm, "OP_GET_UPVALUE", chunk, offset);
        case OP_SETUPVALUE:
            return dbg_print_byteinst(vm, "OP_SETUPVALUE", chunk, offset);
        case OP_GETPROPERTY:
            return dbg_print_constinst(vm, "OP_GETPROPERTY", chunk, offset);
        case OP_SETPROPERTY:
            return dbg_print_constinst(vm, "OP_SETPROPERTY", chunk, offset);

        case OP_CLOSEUPVALUE:
            return dbg_print_simpleinst(vm, "OP_CLOSEVALUE", offset);

        case OP_DEFINEGLOBAL:
            return dbg_print_simpleinst(vm, "OP_DEFINEGLOBAL", offset);
        case OP_GETGLOBAL:
            return dbg_print_simpleinst(vm, "OP_GETGLOBAL", offset);
        case OP_SETGLOBAL:
            return dbg_print_simpleinst(vm, "OP_SETGLOBAL", offset);
        case OP_SWITCHEQUAL:
            return dbg_print_simpleinst(vm, "OP_SWITCHEQUAL", offset);

        case OP_JUMP:
            return dbg_print_jumpinst(vm, "OP_JUMP", 1, chunk, offset);
        case OP_JUMPIFFALSE:
            return dbg_print_jumpinst(vm, "OP_JUMPIFFALSE", 1, chunk, offset);

        case OP_CALL:
            return dbg_print_byteinst(vm, "OP_CALL", chunk, offset);

        case OP_METHOD:
            return dbg_print_constinst(vm, "OP_METHOD", chunk, offset);

        case OP_INVOKE:
            return dbg_print_invokeinst(vm, "OP_INVOKE", chunk, offset);


        case OP_CLOSURE:
        {
            offset++;
            // index for Value
            uint8_t constant = chunk->code[offset++];
            printf("%-16s %4d ", "OP_CLOSURE", constant);
            // accessing the value using the index
            obj_printvalue(vm, chunk->constants.values[constant]);
            printf("\n");

            ObjFunction* function = obj_asfunction(chunk->constants.values[constant]);
            // walk through upvalues
            for(int j = 0; j < function->upvaluecount; j++)
            {
                int isl = chunk->code[offset++];
                int index = chunk->code[offset++];
                printf("%04d	|	%s %d\n", offset - 2, isl ? "local" : "upvalue", index);
            }

            return offset;
        }

        case OP_CLASS:
            return dbg_print_constinst(vm, "OP_CLASS", chunk, offset);

        case OP_INHERIT:
            return dbg_print_simpleinst(vm, "OP_INEHEIRT", offset);

        // class inheritance
        case OP_GETSUPER:
            return dbg_print_constinst(vm, "OP_GETSUPER", chunk, offset);

        case OP_SUPERINVOKE:
            return dbg_print_invokeinst(vm, "OP_SUPERINVOKE", chunk, offset);

        // dispatch to a utility function to display it
        case OP_RETURN:
            return dbg_print_simpleinst(vm, "OP_RETURN", offset);

        case OP_LOOP:
            return dbg_print_jumpinst(vm, "OP_LOOP", -1, chunk, offset);

        case OP_LOOPIFTRUE:
            return dbg_print_jumpinst(vm, "OP_LOOPIFTRUE", -1, chunk, offset);

        case OP_LOOPIFFALSE:
            return dbg_print_jumpinst(vm, "OP_LOOPIFFALSE", -1, chunk, offset);

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
    memwrap_freearray(vm, Entry, table->entries, table->capacity);
    htable_init(vm, table);
    table->entries = NULL;
}

Entry* htable_findentry(Entry* entries, int capacity, ObjString* key)
{
    // use modulo to map the key's hash to the code index
    uint32_t index = key->hash % capacity;
    Entry* tombstone = NULL;

    for(;;)
    {
        // index is 'inserted' here
        Entry* entry = &entries[index];

        if(entry->key == NULL)
        {
            if(obj_isnull(entry->value))
            {
                // empty entry
                return tombstone != NULL ? tombstone : entry;
            }
            else
            {
                // can return tombstone bucket as empty and reuse it
                if(tombstone == NULL)
                    tombstone = entry;
            }
        }
        // compare them in MEMORY
        if(entry->key == key)
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

    // asign the value parameter the entry value
    *value = entry->value;
    return true;
}


void htable_adjustcap(VM* vm, Table* table, int capacity)
{
    int i;
    Entry* dest;
    Entry* entry;
    Entry* entries;
    // create a bucket with capacity entries, new array
    entries = memwrap_allocate(vm, Entry, capacity);
    // initialize every element
    for(i = 0; i < capacity; i++)
    {
        entries[i].key = NULL;
        entries[i].value = obj_nullval;
    }
    // do not copy tombstones over when growing
    table->count = 0;
    // NOTE: entries may end up in different buckets
    // with the same hash as it is divided by the modulo; loop below recalculates everything
    // travers through old array
    for(i = 0; i < table->capacity; i++)
    {
        entry = &table->entries[i];
        if(entry->key == NULL)
        {
            continue;
        }
        // insert into new array
        // pass in new array
        dest = htable_findentry(entries, capacity, entry->key);
        // match old array to new array
        dest->key = entry->key;
        dest->value = entry->value;
        // recound the number of entries
        table->count++;
    }

    memwrap_freearray(vm, Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

// inserting into the table, return false if collision
bool htable_set(VM* vm, Table* table, ObjString* key, Value value)
{
    // make sure array is big enough
    if(table->count + 1 > table->capacity * TABLE_MAX_LOAD)
    {
        int capacity = memwrap_growcap(vm, table->capacity);
        htable_adjustcap(vm, table, capacity);
    }


    Entry* entry = htable_findentry(table->entries, table->capacity, key);

    bool isnewkey = entry->key == NULL;
    // obj_isnull for tombstones; treat them as full objects
    if(isnewkey && obj_isnull(entry->value))
        table->count++;

    entry->key = key;
    entry->value = value;

    return isnewkey;
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
    entry->value = obj_mkbool(true);

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
ObjString* htable_findstring(VM* vm, Table* table, const char* chars, int length, uint32_t hash)
{
    (void)vm;
    if(table->count == 0)
        return NULL;

    // get index
    uint32_t index = hash % table->capacity;

    for(;;)
    {
        // get entry pointer
        Entry* entry = &table->entries[index];
        if(entry->key == NULL)
        {
            // stop if found empty non-tombstone entry
            if(obj_isnull(entry->value))
            {
                // return null if not tombstone(tombstone value is obj_mkbool(true))
                return NULL;
            }
        }
        else if(entry->key->length == length && entry->key->hash == hash && memcmp(entry->key->chars, chars, length) == 0)
        {
            // found the entry
            return entry->key;
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
        // remove not marked (string) object pointers
        if(entry->key != NULL && !entry->key->obj.ismarked)
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
        // mark the string key(ObjString type)
        mem_markobject(vm, (Obj*)entry->key);
        // mark the actual avlue
        mem_markvalue(vm, entry->value);
    }
}

void chunk_init(VM* vm, Chunk* chunk)
{
    chunk->count = 0;
    chunk->capacity = 0;
    // dynamic array starts off completely empty
    chunk->code = NULL;
    // to store current line of code
    chunk->lines = NULL;
    // initialize constant list
    obj_initvalarray(vm, &chunk->constants);
}

void chunk_write(VM* vm, Chunk* chunk, uint8_t byte, int line)
{
    // check if chunk is full
    if(chunk->capacity < chunk->count + 1)
    {
        int oldcapacity = chunk->capacity;
        // get size of new capacity
        chunk->capacity = memwrap_growcap(vm, oldcapacity);
        // mem_realloc memory and grow array
        chunk->code = memwrap_growarray(vm, uint8_t, chunk->code, oldcapacity, chunk->capacity);
        chunk->lines = memwrap_growarray(vm, int, chunk->lines, oldcapacity, chunk->capacity);
    }
    // code is an array, [] is just the index number
    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}


void chunk_free(VM* vm, Chunk* chunk)
{
    // chunk->code is the pointer to the array, capacity is the size
    memwrap_freearray(vm, uint8_t, chunk->code, chunk->capacity);
    memwrap_freearray(vm, int, chunk->lines, chunk->capacity);
    obj_freevalarray(vm, &chunk->constants);
    chunk_init(vm, chunk);
}

int chunk_addconstant(VM* vm, Chunk* chunk, Value value)
{
    // garbage collection
    vm_push(vm, value);
    obj_writevalarray(vm, &chunk->constants, value);
    // garbage collection
    vm_pop(vm);
    // return index of the newly added constant
    return chunk->constants.count - 1;
}

bool obj_istype(Value value, ObjType type)
{
	return obj_isobject(value) && obj_asobject(value)->type == type;
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
    ObjUpvalue** upvalues = memwrap_allocate(vm, ObjUpvalue*, function->upvaluecount);

    for(int i = 0; i < function->upvaluecount; i++)
    {
        // initialize all as null
        upvalues[i] = NULL;
    }


    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvaluecount = function->upvaluecount;
    return closure;
}

ObjString* obj_mkstring(VM* vm, char* chars, int length, uint32_t hash)
{
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    // garbage collection
    vm_push(vm, obj_mkobject(string));
    //printf("allocate\n");
    // for string interning
    htable_set(vm, &vm->strings, string, obj_nullval);
    // garbage collection
    vm_pop(vm);
    return string;
}

ObjClass* obj_mkclass(VM* vm, ObjString* name)
{
    ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    klass->name = name;
    htable_init(vm, &klass->methods);
    return klass;
}


// create new class instance
ObjInstance* obj_mkinstance(VM* vm, ObjClass* klass)
{
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->classobject = klass;
    // memory address of the fields
    htable_init(vm, &instance->fields);
    return instance;
}


ObjFunction* obj_mkfunction(VM* vm)
{
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);

    function->arity = 0;
    function->upvaluecount = 0;
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
    // initial hash value, u at end means unsigned
    uint32_t hash = 2116136261u;
    // traverse through the data to be hashed
    for(int i = 0; i < length; i++)
    {
        // munge the bits from the string key to the hash value; ^= is a bitwise operator
        hash ^= key[i];
        hash *= 16777619;
    }

    return hash;
}


// shorten than obj_copystring because owernship of the char* itself is declared in vm_concatenate(), hence no need to declare memory again
ObjString* obj_takestring(VM* vm, char* chars, int length)
{
    uint32_t hash = obj_hashstring(vm, chars, length);
    ObjString* interned = htable_findstring(vm, &vm->strings, chars, length, hash);

    // if the same string already exists
    if(interned != NULL)
    {
        // free the memory for use
        memwrap_freearray(vm, char, chars, length + 1);
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
        // if we find a string already in vm->srings, no need to copy just return the pointer
        return interned;
    }
    // length +1 for null terminator
    char* heapchars = memwrap_allocate(vm, char, length + 1);
    // copy memory from one location to another; memcpy(*to, *from, size_t (from))
    memcpy(heapchars, chars, length);
    // '\0', a null terminator used to signify the end of the string, placed at the end
    heapchars[length] = '\0';

    return obj_mkstring(vm, heapchars, length, hash);
}


ObjUpvalue* obj_mkupvalue(VM* vm, Value* slot)
{
    (void)vm;
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->next = NULL;
    upvalue->closed = obj_nullval;
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
    // print name and number of parameters
    printf("fun %s(%d params)", function->name->chars, function->arity);
}

void obj_printobject(VM* vm, Value value)
{
    // first class objects can be printed; string and functions
    switch(obj_type(value))
    {
        case OBJ_BOUND_METHOD:
            obj_printfunction(vm, obj_asboundmethod(value)->method->function);
            break;
        case OBJ_CLASS:
            printf("%s", obj_asclass(value)->name->chars);
            break;
        case OBJ_INSTANCE:
            printf("%s instance", obj_asinstance(value)->classobject->name->chars);
            break;
        case OBJ_CLOSURE:
            obj_printfunction(vm, obj_asclosure(value)->function);
            break;
        case OBJ_FUNCTION:
            obj_printfunction(vm, obj_asfunction(value));
            break;
        case OBJ_NATIVE:
            printf("<native fun>");
            break;
        case OBJ_STRING:
            printf("%s", obj_asstring(value)->chars);
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
        int oldcapacity = array->capacity;
        array->capacity = memwrap_growcap(vm, oldcapacity);
        array->values = memwrap_growarray(vm, Value, array->values, oldcapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void obj_freevalarray(VM* vm, ValueArray* array)
{
    memwrap_freearray(vm, Value, array->values, array->capacity);
    obj_initvalarray(vm, array);
}

// actual printing on the virtual machine is done here
void obj_printvalue(VM* vm, Value value)
{
    switch(value.type)
    {
        case VAL_BOOL:
            printf(obj_asbool(value) ? "true" : "false");
            break;
        case VAL_NULL:
            printf("null");
            break;
        case VAL_NUMBER:
            printf("%g", obj_asnumber(value));
            break;
        case VAL_OBJ:
            obj_printobject(vm, value);
            break;
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
            return obj_asbool(a) == obj_asbool(b);
        case VAL_NUMBER:
            return obj_asnumber(a) == obj_asnumber(b);
        // true for all nulls
        case VAL_NULL:
            return true;
        case VAL_OBJ:
            // already interned, occupies the same address
            return obj_asobject(a) == obj_asobject(b);
        default:
            // unreachable
            return false;
    }
}

void scanner_init(const char* source)
{
    g_currentscanner.start = source;
    g_currentscanner.current = source;
    g_currentscanner.line = 1;
}

bool scanner_isalpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool scanner_isdigit(char c)
{
    return c >= '0' && c <= '9';
}

bool scanner_isatend()
{
    return *g_currentscanner.current == '\0';
}

char scanner_advance()
{
    g_currentscanner.current++;
    return g_currentscanner.current[-1];
}

// logical conditioning to check if 2nd character is embedded to first(e.g two char token)
bool scanner_match(char expected)
{
    if(scanner_isatend())
        return false;
    if(*g_currentscanner.current != expected)
    {
        return false;
    }
    g_currentscanner.current++;
    return true;
}

// make a token, uses the scanner's start and current to capture the lexeme and its size
Token scanner_maketoken(TokenType type)
{
    Token token;
    token.type = type;
    token.start = g_currentscanner.start;
    token.length = (int)(g_currentscanner.current - g_currentscanner.start);
    token.line = g_currentscanner.line;

    return token;
}

// similar to scanner_maketoken, but the "lexeme" points to the error message string instead of the scanner's current and start pointers
Token scanner_errortoken(const char* message)
{
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = g_currentscanner.line;

    return token;
}

// returns current character
char scanner_peek()
{
    return *g_currentscanner.current;
}

// returns next character
char scanner_peeknext()
{
    if(scanner_isatend())
        return '\0';
    return g_currentscanner.current[1];
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
                    g_currentscanner.line++;
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
                        {
                            // if not new line or not end, treat as whitespace and advance
                            scanner_advance();
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
TokenType scanner_checkkeyword(int start, int length, const char* rest, TokenType type)
{
    /* hard expression here
	bascially if they are exactly the same, and compares their memory(memcmp)
	int memcmp(const void *str1, const void *str2, size_t n) -> if it is exactly the same, then it is 0
	*/
    if(g_currentscanner.current - g_currentscanner.start == start + length && memcmp(g_currentscanner.start + start, rest, length) == 0)
    {
        return type;
    }

    return TOKEN_IDENTIFIER;
}

// the 'trie' to store the set of strings
TokenType scanner_parseidenttype()
{
    // start of the lexeme
    switch(g_currentscanner.start[0])
    {
        //case 'a': return scanner_checkkeyword(1, 2, "nd", TOKEN_AND);
        case 'a':
            {
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    switch(g_currentscanner.start[1])
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
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    switch(g_currentscanner.start[1])
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
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    switch(g_currentscanner.start[1])
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
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    // check if there is a second letter
                    switch(g_currentscanner.start[1])
                    {
                        case 'l':
                            {
                                // check if there is a third letter
                                if(g_currentscanner.current - g_currentscanner.start > 2)
                                {
                                    switch(g_currentscanner.start[2])
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
                                return scanner_checkkeyword(2, 4, "uals", TOKEN_EQUALEQUAL);
                            }
                            break;
                    }
                }
            }
            break;
        case 'f':
            {
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    switch(g_currentscanner.start[1])
                    {
                        case 'a':
                            {
                                return scanner_checkkeyword(2, 3, "lse", TOKEN_FALSE);
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
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    switch(g_currentscanner.start[1])
                    {
                        case 'f':
                            {
                                return scanner_checkkeyword(2, 0, "", TOKEN_IF);
                            }
                            break;
                        case 's':
                            {
                                return scanner_checkkeyword(2, 0, "", TOKEN_EQUALEQUAL);
                            }
                            break;
                    }
                }
            }
            break;
        case 'n':
            {
                switch(g_currentscanner.start[1])
                {
                    case 'u':
                        {
                            return scanner_checkkeyword(1, 3, "ull", TOKEN_NULL);
                        }
                        break;
                    case 'i':
                        {
                            return scanner_checkkeyword(1, 2, "il", TOKEN_NULL);
                        }
                        break;
                }
            }
            break;
        case 'o':
            {
                return scanner_checkkeyword(1, 1, "r", TOKEN_OR);
            }
            break;
        case 'r':
            {
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    switch(g_currentscanner.start[1])
                    {
                        case 'e':
                            if(g_currentscanner.current - g_currentscanner.start > 2)
                            {
                                switch(g_currentscanner.start[2])
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
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    switch(g_currentscanner.start[1])
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
                if(g_currentscanner.current - g_currentscanner.start > 1)
                {
                    switch(g_currentscanner.start[1])
                    {
                        case 'h':
                            {
                                if(g_currentscanner.current - g_currentscanner.start > 2)
                                {
                                    switch(g_currentscanner.start[2])
                                    {
                                        case 'e':
                                            return scanner_checkkeyword(3, 1, "n", TOKEN_THEN);
                                        case 'i':
                                            return scanner_checkkeyword(3, 1, "s", TOKEN_THIS);
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
    {
        // skip if still letters or digits
        scanner_advance();
    }
    return scanner_maketoken(scanner_parseidenttype());
}

Token scanner_parsenumber()
{
    while(scanner_isdigit(scanner_peek()))
    {
        // while next is still a digit advance
        scanner_advance();
    }
    // look for fractional part
    // if there is a . and next is still digit
    if(scanner_peek() == '.' && scanner_isdigit(scanner_peeknext()))
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
        {
            // allow strings to go until next line
            g_currentscanner.line++;
        }
        scanner_advance();// consume characters until the closing quote is reached
    }

    if(scanner_isatend())
    {
        return scanner_errortoken("Unterminated string.");
    }
    // closing quote
    scanner_advance();
    return scanner_maketoken(TOKEN_STRING);
}

// reading the char, and return a token
Token scanner_scantoken()
{
    scanner_skipspace();
    // reset the g_currentscanner to current
    g_currentscanner.start = g_currentscanner.current;
    // check if at end
    if(scanner_isatend())
    {
        return scanner_maketoken(TOKEN_EOF);
    }
    // if not end of file
    char c = scanner_advance();

    if(scanner_isalpha(c))
    {
        return scanner_parseident();
    }
    if(scanner_isdigit(c))
    {
        return scanner_parsenumber();
    }
    // lexical grammar for the language
    // for single characters
    switch(c)
    {
        case '(':
            return scanner_maketoken(TOKEN_LEFTPAREN);
        case ')':
            return scanner_maketoken(TOKEN_RIGHTPAREN);
        case '{':
            return scanner_maketoken(TOKEN_LEFTBRACE);
        case '}':
            return scanner_maketoken(TOKEN_RIGHTBRACE);
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
            return scanner_maketoken(scanner_match('=') ? TOKEN_BANGEQUAL : TOKEN_BANG);
        case '=':
            return scanner_maketoken(scanner_match('=') ? TOKEN_EQUALEQUAL : TOKEN_EQUAL);
        case '>':
            return scanner_maketoken(scanner_match('=') ? TOKEN_GREATEREQUAL : TOKEN_GREATERTHAN);
        case '<':
            return scanner_maketoken(scanner_match('=') ? TOKEN_LESSEQUAL : TOKEN_LESSTHAN);

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
    // again, go next first then use previous type as the 'current' token
    prs_advance(vm);
    // the way the compiler is designed is that it has to always have a prefix
    ParseFn prefixrule = prs_getrule(vm, g_currentparser.previous.type)->prefix;

    if(prefixrule == NULL)
    {
        prs_error(vm, "Expect expression.");
        return;
    }
    // for assignment precedence
    bool canassign = precedence <= PREC_ASSIGNMENT;
    // vm_call the prefix function, may consume a lot of tokens
    prefixrule(vm, canassign);


    /* after prefix expression is done, look for infix expression
	IMPORTANT: infix only runs if given precedence is LOWER than the operator for the infix
	or more nicely if NEXT/INFIX PRECEDENCE IS HIGHER THAN PREC ASSIGNMENT(parameter above_
	*/
    while(precedence <= prs_getrule(vm, g_currentparser.current.type)->precedence)
    {
        prs_advance(vm);
        ParseFn infixrule = prs_getrule(vm, g_currentparser.previous.type)->infix;

        infixrule(vm, canassign);
    }
    // if = is not consumed as part of the expression, nothing will , hence an error
    if(canassign && prs_matchtoken(vm, TOKEN_EQUAL))
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

// a single 'statement' or line
void prs_expression(VM* vm)
{
    // as assignment is the 2nd lowest, parses evrything
    prs_parseprecedence(vm, PREC_ASSIGNMENT);
}

void prs_block(VM* vm)
{
    // parse until EOF or right brace is 'peeked'
    while(!prs_check(vm, TOKEN_RIGHTBRACE) && !prs_check(vm, TOKEN_EOF))
    {
        // compile rest of block, keeps on parsing until right brace or EOF is 'peeked'
        prs_declaration(vm);
    }

    prs_consume(vm, TOKEN_RIGHTBRACE, "Expect '}' after block.");
}


/* functions */
void prs_function(VM* vm, FunctionType type)
{
    // create separate Compiler for each function
    Compiler compiler;
    // set new compiler(function) as the current one
    prs_initcompiler(vm, &compiler, type);
    prs_beginscope(vm);
    {
        // compile parameters
        prs_consume(vm, TOKEN_LEFTPAREN, "Expect '(' after function name.");
        // if end ) has not been reached
        if(!prs_check(vm, TOKEN_RIGHTPAREN))
        {
            do
            {
                // add number of parameters
                g_currentcompiler->function->arity++;
                if(g_currentcompiler->function->arity > 255)
                {
                    prs_erroratcurrent(vm, "Cannot have more than 255 parameters.");
                }
                // get name
                uint8_t paramconst = prs_parsevariable(vm, "Expect variable name.");
                // scope handled here already
                prs_definevariable(vm, paramconst);
            } while(prs_matchtoken(vm, TOKEN_COMMA));
        }

        prs_consume(vm, TOKEN_RIGHTPAREN, "expect ')' after parameter list");

        // body
        prs_consume(vm, TOKEN_LEFTBRACE, "expect '{' before function body");
        prs_block(vm);

        // create function object
        // ends the current compiler
        ObjFunction* function = prs_endcompiler(vm);
        // compilers are treated like a stack; if current one is ended, like above, return to the previous one
        prs_emitbytes(vm, OP_CLOSURE, prs_makeconst(vm, obj_mkobject(function)));

        /*	by the time the compiler reaches the end of a function prs_declaration,
        every variable reference hass been resolved as either local, upvalue or global.
        each upvalue may return a local var or another upvalue

        -> for each upvalue there are two single-byte operands
        -> if first byte is one, then it captures a local variable in the enclosing function
        -> if first byte is 0, it captures the function's upvalues
        */

        for(int i = 0; i < function->upvaluecount; i++)
        {
            prs_emitbyte(vm, compiler.upvalues[i].islocal ? 1 : 0);
            // emit index
            prs_emitbyte(vm, compiler.upvalues[i].index);
        }
    }
}

// create method for class type
void prs_parsemethod(VM* vm)
{

    prs_consume(vm, TOKEN_IDENTIFIER, "expect method name");
    // get method name
    uint8_t constant = prs_makeconstident(vm, &g_currentparser.previous);

    // method body
    FunctionType type = TYPE_METHOD;

    // if initializer
    if(g_currentparser.previous.length == 4 && memcmp(g_currentparser.previous.start, "init", 4) == 0)
    {
        type = TYPE_INITIALIZER;
    }
    // process the function
    prs_function(vm, type);
    prs_emitbytes(vm, OP_METHOD, constant);
}


void prs_classdecl(VM* vm)
{
    prs_consume(vm, TOKEN_IDENTIFIER, "Expect class name.");
    // get class name
    Token classname = g_currentparser.previous;
    // add to constant table as a string, return its index
    uint8_t nameconst = prs_makeconstident(vm, &g_currentparser.previous);
    // declare that name variable
    prs_declarevariable(vm);
    // takes opcode and takes the constant table index
    prs_emitbytes(vm, OP_CLASS, nameconst);
    // add it to the global hasht; we must DEFINE AFTER DECLARE to use it
    prs_definevariable(vm, nameconst);

    // handle class enclosing for 'this'
    ClassCompiler classcc;
    classcc.name = g_currentparser.previous;
    classcc.hassuper = false;
    classcc.enclosing = g_currentclass;
    // set new class as current
    g_currentclass = &classcc;

    // class inheritance
    if(prs_matchtoken(vm, TOKEN_FROM))
    {
        prs_consume(vm, TOKEN_IDENTIFIER, "Expect parent class name.");
        // get the class variable, looks up the parent class by name and vm_push it to the stack
        rule_variable(vm, false);

        // check that the class names must be different
        if(prs_identequal(vm, &classname, &g_currentparser.previous))
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

        rule_namedvar(vm, classname, false);
        prs_emitbyte(vm, OP_INHERIT);
        classcc.hassuper = true;
    }
    // helper function to geenrate code that LOADS a variable with a given name to te stack
    rule_namedvar(vm, classname, false);
    prs_consume(vm, TOKEN_LEFTBRACE, "Expect '{' before class body.");
    while(!prs_check(vm, TOKEN_RIGHTBRACE) && !prs_check(vm, TOKEN_EOF))
    {
        if(prs_matchtoken(vm, TOKEN_FUN))
        {
            prs_parsemethod(vm);
        }
        else
        {
            prs_error(vm, "unexpected token in class body");
        }
    }
    prs_consume(vm, TOKEN_RIGHTBRACE, "Expect '}' after class body.");
    // no longer need the class, vm_pop it
    prs_emitbyte(vm, OP_POP);

    // close local scope for superclass variable
    if(classcc.hassuper)
    {
        prs_endscope(vm);
    }
    // go back to enclosing/main() class
    g_currentclass = g_currentclass->enclosing;
}


void prs_funcdecl(VM* vm)
{
    uint8_t global = prs_parsevariable(vm, "Expect function name.");
    // scoping
    prs_markinitialized(vm);
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
        // not initialized
        prs_emitbyte(vm, OP_NULL);
    }
    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
    // create global variable here; if local, not added to table
    prs_definevariable(vm, global);
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
    prs_expression(vm);
    // gives an operand on how much to offset the ip; how many bytes of code to skip
    // if falsey, simply adjusts the ip by that amount
    // offset to jump to next (potentially else or elf) statment
    // insert to opcode the then branch statment first, then get offset
    int thenjump = prs_emitjump(vm, OP_JUMPIFFALSE);

    // vm_pop then
    prs_emitbyte(vm, OP_POP);

    /* use BACKPATCHING
	- emit jump first with a placeholder offset, and get how far to jump
	
	
	*/

    prs_statement(vm);

    // below jump wil SURELY jump; this is skipped if the first prs_emitjump is not false
    // need to jump at least 'twice' with an else statement
    // if the original statement is  true, then skip the the else statement
    int elsejump = prs_emitjump(vm, OP_JUMP);

    // if then statment is run; vm_pop the expression inside () after if
    prs_patchjump(vm, thenjump);

    // if else statment is run; vm_pop the expression inside () after if
    prs_emitbyte(vm, OP_POP);
    if(prs_matchtoken(vm, TOKEN_ELSE))
        prs_statement(vm);

    // else if
    if(prs_matchtoken(vm, TOKEN_ELF))
    {
        // go to statement, then go back to IF
        prs_ifstmt(vm);
    }

    /* this actually jumps */
    // last jump that is executed IF FIRST STATEMENT IS TRUE
    // for the second jump
    prs_patchjump(vm, elsejump);
}

void prs_switchstmt(VM* vm)
{
    if(!prs_check(vm, TOKEN_IDENTIFIER))
    {
        prs_erroratcurrent(vm, "Expect identifier after switch.");
    }

    // if no error, prs_consume the identifier
    prs_expression(vm);
    prs_consume(vm, TOKEN_LEFTBRACE, "Expect '{' after switch identifier.");
    prs_consume(vm, TOKEN_CASE, "Expect at least 1 case after switch declaration.");

    /* to store  opcode offsets */
    uint8_t casescount = -1;
    uint8_t capacity = 0;
    // 8 initial switch cases
    int* coffset = memwrap_allocate(vm, int, 8);
    // while next token is a case, match also advances
    do
    {
        // grow array if needed
        if(capacity < casescount + 1)
        {
            int oldcapacity = capacity;
            capacity = memwrap_growcap(vm, oldcapacity);
            coffset = memwrap_growarray(vm, int, coffset, oldcapacity, capacity);
        }

        casescount++;

        prs_expression(vm);
        prs_consume(vm, TOKEN_COLON, "Expect ':' after case expression.");
        // check if both values are equal
        prs_emitbyte(vm, OP_SWITCHEQUAL);
        // jump if false
        int casefalsejump = prs_emitjump(vm, OP_JUMPIFFALSE);
        // parse the statment
        prs_statement(vm);
        // vm_pop the 'true' from OP_SWITCHEQUAL
        prs_emitbyte(vm, OP_POP);
        coffset[casescount] = prs_emitjump(vm, OP_JUMP);
        // jump to end of case if false
        prs_patchjump(vm, casefalsejump);
        // vm_pop the 'false' statment from OP_SWITCHEQUAL
        prs_emitbyte(vm, OP_POP);
    } while(prs_matchtoken(vm, TOKEN_CASE));

    if(prs_matchtoken(vm, TOKEN_DEFAULT))
    {
        prs_consume(vm, TOKEN_COLON, "Expect ':' default case.");
        // running the default statement
        prs_statement(vm);
    }
    // prs_patchjump for each available jump
    for(uint8_t i = 0; i <= casescount; i++)
    {
        prs_patchjump(vm, coffset[i]);
    }

    prs_emitbyte(vm, OP_POP);
    memwrap_freearray(vm, int, coffset, capacity);

    prs_consume(vm, TOKEN_RIGHTBRACE, "Expect '}' at the end of switch statement");
}


void prs_returnstmt(VM* vm)
{
    if(g_currentcompiler->type == TYPE_SCRIPT)
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
        if(g_currentcompiler->type == TYPE_INITIALIZER)
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
    // for possible variable declarations in clause
    prs_beginscope(vm);
    prs_beginloopscope(vm);
    prs_consume(vm, TOKEN_LEFTPAREN, "Expect '(' after 'for'.");
    // initializer clause
    if(prs_matchtoken(vm, TOKEN_SEMICOLON))
    {
        // no initializer
    }
    else if(prs_matchtoken(vm, TOKEN_VAR))
    {
        // for clause scope only
        prs_vardecl(vm);
    }
    else
    {
        prs_exprstmt(vm);
    }

    // for for/while loops, loop starts here, with currentchunk()->count
    int loopstart = prs_currentchunk(vm)->count;

    //  the condition clause
    /* CONDITION CLAUSE
	1. If false, vm_pop the recently calculated expression and skip the loop
	2. if true, go to the body; see increment clause below
	*/
    int exitjump = -1;
    if(!prs_matchtoken(vm, TOKEN_SEMICOLON))
    {
        prs_expression(vm);
        prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // jump out of loop if condition is false
        exitjump = prs_emitjump(vm, OP_JUMPIFFALSE);
        // still need to figure this out, most likely just deleting 'temporary' constants in the scope
        prs_emitbyte(vm, OP_POP);
    }
    // the increment clause
    // if there is something else before the terminating ')'
    if(!prs_matchtoken(vm, TOKEN_RIGHTPAREN))
    {
        /*	INCEREMENT CLAUSE
		1. from the condition clause, first jump OVER the increment, to the body
		2. in the body, run the body
		3. jump BACK to the increment and run it
		4. from the increment jump BACK to the CONDITION clause, back to the cycle
		*/
        // jump the increment clause
        int bodyjump = prs_emitjump(vm, OP_JUMP);
        // starting index for increment
        int incrstart = prs_currentchunk(vm)->count;

        // set continue jump here, right after the increment statement
        prs_markcontjump(vm);
        // run the for expression
        prs_expression(vm);
        // vm_pop expression constant
        prs_emitbyte(vm, OP_POP);
        prs_consume(vm, TOKEN_RIGHTPAREN, "Expect ')' after for clauses.");

        // running the loop
        // goes back to the start of the CONDITION clause of the for loop
        prs_emitloop(vm, loopstart);
        loopstart = incrstart;
        prs_patchjump(vm, bodyjump);
    }
    // running the code inside the loop
    prs_statement(vm);

    prs_emitloop(vm, loopstart);

    // patch the jump in the loop body
    if(exitjump != -1)
    {
        prs_patchjump(vm, exitjump);
        // only vm_pop when THERE EXISTS A CONDITION from the clause
        prs_emitbyte(vm, OP_POP);
    }

    // patch break jumps, if available
    prs_patchbreakjumps(vm);

    prs_endloopscope(vm);
    prs_endscope(vm);
}

void prs_whilestmt(VM* vm)
{
    // index where the statement to loop starts
    int loopstart = prs_currentchunk(vm)->count;
    prs_beginloopscope(vm);

    // set jump for potential continue statement
    prs_markcontjump(vm);

    prs_expression(vm);


    // skip stament if condition is false
    int exitjump = prs_emitjump(vm, OP_JUMPIFFALSE);
    // vm_pop the last expression(true or false)
    prs_emitbyte(vm, OP_POP);

    prs_statement(vm);
    // method to 'loop' the instruction
    prs_emitloop(vm, loopstart);

    prs_patchjump(vm, exitjump);

    prs_emitbyte(vm, OP_POP);

    // patch break jumps, if available
    prs_patchbreakjumps(vm);

    prs_endloopscope(vm);
}

void prs_breakstmt(VM* vm)
{
    if(g_currentcompiler->loopcounttop < 0)
    {
        prs_error(vm, "Break statement must be enclosed in a loop");
        return;
    }

    if(++g_currentcompiler->breakjumpcounts[g_currentcompiler->loopcounttop] > UINT8_COUNT)
    {
        prs_error(vm, "Too many break statments in one loop");
        return;
    }

    int breakjump = prs_emitjump(vm, OP_JUMP);
    int loopdepth = g_currentcompiler->loopcounttop;
    int breakamount = g_currentcompiler->breakjumpcounts[loopdepth];
    g_currentcompiler->breakpatchjumps[g_currentcompiler->loopcounttop][breakamount - 1] = breakjump;

    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after break.");
}


void prs_continuestmt(VM* vm)
{
    if(g_currentcompiler->loopcounttop < 0)
    {
        prs_error(vm, "Continue statement must be enclosed in a loop");
        return;
    }

    if(g_currentcompiler->loopcounttop == g_currentcompiler->continuejumpcap)
    {
        int oldcapacity = g_currentcompiler->continuejumpcap;
        g_currentcompiler->continuejumpcap = memwrap_growcap(vm, oldcapacity);
        g_currentcompiler->continuejumps = memwrap_growarray(vm, int, g_currentcompiler->continuejumps, oldcapacity, g_currentcompiler->continuejumpcap);
    }

    prs_emitloop(vm, g_currentcompiler->continuejumps[g_currentcompiler->loopcounttop]);

    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after continue.");
}

void prs_repeatuntilstmt(VM* vm)
{
    // prs_consume(vm, TOKEN_LEFTBRACE, "Expect '{' after repeat.");
    int loopstart = prs_currentchunk(vm)->count;
    prs_beginloopscope(vm);
    prs_markcontjump(vm);

    // process the statement
    prs_statement(vm);

    prs_consume(vm, TOKEN_UNTIL, "Expect 'until' after repeat statement.");

    // get true or false
    prs_expression(vm);

    // emit loop if false op code
    prs_emitcondloop(vm, loopstart, false);

    // patch possible break jumps
    prs_patchbreakjumps(vm);

    prs_endloopscope(vm);
    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after until exppression.");
}

void prs_dowhilestmt(VM* vm)
{
    int loopstart = prs_currentchunk(vm)->count;
    prs_beginloopscope(vm);
    prs_markcontjump(vm);

    // process the statement
    prs_statement(vm);

    prs_consume(vm, TOKEN_WHILE, "Expect 'until' after repeat statement.");

    // get true or false
    prs_expression(vm);

    // emit loop if true op code
    prs_emitcondloop(vm, loopstart, true);

    // patch possible break jumps
    prs_patchbreakjumps(vm);

    prs_endloopscope(vm);
    prs_consume(vm, TOKEN_SEMICOLON, "Expect ';' after until exppression.");
}

void prs_synchronize(VM* vm)
{
    g_currentparser.panicmode = false;

    // basically turn off the 'error' mode and skips token until something that looks like a statement boundary is found
    // skips tokens indiscriminately until somehing that looks like a statement boundary(eg. semicolon) is found
    while(g_currentparser.current.type != TOKEN_EOF)
    {
        if(g_currentparser.previous.type == TOKEN_SEMICOLON)
            return;

        switch(g_currentparser.current.type)
        {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_RETURN:
                return;
            default:
                // do nothing
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
        // declare variable
        prs_vardecl(vm);
    }
    else
    {
        prs_statement(vm);
    }
    if(g_currentparser.panicmode)
    {
        // for errors
        prs_synchronize(vm);
    }
}

void prs_statement(VM* vm)
{
    if(prs_matchtoken(vm, TOKEN_RETURN))
    {
        // for functions return
        prs_returnstmt(vm);
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
    // parse initial { token
    else if(prs_matchtoken(vm, TOKEN_LEFTBRACE))
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
    // start scan/lexing
    scanner_init(source);
    Compiler compiler;
    prs_initcompiler(vm, &compiler, TYPE_SCRIPT);

    g_currentparser.haderror = false;
    g_currentparser.panicmode = false;

    // vm_call to advance once to 'pump' the g_currentscanner
    prs_advance(vm);
    // while EOF token is not met
    while(!prs_matchtoken(vm, TOKEN_EOF))
    {
        prs_declaration(vm);
    }
    // ends the expression with a return type
    ObjFunction* function = prs_endcompiler(vm);
    // if no error return true
    return g_currentparser.haderror ? NULL : function;
}


// marking compiler roots, for garbage collection
void prs_markroots(VM* vm)
{
    Compiler* compiler = g_currentcompiler;
    while(compiler != NULL)
    {
        mem_markobject(vm, (Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}

Chunk* prs_currentchunk(VM* vm)
{
    (void)vm;
    return &g_currentcompiler->function->chunk;
}

// to handle syntax errors
void prs_errorat(VM* vm, Token* token, const char* message)
{
    (void)vm;
    if(g_currentparser.panicmode)
    {
        // if an error already exists, no need to run other errors
        return;
    }
    g_currentparser.panicmode = true;

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
    g_currentparser.haderror = true;
}

// error from token most recently CONSUMED
void prs_error(VM* vm, const char* message)
{
    prs_errorat(vm, &g_currentparser.previous, message);
}


// handling error from token, the most current one being handed, not yet consumed
void prs_erroratcurrent(VM* vm, const char* message)
{
    // pass in the current parser
    prs_errorat(vm, &g_currentparser.current, message);
}


/* main compile functions */

// pump the compiler, basically go to / 'read' the next token, a SINGLE token
void prs_advance(VM* vm)
{
    //  store next parser as current
    g_currentparser.previous = g_currentparser.current;

    for(;;)
    {
        // gets next token, stores it for later use(the next scan)
        g_currentparser.current = scanner_scantoken();

        if(g_currentparser.current.type != TOKEN_ERROR)
        {
            // if error is not found break
            break;
        }
        // start is the location/pointer of the token source code
        prs_erroratcurrent(vm, g_currentparser.current.start);
    }
}


// advance while skipping the given parameter, give none to skip nothing
void prs_advancewhileskipping(VM* vm, TokenType type)
{
    //  store next parser as current
    g_currentparser.previous = g_currentparser.current;

    for(;;)
    {
        // gets next token, stores it for later use(the next scan)
        g_currentparser.current = scanner_scantoken();

        if(g_currentparser.current.type == type)
            continue;

        if(g_currentparser.current.type != TOKEN_ERROR)
        {
            // if error is not found break
            break;
        }
        // start is the location/pointer of the token source code
        prs_erroratcurrent(vm, g_currentparser.current.start);
    }
}


// SIMILAR to advance but there is a validation for a certain type
// syntax error comes from here, where it is known/expected what the next token will be
void prs_consume(VM* vm, TokenType type, const char* message)
{
    // if current token is equal to the token type being compared to
    if(g_currentparser.current.type == type)
    {
        prs_advance(vm);
        return;
    }
    // if consumes a different type, error
    prs_erroratcurrent(vm, message);
}

bool prs_check(VM* vm, TokenType type)
{
    (void)vm;
    // check if current matches given
    return g_currentparser.current.type == type;
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
    // sends previous line so runtime errors are associated with that line
    chunk_write(vm, prs_currentchunk(vm), byte, g_currentparser.previous.line);
}

// write chunk for multiple chunks, used to write an opcode followed by an operand(eg. in constants)
void prs_emitbytes(VM* vm, uint8_t byte1, uint8_t byte2)
{
    prs_emitbyte(vm, byte1);
    prs_emitbyte(vm, byte2);
}

// for looping statements
void prs_emitloop(VM* vm, int loopstart)
{
    prs_emitbyte(vm, OP_LOOP);

    // int below jumps back, + 2 accounting the OP_LOOP and the instruction's own operand
    int offset = prs_currentchunk(vm)->count - loopstart + 2;
    if(offset > UINT16_MAX)
        prs_error(vm, "Loop body too large.");

    prs_emitbyte(vm, (offset >> 8) & 0xff);
    prs_emitbyte(vm, offset & 0xff);
}

void prs_emitcondloop(VM* vm, int loopstart, bool state)
{
    if(state)
        prs_emitbyte(vm, OP_LOOPIFTRUE);
    else
        prs_emitbyte(vm, OP_LOOPIFFALSE);

    int offset = prs_currentchunk(vm)->count - loopstart + 2;
    if(offset > UINT16_MAX)
        prs_error(vm, "Loop body too large.");

    prs_emitbyte(vm, (offset >> 8) & 0xff);
    prs_emitbyte(vm, offset & 0xff);
}


int prs_emitjump(VM* vm, uint8_t instruction)
{
    /* backpatching */
    // writes a placeholder operand for jump offset
    prs_emitbyte(vm, instruction);
    // hexadecimal number with value of 255
    prs_emitbyte(vm, 0xff);
    prs_emitbyte(vm, 0xff);

    // basically, get the difference in bytes before the two 0xff is added
    return prs_currentchunk(vm)->count - 2;
}

//  emit specific return type
void prs_emitreturn(VM* vm)
{
    // class constructor
    if(g_currentcompiler->type == TYPE_INITIALIZER)
    {
        // return the instance
        prs_emitbytes(vm, OP_GETLOCAL, 0);
    }
    else
    {
        // for functions that return nothing
        prs_emitbyte(vm, OP_NULL);
    }
    // emit return type at the end of a compiler
    prs_emitbyte(vm, OP_RETURN);
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
    // return as byte, the byte being the INDEX of the constantin the constats array
    return (uint8_t)constant;
}

// for constant emit the opcode, then the index
void prs_emitconst(VM* vm, Value value)
{
    // add value to constant table
    prs_emitbytes(vm, OP_CONSTANT, prs_makeconst(vm, value));
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
    // right shift by 8, then bitwise AND with 255(oxff is 111111)
    prs_currentchunk(vm)->code[offset] = (jump >> 8) & 0xff;
    // only AND
    prs_currentchunk(vm)->code[offset + 1] = jump & 0xff;
}

// initialize the compiler
void prs_initcompiler(VM* vm, Compiler* compiler, FunctionType type)
{
    // the 'outer' compiler
    compiler->enclosing = g_currentcompiler;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localcount = 0;
    compiler->scopedepth = 0;
    compiler->function = obj_mkfunction(vm);
    // current is the global variable pointer for the Compiler struct, point to to the parameter
    // basically assign the global pointer
    g_currentcompiler = compiler;

    // for functions
    if(type != TYPE_SCRIPT)
    {
        // function name handled here
        g_currentcompiler->function->name = obj_copystring(vm, g_currentparser.previous.start, g_currentparser.previous.length);
    }

    // compiler implicitly claims slot zero for local variables
    Local* local = &g_currentcompiler->locals[g_currentcompiler->localcount++];
    local->iscaptured = false;

    // for this tags
    // for none function types, for class methods
    if(type != TYPE_FUNCTION)
    {
        local->name.start = "this";
        local->name.length = 4;
    }
    // for functions
    else
    {
        local->name.start = "";
        local->name.length = 0;
    }

    // for loop scopes, for break and continue statements
    compiler->loopcounttop = -1;
    compiler->continuejumpcap = 4;
    compiler->continuejumps = memwrap_allocate(vm, int, 4);

    // use memset to initialize array to 0
    memset(compiler->breakjumpcounts, 0, UINT8_COUNT * sizeof(compiler->breakjumpcounts[0]));
}

ObjFunction* prs_endcompiler(VM* vm)
{
    prs_emitreturn(vm);
    ObjFunction* function = g_currentcompiler->function;

    memwrap_free(vm, int, g_currentcompiler->continuejumps);


    // for debugging
#ifdef DEBUG_PRINT_CODE
    if(!g_currentparser.haderror)
    {
        // if name is NULL then it is the Script type(main()
        dbg_disasmchunk(vm, prs_currentchunk(vm), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    // return back to enclosing compiler after function
    g_currentcompiler = g_currentcompiler->enclosing;
    return function;// return to free
}

void prs_beginscope(VM* vm)
{
    (void)vm;
    g_currentcompiler->scopedepth++;
}

void prs_endscope(VM* vm)
{
    g_currentcompiler->scopedepth--;

    // remove variables out of scope
    while(g_currentcompiler->localcount > 0 && g_currentcompiler->locals[g_currentcompiler->localcount - 1].depth > g_currentcompiler->scopedepth)
    {
        /* at the end of a block scope, when the compiler emits code to free the stack slot for the locals, 
		tell which one to hoist to the heap
		*/
        // if it is captured/used
        if(g_currentcompiler->locals[g_currentcompiler->localcount - 1].iscaptured)
        {
            // op code to move the upvalue to the heap
            prs_emitbyte(vm, OP_CLOSEUPVALUE);
        }
        else
        {
            // if not used anymore/capture simply vm_pop the value off the stack
            prs_emitbyte(vm, OP_POP);
        }

        g_currentcompiler->localcount--;
    }
}

// loop enclosing
void prs_beginloopscope(VM* vm)
{
    (void)vm;
    g_currentcompiler->loopcounttop++;
}

void prs_endloopscope(VM* vm)
{
    (void)vm;
    if(g_currentcompiler->breakjumpcounts[g_currentcompiler->loopcounttop] > 0)
        g_currentcompiler->breakjumpcounts[g_currentcompiler->loopcounttop] = 0;

    g_currentcompiler->loopcounttop--;
}

// mark current chunk for continue jump
void prs_markcontjump(VM* vm)
{
    g_currentcompiler->continuejumps[g_currentcompiler->loopcounttop] = prs_currentchunk(vm)->count;
}

// patch available break jumps
void prs_patchbreakjumps(VM* vm)
{
    for(int i = 0; i < g_currentcompiler->breakjumpcounts[g_currentcompiler->loopcounttop]; i++)
    {
        prs_patchjump(vm, g_currentcompiler->breakpatchjumps[g_currentcompiler->loopcounttop][i]);
    }
}

/* variable declarations */
uint8_t prs_makeconstident(VM* vm, Token* name)
{
    // add to constant table
    return prs_makeconst(vm, obj_mkobject(obj_copystring(vm, name->start, name->length)));
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
    // walk through the local variables
    for(int i = compiler->localcount - 1; i >= 0; i--)
    {
        Local* local = &compiler->locals[i];
        if(prs_identequal(vm, name, &local->name))
        {
            if(local->depth == -1)
            {
                prs_error(vm, "Cannot read local variable in its own initializer.");
            }
            // found the var, return the index
            return i;
        }
    }
    // not found, name is global variable
    return -1;
}


// add upvalue
int prs_addupvalue(VM* vm, Compiler* compiler, uint8_t index, bool isl)
{
    // get current upvalue count
    int upvaluecount = compiler->function->upvaluecount;

    // check whether the upvalue has already been declared
    for(int i = 0; i < upvaluecount; i++)
    {
        // get pointer for each upvalue in the array
        Upvalue* upvalue = &compiler->upvalues[i];
        if(upvalue->index == index && upvalue->islocal == isl)
        {
            // if found, return the index of the upvalue in the upvalue array
            return i;
        }
    }

    if(upvaluecount == UINT8_COUNT)
    {
        prs_error(vm, "Too many closure variables");
        return 0;
    }

    // compiler keeps an array of upvalue structs to track closed-over identifiers
    // indexes in the array match the indexes of ObjClosure at runtime
    // insert to upvalues array
    // insert bool status
    compiler->upvalues[upvaluecount].islocal = isl;
    // insert index
    compiler->upvalues[upvaluecount].index = index;
    // increase count and return
    return compiler->function->upvaluecount++;
}


/*	for closures
- prs_resolveupvalue looks for a local variable declared in any of the surrounding functions
- if it finds one it returns the index for that upvalue variable, otherwise returns -1
*/
int prs_resolveupvalue(VM* vm, Compiler* compiler, Token* name)
{
    if(compiler->enclosing == NULL)
    {
        // if in main()
        return -1;
    }
    
    // looks for local value in enclosing function/compiler
    int local = prs_resolvelocal(vm, compiler->enclosing, name);
    if(local != -1)
    {
        // mark local is captured/used by and upvalue
        compiler->enclosing->locals[local].iscaptured = true;
        // create up value
        return prs_addupvalue(vm, compiler, (uint8_t)local, true);
    }

    // recursion to solve nested upvalues
    // recursive vm_call right in the middle
    // if the enclosing function is main() (NULL), it returns -1
    int upvalue = prs_resolveupvalue(vm, compiler->enclosing, name);
    if(upvalue != -1)
    {
        return prs_addupvalue(vm, compiler, (uint8_t)upvalue, true);
    }


    return -1;
}


void prs_addlocal(VM* vm, Token name)
{
    if(g_currentcompiler->localcount == UINT8_COUNT)
    {
        prs_error(vm, "Too many local variables in block.");
        return;
    }

    Local* local = &g_currentcompiler->locals[g_currentcompiler->localcount++];
    local->name = name;
    // for cases where a variable name is redefined inside another scope, using the variable itself
    local->depth = -1;
    local->iscaptured = false;
}

void prs_declarevariable(VM* vm)
{
    int i;
    Local* local;
    Token* name;
    // global vars are implicitly declared, and are late bound, not 'initialized' here but in the VM
    if(g_currentcompiler->scopedepth == 0)
        return;

    /* local variable declaration happens below */
    name = &g_currentparser.previous;

    // to not allow two variable declarations to have the same name
    // loop only checks to a HIGHER SCOPE; another block overlaping/shadowing is allowed
    // work backwards
    for(i = g_currentcompiler->localcount - 1; i >= 0; i--)
    {
        local = &g_currentcompiler->locals[i];
        // if reach beginning of array(highest scope)
        if(local->depth != -1 && local->depth < g_currentcompiler->scopedepth)
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

uint8_t prs_parsevariable(VM* vm, const char* errormessage)
{
    // requires next token to be an identifier
    prs_consume(vm, TOKEN_IDENTIFIER, errormessage);

    prs_declarevariable(vm);
    if(g_currentcompiler->scopedepth > 0)
    {
        // if scopedepth is not 0, then it is a local not global var
        return 0;
    }
    // return a dummy index
    // at runtime, locals are not looked up by name so no need to insert them to a table
    // return index from the constant table
    return prs_makeconstident(vm, &g_currentparser.previous);
}


void prs_markinitialized(VM* vm)
{
    (void)vm;
    if(g_currentcompiler->scopedepth == 0)
    {
        // if global return
        return;
    }    
    g_currentcompiler->locals[g_currentcompiler->localcount - 1].depth = g_currentcompiler->scopedepth;
}

void prs_definevariable(VM* vm, uint8_t global)
{
    if(g_currentcompiler->scopedepth > 0)
    {
        prs_markinitialized(vm);
        return;
    }
    // opcode for declaration and the constant itself
    prs_emitbytes(vm, OP_DEFINEGLOBAL, global);
}


// for function arguments, returns number of arguments
// each argument expression generates code which leaves value on the stack in preparation for the vm_call
uint8_t prs_parsearglist(VM* vm)
{
    uint8_t argc = 0;
    // if ) has not been reached
    if(!prs_check(vm, TOKEN_RIGHTPAREN))
    {
        do
        {
            // collect the arguments
            prs_expression(vm);
            // cannot have more than 255 arguments as each operand is a single byte(uint8_t)
            if(argc == 255)
            {
                prs_error(vm, "Cannot have more than 255 arguments.");
            }

            argc++;
        } while(prs_matchtoken(vm, TOKEN_COMMA));
    }

    prs_consume(vm, TOKEN_RIGHTPAREN, "Expect ')' after argument list.");
    return argc;
}

void rule_and(VM* vm, bool canassign)
{
    (void)canassign;
    // left hand side is already compiled,
    int endjump = prs_emitjump(vm, OP_JUMPIFFALSE);
    // and if it is false skip it and go to next
    prs_emitbyte(vm, OP_POP);
    prs_parseprecedence(vm, PREC_AND);
    prs_patchjump(vm, endjump);
}


// for binary, eg. 5 + 4
// or INFIX parser, where the operator is in the middle
// entire left hand expression has been compiled, and the infix operator has been consumed
// rule_binary() handles the rest of the arithmetic operator
void rule_binary(VM* vm, bool canassign)
{
    (void)canassign;
    // remember type of operator, already consumed
    TokenType opertype = g_currentparser.previous.type;

    // compile right operand
    // the BIDMAS rule, operands in the right side have HIGHER PRECEDENCE
    ParseRule* rule = prs_getrule(vm, opertype);
    // as binary operators are LEFT ASSOCIATIVE
    // recursively vm_call prs_parseprecedence again
    prs_parseprecedence(vm, (Precedence)(rule->precedence + 1));// conert from rule to enum(precedence) type

    switch(opertype)
    {
            // note how NOT opcode is at the end
            // six binary operators for three instructions only(greater, not, equal)
        case TOKEN_BANGEQUAL:
            // add equal and not to the stack
            prs_emitbytes(vm, OP_EQUAL, OP_NOT);
            break;
        case TOKEN_EQUALEQUAL:
            prs_emitbyte(vm, OP_EQUAL);
            break;
        case TOKEN_GREATERTHAN:
            prs_emitbyte(vm, OP_GREATER);
            break;
        case TOKEN_GREATEREQUAL:
            prs_emitbytes(vm, OP_LESS, OP_NOT);
            break;
        case TOKEN_LESSTHAN:
            prs_emitbyte(vm, OP_LESS);
            break;
        case TOKEN_LESSEQUAL:
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
            {
                // unreachable
                return;
            }
            break;
    }
}


// for function calls
void rule_parsecall(VM* vm, bool canassign)
{
    (void)canassign;
    // again, assumes the function itself(its vm_call name) has been placed on the codestream stack
    // compile arguments using prs_parsearglist
    uint8_t argc = prs_parsearglist(vm);
    // write on the chunk
    prs_emitbytes(vm, OP_CALL, argc);
}

// class members/fields/properties
void rule_dot(VM* vm, bool canassign)
{
    prs_consume(vm, TOKEN_IDENTIFIER, "Expect propery name after class instance.");
    // already consumed
    uint8_t name = prs_makeconstident(vm, &g_currentparser.previous);
    // assignment
    if(canassign && prs_matchtoken(vm, TOKEN_EQUAL))
    {
        // evalute expression to be set
        prs_expression(vm);
        prs_emitbytes(vm, OP_SETPROPERTY, name);
    }
    // for running class methods, access the method and vm_call it at the same time
    else if(prs_matchtoken(vm, TOKEN_LEFTPAREN))
    {
        uint8_t argc = prs_parsearglist(vm);

        /* new OP_INVOKE opcode that takes two operands:
		1. the index of the property name in the constant table
		2. the number of arguments passed in the methods
		*** combines OP_GETPROPERTY and OP_CALL
		*/
        prs_emitbytes(vm, OP_INVOKE, name);
        prs_emitbyte(vm, argc);
    }
    else
    {
        // simply get
        prs_emitbytes(vm, OP_GETPROPERTY, name);
    }
}

void rule_literal(VM* vm, bool canassign)
{
    (void)canassign;
    switch(g_currentparser.previous.type)
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

        default:
            {
                // unreachable
                return;
            }
            break;
    }
}

// parentheses for rule_grouping
void rule_grouping(VM* vm, bool canassign)
{
    (void)canassign;
    // assume initial ( has already been consumed, and recursively vm_call to expression() to compile between the parentheses
    prs_expression(vm);
    // expects a right parentheses, if not received then  error
    prs_consume(vm, TOKEN_RIGHTPAREN, "Expect ')' after expression.");
}


/* parsing the tokens */
void rule_number(VM* vm, bool canassign)
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
    double value = strtod(g_currentparser.previous.start, NULL);
    prs_emitconst(vm, obj_mknumber(value));
}

void rule_or(VM* vm, bool canassign)
{
    (void)canassign;
    // jump if left hand side is true
    // if left is false jump directly to right hand
    int elsejump = prs_emitjump(vm, OP_JUMPIFFALSE);
    // if not skipped(as left is true) jump the right hand
    int endjump = prs_emitjump(vm, OP_JUMP);

    prs_patchjump(vm, elsejump);
    prs_emitbyte(vm, OP_POP);

    prs_parseprecedence(vm, PREC_OR);
    prs_patchjump(vm, endjump);
}

// 'initialize' the string here
void rule_string(VM* vm, bool canassign)
{
    (void)canassign;
    // in a string, eg. "hitagi", the quotation marks are trimmed
    prs_emitconst(vm, obj_mkobject(obj_copystring(vm, g_currentparser.previous.start + 1, g_currentparser.previous.length - 2)));
}

// declare/vm_call variables
void rule_namedvar(VM* vm, Token name, bool canassign)
{
    uint8_t getop, setop;
    // try find a local variable with a given name
    int arg = prs_resolvelocal(vm, g_currentcompiler, &name);
    if(arg != -1)
    {
        getop = OP_GETLOCAL;
        setop = OP_SETLOCAL;
    }
    // for upvalues
    else if((arg = prs_resolveupvalue(vm, g_currentcompiler, &name)) != -1)
    {
        getop = OP_GET_UPVALUE;
        setop = OP_SETUPVALUE;
    }
    else
    {
        arg = prs_makeconstident(vm, &name);
        getop = OP_GETGLOBAL;
        setop = OP_SETGLOBAL;
    }


    // test case to check whether it is a get(just the name) or a reassignment
    // if a = follows right after
    if(canassign && prs_matchtoken(vm, TOKEN_EQUAL))
    {
        prs_expression(vm);
        // reassignment/set
        prs_emitbytes(vm, setop, (uint8_t)arg);
    }
    else
    {
        // as normal get
        prs_emitbytes(vm, getop, (uint8_t)arg);
    }
}

void rule_variable(VM* vm, bool canassign)
{
    rule_namedvar(vm, g_currentparser.previous, canassign);
}

// for super classes, token that mimics as if a user types in 'super'
Token prs_makesyntoken(VM* vm, const char* text)
{
    Token token;
    (void)vm;
    token.start = text;
    // strlen to get char* length
    token.length = (int)strlen(text);
    return token;
}

// for super calls
void rule_super(VM* vm, bool canassign)
{
    (void)canassign;
    // if token is not inside a class
    if(g_currentclass == NULL)
    {
        prs_error(vm, "'super' can only be initialized inside a class.");
    }
    // if class has no parent class
    else if(!g_currentclass->hassuper)
    {
        prs_error(vm, "'super' cannot be used on a class with no parent class.");
    }
    prs_consume(vm, TOKEN_DOT, "Expect '.' after 'super'.");
    prs_consume(vm, TOKEN_IDENTIFIER, "Expect parent class method identifier.");
    uint8_t name = prs_makeconstident(vm, &g_currentparser.previous);// get identifier index
    /*
	in order to access a superclass method on the CURRENT INSTANCE, runtime needs both the receiver and the superclass
	of the surrounding method's class.
	1. first rule_namedvar vm_call generates code to look up the current receiver and vm_push it to the stack
	2. second rule_namedvar emits code to look up the superclass and vm_push that on top
	*/
    rule_namedvar(vm, prs_makesyntoken(vm, "this"), false);
    // if there is a parameter list, vm_invoke super method
    if(prs_matchtoken(vm, TOKEN_LEFTPAREN))
    {
        uint8_t argc = prs_parsearglist(vm);
        rule_namedvar(vm, prs_makesyntoken(vm, "super"), false);
        // super vm_invoke opcode
        prs_emitbytes(vm, OP_SUPERINVOKE, name);
        prs_emitbyte(vm, argc);
    }
    else
    {
        rule_namedvar(vm, prs_makesyntoken(vm, "super"), false);
        prs_emitbytes(vm, OP_GETSUPER, name);
    }
}

// for class methods
void rule_this(VM* vm, bool canassign)
{
    (void)canassign;
    // if not inside a class
    if(g_currentclass == NULL)
    {
        prs_error(vm, "Cannot use 'this' outside of class.");
        return;
    }
    // always false
    rule_variable(vm, false);
}

// unary
void rule_unary(VM* vm, bool canassign)
{
    (void)canassign;
    // leading - token has already been consumed
    TokenType opertype = g_currentparser.previous.type;
    // compile operand
    prs_expression(vm);
    switch(opertype)
    {
        case TOKEN_BANG:
            prs_emitbyte(vm, OP_NOT);
            break;

            // OP_NEGATE should be emitted last, AFTER the constant itself
            // eg. say 4 - 5; 5 needs to be emitted and added to the chunk->constants first before OP_NEGATE
            /*
            * it is important to take note of the precedence
            * e.g -a.b + 3;
            * when the unary negation is called, all of a.b + 3 will be consumed in expression(). Hence, a method is needed
            * to STOP when + is found, or generally when an operand of LOWER PRECEDENCE is found
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
    // point stacktop to the begininng of the empty array
    // stack array(vm->stack) is already indirectly declared, hence no need to allocate memory for it
    vm->stacktop = vm->stack;
    vm->framecount = 0;
    vm->openupvalues = NULL;
}


// IMPORTANT
// variadic function ( ... ), takes a varying number of arguments
void vm_rterror(VM* vm, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);


    // printing the stack trace for the function
    // print out each function that was still executing when the program died and where the execution was at the point it died
    for(int i = vm->framecount - 1; i >= 0; i--)
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
    // pulls from topmost CallFrame on the stack
    CallFrame* frame = &vm->frames[vm->framecount - 1];
    // - 1 to deal with the 1 added initially for the main() CallFrame
    size_t instruction = frame->ip - frame->closure->function->chunk.code - 1;
    int line = frame->closure->function->chunk.lines[instruction];
    fprintf(stderr, "Error in script at [Line %d]\n", line);

    vm_resetstack(vm);
}

void vm_defnative(VM* vm, const char* name, NativeFn function)
{
    // strlen to get char* length
    vm_push(vm, obj_mkobject(obj_copystring(vm, name, (int)strlen(name))));
    vm_push(vm, obj_mkobject(obj_mknative(vm, function)));
    htable_set(vm, &vm->globals, obj_asstring(vm->stack[0]), vm->stack[1]);
    vm_pop(vm);
    vm_pop(vm);
}

Value cfn_clock(VM* vm, int argc, Value* args)
{
    (void)vm;
    (void)argc;
    (void)args;
    // returns elapsed time since program was running
    return obj_mknumber((double)clock() / CLOCKS_PER_SEC);
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
                    if(obj_isstring(args[i]))
                    {
                        os = obj_asstring(args[i]);
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
    return obj_mknumber(0);
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
        if(obj_isnumber(args[0]))
        {
            buf[0] = args[0].as.number;
            buf[1] = 0;
            return obj_mkobject(obj_copystring(vm, buf, 1));
        }
        vm_rterror(vm, "expected #1 to be number");
    }
    vm_rterror(vm, "too few arguments");
    return obj_nullval;
}


void vm_init(VM* vm)
{
    // initialiing the Value stack, also initializing the callframe count
    vm_resetstack(vm);
    vm->objects = NULL;
    htable_init(vm, &vm->globals);
    htable_init(vm, &vm->strings);
    // initializing gray marked obj stack for garbage collection
    vm->graycap = 0;
    vm->graycount = 0;
    vm->graystack = NULL;

    // self adjusting heap to control frequency of GC
    vm->totalalloc = 0;
    vm->nextgc = 1024 * 1024;

    // init initalizer string
    vm->initstring = NULL;
    vm->initstring = obj_copystring(vm, "init", 4);

    vm_defnative(vm, "clock", cfn_clock);
    vm_defnative(vm, "print", cfn_print);
    vm_defnative(vm, "println", cfn_println);
    vm_defnative(vm, "chr", cfn_chr);
}

void vm_free(VM* vm)
{
    vm->initstring = NULL;
    // free all objects, from vm->objects
    mem_freeobjlist(vm);
    htable_free(vm, &vm->globals);
    htable_free(vm, &vm->strings);
}

/* stack operations */
void vm_push(VM* vm, Value value)
{
    // * in front of the pointer means the rvalue itself, assign value(parameter) to it
    *vm->stacktop = value;
    vm->stacktop++;
}

Value vm_pop(VM* vm)
{
    // first move the stack BACK to get the last element(stacktop points to ONE beyond the last element)
    vm->stacktop--;
    return *vm->stacktop;
}
/* end of stack operations */

// PEEK from the STACK, AFTER the compiler passes it through
// return a value from top of the stack but does not vm_pop it, distance being how far down
// this is a C kind of accessing arrays/pointers
Value vm_peek(VM* vm, int distance)
{
    return vm->stacktop[-1 - distance];
}

/* for vm_call stacks/functions  */
bool vm_call(VM* vm, ObjClosure* closure, int argc)
{
    // if number of parameters does not match
    if((closure->function->arity != -1) && (argc != closure->function->arity))
    {
        vm_rterror(vm, "Expected %d arguments but got %d", closure->function->arity, argc);
        return false;
    }
    // as CallFrame is an array, to ensure array does not overflow
    if(vm->framecount == FRAMES_MAX)
    {
        vm_rterror(vm, "Stack overflow.");
        return false;
    }
    // get pointer to next in frame array
    // initializes callframe to the top of the stack
    CallFrame* frame = &vm->frames[vm->framecount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    // set up slots pointer to give frame its window into the stack
    // ensures everyting lines up
    // slots is the 'starting pointer' for the function cll
    frame->slots = vm->stacktop - argc - 1;
    return true;
}

bool vm_callvalue(VM* vm, Value callee, int argc)
{
    if(obj_isobject(callee))
    {
        switch(obj_type(callee))
        {
            case OBJ_BOUND_METHOD:
            {
                // get ObjBoundMethod from value type(callee)
                ObjBoundMethod* bound = obj_asboundmethod(callee);
                // set [-] inside square brackes of top stack pointer to go down the stack
                vm->stacktop[-argc - 1] = bound->receiver;
                //	run vm_call to execute
                return vm_call(vm, bound->method, argc);
            }
            // create class instance
            case OBJ_CLASS:
            {
                ObjClass* klass = obj_asclass(callee);
                // create new instance here
                // - argcounts as above values are parameters
                vm->stacktop[-argc - 1] = obj_mkobject(obj_mkinstance(vm, klass));

                // initializer
                Value initializer;
                // if we find one from the table
                // have a vm->initstring as 'token', ObjString type
                if(htable_get(vm, &klass->methods, vm->initstring, &initializer))
                {
                    return vm_call(vm, obj_asclosure(initializer), argc);
                }
                // if there ARE arguments but the initalizer method cannot be found
                else if(argc != 0)
                {
                    vm_rterror(vm, "Expected 0  arguments but got %d\n", argc);
                    return false;
                }

                return true;
            }
            // ensure type is function
            case OBJ_CLOSURE:
                // vm_call to function happens here
                return vm_call(vm, obj_asclosure(callee), argc);

            case OBJ_NATIVE:
            {
                NativeFn native = obj_asnative(callee);
                Value result = native(vm, argc, vm->stacktop - argc);
                // remove vm_call and arguments from the stack
                vm->stacktop -= argc + 1;
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


bool vm_invokefromclass(VM* vm, ObjClass* klass, ObjString* name, int argc)
{
    Value method;
    if(!htable_get(vm, &klass->methods, name, &method))
    {
        vm_rterror(vm, "Undefined property '%s'.", name->chars);
        return false;
    }

    return vm_call(vm, obj_asclosure(method), argc);
}


// vm_invoke class method, access method + vm_call method
bool vm_invoke(VM* vm, ObjString* name, int argc)
{
    // grab the receiver of the stack
    Value receiver = vm_peek(vm, argc);

    // vm_call method with wrong type, not an objinstance type
    if(!obj_isinstance(receiver))
    {
        vm_rterror(vm, "Tried to vm_invoke a method from a non instance object.");
        return false;
    }

    ObjInstance* instance = obj_asinstance(receiver);

    // for fields()
    Value value;
    if(htable_get(vm, &instance->fields, name, &value))
    {
        vm->stacktop[-argc - 1] = value;
        return vm_callvalue(vm, value, argc);
    }

    // actual function that searches for method and calls it
    return vm_invokefromclass(vm, instance->classobject, name, argc);
}


// bind method and wrap it in a new ObjBoundMethod
bool vm_bindmethod(VM* vm, ObjClass* klass, ObjString* name)
{
    Value method;
    // get method from table and bind it
    if(!htable_get(vm, &klass->methods, name, &method))
    {
        // if method not found
        vm_rterror(vm, "Undefined property %s.", name->chars);
        return false;
    }
    // wrap method in a new ObjBoundMethodd
    ObjBoundMethod* bound = obj_mkboundmethod(vm, vm_peek(vm, 0), obj_asclosure(method));
    // vm_pop the class instance
    vm_pop(vm);
    vm_push(vm, obj_mkobject(bound));
    return true;
}


// get corresponding upvalue
ObjUpvalue* vm_captureupvalue(VM* vm, Value* local)
{
    // set up the linked list
    ObjUpvalue* prevupvalue = NULL;
    // assign at the start of the list
    ObjUpvalue* upvalue = vm->openupvalues;

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

    ObjUpvalue* createdupvalue = obj_mkupvalue(vm, local);
    // insert at the front
    createdupvalue->next = upvalue;
    // ran out of values to search
    if(prevupvalue == NULL)
    {
        // set pointer to the newly added upvalue
        vm->openupvalues = createdupvalue;
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
void vm_closeupvalues(VM* vm, Value* last)
{
    while(vm->openupvalues != NULL && vm->openupvalues->location >= last)
    {
        // pointer to list of openupvalues
        ObjUpvalue* upvalue = vm->openupvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm->openupvalues = upvalue->next;
    }
}

// defining method for class type
void vm_defmethod(VM* vm, ObjString* name)
{
    // method/closure is at the top of the stack
    Value method = vm_peek(vm, 0);
    // class is at the 2nd top
    ObjClass* klass = obj_asclass(vm_peek(vm, 1));
    // add to hashtable
    htable_set(vm, &klass->methods, name, method);
    // vm_pop the method
    vm_pop(vm);
}


// comparison for OP_NOT
bool vm_isfalsey(VM* vm, Value value)
{
    (void)vm;
    // return true if value is the null type or if it is a false bool type
    bool test = obj_isnull(value) || (obj_isbool(value) && !obj_asbool(value));

    return test;
}

// string concatenation
void vm_concatenate(VM* vm)
{
    // vm_peek, so we do not vm_pop it off if calling a GC is needed
    ObjString* second = obj_asstring(vm_peek(vm, 0));
    ObjString* first = obj_asstring(vm_peek(vm, 1));

    int length = first->length + second->length;
    // dynamically allocate memory for the char, chars is now a NULL string
    char* chars = memwrap_allocate(vm, char, length + 1);

    /* NOTE ON C STRINGS, NULL VS EMPTY
	-> null string has no elements, it is an empty charray, ONLY DECLARED
	-> an empty string has the null character '/0'
	*/

    // IMPORTANt -> use memcpy when assinging to a char* pointer
    memcpy(chars, first->chars, first->length);
    // remember to add the first length of bits to chars again, so it will START AFTER the given offset
    memcpy(chars + first->length, second->chars, second->length);
    chars[length] = '\0';
    // declare new ObjString ptr
    ObjString* result = obj_takestring(vm, chars, length);
    // vm_pop the two strings, garbage collection
    vm_pop(vm);
    vm_pop(vm);
    vm_push(vm, obj_mkobject(result));
}


/* starting point of the compiler */
InterpretResult vm_interpret(VM* vm, const char* source)
{
    ObjFunction* function = prs_compile(vm, source);
    if(function == NULL)
    {
        // NULL gets passed from compiler
        return STATUS_COMPILEFAIL;
    }
    vm_push(vm, obj_mkobject(function));
    ObjClosure* closure = obj_mkclosure(vm, function);
    vm_pop(vm);
    vm_push(vm, obj_mkobject(closure));
    // 0 params for main()
    vm_callvalue(vm, obj_mkobject(closure), 0);
    return vm_run(vm);
}


// run the chunk
// most IMPORTANT part of the interpreter
InterpretResult vm_run(VM* vm)
{
    CallFrame* frame = &vm->frames[vm->framecount - 1];


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
        for(Value* slot = vm->stack; slot < vm->stacktop; slot++)
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
                // if next value is not a number
                if(!obj_isnumber(vm_peek(vm, 0)))
                {
                    //printf("\nnot a number\n"); it actually works
                    vm_rterror(vm, "Operand must be a number.");
                    return STATUS_RUNTIMEFAIL;
                }

                vm_push(vm, obj_mknumber(-obj_asnumber(vm_pop(vm))));
                // negates the last element of the stack
                break;

            // literals
            case OP_NULL:
                vm_push(vm, obj_nullval);
                break;
            case OP_TRUE:
                vm_push(vm, obj_mkbool(true));
                break;
            case OP_FALSE:
                vm_push(vm, obj_mkbool(false));
                break;

            // binary opcode
            case OP_ADD:
            {
                // if last two constants are strings
                if(obj_isstring(vm_peek(vm, 0)) && obj_isstring(vm_peek(vm, 1)))
                {
                    vm_concatenate(vm);
                }
                else if(obj_isnumber(vm_peek(vm, 0)) && obj_isnumber(vm_peek(vm, 1)))
                {
                    // in the book, macro is not used and a new algorithm is used directly
                    // initialize new Value struct (obj_mknumber) here
                    vmc_binaryop(obj_mknumber, +, double);
                }
                // handle errors dynamically here
                else
                {
                    vm_rterror(vm, "Operands are incompatible.");
                    return STATUS_RUNTIMEFAIL;
                }
                break;
            }

            case OP_SUBTRACT:
                vmc_binaryop(obj_mknumber, -, double);
                break;
            case OP_MULTIPLY:
                vmc_binaryop(obj_mknumber, *, double);
                break;
            case OP_DIVIDE:
                vmc_binaryop(obj_mknumber, /, double);
                break;

            case OP_MODULO:
                vmc_binaryop(obj_mknumber, %, int);
                break;

            case OP_NOT:
                // again, pops most recent one from the stack, does the operation on it, and pushes it back
                vm_push(vm, obj_mkbool(vm_isfalsey(vm, vm_pop(vm))));
                break;

            // for switch eqal
            case OP_SWITCHEQUAL:
            {
                // only vm_pop second value
                Value b = vm_pop(vm);
                // vm_peek topmost, the first value
                Value a = vm_peek(vm, 0);
                vm_push(vm, obj_mkbool(obj_valequal(vm, a, b)));
                break;
            }
            // implemenation comparison done here
            case OP_EQUAL:
            {
                Value b = vm_pop(vm);
                Value a = vm_pop(vm);
                vm_push(vm, obj_mkbool(obj_valequal(vm, a, b)));
                break;
            }
            case OP_GREATER:
                vmc_binaryop(obj_mkbool, >, double);
                break;
            case OP_LESS:
                vmc_binaryop(obj_mkbool, <, double);
                break;

            case OP_POP:
                vm_pop(vm);
                break;

            case OP_GETLOCAL:
            {
                uint8_t slot = vmc_readbyte();
                // pushes the value to the stack where later instructions can read it
                vm_push(vm, frame->slots[slot]);
                break;
            }

            case OP_SETLOCAL:
            {
                uint8_t slot = vmc_readbyte();
                // all the local var's VARIABLES are stored inside vm->stack
                // takes from top of the stack and stores it in the stack slot
                frame->slots[slot] = vm_peek(vm, 0);
                break;
            }

            case OP_DEFINEGLOBAL:
            {
                // get name from constant table
                ObjString* name = vmc_readstring();
                // take value from the top of the stack
                htable_set(vm, &vm->globals, name, vm_peek(vm, 0));
                vm_pop(vm);
                break;
            }

            case OP_GETGLOBAL:
            {
                // get the name
                ObjString* name = vmc_readstring();
                // create new Value
                Value value;
                // if key not in hash table
                if(!htable_get(vm, &vm->globals, name, &value))
                {
                    vm_rterror(vm, "Undefined variable '%s'.", name->chars);
                    return STATUS_RUNTIMEFAIL;
                }
                vm_push(vm, value);
                break;
            }

            case OP_SETGLOBAL:
            {
                ObjString* name = vmc_readstring();
                // if key not in hash table
                if(htable_set(vm, &vm->globals, name, vm_peek(vm, 0)))
                {
                    // delete the false name
                    htable_delete(vm, &vm->globals, name);
                    vm_rterror(vm, "Undefined variable '%s'.", name->chars);
                    return STATUS_RUNTIMEFAIL;
                }
                break;
            }

            // upvalues set/get
            case OP_GET_UPVALUE:
            {
                // read index
                uint8_t slot = vmc_readbyte();
                // vm_push the value to the stack
                vm_push(vm, *frame->closure->upvalues[slot]->location);
                break;
            }

            case OP_SETUPVALUE:
            {
                // read index
                uint8_t slot = vmc_readbyte();
                // set to the topmost stack
                *frame->closure->upvalues[slot]->location = vm_peek(vm, 0);
                break;
            }

            case OP_GETPROPERTY:
            {
                // to make sure only instances are allowed to have fields
                if(!obj_isinstance(vm_peek(vm, 0)))
                {
                    vm_rterror(vm, "Only instances have properties.");
                    return STATUS_RUNTIMEFAIL;
                }
                // get instance from top most stack
                ObjInstance* instance = obj_asinstance(vm_peek(vm, 0));
                // get identifier name
                ObjString* name = vmc_readstring();
                // set up value to add to the stack
                Value value;
                // get from fields hash table, assign it to instance
                if(htable_get(vm, &instance->fields, name, &value))
                {
                    // vm_pop the instance itself
                    vm_pop(vm);
                    vm_push(vm, value);
                    break;
                }
                // no method as well, error
                if(!vm_bindmethod(vm, instance->classobject, name))
                {
                    return STATUS_RUNTIMEFAIL;
                }
                break;
            }

            case OP_SETPROPERTY:
            {
                // if not an instance
                if(!obj_isinstance(vm_peek(vm, 1)))
                {
                    vm_rterror(vm, "Identifier must be a class instance.");
                    return STATUS_RUNTIMEFAIL;
                }

                // not top most, as the top most is reserved for the new value to be set
                ObjInstance* instance = obj_asinstance(vm_peek(vm, 1));
                //vm_peek(0) is the new value
                htable_set(vm, &instance->fields, vmc_readstring(), vm_peek(vm, 0));
                // vm_pop the already set value
                Value value = vm_pop(vm);
                // vm_pop the property instance itself
                vm_pop(vm);
                // vm_push the value back again
                vm_push(vm, value);
                break;
            }


            case OP_CLOSEUPVALUE:
            {
                // put address to the slot
                vm_closeupvalues(vm, vm->stacktop - 1);
                // vm_pop from the stack
                vm_pop(vm);
                break;
            }
            // will always jump
            case OP_JUMP:
            {
                uint16_t offset = vmc_readshort();
                frame->ip += offset;
                break;
            }
            // for initial if, will not jump if expression inside is true
            case OP_JUMPIFFALSE:
            {
                // offset already put in the stack
                uint16_t offset = vmc_readshort();
                // actual jump instruction is done here; skip over the instruction pointer
                if(vm_isfalsey(vm, vm_peek(vm, 0)))
                {
                    // if evaluated expression inside if statement is false jump
                    frame->ip += offset;
                }
                break;
            }

            case OP_LOOP:
            {
                uint16_t offset = vmc_readshort();
                // jumps back
                frame->ip -= offset;
                break;
            }

            case OP_LOOPIFFALSE:
            {
                // offset already put in the stack
                uint16_t offset = vmc_readshort();
                // bool state is at the top of the stack
                // if false loop back
                if(vm_isfalsey(vm, vm_peek(vm, 0)))
                    frame->ip -= offset;
                // vm_pop the true/false
                vm_pop(vm);
                break;
            }

            case OP_LOOPIFTRUE:
            {
                // offset already put in the stack
                uint16_t offset = vmc_readshort();
                // bool state is at the top of the stack
                // if not false loop back
                if(!vm_isfalsey(vm, vm_peek(vm, 0)))
                    frame->ip -= offset;
                // vm_pop the true/false
                vm_pop(vm);
                break;
            }

            // a callstack to a funcion has the form of function name, param1, param2...
            // the top level code, or caller, also has the same function name, param1, param2... in the right order
            case OP_CALL:
            {
                int argc = vmc_readbyte();
                // vm_call function; pass in the function name istelf[vm_peek(depth)] and the number of arguments
                if(!vm_callvalue(vm, vm_peek(vm, argc), argc))
                {
                    return STATUS_RUNTIMEFAIL;
                }
                // to update pointer if callframe is successful, asnew frame is added
                frame = &vm->frames[vm->framecount - 1];
                break;
            }

            // closures
            case OP_CLOSURE:
            {
                // load compiled function from table
                ObjFunction* function = obj_asfunction(vmc_readconst());
                ObjClosure* closure = obj_mkclosure(vm, function);
                vm_push(vm, obj_mkobject(closure));

                // fill upvalue array over in the interpreter when a closure is created
                // to see upvalues in each slot
                for(int i = 0; i < closure->upvaluecount; i++)
                {
                    // read islocal bool
                    uint8_t isl = vmc_readbyte();
                    // read index for local, if available, in the closure
                    uint8_t index = vmc_readbyte();
                    if(isl)
                    {
                        // get from slots stack
                        closure->upvalues[i] = vm_captureupvalue(vm, frame->slots + index);
                    }
                    // if not local(nested upvalue)
                    else
                    {
                        // get from current upvalue
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }

                break;
            }

            case OP_CLASS:
                // load string for the class' name and vm_push it onto the stack
                vm_push(vm, obj_mkobject(obj_mkclass(vm, vmc_readstring())));
                break;

            case OP_METHOD:
                // get name of the method
                vm_defmethod(vm, vmc_readstring());
                break;

            case OP_INVOKE:
            {
                ObjString* method = vmc_readstring();
                int argc = vmc_readbyte();
                // new vm_invoke function
                if(!vm_invoke(vm, method, argc))
                {
                    return STATUS_RUNTIMEFAIL;
                }
                frame = &vm->frames[vm->framecount - 1];
                break;
            }

            case OP_INHERIT:
            {
                // parent class from 2nd top of the stack
                Value parent = vm_peek(vm, 1);

                // ensure that parent identifier is a class
                if(!obj_isclass(parent))
                {
                    vm_rterror(vm, "Parent identifier is not a class.");
                    return STATUS_RUNTIMEFAIL;
                }
                // child class at the top of the stack
                ObjClass* child = obj_asclass(vm_peek(vm, 0));
                // add all methods from parent to child table
                htable_addall(vm, &obj_asclass(parent)->methods, &child->methods);
                // vm_pop the child class
                vm_pop(vm);
                break;
            }

            case OP_GETSUPER:
            {
                // get method name/identifier
                ObjString* name = vmc_readstring();
                // class identifier is at the top of the stack
                ObjClass* parent = obj_asclass(vm_pop(vm));
                // if binding fails
                if(!vm_bindmethod(vm, parent, name))
                {
                    return STATUS_RUNTIMEFAIL;
                }
                break;
            }
            // super calls optimization
            case OP_SUPERINVOKE:
            {
                ObjString* method = vmc_readstring();
                int count = vmc_readbyte();
                ObjClass* parent = obj_asclass(vm_pop(vm));
                if(!vm_invokefromclass(vm, parent, method, count))
                {
                    return STATUS_RUNTIMEFAIL;
                }
                frame = &vm->frames[vm->framecount - 1];
                break;
            }

            case OP_RETURN:
            {
                // if function returns a value, value will beon top of the stack
                Value result = vm_pop(vm);
                // close lingering closed values
                vm_closeupvalues(vm, frame->slots);

                vm->framecount--;
                // return from 'main()'/script function
                if(vm->framecount == 0)
                {
                    // vm_pop main script function from the stack
                    vm_pop(vm);
                    return STATUS_OK;
                }

                // for a function
                // discard all the slots the callee was using for its parameters
                // basically 're-assign'
                vm->stacktop = frame->slots;
                // vm_push the return value
                vm_push(vm, result);
                // update run function's current frame
                frame = &vm->frames[vm->framecount - 1];
                break;
            }
        }
    }

}


// function for loading scripts
void runfile(VM* vm, const char* path)
{
    // get raw source code from the file
    char* source = readfile(vm, path);
    // get enum type result from VM
    InterpretResult result = vm_interpret(vm, source);
    // free the source code
    free(source);

    if(result == STATUS_COMPILEFAIL)
        exit(51);
    if(result == STATUS_RUNTIMEFAIL)
        exit(61);
}


int main(int argc, const char* argv[])
{
    VM vm;
    vm_init(&vm);

    if(argc == 1)
    {
        repl(&vm);
    }
    else if(argc == 2)
    {
        runfile(&vm, argv[1]);
    }
    else
    {
        fprintf(stderr, "Usage: cfei [path]\n");
        exit(64);
    }
    vm_free(&vm);
    return 0;
}
