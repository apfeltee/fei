
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>




// local variables array in compiler, max of 1 byte is 255
#define UINT8_COUNT (UINT8_MAX + 1)


// track the compiler
#define DEBUG_PRINT_CODE

// execution tracing of the VM
//#define DEBUG_TRACE_EXECUTION


// diagnostic tools for garbage collector	
// 'stress' mode; if this is on, GC runs as often as it possibly can
//#define DEBUG_STRESS_GC
//#define DEBUG_LOG_GC


// for heap-allocated data, for large data types like strings, functinons, instances
// for functions and calls

// type comparisons
#define IS_BOOL(value)		((value).type == VAL_BOOL)
#define IS_NULL(value)		((value).type == VAL_NULL)
#define IS_NUMBER(value)	((value).type == VAL_NUMBER)
#define IS_OBJ(value)	((value).type == VAL_OBJ)



// from VALUE STRUCT to RAW  C nicely used in printing
// also for comparisons -> use values ALREADY CONVERTED to Value struct union to raw C
#define AS_BOOL(value)		((value).as.boolean)
#define AS_NUMBER(value)	((value).as.number)	
#define AS_OBJ(value)		((value).as.obj)

// macros for conversions from code type to struct Value union type
// pass Value struct to the macro
/*	IMPORTANT: macro syntax
#define macroname(parameter) (returntype)
-> here, return a Value type. initializing it inside the macro
-> . means as
IMPORTANT = these macros give a 'tag' to each respective values
*/
#define BOOL_VAL(value)		((Value){VAL_BOOL, {.boolean = value}})		
#define NULL_VAL			((Value){VAL_NULL, {.number = 0}})
#define NUMBER_VAL(value)	((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)		((Value){VAL_OBJ, {.obj = (Obj*)object}})		// pass in as a pointer to the object, receives the actual object


#define OBJ_TYPE(value)	(AS_OBJ(value)->type)		// extracts the tag

// macros for checking(bool) whether an object is a certain type
#define IS_BOUND_METHOD(value)	fei_object_istype(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value)		fei_object_istype(value, OBJ_CLASS)
#define IS_FUNCTION(value)	fei_object_istype(value, OBJ_FUNCTION)
#define IS_INSTANCE(value)	fei_object_istype(value, OBJ_INSTANCE)
#define IS_NATIVE(value)	fei_object_istype(value, OBJ_NATIVE)
#define IS_STRING(value)	fei_object_istype(value, OBJ_STRING)		// takes in raw Value, not raw Obj*
#define IS_CLOSURE(value)	fei_object_istype(value, OBJ_CLOSURE)

// macros to tell that it is safe when creating a tag, by returning the requested type
// take a Value that is expected to conatin a pointer to the heap, first returns pointer second the charray itself
// used to cast as an ObjType pointer, from a Value type
#define AS_BOUND_METHOD(value)	((ObjBoundMethod*)AS_OBJ(value))
#define AS_CLASS(value)		((ObjClass*)AS_OBJ(value))
#define AS_INSTANCE(value)  ((ObjInstance*)AS_OBJ(value))
#define AS_CLOSURE(value)	((ObjClosure*)AS_OBJ(value))
#define AS_STRING(value)	((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)	(((ObjString*)AS_OBJ(value))->chars)		// get chars(char*) from ObjString pointer
#define AS_FUNCTION(value)	((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value)	\
	(((ObjNative*)AS_OBJ(value))->function)


// macro to allocate memory, usedin obj/heap
// use fei_gcmem_reallocate as malloc here; start from null pointer, old size is 0, and new size is count
#define ALLOCATE(type, count)	\
	(type*)fei_gcmem_reallocate(NULL, 0, sizeof(type) * (count))


// free memory, pass in new size as 0 to free
#define FREE(type, pointer) fei_gcmem_reallocate(pointer, sizeof(type), 0)


// C macros
// calculates a new capacity based on a given current capacity, it should SCALE based on the old one
// this one grows by * 2
#define GROW_CAPACITY(capacity) \
	((capacity) < 8 ? 8 : (capacity) * 2)		// capacity becomes 8 for the first time(starts from 0), later it multiplies by 2

// macro to grow array
// make own fei_gcmem_reallocate function
// basically declare our return type here with (type*)
#define GROW_ARRAY(type, pointer, oldCount, newCount)	\
	 (type*)fei_gcmem_reallocate(pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))

// no (type*) because function does not return a type
// 0 is the new capacity
// used to free eg. char arrays
#define FREE_ARRAY(type, pointer, oldCount)	\
	fei_gcmem_reallocate(pointer, sizeof(type) * (oldCount), 0)	\

// max frames is fixed
//#define FRAMES_MAX 64
#define FRAMES_MAX 1024
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)



enum TokenType
{
	// single character
	TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,		// ( )
	TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,		// { }
	TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
	TOKEN_SEMICOLON, TOKEN_COLON, TOKEN_SLASH, TOKEN_STAR,
	TOKEN_MODULO,

	// one or two compare operators
	TOKEN_BANG, TOKEN_BANG_EQUAL,		// !, !=
	TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
	TOKEN_GREATER, TOKEN_GREATER_EQUAL,
	TOKEN_LESS, TOKEN_LESS_EQUAL,

	// literals
	TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,

	// keywords
	TOKEN_AND, TOKEN_CLASS, TOKEN_ELF, TOKEN_ELSE, TOKEN_FALSE,
	TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NULL, TOKEN_OR,
	TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_SWITCH,
	TOKEN_DEFAULT, TOKEN_CASE, TOKEN_THIS, TOKEN_TRUE, 
	TOKEN_VAR, TOKEN_WHILE, TOKEN_BREAK, TOKEN_CONTINUE,
	TOKEN_THEN,

	// do while, repeat until
	TOKEN_DO, TOKEN_REPEAT, TOKEN_UNTIL,

	// class inheritance
	TOKEN_FROM,

	TOKEN_ERROR,
	TOKEN_EOF
};

// for precedence in unary operations
// ordered from lowest precedence to highest precedence
typedef enum
{
	PREC_NONE, 
	PREC_ASSIGNMENT,	// =
	PREC_OR,			// or
	PREC_AND,			// and
	PREC_EQUALITY,		// == !=
	PREC_COMPARISON,	// > >= < <=
	PREC_TERM,			// + -
	PREC_FACTOR,		// * /
	PREC_UNARY,			// ! -
	PREC_CALL,			// . ()
	PREC_PRIMARY
} Precedence;

typedef enum
{
	TYPE_FUNCTION,
	TYPE_SCRIPT,		// top level main()
	TYPE_INITIALIZER,		// class constructors
	TYPE_METHOD,		// class methods
} FunctionType;


// in bytecode format, each instruction has a one-byte operation code(opcode)
// the number controls what kind of instruction we're dealing with- add, subtract, etc
// typedef enums are bytes apparently
// these are INSTRUCTIONS 
typedef enum
{
	OP_CONSTANT,	// chunk needs to know when to produce constants and print them in the right order
					// they have operands, to eg. identify which variable to load
					// OP_CONSTANT take up 2 bytes, one is the opcode itself and the other the constant index
	
	OP_NULL,
	OP_TRUE,
	OP_FALSE,

	// unary operators
	OP_NEGATE,		// operand to negate, utilized in virtual machine

	// literals/declarations
	OP_PRINT,
	OP_POP,			// basically pops a value off the stack and forgets it, used for expression statements
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
	OP_JUMP_IF_FALSE,		// takes a 16-bit operand
	OP_CALL,

	OP_LOOP, 
	OP_LOOP_IF_FALSE,		// repeat until
	OP_LOOP_IF_TRUE,	// do while

	OP_CLOSURE,
	OP_CLASS,
	OP_METHOD,
	OP_INVOKE,

	OP_INHERIT,			// class inheritance
	OP_GET_SUPER,		// for superclasses
	OP_SUPER_INVOKE,

	OP_RETURN,		// means return from current function
} OpCode;			// basically a typdef call to an enum
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
} InterpretResult;

// type tags for the tagged union
enum ValueType
{
	VAL_BOOL,
	VAL_NULL,
	VAL_NUMBER,
	VAL_OBJ,		// for bigger instances such as strings, functions, heap-allocated; the payload is a heap pointer
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

typedef enum TokenType TokenType;
typedef enum ValueType ValueType;
typedef struct Value Value;
typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef struct ObjFunction ObjFunction;
typedef struct Local Local;
typedef struct Upvalue Upvalue;



/* IMPORTANT 
-> use C unions to OVERLAP in memory for the STRUCT
->  size of the union is its LARGEST FIELD
-> unions are like structs but they only allocate memory for the LARGEST FIELD
*/

struct Value
{
	ValueType type;
	union	// the union itself, implemented here
	{
		bool boolean;
		double number;
		Obj* obj;			// pointer to the heap, the payload for bigger types of data
	} as;			// can use . to represent this union
};

typedef struct
{
	TokenType type;			// identifier to type of token, eg. number, + operator, identifier
	const char* start;
	int length;
	int line;
} Token;

// scanner to run through the source code
typedef struct
{
	const char* start;		// marks the beginning of the current lexeme('word', you can say_
	const char* current;	// points to the character being looked at
	int line;				// int to tell the current line being looked at
} Scanner;

// to store current and previous tokens
typedef struct
{
	Token current;
	Token previous;
	bool hadError;		// flag to tell whether the code has a syntax error or no
	bool panicMode;		// flag for error cascades/multiple errors so the parser does not get confused, only returns the first
} Parser;



// simple typdef function type with no arguments and returns nothing
// acts like a "virtual" function , a void function that cam be overidden; actually a void but override it with ParseFn
typedef void(*ParseFn)(bool canAssign);


/*	parse rule, what is needed:
-> a function to compile a PREFIX expression starting with token of that type
-> a function to cimpile an INFIX expression whose left operand is followed by a token of that type
-> precedence of an infix expression with the tokenas an operator
*/
typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

struct Local
{
	Token name;
	int depth;			// depth of the variable, corresponding to scoreDepth in the struct below
	bool isCaptured;	// track whether the local is captured by a closure or no
};

struct Upvalue
{
	bool isLocal;
	int index;			// matches the index of the local variable in ObjClosure
};

// stack for local variables
typedef struct Compiler		// give name to struct itself(the name comes after), only used below in enclosing
{
	struct Compiler* enclosing;			// pointer to the 'outer'/enclosing compiler, to return to after function

	// wrapping the whole program into one big main() function
	ObjFunction* function;
	FunctionType type;

	Local locals[UINT8_COUNT];		// array to store locals, ordered in the order of declarations
	int localCount;					// tracks amount of locals in a scope
	Upvalue upvalues[UINT8_COUNT];
	int scopeDepth;					// number of scopes/blocks surrounding the code

	// for loop breaks and continues, loop enclosing
	int loopCountTop;
	int* continueJumps;
	int continueJumpCapacity;			// only for continue jumpbs

	// for patching all break statements
	int breakPatchJumps[UINT8_COUNT][UINT8_COUNT];
	int breakJumpCounts[UINT8_COUNT];

} Compiler;


// for 'this' tokens, a main() for class
typedef struct ClassCompiler
{
	struct ClassCompiler* enclosing;
	Token name;
	bool hasSuperclass;			// to end scope in superclass declaration
} ClassCompiler;



// the constant pool is array of values
typedef struct
{
	int capacity;
	int count;
	Value* values;
} ValueArray;




/* dynamic array for bytecode */
// btyecode is a series of instructions, this is a struct to hold instructions
// create own dynamic array
typedef struct
{
	int count;					// current size
	int capacity;				// max array size
	uint8_t* code;				// 1 byte unsigned int, to store the CODESTREAM
	int* lines;					// array of integers that parallels the bytecode/codestream, to get where each location of the bytecode is
	ValueArray constants;		// store double value literals
} Chunk;



/* the top two are simply wrapper around bytes */


// in terms of the language's bytecode datatypes (eg. Value, objString)
// hash function is in object.c, applied to every string for cache

typedef struct
{
	ObjString* key;			// use ObjString pointer as key
	Value value;			// the value/data type
} Entry;

// the table, an array of entries
typedef struct
{
	int count;
	int capacity;
	Entry* entries;
} Table;



struct Obj					// as no typedef is used, 'struct' itself will always havae to be typed
{
	ObjType type;
	struct Obj* next;		// linked list or intrusive list, to avoid memory leaks, obj itself as a node
							// traverse the list to find every object that has been allocated on the heap

	// for mark-sweep garbage collection
	bool isMarked;
};

// for functions and calls
struct ObjFunction
{
	Obj obj;
	int arity;				// store number of parameters
	int upvalueCount;		// to track upValues
	Chunk chunk;			// to store the function information
	ObjString* name;
};


typedef struct ObjUpvalue			// define ObjUpvalue here to use them inside the struct
{
	Obj obj;
	Value* location;			// pointer to value in the enclosing ObjClosure
	
	Value closed;		// to store closed upvalue
								
	// intrusive/linked list to track sorted openvalues
	// ordered by the stack slot they point to
	struct ObjUpvalue* next;
} ObjUpvalue;

// for closures
typedef struct
{
	// points to an ObjFunction and Obj header
	Obj obj;					// Obj header
	ObjFunction* function;
	
	// for upvalues
	ObjUpvalue** upvalues;		// array of upvalue pointers
	int upvalueCount;
} ObjClosure;


/*  NATIVE FUNCTIONS(file systems, user input etc.)
-> native functions reference a call to native C code insted of bytecode */
typedef Value(*NativeFn)(int argCount, Value* args);		// rename Value to NativeFn

typedef struct {
	Obj obj;
	NativeFn function;
} ObjNative;


struct ObjString			// using struct inheritance
{
	Obj obj;
	int length;
	char* chars;
	uint32_t hash;		// for hash table, for cache(temporary storage area); each ObjString has a hash code for itself
};


// class object type
typedef struct
{
	Obj obj;
	ObjString* name;			// not needed for uer's program, but helps the dev in debugging
	Table methods;				// hash table for storing methods
} ObjClass;

typedef struct
{
	Obj obj;			// inherits from object, the "object" tag
	ObjClass* kelas;	// pointer to class types
	Table fields;		// use a hash table to store fields
} ObjInstance;


// struct for class methods
typedef struct
{
	Obj obj;
	Value receiver;					// wraps receiver and function/method/closure together, receiver is the ObjInstance / lcass type
	ObjClosure* method;
} ObjBoundMethod;		

// the call stack
// keep track where on the stack a function's local begin, where the caller should resume, etc.
// a call frame represents a single ongoing function call
// each time a function is called, create this struct
typedef struct
{
	ObjClosure* closure;
	uint8_t* ip;		// store ip on where in the VM the function is
	Value* slots;		// this points into the VM's value stack at the first slot the function can use
} CallFrame;

typedef struct
{
	// since the whole program is one big 'main()' use callstacks
	CallFrame frames[FRAMES_MAX];		
	int frameCount;				// stores current height of the stack

	Value stack[STACK_MAX];			// stack array is 'indirectly' declared inline here
	Value* stackTop;			// pointer to the element just PAST the element containing the top value of the stack

	Table globals;		// for storing global variables
	Table strings;		// for string interning, to make sure every equal string takes one memory

	ObjString* initString;			// init string for class constructors

	ObjUpvalue* openUpvalues;		// track all upvalues; points to the first node of the linked list

	Obj* objects;		// pointer to the header of the Obj itself/node, start of the list
						// nicely used in GARBAGE COLLECTION, where objects are nicely erased in the middle

	// stack to store gray marked Objects for garbage collection
	int grayCapacity;		
	int grayCount;
	Obj** grayStack;			// array of pointers pointing to a particular subgraph

	// self-adjusting-g-heap, to control frequency of GC, bytesAllocated is the running total
	size_t bytesAllocated;		// size_t is a 32 bit(integer/4bytes), represents size of an object in bytes
	size_t nextGC;				// threhsold that triggers the GC
} VM;





/* main.c */
void *fei_gcmem_reallocate(void *pointer, size_t oldSize, size_t newSize);
void fei_gcmem_freeobject(Obj *object);
void fei_gcmem_markobject(Obj *object);
void fei_gcmem_markvalue(Value value);
void fei_gcmem_markarray(ValueArray *array);
void fei_gcmem_markroots(void);
void fei_gcmem_blackenobject(Obj *object);
void fei_gcmem_tracerefs(void);
void fei_gcmem_sweep(void);
void fei_gcmem_collectgarbage(void);
void fei_gcmem_freeobjects(void);
void fei_valarray_init(ValueArray *array);
void fei_valarray_push(ValueArray *array, Value value);
void fei_valarray_destroy(ValueArray *array);
void fei_value_printvalue(Value value);
bool fei_value_compare(Value a, Value b);
Obj *fei_object_allocobject(size_t size, ObjType type);
ObjBoundMethod *fei_object_makeboundmethod(Value receiver, ObjClosure *method);
ObjClosure *fei_object_makeclosure(ObjFunction *function);
ObjString *fei_object_allocstring(char *chars, int length, uint32_t hash);
ObjClass *fei_object_makeclass(ObjString *name);
ObjInstance *fei_object_makeinstance(ObjClass *kelas);
ObjFunction *fei_object_makefunction(void);
ObjNative *fei_object_makenativefunc(NativeFn function);
uint32_t fei_object_hashstring(const char *key, int length);
ObjString *fei_object_takestring(char *chars, int length);
ObjString *fei_object_copystring(const char *chars, int length);
ObjUpvalue *fei_object_makeupvalue(Value *slot);
void fei_object_printfunc(ObjFunction *function);
void fei_object_printobject(Value value);
void fei_table_init(Table *table);
void fei_table_destroy(Table *table);
Entry *fei_table_findentry(Entry *entries, int capacity, ObjString *key);
bool fei_table_get(Table *table, ObjString *key, Value *value);
void fei_table_adjustcapacity(Table *table, int capacity);
bool fei_table_set(Table *table, ObjString *key, Value value);
bool fei_table_delete(Table *table, ObjString *key);
void fei_table_mergefrom(Table *from, Table *to);
ObjString *fei_table_findstring(Table *table, const char *chars, int length, uint32_t hash);
void fei_table_removeunreachable(Table *table);
void fei_table_mark(Table *table);
void fei_chunk_init(Chunk *chunk);
void fei_chunk_pushbyte(Chunk *chunk, uint8_t byte, int line);
void fei_chunk_destroy(Chunk *chunk);
int fei_chunk_pushconst(Chunk *chunk, Value value);
int fei_dbgutil_printsimpleir(const char *name, int offset);
int fei_dbgutil_printbyteir(const char *name, Chunk *chunk, int offset);
int fei_dbgutil_printconstir(const char *name, Chunk *chunk, int offset);
int fei_dbgutil_printinvokeir(const char *name, Chunk *chunk, int offset);
int fei_dbgutil_printjumpir(const char *name, int sign, Chunk *chunk, int offset);
void fei_dbgdisas_chunk(Chunk *chunk, const char *name);
int fei_dbgdisas_instr(Chunk *chunk, int offset);
void fei_lexer_initsource(const char *source);
bool fei_lexutil_isalpha(char c);
bool fei_lexutil_isdigit(char c);
bool fei_lexer_isatend(void);
char fei_lexer_advance(void);
bool fei_lexer_match(char expected);
Token fei_lexer_maketoken(TokenType type);
Token fei_lexer_errortoken(const char *message);
char fei_lexer_peekcurrent(void);
char fei_lexer_peeknext(void);
void fei_lexer_skipspace(void);
TokenType fei_lexer_checkkw(int start, int length, const char *rest, TokenType type);
TokenType fei_lexer_scantype(void);
Token fei_lexer_scanident(void);
Token fei_lexer_scannumber(void);
Token fei_lexer_scanstring(void);
Token fei_lexer_scantoken(void);
Chunk *fei_compiler_currentchunk(void);
void fei_compiler_raiseat(Token *token, const char *message);
void fei_compiler_raiseerror(const char *message);
void fei_compiler_raisehere(const char *message);
void fei_compiler_advancenext(void);
void fei_compiler_advanceskipping(TokenType type);
void fei_compiler_consume(TokenType type, const char *message);
bool fei_compiler_check(TokenType type);
bool fei_compiler_match(TokenType type);
void fei_compiler_emitbyte(uint8_t byte);
void fei_compiler_emitbytes(uint8_t byte1, uint8_t byte2);
void fei_compiler_emitloop(int loopStart);
void fei_compiler_emitcondloop(int loopStart, bool state);
int fei_compiler_emitjump(uint8_t instruction);
void fei_compiler_emitreturn(void);
uint8_t fei_compiler_makeconst(Value value);
void fei_compiler_emitconst(Value value);
void fei_compiler_patchjump(int offset);
void fei_compiler_init(Compiler *compiler, FunctionType type);
ObjFunction *fei_compiler_endcompiler(void);
void fei_compiler_beginscope(void);
void fei_compiler_endscope(void);
void fei_compiler_beginloopscope(void);
void fei_compiler_endloopscope(void);
void fei_compiler_markcontinuejump(void);
void fei_compiler_patchbreakjumps(void);
uint8_t fei_compiler_makeidentconst(Token *name);
bool fei_compiler_identsequal(Token *a, Token *b);
int fei_compiler_resolvelocal(Compiler *compiler, Token *name);
int fei_compiler_addupvalue(Compiler *compiler, uint8_t index, bool isLocal);
int fei_compiler_resolveupvalue(Compiler *compiler, Token *name);
void fei_compiler_addlocal(Token name);
void fei_compiler_declvarfromcurrent(void);
uint8_t fei_compiler_parsevarfromcurrent(const char *errorMessage);
void fei_compiler_markinit(void);
void fei_compiler_defvarindex(uint8_t global);
uint8_t fei_compiler_parsearglist(void);
void fei_compiler_declnamedvar(Token name, bool canAssign);
Token fei_compiler_makesyntoken(const char *text);
void fei_compiler_parseprec(Precedence precedence);
ParseRule *fei_compiler_getrule(TokenType type);
void fei_compiler_parseexpr(void);
void fei_compiler_parseblock(void);
void fei_compiler_parsefuncdecl(FunctionType type);
void fei_compiler_parsemethoddecl(void);
void fei_compiler_parseclassdecl(void);
void fei_compiler_parseclassfuncdecl(void);
void fei_compiler_parsevardecl(void);
void fei_compiler_parseexprstmt(void);
void fei_compiler_parseifstmt(void);
void fei_compiler_parseswitchstmt(void);
void fei_compiler_parseprintstmt(void);
void fei_compiler_parsereturnstmt(void);
void fei_compiler_parseforstmt(void);
void fei_compiler_parsewhilestmt(void);
void fei_compiler_parsebreakstmt(void);
void fei_compiler_parsecontinuestmt(void);
void fei_compiler_parserepeatuntilstmt(void);
void fei_compiler_parsedowhilestmt(void);
void fei_compiler_synchronize(void);
void fei_compiler_parsedeclaration(void);
void fei_compiler_parsestatement(void);
ObjFunction *fei_compiler_compilesource(const char *source);
void fei_compiler_markroots(void);
void fei_vm_resetstack(void);
void fei_vm_raiseruntimeerror(const char *format, ...);
void fei_vm_defnative(const char *name, NativeFn function);
void fei_vm_init(void);
void fei_vm_destroy(void);
void fei_vm_pushvalue(Value value);
Value fei_vm_popvalue(void);
Value fei_vm_peekvalue(int distance);
bool fei_vm_callclosure(ObjClosure *closure, int argCount);
bool fei_vm_callvalue(Value callee, int argCount);
bool fei_class_invokemethod(ObjClass *kelas, ObjString *name, int argCount);
bool fei_vm_stackinvoke(ObjString *name, int argCount);
bool fei_class_bindmethod(ObjClass *kelas, ObjString *name);
ObjUpvalue *fei_vm_captureupvalue(Value *local);
void fei_vm_closeupvalues(Value *last);
void fei_vm_stackdefmethod(ObjString *name);
bool fei_value_isfalsey(Value value);
void fei_vmdo_strconcat(void);
InterpretResult fei_vm_evalsource(const char *source);
InterpretResult fei_vm_exec(void);
void repl(void);
char *readFile(const char *path);
void runFile(const char *path);

static inline bool fei_object_istype(Value value, ObjType type)				// inline function, initialized in .h file
{
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

// initialize virtual machine here
VM vm;
Scanner scanner;
Parser parser;
ClassCompiler* currentClass = NULL;	
Compiler* current = NULL;


// growth factor for garbage collection heap
#define GC_HEAP_GROW_FACTOR 2


// A void pointer is a pointer that has no associated data type with it.
// A void pointer can hold address of any type and can be typcasted to any type.
void* fei_gcmem_reallocate(void* pointer, size_t oldSize, size_t newSize)
{
	vm.bytesAllocated += newSize - oldSize;		// self adjusting heap for garbage collection

	if (newSize > oldSize)		// when allocating NEW memory, not when freeing as collecGarbage will cal void* reallocate itself
	{
#ifdef DEBUG_STRESS_GC
		fei_gcmem_collectgarbage();
#endif
	
		// run collecter if bytesAllocated is above threshold
		if (vm.bytesAllocated > vm.nextGC)
		{
			fei_gcmem_collectgarbage();
		}
	
	}

	if (newSize == 0)
	{
		free(pointer);
		return NULL;
	}

	// C realloc
	void* result = realloc(pointer, newSize);

	// if there is not enought memory, realloc will return null
	if (result == NULL) exit(1);	// exit with code 1

	return result;
}


// you can pass in a'lower' struct pointer, in this case Obj*, and get the higher level which is ObjFunction
void fei_gcmem_freeobject(Obj* object)		// to handle different types
{
#ifdef DEBUG_LOG_GC
	printf("%p free type %d\n", (void*)object, object->type);
#endif

	switch (object->type)
	{
	case OBJ_BOUND_METHOD:
		FREE(ObjBoundMethod, object);
		break;
	
	case OBJ_CLASS:
	{
		// free class type
		ObjClass* kelas = (ObjClass*)object;
		fei_table_destroy(&kelas->methods);
		FREE(ObjClass, object);
		break;
	}
	case OBJ_INSTANCE:
	{
		ObjInstance* instance = (ObjInstance*)object;
		fei_table_destroy(&instance->fields);
		FREE(ObjInstance, object);
		break;
	}
	case OBJ_CLOSURE:
	{
		// free upvalues
		ObjClosure* closure = (ObjClosure*)object;
		FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);		
		
		FREE(ObjClosure, object);		// only free the closure, not the function itself
		break;
	}
	case OBJ_FUNCTION:		// return bits(chunk) borrowed to the operating syste,
	{
		ObjFunction* function = (ObjFunction*)object;
		fei_chunk_destroy(&function->chunk);
		FREE(ObjFunction, object);
		break;
	}
	case OBJ_NATIVE:
	{
		FREE(ObjNative, object);
		break;
	}
	case OBJ_STRING: 
	{
		ObjString* string = (ObjString*)object;
		FREE_ARRAY(char, string->chars, string->length + 1);
		FREE(ObjString, object);
		break;
	}
	case OBJ_UPVALUE:
	{
		FREE(ObjUpvalue, object);
		break;
	}
	}
}

/*		garbage collection		 */	

void fei_gcmem_markobject(Obj* object)
{
	if (object == NULL) return;				// in some places the pointer is empty
	if (object->isMarked) return;			// object is already marked
	
	object->isMarked = true;

	// create a worklist of grayobjects to traverse later, use a stack to implement it
	if (vm.grayCapacity < vm.grayCount + 1)			// if need more space, allocate
	{	
		vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
		vm.grayStack = realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);			// use native realloc here
	}

	if (vm.grayStack == NULL) exit(1);			// if fail to allocate memory for the gray stack	

	// add the 'gray' object to the working list
	vm.grayStack[vm.grayCount++] = object;

#ifdef DEBUG_LOG_GC
	printf("%p marked ", (void*)object);
	fei_value_printvalue(OBJ_VAL(object));		// you cant print first class objects, like how you would print in the actual repl
	printf("\n");
#endif
}

void fei_gcmem_markvalue(Value value)
{
	if (!IS_OBJ(value)) return;		// if value is not first class Objtype return
	fei_gcmem_markobject(AS_OBJ(value));
}


// marking array of values/constants of a function, used in fei_gcmem_blackenobject, case OBJ_FUNCTION
void fei_gcmem_markarray(ValueArray* array)
{
	for (int i = 0; i < array->count; i++)
	{
		fei_gcmem_markvalue(array->values[i]);			// mark each Value in the array
	}
}


void fei_gcmem_markroots()
{
	// assiging a pointer to a full array means assigning the pointer to the FIRST element of that array
	for (Value* slot = vm.stack; slot < vm.stackTop; slot++)		// walk through all values/slots in the Value* array
	{
		fei_gcmem_markvalue(*slot);
	}

	// mark closures
	for (int i = 0; i < vm.frameCount; i++)
	{
		fei_gcmem_markobject((Obj*)vm.frames[i].closure);			// mark ObjClosure  type
	}

	// mark upvalues, walk through the linked list of upvalues
	for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next)
	{
		fei_gcmem_markobject((Obj*)upvalue);
	}


	fei_table_mark(&vm.globals);			// mark global variables, belongs in the VM/hashtable

	// compiler also grabs memory; special function only for 'backend' processes
	fei_compiler_markroots();		// declared in compiler.h

	fei_gcmem_markobject((Obj*)vm.initString);		// mark objstring for init 
}


// actual tracing of each gray object and marking it black
void fei_gcmem_blackenobject(Obj* object)
{
#ifdef DEBUG_LOG_GC
	printf("%p blackened ", (void*)object);
	fei_value_printvalue(OBJ_VAL(object));
	printf("\n");
#endif


	switch (object->type)
	{
	case OBJ_BOUND_METHOD:
	{
		ObjBoundMethod* bound = (ObjBoundMethod*)object;
		fei_gcmem_markvalue(bound->receiver);
		fei_gcmem_markobject((Obj*)bound->method);
		break;
	}

	case OBJ_UPVALUE:		// simply mark the closed value
		fei_gcmem_markvalue(((ObjUpvalue*)object)->closed);
		break;

	case OBJ_FUNCTION:		// mark the name and its value array of constants
	{
		// you can get the coressponding 'higher' object type from a lower derivation struct in C using (higher*)lower
		ObjFunction* function = (ObjFunction*)object;		
		fei_gcmem_markobject((Obj*)function->name);		// mark its name, an ObjString type
		fei_gcmem_markarray(&function->chunk.constants);		// mark value array of chunk constants, pass it in AS A POINTER using &
		break;
	}

	case OBJ_CLOSURE:				// mark the function and all of the closure's upvalues
	{
		ObjClosure* closure = (ObjClosure*)object;
		fei_gcmem_markobject((Obj*)closure->function);
		for (int i = 0; i < closure->upvalueCount; i++)
		{
			fei_gcmem_markobject((Obj*)closure->upvalues[i]);
		}
		break;
	}

	case OBJ_CLASS:
	{
		ObjClass* kelas = (ObjClass*)object;
		fei_gcmem_markobject((Obj*)kelas->name);
		fei_table_mark(&kelas->methods);
		break;
	}

	case OBJ_INSTANCE:
	{
		ObjInstance* instance = (ObjInstance*)object;
		fei_gcmem_markobject((Obj*)instance->kelas);
		fei_table_mark(&instance->fields);
		break;
	}
		// these two objects contain NO OUTGOING REFERENCES there is nothing to traverse
	case OBJ_NATIVE:
	case OBJ_STRING:
		break;
	}
}


// traversing the gray stack work list
void fei_gcmem_tracerefs()
{
	while (vm.grayCount > 0)
	{
		// pop Obj* (pointer) from the stack
		// note how -- is the prefix; subtract first then use it as an index
		// --vm.grayCount already decreases its count, hence everything is already 'popped'
		Obj* object = vm.grayStack[--vm.grayCount];			
		fei_gcmem_blackenobject(object);
	}
}


// sweeping all unreachable values
void fei_gcmem_sweep()
{
	Obj* previous = NULL;
	Obj* object = vm.objects;		// linked intrusive list of Objects in the VM
	
	while (object != NULL)
	{
		if (object->isMarked)		// object marked, do not free
		{
			object->isMarked = false;			// reset the marking to 'white'
			previous = object;
			object = object->next;
		}
		else      // free the unreachable object
		{
			Obj* unreached = object;
			object = object->next;

			if (previous != NULL)	// link to previous object if previous not null
			{
				previous->next = object;
			}
			else             // if not set the next as the start of the list
			{
				vm.objects = object;	
			}

			fei_gcmem_freeobject(unreached);			// method that actually frees the object
		}
	}
}

void fei_gcmem_collectgarbage()
{
#ifdef DEBUG_LOG_GC
	printf("--Garbage Collection Begin\n");
	size_t before = vm.bytesAllocated;
#endif

	fei_gcmem_markroots();			// function to start traversing the graph, from the root and marking them
	fei_gcmem_tracerefs();		// tracing each gray marked object

	// removing intern strings, BEFORE the sweep so the pointers can still access its memory
	// function defined in hahst.c
	fei_table_removeunreachable(&vm.strings);

	fei_gcmem_sweep();				// free all unreachable roots

	// adjust size of threshold
	vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
	printf("--Garbage Collection End\n");
	printf("	collected %zd bytes (from %zd to %zd) next at %zd\n",
		before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}


/*		end of garbage collection		 */



void fei_gcmem_freeobjects()			// free from VM
{
	Obj* object = vm.objects;
	// free from the whole list
	while (object != NULL)
	{
		Obj* next = object->next;
		fei_gcmem_freeobject(object);
		object = next;
	}

	free(vm.grayStack);			// free gray marked obj stack used for garbage collection
}

void fei_valarray_init(ValueArray* array)
{
	array->count = 0;
	array->capacity = 0;
	array->values = NULL;
}

void fei_valarray_push(ValueArray* array, Value value)
{
	if (array->capacity < array->count + 1)
	{
		int oldCapacity = array->capacity;
		array->capacity = GROW_CAPACITY(oldCapacity);
		array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
	}

	array->values[array->count] = value;
	array->count++;
}

void fei_valarray_destroy(ValueArray* array)
{
	FREE_ARRAY(Value, array->values, array->capacity);
	fei_valarray_init(array);
}

// actual printing on the virtual machine is done here
void fei_value_printvalue(Value value)
{
	switch (value.type)
	{
	case VAL_BOOL:
		printf(AS_BOOL(value) ? "true" : "false"); break;
	case VAL_NULL:
		printf("null"); break;
	case VAL_NUMBER:
		printf("%g", AS_NUMBER(value)); break;
	case VAL_OBJ: fei_object_printobject(value); break;			// print heap allocated value, from object.h
	}
}

// comparison function used in VM run()
// used in ALL types of data(num, string, bools)
bool fei_value_compare(Value a, Value b)
{
	if (a.type != b.type) return false;				// if type is different return false

	switch (a.type)
	{
	case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
	case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
	case VAL_NULL: return true;				// true for all nulls
	case VAL_OBJ: return AS_OBJ(a) == AS_OBJ(b);		// already interned, occupies the same address
	default:
		return false;		// unreachable
	}
}



/* copying the string from the const char* in the source code to the heap 
-> does not reuse string pointers from the source code
*/

// macro to avoid redundantly cast void* back to desired type
#define ALLOCATE_OBJ(type, objectType)	\
	(type*)fei_object_allocobject(sizeof(type), objectType)

Obj* fei_object_allocobject(size_t size, ObjType type)
{
	Obj* object = (Obj*)fei_gcmem_reallocate(NULL, 0, size);		// allocate memory for obj
	object->type = type;
	object->isMarked = false;

	// every time an object is allocated, insert to the list
	// insert as the HEAD; the latest one inserted will be at the start
	object->next = vm.objects;			// vm from virtualm.h, with extern
	vm.objects = object;		

#ifdef DEBUG_LOG_GC
	printf("%p allocate %zd for %d\n", (void*)object, size, type);			// %ld prints LONG INT
																			// (void*) for 'native pointer type'
#endif


	return object;
}

// new bound method for classes
ObjBoundMethod* fei_object_makeboundmethod(Value receiver, ObjClosure* method)
{
	ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
	bound->receiver = receiver;
	bound->method = method;
	return bound;
}


// create new closure
ObjClosure* fei_object_makeclosure(ObjFunction* function)
{
	// initialize array of upvalue pointers
	// upvalues carry over
	ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);

	for (int i = 0; i < function->upvalueCount; i++)
	{
		upvalues[i] = NULL;				// initialize all as null
	}


	ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjString* fei_object_allocstring(char* chars, int length, uint32_t hash)			// pass in hash
{
	ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
	string->length = length;
	string->chars = chars;
	string->hash = hash;

	fei_vm_pushvalue(OBJ_VAL(string));		// garbage collection
	//printf("allocate\n");
	fei_table_set(&vm.strings, string, NULL_VAL);		// for string interning
	fei_vm_popvalue();			// garbage collection

	return string;
}

ObjClass* fei_object_makeclass(ObjString* name)
{
	ObjClass* kelas = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);		// kelas not class for compiling in c++
	kelas->name = name;
	fei_table_init(&kelas->methods);
	return kelas;
}


// create new class instance
ObjInstance* fei_object_makeinstance(ObjClass* kelas)
{
	ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
	instance->kelas = kelas;
	fei_table_init(&instance->fields);		// memory address of the fields
	return instance;
}


ObjFunction* fei_object_makefunction()
{
	ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);

	function->arity = 0;
	function->upvalueCount = 0;
	function->name = NULL;
	fei_chunk_init(&function->chunk);
	return function;
}

// new native function
ObjNative* fei_object_makenativefunc(NativeFn function)
{
	ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
	native->function = function;
	return native;
}



// hash function, the FNV-1a
uint32_t fei_object_hashstring(const char* key, int length)
{
	uint32_t hash = 2116136261u;		// initial hash value, u at end means unsigned

	for (int i = 0; i < length; i++)	// traverse through the data to be hashed
	{
		hash ^= key[i];			// munge the bits from the string key to the hash value; ^= is a bitwise operator
		hash *= 16777619;
	}

	return hash;
}


// shorten than fei_object_copystring because owernship of the char* itself is declared in concatenate(), hence no need to declare memory again
ObjString* fei_object_takestring(char* chars, int length)
{
	uint32_t hash = fei_object_hashstring(chars, length);
	ObjString* interned = fei_table_findstring(&vm.strings, chars, length, hash);


	if (interned != NULL)		// if the same string already exists
	{
		FREE_ARRAY(char, chars, length + 1);		// free the memory for use
		return interned;
	}


	return fei_object_allocstring(chars, length, hash);
}

// copy string from source code to memory
ObjString* fei_object_copystring(const char* chars, int length)
{
	uint32_t hash = fei_object_hashstring(chars, length);
	ObjString* interned = fei_table_findstring(&vm.strings, chars, length, hash);

	if (interned != NULL) {
		return interned;	// if we find a string already in vm.srings, no need to copy just return the pointer
	}
	char* heapChars = ALLOCATE(char, length + 1);	// length +1 for null terminator
	memcpy(heapChars, chars, length);			// copy memory from one location to another; memcpy(*to, *from, size_t (from))
	heapChars[length] = '\0';		// '\0', a null terminator used to signify the end of the string, placed at the end

	return fei_object_allocstring(heapChars, length, hash);
}


ObjUpvalue* fei_object_makeupvalue(Value* slot)
{
	ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
	upvalue->location = slot;
	upvalue->next = NULL;
	upvalue->closed = NULL_VAL;
	return upvalue;
}

void fei_object_printfunc(ObjFunction* function)
{
	if (function->name == NULL)
	{
		printf("<script>");
		return;
	}
	printf("fun %s(%d params)", function->name->chars, function->arity);		// print name and number of parameters
}

void fei_object_printobject(Value value)
{
	// first class objects can be printed; string and functions
	switch (OBJ_TYPE(value))
	{
	case OBJ_BOUND_METHOD:
		fei_object_printfunc(AS_BOUND_METHOD(value)->method->function);
		break;
	case OBJ_CLASS:
		printf("%s", AS_CLASS(value)->name->chars);
		break;
	case OBJ_INSTANCE:
		printf("%s instance", AS_INSTANCE(value)->kelas->name->chars);
		break;
	case OBJ_CLOSURE:
		fei_object_printfunc(AS_CLOSURE(value)->function);
		break;
	case OBJ_FUNCTION:
		fei_object_printfunc(AS_FUNCTION(value));
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

// the hash table can only be 75% full
#define TABLE_MAX_LOAD 0.75

void fei_table_init(Table* table)
{
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void fei_table_destroy(Table* table)
{
	FREE_ARRAY(Entry, table->entries, table->capacity);
	fei_table_init(table);
}

Entry* fei_table_findentry(Entry* entries, int capacity, ObjString* key)
{
	uint32_t index = key->hash % capacity;		// use modulo to map the key's hash to the code index
	Entry* tombstone = NULL;

	for (;;)
	{
		Entry* entry = &entries[index];		// index is 'inserted' here
		
		if (entry->key == NULL)
		{
			if (IS_NULL(entry->value))
			{	
				return tombstone != NULL ? tombstone : entry;		// empty entry
			}
			else {
				if (tombstone == NULL) tombstone = entry;		// can return tombstone bucket as empty and reuse it
			}
		}
		if (entry->key == key)		// compare them in MEMORY
		{
			return entry;
		}
		

		index = (index + 1) % capacity;		
	}
}

bool fei_table_get(Table* table, ObjString* key, Value* value)
{
	if (table->count == 0) return false;

	Entry* entry = fei_table_findentry(table->entries, table->capacity, key);
	if (entry->key == NULL) return false;

	*value = entry->value;			// asign the value parameter the entry value
	return true;
}


void fei_table_adjustcapacity(Table* table, int capacity)
{
	Entry* entries = ALLOCATE(Entry, capacity);			// create a bucket with capacity entries, new array
	for (int i = 0; i < capacity; i++)		// initialize every element			
	{
		entries[i].key = NULL;				
		entries[i].value = NULL_VAL;
	}

	table->count = 0;		// do not copy tombstones over when growing
	// NOTE: entries may end up in different buckets
	// with the same hash as it is divided by the modulo; loop below recalculates everything
	for (int i = 0; i < table->capacity; i++)	// travers through old array
	{
		Entry* entry = &table->entries[i];
		if (entry->key == NULL) continue;		

		// insert into new array
		Entry* dest = fei_table_findentry(entries, capacity, entry->key);			// pass in new array
		dest->key = entry->key;					// match old array to new array
		dest->value = entry->value;
		table->count++;			// recound the number of entries
	}

	FREE_ARRAY(Entry, table->entries, table->capacity);
	table->entries = entries;
	table->capacity = capacity;
}

// inserting into the table, return false if collision
bool fei_table_set(Table* table, ObjString* key, Value value)
{
	// make sure array is big enough
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD)
	{
		int capacity = GROW_CAPACITY(table->capacity);
		fei_table_adjustcapacity(table, capacity);
	}


	Entry* entry = fei_table_findentry(table->entries, table->capacity, key);

	bool isNewKey = entry->key == NULL;
	if (isNewKey && IS_NULL(entry->value)) table->count++;		// IS_NULL for tombstones; treat them as full objects

	entry->key = key;
	entry->value = value;

	return isNewKey;
}


bool fei_table_delete(Table* table, ObjString* key)
{
	if (table->count == 0) return false;

	// find entry
	Entry* entry = fei_table_findentry(table->entries, table->capacity, key);
	if (entry->key == NULL) return false;

	// place tombstone
	entry->key = NULL;
	entry->value = BOOL_VAL(true);		//BOOL_VAL(true) as the tombstone

	return true;
}

void fei_table_mergefrom(Table* from, Table* to)
{
	for (int i = 0; i < from->capacity; i++)
	{
		Entry* entry = &from->entries[i];
		if (entry->key != NULL)
		{
			fei_table_set(to, entry->key, entry->value);
		}
	}
}

// used in VM to find the string
ObjString* fei_table_findstring(Table* table, const char* chars, int length, uint32_t hash)		// pass in raw character array 
{
	
	if (table->count == 0) return NULL;

	uint32_t index = hash % table->capacity;	// get index
	
	for (;;)
	{
		Entry* entry = &table->entries[index];		// get entry pointer
		if (entry->key == NULL)
		{
			// stop if found empty non-tombstone entry
			if (IS_NULL(entry->value)) return NULL;		// return null if not tombstone(tombstone value is BOOL_VAL(true))
		}
		else if (entry->key->length == length && entry->key->hash == hash
			&& memcmp(entry->key->chars, chars, length) == 0)
		{
			return entry->key;		// found the entry
		}

		index = (index + 1) % table->capacity;
	}
}

// removing unreachable pointers, used to remove string interns in garbage collection
void fei_table_removeunreachable(Table* table)
{
	for (int i = 0; i < table->capacity; i++)
	{
		Entry* entry = &table->entries[i];
		if (entry->key != NULL && !entry->key->obj.isMarked)		// remove not marked (string) object pointers
		{
			fei_table_delete(table, entry->key);
		}
	}
}


// mark global variables, used in VM for garbage collection
void fei_table_mark(Table* table)
{
	for (int i = 0; i < table->capacity; i++)
	{
		Entry* entry = &table->entries[i];
		// need to mark both the STRING KEYS and the actual value/obj itself
		fei_gcmem_markobject((Obj*)entry->key);			// mark the string key(ObjString type)
		fei_gcmem_markvalue(entry->value);				// mark the actual avlue
	}
}

void fei_chunk_init(Chunk* chunk)
{
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->code = NULL;			// dynamic array starts off completely empty
	chunk->lines = NULL;		// to store current line of code
	fei_valarray_init(&chunk->constants);		// initialize constant list
}

void fei_chunk_pushbyte(Chunk* chunk, uint8_t byte, int line)
{
	if (chunk->capacity < chunk->count + 1)				// check if chunk is full
	{
		int oldCapacity = chunk->capacity;
		chunk->capacity = GROW_CAPACITY(oldCapacity);	// get size of new capacity
		chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);	// reallocate memory and grow array
		chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
	}
		
	chunk->code[chunk->count] = byte;	// code is an array, [] is just the index number
	chunk->lines[chunk->count] = line;
	chunk->count++;
}



void fei_chunk_destroy(Chunk* chunk)
{
	FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);		// chunk->code is the pointer to the array, capacity is the size
	FREE_ARRAY(int, chunk->lines, chunk->capacity);
	fei_valarray_destroy(&chunk->constants);
	fei_chunk_init(chunk);
}

int fei_chunk_pushconst(Chunk* chunk, Value value)
{
	fei_vm_pushvalue(value);			// garbage collection
	fei_valarray_push(&chunk->constants, value);
	fei_vm_popvalue();					// garbage collection
	return chunk->constants.count - 1;			// return index of the newly added constant
}




int fei_dbgutil_printsimpleir(const char* name, int offset)
{
	printf("%s\n", name);	// print as a string, or char*
	return offset + 1;
}

int fei_dbgutil_printbyteir(const char* name, Chunk* chunk, int offset)
{
	uint8_t slot = chunk->code[offset + 1];
	printf("%-16s %4d\n", name, slot);
	return offset + 2;
}

int fei_dbgutil_printconstir(const char* name, Chunk* chunk, int offset)
{
	uint8_t constant = chunk->code[offset + 1];		// pullout the constant index from the subsequent byte in the chunk
	printf("%-16s %4d '", name, constant);			// print out name of the opcode, then the constant index
	fei_value_printvalue(chunk->constants.values[constant]);	//	display the value of the constant,  user defined function
	printf("'\n");
	return offset + 2;			//OP_RETURN is a single byte, and the other byte is the operand, hence offsets by 2
}

int fei_dbgutil_printinvokeir(const char* name, Chunk* chunk, int offset)
{
	uint8_t constant = chunk->code[offset + 1];				// get index of the name first
	uint8_t argCount = chunk->code[offset + 2];				// then get number of arguments
	printf("%-16s (%d args) %4d", name, argCount, constant);
	fei_value_printvalue(chunk->constants.values[constant]);			// print the method
	printf("\n");
	return offset + 3;
}

int fei_dbgutil_printjumpir(const char* name, int sign, Chunk* chunk, int offset)
{
	uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);	// get jump
	jump |= chunk->code[offset + 2];
	printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
	return offset + 3;

}

void fei_dbgdisas_chunk(Chunk* chunk, const char* name)
{
	printf("== %s ==\n", name);				// print a little header for debugging

	for (int offset = 0; offset < chunk->count;)	// for every existing instruction in the chunk
	{	
		offset = fei_dbgdisas_instr(chunk, offset);		// disassemble individually, offset will be controlled from this function
	}
}

int fei_dbgdisas_instr(Chunk* chunk, int offset)
{
	printf("%04d ", offset);	// print byte offset of the given instruction, or the index
	/* quick note on C placeholders
	say, we have int a = 2
	if %2d, it will be " 2"
	if %02d, it will be "02'
	*/


	// show source line each instruction was compiled from
	if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1])		// show a | for any instruction that comes from the 
																			//same source as its preceding one

	{
		printf("	| ");
	}
	else
	{
		printf("%4d ", chunk->lines[offset]);
	}

	uint8_t instruction = chunk->code[offset];		// takes one byte, or an element, from the container
	switch (instruction)
	{
	case OP_CONSTANT:
		return fei_dbgutil_printconstir("OP_CONSTANT", chunk, offset);		// pass in chunk to get ValueArray element
	
	// literals
	case OP_NULL:
		return fei_dbgutil_printsimpleir("OP_NULL", offset);
	case OP_TRUE:
		return fei_dbgutil_printsimpleir("OP_TRUE", offset);
	case OP_FALSE:
		return fei_dbgutil_printsimpleir("OP_FALSE", offset);

	case OP_EQUAL:
		return fei_dbgutil_printsimpleir("OP_EQUAL", offset);
	case OP_GREATER:
		return fei_dbgutil_printsimpleir("OP_GREATER", offset);
	case OP_LESS:
		return fei_dbgutil_printsimpleir("OP+LESS", offset);

	// unary
	case OP_NEGATE:
		return fei_dbgutil_printsimpleir("OP_NEGATE", offset);
	
	// binary
	case OP_ADD:
		return fei_dbgutil_printsimpleir("OP_ADD", offset);
	case OP_SUBTRACT:
		return fei_dbgutil_printsimpleir("OP_MINUS", offset);
	case OP_MULTIPLY:
		return fei_dbgutil_printsimpleir("OP_MULTIPLY", offset);
	case OP_DIVIDE:
		return fei_dbgutil_printsimpleir("OP_DIVIDE", offset);
	case OP_MODULO:
		return fei_dbgutil_printsimpleir("OP_MODULO", offset);

	case OP_NOT:
		return fei_dbgutil_printsimpleir("OP_NOT", offset);

	case OP_POP:
		return fei_dbgutil_printsimpleir("OP_POP", offset);

	// names for local variables do not get carried over, hence only the slot number is shown
	case OP_GET_LOCAL:
		return fei_dbgutil_printbyteir("OP_GET_LOCAL", chunk, offset);		
	case OP_SET_LOCAL:
		return fei_dbgutil_printbyteir("OP_SET_LOCAL", chunk, offset);

	case OP_GET_UPVALUE:
		return fei_dbgutil_printbyteir("OP_GET_UPVALUE", chunk, offset);
	case OP_SET_UPVALUE:
		return fei_dbgutil_printbyteir("OP_SET_UPVALUE", chunk, offset);
	case OP_GET_PROPERTY:
		return fei_dbgutil_printconstir("OP_GET_PROPERTY", chunk, offset);
	case OP_SET_PROPERTY:
		return fei_dbgutil_printconstir("OP_SET_PROPERTY", chunk, offset);

	case OP_CLOSE_UPVALUE:
		return fei_dbgutil_printsimpleir("OP_CLOSE_VALUE", offset);

	case OP_DEFINE_GLOBAL:
		return fei_dbgutil_printsimpleir("OP_DEFINE_GLOBAL", offset);
	case OP_GET_GLOBAL:
		return fei_dbgutil_printsimpleir("OP_GET_GLOBAL", offset);
	case OP_SET_GLOBAL:
		return fei_dbgutil_printsimpleir("OP_SET_GLOBAL", offset);
	case OP_PRINT:
		return fei_dbgutil_printsimpleir("OP_PRINT", offset);

	case OP_SWITCH_EQUAL:
		return fei_dbgutil_printsimpleir("OP_SWITCH_EQUAL", offset);

	case OP_JUMP:
		return fei_dbgutil_printjumpir("OP_JUMP", 1, chunk, offset);
	case OP_JUMP_IF_FALSE:
		return fei_dbgutil_printjumpir("OP_JUMP_IF_FALSE", 1, chunk, offset);

	case OP_CALL:
		return fei_dbgutil_printbyteir("OP_CALL", chunk, offset);

	case OP_METHOD:
		return fei_dbgutil_printconstir("OP_METHOD", chunk, offset);

	case OP_INVOKE:
		return fei_dbgutil_printinvokeir("OP_INVOKE", chunk, offset);


	case OP_CLOSURE:
	{
		offset++;
		uint8_t constant = chunk->code[offset++];			// index for Value
		printf("%-16s %4d ", "OP_CLOSURE", constant);
		fei_value_printvalue(chunk->constants.values[constant]);		// accessing the value using the index
		printf("\n");

		ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
		for (int j = 0; j < function->upvalueCount; j++)	// walk through upvalues
		{
			int isLocal = chunk->code[offset++];
			int index = chunk->code[offset++];
			printf("%04d	|	%s %d\n", offset - 2, isLocal ? "local" : "upvalue", index);
		}

		return offset;
	}

	case OP_CLASS:
		return fei_dbgutil_printconstir("OP_CLASS", chunk, offset);

	case OP_INHERIT:
		return fei_dbgutil_printsimpleir("OP_INEHEIRT", offset);


	case OP_GET_SUPER:				// class inheritance
		return fei_dbgutil_printconstir("OP_GET_SUPER", chunk, offset);

	case OP_SUPER_INVOKE:
		return fei_dbgutil_printinvokeir("OP_SUPER_INVOKE", chunk, offset);

	case OP_RETURN:
		return fei_dbgutil_printsimpleir("OP_RETURN", offset);		// dispatch to a utility function to display it

	case OP_LOOP:
		return fei_dbgutil_printjumpir("OP_LOOP", -1, chunk, offset);

	case OP_LOOP_IF_TRUE:
		return fei_dbgutil_printjumpir("OP_LOOP_IF_TRUE", -1, chunk, offset);

	case OP_LOOP_IF_FALSE:
		return fei_dbgutil_printjumpir("OP_LOOP_IF_FALSE", -1, chunk, offset);

	default:
		printf("Unknown opcode %d\n", instruction);
		return offset + 1;
	}
}


void fei_lexer_initsource(const char* source)
{
	scanner.start = source;			// again, pointing to a string array means pointing to the beginning
	scanner.current = source;		
	scanner.line = 1;
}

// to check for identifiers(eg. for, while, print)
bool fei_lexutil_isalpha(char c)
{
	return (c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		c == '_';
} 

bool fei_lexutil_isdigit(char c)
{
	return c >= '0' && c <= '9';		// let string comparison handle it
}

// to get EOF symbol -> '\0'
bool fei_lexer_isatend()
{
	return *scanner.current == '\0';
}



// goes to next char
char fei_lexer_advance()
{
	scanner.current++;				// advance to next 
	return scanner.current[-1];		// return previous one
}

// logical conditioning to check if 2nd character is embedded to first(e.g two char token)
bool fei_lexer_match(char expected)
{
	if (fei_lexer_isatend()) return false;			// if already at end, error
	if (*scanner.current != expected)
	{
		//printf("no match");
		return false;				// if current char does not equal expected char, it is false
	}
	//printf("match");
	scanner.current++;		// if yes, advance to next
	return true;
}

// make a token, uses the scanner's start and current to capture the lexeme and its size
Token fei_lexer_maketoken(TokenType type)
{
	Token token;
	token.type = type;
	token.start = scanner.start;
	token.length = (int)(scanner.current - scanner.start);
	token.line = scanner.line;

	return token;
}

// similar to fei_lexer_maketoken, but the "lexeme" points to the error message string instead of the scanner's current and start pointers
Token fei_lexer_errortoken(const char* message)
{
	Token token;
	token.type = TOKEN_ERROR;
	token.start = message;
	token.length = (int)strlen(message);	// get string length and turn to int
	token.line = scanner.line;

	return token;
}

// returns current character
char fei_lexer_peekcurrent()
{
	return *scanner.current;
}

// returns next character
char fei_lexer_peeknext()
{
	if (fei_lexer_isatend()) return '\0';
	return scanner.current[1];		// C syntax, basically return index 1 (or second) from the array/pointer
}

// skipping white spaces, tabs, etc.
void fei_lexer_skipspace()
{
	for (;;)
	{
		char c = fei_lexer_peekcurrent();
		switch (c)
		{
		case ' ':
		case '\r':
		case '\t':
			fei_lexer_advance();
			break;

		case '\n':				// if a new line is found, also add line number
			scanner.line++;		
			fei_lexer_advance();
			break;

		// for comments
		case '/':
			if (fei_lexer_peeknext() == '/')
			{
				// comment goes until end of line
				while (fei_lexer_peekcurrent() != '\n' && !fei_lexer_isatend()) fei_lexer_advance();		// if not new line or not end, treat as whitespace and advance
			}
			else {
				return;
			}

		default:
			return;
		}
	}
}


// to check for identifiers, if they are keyword or not. rest means the rest of the letter
TokenType fei_lexer_checkkw(int start, int length, const char* rest, TokenType type)
{
	/* hard expression here
	bascially if they are exactly the same, and compares their memory(memcmp)
	int memcmp(const void *str1, const void *str2, size_t n) -> if it is exactly the same, then it is 0
	*/
	if (scanner.current - scanner.start == start + length && memcmp(scanner.start + start, rest, length) == 0)
	{
		return type;
	}

	return TOKEN_IDENTIFIER;
}
// the 'trie' to store the set of strings
TokenType fei_lexer_scantype()
{
	switch (scanner.start[0])		// start of the lexeme
	{
	//case 'a': return fei_lexer_checkkw(1, 2, "nd", TOKEN_AND);
	case 'a':
	{
		if (scanner.current - scanner.start > 1)
		{
			switch (scanner.start[1])
			{
			case 'n': return fei_lexer_checkkw(2, 1, "d", TOKEN_AND);
			case 's': return fei_lexer_checkkw(2, 6, "signed", TOKEN_EQUAL);
			}
		}
	}
	case 'b': return fei_lexer_checkkw(1, 4, "reak", TOKEN_BREAK);
	case 'c':
	{
		if (scanner.current - scanner.start > 1)
		{
			switch (scanner.start[1])
			{
			case 'a': return fei_lexer_checkkw(2, 2, "se", TOKEN_CASE);
			case 'l': return fei_lexer_checkkw(2, 3, "ass", TOKEN_CLASS);
			case 'o': return fei_lexer_checkkw(2, 6, "ntinue", TOKEN_CONTINUE);
			}
		}
	}
	case 'd':
	{
		if (scanner.current - scanner.start > 1)
		{
			switch (scanner.start[1])
			{
			case 'e': return fei_lexer_checkkw(2, 5, "fault", TOKEN_DEFAULT);
			case 'o': return fei_lexer_checkkw(2, 0, "", TOKEN_DO);
			}
		}
	}
	case 'e': 
		if (scanner.current - scanner.start > 1)
		{
			switch (scanner.start[1])	// check if there is a second letter
			{
			case 'l':
				{
					if (scanner.current - scanner.start > 2)	// check if there is a third letter
					{
						switch (scanner.start[2])
						{
						case 's': return fei_lexer_checkkw(3, 1, "e", TOKEN_ELSE);
						case 'f': return fei_lexer_checkkw(3, 0, "", TOKEN_ELF);			// already matched
						}
					}
				}
			case 'q': return fei_lexer_checkkw(2, 4, "uals", TOKEN_EQUAL_EQUAL);
			}
		}
	case 'f':
		if (scanner.current - scanner.start > 1)	// check if there is a second letter
		{
			switch (scanner.start[1])
			{
			case 'a': return fei_lexer_checkkw(2, 3, "lse", TOKEN_FALSE);	// starts from 2 not 3, as first letter is already an f
			case 'o': return fei_lexer_checkkw(2, 1, "r", TOKEN_FOR);
			case 'r': return fei_lexer_checkkw(2, 2, "om", TOKEN_FROM);
			case 'n': return fei_lexer_checkkw(2, 0, "", TOKEN_FUN);
			case 'u': return fei_lexer_checkkw(2, 6, "nction", TOKEN_FUN);
			}
		}
	case 'i':
		if (scanner.current - scanner.start > 1)
		{
			switch (scanner.start[1])
			{
			case 'f': return fei_lexer_checkkw(2, 0, "", TOKEN_IF);
			case 's': return fei_lexer_checkkw(2, 0, "", TOKEN_EQUAL_EQUAL);
			}
		}
	case 'n': return fei_lexer_checkkw(1, 3, "ull", TOKEN_NULL);
	case 'o': return fei_lexer_checkkw(1, 1, "r", TOKEN_OR);
	case 'p': return fei_lexer_checkkw(1, 7, "rint__keyword", TOKEN_PRINT);
	//case 'r': return fei_lexer_checkkw(1, 5, "eturn", TOKEN_RETURN);
	case 'r':
		if (scanner.current - scanner.start > 1)
		{
			switch (scanner.start[1])
			{
			case 'e':
				if (scanner.current - scanner.start > 2)
				{
					switch (scanner.start[2])
					{
					case 't': return fei_lexer_checkkw(3, 3, "urn", TOKEN_RETURN);
					case 'p': return fei_lexer_checkkw(3, 3, "eat", TOKEN_REPEAT);
					}
				}
			}
		}
	case 's': 
		if (scanner.current - scanner.start > 1)			// if there is a second letter
		{
			switch (scanner.start[1])
			{
			case 'u': return fei_lexer_checkkw(2, 3, "per", TOKEN_SUPER);
			case 'w': return fei_lexer_checkkw(2, 4, "itch", TOKEN_SWITCH);
			}
		}
	case 't':
		if (scanner.current - scanner.start > 1)
		{
			switch (scanner.start[1])
			{
			//case 'h': return fei_lexer_checkkw(2, 2, "is", TOKEN_THIS);
			case 'h':
				if (scanner.current - scanner.start > 2)	// check if there is a third letter
				{
					switch (scanner.start[2])
					{
					case 'e': return fei_lexer_checkkw(3, 1, "n", TOKEN_THEN);
					case 'i': return fei_lexer_checkkw(3, 1, "s", TOKEN_THIS);			// already matched
					}
				}
			case 'r': return fei_lexer_checkkw(2, 2, "ue", TOKEN_TRUE);
			}
		}
	case 'u': return fei_lexer_checkkw(1, 4, "ntil", TOKEN_UNTIL);
	case 'v': return fei_lexer_checkkw(1, 2, "ar", TOKEN_VAR);
	case 'w': return fei_lexer_checkkw(1, 4, "hile", TOKEN_WHILE);

	}


	return TOKEN_IDENTIFIER;
}

Token fei_lexer_scanident()
{
	while (fei_lexutil_isalpha(fei_lexer_peekcurrent()) || fei_lexutil_isdigit(fei_lexer_peekcurrent())) fei_lexer_advance();		// skip if still letters or digits 
	return fei_lexer_maketoken(fei_lexer_scantype());
}

Token fei_lexer_scannumber()
{
	while (fei_lexutil_isdigit(fei_lexer_peekcurrent())) fei_lexer_advance();		// while next is still a digit advance

	// look for fractional part	
	if (fei_lexer_peekcurrent() == '.' && fei_lexutil_isdigit(fei_lexer_peeknext()))		// if there is a . and next is still digit
	{
		// consume '.'
		fei_lexer_advance();

		while (fei_lexutil_isdigit(fei_lexer_peekcurrent())) fei_lexer_advance();
	}

	return fei_lexer_maketoken(TOKEN_NUMBER);
}

// for string tokens
Token fei_lexer_scanstring()
{
	while (fei_lexer_peekcurrent() != '"' && !fei_lexer_isatend())
	{
		if (fei_lexer_peekcurrent() == '\n') scanner.line++;		// allow strings to go until next line
		fei_lexer_advance();		// consume characters until the closing quote is reached
	}

	if (fei_lexer_isatend()) return fei_lexer_errortoken("Unterminated string.");

	// closing quote
	fei_lexer_advance();
	return fei_lexer_maketoken(TOKEN_STRING);

	// convert lexeme to runtime value later
}

// reading the char, and return a token
Token fei_lexer_scantoken()
{
	fei_lexer_skipspace();

	scanner.start = scanner.current;		// reset the scanner to current

	if (fei_lexer_isatend()) return fei_lexer_maketoken(TOKEN_EOF);		// check if at end

	// if not end of file
	char c = fei_lexer_advance();

	if (fei_lexutil_isalpha(c)) return fei_lexer_scanident();
	if (fei_lexutil_isdigit(c)) return fei_lexer_scannumber();		// fei_lexer_scannumber() is a TOKEN_NUMBER


	// lexical grammar for the language
	switch (c)
	{
		// for single characters
	case '(': return fei_lexer_maketoken(TOKEN_LEFT_PAREN);
	case ')': return fei_lexer_maketoken(TOKEN_RIGHT_PAREN);
	case '{': return fei_lexer_maketoken(TOKEN_LEFT_BRACE);
	case '}': return fei_lexer_maketoken(TOKEN_RIGHT_BRACE);
	case ';': return fei_lexer_maketoken(TOKEN_SEMICOLON);
	case ':': return fei_lexer_maketoken(TOKEN_COLON);
	case ',': return fei_lexer_maketoken(TOKEN_COMMA);
	case '.': return fei_lexer_maketoken(TOKEN_DOT);
	case '-': return fei_lexer_maketoken(TOKEN_MINUS);
	case '+': return fei_lexer_maketoken(TOKEN_PLUS);
	case '*': return fei_lexer_maketoken(TOKEN_STAR);
	case '/': return fei_lexer_maketoken(TOKEN_SLASH);
	case '%': return fei_lexer_maketoken(TOKEN_MODULO);

		// for two characters
	case '!':
		return fei_lexer_maketoken(fei_lexer_match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
	case '=':
		return fei_lexer_maketoken(fei_lexer_match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
	case '>':
		return fei_lexer_maketoken(fei_lexer_match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
	case '<':
		return fei_lexer_maketoken(fei_lexer_match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);

		// literal tokens
	case '"': return fei_lexer_scanstring();			// string token
	}



	return fei_lexer_errortoken("Unexpected character.");
}


static void fei_comprule_logicaland(bool canAssign);
static void fei_comprule_binary(bool canAssign);
static void fei_comprule_call(bool canAssign);
static void fei_comprule_dot(bool canAssign);
static void fei_comprule_literal(bool canAssign);
static void fei_comprule_grouping(bool canAssign);
static void fei_comprule_number(bool canAssign);
static void fei_comprule_logicalor(bool canAssign);
static void fei_comprule_string(bool canAssign);
static void fei_comprule_variable(bool canAssign);
static void fei_comprule_super(bool canAssign);
static void fei_comprule_this(bool canAssign);
static void fei_comprule_unary(bool canAssign);


/* the array of ParseRules 
uses C99 DESIGNATED INITIALIZER syntax
use {struct members} to initialize a struct
[index number] = {struct members}, the index number can be seen clearly
token enums from scanner is reused
*/
ParseRule rules[] =
{
	// function calls are like infixes, with high precedence on the left, ( in the middle for arguments, then ) at the end
	[TOKEN_LEFT_PAREN]		= {fei_comprule_grouping,	fei_comprule_call,	 PREC_CALL},		// call for functions	
	[TOKEN_RIGHT_PAREN]		= {NULL,     NULL,   PREC_NONE},
	[TOKEN_LEFT_BRACE]		= {NULL,     NULL,   PREC_NONE},
	[TOKEN_RIGHT_BRACE]		= {NULL,     NULL,   PREC_NONE},
	[TOKEN_COMMA]			= {NULL,     NULL,   PREC_NONE},
	[TOKEN_DOT]				= {NULL,     fei_comprule_dot,   PREC_CALL},
	[TOKEN_MINUS]			= {fei_comprule_unary,    fei_comprule_binary, PREC_TERM},
	[TOKEN_PLUS]			= {NULL,     fei_comprule_binary, PREC_TERM},
	[TOKEN_SEMICOLON]		= {NULL,     NULL,   PREC_NONE},
	[TOKEN_SLASH]			= {NULL,     fei_comprule_binary, PREC_FACTOR},
	[TOKEN_STAR]			= {NULL,     fei_comprule_binary, PREC_FACTOR},
	[TOKEN_MODULO]			= {NULL,	 fei_comprule_binary, PREC_FACTOR},
	[TOKEN_BANG]			= {fei_comprule_unary,     NULL,   PREC_NONE},
	[TOKEN_BANG_EQUAL]		= {NULL,     fei_comprule_binary,   PREC_EQUALITY},	// equality precedence
	[TOKEN_EQUAL]			= {NULL,     fei_comprule_binary,   PREC_COMPARISON},		// comaprison precedence
	[TOKEN_EQUAL_EQUAL]		= {NULL,     fei_comprule_binary,   PREC_COMPARISON},
	[TOKEN_GREATER]			= {NULL,     fei_comprule_binary,   PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL]	= {NULL,     fei_comprule_binary,   PREC_COMPARISON},
	[TOKEN_LESS]			= {NULL,     fei_comprule_binary,   PREC_COMPARISON},
	[TOKEN_LESS_EQUAL]		= {NULL,      fei_comprule_binary,  PREC_COMPARISON},
	[TOKEN_IDENTIFIER]		= {fei_comprule_variable,     NULL,   PREC_NONE},
	[TOKEN_STRING]			= {fei_comprule_string,     NULL,   PREC_NONE},
	[TOKEN_NUMBER]			= {fei_comprule_number,   NULL,   PREC_NONE},
	[TOKEN_AND]				= {NULL,     fei_comprule_logicaland,   PREC_AND},
	[TOKEN_CLASS]			= {NULL,     NULL,   PREC_NONE},
	[TOKEN_ELSE]			= {NULL,     NULL,   PREC_NONE},
	[TOKEN_FALSE]			= {fei_comprule_literal,     NULL,   PREC_NONE},
	[TOKEN_FOR]				= {NULL,     NULL,   PREC_NONE},
	[TOKEN_FUN]				= {NULL,     NULL,   PREC_NONE},
	[TOKEN_IF]				= {NULL,     NULL,   PREC_NONE},
	[TOKEN_SWITCH] = {NULL,     NULL,   PREC_NONE},
	[TOKEN_NULL]			= {fei_comprule_literal,     NULL,   PREC_NONE},
	[TOKEN_OR]				= {NULL,     fei_comprule_logicalor,   PREC_OR},
	[TOKEN_PRINT]			= {NULL,     NULL,   PREC_NONE},
	[TOKEN_RETURN]			= {NULL,     NULL,   PREC_NONE},
	[TOKEN_SUPER]			= {fei_comprule_super,     NULL,   PREC_NONE},
	[TOKEN_THIS]			= {fei_comprule_this,     NULL,   PREC_NONE},
	[TOKEN_TRUE]			= {fei_comprule_literal,     NULL,   PREC_NONE},
	[TOKEN_VAR]				= {NULL,     NULL,   PREC_NONE},
	[TOKEN_WHILE]			= {NULL,     NULL,   PREC_NONE},
	[TOKEN_ERROR]			= {NULL,     NULL,   PREC_NONE},
	[TOKEN_EOF]				= {NULL,     NULL,   PREC_NONE},
};


Chunk* fei_compiler_currentchunk()
{
	return &current->function->chunk;
}

// to handle syntax errors
void fei_compiler_raiseat(Token* token, const char* message)
{
	if (parser.panicMode) return;		// if an error already exists, no need to run other errors
	parser.panicMode = true;

	fprintf(stderr, "Error at [Line %d]", token->line);

	if (token->type == TOKEN_EOF)
	{
		fprintf(stderr, " at end");
	}
	else if (token->type == TOKEN_ERROR)
	{
		// nothing
	}
	else {
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);
	parser.hadError = true;
}

// error from token most recently CONSUMED
void fei_compiler_raiseerror(const char* message)
{
	fei_compiler_raiseat(&parser.previous, message);
}


// handling error from token, the most current one being handed, not yet consumed
void fei_compiler_raisehere(const char* message)			// manually provide the message
{
	fei_compiler_raiseat(&parser.current, message);				// pass in the current parser
}


/* main compile functions */

// pump the compiler, basically go to / 'read' the next token, a SINGLE token
void fei_compiler_advancenext()
{
	parser.previous = parser.current;		//  store next parser as current

	for (;;)
	{
		parser.current = fei_lexer_scantoken();		// gets next token, stores it for later use(the next scan) 

		if (parser.current.type != TOKEN_ERROR) break;			// if error is not found break

		fei_compiler_raisehere(parser.current.start);			// start is the location/pointer of the token source code
	}
}


// advance while skipping the given parameter, give none to skip nothing
void fei_compiler_advanceskipping(TokenType type)
{
	parser.previous = parser.current;		//  store next parser as current

	for (;;)
	{
		parser.current = fei_lexer_scantoken();		// gets next token, stores it for later use(the next scan) 

		if (parser.current.type == type)
			continue;

		if (parser.current.type != TOKEN_ERROR) break;			// if error is not found break

		fei_compiler_raisehere(parser.current.start);			// start is the location/pointer of the token source code
	}
}


// SIMILAR to advance but there is a validation for a certain type
// syntax error comes from here, where it is known/expected what the next token will be
void fei_compiler_consume(TokenType type, const char* message)
{
	if (parser.current.type == type)			// if current token is equal to the token type being compared to
	{
		fei_compiler_advancenext();
		return;
	}

	fei_compiler_raisehere(message);		// if consumes a different type, error
}

bool fei_compiler_check(TokenType type)
{
	return parser.current.type == type;			// check if current matches given
}


bool fei_compiler_match(TokenType type)
{
	if (!fei_compiler_check(type)) return false;
	fei_compiler_advancenext();
	return true;
}

/* emitting BYTECODE for the VM to understand */
// the fei_chunk_pushbyte for the compiler
void fei_compiler_emitbyte(uint8_t byte)
{
	fei_chunk_pushbyte(fei_compiler_currentchunk(), byte, parser.previous.line);		// sends previous line so runtime errors are associated with that line
}

// write chunk for multiple chunks, used to write an opcode followed by an operand(eg. in constants)
void fei_compiler_emitbytes(uint8_t byte1, uint8_t byte2)
{
	fei_compiler_emitbyte(byte1);
	fei_compiler_emitbyte(byte2);
}

// for looping statements
void fei_compiler_emitloop(int loopStart)
{
	fei_compiler_emitbyte(OP_LOOP);

	// int below jumps back, + 2 accounting the OP_LOOP and the instruction's own operand
	int offset = fei_compiler_currentchunk()->count - loopStart + 2;			
	if (offset > UINT16_MAX) fei_compiler_raiseerror("Loop body too large.");

	fei_compiler_emitbyte((offset >> 8) & 0xff);
	fei_compiler_emitbyte(offset & 0xff);
}

void fei_compiler_emitcondloop(int loopStart, bool state)
{
	if (state) 
		fei_compiler_emitbyte(OP_LOOP_IF_TRUE);
	else 
		fei_compiler_emitbyte(OP_LOOP_IF_FALSE);

	int offset = fei_compiler_currentchunk()->count - loopStart + 2;
	if (offset > UINT16_MAX) fei_compiler_raiseerror("Loop body too large.");

	fei_compiler_emitbyte((offset >> 8) & 0xff);
	fei_compiler_emitbyte(offset & 0xff);
}



int fei_compiler_emitjump(uint8_t instruction)
{
	/* backpatching */
	fei_compiler_emitbyte(instruction);	// writes a placeholder operand for jump offset
	fei_compiler_emitbyte(0xff);			// hexadecimal number with value of 255
	fei_compiler_emitbyte(0xff);

	// basically, get the difference in bytes before the two 0xff is added
	return fei_compiler_currentchunk()->count - 2;
}

//  emit specific return type
void fei_compiler_emitreturn()
{
	if (current->type == TYPE_INITIALIZER)		// class constructor
	{	
		fei_compiler_emitbytes(OP_GET_LOCAL, 0);					// return the instance
	}
	else
	{
		fei_compiler_emitbyte(OP_NULL);			// for functions that return nothing
	}
	
	fei_compiler_emitbyte(OP_RETURN);		// emit return type at the end of a compiler
}

// to insert into constant table
uint8_t fei_compiler_makeconst(Value value)
{
	int constant = fei_chunk_pushconst(fei_compiler_currentchunk(), value);
	if (constant > UINT8_MAX)
	{
		fei_compiler_raiseerror("Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)constant;		// return as byte, the byte being the INDEX of the constantin the constats array
}

void fei_compiler_emitconst(Value value)		// for constant emit the opcode, then the index
{
	fei_compiler_emitbytes(OP_CONSTANT, fei_compiler_makeconst(value));	// add value to constant table
}

void fei_compiler_patchjump(int offset)
{
	// - 2 to adjust for the jump offset itself
	int jump = fei_compiler_currentchunk()->count - offset - 2;

	if (jump > UINT16_MAX)
	{
		fei_compiler_raiseerror("Too much code to jump over.");
	}

	// the fei_compiler_patchjump provides the VALUE or amount to JUMP
	fei_compiler_currentchunk()->code[offset] = (jump >> 8) & 0xff;		// right shift by 8, then bitwise AND with 255(oxff is 111111)
	fei_compiler_currentchunk()->code[offset + 1] = jump & 0xff;			// only AND
}

// initialize the compiler
void fei_compiler_init(Compiler* compiler, FunctionType type)
{
	compiler->enclosing = current;			// the 'outer' compiler
	compiler->function = NULL;
	compiler->type = type;
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	compiler->function = fei_object_makefunction();
	current = compiler;				// current is the global variable pointer for the Compiler struct, point to to the parameter
									// basically assign the global pointer 

	// for functions
	if (type != TYPE_SCRIPT)
	{
		current->function->name = fei_object_copystring(parser.previous.start, parser.previous.length);		// function name handled here
	}

	// compiler implicitly claims slot zero for local variables
	Local* local = &current->locals[current->localCount++];
	local->isCaptured = false;
	
	// for this tags 
	if (type != TYPE_FUNCTION)			// for none function types, for class methods
	{
		local->name.start = "this";
		local->name.length = 4;
	}
	else                               // for functions
	{
		local->name.start = "";
		local->name.length = 0;
	}

	// for loop scopes, for break and continue statements
	compiler->loopCountTop = -1;
	compiler->continueJumpCapacity = 4;
	compiler->continueJumps = ALLOCATE(int, 4);

	// use memset to initialize array to 0
	memset(compiler->breakJumpCounts, 0, UINT8_COUNT * sizeof(compiler->breakJumpCounts[0]));
}

ObjFunction* fei_compiler_endcompiler()
{
	fei_compiler_emitreturn();
	ObjFunction* function = current->function;

	FREE(int, current->continueJumps);


	// for debugging
#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError)
	{
		fei_dbgdisas_chunk(fei_compiler_currentchunk(), function->name != NULL ? function->name->chars : "<script>");	// if name is NULL then it is the Script type(main()
	}
#endif

	current = current->enclosing;	// return back to enclosing compiler after function
	return function;			// return to free
}

void fei_compiler_beginscope()
{
	current->scopeDepth++;
}

void fei_compiler_endscope()
{
	current->scopeDepth--;

	// remove variables out of scope
	while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth)
	{
		/* at the end of a block scope, when the compiler emits code to free the stack slot for the locals, 
		tell which one to hoist to the heap
		*/
		if (current->locals[current->localCount - 1].isCaptured)	// if it is captured/used
		{
			fei_compiler_emitbyte(OP_CLOSE_UPVALUE);	// op code to move the upvalue to the heap
		}
		else{
			fei_compiler_emitbyte(OP_POP);			// if not used anymore/capture simply pop the value off the stack
		}

		current->localCount--;
	}
}

// loop enclosing
void fei_compiler_beginloopscope()
{
	current->loopCountTop++;
}

void fei_compiler_endloopscope()
{
	if (current->breakJumpCounts[current->loopCountTop] > 0)
		current->breakJumpCounts[current->loopCountTop] = 0;

	current->loopCountTop--;
}

// mark current chunk for continue jump
void fei_compiler_markcontinuejump()
{
	current->continueJumps[current->loopCountTop] = fei_compiler_currentchunk()->count;
}

// patch available break jumps
void fei_compiler_patchbreakjumps()
{
	for (int i = 0; i < current->breakJumpCounts[current->loopCountTop]; i++)
	{
		fei_compiler_patchjump(current->breakPatchJumps[current->loopCountTop][i]);
	}
}

/* variable declarations */
uint8_t fei_compiler_makeidentconst(Token* name)
{
	return fei_compiler_makeconst(OBJ_VAL(fei_object_copystring(name->start, name->length)));	// add to constant table
}

bool fei_compiler_identsequal(Token* a, Token* b)
{
	if (a->length != b->length) return false;
	return memcmp(a->start, b->start, a->length) == 0;
}


int fei_compiler_resolvelocal(Compiler* compiler, Token* name)
{
	for (int i = compiler->localCount - 1; i >= 0; i--)		// walk through the local variables
	{
		Local* local = &compiler->locals[i];
		if (fei_compiler_identsequal(name, &local->name))			
		{	
			if (local->depth == -1)
			{
				fei_compiler_raiseerror("Cannot read local variable in its own initializer.");
			}
			return i;			// found the var, return the index
		}
	}
	
	return -1;			// not found, name is global variable
}


// add upvalue
int fei_compiler_addupvalue(Compiler* compiler, uint8_t index, bool isLocal)
{
	int upvalueCount = compiler->function->upvalueCount;	// get current upvalue count

	// check whether the upvalue has already been declared
	for (int i = 0; i < upvalueCount; i++)
	{
		Upvalue* upvalue = &compiler->upvalues[i];			// get pointer for each upvalue in the array
		if (upvalue->index == index && upvalue->isLocal == isLocal)
		{
			return i;				// if found, return the index of the upvalue in the upvalue array
		}
	}

	if (upvalueCount == UINT8_COUNT)
	{
		fei_compiler_raiseerror("Too many closure variables");
		return 0;
	}

	// compiler keeps an array of upvalue structs to track closed-over identifiers
	// indexes in the array match the indexes of ObjClosure at runtime
	// insert to upvalues array
	compiler->upvalues[upvalueCount].isLocal = isLocal;		// insert bool status
	compiler->upvalues[upvalueCount].index = index;			// insert index
	return compiler->function->upvalueCount++;				// increase count and return
}


/*	for closures
- fei_compiler_resolveupvalue looks for a local variable declared in any of the surrounding functions
- if it finds one it returns the index for that upvalue variable, otherwise returns -1
*/
int fei_compiler_resolveupvalue(Compiler* compiler, Token* name)
{
	if (compiler->enclosing == NULL) return -1;		// if in main()

	int local = fei_compiler_resolvelocal(compiler->enclosing, name);	// looks for local value in enclosing function/compiler
	if (local != -1)
	{
		compiler->enclosing->locals[local].isCaptured = true;	// mark local is captured/used by and upvalue
		return fei_compiler_addupvalue(compiler, (uint8_t)local, true);		// create up value
	}

	// recursion to solve nested upvalues
	// recursive call right in the middle
	int upvalue = fei_compiler_resolveupvalue(compiler->enclosing, name);	// if the enclosing function is main() (NULL), it returns -1
	if (upvalue != -1)
	{
		return fei_compiler_addupvalue(compiler, (uint8_t)upvalue, true);
	}


	return -1;
}


void fei_compiler_addlocal(Token name)
{
	if (current->localCount == UINT8_COUNT)
	{
		fei_compiler_raiseerror("Too many local variables in block.");
		return;
	}

	Local* local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = -1;			// for cases where a variable name is redefined inside another scope, using the variable itself
	local->isCaptured = false;
}

void fei_compiler_declvarfromcurrent()	// for local variables
{
	// global vars are implicitly declared, and are late bound, not 'initialized' here but in the VM
	if (current->scopeDepth == 0) return;


	/* local variable declaration happens below */
	Token* name = &parser.previous;

	// to not allow two variable declarations to have the same name
	// loop only checks to a HIGHER SCOPE; another block overlaping/shadowing is allowed
	// work backwards
	for (int i = current->localCount - 1; i >= 0; i--)			
	{
		Local* local = &current->locals[i];
		if (local->depth != -1 && local->depth < current->scopeDepth)	// if reach beginning of array(highest scope)
		{
			break;
		}

		if (fei_compiler_identsequal(name, &local->name))
		{
			fei_compiler_raiseerror("Variable with this name exists in scope.");
		}
	}

	fei_compiler_addlocal(*name);
}

uint8_t fei_compiler_parsevarfromcurrent(const char* errorMessage)
{
	fei_compiler_consume(TOKEN_IDENTIFIER, errorMessage);		// requires next token to be an identifier

	fei_compiler_declvarfromcurrent();
	if (current->scopeDepth > 0) return 0;			// if scopeDepth is not 0, then it is a local not global var
	// return a dummy index
	// at runtime, locals are not looked up by name so no need to insert them to a table


	return fei_compiler_makeidentconst(&parser.previous);	// return index from the constant table	
}


void fei_compiler_markinit()
{
	if (current->scopeDepth == 0) return;				// if global return
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

void fei_compiler_defvarindex(uint8_t global)
{
	if (current->scopeDepth > 0)
	{
		fei_compiler_markinit();
		return;
	}

	fei_compiler_emitbytes(OP_DEFINE_GLOBAL, global);	// opcode for declaration and the constant itself
}


// for function arguments, returns number of arguments
// each argument expression generates code which leaves value on the stack in preparation for the call
uint8_t fei_compiler_parsearglist()
{
	uint8_t argCount = 0;
	if (!fei_compiler_check(TOKEN_RIGHT_PAREN))		// if ) has not been reached
	{
		do
		{
			fei_compiler_parseexpr();		// collect the arguments
			
			if (argCount == 255)			// cannot have more than 255 arguments as each operand is a single byte(uint8_t)
			{		
				fei_compiler_raiseerror("Cannot have more than 255 arguments.");
			}

			argCount++;
		} while (fei_compiler_match(TOKEN_COMMA));
	}

	fei_compiler_consume(TOKEN_RIGHT_PAREN, "Expect ')' after argument list.");
	return argCount;
}

static void fei_comprule_logicaland(bool canAssign)
{
	int endJump = fei_compiler_emitjump(OP_JUMP_IF_FALSE);		// left hand side is already compiled,
													// and if it is false skip it and go to next

	fei_compiler_emitbyte(OP_POP);
	fei_compiler_parseprec(PREC_AND);


	fei_compiler_patchjump(endJump);
}



// for binary, eg. 5 + 4
// or INFIX parser, where the operator is in the middle
// entire left hand expression has been compiled, and the infix operator has been consumed
// fei_comprule_binary() handles the rest of the arithmetic operator
static void fei_comprule_binary(bool canAssign)
{
	// remember type of operator, already consumed
	TokenType  operatorType = parser.previous.type;
	
	// compile right operand
	ParseRule* rule = fei_compiler_getrule(operatorType);		// the BIDMAS rule, operands in the right side have HIGHER PRECEDENCE
													// as binary operators are LEFT ASSOCIATIVE
	// recursively call fei_compiler_parseprec again
	fei_compiler_parseprec((Precedence)(rule->precedence + 1));		// conert from rule to enum(precedence) type

	switch (operatorType)
	{
		// note how NOT opcode is at the end
		// six binary operators for three instructions only(greater, not, equal)
	case TOKEN_BANG_EQUAL: fei_compiler_emitbytes(OP_EQUAL, OP_NOT); break;		// add equal and not to the stack
	case TOKEN_EQUAL_EQUAL: fei_compiler_emitbyte(OP_EQUAL); break;
	case TOKEN_GREATER: fei_compiler_emitbyte(OP_GREATER); break;
	case TOKEN_GREATER_EQUAL: fei_compiler_emitbytes(OP_LESS, OP_NOT); break;
	case TOKEN_LESS: fei_compiler_emitbyte(OP_LESS);	break;
	case TOKEN_LESS_EQUAL: fei_compiler_emitbytes(OP_GREATER, OP_NOT); break;

	case TOKEN_PLUS:	fei_compiler_emitbyte(OP_ADD); break;
	case TOKEN_MINUS:	fei_compiler_emitbyte(OP_SUBTRACT); break;
	case TOKEN_STAR:	fei_compiler_emitbyte(OP_MULTIPLY); break;
	case TOKEN_SLASH:	fei_compiler_emitbyte(OP_DIVIDE); break;
	case TOKEN_MODULO:  fei_compiler_emitbyte(OP_MODULO); break;
	default:
		return;			// unreachable
	}
}


// for function calls
static void fei_comprule_call(bool canAssign)
{
	// again, assumes the function itself(its call name) has been placed on the codestream stack
	uint8_t argCount = fei_compiler_parsearglist();		// compile arguments using fei_compiler_parsearglist
	fei_compiler_emitbytes(OP_CALL, argCount);			// write on the chunk
}

// class members/fields/properties
static void fei_comprule_dot(bool canAssign)
{
	fei_compiler_consume(TOKEN_IDENTIFIER, "Expect propery name after class instance.");
	uint8_t name = fei_compiler_makeidentconst(&parser.previous);			// already consumed

	if (canAssign && fei_compiler_match(TOKEN_EQUAL))		// assignment
	{	
		fei_compiler_parseexpr();					// evalute expression to be set
		fei_compiler_emitbytes(OP_SET_PROPERTY, name);
	}
	else if (fei_compiler_match(TOKEN_LEFT_PAREN))			// for running class methods, access the method and call it at the same time
	{
		uint8_t argCount = fei_compiler_parsearglist();
		
		/* new OP_INVOKE opcode that takes two operands:
		1. the index of the property name in the constant table
		2. the number of arguments passed in the methods
		*** combines OP_GET_PROPERTY and OP_CALL
		*/
		fei_compiler_emitbytes(OP_INVOKE, name);			
		fei_compiler_emitbyte(argCount);
	}
	else								// simply get
	{
		fei_compiler_emitbytes(OP_GET_PROPERTY, name);
	}
}

static void fei_comprule_literal(bool canAssign)
{
	switch (parser.previous.type)
	{
	case TOKEN_FALSE: fei_compiler_emitbyte(OP_FALSE); break;
	case TOKEN_TRUE: fei_compiler_emitbyte(OP_TRUE); break;
	case TOKEN_NULL: fei_compiler_emitbyte(OP_NULL); break;

	default:		// unreachable
		return;
	}
}

// parentheses for grouping
static void fei_comprule_grouping(bool canAssign)
{
	// assume initial ( has already been consumed, and recursively call to fei_compiler_parseexpr() to compile between the parentheses
	fei_compiler_parseexpr();
	fei_compiler_consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");	// expects a right parentheses, if not received then  error
}


/* parsing the tokens */
static void fei_comprule_number(bool canAssign)
{
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
	fei_compiler_emitconst(NUMBER_VAL(value));
}

static void fei_comprule_logicalor(bool canAssign)
{
	// jump if left hand side is true
	int elseJump = fei_compiler_emitjump(OP_JUMP_IF_FALSE);		// if left is false jump directly to right hand
	int endJump = fei_compiler_emitjump(OP_JUMP);				// if not skipped(as left is true) jump the right hand

	fei_compiler_patchjump(elseJump);
	fei_compiler_emitbyte(OP_POP);

	fei_compiler_parseprec(PREC_OR);
	fei_compiler_patchjump(endJump);
}

// 'initialize' the string here
static void fei_comprule_string(bool canAssign)
{
	// in a string, eg. "hitagi", the quotation marks are trimmed
	fei_compiler_emitconst(OBJ_VAL(fei_object_copystring(parser.previous.start + 1, parser.previous.length - 2)));
}

// declare/call variables
void fei_compiler_declnamedvar(Token name, bool canAssign)
{
	uint8_t getOp, setOp;
	int arg = fei_compiler_resolvelocal(current, &name);		// try find a local variable with a given name
	if (arg != -1)
	{
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	}
	else if ((arg = fei_compiler_resolveupvalue(current, &name)) != -1)		// for upvalues
	{
		getOp = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE;
	}
	else
	{
		arg = fei_compiler_makeidentconst(&name);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
	}

	
	// test case to check whether it is a get(just the name) or a reassignment
	if (canAssign && fei_compiler_match(TOKEN_EQUAL))		// if a = follows right after
	{
		fei_compiler_parseexpr();
		fei_compiler_emitbytes(setOp, (uint8_t)arg);			// reassignment/set
	}
	else
	{
		fei_compiler_emitbytes(getOp, (uint8_t)arg);			// as normal get
		// printf("gest");
	}

}


static void fei_comprule_variable(bool canAssign)
{
	fei_compiler_declnamedvar(parser.previous, canAssign);
}

// for super classes, token that mimics as if a user types in 'super'
Token fei_compiler_makesyntoken(const char* text)
{
	Token token;
	token.start = text;
	token.length = (int)strlen(text);			// strlen to get char* length
	return token;
}

// for super calls
static void fei_comprule_super(bool canAssign)
{
	// if token is not inside a class
	if (currentClass == NULL)
	{
		fei_compiler_raiseerror("'super' can only be initialized inside a class.");
	}
	else if (!currentClass->hasSuperclass)		// if class has no parent class
	{
		fei_compiler_raiseerror("'super' cannot be used on a class with no parent class.");
	}


	fei_compiler_consume(TOKEN_DOT, "Expect '.' after 'super'.");
	fei_compiler_consume(TOKEN_IDENTIFIER, "Expect parent class method identifier.");
	uint8_t name = fei_compiler_makeidentconst(&parser.previous);			// get identifier index


	/*
	in order to access a superclass method on the CURRENT INSTANCE, runtime needs both the receiver and the superclass
	of the surrounding method's class.
	1. first fei_compiler_declnamedvar call generates code to look up the current receiver and push it to the stack
	2. second fei_compiler_declnamedvar emits code to look up the superclass and push that on top
	*/
	fei_compiler_declnamedvar(fei_compiler_makesyntoken("this"), false);
	if (fei_compiler_match(TOKEN_LEFT_PAREN))			// if there is a parameter list, invoke super method
	{
		uint8_t argCount = fei_compiler_parsearglist();
		fei_compiler_declnamedvar(fei_compiler_makesyntoken("super"), false);
		fei_compiler_emitbytes(OP_SUPER_INVOKE, name);		// super invoke opcode
		fei_compiler_emitbyte(argCount);
	}
	else
	{
		fei_compiler_declnamedvar(fei_compiler_makesyntoken("super"), false);
		fei_compiler_emitbytes(OP_GET_SUPER, name);
	}
}


// for class methods
static void fei_comprule_this(bool canAssign)
{
	// if not inside a class
	if (currentClass == NULL)
	{
		fei_compiler_raiseerror("Cannot use 'this' outside of class.");
		return;
	}

	fei_comprule_variable(false);			// always false
}


static void fei_comprule_unary(bool canAssign)
{
	TokenType operatorType = parser.previous.type;		// leading - token has already been consumed 

	// compile operand
	fei_compiler_parseexpr();

	switch (operatorType)
	{
	case TOKEN_BANG: fei_compiler_emitbyte(OP_NOT); break;


		// OP_NEGATE should be emitted last, AFTER the constant itself 
		// eg. say 4 - 5; 5 needs to be emitted and added to the chunk->constants first before OP_NEGATE
		/* it is important to take note of the precedence
		e.g -a.b + 3;
		when the unary negation is called, all of a.b + 3 will be consumed in fei_compiler_parseexpr(). Hence, a method is needed
		to STOP when + is found, or generally when an operand of LOWER PRECEDENCE is found
		*/
	case TOKEN_MINUS: fei_compiler_emitbyte(OP_NEGATE); break;			
	default:
		return;		
	}
}






// for inserting where the unary operator should lie
// starts at current token and parses any expression at the given precedence level or higher
// for example, if fei_compiler_parseprec(PREC_COMPARISON) is called, it will parse unaries, terms, and factors
// ubt not or, and or assignment operators as they are lower. Basically parse anything that is ABOVE the given precedence
void fei_compiler_parseprec(Precedence precedence)
{

	/*	PREFIX FIRST
	look up for a prefix token, and the FIRSt token is ALWAYS going to be a prefix
	*/
	fei_compiler_advancenext();		// again, go next first then use previous type as the 'current' token
	// the way the compiler is designed is that it has to always have a prefix
	ParseFn prefixRule = fei_compiler_getrule(parser.previous.type)->prefix;

	if (prefixRule == NULL)
	{
		fei_compiler_raiseerror("Expect expression.");
		return;
	}

	//

	bool canAssign = precedence <= PREC_ASSIGNMENT;			// for assignment precedence	
	prefixRule(canAssign);			// call the prefix function, may consume a lot of tokens

	
	/* after prefix expression is done, look for infix expression
	IMPORTANT: infix only runs if given precedence is LOWER than the operator for the infix
	or more nicely if NEXT/INFIX PRECEDENCE IS HIGHER THAN PREC ASSIGNMENT(parameter above_
	*/

	
	while (precedence <= fei_compiler_getrule(parser.current.type)->precedence)
	{
		fei_compiler_advancenext();
		ParseFn infixRule = fei_compiler_getrule(parser.previous.type)->infix;
		
		infixRule(canAssign);
	}

	//consume(TOKEN_AND, "consume and failed");

	if (canAssign && fei_compiler_match(TOKEN_EQUAL))		// if = is not consumed as part of the expression, nothing will , hence an error
	{
		fei_compiler_raiseerror("Invalid Assignment target.");
	}
}

// get pointer to ParseRule struct according to type parameter
ParseRule* fei_compiler_getrule(TokenType type)
{
	return &rules[type];		
}

void fei_compiler_parseexpr()		// a single 'statement' or line
{
	fei_compiler_parseprec(PREC_ASSIGNMENT);	// as assignment is the 2nd lowest, parses evrything
}

void fei_compiler_parseblock()
{
	while (!fei_compiler_check(TOKEN_RIGHT_BRACE) && !fei_compiler_check(TOKEN_EOF))	// parse until EOF or right brace is 'peeked'
	{
		fei_compiler_parsedeclaration();		// compile rest of block, keeps on parsing until right brace or EOF is 'peeked'		
	}

	fei_compiler_consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");			
}


/* functions */
void fei_compiler_parsefuncdecl(FunctionType type)
{
	// create separate Compiler for each function
	Compiler compiler;
	fei_compiler_init(&compiler, type);		// set new compiler(function) as the current one
	fei_compiler_beginscope();

	// compile parameters
	fei_compiler_consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
	
	if (!fei_compiler_check(TOKEN_RIGHT_PAREN))		// if end ) has not been reached
	{
		do {
			current->function->arity++;		// add number of parameters
			if (current->function->arity > 255)
			{
				fei_compiler_raisehere("Cannot have more than 255 parameters.");
			}

			uint8_t paramConstant = fei_compiler_parsevarfromcurrent("Expect variable name.");		// get name
			fei_compiler_defvarindex(paramConstant);			// scope handled here already
		} while (fei_compiler_match(TOKEN_COMMA));
	}	

	fei_compiler_consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameter list.");

	// body
	fei_compiler_consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
	fei_compiler_parseblock();

	// create function object
	ObjFunction* function = fei_compiler_endcompiler();				// ends the current compiler
	// compilers are treated like a stack; if current one is ended, like above, return to the previous one

	// fei_compiler_emitbytes(OP_CONSTANT, fei_compiler_makeconst(OBJ_VAL(function)));
	fei_compiler_emitbytes(OP_CLOSURE, fei_compiler_makeconst(OBJ_VAL(function)));

	/*	by the time the compiler reaches the end of a function declaration,
	every variable reference hass been resolved as either local, upvalue or global.
	each upvalue may return a local var or another upvalue

	-> for each upvalue there are two single-byte operands
	-> if first byte is one, then it captures a local variable in the enclosing function
	-> if first byte is 0, it captures the function's upvalues
	*/
	
	for (int i = 0; i < function->upvalueCount; i++)
	{
		fei_compiler_emitbyte(compiler.upvalues[i].isLocal ? 1 : 0);
		fei_compiler_emitbyte(compiler.upvalues[i].index);				 // emit index
	}

}

// create method for class type
void fei_compiler_parsemethoddecl()
{
	fei_compiler_consume(TOKEN_IDENTIFIER, "Expect method name.");
	uint8_t constant = fei_compiler_makeidentconst(&parser.previous);	// get method name
	
	// method body
	FunctionType type = TYPE_METHOD;

	// if initializer 
	if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0)
	{
		type = TYPE_INITIALIZER;
	}

	fei_compiler_parsefuncdecl(type);				// process the function

	fei_compiler_emitbytes(OP_METHOD, constant);
}


void fei_compiler_parseclassdecl()
{
	fei_compiler_consume(TOKEN_IDENTIFIER, "Expect class name.");
	Token className = parser.previous;					// get class name
	uint8_t nameConstant = fei_compiler_makeidentconst(&parser.previous);		// add to constant table as a string, return its index
	fei_compiler_declvarfromcurrent();						// declare that name variable

	fei_compiler_emitbytes(OP_CLASS, nameConstant);			// takes opcode and takes the constant table index
	fei_compiler_defvarindex(nameConstant);			// add it to the global hasht; we must DEFINE AFTER DECLARE to use it

	// handle class enclosing for 'this'
	ClassCompiler classCompiler;
	classCompiler.name = parser.previous;
	classCompiler.hasSuperclass = false;
	classCompiler.enclosing = currentClass;
	currentClass = &classCompiler;			// set new class as current

	// class inheritance
	if (fei_compiler_match(TOKEN_FROM))
	{
		fei_compiler_consume(TOKEN_IDENTIFIER, "Expect parent class name.");
		fei_comprule_variable(false);			// get the class variable, looks up the parent class by name and push it to the stack
		
		// check that the class names must be different
		if (fei_compiler_identsequal(&className, &parser.previous))
		{
			fei_compiler_raiseerror("Cannot inherit class from itself");
		}
		
		/* super classes
		- create new lexical scope to ensure that if we declare two classes in the same scope, each has a different
		local slot to store the superclasses
		*/
		fei_compiler_beginscope();
		fei_compiler_addlocal(fei_compiler_makesyntoken("super"));
		fei_compiler_defvarindex(0);

		fei_compiler_declnamedvar(className, false);
		fei_compiler_emitbyte(OP_INHERIT);
		classCompiler.hasSuperclass = true;
	}


	fei_compiler_declnamedvar(className, false);			// helper function to geenrate code that LOADS a variable with a given name to te stack

	fei_compiler_consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
	while (!fei_compiler_check(TOKEN_RIGHT_BRACE) && !fei_compiler_check(TOKEN_EOF))
	{
		fei_compiler_parsemethoddecl();
	}

	fei_compiler_consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
	fei_compiler_emitbyte(OP_POP);			// no longer need the class, pop it

	// close local scope for superclass variable
	if (classCompiler.hasSuperclass)
	{
		fei_compiler_endscope();
	}

	currentClass = currentClass->enclosing;		// go back to enclosing/main() class
}


void fei_compiler_parseclassfuncdecl()
{
	uint8_t global = fei_compiler_parsevarfromcurrent("Expect function name.");
	fei_compiler_markinit();					// scoping
	fei_compiler_parsefuncdecl(TYPE_FUNCTION);	
	fei_compiler_defvarindex(global);
}

void fei_compiler_parsevardecl()
{
	uint8_t global = fei_compiler_parsevarfromcurrent("Expect variable name.");

	if (fei_compiler_match(TOKEN_EQUAL))
	{
		fei_compiler_parseexpr();
	}
	else
	{
		fei_compiler_emitbyte(OP_NULL);		// not initialized
	}
	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

	fei_compiler_defvarindex(global);			// create global variable here; if local, not added to table
}

void fei_compiler_parseexprstmt()
{
	fei_compiler_parseexpr();
	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
	fei_compiler_emitbyte(OP_POP);
}

// if method
void fei_compiler_parseifstmt()
{
//	fei_compiler_consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
	fei_compiler_parseexpr();													// compile the expression statment inside; fei_compiler_parseprec()
	// after compiling expression above conditon value will be left at the top of the stack
//	fei_compiler_consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	fei_compiler_consume(TOKEN_THEN, "Missing 'then' keyword after if expression.");

	// gives an operand on how much to offset the ip; how many bytes of code to skip
	// if falsey, simply adjusts the ip by that amount
	// offset to jump to next (potentially else or elf) statment
	// insert to opcode the then branch statment first, then get offset
	int thenJump = fei_compiler_emitjump(OP_JUMP_IF_FALSE);	/* this gets distance */


	fei_compiler_emitbyte(OP_POP);	// pop then

	/* use BACKPATCHING
	- emit jump first with a placeholder offset, and get how far to jump
	
	
	*/
	
	fei_compiler_parsestatement();

	// below jump wil SURELY jump; this is skipped if the first fei_compiler_emitjump is not false
	int elseJump = fei_compiler_emitjump(OP_JUMP);			// need to jump at least 'twice' with an else statement
												// if the original statement is  true, then skip the the else statement

		// if then statment is run; pop the expression inside () after if
	fei_compiler_patchjump(thenJump);	/* this actually jumps */

	fei_compiler_emitbyte(OP_POP);		// if else statment is run; pop the expression inside () after if
	if (fei_compiler_match(TOKEN_ELSE)) fei_compiler_parsestatement();

	if (fei_compiler_match(TOKEN_ELF))	// else if
	{
		// go to statement, then go back to IF
		fei_compiler_parseifstmt();
	}

	/* this actually jumps */
	// last jump that is executed IF FIRST STATEMENT IS TRUE
	fei_compiler_patchjump(elseJump);			// for the second jump
}

void fei_compiler_parseswitchstmt()
{
	// fei_compiler_consume(TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
	if (!fei_compiler_check(TOKEN_IDENTIFIER))	// check next token
	{
		fei_compiler_raisehere("Expect identifier after switch.");
	}

	// if no error, consume the identifier
	fei_compiler_parseexpr();
	fei_compiler_consume(TOKEN_LEFT_BRACE, "Expect '{' after switch identifier.");
	fei_compiler_consume(TOKEN_CASE, "Expect at least 1 case after switch declaration.");

	/* to store  opcode offsets */
	uint8_t casesCount = -1;
	uint8_t capacity = 0;
	int* casesOffset = ALLOCATE(int, 8);			// 8 initial switch cases

	do		// while next token is a case, match also advances
	{
		// grow array if needed
		if (capacity < casesCount + 1)
		{
			int oldCapacity = capacity;
			capacity = GROW_CAPACITY(oldCapacity);
			casesOffset = GROW_ARRAY(int, casesOffset, oldCapacity, capacity);
		}
		
		casesCount++; 

		fei_compiler_parseexpr();
		fei_compiler_consume(TOKEN_COLON, "Expect ':' after case expression.");
		fei_compiler_emitbyte(OP_SWITCH_EQUAL);			// check if both values are equal

		int caseFalseJump = fei_compiler_emitjump(OP_JUMP_IF_FALSE);					// jump if false
		//printf("\ncase false jump offset: %d", caseFalseJump);

		// parse the statment
		fei_compiler_parsestatement();

		fei_compiler_emitbyte(OP_POP);		// pop the 'true' from OP_SWITCH_EQUAL
		casesOffset[casesCount] = fei_compiler_emitjump(OP_JUMP);
		//printf("\ncase true jump offset: %d", casesOffset[casesCount]);
		
		// jump to end of case if false
		fei_compiler_patchjump(caseFalseJump);
		fei_compiler_emitbyte(OP_POP);		// pop the 'false' statment from OP_SWITCH_EQUAL
	} while (fei_compiler_match(TOKEN_CASE));

	if (fei_compiler_match(TOKEN_DEFAULT))
	{
		fei_compiler_consume(TOKEN_COLON, "Expect ':' default case.");
		fei_compiler_parsestatement();		// running the default statement
	}
	//fei_compiler_consume(TOKEN_DEFAULT, "Default case not provided for switch.");


	// fei_compiler_patchjump for each available jump
	for (uint8_t i = 0; i <= casesCount; i++)
	{
		fei_compiler_patchjump(casesOffset[i]);
	}
		
	fei_compiler_emitbyte(OP_POP);			// pop switch constant
	FREE_ARRAY(int, casesOffset, capacity);

	fei_compiler_consume(TOKEN_RIGHT_BRACE, "Expect '}' at the end of switch statement");
}


void fei_compiler_parseprintstmt()
{
	fei_compiler_parseexpr();			// this is the function that actually processes the experssion
	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after value.");		// try consume ;, if fails show message
	fei_compiler_emitbyte(OP_PRINT);
}

void fei_compiler_parsereturnstmt()
{
	if (current->type == TYPE_SCRIPT)
	{
		fei_compiler_raiseerror("Cannot return from top-level code.");
	}
	if (fei_compiler_match(TOKEN_SEMICOLON))
	{
		fei_compiler_emitreturn();
	}
	else
	{
		// error in returning from an initializer
		if (current->type == TYPE_INITIALIZER)
		{
			fei_compiler_raiseerror("Cannot return a value from an initializer");
		}

		fei_compiler_parseexpr();
		fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
		fei_compiler_emitbyte(OP_RETURN);
	}
}

void fei_compiler_parseforstmt()
{
	fei_compiler_beginscope();			// for possible variable declarations in clause

	fei_compiler_beginloopscope();

	fei_compiler_consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

	// initializer clause
	if (fei_compiler_match(TOKEN_SEMICOLON))
	{
		// no initializer
	}
	else if (fei_compiler_match(TOKEN_VAR))
	{
		fei_compiler_parsevardecl();			// for clause scope only
	}
	else
	{
		fei_compiler_parseexprstmt();
	}

	// for for/while loops, loop starts here, with currenChunk()->count
	int loopStart = fei_compiler_currentchunk()->count;

	//  the condition clause
	/* CONDITION CLAUSE
	1. If false, pop the recently calculated expression and skip the loop
	2. if true, go to the body; see increment clause below
	*/
	int exitJump = -1;
	if (!fei_compiler_match(TOKEN_SEMICOLON))
	{
		fei_compiler_parseexpr();
		fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

		// jump out of loop if condition is false
		exitJump = fei_compiler_emitjump(OP_JUMP_IF_FALSE);
		fei_compiler_emitbyte(OP_POP);				// still need to figure this out, most likely just deleting 'temporary' constants in the scope
	}

	// the increment clause
	if (!fei_compiler_match(TOKEN_RIGHT_PAREN))		// if there is something else before the terminating ')'
	{
		/*	INCEREMENT CLAUSE
		1. from the condition clause, first jump OVER the increment, to the body
		2. in the body, run the body
		3. jump BACK to the increment and run it
		4. from the increment jump BACK to the CONDITION clause, back to the cycle
		*/

		// for continue
		

		int bodyJump = fei_compiler_emitjump(OP_JUMP);		// jump the increment clause

		int incrementStart = fei_compiler_currentchunk()->count;		// starting index for increment

		// set continue jump here, right after the increment statement
		fei_compiler_markcontinuejump();

		fei_compiler_parseexpr();			// run the for expression
		fei_compiler_emitbyte(OP_POP);		// pop expression constant
		fei_compiler_consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

		// running the loop
		fei_compiler_emitloop(loopStart);		// goes back to the start of the CONDITION clause of the for loop
		loopStart = incrementStart;
		fei_compiler_patchjump(bodyJump);
	}

	fei_compiler_parsestatement();		// running the code inside the loop

	fei_compiler_emitloop(loopStart);

	// patch the jump in the loop body
	if (exitJump != -1)
	{
		fei_compiler_patchjump(exitJump);
		fei_compiler_emitbyte(OP_POP);		// only pop when THERE EXISTS A CONDITION from the clause
	}

	// patch break jumps, if available
	fei_compiler_patchbreakjumps();
	
	fei_compiler_endloopscope();
	fei_compiler_endscope();
}

void fei_compiler_parsewhilestmt()
{
	int loopStart = fei_compiler_currentchunk()->count;		// index where the statement to loop starts
	fei_compiler_beginloopscope();

	// set jump for potential continue statement
	fei_compiler_markcontinuejump();

	fei_compiler_parseexpr();


	int exitJump = fei_compiler_emitjump(OP_JUMP_IF_FALSE);			// skip stament if condition is false

	fei_compiler_emitbyte(OP_POP);			// pop the last expression(true or false)
	
	fei_compiler_parsestatement();

	fei_compiler_emitloop(loopStart);		// method to 'loop' the instruction

	fei_compiler_patchjump(exitJump);

	fei_compiler_emitbyte(OP_POP);

	// patch break jumps, if available
	fei_compiler_patchbreakjumps();

	fei_compiler_endloopscope();
}

void fei_compiler_parsebreakstmt()
{
	if (current->loopCountTop < 0)
	{
		fei_compiler_raiseerror("Break statement must be enclosed in a loop");
		return;
	}

	if (++current->breakJumpCounts[current->loopCountTop] > UINT8_COUNT)
	{
		fei_compiler_raiseerror("Too many break statments in one loop");
		return;
	}

	int breakJump = fei_compiler_emitjump(OP_JUMP);
	int loopDepth = current->loopCountTop;
	int breakAmount = current->breakJumpCounts[loopDepth];
	current->breakPatchJumps[current->loopCountTop][breakAmount - 1] = breakJump;

	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after break.");
}


void fei_compiler_parsecontinuestmt()
{
	if (current->loopCountTop < 0)
	{
		fei_compiler_raiseerror("Continue statement must be enclosed in a loop");
		return;
	}

	if (current->loopCountTop == current->continueJumpCapacity)
	{
		int oldCapacity = current->continueJumpCapacity;
		current->continueJumpCapacity = GROW_CAPACITY(oldCapacity);
		current->continueJumps = GROW_ARRAY(int, current->continueJumps, oldCapacity, current->continueJumpCapacity);
	}

	fei_compiler_emitloop(current->continueJumps[current->loopCountTop]);

	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after continue.");
}

void fei_compiler_parserepeatuntilstmt()
{
	// fei_compiler_consume(TOKEN_LEFT_BRACE, "Expect '{' after repeat.");
	int loopStart = fei_compiler_currentchunk()->count;
	fei_compiler_beginloopscope();
	fei_compiler_markcontinuejump();

	// process the statement
	fei_compiler_parsestatement();

	fei_compiler_consume(TOKEN_UNTIL, "Expect 'until' after repeat statement.");

	// get true or false
	fei_compiler_parseexpr();

	// emit loop if false op code
	fei_compiler_emitcondloop(loopStart, false);

	// patch possible break jumps
	fei_compiler_patchbreakjumps();

	fei_compiler_endloopscope();
	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after until exppression.");
}

void fei_compiler_parsedowhilestmt()
{
	int loopStart = fei_compiler_currentchunk()->count;
	fei_compiler_beginloopscope();
	fei_compiler_markcontinuejump();

	// process the statement
	fei_compiler_parsestatement();

	fei_compiler_consume(TOKEN_WHILE, "Expect 'until' after repeat statement.");

	// get true or false
	fei_compiler_parseexpr();

	// emit loop if true op code
	fei_compiler_emitcondloop(loopStart, true);

	// patch possible break jumps
	fei_compiler_patchbreakjumps();

	fei_compiler_endloopscope();
	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after until exppression.");
}

void fei_compiler_synchronize()
{
	parser.panicMode = false;

	//printf("panic mode");

	// basically turn off the 'error' mode and skips token until something that looks like a statement boundary is found
	// skips tokens indiscriminately until somehing that looks like a statement boundary(eg. semicolon) is found
	while (parser.current.type != TOKEN_EOF)
	{
		if (parser.previous.type == TOKEN_SEMICOLON) return;

		switch (parser.current.type)
		{
		case TOKEN_CLASS:
		case TOKEN_FUN:
		case TOKEN_VAR:
		case TOKEN_FOR:
		case TOKEN_IF:
		case TOKEN_WHILE:
		case TOKEN_PRINT:
		case TOKEN_RETURN:
			return;
		default: // do nothing 
			;
		}

		fei_compiler_advancenext();
	}
}

void fei_compiler_parsedeclaration()
{
	if (fei_compiler_match(TOKEN_CLASS))
	{
		fei_compiler_parseclassdecl();
	}
	else if (fei_compiler_match(TOKEN_FUN))
	{
		fei_compiler_parseclassfuncdecl();
	}
	else if (fei_compiler_match(TOKEN_VAR))
	{
		fei_compiler_parsevardecl();		// declare variable
	}
	else
	{
		fei_compiler_parsestatement();
	}
	if (parser.panicMode) fei_compiler_synchronize();		// for errors
}

void fei_compiler_parsestatement()					// either an expression or a print
{

	if (fei_compiler_match(TOKEN_PRINT))			
	{
		fei_compiler_parseprintstmt();
	}
	else if (fei_compiler_match(TOKEN_RETURN))
	{
		fei_compiler_parsereturnstmt();			// for functions return
	}
	else if (fei_compiler_match(TOKEN_WHILE))
	{
		fei_compiler_parsewhilestmt();
	}
	else if (fei_compiler_match(TOKEN_FOR))
	{
		fei_compiler_parseforstmt();
	}
	else if (fei_compiler_match(TOKEN_SWITCH))
	{
		fei_compiler_parseswitchstmt();
	}
	else if (fei_compiler_match(TOKEN_BREAK))
	{
		fei_compiler_parsebreakstmt();
	}
	else if (fei_compiler_match(TOKEN_CONTINUE))
	{
		fei_compiler_parsecontinuestmt();
	}
	else if (fei_compiler_match(TOKEN_IF))
	{
		fei_compiler_parseifstmt();
	}
	else if (fei_compiler_match(TOKEN_REPEAT))
	{
		fei_compiler_parserepeatuntilstmt();
	}
	else if (fei_compiler_match(TOKEN_DO))
	{
		fei_compiler_parsedowhilestmt();
	}
	else if (fei_compiler_match(TOKEN_LEFT_BRACE))		// parse initial { token
	{
		fei_compiler_beginscope();
		fei_compiler_parseblock();
		fei_compiler_endscope();
	}
	else
	{
		fei_compiler_parseexprstmt();
	}
}

ObjFunction* fei_compiler_compilesource(const char* source)
{
	fei_lexer_initsource(source);			// start scan/lexing
	Compiler compiler;
	fei_compiler_init(&compiler, TYPE_SCRIPT);

	parser.hadError = false;
	parser.panicMode = false;

	fei_compiler_advancenext();						// call to advance once to 'pump' the scanner
	
	while (!fei_compiler_match(TOKEN_EOF))		/// while EOF token is not met
	{
		fei_compiler_parsedeclaration();
	}

	
	ObjFunction* function = fei_compiler_endcompiler();					// ends the expression with a return type
	return parser.hadError ? NULL : function;		// if no error return true
}


// marking compiler roots, for garbage collection
void fei_compiler_markroots()
{
	Compiler* compiler = current;
	while (compiler != NULL)
	{
		fei_gcmem_markobject((Obj*)compiler->function);
		compiler = compiler->enclosing;
	}
}



static Value cfn_clock(int argCount, Value* args)
{
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);		// returns elapsed time since program was running
}

static Value cfn_print(/*State* vm, */int argc, Value* args)
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

static Value cfn_println(/*State* vm, */int argc, Value* args)
{
    Value r;
    r = cfn_print(/*vm, */argc, args);
    fprintf(stdout, "\n");
    fflush(stdout);
    return r;
}

void fei_vm_resetstack()
{
	// point stackStop to the begininng of the empty array
	vm.stackTop = vm.stack;		// stack array(vm.stack) is already indirectly declared, hence no need to allocate memory for it
	vm.frameCount = 0;
	vm.openUpvalues = NULL;
}

// IMPORTANT
// variadic function ( ... ), takes a varying number of arguments
void fei_vm_raiseruntimeerror(const char* format, ...)
{
	
	va_list args;	// list from the varying parameter
	va_start(args, format);
	vprintf(format, args);		// unlike book, not vprintf(stderr, format, args)
	va_end(args);
	fputs("\n", stderr);	// fputs; write a string to the stream but not including the null character
	

	// printing the stack trace for the function
	// print out each function that was still executing when the program died and where the execution was at the point it died
	for (int i = vm.frameCount - 1; i >= 0; i--)
	{
		CallFrame* frame = &vm.frames[i];
		ObjFunction* function = frame->closure->function;
		// - 1 because IP is sitting on the NEXT INSTRUCTION to be executed
		size_t instruction = frame->ip - function->chunk.code - 1;
		fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
		if (function->name == NULL)
		{
			fprintf(stderr, "script\n");
		}
		else
		{
			fprintf(stderr, "%s(%d)\n", function->name->chars, function->arity);
		}

	}


	// tell which line the error occurred
	CallFrame* frame = &vm.frames[vm.frameCount - 1];		// pulls from topmost CallFrame on the stack
	size_t instruction = frame->ip - frame->closure->function->chunk.code - 1;	// - 1 to deal with the 1 added initially for the main() CallFrame
	int line = frame->closure->function->chunk.lines[instruction];
	fprintf(stderr, "Error in script at [Line %d]\n", line);

	fei_vm_resetstack();
}

void fei_vm_defnative(const char* name, NativeFn function)
{
	fei_vm_pushvalue(OBJ_VAL(fei_object_copystring(name, (int)strlen(name))));			// strlen to get char* length
	fei_vm_pushvalue(OBJ_VAL(fei_object_makenativefunc(function)));
	fei_table_set(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
	fei_vm_popvalue();
	fei_vm_popvalue();
}


void fei_vm_init()
{
	fei_vm_resetstack();			// initialiing the Value stack, also initializing the callframe count
	vm.objects = NULL;
	fei_table_init(&vm.globals);
	fei_table_init(&vm.strings);

	// initializing gray marked obj stack for garbage collection
	vm.grayCapacity = 0;
	vm.grayCount = 0;
	vm.grayStack = NULL;

	// self adjusting heap to control frequency of GC
	vm.bytesAllocated = 0;
	vm.nextGC = 1024 * 1024;


	// init initalizer string
	vm.initString = NULL;
	vm.initString = fei_object_copystring("init", 4);

	fei_vm_defnative("clock", cfn_clock);
	fei_vm_defnative("print", cfn_print);
	fei_vm_defnative("println", cfn_println);

}

void fei_vm_destroy()
{
	vm.initString = NULL;
	fei_gcmem_freeobjects();		// free all objects, from vm.objects
	fei_table_destroy(&vm.globals);
	fei_table_destroy(&vm.strings);
}

/* stack operations */
void fei_vm_pushvalue(Value value)
{
	*vm.stackTop = value;		// * in front of the pointer means the rvalue itself, assign value(parameter) to it
	vm.stackTop++;
}

Value fei_vm_popvalue()
{
	vm.stackTop--;		// first move the stack BACK to get the last element(stackTop points to ONE beyond the last element)
	return  *vm.stackTop;
}
/* end of stack operations */

// PEEK from the STACK, AFTER the compiler passes it through
// return a value from top of the stack but does not pop it, distance being how far down
// this is a C kind of accessing arrays/pointers
Value fei_vm_peekvalue(int distance)
{
	return vm.stackTop[-1 - distance];
}


/* for call stacks/functions  */
bool fei_vm_callclosure(ObjClosure* closure, int argCount)
{

	if (argCount != closure->function->arity)	// if number of parameters does not match
	{
		fei_vm_raiseruntimeerror("Expected %d arguments but got %d", closure->function->arity, argCount);
		return false;
	}

	// as CallFrame is an array, to ensure array does not overflow
	if (vm.frameCount == FRAMES_MAX)
	{
	fei_vm_raiseruntimeerror("Stack overflow.");
	return false;
	}

	// get pointer to next in frame array
	CallFrame* frame = &vm.frames[vm.frameCount++];			// initializes callframe to the top of the stack
	frame->closure = closure;
	frame->ip = closure->function->chunk.code;

	// set up slots pointer to give frame its window into the stack
	// ensures everyting lines up
	// slots is the 'starting pointer' for the function cll
	frame->slots = vm.stackTop - argCount - 1;
	return true;
}

bool fei_vm_callvalue(Value callee, int argCount)
{
	if (IS_OBJ(callee))
	{
		switch (OBJ_TYPE(callee))
		{
		case OBJ_BOUND_METHOD:
		{
			ObjBoundMethod* bound = AS_BOUND_METHOD(callee);		// get ObjBoundMethod from value type(callee)
			vm.stackTop[-argCount - 1] = bound->receiver;		// set [-] inside square brackes of top stack pointer to go down the stack
			return fei_vm_callclosure(bound->method, argCount);			//	run call to execute
		}
		case OBJ_CLASS:		// create class instance
		{
			ObjClass* kelas = AS_CLASS(callee);
			// create new instance here
			vm.stackTop[-argCount - 1] = OBJ_VAL(fei_object_makeinstance(kelas));		// - argcounts as above values are parameters

			// initializer
			Value initializer;
			// if we find one from the table
			if (fei_table_get(&kelas->methods, vm.initString, &initializer))			// have a vm.initString as 'token', ObjString type	
			{
				return fei_vm_callclosure(AS_CLOSURE(initializer), argCount);
			}
			else if (argCount != 0)   // if there ARE arguments but the initalizer method cannot be found
			{
				fei_vm_raiseruntimeerror("Expected 0  arguments but got %d\n", argCount);
				return false;
			}

			return true;
		}
		case OBJ_CLOSURE:				// ensure type is function
			return fei_vm_callclosure(AS_CLOSURE(callee), argCount);		// call to function happens here

		case OBJ_NATIVE:
		{
			NativeFn native = AS_NATIVE(callee);
			Value result = native(argCount, vm.stackTop - argCount);
			vm.stackTop -= argCount + 1;				// remove call and arguments from the stack
			fei_vm_pushvalue(result);
			return true;
		}
		default:
			break;
		}
	}

	fei_vm_raiseruntimeerror("Non-function or non-class type is called.");
	return false;
}


bool fei_class_invokemethod(ObjClass* kelas, ObjString* name, int argCount)
{
	Value method;
	if (!fei_table_get(&kelas->methods, name, &method))
	{
		fei_vm_raiseruntimeerror("Undefined property '%s'.", name->chars);
		return false;
	}

	return fei_vm_callclosure(AS_CLOSURE(method), argCount);
}




// invoke class method, access method + call method
bool fei_vm_stackinvoke(ObjString* name, int argCount)
{
	Value receiver = fei_vm_peekvalue(argCount);		// grab the receiver of the stack

	// call method with wrong type, not an objinstance type
	if (!IS_INSTANCE(receiver))
	{
		fei_vm_raiseruntimeerror("Tried to invoke a method from a non instance object.");
		return false;
	}

	ObjInstance* instance = AS_INSTANCE(receiver);

	// for fields()
	Value value;
	if (fei_table_get(&instance->fields, name, &value))
	{
		vm.stackTop[-argCount - 1] = value;
		return fei_vm_callvalue(value, argCount);
	}



	return fei_class_invokemethod(instance->kelas, name, argCount);		// actual function that searches for method and calls it
}



// bind method and wrap it in a new ObjBoundMethod
bool fei_class_bindmethod(ObjClass* kelas, ObjString* name)
{
	Value method;			
	if (!fei_table_get(&kelas->methods, name, &method))			// get method from table and bind it
	{
		// if method not found
		fei_vm_raiseruntimeerror("Undefined property %s.", name->chars);
		return false;
	}
	ObjBoundMethod* bound = fei_object_makeboundmethod(fei_vm_peekvalue(0), AS_CLOSURE(method));		// wrap method in a new ObjBoundMethodd

	fei_vm_popvalue();		// pop the class instance
	fei_vm_pushvalue(OBJ_VAL(bound));
	return true;
}


// get corresponding upvalue 
ObjUpvalue* fei_vm_captureupvalue(Value* local)
{
	// set up the linked list
	ObjUpvalue* prevUpvalue = NULL;
	ObjUpvalue* upvalue = vm.openUpvalues;		// assign at the start of the list

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
	while (upvalue != NULL && upvalue->location > local)	// pointer comparison: only find the ones ABOVE local
	{
		prevUpvalue = upvalue;
		upvalue = upvalue->next;
	}

	if (upvalue != NULL && upvalue->location == local)		// if the location/local/indeces match
	{
		return upvalue;				// return already created upvalue
	}

	ObjUpvalue* createdUpvalue = fei_object_makeupvalue(local);
	createdUpvalue->next = upvalue;			// insert at the front
	
	if (prevUpvalue == NULL)	// ran out of values to search
	{
		vm.openUpvalues = createdUpvalue;			// set pointer to the newly added upvalue
	}
	else			// found local slot BELOW the one we are looking for
	{
		prevUpvalue->next = createdUpvalue;				// link next slot(the value below) to the newly inserted upvalue
	}
	
	return createdUpvalue;
}

// closes every upvalue it can find that points to the slot or any above the stack
void fei_vm_closeupvalues(Value* last)			// takes pointer to stack slot
{
	while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last)
	{
		ObjUpvalue* upvalue = vm.openUpvalues;	// pointer to list of openupvalues
		upvalue->closed = *upvalue->location;
		upvalue->location = &upvalue->closed;
		vm.openUpvalues = upvalue->next;
	}
}

// defining method for class type
void fei_vm_stackdefmethod(ObjString* name)
{
	Value method = fei_vm_peekvalue(0);				// method/closure is at the top of the stack
	ObjClass* kelas = AS_CLASS(fei_vm_peekvalue(1));	// class is at the 2nd top
	fei_table_set(&kelas->methods, name, method);	// add to hashtable
	fei_vm_popvalue();				// pop the method
}



// comparison for OP_NOT
bool fei_value_isfalsey(Value value)
{
	// return true if value is the null type or if it is a false bool type
	bool test = IS_NULL(value) || (IS_BOOL(value) && !AS_BOOL(value));

	return test;
}

// string concatenation
void fei_vmdo_strconcat()
{
	ObjString* second = AS_STRING(fei_vm_peekvalue(0));				// peek, so we do not pop it off if calling a GC is needed
	ObjString* first = AS_STRING(fei_vm_peekvalue(1));
	
	int length = first->length + second->length;
	char* chars = ALLOCATE(char, length + 1);		// dynamically allocate memory for the char, chars is now a NULL string

	/* NOTE ON C STRINGS, NULL VS EMPTY
	-> null string has no elements, it is an empty charray, ONLY DECLARED
	-> an empty string has the null character '/0'
	*/

	// IMPORTANt -> use memcpy when assinging to a char* pointer
	memcpy(chars, first->chars, first->length);		// memcpy function, copy to chars, from first->chars, with second->length number of bits
	memcpy(chars + first->length, second->chars, second->length);		// remember to add the first length of bits to chars again, so it will START AFTER the given offset
	chars[length] = '\0';			// IMPORTANT-> terminating character for Cstring, if not put will get n2222

	ObjString* result = fei_object_takestring(chars, length);		// declare new ObjString ptr
	fei_vm_popvalue();			// pop the two strings, garbage collection
	fei_vm_popvalue();		
	fei_vm_pushvalue(OBJ_VAL(result));
}


/* starting point of the compiler */
InterpretResult fei_vm_evalsource(const char* source)
{
	ObjFunction* function = fei_compiler_compilesource(source);
	if (function == NULL) return INTERPRET_COMPILE_ERROR;		// NULL gets passed from compiler

	fei_vm_pushvalue(OBJ_VAL(function));
	ObjClosure* closure = fei_object_makeclosure(function);
	fei_vm_popvalue();
	fei_vm_pushvalue(OBJ_VAL(closure));
	fei_vm_callvalue(OBJ_VAL(closure), 0);			// 0 params for main()


	return fei_vm_exec();
}


// run the chunk
// most IMPORTANT part of the interpreter
InterpretResult fei_vm_exec()		// static means the scope of the function is only to this file
{
	CallFrame* frame = &vm.frames[vm.frameCount - 1];

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

#define READ_BYTE() (*frame->ip++)		
#define READ_CONSTANT()		\
	(frame->closure->function->chunk.constants.values[READ_BYTE()])	
#define READ_STRING() AS_STRING(READ_CONSTANT())

// for patch jumps
// yanks next two bytes from the chunk(used to calculate the offset earlier) and return a 16-bit integer out of it
// use bitwise OR
#define READ_SHORT()	\
	(frame->ip += 2, \
	(uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

// MACRO for binary operations
// take two last constants, and push ONE final value doing the operations on both of them
// this macro needs to expand to a series of statements, read a-virtual-machine for more info, this is a macro trick or a SCOPE BLOCK
// pass in an OPERAOTR as a MACRO
// valueType is a Value struct
// first check that both operands are numbers
#define BINARY_OP(valueType, op, downcastType)	\
	do {	\
		if (!IS_NUMBER(fei_vm_peekvalue(0)) || !IS_NUMBER(fei_vm_peekvalue(1)))	\
		{	\
			fei_vm_raiseruntimeerror("Operands must be numbers.");	\
			return INTERPRET_RUNTIME_ERROR;		\
		}	\
		downcastType b = (downcastType)AS_NUMBER(fei_vm_popvalue());	\
		downcastType a = (downcastType)AS_NUMBER(fei_vm_popvalue());	\
		fei_vm_pushvalue(valueType(a op b));	\
	} while(false)	\

	for (;;)
	{
	// fei_dbgdisas_instr needs an byte offset, do pointer math to convert ip back to relative offset
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
		for (Value* slot = vm.stack; slot < vm.stackTop; slot++)
		{
			printf("[ ");
			fei_value_printvalue(*slot);
			printf(" ]");
		}

		
		fei_dbgdisas_instr(&frame->closure->function->chunk, 
			(int)(frame->ip - frame->closure->function->chunk.code));
#endif
		uint8_t instruction;
		switch (instruction = READ_BYTE())			// get result of the byte read, every set of instruction starts with an opcode
		{
			case OP_CONSTANT: 
			{
				// function is smart; chunk advances by 1 on first read, then in the READ_CONSTANT() macro it reads again which advances by 1 and returns the INDEX
				Value constant = READ_CONSTANT();		// READ the next line, which is the INDEX of the constant in the constants array
				fei_vm_pushvalue(constant);		// push to stack
				break;			// break from the switch
			}
			// unary opcode
			case OP_NEGATE: 
				if (!IS_NUMBER(fei_vm_peekvalue(0)))		// if next value is not a number
				{
					//printf("\nnot a number\n"); it actually works
					fei_vm_raiseruntimeerror("Operand must be a number.");
					return INTERPRET_RUNTIME_ERROR;
				}
				
				fei_vm_pushvalue(NUMBER_VAL(-AS_NUMBER(fei_vm_popvalue()))); 
				break;  // negates the last element of the stack
			
			// literals
			case OP_NULL: fei_vm_pushvalue(NULL_VAL); break;
			case OP_TRUE: fei_vm_pushvalue(BOOL_VAL(true)); break;
			case OP_FALSE: fei_vm_pushvalue(BOOL_VAL(false)); break;

			// binary opcode
			case OP_ADD: 
			{
				if (IS_STRING(fei_vm_peekvalue(0)) && IS_STRING(fei_vm_peekvalue(1)))	// if last two constants are strings
				{
					fei_vmdo_strconcat();
				}
				else if (IS_NUMBER(fei_vm_peekvalue(0)) && IS_NUMBER(fei_vm_peekvalue(1)))
				{
					// in the book, macro is not used and a new algorithm is used directly
					BINARY_OP(NUMBER_VAL, +, double); 		// initialize new Value struct (NUMBER_VAL) here
				}
				else		// handle errors dynamically here
				{
					//printf("operands error");
					fei_vm_raiseruntimeerror("Operands are incompatible.");
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}
			
			case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -, double); break;
			case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *, double); break;
			case OP_DIVIDE: BINARY_OP(NUMBER_VAL, /, double); break;

			case OP_MODULO: BINARY_OP(NUMBER_VAL, %, int); break;

			case OP_NOT:
				fei_vm_pushvalue(BOOL_VAL(fei_value_isfalsey(fei_vm_popvalue())));		// again, pops most recent one from the stack, does the operation on it, and pushes it back
				break;

			// for switch eqal
			case OP_SWITCH_EQUAL:
			{
				Value b = fei_vm_popvalue();		// only pop second value
				Value a = fei_vm_peekvalue(0);		// peek topmost, the first value
				fei_vm_pushvalue(BOOL_VAL(fei_value_compare(a, b)));
				break;
			}

			case OP_EQUAL:		// implemenation comparison done here
			{
				Value b = fei_vm_popvalue();
				Value a = fei_vm_popvalue();
				fei_vm_pushvalue(BOOL_VAL(fei_value_compare(a, b)));
				break;
			}
			case OP_GREATER: BINARY_OP(BOOL_VAL, > , double); break;
			case OP_LESS: BINARY_OP(BOOL_VAL, < , double); break;


			case OP_PRINT:
			{
				// ACTUAL PRINTING IS DONE HERE
				fei_value_printvalue(fei_vm_popvalue());		// pop the stack and print the value, getting it from value.c
				printf("\n");
				break;
			}

			case OP_POP: fei_vm_popvalue(); break;

			case OP_GET_LOCAL:
			{
				uint8_t slot = READ_BYTE();
				fei_vm_pushvalue(frame->slots[slot]);			// pushes the value to the stack where later instructions can read it
				break;
			}

			case OP_SET_LOCAL:
			{
				uint8_t slot = READ_BYTE();
				// all the local var's VARIABLES are stored inside vm.stack
				frame->slots[slot] = fei_vm_peekvalue(0);		// takes from top of the stack and stores it in the stack slot
				break;
			}

			case OP_DEFINE_GLOBAL:
			{	
				ObjString* name = READ_STRING();		// get name from constant table
				fei_table_set(&vm.globals, name, fei_vm_peekvalue(0));	// take value from the top of the stack
				fei_vm_popvalue();
				break;
			}

			case OP_GET_GLOBAL:
			{
				ObjString* name = READ_STRING();	// get the name
				Value value;		// create new Value
				if (!fei_table_get(&vm.globals, name, &value))	// if key not in hash table
				{
					fei_vm_raiseruntimeerror("Undefined variable '%s'.", name->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				fei_vm_pushvalue(value);
				break;
			}

			case OP_SET_GLOBAL:
			{
				ObjString* name = READ_STRING();
				if (fei_table_set(&vm.globals, name, fei_vm_peekvalue(0)))	// if key not in hash table
				{
					fei_table_delete(&vm.globals, name);		// delete the false name 
					fei_vm_raiseruntimeerror("Undefined variable '%s'.", name->chars);
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}

			// upvalues set/get
			case OP_GET_UPVALUE:
			{
				uint8_t slot = READ_BYTE();		// read index
				fei_vm_pushvalue(*frame->closure->upvalues[slot]->location);		// push the value to the stack
				break;
			}

			case OP_SET_UPVALUE:
			{
				uint8_t slot = READ_BYTE();		// read index
				*frame->closure->upvalues[slot]->location = fei_vm_peekvalue(0);		// set to the topmost stack
				break;
			}
			
			case OP_GET_PROPERTY:
			{
				// to make sure only instances are allowed to have fields
				if (!IS_INSTANCE(fei_vm_peekvalue(0)))
				{
					fei_vm_raiseruntimeerror("Only instances have properties.");
					return INTERPRET_RUNTIME_ERROR;
				}

				ObjInstance* instance = AS_INSTANCE(fei_vm_peekvalue(0));		// get instance from top most stack
				ObjString* name = READ_STRING();					// get identifier name

				Value value;		// set up value to add to the stack
				if (fei_table_get(&instance->fields, name, &value))		// get from fields hash table, assign it to instance
				{
					fei_vm_popvalue();		// pop the instance itself
					fei_vm_pushvalue(value);
					break;
				}
				
				if (!fei_class_bindmethod(instance->kelas, name))		// no method as well, error
				{
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}

			case OP_SET_PROPERTY:
			{
				if (!IS_INSTANCE(fei_vm_peekvalue(1)))		// if not an instance
				{
					fei_vm_raiseruntimeerror("Identifier must be a class instance.");
					return INTERPRET_RUNTIME_ERROR;
				}

				// not top most, as the top most is reserved for the new value to be set
				ObjInstance* instance = AS_INSTANCE(fei_vm_peekvalue(1));		
				fei_table_set(&instance->fields, READ_STRING(), fei_vm_peekvalue(0));		//peek(0) is the new value

				Value value = fei_vm_popvalue();		// pop the already set value
				fei_vm_popvalue();		// pop the property instance itself
				fei_vm_pushvalue(value);		// push the value back again
				break;	
			}



			case OP_CLOSE_UPVALUE:
			{
				fei_vm_closeupvalues(vm.stackTop - 1);		// put address to the slot
				fei_vm_popvalue();			// pop from the stack
				break;
			}


			case OP_JUMP:		// will always jump
			{
				uint16_t offset = READ_SHORT();
				frame->ip += offset;
				break;
			}

			case OP_JUMP_IF_FALSE:		// for initial if, will not jump if expression inside is true
			{
				uint16_t offset = READ_SHORT();				// offset already put in the stack
				// actual jump instruction is done here; skip over the instruction pointer
				if (fei_value_isfalsey(fei_vm_peekvalue(0))) frame->ip += offset;		// if evaluated expression inside if statement is false jump
				break;
			}

			case OP_LOOP:
			{
				uint16_t offset = READ_SHORT();
				frame->ip -= offset;		// jumps back
				break;
			}

			case OP_LOOP_IF_FALSE:
			{
				uint16_t offset = READ_SHORT();				// offset already put in the stack
				// bool state is at the top of the stack
				// if false loop back
				if (fei_value_isfalsey(fei_vm_peekvalue(0))) frame->ip -= offset;
				fei_vm_popvalue();			// pop the true/false
				break;
			}

			case OP_LOOP_IF_TRUE:
			{
				uint16_t offset = READ_SHORT();				// offset already put in the stack
				// bool state is at the top of the stack
				// if not false loop back
				if (!fei_value_isfalsey(fei_vm_peekvalue(0))) frame->ip -= offset;
				fei_vm_popvalue();			// pop the true/false
				break;
			}

			// a callstack to a funcion has the form of function name, param1, param2...
			// the top level code, or caller, also has the same function name, param1, param2... in the right order
			case OP_CALL:
			{
				int argCount = READ_BYTE();
				if (!fei_vm_callvalue(fei_vm_peekvalue(argCount), argCount))	// call function; pass in the function name istelf[peek(depth)] and the number of arguments
				{
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm.frames[vm.frameCount - 1];			// to update pointer if callFrame is successful, asnew frame is added
				break;
			}

			// closures
			case OP_CLOSURE:
			{
				ObjFunction* function = AS_FUNCTION(READ_CONSTANT());		// load compiled function from table
				ObjClosure* closure = fei_object_makeclosure(function);
				fei_vm_pushvalue(OBJ_VAL(closure));

				// fill upvalue array over in the interpreter when a closure is created
				// to see upvalues in each slot
				for (int i = 0; i < closure->upvalueCount; i++)
				{
					uint8_t isLocal = READ_BYTE();		// read isLocal bool
					uint8_t index = READ_BYTE();		// read index for local, if available, in the closure
					if (isLocal)
					{
						closure->upvalues[i] = fei_vm_captureupvalue(frame->slots + index);		// get from slots stack

					}
					else				// if not local(nested upvalue)
					{
						closure->upvalues[i] = frame->closure->upvalues[index];				// get from current upvalue
					}
				}

				break;
			}

			case OP_CLASS:
				fei_vm_pushvalue(OBJ_VAL(fei_object_makeclass(READ_STRING())));			// load string for the class' name and push it onto the stack
				break;

			case OP_METHOD:
				fei_vm_stackdefmethod(READ_STRING());		// get name of the method
				break;

			case OP_INVOKE:
			{
				ObjString* method = READ_STRING();
				int argCount = READ_BYTE();
				if (!fei_vm_stackinvoke(method, argCount))		// new invoke function
				{
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm.frames[vm.frameCount - 1];
				break;
			}

			case OP_INHERIT:
			{
				Value parent = fei_vm_peekvalue(1);		// parent class from 2nd top of the stack

				// ensure that parent identifier is a class
				if (!IS_CLASS(parent))
				{
					fei_vm_raiseruntimeerror("Parent identifier is not a class.");
					return INTERPRET_RUNTIME_ERROR;
				}

				ObjClass* child = AS_CLASS(fei_vm_peekvalue(0));		// child class at the top of the stack
				fei_table_mergefrom(&AS_CLASS(parent)->methods, &child->methods);	// add all methods from parent to child table
				fei_vm_popvalue();				// pop the child class
				break;
			}

			case OP_GET_SUPER:
			{
				ObjString* name = READ_STRING();		// get method name/identifier
				ObjClass* parent = AS_CLASS(fei_vm_popvalue());		// class identifier is at the top of the stack
				if (!fei_class_bindmethod(parent, name))			// if binding fails
				{
					return INTERPRET_RUNTIME_ERROR;
				}
				break;
			}

			case OP_SUPER_INVOKE:		// super calls optimization
			{
				ObjString* method = READ_STRING();
				int count = READ_BYTE();
				ObjClass* parent = AS_CLASS(fei_vm_popvalue());
				if (!fei_class_invokemethod(parent, method, count))
				{
					return INTERPRET_RUNTIME_ERROR;
				}
				frame = &vm.frames[vm.frameCount - 1];
				break;
			}

			case OP_RETURN:				
			{
				Value result = fei_vm_popvalue();	// if function returns a value, value will beon top of the stack

				fei_vm_closeupvalues(frame->slots);   // close lingering closed values

				vm.frameCount--;
				if (vm.frameCount == 0)		// return from 'main()'/script function
				{
					fei_vm_popvalue();						// pop main script function from the stack
					return INTERPRET_OK;
				}

				// for a function
				// discard all the slots the callee was using for its parameters
				vm.stackTop = frame->slots;		// basically 're-assign'
				fei_vm_pushvalue(result);		// push the return value

				frame = &vm.frames[vm.frameCount - 1];		// update run function's current frame
				break;
			}
		}
	}


#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
#undef BINARY_OP
}

// for REPL, the print eval read loop 
void repl()
{
	char line[1024];		// char array to hold everything, with a length limit
	for (;;)
	{
		printf(">> ");

		/* info on fgets
		- fgets is like cin or getline cin 
		- basically get a line everytime
		- parameters: fgets(char array, char size, filestream). In this case the file stream is stdin, the current keyboard or from the standard input
		- char array is a pointer to where the string will be copied
		- use if so that if line overloads, we go to the next line
		*/
		if (!fgets(line, sizeof(line), stdin))
		{
			printf("\n");
			break;
		}

		fei_vm_evalsource(line);
	}
}

// get raw source code from file
char* readFile(const char* path)
{
	/*	Reading files in C
	FILE* file = fopen(const char *file_name, const char *mode_of_operation
	r(read) = searches file, and sets up a pointer to the first character. If not found returns null
	w(write)
	a(read, set to last pointer)
	rb(special read to open non-text files, a binary file)
	*/
	FILE* file = fopen(path, "rb");
	if (file == NULL)			// if file does not exist or user does not have access
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
	size_t fileSize = ftell(file);		// ftell is used to find position of file pointer, used to denote size
	// top two lines used to get file size
	rewind(file);			// sets file pointer to the beginning of the file

	char* buffer = (char*)malloc(fileSize + 1);					// allocate a char*(string) to the size of the file
	if (buffer == NULL)
	{
		fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
		exit(74);
	}

	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);			// read the file
	/* notes on fread in C
	size_t fread(void * buffer, size_t size, size_t count, FILE * stream)
	buffer =  a pointer to the block of memoery with a least (size*count) byte
	size = size of the result type
	count = number of elements/ file size
	*/

	if (bytesRead < fileSize)			// if read size less than file size
	{
		fprintf(stderr, "Could not read \"%s\".\n", path);
		exit(74);
	}

	buffer[bytesRead] = '\0';		// mark the last character as '\0', the end of file symbol

	fclose(file);
	return buffer;
}

// function for loading scripts
void runFile(const char* path)
{
	char* source = readFile(path);						// get raw source code from the file
	InterpretResult result = fei_vm_evalsource(source);			// get enum type result from VM
	free(source);	// free the source code

	if (result == INTERPRET_COMPILE_ERROR) exit(51);
	if (result == INTERPRET_RUNTIME_ERROR) exit(61);
}


int main(int argc, const char* argv[])		// used in the command line, argc being the amount of arguments and argv the array
{
	fei_vm_init();
	// the FIRST argument will always be the name of the executable being run(e.g node, python in terminal)

	if (argc == 1)		// if number of argument is one, run the repl 
	{
		repl();
	}
	else if (argc == 2)	// if number of arguments is two, the second one being the file, run the second file
	{
		runFile(argv[1]);
	}
	else
	{
		fprintf(stderr, "Usage: cfei [path]\n");	// fprintf; print on file but not on console, first argument being the file pointer
													// in this case it prints STANDARD ERROR
		exit(64);
	}

	fei_vm_destroy();
	return 0;
}

