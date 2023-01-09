#include <stdio.h>
#include <stdlib.h>			// to display errors
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "memory.h"			// for switch statements and marking the roots

/*	A compiler has two jobs really:
	- it parses the user's source code
	- it takes knowledge and outputs low-level instructions that produce the same semantics

	this is a SINGLE_PASS COMPILER -> both processes above are 'threaded' together, unlike an AST
IMPORTANT:
	->this compiler COMPILES an EXPRESSION only, not the whole code, hence it is more of an INTERPRETER
*/


#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

// to store current and previous tokens
typedef struct
{
	Token current;
	Token previous;
	bool hadError;		// flag to tell whether the code has a syntax error or no
	bool panicMode;		// flag for error cascades/multiple errors so the parser does not get confused, only returns the first
} Parser;

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

typedef struct
{
	Token name;
	int depth;			// depth of the variable, corresponding to scoreDepth in the struct below
	bool isCaptured;	// track whether the local is captured by a closure or no
} Local;


typedef struct
{
	bool isLocal;
	int index;			// matches the index of the local variable in ObjClosure
} Upvalue;	

typedef enum
{
	TYPE_FUNCTION,
	TYPE_SCRIPT,		// top level main()
	TYPE_INITIALIZER,		// class constructors
	TYPE_METHOD,		// class methods
} FunctionType;

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

Parser parser;

ClassCompiler* currentClass = NULL;	

Compiler* current = NULL;

/* compiler.c */
ObjFunction *fei_compiler_compilesource(const char *source);
void fei_compiler_markroots(void);

static Chunk *fei_compiler_currentchunk(void);
static void fei_compiler_raiseat(Token *token, const char *message);
static void fei_compiler_raiseerror(const char *message);
static void fei_compiler_raisehere(const char *message);
static void fei_compiler_advancenext(void);
static void fei_compiler_advanceskipping(TokenType type);
static void fei_compiler_consume(TokenType type, const char *message);
static bool fei_compiler_check(TokenType type);
static bool fei_compiler_match(TokenType type);
static void fei_compiler_emitbyte(uint8_t byte);
static void fei_compiler_emitbytes(uint8_t byte1, uint8_t byte2);
static void fei_compiler_emitloop(int loopStart);
static void fei_compiler_emitcondloop(int loopStart, bool state);
static int fei_compiler_emitjump(uint8_t instruction);
static void fei_compiler_emitreturn(void);
static uint8_t fei_compiler_makeconst(Value value);
static void fei_compiler_emitconst(Value value);
static void fei_compiler_patchjump(int offset);
static void fei_compiler_init(Compiler *compiler, FunctionType type);
static ObjFunction *fei_compiler_endcompiler(void);
static void fei_compiler_beginscope(void);
static void fei_compiler_endscope(void);
static void fei_compiler_beginloopscope(void);
static void fei_compiler_endloopscope(void);
static void fei_compiler_markcontinuejump(void);
static void fei_compiler_patchbreakjumps(void);
static uint8_t fei_compiler_makeidentconst(Token *name);
static bool fei_compiler_identsequal(Token *a, Token *b);
static int fei_compiler_resolvelocal(Compiler *compiler, Token *name);
static int fei_compiler_addupvalue(Compiler *compiler, uint8_t index, bool isLocal);
static int fei_compiler_resolveupvalue(Compiler *compiler, Token *name);
static void fei_compiler_addlocal(Token name);
static void fei_compiler_declvarfromcurrent(void);
static uint8_t fei_compiler_parsevarfromcurrent(const char *errorMessage);
static void fei_compiler_markinit(void);
static void fei_compiler_defvarindex(uint8_t global);
static uint8_t fei_compiler_parsearglist(void);
static void fei_comprule_logicaland(bool canAssign);
static void fei_comprule_binary(bool canAssign);
static void fei_comprule_call(bool canAssign);
static void fei_comprule_dot(bool canAssign);
static void fei_comprule_literal(bool canAssign);
static void fei_comprule_grouping(bool canAssign);
static void fei_comprule_number(bool canAssign);
static void fei_comprule_logicalor(bool canAssign);
static void fei_comprule_string(bool canAssign);
static void fei_compiler_declnamedvar(Token name, bool canAssign);
static void fei_comprule_variable(bool canAssign);
static Token fei_compiler_makesyntoken(const char *text);
static void fei_comprule_super(bool canAssign);
static void fei_comprule_this(bool canAssign);
static void fei_comprule_unary(bool canAssign);
static void fei_compiler_parseprec(Precedence precedence);
static ParseRule *fei_compiler_getrule(TokenType type);
static void fei_compiler_parseexpr(void);
static void fei_compiler_parseblock(void);
static void fei_compiler_parsefuncdecl(FunctionType type);
static void fei_compiler_parsemethoddecl(void);
static void fei_compiler_parseclassdecl(void);
static void fei_compiler_parseclassfuncdecl(void);
static void fei_compiler_parsevardecl(void);
static void fei_compiler_parseexprstmt(void);
static void fei_compiler_parseifstmt(void);
static void fei_compiler_parseswitchstmt(void);
static void fei_compiler_parseprintstmt(void);
static void fei_compiler_parsereturnstmt(void);
static void fei_compiler_parseforstmt(void);
static void fei_compiler_parsewhilestmt(void);
static void fei_compiler_parsebreakstmt(void);
static void fei_compiler_parsecontinuestmt(void);
static void fei_compiler_parserepeatuntilstmt(void);
static void fei_compiler_parsedowhilestmt(void);
static void fei_compiler_synchronize(void);
static void fei_compiler_parsedeclaration(void);
static void fei_compiler_parsestatement(void);



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


static Chunk* fei_compiler_currentchunk()
{
	return &current->function->chunk;
}

// to handle syntax errors
static void fei_compiler_raiseat(Token* token, const char* message)
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
static void fei_compiler_raiseerror(const char* message)
{
	fei_compiler_raiseat(&parser.previous, message);
}


// handling error from token, the most current one being handed, not yet consumed
static void fei_compiler_raisehere(const char* message)			// manually provide the message
{
	fei_compiler_raiseat(&parser.current, message);				// pass in the current parser
}


/* main compile functions */

// pump the compiler, basically go to / 'read' the next token, a SINGLE token
static void fei_compiler_advancenext()
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
static void fei_compiler_advanceskipping(TokenType type)
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
static void fei_compiler_consume(TokenType type, const char* message)
{
	if (parser.current.type == type)			// if current token is equal to the token type being compared to
	{
		fei_compiler_advancenext();
		return;
	}

	fei_compiler_raisehere(message);		// if consumes a different type, error
}

static bool fei_compiler_check(TokenType type)
{
	return parser.current.type == type;			// check if current matches given
}


static bool fei_compiler_match(TokenType type)
{
	if (!fei_compiler_check(type)) return false;
	fei_compiler_advancenext();
	return true;
}

/* emitting BYTECODE for the VM to understand */
// the writeChunk for the compiler
static void fei_compiler_emitbyte(uint8_t byte)
{
	writeChunk(fei_compiler_currentchunk(), byte, parser.previous.line);		// sends previous line so runtime errors are associated with that line
}

// write chunk for multiple chunks, used to write an opcode followed by an operand(eg. in constants)
static void fei_compiler_emitbytes(uint8_t byte1, uint8_t byte2)
{
	fei_compiler_emitbyte(byte1);
	fei_compiler_emitbyte(byte2);
}

// for looping statements
static void fei_compiler_emitloop(int loopStart)
{
	fei_compiler_emitbyte(OP_LOOP);

	// int below jumps back, + 2 accounting the OP_LOOP and the instruction's own operand
	int offset = fei_compiler_currentchunk()->count - loopStart + 2;			
	if (offset > UINT16_MAX) fei_compiler_raiseerror("Loop body too large.");

	fei_compiler_emitbyte((offset >> 8) & 0xff);
	fei_compiler_emitbyte(offset & 0xff);
}

static void fei_compiler_emitcondloop(int loopStart, bool state)
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



static int fei_compiler_emitjump(uint8_t instruction)
{
	/* backpatching */
	fei_compiler_emitbyte(instruction);	// writes a placeholder operand for jump offset
	fei_compiler_emitbyte(0xff);			// hexadecimal number with value of 255
	fei_compiler_emitbyte(0xff);

	// basically, get the difference in bytes before the two 0xff is added
	return fei_compiler_currentchunk()->count - 2;
}

//  emit specific return type
static void fei_compiler_emitreturn()
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
static uint8_t fei_compiler_makeconst(Value value)
{
	int constant = addConstant(fei_compiler_currentchunk(), value);
	if (constant > UINT8_MAX)
	{
		fei_compiler_raiseerror("Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)constant;		// return as byte, the byte being the INDEX of the constantin the constats array
}

static void fei_compiler_emitconst(Value value)		// for constant emit the opcode, then the index
{
	fei_compiler_emitbytes(OP_CONSTANT, fei_compiler_makeconst(value));	// add value to constant table
}

static void fei_compiler_patchjump(int offset)
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
static void fei_compiler_init(Compiler* compiler, FunctionType type)
{
	compiler->enclosing = current;			// the 'outer' compiler
	compiler->function = NULL;
	compiler->type = type;
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	compiler->function = newFunction();
	current = compiler;				// current is the global variable pointer for the Compiler struct, point to to the parameter
									// basically assign the global pointer 

	// for functions
	if (type != TYPE_SCRIPT)
	{
		current->function->name = copyString(parser.previous.start, parser.previous.length);		// function name handled here
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

static ObjFunction* fei_compiler_endcompiler()
{
	fei_compiler_emitreturn();
	ObjFunction* function = current->function;

	FREE(int, current->continueJumps);


	// for debugging
#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError)
	{
		disassembleChunk(fei_compiler_currentchunk(), function->name != NULL ? function->name->chars : "<script>");	// if name is NULL then it is the Script type(main()
	}
#endif

	current = current->enclosing;	// return back to enclosing compiler after function
	return function;			// return to free
}

static void fei_compiler_beginscope()
{
	current->scopeDepth++;
}

static void fei_compiler_endscope()
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
static void fei_compiler_beginloopscope()
{
	current->loopCountTop++;
}

static void fei_compiler_endloopscope()
{
	if (current->breakJumpCounts[current->loopCountTop] > 0)
		current->breakJumpCounts[current->loopCountTop] = 0;

	current->loopCountTop--;
}

// mark current chunk for continue jump
static void fei_compiler_markcontinuejump()
{
	current->continueJumps[current->loopCountTop] = fei_compiler_currentchunk()->count;
}

// patch available break jumps
static void fei_compiler_patchbreakjumps()
{
	for (int i = 0; i < current->breakJumpCounts[current->loopCountTop]; i++)
	{
		fei_compiler_patchjump(current->breakPatchJumps[current->loopCountTop][i]);
	}
}

/* variable declarations */
static uint8_t fei_compiler_makeidentconst(Token* name)
{
	return fei_compiler_makeconst(OBJ_VAL(copyString(name->start, name->length)));	// add to constant table
}

static bool fei_compiler_identsequal(Token* a, Token* b)
{
	if (a->length != b->length) return false;
	return memcmp(a->start, b->start, a->length) == 0;
}


static int fei_compiler_resolvelocal(Compiler* compiler, Token* name)
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
static int fei_compiler_addupvalue(Compiler* compiler, uint8_t index, bool isLocal)
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
static int fei_compiler_resolveupvalue(Compiler* compiler, Token* name)
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


static void fei_compiler_addlocal(Token name)
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

static void fei_compiler_declvarfromcurrent()	// for local variables
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

static uint8_t fei_compiler_parsevarfromcurrent(const char* errorMessage)
{
	fei_compiler_consume(TOKEN_IDENTIFIER, errorMessage);		// requires next token to be an identifier

	fei_compiler_declvarfromcurrent();
	if (current->scopeDepth > 0) return 0;			// if scopeDepth is not 0, then it is a local not global var
	// return a dummy index
	// at runtime, locals are not looked up by name so no need to insert them to a table


	return fei_compiler_makeidentconst(&parser.previous);	// return index from the constant table	
}


static void fei_compiler_markinit()
{
	if (current->scopeDepth == 0) return;				// if global return
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void fei_compiler_defvarindex(uint8_t global)
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
static uint8_t fei_compiler_parsearglist()
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
	fei_compiler_emitconst(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

// declare/call variables
static void fei_compiler_declnamedvar(Token name, bool canAssign)
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
static Token fei_compiler_makesyntoken(const char* text)
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
static void fei_compiler_parseprec(Precedence precedence)
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
static ParseRule* fei_compiler_getrule(TokenType type)
{
	return &rules[type];		
}

static void fei_compiler_parseexpr()		// a single 'statement' or line
{
	fei_compiler_parseprec(PREC_ASSIGNMENT);	// as assignment is the 2nd lowest, parses evrything
}

static void fei_compiler_parseblock()
{
	while (!fei_compiler_check(TOKEN_RIGHT_BRACE) && !fei_compiler_check(TOKEN_EOF))	// parse until EOF or right brace is 'peeked'
	{
		fei_compiler_parsedeclaration();		// compile rest of block, keeps on parsing until right brace or EOF is 'peeked'		
	}

	fei_compiler_consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");			
}


/* functions */
static void fei_compiler_parsefuncdecl(FunctionType type)
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
static void fei_compiler_parsemethoddecl()
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


static void fei_compiler_parseclassdecl()
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


static void fei_compiler_parseclassfuncdecl()
{
	uint8_t global = fei_compiler_parsevarfromcurrent("Expect function name.");
	fei_compiler_markinit();					// scoping
	fei_compiler_parsefuncdecl(TYPE_FUNCTION);	
	fei_compiler_defvarindex(global);
}

static void fei_compiler_parsevardecl()
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

static void fei_compiler_parseexprstmt()
{
	fei_compiler_parseexpr();
	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
	fei_compiler_emitbyte(OP_POP);
}

// if method
static void fei_compiler_parseifstmt()
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

static void fei_compiler_parseswitchstmt()
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


static void fei_compiler_parseprintstmt()
{
	fei_compiler_parseexpr();			// this is the function that actually processes the experssion
	fei_compiler_consume(TOKEN_SEMICOLON, "Expect ';' after value.");		// try consume ;, if fails show message
	fei_compiler_emitbyte(OP_PRINT);
}

static void fei_compiler_parsereturnstmt()
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

static void fei_compiler_parseforstmt()
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

static void fei_compiler_parsewhilestmt()
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

static void fei_compiler_parsebreakstmt()
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


static void fei_compiler_parsecontinuestmt()
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

static void fei_compiler_parserepeatuntilstmt()
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

static void fei_compiler_parsedowhilestmt()
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

static void fei_compiler_synchronize()
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

static void fei_compiler_parsedeclaration()
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

static void fei_compiler_parsestatement()					// either an expression or a print
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
		markObject((Obj*)compiler->function);
		compiler = compiler->enclosing;
	}
}
