#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

// scanner to run through the source code
typedef struct
{
	const char* start;		// marks the beginning of the current lexeme('word', you can say_
	const char* current;	// points to the character being looked at
	int line;				// int to tell the current line being looked at
} Scanner;

Scanner scanner;

/* scanner.c */
void fei_lexer_initsource(const char *source);
static _Bool fei_lexutil_isalpha(char c);
static _Bool fei_lexutil_isdigit(char c);
static _Bool fei_lexer_isatend(void);
static char fei_lexer_advance(void);
static _Bool fei_lexer_match(char expected);
static Token fei_lexer_maketoken(TokenType type);
static Token fei_lexer_errortoken(const char *message);
static char fei_lexer_peekcurrent(void);
static char fei_lexer_peeknext(void);
static void fei_lexer_skipspace(void);
static TokenType fei_lexer_checkkw(int start, int length, const char *rest, TokenType type);
static TokenType fei_lexer_scantype(void);
static Token fei_lexer_scanident(void);
static Token fei_lexer_scannumber(void);
static Token fei_lexer_scanstring(void);
Token fei_lexer_scantoken(void);


void fei_lexer_initsource(const char* source)
{
	scanner.start = source;			// again, pointing to a string array means pointing to the beginning
	scanner.current = source;		
	scanner.line = 1;
}

// to check for identifiers(eg. for, while, print)
static bool fei_lexutil_isalpha(char c)
{
	return (c >= 'a' && c <= 'z') ||
		(c >= 'A' && c <= 'Z') ||
		c == '_';
} 

static bool fei_lexutil_isdigit(char c)
{
	return c >= '0' && c <= '9';		// let string comparison handle it
}

// to get EOF symbol -> '\0'
static bool fei_lexer_isatend()
{
	return *scanner.current == '\0';
}



// goes to next char
static char fei_lexer_advance()
{
	scanner.current++;				// advance to next 
	return scanner.current[-1];		// return previous one
}

// logical conditioning to check if 2nd character is embedded to first(e.g two char token)
static bool fei_lexer_match(char expected)
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
static Token fei_lexer_maketoken(TokenType type)
{
	Token token;
	token.type = type;
	token.start = scanner.start;
	token.length = (int)(scanner.current - scanner.start);
	token.line = scanner.line;

	return token;
}

// similar to fei_lexer_maketoken, but the "lexeme" points to the error message string instead of the scanner's current and start pointers
static Token fei_lexer_errortoken(const char* message)
{
	Token token;
	token.type = TOKEN_ERROR;
	token.start = message;
	token.length = (int)strlen(message);	// get string length and turn to int
	token.line = scanner.line;

	return token;
}

// returns current character
static char fei_lexer_peekcurrent()
{
	return *scanner.current;
}

// returns next character
static char fei_lexer_peeknext()
{
	if (fei_lexer_isatend()) return '\0';
	return scanner.current[1];		// C syntax, basically return index 1 (or second) from the array/pointer
}

// skipping white spaces, tabs, etc.
static void fei_lexer_skipspace()
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
static TokenType fei_lexer_checkkw(int start, int length, const char* rest, TokenType type)
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
static TokenType fei_lexer_scantype()
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

static Token fei_lexer_scanident()
{
	while (fei_lexutil_isalpha(fei_lexer_peekcurrent()) || fei_lexutil_isdigit(fei_lexer_peekcurrent())) fei_lexer_advance();		// skip if still letters or digits 
	return fei_lexer_maketoken(fei_lexer_scantype());
}

static Token fei_lexer_scannumber()
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
static Token fei_lexer_scanstring()
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

