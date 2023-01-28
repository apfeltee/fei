
#include "fei.h"


void fei_chunk_init(FeiState* state, FeiBytecodeList* chunk)
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

void fei_chunk_pushbyte(FeiState* state, FeiBytecodeList* chunk, uint8_t byte, int line)
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

void fei_chunk_destroy(FeiState* state, FeiBytecodeList* chunk)
{
    // chunk->code is the pointer to the array, capacity is the size
    FREE_ARRAY(state, sizeof(uint8_t), chunk->code, chunk->capacity);
    FREE_ARRAY(state, sizeof(int), chunk->lines, chunk->capacity);
    fei_valarray_destroy(state, &chunk->constants);
    fei_chunk_init(state, chunk);
}

int fei_chunk_pushconst(FeiState* state, FeiBytecodeList* chunk, FeiValue value)
{
    // garbage collection
    fei_vm_stackpush(state, value);
    fei_valarray_push(state, &chunk->constants, value);
    // garbage collection
    fei_vm_stackpop(state);
    // return index of the newly added constant
    return fei_valarray_count(&chunk->constants) - 1;
}

void fei_lexer_initsource(FeiState* state, const char* source, size_t len)
{
    memset(&state->aststate.scanner, 0, sizeof(FeiAstLexer));
    state->aststate.scanner.startsrc = source;
    state->aststate.scanner.currentsrc = source;
    state->aststate.scanner.length = len;
    state->aststate.scanner.line = 1;
}

// to check for identifiers(eg. for, while, print)
bool fei_lexutil_isalpha(FeiState* state, char c)
{
    (void)state;
    return (
        ((c >= 'a') && (c <= 'z')) ||
        ((c >= 'A') && (c <= 'Z')) ||
        (c == '_')
    );
}

bool fei_lexutil_isdigit(FeiState* state, char c)
{
    (void)state;
    // let string comparison handle it
    return (
        (c >= '0') && (c <= '9')
    );
}

const char* fei_lexer_tokenname(int t)
{
    switch(t)
    {
        case TOKEN_OPENPAREN: return "(";
        case TOKEN_CLOSEPAREN: return ")";
        case TOKEN_LEFTBRACE: return "{";
        case TOKEN_RIGHTBRACE: return "}";
        case TOKEN_OPENBRACKET: return "[";
        case TOKEN_CLOSEBRACKET: return "]";
        case TOKEN_COMMA: return ",";
        case TOKEN_DOT: return ".";
        case TOKEN_MINUS: return "-";
        case TOKEN_PLUS: return "+";
        case TOKEN_SEMICOLON: return ";";
        case TOKEN_COLON: return ":";
        case TOKEN_SLASH: return "/";
        case TOKEN_STAR: return "*";
        case TOKEN_MODULO: return "%";
        case TOKEN_SHIFTLEFT: return "<<";
        case TOKEN_SHIFTRIGHT: return ">>";
        case TOKEN_BITXOR: return "^";
        case TOKEN_BITNOT: return "~";
        case TOKEN_BITOR: return "|";
        case TOKEN_BITAND: return "&";
        case TOKEN_LOGICALNOT: return "!";
        case TOKEN_NOTEQUAL: return "!=";
        case TOKEN_ASSIGN: return "=";
        case TOKEN_EQUAL: return "==";
        case TOKEN_GREATERTHAN: return ">";
        case TOKEN_GREATEREQUAL: return ">=";
        case TOKEN_LESSTHAN: return "<";
        case TOKEN_LESSEQUAL: return "<=";
        case TOKEN_IDENTIFIER: return "identifier";
        case TOKEN_STRING: return "string";
        case TOKEN_NUMBER: return "number";
        case TOKEN_KWAND: return "and";
        case TOKEN_KWCLASS: return "class";
        case TOKEN_KWELF: return "elf";
        case TOKEN_KWELSE: return "else";
        case TOKEN_KWFALSE: return "false";
        case TOKEN_KWFOR: return "for";
        case TOKEN_KWFUN: return "fun";
        case TOKEN_KWIF: return "if";
        case TOKEN_KWNULL: return "null";
        case TOKEN_KWOR: return "or";
        case TOKEN_KWPRINT: return "print";
        case TOKEN_KWRETURN: return "return";
        case TOKEN_KWSUPER: return "super";
        case TOKEN_KWSWITCH: return "switch";
        case TOKEN_KWDEFAULT: return "default";
        case TOKEN_KWCASE: return "case";
        case TOKEN_KWTHIS: return "this";
        case TOKEN_KWTRUE: return "true";
        case TOKEN_KWVAR: return "var";
        case TOKEN_KWWHILE: return "while";
        case TOKEN_KWBREAK: return "break";
        case TOKEN_KWCONTINUE: return "continue";
        case TOKEN_KWDO: return "do";
        case TOKEN_KWREPEAT: return "repeat";
        case TOKEN_KWUNTIL: return "until";
        case TOKEN_KWFROM: return "from";
        case TOKEN_ERROR: return "error";
        case TOKEN_EOF: return "eof";
        default:
            break;
    }
    return "unknown/illegal";
}

// to get EOF symbol -> '\0'
bool fei_lexer_isatend(FeiState* state)
{
    return *state->aststate.scanner.currentsrc == '\0';
}

// goes to next char
char fei_lexer_advance(FeiState* state)
{
    // advance to next
    state->aststate.scanner.currentsrc++;
    // return previous one
    return state->aststate.scanner.currentsrc[-1];
}

// logical conditioning to check if 2nd character is embedded to first(e.g two char token)
bool fei_lexer_match(FeiState* state, char expected)
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
FeiAstToken fei_lexer_maketoken(FeiState* state, FeiAstTokType type)
{
    FeiAstToken token;
    token.type = type;
    token.toksrc = state->aststate.scanner.startsrc;
    token.length = (int)(state->aststate.scanner.currentsrc - state->aststate.scanner.startsrc);
    token.line = state->aststate.scanner.line;
    return token;
}

// similar to fei_lexer_maketoken, but the "lexeme" points to the error message string instead of the scanner's current and start pointers
FeiAstToken fei_lexer_errortoken(FeiState* state, const char* message)
{
    FeiAstToken token;
    token.type = TOKEN_ERROR;
    token.toksrc = message;
    token.length = (int)strlen(message);
    token.line = state->aststate.scanner.line;
    return token;
}

// returns current character
char fei_lexer_peekcurrent(FeiState* state)
{
    return *state->aststate.scanner.currentsrc;
}

// returns next character
char fei_lexer_peeknext(FeiState* state)
{
    if(fei_lexer_isatend(state))
    {
        return '\0';
    }
    return state->aststate.scanner.currentsrc[1];
}

// skipping white spaces, tabs, etc.
void fei_lexer_skipspace(FeiState* state)
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
FeiAstTokType fei_lexer_checkkw(FeiState* state, int start, int length, const char* rest, FeiAstTokType type)
{
    FeiAstLexer* scn;
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
FeiAstTokType fei_lexer_scantype(FeiState* state)
{
    FeiAstLexer* scn;
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

FeiAstToken fei_lexer_scanident(FeiState* state)
{
    char cur;
    while(true)
    {
        cur = fei_lexer_peekcurrent(state);
        if((fei_lexutil_isalpha(state, cur) || fei_lexutil_isdigit(state, cur)))
        {
            // skip if still letters or digits
            fei_lexer_advance(state);
        }
        else
        {
            break;
        }
    }
    return fei_lexer_maketoken(state, fei_lexer_scantype(state));
}

FeiAstToken fei_lexer_scannumber(FeiState* state)
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
FeiAstToken fei_lexer_scanstring(FeiState* state)
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
FeiAstToken fei_lexer_scantoken(FeiState* state)
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
                return fei_lexer_maketoken(state, TOKEN_OPENPAREN);
            }
            break;
        case ')':
            {
                return fei_lexer_maketoken(state, TOKEN_CLOSEPAREN);
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
        case '[':
            {
                return fei_lexer_maketoken(state, TOKEN_OPENBRACKET);
            }
            break;
        case ']':
            {
                return fei_lexer_maketoken(state, TOKEN_CLOSEBRACKET);
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
        case '&':
            {
                if(fei_lexer_match(state, '&'))
                {
                    /* just reuse the keyword 'and' */
                    return fei_lexer_maketoken(state, TOKEN_KWAND);
                }
                else
                {
                    return fei_lexer_maketoken(state, TOKEN_BITAND);
                }
            }
            break;
        case '|':
            {
                if(fei_lexer_match(state, '|'))
                {
                    /* reuse keyword 'or' */
                    return fei_lexer_maketoken(state, TOKEN_KWOR);
                }
                else
                {
                    return fei_lexer_maketoken(state, TOKEN_BITOR);
                }
            }
            break;
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
                if(fei_lexer_match(state, '>'))
                {
                    return fei_lexer_maketoken(state, TOKEN_SHIFTRIGHT);
                }
                else if(fei_lexer_match(state, '='))
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
                if(fei_lexer_match(state, '<'))
                {
                    return fei_lexer_maketoken(state, TOKEN_SHIFTLEFT);
                }
                else if(fei_lexer_match(state, '='))
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

FeiBytecodeList* fei_compiler_currentchunk(FeiState* state)
{
    return &state->aststate.compiler->programfunc->chunk;
}

// to handle syntax errors
void fei_compiler_raiseatv(FeiState* state, FeiAstToken* token, const char* fmt, va_list va)
{
    // if an error already exists, no need to run other errors
    if(state->aststate.parser.panicmode)
    {
        return;
    }
    state->aststate.parser.panicmode = true;
    fprintf(stderr, "!!!SYNTAX ERROR!!!\n");
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
    fprintf(stderr, ": ");
    vfprintf(stderr, fmt, va);
    fprintf(stderr, "\n");
    state->aststate.parser.haderror = true;
}

void fei_compiler_raiseat(FeiState* state, FeiAstToken* token, const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    fei_compiler_raiseatv(state, token, fmt, va);
    va_end(va);
}

// error from token most recently CONSUMED
void fei_compiler_raiseerrorv(FeiState* state, const char* fmt, va_list va)
{
    fei_compiler_raiseatv(state, &state->aststate.parser.prevtoken, fmt, va);
}

void fei_compiler_raiseerror(FeiState* state, const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    fei_compiler_raiseerrorv(state, fmt, va);
    va_end(va);
}


// handling error from token, the most current one being handed, not yet consumed
void fei_compiler_raiseherev(FeiState* state, const char* fmt, va_list va)
{
    // pass in the current parser
    fei_compiler_raiseatv(state, &state->aststate.parser.currtoken, fmt, va);
}

void fei_compiler_raisehere(FeiState* state, const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    fei_compiler_raiseherev(state, fmt, va);
    va_end(va);
}

/* main compile functions */

// pump the compiler, basically go to / 'read' the next token, a SINGLE token
void fei_compiler_advancenext(FeiState* state)
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
void fei_compiler_advanceskipping(FeiState* state, FeiAstTokType type)
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
void fei_compiler_consumev(FeiState* state, FeiAstTokType type, const char* fmt, va_list va)
{
    // if current token is equal to the token type being compared to
    if(state->aststate.parser.currtoken.type == type)
    {
        fei_compiler_advancenext(state);
        return;
    }
    // if consumes a different type, error
    fei_compiler_raiseherev(state, fmt, va);
}

void fei_compiler_consume(FeiState* state, FeiAstTokType type, const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    fei_compiler_consumev(state, type, fmt, va);
    va_end(va);
}

bool fei_compiler_check(FeiState* state, FeiAstTokType type)
{
    // check if current matches given
    return state->aststate.parser.currtoken.type == type;
}

bool fei_compiler_match(FeiState* state, FeiAstTokType type)
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
void fei_compiler_emitbyte(FeiState* state, uint8_t byte)
{
    // sends previous line so runtime errors are associated with that line
    fei_chunk_pushbyte(state, fei_compiler_currentchunk(state), byte, state->aststate.parser.prevtoken.line);
}

// write chunk for multiple chunks, used to write an opcode followed by an operand(eg. in constants)
void fei_compiler_emitbytes(FeiState* state, uint8_t byte1, uint8_t byte2)
{
    fei_compiler_emitbyte(state, byte1);
    fei_compiler_emitbyte(state, byte2);
}

// for looping statements
void fei_compiler_emitloop(FeiState* state, int loopstart)
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

void fei_compiler_emitcondloop(FeiState* state, int loopstart, bool condstate)
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

int fei_compiler_emitjump(FeiState* state, uint8_t instruction)
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
void fei_compiler_emitreturn(FeiState* state)
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
uint8_t fei_compiler_makeconst(FeiState* state, FeiValue value)
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
void fei_compiler_emitconst(FeiState* state, FeiValue value)
{
    // add value to constant table
    fei_compiler_emitbytes(state, OP_CONSTANT, fei_compiler_makeconst(state, value));
}

void fei_compiler_patchjump(FeiState* state, int offset)
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
void fei_compiler_init(FeiState* state, FeiAstCompiler* compiler, FuncType type)
{
    size_t i;
    FeiAstLocal* local;
    FeiAstCompiler* astcc;
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
        astcc->programfunc->name = fei_string_copy(state, state->aststate.parser.prevtoken.toksrc, state->aststate.parser.prevtoken.length);
    }
    for(i=0; i<CFG_MAX_COMPILERLOCALS; i++)
    {
        memset(&astcc->proglocals[i], 0, sizeof(FeiAstLocal));
    }    
    // compiler implicitly claims slot zero for local variables
    local = &astcc->proglocals[astcc->progloccount++];
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

ObjFunction* fei_compiler_endcompiler(FeiState* state)
{
    ObjFunction* function;
    FeiAstCompiler* astcc;
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

void fei_compiler_beginscope(FeiState* state)
{
    state->aststate.compiler->scopedepth++;
}

void fei_compiler_endscope(FeiState* state)
{
    FeiAstCompiler* cc;
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
void fei_compiler_beginloopscope(FeiState* state)
{
    state->aststate.compiler->loopcounttop++;
}

void fei_compiler_endloopscope(FeiState* state)
{
    FeiAstCompiler* astcc;
    astcc = state->aststate.compiler;
    if(astcc->breakjumpcounts[astcc->loopcounttop] > 0)
    {
        astcc->breakjumpcounts[astcc->loopcounttop] = 0;
    }
    astcc->loopcounttop--;
}

// mark current chunk for continue jump
void fei_compiler_markcontinuejump(FeiState* state)
{
    FeiAstCompiler* astcc;
    astcc = state->aststate.compiler;
    astcc->continuejumps[astcc->loopcounttop] = fei_compiler_currentchunk(state)->count;
}

// patch available break jumps
void fei_compiler_patchbreakjumps(FeiState* state)
{
    int i;
    FeiAstCompiler* astcc;
    astcc = state->aststate.compiler;
    for(i = 0; i < astcc->breakjumpcounts[astcc->loopcounttop]; i++)
    {
        fei_compiler_patchjump(state, astcc->breakpatchjumps[astcc->loopcounttop][i]);
    }
}

/* variable declarations */
uint8_t fei_compiler_makeidentconst(FeiState* state, FeiAstToken* name)
{
    // add to constant table
    return fei_compiler_makeconst(state, fei_value_makeobject(state, fei_string_copy(state, name->toksrc, name->length)));
}

bool fei_compiler_identsequal(FeiState* state, FeiAstToken* a, FeiAstToken* b)
{
    (void)state;
    if(a->length != b->length)
    {
        return false;
    }
    return memcmp(a->toksrc, b->toksrc, a->length) == 0;
}


int fei_compiler_resolvelocal(FeiState* state, FeiAstCompiler* compiler, FeiAstToken* name)
{
    int i;
    FeiAstLocal* local;
    // walk through the local variables
    for(i = compiler->progloccount - 1; i >= 0; i--)
    {
        local = &compiler->proglocals[i];
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
int fei_compiler_addupvalue(FeiState* state, FeiAstCompiler* compiler, uint8_t index, bool islocal)
{
    int i;
    int upvaluecount;
    FeiAstUpvalue* upvalue;
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
int fei_compiler_resolveupvalue(FeiState* state, FeiAstCompiler* compiler, FeiAstToken* name)
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

void fei_compiler_addlocal(FeiState* state, FeiAstToken name)
{
    FeiAstLocal* local;
    FeiAstCompiler* astcc;
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

void fei_compiler_declvarfromcurrent(FeiState* state)// for local variables
{
    int i;
    FeiAstLocal* local;
    FeiAstToken* name;
    FeiAstCompiler* astcc;
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

uint8_t fei_compiler_parsevarfromcurrent(FeiState* state, const char* errormessage)
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

void fei_compiler_markinit(FeiState* state)
{
    FeiAstCompiler* astcc;
    astcc = state->aststate.compiler;
    if(astcc->scopedepth == 0)
    {
        // if global return
        return;
    }
    astcc->proglocals[astcc->progloccount - 1].depth = astcc->scopedepth;
}

void fei_compiler_defvarindex(FeiState* state, uint8_t global)
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
uint8_t fei_compiler_parsearglist(FeiState* state, const char* contextname, FeiAstTokType tokbegin, FeiAstTokType tokend)
{
    uint8_t argcount;
    argcount = 0;
    // if ) has not been reached
    if(!fei_compiler_check(state, tokend))
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
    fei_compiler_consume(state, tokend, "expect '%s' after %s", fei_lexer_tokenname(tokend));
    return argcount;
}

static void fei_comprule_index(FeiState* state, bool canassign)
{
    (void)canassign;
    fei_compiler_parseexpr(state);
    fei_compiler_consume(state, TOKEN_CLOSEBRACKET, "expect ']' after index expression");
    if(fei_compiler_match(state, TOKEN_ASSIGN))
    {
        fei_compiler_parseexpr(state);
        fei_compiler_emitbyte(state, OP_SETINDEX);
    }
    else
    {
        fei_compiler_emitbyte(state, OP_GETINDEX);
    }
}

static void fei_comprule_arraylit(FeiState* state, bool canassign)
{
    int itemcount;
    (void)canassign;
    itemcount = 0;
    itemcount = fei_compiler_parsearglist(state, "array literal", TOKEN_OPENBRACKET, TOKEN_CLOSEBRACKET);
    fei_compiler_emitbytes(state, OP_MAKEARRAY, itemcount);
}

static void fei_comprule_logicaland(FeiState* state, bool canassign)
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
static void fei_comprule_binary(FeiState* state, bool canassign)
{
    FeiAstRule* rule;
    FeiAstTokType operatortype;
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
        case TOKEN_SHIFTLEFT:
            {
                fei_compiler_emitbyte(state, OP_SHIFTLEFT);
            }
            break;
        case TOKEN_SHIFTRIGHT:
            {
                fei_compiler_emitbyte(state, OP_SHIFTRIGHT);
            }
            break;
        case TOKEN_BITXOR:
            {
                fei_compiler_emitbyte(state, OP_BITXOR);
            }
            break;
        case TOKEN_BITAND:
            {
                fei_compiler_emitbyte(state, OP_BITAND);
            }
            break;
        case TOKEN_BITOR:
            {
                fei_compiler_emitbyte(state, OP_BITOR);
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


// parentheses for grouping
static void fei_comprule_grouping(FeiState* state, bool canassign)
{
    (void)canassign;
    // assume initial ( has already been consumed,
    // and recursively call to fei_compiler_parseexpr() to compile between the parentheses
    fei_compiler_parseexpr(state);
    // expects a right parentheses, if not received then  error
    fei_compiler_consume(state, TOKEN_CLOSEPAREN, "Expect ')' after expression.");
}

// for function calls
static void fei_comprule_call(FeiState* state, bool canassign)
{
    (void)canassign;
    // again, assumes the function itself(its call name) has been placed on the codestream stack
    uint8_t argcount = fei_compiler_parsearglist(state, "call arguments", TOKEN_OPENPAREN, TOKEN_CLOSEPAREN);
    fei_compiler_emitbytes(state, OP_CALL, argcount);
}

// class members/fields/properties
static void fei_comprule_dot(FeiState* state, bool canassign)
{
    uint8_t name;
    uint8_t argcount;
    fei_compiler_consume(state, TOKEN_IDENTIFIER, "Expect propery name after class instance.");
    // already consumed
    name = fei_compiler_makeidentconst(state, (&state->aststate.parser.prevtoken));
    // assignment
    if(canassign && fei_compiler_match(state, TOKEN_ASSIGN))
    {
        // evalute expression to be set
        fei_compiler_parseexpr(state);
        fei_compiler_emitbytes(state, OP_SET_PROPERTY, name);
    }
    // for running class methods, access the method and call it at the same time
    else if(fei_compiler_match(state, TOKEN_OPENPAREN))
    {
        argcount = fei_compiler_parsearglist(state, "dot-function call", TOKEN_OPENPAREN, TOKEN_CLOSEPAREN);
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

static void fei_comprule_literal(FeiState* state, bool canassign)
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


/* parsing the tokens */
static void fei_comprule_number(FeiState* state, bool canassign)
{
    FeiValue val;
    int64_t fixed;
    double dv;
    (void)canassign;
    // strtod below converts from string to double
    // assume that token for the number literal has already been consumed and is stored in previous
    // double strtod(const char* str, char** endptr)
    // endptr is the first non-double character after teh char* str character; if none then null
    /*	The way it works:
    -> in scanner, if a digit exists after a digit, it advances() (skips) the current
    -> hence, we get that the start points to the START of the digit, and using strtod smartly it reaches until the last digit
    */
    dv = strtod(state->aststate.parser.prevtoken.toksrc, NULL);
    fixed = (int64_t)dv;
    #if 1
    if(fixed == dv)
    {
        fprintf(stderr, "making fixed: fixed=%d\n", fixed);
        val = fei_value_makefixednumber(state, fixed);
    }
    else
    #endif
    {
        fprintf(stderr, "making float: dv=%g\n", dv);
        val = fei_value_makefloatnumber(state, dv);
    }
    //printf("num %c\n", *state->aststate.parser.prevtoken.toksrc);
    fei_compiler_emitconst(state, val);
}

static void fei_comprule_logicalor(FeiState* state, bool canassign)
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
static void fei_comprule_string(FeiState* state, bool canassign)
{
    (void)canassign;
    // in a string, eg. "hitagi", the quotation marks are trimmed
    fei_compiler_emitconst(state, fei_value_makeobject(state, fei_string_copy(state,
        state->aststate.parser.prevtoken.toksrc + 1,
        state->aststate.parser.prevtoken.length - 2)
    ));
}

// declare/call variables
void fei_compiler_declnamedvar(FeiState* state, FeiAstToken name, bool canassign)
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

static void fei_comprule_variable(FeiState* state, bool canassign)
{
    (void)canassign;
    fei_compiler_declnamedvar(state, state->aststate.parser.prevtoken, canassign);
}

// for super classes, token that mimics as if a user types in 'super'
FeiAstToken fei_compiler_makesyntoken(FeiState* state, const char* text)
{
    FeiAstToken token;
    (void)state;
    token.toksrc = text;
    token.length = (int)strlen(text);
    return token;
}

// for super calls
static void fei_comprule_super(FeiState* state, bool canassign)
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
    if(fei_compiler_match(state, TOKEN_OPENPAREN))
    {
        argcount = fei_compiler_parsearglist(state, "super invoke params", TOKEN_OPENPAREN, TOKEN_CLOSEPAREN);
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
static void fei_comprule_this(FeiState* state, bool canassign)
{
    (void)canassign;
    // if not inside a class
    if(state->aststate.classcompiler == NULL)
    {
        fei_compiler_raiseerror(state, "cannot use 'this' outside of class");
        return;
    }
    // always false
    fei_comprule_variable(state, false);
}

static void fei_comprule_unary(FeiState* state, bool canassign)
{
    FeiAstTokType operatortype;
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
void fei_compiler_parseprec(FeiState* state, Precedence precedence)
{
    int nowtyp;
    bool canassign;
    FeiAstRule* rule;
    ParseFn prefixrule;
    ParseFn infixrule;
    /*
    * PREFIX FIRST
    * look up for a prefix token, and the FIRSt token is ALWAYS going to be a prefix
    */
    fei_compiler_advancenext(state);// again, go next first then use previous type as the 'current' token
    // the way the compiler is designed is that it has to always have a prefix
    nowtyp = state->aststate.parser.prevtoken.type;
    prefixrule = fei_compiler_getrule(state, nowtyp)->prefix;
    if(prefixrule == NULL)
    {
        fei_compiler_raiseerror(state, "no parse rule for prefix '%s'", fei_lexer_tokenname(nowtyp));
        return;
    }
    canassign = precedence <= PREC_ASSIGNMENT;// for assignment precedence
    prefixrule(state, canassign);// call the prefix function, may consume a lot of tokens
    /*
    * after prefix expression is done, look for infix expression
    * IMPORTANT: infix only runs if given precedence is LOWER than the operator for the infix
    * or more nicely if NEXT/INFIX PRECEDENCE IS HIGHER THAN PREC ASSIGNMENT(parameter above_
    */
    while(true)
    {
        nowtyp = state->aststate.parser.currtoken.type;
        rule = fei_compiler_getrule(state, nowtyp);
        if(rule == NULL)
        {
            fei_compiler_raiseerror(state, "cannot get parser rule for token '%s' (%d)", fei_lexer_tokenname(nowtyp), nowtyp);
            break;
        }
        if(precedence <= rule->precedence)
        {
            fei_compiler_advancenext(state);
            infixrule = fei_compiler_getrule(state, state->aststate.parser.prevtoken.type)->infix;
            infixrule(state, canassign);
        }
        else
        {
            break;
        }
    }
    //consume(TOKEN_KWAND, "consume and failed");
    // if = is not consumed as part of the expression, nothing will , hence an error
    if(canassign && fei_compiler_match(state, TOKEN_ASSIGN))
    {
        fei_compiler_raiseerror(state, "invalid assignment target");
    }
}


static void fei_comprule_logicaland(FeiState* state, bool canassign);
static void fei_comprule_binary(FeiState* state, bool canassign);
static void fei_comprule_call(FeiState* state, bool canassign);
static void fei_comprule_dot(FeiState* state, bool canassign);
static void fei_comprule_literal(FeiState* state, bool canassign);
static void fei_comprule_grouping(FeiState* state, bool canassign);
static void fei_comprule_number(FeiState* state, bool canassign);
static void fei_comprule_logicalor(FeiState* state, bool canassign);
static void fei_comprule_string(FeiState* state, bool canassign);
static void fei_comprule_variable(FeiState* state, bool canassign);
static void fei_comprule_super(FeiState* state, bool canassign);
static void fei_comprule_this(FeiState* state, bool canassign);
static void fei_comprule_unary(FeiState* state, bool canassign);
static void fei_comprule_index(FeiState* state, bool canassign);
static void fei_comprule_arraylit(FeiState* state, bool canassign);

// get pointer to FeiAstRule struct according to type parameter
FeiAstRule* fei_compiler_getrule(FeiState* state, FeiAstTokType type)
{
    static FeiAstRule rule;
    (void)state;
    switch(type)
    {
        case TOKEN_OPENPAREN: rule = (FeiAstRule){ fei_comprule_grouping, fei_comprule_call, PREC_CALL }; break;
        case TOKEN_CLOSEPAREN: rule = (FeiAstRule){ NULL, NULL, PREC_NONE }; break;
        case TOKEN_LEFTBRACE: rule = (FeiAstRule){ NULL, NULL, PREC_NONE }; break;
        case TOKEN_RIGHTBRACE: rule = (FeiAstRule){ NULL, NULL, PREC_NONE }; break;
        case TOKEN_OPENBRACKET: rule = (FeiAstRule) { fei_comprule_arraylit, fei_comprule_index, PREC_TERM }; break;
        case TOKEN_CLOSEBRACKET: rule = (FeiAstRule){ NULL, NULL, PREC_NONE }; break;
        case TOKEN_COMMA: rule = (FeiAstRule){ NULL, NULL, PREC_NONE };break;
        case TOKEN_DOT: rule = (FeiAstRule){ NULL, fei_comprule_dot, PREC_CALL }; break;
        case TOKEN_MINUS: rule = (FeiAstRule){ fei_comprule_unary, fei_comprule_binary, PREC_TERM }; break;
        case TOKEN_PLUS: rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_TERM }; break;
        case TOKEN_SEMICOLON: rule = (FeiAstRule){ NULL, NULL, PREC_NONE }; break;
        case TOKEN_SLASH:
        case TOKEN_MODULO:
        case TOKEN_STAR:
        case TOKEN_SHIFTLEFT:
        case TOKEN_SHIFTRIGHT:
        case TOKEN_BITOR:
        case TOKEN_BITXOR:
        case TOKEN_BITAND:
            rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_FACTOR };
            break;
        case TOKEN_LOGICALNOT: rule = (FeiAstRule){ fei_comprule_unary, NULL, PREC_NONE }; break;
        case TOKEN_NOTEQUAL:
            rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_EQUALITY };
            break;// equality precedence
        case TOKEN_ASSIGN:
            rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;// comaprison precedence
        case TOKEN_EQUAL:
            rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_GREATERTHAN:
            rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_GREATEREQUAL:
            rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_LESSTHAN:
            rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_LESSEQUAL:
            rule = (FeiAstRule){ NULL, fei_comprule_binary, PREC_COMPARISON };
            break;
        case TOKEN_IDENTIFIER:
            rule = (FeiAstRule){ fei_comprule_variable, NULL, PREC_NONE };
            break;
        case TOKEN_STRING:
            rule = (FeiAstRule){ fei_comprule_string, NULL, PREC_NONE };
            break;
        case TOKEN_NUMBER:
            rule = (FeiAstRule){ fei_comprule_number, NULL, PREC_NONE };
            break;
        case TOKEN_KWAND:
            rule = (FeiAstRule){ NULL, fei_comprule_logicaland, PREC_AND };
            break;
        case TOKEN_KWCLASS:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWELSE:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWFALSE:
            rule = (FeiAstRule){ fei_comprule_literal, NULL, PREC_NONE };
            break;
        case TOKEN_KWFOR:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWFUN:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWIF:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWSWITCH:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWNULL:
            rule = (FeiAstRule){ fei_comprule_literal, NULL, PREC_NONE };
            break;
        case TOKEN_KWOR:
            rule = (FeiAstRule){ NULL, fei_comprule_logicalor, PREC_OR };
            break;
        case TOKEN_KWPRINT:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWRETURN:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWSUPER:
            rule = (FeiAstRule){ fei_comprule_super, NULL, PREC_NONE };
            break;
        case TOKEN_KWTHIS:
            rule = (FeiAstRule){ fei_comprule_this, NULL, PREC_NONE };
            break;
        case TOKEN_KWTRUE:
            rule = (FeiAstRule){ fei_comprule_literal, NULL, PREC_NONE };
            break;
        case TOKEN_KWVAR:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_KWWHILE:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_ERROR:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        case TOKEN_EOF:
            rule = (FeiAstRule){ NULL, NULL, PREC_NONE };
            break;
        default:
            {
                return NULL;
            };
    }
    return &rule;
}

// a single 'statement' or line
void fei_compiler_parseexpr(FeiState* state)
{
    // as assignment is the 2nd lowest, parses evrything
    fei_compiler_parseprec(state, PREC_ASSIGNMENT);
}

void fei_compiler_parseblock(FeiState* state)
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
void fei_compiler_parsefuncdecl(FeiState* state, FuncType type)
{
    int i;
    uint8_t paramconstant;
    FeiAstCompiler compiler;
    ObjFunction* function;
    // create separate FeiAstCompiler for each function
    fei_compiler_init(state, &compiler, type);
    fei_compiler_beginscope(state);
    // compile parameters
    fei_compiler_consume(state, TOKEN_OPENPAREN, "Expect '(' after function name.");
    if(!fei_compiler_check(state, TOKEN_CLOSEPAREN))// if end ) has not been reached
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
    fei_compiler_consume(state, TOKEN_CLOSEPAREN, "Expect ')' after parameter list.");
    // body
    fei_compiler_consume(state, TOKEN_LEFTBRACE, "Expect '{' before function body.");
    fei_compiler_parseblock(state);
    // create function object
    function = fei_compiler_endcompiler(state);// ends the current compiler
    // compilers are treated like a stack; if current one is ended, like above, return to the previous one
    // fei_compiler_emitbytes(state, OP_CONSTANT, fei_compiler_makeconst(state, fei_value_makeobject(state, function)));
    fei_compiler_emitbytes(state, OP_CLOSURE, fei_compiler_makeconst(state, fei_value_makeobject(state, function)));
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
void fei_compiler_parsemethoddecl(FeiState* state)
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


void fei_compiler_parseclassdecl(FeiState* state)
{
    uint8_t nameconstant;
    FeiAstToken classname;
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

void fei_compiler_parseclassfuncdecl(FeiState* state)
{
    uint8_t global;
    global = fei_compiler_parsevarfromcurrent(state, "Expect function name.");
    fei_compiler_markinit(state);// scoping
    fei_compiler_parsefuncdecl(state, TYPE_FUNCTION);
    fei_compiler_defvarindex(state, global);
}

void fei_compiler_parsevardecl(FeiState* state)
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

void fei_compiler_parseexprstmt(FeiState* state)
{
    fei_compiler_parseexpr(state);
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after expression.");
    }
    fei_compiler_emitbyte(state, OP_POP);
}

// if method
void fei_compiler_parseifstmt(FeiState* state)
{
    int elsejump;
    int thenjump;
    //fei_compiler_consume(state, TOKEN_OPENPAREN, "Expect '(' after 'if'.");
    // compile the expression statment inside; fei_compiler_parseprec()
    // after compiling expression above conditon value will be left at the top of the stack
    //fei_compiler_consume(state, TOKEN_CLOSEPAREN, "Expect ')' after condition.");
    // gives an operand on how much to offset the ip; how many bytes of code to skip
    // if falsey, simply adjusts the ip by that amount
    // offset to jump to next (potentially else or elf) statment
    // insert to opcode the then branch statment first, then get offset
    fei_compiler_parseexpr(state);
    /* this gets distance */
    thenjump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);
    fei_compiler_emitbyte(state, OP_POP);
    /* use BACKPATCHING: emit jump first with a placeholder offset, and get how far to jump */
    fei_compiler_parsestatement(state);
    // below jump wil SURELY jump; this is skipped if the first fei_compiler_emitjump is not false
    // need to jump at least 'twice' with an else statement
    // if the original statement is  true, then skip the the else statement
    // if then statment is run; pop the expression inside () after if
    elsejump = fei_compiler_emitjump(state, OP_JUMP);
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

void fei_compiler_parseswitchstmt(FeiState* state)
{
    uint8_t i;
    uint8_t casescount;
    uint8_t capacity;
    int* casesoffset;
    int oldcapacity;
    int casefalsejump;
    // check next token
    if(!fei_compiler_check(state, TOKEN_IDENTIFIER))
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
        fei_compiler_consume(state, TOKEN_COLON, "expect ':' after 'case' expression");
        // check if both values are equal
        fei_compiler_emitbyte(state, OP_SWITCH_EQUAL);
        // jump if false
        casefalsejump = fei_compiler_emitjump(state, OP_JUMP_IF_FALSE);
        // parse the statment
        fei_compiler_parsestatement(state);
        // pop the 'true' from OP_SWITCH_EQUAL
        fei_compiler_emitbyte(state, OP_POP);
        casesoffset[casescount] = fei_compiler_emitjump(state, OP_JUMP);
        // jump to end of case if false
        fei_compiler_patchjump(state, casefalsejump);
        // pop the 'false' statment from OP_SWITCH_EQUAL
        fei_compiler_emitbyte(state, OP_POP);
    } while(fei_compiler_match(state, TOKEN_KWCASE));
    if(fei_compiler_match(state, TOKEN_KWDEFAULT))
    {
        fei_compiler_consume(state, TOKEN_COLON, "expect ':' after 'default'");
        // running the default statement
        fei_compiler_parsestatement(state);
    }
    // fei_compiler_patchjump for each available jump
    for(i = 0; i <= casescount; i++)
    {
        fei_compiler_patchjump(state, casesoffset[i]);
    }
    fei_compiler_emitbyte(state, OP_POP);// pop switch constant
    FREE_ARRAY(state, sizeof(int), casesoffset, capacity);
    fei_compiler_consume(state, TOKEN_RIGHTBRACE, "expect '}' at the end of switch statement");
}

void fei_compiler_parseprintstmt(FeiState* state)
{
    // this is the function that actually processes the experssion
    fei_compiler_parseexpr(state);
    if(fei_compiler_match(state, TOKEN_SEMICOLON))
    {
        //fei_compiler_consume(state, TOKEN_SEMICOLON, "Expect ';' after value.");// try consume ;, if fails show message
    }
    fei_compiler_emitbyte(state, OP_PRINT);
}

void fei_compiler_parsereturnstmt(FeiState* state)
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

void fei_compiler_parseforstmt(FeiState* state)
{
    int exitjump;
    int bodyjump;
    int loopstart;
    int incrementstart;
    // for possible variable declarations in clause
    fei_compiler_beginscope(state);
    fei_compiler_beginloopscope(state);
    fei_compiler_consume(state, TOKEN_OPENPAREN, "Expect '(' after 'for'.");
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
    if(!fei_compiler_match(state, TOKEN_CLOSEPAREN))
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
        fei_compiler_consume(state, TOKEN_CLOSEPAREN, "Expect ')' after for clauses.");
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

void fei_compiler_parsewhilestmt(FeiState* state)
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

void fei_compiler_parsebreakstmt(FeiState* state)
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

void fei_compiler_parsecontinuestmt(FeiState* state)
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

void fei_compiler_parserepeatuntilstmt(FeiState* state)
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

void fei_compiler_parsedowhilestmt(FeiState* state)
{
    int loopstart = fei_compiler_currentchunk(state)->count;
    fei_compiler_beginloopscope(state);
    fei_compiler_markcontinuejump(state);
    // process the statement
    fei_compiler_parsestatement(state);
    fei_compiler_consume(state, TOKEN_KWWHILE, "expect 'until' after 'repeat'");
    // get true or false
    fei_compiler_parseexpr(state);
    // emit loop if true op code
    fei_compiler_emitcondloop(state, loopstart, true);
    // patch possible break jumps
    fei_compiler_patchbreakjumps(state);
    fei_compiler_endloopscope(state);
    fei_compiler_consume(state, TOKEN_SEMICOLON, "expect ';' after 'until'");
}

void fei_compiler_synchronize(FeiState* state)
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

void fei_compiler_parsedeclaration(FeiState* state)
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

void fei_compiler_parsestatement(FeiState* state)// either an expression or a print
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

ObjFunction* fei_compiler_compilesource(FeiState* state, const char* source, size_t len)
{
    FeiAstCompiler compiler;
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
void fei_compiler_markroots(FeiState* state)
{
    FeiAstCompiler* compiler;
    compiler = state->aststate.compiler;
    while(compiler != NULL)
    {
        fei_gcmem_markobject(state, (FeiObject*)compiler->programfunc);
        compiler = compiler->enclosing;
    }
}

