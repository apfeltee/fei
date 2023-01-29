
#include "fei.h"

/*
* eventually this interface *should* also support creating a string.
*/
FeiWriter* fei_writer_init(FeiState* state)
{
    FeiWriter* wr;
    wr = (FeiWriter*)ALLOCATE(state, sizeof(FeiWriter), 1);
    wr->filehandle = NULL;
    wr->filemustclose = false;
    return wr;
}

void fei_writer_destroy(FeiState* state, FeiWriter* wr)
{
    FREE(state, sizeof(FeiWriter), wr);
}

FeiWriter* fei_writer_initfile(FeiState* state, FILE* fh, bool alsoclose)
{
    FeiWriter* wr;
    wr = fei_writer_init(state);
    wr->filehandle = fh;
    wr->filemustclose = alsoclose;
    return wr;
}


void fei_writer_appendstringlen(FeiWriter* wr, const char* str, size_t len)
{
    if(wr->filehandle != NULL)
    {
        fwrite(str, len, sizeof(char), wr->filehandle);
    }
}

void fei_writer_appendstring(FeiWriter* wr, const char* str)
{
    return fei_writer_appendstringlen(wr, str, strlen(str));
}

void fei_writer_appendchar(FeiWriter* wr, int c)
{
    char actualch;
    actualch = c;
    fei_writer_appendstringlen(wr, &actualch, 1);
}

void fei_writer_appendfmtva(FeiWriter* wr, const char* fmt, va_list va)
{
    if(wr->filehandle != NULL)
    {
        vfprintf(wr->filehandle, fmt, va);
    }
}

void fei_writer_appendfmt(FeiWriter* wr, const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    fei_writer_appendfmtva(wr, fmt, va);
    va_end(va);
}

void fei_writer_appendescapedchar(FeiWriter* wr, int ch)
{
    switch(ch)
    {
        case '\'':
            {
                fei_writer_appendstring(wr, "\\\'");
            }
            break;
        case '\"':
            {
                fei_writer_appendstring(wr, "\\\"");
            }
            break;
        case '\\':
            {
                fei_writer_appendstring(wr, "\\\\");
            }
            break;
        case '\b':
            {
                fei_writer_appendstring(wr, "\\b");
            }
            break;
        case '\f':
            {
                fei_writer_appendstring(wr, "\\f");
            }
            break;
        case '\n':
            {
                fei_writer_appendstring(wr, "\\n");
            }
            break;
        case '\r':
            {
                fei_writer_appendstring(wr, "\\r");
            }
            break;
        case '\t':
            {
                fei_writer_appendstring(wr, "\\t");
            }
            break;
        default:
            {
                /*
                static const char* const hexchars = "0123456789ABCDEF";
                os << '\\';
                if(ch <= 255)
                {
                    os << 'x';
                    os << hexchars[(ch >> 4) & 0xf];
                    os << hexchars[ch & 0xf];
                }
                else
                {
                    os << 'u';
                    os << hexchars[(ch >> 12) & 0xf];
                    os << hexchars[(ch >> 8) & 0xf];
                    os << hexchars[(ch >> 4) & 0xf];
                    os << hexchars[ch & 0xf];
                }
                */
                fei_writer_appendfmt(wr, "\\x%02x", (unsigned char)ch);
            }
            break;
    }
}

void fei_writer_appendquotedstring(FeiWriter* wr, const char* str, size_t len, bool withquot)
{
    char ch;
    int bch;
    size_t i;
    if(withquot)
    {
        fei_writer_appendchar(wr, '"');
    }
    for(i=0; i<len; i++)
    {
        bch = str[i];
        if((bch < 32) || (bch > 127) || (bch == '\"') || (bch == '\\'))
        {
            fei_writer_appendescapedchar(wr, bch);
        }
        else
        {
            ch = bch;
            fei_writer_appendstringlen(wr, &ch, 1);
        }
    }
    if(withquot)
    {
        fei_writer_appendchar(wr, '"');
    }
}



