
#include "fei.h"

/*
* eventually this interface *should* also support creating a string.
*/
Writer* fei_writer_init(FeiState* state)
{
    Writer* wr;
    wr = (Writer*)ALLOCATE(state, sizeof(Writer), 1);
    wr->filehandle = NULL;
    wr->filemustclose = false;
    return wr;
}

void fei_writer_destroy(FeiState* state, Writer* wr)
{
    FREE(state, sizeof(Writer), wr);
}

Writer* fei_writer_initfile(FeiState* state, FILE* fh, bool alsoclose)
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

void fei_writer_appendchar(Writer* wr, int c)
{
    char actualch;
    actualch = c;
    fei_writer_appendstringlen(wr, &actualch, 1);
}

void fei_writer_appendfmtva(Writer* wr, const char* fmt, va_list va)
{
    if(wr->filehandle != NULL)
    {
        vfprintf(wr->filehandle, fmt, va);
    }
}

void fei_writer_appendfmt(Writer* wr, const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    fei_writer_appendfmtva(wr, fmt, va);
    va_end(va);
}



