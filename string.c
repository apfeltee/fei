
#include "fei.h"

// hash function, the FNV-1a
uint32_t fei_string_gethash(FeiState* state, const char* key, int length)
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

/* dynamic string impl via https://github.com/jasonmaclafferty/String */
FeiString* fei_string_make(FeiState* state, int length)
{
    int cap;
    char* strbuf;
    FeiString* str;
    cap = length;
    if(cap < 200)
    {
        cap = 200;
    }
    str = (FeiString*)fei_object_allocobject(state, sizeof(FeiString), OBJ_STRING);
    strbuf = (char*)calloc(cap + 1, sizeof(char));
    str->chars = strbuf;
    str->capacity = cap;
    str->length = 0;
    return str;
}

bool fei_string_destroy(FeiState* state, FeiString* str)
{
    if(str != NULL)
    {
        free(str->chars);
        fei_gcmem_reallocate(state, str, sizeof(FeiString), 0);
        return true;
    }
    return false;
}

bool fei_string_appendobj(FeiState* state, FeiString* dest, const FeiString* str2)
{
    return fei_string_append(state, dest, str2->chars, str2->length);
}

bool fei_string_append(FeiState* state, FeiString* dest, const char* strdata, int length)
{
    enum {
        string_chunk_size = 52,
    };
    int str1_pos;
    int str2_pos;
    char* temp;
    (void)state;
    temp = NULL;
    if((dest != NULL) && ((strdata != NULL) && (length > 0)))
    {
        if(length > 0)
        {
            /* + 2 for the '\0' characters */
            if(dest->capacity < dest->length + length + 2)
            {
                temp = (char*)realloc(dest->chars, dest->length + length + string_chunk_size);
                if(temp != NULL)
                {
                    dest->chars = temp;
                    dest->capacity = dest->length + length + string_chunk_size;
                }
            }
            if(dest->capacity >= dest->length + length + 2)
            {
                for(str1_pos = dest->length, str2_pos = 0; str2_pos < length; str1_pos++, str2_pos++)
                {
                    dest->chars[str1_pos] = strdata[str2_pos];
                    dest->length++;
                }
                dest->chars[str1_pos] = strdata[str2_pos]; /* copy null char */
            }
        }
        return true;
    }
    return false;
}

FeiString* fei_object_allocstring(FeiState* state, const char* chars, int length, uint32_t hash)// pass in hash
{
    FeiString* objstr;
    state->ocount.cntstring++;
    objstr = fei_string_make(state, length);
    fei_string_append(state, objstr, chars, length);
    objstr->hash = hash;
    // garbage collection
    fei_vm_stackpush(state, fei_value_makeobject(state, objstr));
    //printf("allocate\n");
    // for string interning
    fei_valtable_set(state, state->vmstate.strings, objstr, fei_value_makenull(state));
    // garbage collection
    fei_vm_stackpop(state);
    return objstr;
}

// shorten than fei_string_copy because owernship of the char* itself is declared in concatenate(), hence no need to declare memory again
FeiString* fei_string_take(FeiState* state, char* chars, int length)
{
    uint32_t hash;
    FeiString* interned;
    hash = fei_string_gethash(state, chars, length);
    interned = fei_valtable_findstring(state, state->vmstate.strings, chars, length, hash);
    if(interned != NULL)
    {
        FREE_ARRAY(state, sizeof(char), chars, length + 1);
        return interned;
    }
    return fei_object_allocstring(state, chars, length, hash);
}

// copy string from source code to memory
FeiString* fei_string_copy(FeiState* state, const char* chars, int length)
{
    uint32_t hash;
    FeiString* interned;
    hash = fei_string_gethash(state, chars, length);
    interned = fei_valtable_findstring(state, state->vmstate.strings, chars, length, hash);
    if(interned != NULL)
    {
        return interned;
    }
    return fei_object_allocstring(state, chars, length, hash);
}


