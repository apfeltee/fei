
#include "fei.h"

void fei_tostring_func(FeiState* state, FeiWriter* wr, FeiObjFunction* function)
{
    (void)state;
    if(function->name == NULL)
    {
        fei_writer_appendfmt(wr, "<script>");
    }
    else
    {
        //fei_writer_appendfmt(wr, "<function '%s'(%d params)>", function->name->chars, function->arity);
        fei_writer_appendfmt(wr, "<function (%d params)>", function->arity);

    }
}

void fei_tostring_string(FeiState* state, FeiWriter* wr, FeiString* ostr, bool withquot)
{
    size_t len;
    char* str;
    (void)state;
    len = ostr->length;
    str = ostr->chars;
    if(withquot)
    {
        fei_writer_appendquotedstring(wr, str, len, true);
    }
    else
    {
        fei_writer_appendstringlen(wr, str, len);
    }
}

void fei_tostring_array(FeiState* state, FeiWriter* wr, FeiArray* arr, bool withquot)
{
    size_t i;
    size_t len;
    FeiValue val;
    len = fei_array_count(arr);
    fei_writer_appendchar(wr, '[');
    for(i=0; i<len; i++)
    {
        val = fei_valarray_get(arr->items, i);
        if(fei_value_isarray(val) && (fei_value_asarray(val) == arr))
        {
            fei_writer_appendfmt(wr, "(recursion)");
        }
        else
        {
            fei_tostring_value(state, wr, val, withquot);
        }
        if((i+1) < len)
        {
            fei_writer_appendchar(wr, ',');
        }
    }
    fei_writer_appendchar(wr, ']');
}


void fei_tostring_table(FeiState* state, FeiWriter* wr, FeiTable* tbl, bool withquot)
{
    size_t i;
    size_t len;
    FeiValTabEntry* ent;
    len = fei_table_count(tbl);
    fei_writer_appendchar(wr, '{');
    for(i=0; i<len; i++)
    {
        ent = &tbl->table->entries[i];
        if(ent->key == NULL)
        {
            fei_writer_appendfmt(wr, "<NULL>");
        }
        else
        {
            fei_writer_appendfmt(wr, "\"");
            fei_writer_appendquotedstring(wr, ent->key->chars, ent->key->length, true);
            fei_writer_appendfmt(wr, "\"");
        }
        fei_writer_appendfmt(wr, ": ");
        if(fei_value_isarray(ent->value) && (fei_value_astable(ent->value) == tbl))
        {
            fei_writer_appendfmt(wr, "(recursion)");
        }
        else
        {
            fei_tostring_value(state, wr, ent->value, withquot);
        }
        if((i+1) < len)
        {
            fei_writer_appendchar(wr, ',');
        }
    }
    fei_writer_appendchar(wr, '}');
}


// actual printing on the virtual machine is done here
void fei_tostring_value(FeiState* state, FeiWriter* wr, FeiValue value, bool withquot)
{
    switch(value.type)
    {
        case VAL_UNDEF:
            {
                fei_writer_appendfmt(wr, "<uninitialized>");
            }
            break;
        case VAL_BOOL:
            {
                fei_writer_appendfmt(wr, fei_value_asbool(value) ? "true" : "false");
            }
            break;
        case VAL_NULL:
            {
                fei_writer_appendfmt(wr, "null");
            }
            break;
        case VAL_NUMBER:
            {
                if(value.isfixednumber)
                {
                    fei_writer_appendfmt(wr, "%ld", fei_value_asfixednumber(value));
                }
                else
                {
                    fei_writer_appendfmt(wr, "%g", fei_value_asfloatnumber(value));
                }
            }
            break;
        case VAL_OBJ:
            {
                fei_tostring_object(state, wr, value, withquot);
            }
            break;
    }
}

void fei_tostring_object(FeiState* state, FeiWriter* wr, FeiValue value, bool withquot)
{
    FeiClass* klass;
    FeiInstance* instance;
    (void)state;
    // first class objects can be printed; string and functions
    switch(fei_value_objtype(value))
    {
        case OBJ_BOUND_METHOD:
            {
                fei_tostring_func(state, wr, fei_value_asbound_method(value)->method->function);
            }
            break;
        case OBJ_CLASS:
            {
                klass = fei_value_asclass(value);
                fei_writer_appendfmt(wr, "<class '%.*s'>", klass->name->length, klass->name->chars);
            }
            break;
        case OBJ_INSTANCE:
            {
                instance = fei_value_asinstance(value);
                klass = instance->classobject;
                fei_writer_appendfmt(wr, "<instance '%.*s'>", klass->name->length, klass->name->chars);
            }
            break;
        case OBJ_CLOSURE:
            {
                fei_tostring_func(state, wr, fei_value_asclosure(value)->function);
            }
            break;
        case OBJ_FUNCTION:
            {
                fei_tostring_func(state, wr, fei_value_asfunction(value));
            }
            break;
        case OBJ_NATIVE:
            {
                fei_writer_appendfmt(wr, "<function (native)>");
            }
            break;
        case OBJ_STRING:
            {
                fei_tostring_string(state, wr, fei_value_asstring(value), withquot);
            }
            break;
        case OBJ_UPVALUE:
            {
                fei_writer_appendfmt(wr, "<upvalue>");
            }
            break;
        case OBJ_ARRAY:
            {
                fei_tostring_array(state, wr, fei_value_asarray(value), withquot);
            }
            break;
        case OBJ_TABLE:
            {
                fei_tostring_table(state, wr, fei_value_astable(value), withquot);
            }
            break;
        default:
            {
                fei_writer_appendfmt(wr, "<object '%s'>", fei_value_typename(value));
            }
            break;
    }
}

