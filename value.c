
#include <math.h>
#include "fei.h"

const char* fei_object_typename(FeiObject* v)
{
    switch(v->type)
    {
        case OBJ_BOUND_METHOD:
            {
                return "boundmethod";
            }
            break;
        case OBJ_INSTANCE:
            {
                return "instance";
            }
            break;
        case OBJ_CLASS:
            {
                return "class";
            }
            break;
        case OBJ_CLOSURE:
            {
                return "closure";
            }
            break;
        case OBJ_FUNCTION:
            {
                return "scriptfunction";
            }
            break;
        case OBJ_NATIVE:
            {
                return "nativefunction";
            }
            break;
        case OBJ_STRING:
            {
                return "string";
            }
            break;
        case OBJ_UPVALUE:
            {
                return "upvalue";
            }
            break;
        case OBJ_ARRAY:
            {
                return "array";
            }
            break;
        default:
            {
            }
            break;
    }
    return "unknown";
}

const char* fei_value_typename(FeiValue v)
{
    switch(v.type)
    {
        case VAL_UNDEF:
            {
                return "undef";
            }
            break;
        case VAL_BOOL:
            {
                return "bool";
            }
            break;
        case VAL_NULL:
            {
                return "null";
            }
            break;
        case VAL_NUMBER:
            {
                return "number";
            }
            break;
        case VAL_OBJ:
            {
                return fei_object_typename(v.as.valobjptr);
            }
            break;
        default:
            {
            }
            break;
    }
    return "unknown";
}

// comparison function used in VM run()
// used in ALL types of data(num, string, bools)
bool fei_value_compare(FeiState* state, FeiValue a, FeiValue b)
{
    (void)state;
    if(a.type != b.type)
    {
        return false;
    }
    switch(a.type)
    {
        case VAL_BOOL:
            {
                return fei_value_asbool(a) == fei_value_asbool(b);
            }
            break;
        case VAL_NUMBER:
            {
                if(a.isfixednumber && b.isfixednumber)
                {
                    return fei_value_asfixednumber(a) == fei_value_asfixednumber(b);
                }
                return fei_value_asfloatnumber(a) == fei_value_asfloatnumber(b);
            }
            break;
        case VAL_NULL:
            {
                // true for all nulls
                return true;
            }
            break;
        case VAL_OBJ:
            {
                // already interned, occupies the same address
                return fei_value_asobject(a) == fei_value_asobject(b);
            }
            break;
        default:
            {
            }
            break;
    }
    return false;
}
