
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

bool fei_valcompare_string(FeiState* state, FeiString* stra, FeiString* strb)
{
    (void)state;
    if(stra->length == strb->length)
    {
        return (memcmp(stra->chars, strb->chars, stra->length) == 0);
    }
    return false;
}

bool fei_valcompare_array(FeiState* state, FeiArray* arra, FeiArray* arrb)
{
    int i;
    int lena;
    int lenb;
    bool b;
    FeiValue vala;
    FeiValue valb;
    lena = fei_array_count(arra);
    lenb = fei_array_count(arrb);
    if(lena == lenb)
    {
        for(i=0; i<lena; i++)
        {
            vala = fei_array_get(arra, i);
            valb = fei_array_get(arrb, i);
            //fei_vm_dumpval(state, vala, "valcompare_array(%d):vala", i);
            //fei_vm_dumpval(state, valb, "valcompare_array(%d):valb", i);
            b = fei_value_compare(state, vala, valb);
            if(!b)
            {
                return false;
            }
        }
        return true;
    }
    return false;
}

bool fei_valcompare_object(FeiState* state, FeiObject* oba, FeiObject* obb)
{
    if(oba->type == obb->type)
    {
        switch(oba->type)
        {
            case OBJ_ARRAY:
                {
                    return fei_valcompare_array(state, (FeiArray*)oba, (FeiArray*)obb);
                }
                break;
            case OBJ_STRING:
                {
                    return fei_valcompare_string(state, (FeiString*)oba, (FeiString*)obb);
                }
                break;
            default:
                {
                    fprintf(stderr, "missing comparison for object type '%s'\n", fei_value_typename(fei_value_makeobject(state, oba)));
                }
                break;
        }
    }
    // already interned, occupies the same address
    return oba == obb;
}

// comparison function used in VM run()
// used in ALL types of data(num, string, bools)
bool fei_value_compare(FeiState* state, FeiValue a, FeiValue b)
{
    FeiObject* oba;
    FeiObject* obb;
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
                oba = fei_value_asobject(a);
                obb = fei_value_asobject(b);
                return fei_valcompare_object(state, oba, obb);
            }
            break;
        default:
            {
            }
            break;
    }
    return false;
}
