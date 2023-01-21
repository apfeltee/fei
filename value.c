
#include "fei.h"

// from VALUE STRUCT to RAW  C nicely used in printing
// also for comparisons -> use values ALREADY CONVERTED to FeiValue struct union to raw C
bool fei_value_asbool(FeiValue v)
{
    return v.as.valbool;
}

double fei_value_asfloatnumber(FeiValue v)
{
    if(v.type != VAL_NUMBER)
    {
        return 0;
    }
    if(v.isfixednumber)
    {
        return v.as.valfixednum;
    }
    return v.as.valfloatnum;
}

int64_t fei_value_asfixednumber(FeiValue v)
{
    if(v.type != VAL_NUMBER)
    {
        return 0;
    }
    if(!v.isfixednumber)
    {
        return v.as.valfloatnum;
    }
    return v.as.valfixednum;
}

FeiObject* fei_value_asobj(FeiValue v)
{
    return v.as.valobjptr;
}

FeiObjType OBJ_TYPE(FeiValue v)
{
    return fei_value_asobj(v)->type;
}

bool fei_object_istype(FeiValue value, FeiObjType type)
{
    return fei_value_isobj(value) && fei_value_asobj(value)->type == type;
}

bool fei_value_isboundmethod(FeiValue v)
{
    return fei_object_istype(v, OBJ_BOUND_METHOD);
}

bool fei_value_isclass(FeiValue v)
{
    return fei_object_istype(v, OBJ_CLASS);
}

bool fei_value_isfunction(FeiValue v)
{
    return fei_object_istype(v, OBJ_FUNCTION);
}

bool fei_value_isinstance(FeiValue v)
{
    return fei_object_istype(v, OBJ_INSTANCE);
}

bool fei_value_isnative(FeiValue v)
{
    return fei_object_istype(v, OBJ_NATIVE);
}

bool fei_value_isstring(FeiValue v)
{
    return fei_object_istype(v, OBJ_STRING);
}

bool fei_value_isclosure(FeiValue v)
{
    return fei_object_istype(v, OBJ_CLOSURE);
}

bool fei_value_isobject(FeiValue v)
{
    return v.type == VAL_OBJ;
}

// comparison for OP_NOT
bool fei_value_isfalsey(FeiState* state, FeiValue value)
{
    bool test;
    (void)state;
    // return true if value is the null type or if it is a false bool type
    test = (
        fei_value_isnull(value) ||
        (fei_value_isbool(value) && !fei_value_asbool(value))
    );
    return test;
}

int fei_value_gettype(FeiValue v)
{
    if(fei_value_isobject(v))
    {
        return v.as.valobjptr->type;
    }
    return v.type;
}

ObjBoundMethod* fei_value_asbound_method(FeiValue v)
{
    return (ObjBoundMethod*)fei_value_asobj(v);
};

ObjClass* fei_value_asclass(FeiValue v)
{
    return (ObjClass*)fei_value_asobj(v);
}

ObjInstance* fei_value_asinstance(FeiValue v)
{
    return (ObjInstance*)fei_value_asobj(v);
}

ObjClosure* fei_value_asclosure(FeiValue v)
{
    return (ObjClosure*)fei_value_asobj(v);
}

ObjString* fei_value_asstring(FeiValue v)
{
    return (ObjString*)fei_value_asobj(v);
}

char* fei_value_ascstring(FeiValue v)
{
    return fei_value_asstring(v)->chars;
}

ObjFunction* fei_value_asfunction(FeiValue v)
{
    return (ObjFunction*)fei_value_asobj(v);
}

NativeFn fei_value_asnative(FeiValue v)
{
    return ((ObjNative*)fei_value_asobj(v))->function;
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
                else if(!a.isfixednumber && !b.isfixednumber)
                {
                    return fei_value_asfloatnumber(a) == fei_value_asfloatnumber(b);
                }
                return fei_value_asnumber(a) == fei_value_asnumber(b);
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
                return fei_value_asobj(a) == fei_value_asobj(b);
            }
            break;
        default:
            {
            }
            break;
    }
    return false;
}


