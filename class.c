
#include "fei.h"

ObjClass* fei_object_makeclass(FeiState* state, ObjString* name)
{
    ObjClass* klassobj;
    state->ocount.cntclass++;
    klassobj = (ObjClass*)fei_object_allocobject(state, sizeof(ObjClass), OBJ_CLASS);
    klassobj->name = name;
    fei_table_initcapacity(state, &klassobj->methods, 4);
    return klassobj;
}

ObjClass* fei_object_makeclass_str(FeiState* state, const char* name)
{
    ObjString* s;
    s = fei_string_copy(state, name, strlen(name));
    return fei_object_makeclass(state, s);
}

// create new class instance
ObjInstance* fei_object_makeinstance(FeiState* state, ObjClass* klassobj)
{
    ObjInstance* instance;
    state->ocount.cntinstance++;
    instance = (ObjInstance*)fei_object_allocobject(state, sizeof(ObjInstance), OBJ_INSTANCE);
    instance->classobject = klassobj;
    fei_table_initempty(state, &instance->fields);
    //fei_table_initcapacity(state, &instance->fields, 4);
    return instance;
}


bool fei_class_invokemethod(FeiState* state, ObjClass* klassobj, ObjString* name, int argcount)
{
    FeiValue method;
    if(!fei_table_get(state, &klassobj->methods, name, &method))
    {
        fei_vm_raiseruntimeerror(state, "cannot invoke undefined property '%s'", name->chars);
        return false;
    }
    return fei_vm_callclosure(state, fei_value_asclosure(method), argcount);
}

// bind method and wrap it in a new ObjBoundMethod
bool fei_class_bindmethod(FeiState* state, ObjClass* klassobj, ObjString* name, FeiValue val, bool isfield, bool force)
{
    bool mustdef;
    FeiValue method;
    Table* tab;
    ObjBoundMethod* bound;
    mustdef = false;
    tab = &klassobj->methods;

    // get method from table and bind it
    if(!fei_table_get(state, tab, name, &method))
    {
        mustdef = true;
        if(!force)
        {
            // if method not found
            fei_vm_raiseruntimeerror(state, "cannot bind undefined property '%s'", name->chars);
            return false;
        }
    }
    if(mustdef)
    {
        fei_table_set(state, tab, name, val);
        method = val;
    }
    if(fei_value_isboundmethod(method))
    {
        // wrap method in a new ObjBoundMethodd
        bound = fei_object_makeboundmethod(state, val, (FeiObject*)fei_value_asclosure(method));
    }
    else
    {
        bound = fei_object_makeboundmethod(state, val, (FeiObject*)fei_value_asfunction(method));
    }
    // pop the class instance
    fei_vm_stackpush(state, fei_value_makeobject(state, bound));
    return true;
}

bool fei_class_setmethod(FeiState* state, ObjClass* klass, ObjString* name, FeiValue method)
{
    return fei_table_set(state, &klass->methods, name, method);
}

void fei_class_inherit(FeiState* state, ObjClass* base, ObjClass* inheritme)
{
    fei_table_mergefrom(state, &inheritme->methods, &base->methods);
}

/*
* unconditionally set an item on a ObjClass.
* if $isfield is true, then set as a field (a sort-of function, that is not called per-se. think javascript .length)
*/
bool fei_class_defmethod(FeiState* state, ObjClass* klassobj, const char* strname, NativeFn fn, bool isfield)
{
    FeiValue ofv;
    ObjString* name;
    ObjNative* ofn;
    name = fei_string_copy(state, strname, strlen(strname));
    ofn = fei_object_makenativefunc(state, fn);
    ofv = fei_value_makeobject(state, ofn);
    fei_vm_stackpush(state, ofv);
    return fei_class_bindmethod(state, klassobj, name, ofv, isfield, true);
}

