
#include "fei.h"

FeiObject* fei_object_allocobject(FeiState* state, size_t size, FeiObjType type)
{
    FeiObject* object;
    object = (FeiObject*)fei_gcmem_reallocate(state, NULL, 0, size);
    memset(object, 0, sizeof(FeiObject));
    object->type = type;
    object->ismarked = false;
    // every time an object is allocated, insert to the list
    // insert as the HEAD; the latest one inserted will be at the start
    object->next = state->gcstate.objects;// vm from virtualm.h, with extern
    state->gcstate.objects = object;
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("%p allocate %zd for %d\n", (void*)object, size, type);
    #endif

    return object;
}

// new bound method for classes
ObjBoundMethod* fei_object_makeboundmethod(FeiState* state, FeiValue receiver, FeiObject* method)
{
    ObjBoundMethod* bound;
    state->ocount.cntbound++;
    bound = (ObjBoundMethod*)fei_object_allocobject(state, sizeof(ObjBoundMethod), OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

// create new closure
ObjClosure* fei_object_makeclosure(FeiState* state, ObjFunction* function)
{
    int i;
    ObjClosure* closure;
    ObjUpvalue** upvalues;
    state->ocount.cntclosure++;
    state->ocount.cntupval++;
    // initialize array of upvalue pointers
    // upvalues carry over
    upvalues = (ObjUpvalue**)ALLOCATE(state, sizeof(ObjUpvalue*), function->upvaluecount);
    for(i = 0; i < function->upvaluecount; i++)
    {
        upvalues[i] = NULL;
    }
    closure = (ObjClosure*)fei_object_allocobject(state, sizeof(ObjClosure), OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvaluecount = function->upvaluecount;
    return closure;
}

ObjFunction* fei_object_makefunction(FeiState* state)
{
    ObjFunction* function;
    state->ocount.cntfunction++;
    function = (ObjFunction*)fei_object_allocobject(state, sizeof(ObjFunction), OBJ_FUNCTION);
    function->arity = 0;
    function->upvaluecount = 0;
    function->name = NULL;
    fei_chunk_init(state, &function->chunk);
    return function;
}

// new native function
ObjNative* fei_object_makenativefunc(FeiState* state, NativeFn function)
{
    ObjNative* native;
    state->ocount.cntnative++;
    native = (ObjNative*)fei_object_allocobject(state, sizeof(ObjNative), OBJ_NATIVE);
    native->function = function;
    return native;
}

ObjUpvalue* fei_object_makeupvalue(FeiState* state, FeiValue* slot)
{
    ObjUpvalue* upvalue;
    state->ocount.cntupval++;
    upvalue = (ObjUpvalue*)fei_object_allocobject(state, sizeof(ObjUpvalue), OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->next = NULL;
    upvalue->closed = fei_value_makenull(state);
    return upvalue;
}

