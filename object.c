
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
FeiObjBoundMethod* fei_object_makeboundmethod(FeiState* state, FeiValue receiver, FeiObject* method)
{
    FeiObjBoundMethod* bound;
    state->ocount.cntbound++;
    bound = (FeiObjBoundMethod*)fei_object_allocobject(state, sizeof(FeiObjBoundMethod), OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

// create new closure
FeiObjClosure* fei_object_makeclosure(FeiState* state, FeiObjFunction* function)
{
    int i;
    FeiObjClosure* closure;
    FeiObjUpvalue** upvalues;
    state->ocount.cntclosure++;
    state->ocount.cntupval++;
    // initialize array of upvalue pointers
    // upvalues carry over
    upvalues = (FeiObjUpvalue**)ALLOCATE(state, sizeof(FeiObjUpvalue*), function->upvaluecount);
    for(i = 0; i < function->upvaluecount; i++)
    {
        upvalues[i] = NULL;
    }
    closure = (FeiObjClosure*)fei_object_allocobject(state, sizeof(FeiObjClosure), OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvaluecount = function->upvaluecount;
    return closure;
}

FeiObjFunction* fei_object_makefunction(FeiState* state)
{
    FeiObjFunction* function;
    state->ocount.cntfunction++;
    function = (FeiObjFunction*)fei_object_allocobject(state, sizeof(FeiObjFunction), OBJ_FUNCTION);
    function->arity = 0;
    function->upvaluecount = 0;
    function->name = NULL;
    fei_chunk_init(state, &function->chunk);
    return function;
}

// new native function
FeiObjNative* fei_object_makenativefunc(FeiState* state, FeiNativeFn function)
{
    FeiObjNative* native;
    state->ocount.cntnative++;
    native = (FeiObjNative*)fei_object_allocobject(state, sizeof(FeiObjNative), OBJ_NATIVE);
    native->function = function;
    return native;
}

FeiObjUpvalue* fei_object_makeupvalue(FeiState* state, FeiValue* slot)
{
    FeiObjUpvalue* upvalue;
    state->ocount.cntupval++;
    upvalue = (FeiObjUpvalue*)fei_object_allocobject(state, sizeof(FeiObjUpvalue), OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->next = NULL;
    upvalue->closed = fei_value_makenull(state);
    return upvalue;
}

