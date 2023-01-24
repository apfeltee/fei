
#include "fei.h"

// A void pointer is a pointer that has no associated data type with it.
// A void pointer can hold address of any type and can be typcasted to any type.
void* fei_gcmem_reallocate(FeiState* state, void* pointer, size_t oldsize, size_t newsize)
{
    void* memdata;
    // self adjusting heap for garbage collection
    state->gcstate.bytesallocated += newsize - oldsize;
    // when allocating NEW memory, not when freeing as collecgarbage will cal void* reallocate itself
    if(newsize > oldsize)
    {
        #if defined(DEBUG_STRESS_GC) && (DEBUG_STRESS_GC == 1)
            fei_gcmem_collectgarbage(state);
        #endif
        // run collecter if bytesallocated is above threshold
        if(state->gcstate.bytesallocated > state->gcstate.nextgc)
        {
            fei_gcmem_collectgarbage(state);
        }
    }
    if(newsize == 0)
    {
        free(pointer);
        return NULL;
    }
    memdata = NULL;
    if(pointer == NULL)
    {
        memdata = malloc(newsize);
    }
    else
    {
        memdata = realloc(pointer, newsize);
    }
    if(memdata == NULL)
    {
        fprintf(stderr, "internal error: failed to realloc to %d bytes!\n", (int)newsize);
        exit(1);
    }
    if(pointer == NULL)
    {
        memset(memdata, 0, newsize);
    }
    return memdata;
}


// you can pass in a'lower' struct pointer, in this case FeiObject*, and get the higher level which is ObjFunction
void fei_gcmem_freeobject(FeiState* state, FeiObject* object)
{
    ObjClass* klassobj;
    ObjInstance* instance;
    ObjClosure* closure;
    ObjFunction* function;
    ObjString* string;

    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("%p free type %d\n", (void*)object, object->type);
    #endif
    switch(object->type)
    {
        case OBJ_ARRAY:
            {
                fei_array_destroy(state, (ObjArray*)object);
            }
            break;
        case OBJ_BOUND_METHOD:
            {
                FREE(state, sizeof(ObjBoundMethod), object);
            }
            break;
        case OBJ_CLASS:
            {
                klassobj = (ObjClass*)object;
                fei_table_destroy(state, &klassobj->methods);
                fei_table_destroy(state, &klassobj->fieldlike);
                FREE(state, sizeof(ObjClass), object);
            }
            break;
        case OBJ_INSTANCE:
            {
                instance = (ObjInstance*)object;
                fei_table_destroy(state, &instance->fields);
                FREE(state, sizeof(ObjInstance), object);
            }
            break;
        case OBJ_CLOSURE:
            {
                // free upvalues
                closure = (ObjClosure*)object;
                FREE_ARRAY(state, sizeof(ObjUpvalue*), closure->upvalues, closure->upvaluecount);
                // only free the closure, not the function itself
                FREE(state, sizeof(ObjClosure), object);
            }
            break;
        case OBJ_FUNCTION:
            {
                function = (ObjFunction*)object;
                fei_chunk_destroy(state, &function->chunk);
                FREE(state, sizeof(ObjFunction), object);
            }
            break;
        case OBJ_NATIVE:
            {
                FREE(state, sizeof(ObjNative), object);
            }
            break;
        case OBJ_STRING:
            {
                string = (ObjString*)object;
                fei_string_destroy(state, string);
            }
            break;
        case OBJ_UPVALUE:
            {
                FREE(state, sizeof(ObjUpvalue), object);
            }
            break;
    }
}

/*		garbage collection		 */

void fei_gcmem_markobject(FeiState* state, FeiObject* object)
{
    if(object == NULL)
    {
        return;
    }
    if(object->ismarked)
    {
        return;
    }
    object->ismarked = true;
    // create a worklist of grayobjects to traverse later, use a stack to implement it
    // if need more space, allocate
    if(state->gcstate.graycapacity < state->gcstate.graycount + 1)
    {
        state->gcstate.graycapacity = GROW_CAPACITY(state->gcstate.graycapacity);
        // use native realloc here
        state->gcstate.graystack = (FeiObject**)realloc(state->gcstate.graystack, sizeof(FeiObject*) * state->gcstate.graycapacity);
    }
    if(state->gcstate.graystack == NULL)
    {
        fprintf(stderr, "internal error: failed to allocate memory for gray stack\n");
        exit(1);
    }
    // add the 'gray' object to the working list
    state->gcstate.graystack[state->gcstate.graycount++] = object;
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        fprintf(stderr, "%p marked ", (void*)object);
        // you cant print first class objects, like how you would print in the actual repl
        fei_value_printvalue(state, state->iowriter_stderr, fei_value_makeobject(state, object), true);
        fprintf(stderr, "\n");
    #endif
}

void fei_gcmem_markvalue(FeiState* state, FeiValue value)
{
    // if value is not first class Objtype return
    if(!fei_value_isobject(value))
    {
        return;
    }
    fei_gcmem_markobject(state, fei_value_asobject(value));
}

// marking array of values/constants of a function, used in fei_gcmem_blackenobject, case OBJ_FUNCTION
void fei_gcmem_markarray(FeiState* state, ValArray* array)
{
    size_t i;
    for(i = 0; i < fei_valarray_count(array); i++)
    {
        fei_gcmem_markvalue(state, fei_valarray_get(state, array, i));// mark each FeiValue in the array
    }
}

void fei_gcmem_markroots(FeiState* state)
{
    int i;
    FeiValue* slot;
    ObjUpvalue* upvalue;
    // assiging a pointer to a full array means assigning the pointer to the FIRST element of that array
    for(slot = state->vmstate.stackvalues; slot < state->vmstate.stacktop; slot++)// walk through all values/slots in the FeiValue* array
    {
        fei_gcmem_markvalue(state, *slot);
    }
    // mark closures
    for(i = 0; i < state->vmstate.framecount; i++)
    {
        fei_gcmem_markobject(state, (FeiObject*)fei_vm_frameget(state, i)->closure);
    }
    // mark upvalues, walk through the linked list of upvalues
    for(upvalue = state->vmstate.openupvalues; upvalue != NULL; upvalue = upvalue->next)
    {
        fei_gcmem_markobject(state, (FeiObject*)upvalue);
    }
    // mark global variables, belongs in the VM/hashtable
    fei_table_mark(state, &state->vmstate.globals);
    // compiler also grabs memory
    fei_compiler_markroots(state);
    fei_gcmem_markobject(state, (FeiObject*)state->vmstate.initstring);
}

// actual tracing of each gray object and marking it black
void fei_gcmem_blackenobject(FeiState* state, FeiObject* object)
{
    int i;
    ObjArray* arr;
    ObjBoundMethod* bound;
    ObjClass* klassobj;
    ObjInstance* instance;
    ObjFunction* function;
    ObjClosure* closure;
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        fprintf(stderr, "%p blackened ", (void*)object);
        fei_value_printvalue(state, state->iowriter_stderr, fei_value_makeobject(state, object), true);
        fprintf(stderr, "\n");
    #endif
    switch(object->type)
    {
        case OBJ_ARRAY:
            {
                arr = (ObjArray*)object;
                fei_gcmem_markarray(state, &arr->items);
            }
            break;
        case OBJ_BOUND_METHOD:
            {
                bound = (ObjBoundMethod*)object;
                fei_gcmem_markvalue(state, bound->receiver);
                fei_gcmem_markobject(state, (FeiObject*)bound->method);
            }
            break;
        case OBJ_UPVALUE:
            {
                fei_gcmem_markvalue(state, ((ObjUpvalue*)object)->closed);
            }
            break;
        case OBJ_FUNCTION:
            {
                // mark the name and its value array of constants
                // you can get the coressponding 'higher' object type from a lower derivation struct in C using (higher*)lower
                function = (ObjFunction*)object;
                fei_gcmem_markobject(state, (FeiObject*)function->name);// mark its name, an ObjString type
                fei_gcmem_markarray(state, &function->chunk.constants);// mark value array of chunk constants, pass it in AS A POINTER using &
            }
            break;
        case OBJ_CLOSURE:
            {
                // mark the function and all of the closure's upvalues
                closure = (ObjClosure*)object;
                fei_gcmem_markobject(state, (FeiObject*)closure->function);
                for(i = 0; i < closure->upvaluecount; i++)
                {
                    fei_gcmem_markobject(state, (FeiObject*)closure->upvalues[i]);
                }
            }
            break;
        case OBJ_CLASS:
            {
                klassobj = (ObjClass*)object;
                fei_gcmem_markobject(state, (FeiObject*)klassobj->name);
                fei_table_mark(state, &klassobj->methods);
            }
            break;
        case OBJ_INSTANCE:
            {
                instance = (ObjInstance*)object;
                fei_gcmem_markobject(state, (FeiObject*)instance->classobject);
                fei_table_mark(state, &instance->fields);
            }
            break;
        // these two objects contain NO OUTGOING REFERENCES there is nothing to traverse
        case OBJ_NATIVE:
        case OBJ_STRING:
            {
            }
            break;
    }
}

// traversing the gray stack work list
void fei_gcmem_tracerefs(FeiState* state)
{
    FeiObject* object;
    while(state->gcstate.graycount > 0)
    {
        // pop FeiObject* (pointer) from the stack
        // note how -- is the prefix; subtract first then use it as an index
        // --state->gcstate.graycount already decreases its count, hence everything is already 'popped'
        object = state->gcstate.graystack[--state->gcstate.graycount];
        fei_gcmem_blackenobject(state, object);
    }
}

// sweeping all unreachable values
void fei_gcmem_sweep(FeiState* state)
{
    FeiObject* object;
    FeiObject* previous;
    FeiObject* unreached;
    previous = NULL;
    // linked intrusive list of Objects in the VM
    object = state->gcstate.objects;
    while(object != NULL)
    {
        // object marked, do not free
        if(object->ismarked)
        {
            // reset the marking to 'white'
            object->ismarked = false;
            previous = object;
            object = object->next;
        }
        // free the unreachable object
        else
        {
            unreached = object;
            object = object->next;
            // link to previous object if previous not null
            if(previous != NULL)
            {
                previous->next = object;
            }
            // if not set the next as the start of the list
            else
            {
                state->gcstate.objects = object;
            }
            // method that actually frees the object
            fei_gcmem_freeobject(state, unreached);
        }
    }
}

void fei_gcmem_collectgarbage(FeiState* state)
{
    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("--Garbage Collection Begin\n");
        size_t before = state->gcstate.bytesallocated;
    #endif
    // function to start traversing the graph, from the root and marking them
    fei_gcmem_markroots(state);
    // tracing each gray marked object
    fei_gcmem_tracerefs(state);
    // removing intern strings, BEFORE the sweep so the pointers can still access its memory
    // function defined in hahst.c
    fei_table_removeunreachable(state, &state->vmstate.strings);
    // free all unreachable roots
    fei_gcmem_sweep(state);
    // adjust size of threshold
    state->gcstate.nextgc = state->gcstate.bytesallocated * GC_HEAP_GROW_FACTOR;

    #if defined(DEBUG_LOG_GC) && (DEBUG_LOG_GC == 1)
        printf("--Garbage Collection End\n");
        printf("	collected %zd bytes (from %zd to %zd) next at %zd\n", before - state->gcstate.bytesallocated, before, state->gcstate.bytesallocated, state->gcstate.nextgc);
    #endif
}


/*		end of garbage collection		 */
void fei_gcmem_freeobjects(FeiState* state)// free from VM
{
    FeiObject* next;
    FeiObject* object;
    object = state->gcstate.objects;
    // free from the whole list
    while(object != NULL)
    {
        next = object->next;
        fei_gcmem_freeobject(state, object);
        object = next;
    }
    free(state->gcstate.graystack);// free gray marked obj stack used for garbage collection
}



