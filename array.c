
#include "fei.h"


void fei_valarray_init(FeiState* state, ValArray* array)
{
    (void)state;
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}

size_t fei_valarray_count(ValArray* arr)
{
    return arr->count;
}

FeiValue fei_valarray_get(FeiState* state, ValArray* arr, int idx)
{
    (void)state;
    return arr->values[idx];
}

void fei_valarray_push(FeiState* state, ValArray* array, FeiValue value)
{
    int oldcap;
    if(array->capacity < array->count + 1)
    {
        oldcap = array->capacity;
        array->capacity = GROW_CAPACITY(oldcap);
        array->values = (FeiValue*)GROW_ARRAY(state, sizeof(FeiValue), array->values, oldcap, array->capacity);
    }
    array->values[array->count] = value;
    array->count++;
}

FeiValue fei_valarray_pop(FeiState* state, ValArray* array)
{
    FeiValue rv;
    rv = array->values[array->count-1];
    array->count--;
    return rv;
}

void fei_valarray_destroy(FeiState* state, ValArray* array)
{
    FREE_ARRAY(state, sizeof(FeiValue), array->values, array->capacity);
    fei_valarray_init(state, array);
}

ObjArray* fei_object_makearray(FeiState* state)
{
    ObjArray* arr;
    state->ocount.cntarray++;
    arr = (ObjArray*)fei_object_allocobject(state, sizeof(ObjBoundMethod), OBJ_ARRAY);
    fei_valarray_init(state, &arr->items);
    return arr;
}

size_t fei_array_count(ObjArray* arr)
{
    return fei_valarray_count(&arr->items);
}

bool fei_array_push(FeiState* state, ObjArray* arr, FeiValue val)
{
    fei_valarray_push(state, &arr->items, val);
    return true;
}

FeiValue fei_array_pop(FeiState* state, ObjArray* arr)
{
    return fei_valarray_pop(state, &arr->items);
}

bool fei_array_destroy(FeiState* state, ObjArray* arr)
{
    if(arr != NULL)
    {
        fei_valarray_destroy(state, &arr->items);
        fei_gcmem_reallocate(state, arr, sizeof(ObjArray), 0);
        return true;
    }
    return false;
}


