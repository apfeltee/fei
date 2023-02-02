
#include "fei.h"


FeiValArray* fei_valarray_make(FeiState* state)
{
    FeiValArray* array;
    array = ALLOCATE(state, sizeof(FeiValArray), 1);
    array->state = state;
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
    return array;
}

size_t fei_valarray_count(FeiValArray* arr)
{
    return arr->count;
}

FeiValue fei_valarray_get(FeiValArray* arr, int idx)
{
    if((idx < arr->count) && (arr->values != NULL))
    {
        return arr->values[idx];
    }
    return fei_value_makenull(arr->state);
}

void fei_valarray_push(FeiValArray* array, FeiValue value)
{
    int oldcap;
    FeiState* state;
    state = array->state;
    if(array->capacity < array->count + 1)
    {
        oldcap = array->capacity;
        array->capacity = GROW_CAPACITY(oldcap);
        array->values = (FeiValue*)GROW_ARRAY(state, sizeof(FeiValue), array->values, oldcap, array->capacity);
    }
    array->values[array->count] = value;
    array->count++;
}

FeiValue fei_valarray_pop(FeiValArray* array)
{
    FeiValue rv;
    rv = array->values[array->count-1];
    array->count--;
    return rv;
}

void fei_valarray_destroy(FeiValArray* array)
{
    FeiState* state;
    state = array->state;
    FREE_ARRAY(state, sizeof(FeiValue), array->values, array->capacity);
    //fei_valarray_init(state, array);
    FREE(state, sizeof(FeiValArray), array);
}

FeiArray* fei_object_makearray(FeiState* state)
{
    FeiArray* arr;
    state->ocount.cntarray++;
    arr = (FeiArray*)fei_object_allocobject(state, sizeof(FeiObjBoundMethod), OBJ_ARRAY);
    arr->state = state;
    arr->items = fei_valarray_make(state);
    return arr;
}

size_t fei_array_count(FeiArray* arr)
{
    return fei_valarray_count(arr->items);
}

bool fei_array_push(FeiArray* arr, FeiValue val)
{
    fei_valarray_push(arr->items, val);
    return true;
}

FeiValue fei_array_get(FeiArray* arr, size_t idx)
{
    return fei_valarray_get(arr->items, idx);
}

FeiValue fei_array_pop(FeiArray* arr)
{
    return fei_valarray_pop(arr->items);
}

bool fei_array_destroy(FeiArray* arr)
{
    FeiState* state;
    if(arr != NULL)
    {
        state = arr->state;
        fei_valarray_destroy(arr->items);
        fei_gcmem_reallocate(state, arr, sizeof(FeiArray), 0);
        return true;
    }
    return false;
}


