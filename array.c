
#include "fei.h"


void fei_valarray_init(FeiState* state, FeiValArray* array)
{
    (void)state;
    array->count = 0;
    array->capacity = 0;
    array->values = NULL;
}

size_t fei_valarray_count(FeiValArray* arr)
{
    return arr->count;
}

FeiValue fei_valarray_get(FeiState* state, FeiValArray* arr, int idx)
{
    (void)state;
    return arr->values[idx];
}

void fei_valarray_push(FeiState* state, FeiValArray* array, FeiValue value)
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

FeiValue fei_valarray_pop(FeiState* state, FeiValArray* array)
{
    FeiValue rv;
    rv = array->values[array->count-1];
    array->count--;
    return rv;
}

void fei_valarray_destroy(FeiState* state, FeiValArray* array)
{
    FREE_ARRAY(state, sizeof(FeiValue), array->values, array->capacity);
    fei_valarray_init(state, array);
}

FeiArray* fei_object_makearray(FeiState* state)
{
    FeiArray* arr;
    state->ocount.cntarray++;
    arr = (FeiArray*)fei_object_allocobject(state, sizeof(FeiObjBoundMethod), OBJ_ARRAY);
    fei_valarray_init(state, &arr->items);
    return arr;
}

size_t fei_array_count(FeiArray* arr)
{
    return fei_valarray_count(&arr->items);
}

bool fei_array_push(FeiState* state, FeiArray* arr, FeiValue val)
{
    fei_valarray_push(state, &arr->items, val);
    return true;
}

FeiValue fei_array_get(FeiState* state, FeiArray* arr, size_t idx)
{
    return fei_valarray_get(state, &arr->items, idx);
}

FeiValue fei_array_pop(FeiState* state, FeiArray* arr)
{
    return fei_valarray_pop(state, &arr->items);
}

bool fei_array_destroy(FeiState* state, FeiArray* arr)
{
    if(arr != NULL)
    {
        fei_valarray_destroy(state, &arr->items);
        fei_gcmem_reallocate(state, arr, sizeof(FeiArray), 0);
        return true;
    }
    return false;
}


