
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

void fei_valarray_destroy(FeiState* state, ValArray* array)
{
    FREE_ARRAY(state, sizeof(FeiValue), array->values, array->capacity);
    fei_valarray_init(state, array);
}


