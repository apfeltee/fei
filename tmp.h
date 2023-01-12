void fei_valarray_init(State* state, ValArray* array)
{
    (void)state;
    array->list = da_make(state, array->list, 0, sizeof(Value));
}

void fei_valarray_push(State* state, ValArray* array, Value value)
{
    da_push(state, array->list, &value);
}

void fei_valarray_destroy(State* state, ValArray* array)
{
    da_destroy(state, array->list);
    fei_valarray_init(state, array);
}