
#include "fei.h"

static FeiValue cfn_clock(FeiState* state, FeiValue instance, int argcount, FeiValue* args)
{
    (void)state;
    (void)instance;
    (void)argcount;
    (void)args;
    return fei_value_makefloatnumber(state, (double)clock() / CLOCKS_PER_SEC);
}

static FeiValue cfn_print(FeiState* state, FeiValue instance, int argc, FeiValue* args)
{
    int i;
    (void)state;
    (void)instance;
    for(i = 0; i < argc; i++)
    {
        fei_tostring_value(state, state->iowriter_stdout, args[i], false);
        fflush(stdout);
    }
    return fei_value_makenull(state);
}


static FeiValue cfn_maketable(FeiState* state, FeiValue instance, int argc, FeiValue* args)
{
    (void)state;
    (void)instance;
    return fei_value_makeobject(state, fei_table_make(state));
}

static FeiValue cfn_println(FeiState* state, FeiValue instance, int argc, FeiValue* args)
{
    FeiValue r;
    r = cfn_print(state, instance, argc, args);
    fprintf(stdout, "\n");
    fflush(stdout);
    return r;
}

static FeiValue objfn_string_length(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    FeiString* s;
    (void)state;
    (void)argc;
    (void)argv;
    s = fei_value_asstring(instance);
    return fei_value_makefixednumber(state, s->length);
}

static FeiValue objfn_string_size(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    (void)state;
    (void)argc;
    (void)argv;
    fprintf(stderr, "in objfn_string_size:instance=[[[");
    fei_tostring_value(state, state->iowriter_stderr, instance, true);
    fprintf(stderr, "]]]\n");
    return fei_value_makenull(state);
}

static FeiValue objfn_string_ord(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    FeiString* s;
    (void)state;
    (void)argc;
    (void)argv;
    s = fei_value_asstring(instance);
    return fei_value_makefixednumber(state, s->chars[0]);
}

static FeiValue objfn_number_chr(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    char c;
    (void)state;
    (void)argc;
    (void)argv;
    c = fei_value_asnumber(instance);
    return fei_value_makeobject(state, fei_string_copy(state, &c, 1));
}

static FeiValue objfn_array_length(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    FeiArray* arr;
    (void)state;
    (void)argc;
    (void)argv;
    arr = fei_value_asarray(instance);
    return fei_value_makefixednumber(state, fei_array_count(arr));
}

static FeiValue objfn_array_push(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    int i;
    FeiArray* arr;
    (void)state;
    (void)argc;
    (void)argv;
    arr = fei_value_asarray(instance);
    for(i=0; i<argc; i++)
    {
        fei_valarray_push(arr->items, argv[i]);
    }
    return fei_value_makefixednumber(state, i);
}

static FeiValue objfn_array_pop(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    FeiArray* arr;
    (void)state;
    (void)argc;
    (void)argv;
    arr = fei_value_asarray(instance);
    if(fei_array_count(arr) > 0)
    {
        return fei_array_pop(arr);
    }
    return fei_value_makenull(state);
}

static FeiValue objfn_array_join(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    int i;
    int alen;
    int seplen;
    const char* sepstr;
    FeiValue val;
    FeiString* sepobj;
    FeiArray* arr;
    FeiWriter* wr;
    (void)state;
    (void)argc;
    (void)argv;
    sepstr = "";
    seplen = 0;
    arr = fei_value_asarray(instance);
    wr = fei_writer_initstring(state);
    if(argc > 0 && fei_value_isstring(argv[0]))
    {
        sepobj = fei_value_asstring(argv[0]);
        sepstr = sepobj->chars;
        seplen = sepobj->length;
    }
    alen = fei_array_count(arr);
    for(i=0; i<alen; i++)
    {
        val = fei_array_get(arr, i);
        fei_tostring_value(state, wr, val, false);
        if((i+1) < alen)
        {
            fei_writer_appendstringlen(wr, sepstr, seplen);
        }
    }
    val = fei_value_makeobject(state, wr->string);
    fei_writer_destroy(wr, false);
    return val;
}

static FeiValue objfn_array_map(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    int i;
    int alen;
    int seplen;
    const char* sepstr;
    FeiValue val;
    FeiString* sepobj;
    FeiArray* arr;
    FeiWriter* wr;
    (void)state;
    (void)argc;
    (void)argv;
    sepstr = "";
    seplen = 0;
    arr = fei_value_asarray(instance);
    wr = fei_writer_initstring(state);
    if(argc > 0 && fei_value_isstring(argv[0]))
    {
        sepobj = fei_value_asstring(argv[0]);
        sepstr = sepobj->chars;
        seplen = sepobj->length;
    }
    alen = fei_array_count(arr);
    for(i=0; i<alen; i++)
    {
        val = fei_array_get(arr, i);
        fei_tostring_value(state, wr, val, false);
        if((i+1) < alen)
        {
            fei_writer_appendstringlen(wr, sepstr, seplen);
        }
    }
    val = fei_value_makeobject(state, wr->string);
    fei_writer_destroy(wr, false);
    return val;
}

void fei_state_setupglobals(FeiState* state)
{
    fei_vm_defnative(state, "clock", cfn_clock);
    fei_vm_defnative(state, "print", cfn_print);
    fei_vm_defnative(state, "println", cfn_println);
    fei_vm_defnative(state, "Table", cfn_maketable);
}

void fei_state_setupstring(FeiState* state)
{
    fei_class_defmethod(state, state->objstring.classobj, "length", objfn_string_length, true);
    fei_class_defmethod(state, state->objstring.classobj, "size", objfn_string_size, false);
    fei_class_defmethod(state, state->objstring.classobj, "ord", objfn_string_ord, true);
}

void fei_state_setupnumber(FeiState* state)
{
    fei_class_defmethod(state, state->objnumber.classobj, "chr", objfn_number_chr, true);
}

void fei_state_setuparray(FeiState* state)
{

    fei_class_defmethod(state, state->objarray.classobj, "length", objfn_array_length, true);
    fei_class_defmethod(state, state->objarray.classobj, "push", objfn_array_push, false);
    fei_class_defmethod(state, state->objarray.classobj, "pop", objfn_array_pop, false);
    fei_class_defmethod(state, state->objarray.classobj, "join", objfn_array_join, false);
    fei_class_defmethod(state, state->objarray.classobj, "map", objfn_array_map, false);
}

