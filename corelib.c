
#include "fei.h"

static FeiValue cfn_clock(FeiState* state, FeiValue instance, int argcount, FeiValue* args)
{
    (void)state;
    (void)instance;
    (void)argcount;
    (void)args;
    return fei_value_makenumber((double)clock() / CLOCKS_PER_SEC);
}

static FeiValue cfn_print(FeiState* state, FeiValue instance, int argc, FeiValue* args)
{
    int i;
    (void)state;
    (void)instance;
    for(i = 0; i < argc; i++)
    {
        fei_value_printvalue(state, state->iowriter_stdout, args[i], false);
        fflush(stdout);
    }
    return fei_value_makenumber(0);
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
    (void)state;
    (void)argc;
    (void)argv;
    return fei_value_makenumber(fei_value_asstring(instance)->length);
}

static FeiValue objfn_string_size(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    (void)state;
    (void)argc;
    (void)argv;
    fprintf(stderr, "in objfn_string_size:instance=[[[");
    fei_value_printvalue(state, state->iowriter_stderr, instance, true);
    fprintf(stderr, "]]]\n");
    return fei_value_makenull();
}

static FeiValue objfn_number_chr(FeiState* state, FeiValue instance, int argc, FeiValue* argv)
{
    char c;
    (void)state;
    (void)argc;
    (void)argv;
    c = fei_value_asnumber(instance);
    return fei_value_makeobject(fei_string_copy(state, &c, 1));
}

void fei_state_setupglobals(FeiState* state)
{
    fei_vm_defnative(state, "clock", cfn_clock);
    fei_vm_defnative(state, "print", cfn_print);
    fei_vm_defnative(state, "println", cfn_println);
}

void fei_state_setupstring(FeiState* state)
{
    fei_class_defmethod(state, state->objstringclass, "length", objfn_string_length, true);
    fei_class_defmethod(state, state->objstringclass, "size", objfn_string_size, false);
}

void fei_state_setupnumber(FeiState* state)
{
    fei_class_defmethod(state, state->objnumberclass, "chr", objfn_number_chr, true);
}
