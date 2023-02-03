
#include "fei.h"

#define FEI_CFG_NEXTGC (1024 * 1024)
//#define FEI_CFG_NEXTGC (1024)


void fei_state_makeprimitive(FeiState* state, FeiPrimitive* pm, const char* name)
{
    pm->classobj = fei_object_makeclass_str(state, name);
    pm->instobj = fei_object_makeinstance(state, pm->classobj);
}

void fei_state_setdefaultconfig(FeiState* state)
{
    state->config.traceinstructions = false;
    state->config.tracestackvalues = false;
    state->config.printcreatedobjcount = false;
}

FeiState* fei_state_init()
{
    FeiState* state;
    FeiVMFrame** dfa;
    dfa = NULL;
    state = (FeiState*)malloc(sizeof(FeiState));
    memset(state, 0, sizeof(FeiState));
    fei_state_setdefaultconfig(state);
    state->aststate.compiler = NULL;
    state->aststate.classcompiler = NULL;
    {
        state->gcstate.objects = NULL;
        // initializing gray marked obj stack for garbage collection
        state->gcstate.graycapacity = 0;
        state->gcstate.graycount = 0;
        state->gcstate.graystack = NULL;

        // self adjusting heap to control frequency of GC
        state->gcstate.bytesallocated = 0;
        state->gcstate.nextgc = FEI_CFG_NEXTGC;
    }
    state->vmstate.globals = fei_valtable_make(state, 32);
    state->vmstate.strings = fei_valtable_make(state, 32);
    {
        // initializing the FeiValue stack, also initializing the callframe count
        fei_vm_resetstack(state);
        dfa = (FeiVMFrame**)da_make(dfa, 0, sizeof(FeiVMFrame));
        state->vmstate.frameobjects = dfa;
        // init initalizer string
        state->vmstate.initstring = fei_string_copy(state, "init", 4);
    }
    fei_state_setupglobals(state);
    state->iowriter_stdout = fei_writer_initfile(state, stdout, false);
    state->iowriter_stderr = fei_writer_initfile(state, stderr, false);
    {
        fei_state_makeprimitive(state, &state->objstring, "String");
        fei_state_makeprimitive(state, &state->objnumber, "Number");
        fei_state_makeprimitive(state, &state->objarray, "Array");
    }
    fei_state_setupstring(state);
    fei_state_setupnumber(state);
    fei_state_setuparray(state);
    return state;
}

void print_counts(FeiState* state)
{
    /*
    * useful to figure out what gets created how often, etc.
    * especially when debugging numbers.
    */
    int64_t total;
    total = 0;
    #define print_count_for(__field__, str) \
        { \
            fprintf(stderr, "created %zd %s\n", state->ocount.__field__, str); \
            total += state->ocount.__field__; \
        }
    print_count_for(cntstring, "FeiString");
    print_count_for(cntfunction, "FeiObjFunction");
    print_count_for(cntclass, "FeiClass");
    print_count_for(cntinstance, "FeiInstance");
    print_count_for(cntbound, "FeiObjBoundMethod");
    print_count_for(cntupval, "FeiObjUpval");
    print_count_for(cntclosure, "FeiObjClosure");
    print_count_for(cntnative, "FeiObjNative");
    print_count_for(cntnumfloat, "floating point number");
    print_count_for(cntnumfixed, "fixed point number");
    fprintf(stderr, "which makes %zd objects in total\n", total);

}

void fei_state_destroy(FeiState* state)
{
    size_t i;
    state->vmstate.initstring = NULL;
    // free all objects, from state->gcstate.objects
    fei_gcmem_freeobjects(state);
    fei_valtable_destroy(state, state->vmstate.globals);
    fei_valtable_destroy(state, state->vmstate.strings);
    fei_writer_destroy(state->iowriter_stdout, true);
    fei_writer_destroy(state->iowriter_stderr, true);
    for(i=0; i<da_count(state->vmstate.frameobjects); i++)
    {
        free(state->vmstate.frameobjects[i]);
    }
    da_destroy(state->vmstate.frameobjects);
    if(state->config.printcreatedobjcount)
    {
        print_counts(state);
    }
    free(state);
}

void fei_vm_raiseruntimeerror(FeiState* state, const char* format, ...)
{
    int i;
    int line;
    size_t instruction;
    va_list args;
    FeiVMFrame* frame;
    FeiObjFunction* function;
    fprintf(stderr, "!!!RUNTIME ERROR!!!\n");
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fprintf(stderr, "\n");
    // printing the stack trace for the function
    // print out each function that was still executing when the program died and where the execution was at the point it died
    for(i = state->vmstate.framecount - 1; i >= 0; i--)
    {
        frame = fei_vm_frameget(state, i);
        function = frame->closure->function;
        // - 1 because IP is sitting on the NEXT INSTRUCTION to be executed
        instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if(function->name == NULL)
        {
            fprintf(stderr, "script\n");
        }
        else
        {
            fprintf(stderr, "%s(%d)\n", function->name->chars, function->arity);
        }
    }
    // tell which line the error occurred
    // pulls from topmost FeiVMFrame on the stack
    frame = fei_vm_frameget(state, state->vmstate.framecount - 1);
    // - 1 to deal with the 1 added initially for the main() FeiVMFrame
    instruction = frame->ip - frame->closure->function->chunk.code - 1;
    line = frame->closure->function->chunk.lines[instruction];
    fprintf(stderr, "Error in script at [Line %d]\n", line);
    fei_vm_resetstack(state);
}

void fei_vm_defnative(FeiState* state, const char* name, FeiNativeFn function)
{
    FeiString* objname;
    FeiObjNative* objnat;
    objname = fei_string_copy(state, name, (int)strlen(name));
    objnat = fei_object_makenativefunc(state, function);
    fei_valtable_set(state, state->vmstate.globals, objname, fei_value_makeobject(state, objnat));        
}

void fei_vm_resetstack(FeiState* state)
{
    // point stackstop to the begininng of the empty array
    // stack array(state->vmstate.stackvalues) is already indirectly declared, hence no need to allocate memory for it
    state->vmstate.stacktop = state->vmstate.stackvalues;
    state->vmstate.framecount = 0;
    state->vmstate.openupvalues = NULL;
}



/* starting point of the compiler */
FeiResultCode fei_vm_evalsource(FeiState* state, const char* source, size_t len)
{
    FeiObjClosure* closure;
    FeiObjFunction* function;
    function = fei_compiler_compilesource(state, source, len);
    if(function == NULL)
    {
        return STATUS_SYNTAXERROR;
    }
    fei_vm_stackpush(state, fei_value_makeobject(state, function));
    closure = fei_object_makeclosure(state, function);
    fei_vm_stackpop(state);
    fei_vm_stackpush(state, fei_value_makeobject(state, closure));
    // 0 params for main()
    fei_vm_callvalue(state, fei_value_makenull(state), fei_value_makeobject(state, closure), 0);
    return fei_vm_exec(state);
}


