
#include <math.h>
#include "fei.h"


    /* info on the macros below
Below macros are FUNCTIONSt that take ZERO arguments, and what is inside () is their return value
READ_BYTE:	
	macro to ACCESS the BYTE(uin8_t) from the POINTER(ip), and increment it
	reads byte currently pointed at ip, then advances the instruction pointer
READ_CONSTANT:
	return constants.values element, from READ_BYTE(), which points exactly to the NEXT index
READ STRING:
	return as object string, read directly from the vm(oip)
*/

static inline uint8_t READ_BYTE(FeiVMFrame* frame)
{
    return (*frame->ip++);
}

static inline FeiValue READ_CONSTANT(FeiState* state, FeiVMFrame* frame)
{
    return fei_valarray_get(state, &frame->closure->function->chunk.constants, READ_BYTE(frame));
}

static inline ObjString* READ_STRING(FeiState* state, FeiVMFrame* frame)
{
    return fei_value_asstring(READ_CONSTANT(state, frame));
}

// for patch jumps
// yanks next two bytes from the chunk(used to calculate the offset earlier) and return a 16-bit integer out of it
// use bitwise OR
static inline uint16_t READ_SHORT(FeiVMFrame* frame)
{
    frame->ip += 2;
    return (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]);
}

FeiVMFrame* fei_vm_frameget(FeiState* state, int idx)
{
    long cnt;
    long cap;
    bool mustalloc;
    FeiVMFrame* fr;
    FeiVMFrame** frobs;
    (void)cap;
    frobs = state->vmstate.frameobjects;
    cnt = da_count(frobs);
    cap = da_capacity(frobs);
    mustalloc = (
        (
            (idx >= cnt) ||
            (cnt == 0)
        )
        #if 0
        &&
        (
            (cap > 0) &&
            (idx < cap)
        )
        #endif
    );
    fr = NULL;
    /*
    * do *NOT* initialize stuff anywhere inside these if-else clauses.
    * their purpose is to either get an existing index, or allocate a new frame.
    * NOTHING ELSE!
    */
    if(mustalloc)
    {
        fr = (FeiVMFrame*)malloc(sizeof(FeiVMFrame));
        memset(fr, 0, sizeof(FeiVMFrame));
        da_push(state->vmstate.frameobjects, sizeof(FeiVMFrame), fr);
    }
    else
    {
        fr = state->vmstate.frameobjects[idx];
    }
    fr->level = idx;
    fr->stackpos = state->vmstate.stackindex;
    fr->stackptr = state->vmstate.stackvalues;
    return fr;
}


#if defined(USE_DYNVALUE) && (USE_DYNVALUE == 1)
    #define ALSO_PRINTTOP 0
#endif

void dumpstack(FeiState* state, const char* fmt, ...)
{
    int i;
    int len;
    FeiValue* itm;
    FeiValue* next;
    va_list va;
    if(getenv("FEI_NODUMP") != NULL)
    {
        return;
    }
    va_start(va, fmt);
    fprintf(stderr, "in ");
    vfprintf(stderr, fmt, va);
    va_end(va);
    fprintf(stderr, ": stackindex=%d:\n", state->vmstate.stackindex);
    len = CFG_MAX_VMSTACK;
    for(i=0; i!=len; i++)
    {
        next = NULL;
        itm = &state->vmstate.stackvalues[i];
        next = &state->vmstate.stackvalues[i + 1];
        if(itm->type == VAL_UNDEF && (next->type == VAL_UNDEF))
        {
            break;
        }
        fprintf(stderr, "  stack(values/top)[%d] = ", i);
        #if defined(ALSO_PRINTTOP) && (ALSO_PRINTTOP == 1)
        fprintf(stderr, "{");
        #endif
        fei_value_printvalue(state, state->iowriter_stderr, state->vmstate.stackvalues[i], true);
        #if defined(ALSO_PRINTTOP) && (ALSO_PRINTTOP == 1)
            fprintf(stderr, ", ");
            fei_value_printvalue(state, state->iowriter_stderr, state->vmstate.stacktop[i], true);
            fprintf(stderr, "}");
        #endif
        fprintf(stderr, "\n");
    }
}

/*
// pointer to the element just PAST the element containing the top value of the stack
// == stackvalues[size(stackvalues) - 2]
FeiValue* stacktop;
*/
static inline void fei_vm_stackpush_inline(FeiState* state, FeiValue value)
{
    state->vmstate.stackindex++;
    // * in front of the pointer means the rvalue itself, assign value(parameter) to it
    *state->vmstate.stacktop = value;
    state->vmstate.stacktop++;
}

static inline FeiValue fei_vm_stackpop_inline(FeiState* state)
{
    state->vmstate.stackindex--;
    // first move the stack BACK to get the last element(stacktop points to ONE beyond the last element)    
    state->vmstate.stacktop--;
    return *state->vmstate.stacktop;
}

static inline FeiValue fei_vm_stackpeek_inline(FeiState* state, int distance)
{
    return state->vmstate.stacktop[-1 - distance];
}

void fei_vm_stackpush(FeiState* state, FeiValue value)
{
    fei_vm_stackpush_inline(state, value);
}

FeiValue fei_vm_stackpop(FeiState* state)
{
    return fei_vm_stackpop_inline(state);
}

FeiValue fei_vm_stackpeek(FeiState* state, int distance)
{
    return fei_vm_stackpeek_inline(state, distance);
}


/* for call stacks/functions  */
bool fei_vm_callclosure(FeiState* state, ObjClosure* closure, int argcount)
{
    FeiVMFrame* frame;
    // if number of parameters does not match
    if(argcount != closure->function->arity)
    {
        fei_vm_raiseruntimeerror(state, "expected %d arguments but got %d", closure->function->arity, argcount);
        return false;
    }
    // as FeiVMFrame is an array, to ensure array does not overflow
    if(state->vmstate.framecount == CFG_MAX_VMFRAMES)
    {
        fprintf(stderr, "internal error: stack overflow!\n");
        fei_vm_raiseruntimeerror(state, "stack overflow");
        return false;
    }
    // get pointer to next in frame array
    // initializes callframe to the top of the stack
    frame = fei_vm_frameget(state, state->vmstate.framecount++);
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    // set up yn pointer to give frame its window into the stack
    // ensures everyting lines up
    // slots is the 'starting pointer' for the function cll
    frame->slots = state->vmstate.stacktop - argcount - 1;
    return true;
}

bool fei_vm_callvalue(FeiState* state, FeiValue instval, FeiValue callee, int argcount)
{
    NativeFn natfn;
    FeiValue result;
    FeiValue initializer;
    ObjInstance* instance;
    ObjClass* klass;
    ObjBoundMethod* bound;
    if(fei_value_isobj(callee))
    {
        switch(fei_value_objtype(callee))
        {
            case OBJ_BOUND_METHOD:
                {
                    // get ObjBoundMethod from value type(callee)
                    bound = fei_value_asbound_method(callee);
                    state->vmstate.stacktop[-argcount - 1] = bound->receiver;
                    // run call to execute
                    return fei_vm_callclosure(state, bound->method, argcount);
                }
                break;
            // create class instance
            case OBJ_CLASS:
                {
                    klass = fei_value_asclass(callee);
                    // create new instance here
                    instance = fei_object_makeinstance(state, klass);
                    state->vmstate.stacktop[-argcount - 1] = fei_value_makeobject(state, instance);
                    // if we find one from the table
                    if(fei_table_get(state, &klass->methods, state->vmstate.initstring, &initializer))
                    {
                        return fei_vm_callclosure(state, fei_value_asclosure(initializer), argcount);
                    }
                    // if there ARE arguments but the initalizer method cannot be found
                    else if(argcount != 0)
                    {
                        fei_vm_raiseruntimeerror(state, "class has not initializer, but still received %d arguments", argcount);
                        return false;
                    }
                    return true;
                }
                break;
            case OBJ_CLOSURE:
                {
                    // call to function happens here
                    return fei_vm_callclosure(state, fei_value_asclosure(callee), argcount);
                }
                break;
            case OBJ_NATIVE:
                {
                    natfn = fei_value_asnative(callee);
                    result = natfn(state, instval, argcount, state->vmstate.stacktop - argcount);
                    // remove call and arguments from the stack
                    state->vmstate.stacktop -= argcount + 1;
                    fei_vm_stackpush(state, result);
                    return true;
                }
                break;
            default:
                {
                }
                break;
        }
    }
    fei_vm_raiseruntimeerror(state, "only functions or classes can be called");
    return false;
}

// get corresponding upvalue
ObjUpvalue* fei_vm_captureupvalue(FeiState* state, FeiValue* local)
{
    // set up the linked list
    ObjUpvalue* upvalue;
    ObjUpvalue* prevupvalue;
    ObjUpvalue* createdupvalue;
    prevupvalue = NULL;

    // assign at the start of the list
    upvalue = state->vmstate.openupvalues;

    // look for an existing upvalue in the list
    /*  LINKED LIST
    1. start at the head of the list, which is the upvalue CLOSET to the TOP OF THE STACK
    2. walk through the list, using a little pointer comparison to iterate past every upvalue pointing
        to slots ABOVE the one we are looking for
    -- upvalue->location (array of the indexes for the locals) is the stack    
    THREE ways to exit the loop:
    1. local slot stopped is the slot we're looking for
    2. ran ouf ot upvalues to search
    3. found an upvalue whose local slot is BELOW the one we're looking for
    */
    // pointer comparison: only find the ones ABOVE local
    while(upvalue != NULL && upvalue->location > local)
    {
        prevupvalue = upvalue;
        upvalue = upvalue->next;
    }
    // if the location/local/indeces match
    if(upvalue != NULL && upvalue->location == local)
    {
        // return already created upvalue
        return upvalue;
    }
    createdupvalue = fei_object_makeupvalue(state, local);
    // insert at the front
    createdupvalue->next = upvalue;
    // ran out of values to search
    if(prevupvalue == NULL)
    {
        // set pointer to the newly added upvalue
        state->vmstate.openupvalues = createdupvalue;
    }
    // found local slot BELOW the one we are looking for
    else
    {
        // link next slot(the value below) to the newly inserted upvalue
        prevupvalue->next = createdupvalue;
    }
    return createdupvalue;
}

// closes every upvalue it can find that points to the slot or any above the stack
void fei_vm_closeupvalues(FeiState* state, FeiValue* last)// takes pointer to stack slot
{
    ObjUpvalue* upvalue;
    while(state->vmstate.openupvalues != NULL && state->vmstate.openupvalues->location >= last)
    {
        // pointer to list of openupvalues
        upvalue = state->vmstate.openupvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        state->vmstate.openupvalues = upvalue->next;
    }
}

bool fei_instance_getproperty(FeiState* state, ObjInstance* instance, ObjString* name, FeiValue* dest)
{
    FeiValue val;
    if(fei_table_get(state, &instance->fields, name, &val))
    {
        *dest = val;
        return true;
    }
    return false;
}

// defining method for class type
void fei_vm_classdefmethodfromstack(FeiState* state, ObjString* name)
{
    FeiValue method;
    ObjClass* klassobj;
    // method/closure is at the top of the stack
    method = fei_vm_stackpeek_inline(state, 0);
    // class is at the 2nd top
    klassobj = fei_value_asclass(fei_vm_stackpeek_inline(state, 1));
    fei_class_setmethod(state, klassobj, name, method);
    // pop the method
    fei_vm_stackpop_inline(state);
}

// string concatenation
void fei_vmdo_strconcat(FeiState* state)
{
    ObjString* first;
    ObjString* second;
    ObjString* result;
    // peek, so we do not pop it off if calling a GC is needed
    second = fei_value_asstring(fei_vm_stackpeek_inline(state, 0));
    first = fei_value_asstring(fei_vm_stackpeek_inline(state, 1));
    fei_vm_stackpop_inline(state);
    fei_vm_stackpop_inline(state);
    result = fei_string_copy(state, first->chars, first->length);
    fei_string_append(state, first, second->chars, second->length);
    fei_vm_stackpush(state, fei_value_makeobject(state, result));
}


bool fei_vmdo_return(FeiState* state)
{
    // if function returns a value, value will beon top of the stack
    FeiValue result = fei_vm_stackpop_inline(state);
    // close lingering closed values
    fei_vm_closeupvalues(state, state->vmstate.topframe->slots);
    state->vmstate.framecount--;
    // return from 'main()'/script function
    if(state->vmstate.framecount == 0)
    {
        // pop main script function from the stack
        fei_vm_stackpop_inline(state);
        return false;
    }
    // for a function
    // discard all the slots the callee was using for its parameters
    // basically 're-assign'
    state->vmstate.stacktop = state->vmstate.topframe->slots;
    // push the return value
    fei_vm_stackpush(state, result);
    // update run function's current frame
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    return true;
}

bool fei_vmdo_superinvoke(FeiState* state)
{
    int count;
    ObjClass* parent;
    ObjString* method;
    method = READ_STRING(state, state->vmstate.topframe);
    count = READ_BYTE(state->vmstate.topframe);
    parent = fei_value_asclass(fei_vm_stackpop_inline(state));
    if(!fei_class_invokemethod(state, parent, method, count))
    {
        return false;
    }
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    return true;
}

bool fei_vmdo_getsuper(FeiState* state)
{
    FeiValue val;
    ObjString* name;
    ObjClass* parent;
    // get method name/identifier
    name = READ_STRING(state, state->vmstate.topframe);
    // class identifier is at the top of the stack
    parent = fei_value_asclass(fei_vm_stackpop_inline(state));
    // if binding fails
    val = fei_vm_stackpeek(state, 0);
     
     
    if(!fei_class_bindmethod(state, parent, name, val, false, false))
    {
        return false;
    }
    fei_vm_stackpop(state);
    return true;
}

bool fei_vmdo_inherit(FeiState* state)
{
    FeiValue parent;
    ObjClass* child;
    // parent class from 2nd top of the stack
    // ensure that parent identifier is a class
    parent = fei_vm_stackpeek_inline(state, 1);
    if(!fei_value_isclass(parent))
    {
        fei_vm_raiseruntimeerror(state, "parent identifier is not a class");
        return false;
    }
    // child class at the top of the stack
    child = fei_value_asclass(fei_vm_stackpeek_inline(state, 0));
    // add all methods from parent to child table
    //fei_table_mergefrom(state, &fei_value_asclass(parent)->methods, &child->methods);
    fei_class_inherit(state, fei_value_asclass(parent), child);
    // pop the child class
    fei_vm_stackpop_inline(state);
    return true;
}

bool fei_vmdo_invokemethod(FeiState* state)
{
    int argcount;
    ObjString* method;
    method = READ_STRING(state, state->vmstate.topframe);
    argcount = READ_BYTE(state->vmstate.topframe);
    // new invoke function
    if(!fei_vm_classinvokefromstack(state, method, argcount))
    {
        return false;
    }
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    return true;
}

bool fei_vmdo_makeclosure(FeiState* state)
{
    int i;
    uint8_t index;
    uint8_t islocal;
    ObjClosure* closure;
    ObjFunction* function;
    // load compiled function from table
    function = fei_value_asfunction(READ_CONSTANT(state, state->vmstate.topframe));
    closure = fei_object_makeclosure(state, function);
    fei_vm_stackpush(state, fei_value_makeobject(state, closure));
    // fill upvalue array over in the interpreter when a closure is created
    // to see upvalues in each slot
    for(i = 0; i < closure->upvaluecount; i++)
    {
        // read islocal bool
        islocal = READ_BYTE(state->vmstate.topframe);
        // read index for local, if available, in the closure
        index = READ_BYTE(state->vmstate.topframe);
        if(islocal)
        {
            // get from slots stack
            closure->upvalues[i] = fei_vm_captureupvalue(state, state->vmstate.topframe->slots + index);
        }
        // if not local(nested upvalue)
        else
        {
            // get from current upvalue
            closure->upvalues[i] = state->vmstate.topframe->closure->upvalues[index];
        }
    }
    return true;
}


bool fei_vmdo_setproperty(FeiState* state)
{
    FeiValue peekval;
    FeiValue peekinst;
    ObjString* name;
    ObjInstance* instance;
    peekinst = fei_vm_stackpeek_inline(state, 1);
    if(!fei_value_isinstance(peekinst))
    {
        fei_vm_raiseruntimeerror(state, "setproperty() where parent symbol is not a class instance");
        return false;
    }
    // not top most, as the top most is reserved for the new value to be set
    instance = fei_value_asinstance(peekinst);
    //peek(0) is the new value
    peekval = fei_vm_stackpeek_inline(state, 0);
    name = READ_STRING(state, state->vmstate.topframe);
    fei_table_set(state, &instance->fields, name, peekval);
    // pop the already set value
    fei_vm_stackpop_inline(state);
    // pop the property instance itself
    fei_vm_stackpop_inline(state);
    // push the value back again
    fei_vm_stackpush(state, peekval);
    return true;
}

bool fei_vmdo_call(FeiState* state)
{
    int argcount;
    FeiValue peeked;
    FeiValue instance;
    instance = fei_value_makenull(state);
    //dumpstack(state, "vmdo_call");
    argcount = READ_BYTE(state->vmstate.topframe);
    peeked = fei_vm_stackpeek_inline(state, argcount);
    // call function; pass in the function name istelf[peek(depth)] and the number of arguments
    if(!fei_vm_callvalue(state, instance, peeked, argcount))
    {
        return false;
    }
    // to update pointer if callframe is successful, asnew frame is added
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    return true;
}

ObjClass* fei_vm_getclassfor(FeiState* state, int typ)
{
    switch(typ)
    {
        case OBJ_STRING:
            return state->objstring.classobj;
        case VAL_NUMBER:
            return state->objnumber.classobj;
        default:
            break;
    }
    return NULL;
}

bool fei_vm_otherproperty(FeiState* state, ObjString* name, int typ, bool asfield)
{
    bool b;
    FeiValue v;
    ObjClass* klass;
    Table* tab;
    klass = fei_vm_getclassfor(state, typ);
    //fprintf(stderr, "fei_vm_otherproperty: typ=%d klass=%p (%s) name=%.*s\n", typ, klass, klass->name->chars, name->length, name->chars);
    if(klass != NULL)
    {
        tab = &klass->methods;
        if(asfield)
        {
            tab = &klass->fieldlike;
        }
        b = fei_table_get(state, tab, name, &v);
        //fprintf(stderr, "get table value: %d\n", b);
        if(b)
        {
            fei_vm_stackpush(state, v);
            return true;
        }
    }
    return false;
}

// invoke class method, access method + call method
bool fei_vm_classinvoke(FeiState* state, FeiValue receiver, ObjString* name, int argcount)
{
    int typ;
    bool isinst;
    bool isother;
    ObjClass* tclass;
    FeiValue value;
    ObjInstance* instance;
    isother = false;
    // call method with wrong type, not an objinstance type
    typ = fei_value_gettype(receiver);
    tclass = fei_vm_getclassfor(state, typ);
    isinst = fei_value_isinstance(receiver);
    if((isinst == false) || (tclass == NULL))
    {
        isother = fei_vm_otherproperty(state, name, typ, false);
        if(!isother && !isinst)
        {
            fei_vm_raiseruntimeerror(state, "classinvokefromstack: cannot invoke method on something not an instance");
            return false;
        }
    }
    if(isother)
    {
        value = fei_vm_stackpop(state);
        return fei_vm_callvalue(state, receiver, value, argcount);
    }
    instance = fei_value_asinstance(receiver);
    // for fields()
    if(fei_table_get(state, &instance->fields, name, &value))
    {
        state->vmstate.stacktop[-argcount - 1] = value;
        return fei_vm_callvalue(state, fei_value_makenull(state), value, argcount);
    }
    // actual function that searches for method and calls it
    return fei_class_invokemethod(state, instance->classobject, name, argcount);
}

bool fei_vm_classinvokefromstack(FeiState* state, ObjString* name, int argcount)
{
    FeiValue receiver;
    // grab the receiver of the stack
    receiver = fei_vm_stackpeek_inline(state, argcount);
    return fei_vm_classinvoke(state, receiver, name, argcount);    
}

bool fei_vmdo_getproperty(FeiState* state)
{
    int typ;
    bool b;
    FeiValue value;
    FeiValue peeked;
    ObjClass* klass;
    ObjString* name;
    ObjInstance* instance;
    peeked = fei_vm_stackpeek_inline(state, 0);
    name = READ_STRING(state, state->vmstate.topframe);
    // to make sure only instances are allowed to have fields
    if(!fei_value_isinstance(peeked))
    {
        typ = fei_value_gettype(peeked);
        if(fei_vm_otherproperty(state, name, typ, true))
        {
            value = fei_vm_stackpop(state);
            return fei_vm_callvalue(state, peeked, value, 0);
        }
        else
        {
            klass = fei_vm_getclassfor(state, typ);
            //fprintf(stderr, "getproperty: klass=%p\n", klass);
            if(klass != NULL)
            {
                b = fei_table_get(state, &klass->methods, name, &value);
                //fprintf(stderr, "get method: %d\n", b);
                if(b)
                {
                    fei_vm_stackpop(state);
                    fei_vm_stackpush(state, value);
                    //fei_vm_stackpush(state, peeked);
                    return true;
                }
            }
        }
        fei_vm_raiseruntimeerror(state, "only instances have properties");
        return false;
    }
    // get instance from top most stack
    instance = fei_value_asinstance(peeked);
    // get identifier name
    // get from fields hash table, assign it to instance
    if(fei_instance_getproperty(state, instance, name, &value))
    {
        // pop the instance itself
        fei_vm_stackpop_inline(state);
        fei_vm_stackpush(state, value);
        return true;
    }
    value =  fei_vm_stackpeek(state, 0);
    // no method as well, error
    if(!fei_class_bindmethod(state, instance->classobject, name, value, false, false))
    {
        return false;
    }
    fei_vm_stackpop(state);
    return true;
}

const char* op2name(int op)
{
    switch(op)
    {
        case OP_NEGATE: return "OP_NEGATE";
        case OP_ADD: return "OP_ADD";
        case OP_SUBTRACT: return "OP_SUBTRACT";
        case OP_MULTIPLY: return "OP_MULTIPLY";
        case OP_DIVIDE: return "OP_DIVIDE";
        case OP_MODULO: return "OP_MODULO";
        case OP_GREATER: return "OP_GREATER";
        case OP_LESS: return "OP_LESS";
        case OP_CONSTANT: return "OP_CONSTANT";
        case OP_NULL: return "OP_NULL";
        case OP_TRUE: return "OP_TRUE";
        case OP_FALSE: return "OP_FALSE";
        case OP_NOT: return "OP_NOT";
        case OP_SWITCH_EQUAL: return "OP_SWITCH_EQUAL";
        case OP_EQUAL: return "OP_EQUAL";
        case OP_PRINT: return "OP_PRINT";
        case OP_POP: return "OP_POP";
        case OP_GET_LOCAL: return "OP_GET_LOCAL";
        case OP_SET_LOCAL: return "OP_SET_LOCAL";
        case OP_DEFINE_GLOBAL: return "OP_DEFINE_GLOBAL";
        case OP_GET_GLOBAL: return "OP_GET_GLOBAL";
        case OP_SET_GLOBAL: return "OP_SET_GLOBAL";
        case OP_GET_UPVALUE: return "OP_GET_UPVALUE";
        case OP_SET_UPVALUE: return "OP_SET_UPVALUE";
        case OP_GET_PROPERTY: return "OP_GET_PROPERTY";
        case OP_SET_PROPERTY: return "OP_SET_PROPERTY";
        case OP_CLOSE_UPVALUE: return "OP_CLOSE_UPVALUE";
        case OP_JUMP: return "OP_JUMP";
        case OP_JUMP_IF_FALSE: return "OP_JUMP_IF_FALSE";
        case OP_LOOP: return "OP_LOOP";
        case OP_LOOP_IF_FALSE: return "OP_LOOP_IF_FALSE";
        case OP_LOOP_IF_TRUE: return "OP_LOOP_IF_TRUE";
        case OP_CALL: return "OP_CALL";
        case OP_CLOSURE: return "OP_CLOSURE";
        case OP_CLASS: return "OP_CLASS";
        case OP_METHOD: return "OP_METHOD";
        case OP_INVOKE: return "OP_INVOKE";
        case OP_INHERIT: return "OP_INHERIT";
        case OP_GET_SUPER: return "OP_GET_SUPER";
        case OP_SUPER_INVOKE: return "OP_SUPER_INVOKE";
        case OP_RETURN: return "OP_RETURN";
    }
    return "?unknown?";
}

bool fei_vmdo_unary(FeiState* state, uint8_t instruc)
{
    double dnum;
    double dres;
    int64_t inum;
    int64_t ires;
    FeiValue poked;
    FeiValue popped;
    poked = fei_vm_stackpeek_inline(state, 0);
    if(!fei_value_isnumber(poked))
    {
        fei_vm_raiseruntimeerror(state, "operand must be a number");
        return false;
    }
    popped = fei_vm_stackpop_inline(state);
    switch(instruc)
    {
        case OP_NEGATE:
            {
                if(popped.isfixednumber)
                {
                    inum = fei_value_asfixednumber(popped);
                    ires = -inum;
                }
                else
                {
                    dnum = fei_value_asfloatnumber(popped);
                    dres = -dnum;
                }
            }
            break;
        default:
            {
                fei_vm_raiseruntimeerror(state, "invalid instruction '%d' for vmdo_unary", instruc);
                return false;
            }
            break;
    }
    if(popped.isfixednumber)
    {
        fei_vm_stackpush(state, fei_value_makefixednumber(state, ires));
    }
    else
    {
        fei_vm_stackpush(state, fei_value_makefloatnumber(state, dres));
    }
    return true;
}

 //compares if the float f1 is equal with f2 and returns 1 if true and 0 if false
int compare_float(double f1, double f2)
{
    float precision = 0.00001;
    if(((f1 - precision) < f2) && ((f1 + precision) > f2))
    {
        return 1;
    }
    return 0;
}

bool fei_vmdo_binary(FeiState* state, uint8_t instruc)
{
    bool leftfixed;
    bool rightfixed;
    bool isfloat;
    FeiValue res;
    FeiValue valright;
    FeiValue valleft;
    FeiValue pokeright;
    FeiValue pokeleft;
    double fvleft;
    double fvright;
    int64_t nvleft;
    int64_t nvright;
    pokeright = fei_vm_stackpeek_inline(state, 0);
    pokeleft = fei_vm_stackpeek_inline(state, 1);
    if((instruc == OP_ADD) && (fei_value_isstring(pokeright) && fei_value_isstring(pokeleft)))
    {
        fei_vmdo_strconcat(state);
        return true;
    }
    if(fei_value_isnumber(pokeright) && fei_value_isnumber(pokeleft))
    {
        valright = fei_vm_stackpop_inline(state);
        valleft = fei_vm_stackpop_inline(state);
        // do NOT turn these into macros, since some of can be optimized further.
        // macros would make that much more difficult.
        res = fei_value_makenull(state);
        nvright = 0;
        nvleft = 0;
        fvright = 0;
        fvleft = 0;
        leftfixed = valleft.isfixednumber;
        rightfixed = valright.isfixednumber;
        isfloat = (!leftfixed && !rightfixed);
        /*
        * first, attempt to do cheaper, faster fixed-point arithmetics.
        * *IF* that fails, "fall back" to floating point:
        * the crucial part here is that fei_value_asfixednumber will fall back to
        * implicitly converting the stored floating point, and likewise, fei_value_asfloatnumber
        * will implicitly convert the stored fixed point.
        * that's not exactly ideal, but is, at least as of now, the cheapest
        * form to have both fixed point and floating point numbers.
        */
        switch(instruc)
        {
            case OP_ADD:
                {
                    if(!isfloat)
                    {
                        nvright = fei_value_asfixednumber(valright);
                        nvleft = fei_value_asfixednumber(valleft);
                        res = fei_value_makefixednumber(state, nvleft + nvright);
                    }
                    else
                    {
                        fvright = fei_value_asfloatnumber(valright);
                        fvleft = fei_value_asfloatnumber(valleft);
                        res = fei_value_makefloatnumber(state, fvleft + fvright);
                    }
                }
                break;
            case OP_SUBTRACT:
                {
                    if(leftfixed && rightfixed)
                    {
                        nvright = fei_value_asfixednumber(valright);
                        nvleft = fei_value_asfixednumber(valleft);
                        res = fei_value_makefixednumber(state, nvleft - nvright);
                    }
                    else
                    {
                        fvright = fei_value_asfloatnumber(valright);
                        fvleft = fei_value_asfloatnumber(valleft);
                        res = fei_value_makefloatnumber(state, fvleft - fvright);
                    }
                }
                break;
            case OP_MULTIPLY:
                {
                    if(leftfixed && rightfixed)
                    {
                        nvright = fei_value_asfixednumber(valright);
                        nvleft = fei_value_asfixednumber(valleft);
                        res = fei_value_makefixednumber(state, nvleft * nvright);
                    }
                    else
                    {
                        fvright = fei_value_asfloatnumber(valright);
                        fvleft = fei_value_asfloatnumber(valleft);
                        res = fei_value_makefloatnumber(state, fvleft * fvright);
                    }
                }
                break;
            case OP_DIVIDE:
                {
                    if(leftfixed && rightfixed)
                    {
                        nvright = fei_value_asfixednumber(valright);
                        nvleft = fei_value_asfixednumber(valleft);
                        res = fei_value_makefixednumber(state, nvleft / nvright);
                    }
                    else
                    {
                        fvright = fei_value_asfloatnumber(valright);
                        fvleft = fei_value_asfloatnumber(valleft);
                        res = fei_value_makefloatnumber(state, fvleft / fvright);
                    }
                }
                break;
            case OP_MODULO:
                {
                    if(leftfixed && rightfixed)
                    {
                        nvright = fei_value_asfixednumber(valright);
                        nvleft = fei_value_asfixednumber(valleft);
                        res = fei_value_makefixednumber(state, nvleft % nvright);
                    }
                    else
                    {
                        fvright = fei_value_asfloatnumber(valright);
                        fvleft = fei_value_asfloatnumber(valleft);
                        res = fei_value_makefloatnumber(state, fmod(fvleft, fvright));
                    }
                }
                break;
            case OP_GREATER:
                {
                    if(leftfixed && rightfixed)
                    {
                        nvright = fei_value_asfixednumber(valright);
                        nvleft = fei_value_asfixednumber(valleft);
                        res = fei_value_makefixednumber(state, nvleft > nvright);
                    }
                    else
                    {
                        fvright = fei_value_asfloatnumber(valright);
                        fvleft = fei_value_asfloatnumber(valleft);
                        res = fei_value_makebool(state, fvleft > fvright);
                    }
                }
                break;
            case OP_LESS:
                {
                    if(leftfixed && rightfixed)
                    {
                        nvright = fei_value_asfixednumber(valright);
                        nvleft = fei_value_asfixednumber(valleft);
                        res = fei_value_makebool(state, nvleft < nvright);
                    }
                    else
                    {
                        fvright = fei_value_asfloatnumber(valright);
                        fvleft = fei_value_asfloatnumber(valleft);
                        res = fei_value_makebool(state, fvleft < fvright);
                    }
                }
                break;
            default:
                {
                    fei_vm_raiseruntimeerror(state, "invalid instruction '%d' for vmdo_binary", instruc);
                    return false;
                }
        }
        #if 0
        fprintf(stderr, "binary(%s): left=(%ld | %g) right=(%ld | %g) ==(%ld | %g)\n",
            op2name(instruc),
            fei_value_asfixednumber(valleft), fei_value_asfloatnumber(valleft),
            fei_value_asfixednumber(valright), fei_value_asfloatnumber(valright),
            fei_value_asfixednumber(res), fei_value_asfloatnumber(res)            
            
        );
        #endif
        fei_vm_stackpush(state, res);
    }
    else
    {
        fei_vm_raiseruntimeerror(state, "operands are incompatible");
        return false;
    }
    return true;
}



bool fei_vmdo_switchequal(FeiState* state)
{
    FeiValue a;
    FeiValue b;
    // only pop second value
    b = fei_vm_stackpop_inline(state);
    // peek topmost, the first value
    a = fei_vm_stackpeek_inline(state, 0);
    fei_vm_stackpush(state, fei_value_makebool(state, fei_value_compare(state, a, b)));
    return true;
}

bool fei_vmdo_compare(FeiState* state)
{
    FeiValue a;
    FeiValue b;
    b = fei_vm_stackpop_inline(state);
    a = fei_vm_stackpop_inline(state);
    fei_vm_stackpush(state, fei_value_makebool(state, fei_value_compare(state, a, b)));
    return true;
}

bool fei_vmdo_logicalnot(FeiState* state)
{
    bool isfalsey;
    FeiValue popped;
    popped = fei_vm_stackpop_inline(state);
    isfalsey = fei_value_isfalsey(state, popped);
    fei_vm_stackpush(state, fei_value_makebool(state, isfalsey));
    return true;
}

bool fei_vmdo_defineglobal(FeiState* state)
{
    ObjString* name;
    // get name from constant table
    name = READ_STRING(state, state->vmstate.topframe);
    // take value from the top of the stack
    fei_table_set(state, &state->vmstate.globals, name, fei_vm_stackpeek_inline(state, 0));
    fei_vm_stackpop_inline(state);
    return true;
}

bool fei_vmdo_setglobal(FeiState* state)
{
    ObjString* name;
    name = READ_STRING(state, state->vmstate.topframe);
    // if key not in hash table
    if(fei_table_set(state, &state->vmstate.globals, name, fei_vm_stackpeek_inline(state, 0)))
    {
        //fei_table_delete(state, &state->vmstate.globals, name);// delete the false name
        //fei_vm_raiseruntimeerror(state, "undefined variable '%s'", name->chars);
        //return STATUS_RTERROR;
    }
    return true;
}

bool fei_vmdo_getglobal(FeiState* state)
{
    FeiValue value;
    ObjString* name;
    // get the name
    name = READ_STRING(state, state->vmstate.topframe);
    // if key not in hash table
    if(!fei_table_get(state, &state->vmstate.globals, name, &value))
    {
        fei_vm_raiseruntimeerror(state, "undefined variable '%.*s'", name->length, name->chars);
        return false;
    }
    fei_vm_stackpush(state, value);
    return true;
}

bool fei_vmdo_setlocal(FeiState* state)
{
    uint8_t slot;
    slot = READ_BYTE(state->vmstate.topframe);
    // all the local var's VARIABLES are stored inside state->vmstate.stackvalues
    // takes from top of the stack and stores it in the stack slot
    state->vmstate.topframe->slots[slot] = fei_vm_stackpeek_inline(state, 0);
    return true;
}

bool fei_vmdo_getlocal(FeiState* state)
{
    uint8_t slot;
    slot = READ_BYTE(state->vmstate.topframe);
    // pushes the value to the stack where later instructions can read it
    fei_vm_stackpush(state, state->vmstate.topframe->slots[slot]);
    return true;
}

bool fei_vmdo_getconstant(FeiState* state)
{
    FeiValue constant;
    // READ the next line, which is the INDEX of the constant in the constants array
    constant = READ_CONSTANT(state, state->vmstate.topframe);
    fei_vm_stackpush(state, constant);
    return true;
}

bool fei_vmdo_setupvalue(FeiState* state)
{
    uint8_t slot;
    // read index
    slot = READ_BYTE(state->vmstate.topframe);
    // set to the topmost stack
    *(state->vmstate.topframe->closure->upvalues[slot]->location) = fei_vm_stackpeek_inline(state, 0);
    return true;
}

bool fei_vmdo_getupvalue(FeiState* state)
{
    uint8_t slot;
    // read index
    slot = READ_BYTE(state->vmstate.topframe);
    // push the value to the stack
    fei_vm_stackpush(state, *(state->vmstate.topframe->closure->upvalues[slot]->location));
    return true;
}

bool fei_vmdo_closeupvalue(FeiState* state)
{
    // put address to the slot
    fei_vm_closeupvalues(state, state->vmstate.stacktop - 1);
    // pop from the stack
    fei_vm_stackpop_inline(state);
    return true;
}

bool fei_vmdo_jumpalways(FeiState* state)
{
    uint16_t offset;
    (void)state;
    offset = READ_SHORT(state->vmstate.topframe);
    state->vmstate.topframe->ip += offset;
    return true;
}

bool fei_vmdo_jumpiffalse(FeiState* state)
{
    uint16_t offset;
    FeiValue peeked;
    // offset already put in the stack
    offset = READ_SHORT(state->vmstate.topframe);
    peeked = fei_vm_stackpeek_inline(state, 0);
    // actual jump instruction is done here; skip over the instruction pointer
    if(fei_value_isfalsey(state, peeked))
    {
        // if evaluated expression inside if statement is false jump
        state->vmstate.topframe->ip += offset;
    }
    return true;
}

bool fei_vmdo_loop(FeiState* state)
{
    uint16_t offset;
    (void)state;
    offset = READ_SHORT(state->vmstate.topframe);
    // jumps back
    state->vmstate.topframe->ip -= offset;
    return true;
}

bool fei_vmdo_loopiffalse(FeiState* state)
{
    uint16_t offset;
    // offset already put in the stack
    offset = READ_SHORT(state->vmstate.topframe);
    // bool state is at the top of the stack
    // if false loop back
    if(fei_value_isfalsey(state, fei_vm_stackpeek_inline(state, 0)))
    {
        state->vmstate.topframe->ip -= offset;
    }
    // pop the true/false
    fei_vm_stackpop_inline(state);
    return true;
}

bool fei_vmdo_loopiftrue(FeiState* state)
{
    uint16_t offset;
    (void)state;
    // offset already put in the stack
    offset = READ_SHORT(state->vmstate.topframe);
    // bool state is at the top of the stack
    // if not false loop back
    if(!fei_value_isfalsey(state, fei_vm_stackpeek_inline(state, 0)))
    {
        state->vmstate.topframe->ip -= offset;
    }
    // pop the true/false
    fei_vm_stackpop_inline(state);
    return true;
}

typedef bool(*VMPrimitive)(FeiState*);

#define exec_vmprim(fn) \
    { \
        if(!(fn)(state)) \
        { \
            return STATUS_RTERROR; \
        } \
    }

// run the chunk
ResultCode fei_vm_exec(FeiState* state)
{
    uint8_t instruction;
    state->vmstate.topframe = fei_vm_frameget(state, state->vmstate.framecount - 1);
    while(true)
    {
        // fei_dbgdisas_instr needs an byte offset, do pointer math to convert ip back to relative offset
        // from the beginning of the chunk (subtract current ip from the starting ip)
        // IMPORTANT -> only for debugging the VM
        #if defined(DEBUG_TRACE_EXECUTION) && (DEBUG_TRACE_EXECUTION == 1)
            // for stack tracing
            printf("      ");
            /* note on C POINTERSE
            -> pointing to the array itself means pointing to the start of the array, or the first element of the array
            -> ++/-- means moving through the array (by 1 or - 1)
            -> you can use operands like < > to tell compare how deep are you in the array
            */
            // prints every existing value in the stack
            for(FeiValue* slot = state->vmstate.stackvalues; slot < state->vmstate.stacktop; slot++)
            {
                fprintf(stderr, "[ ");
                fei_value_printvalue(state, state->iowriter_stderr, *slot, true);
                fprintf(stderr, " ]");
            }
            fei_dbgdisas_instr(state, &state->vmstate.topframe->closure->function->chunk, (int)(state->vmstate.topframe->ip - state->vmstate.topframe->closure->function->chunk.code));
        #endif
        // get result of the byte read, every set of instruction starts with an opcode
        switch(instruction = READ_BYTE(state->vmstate.topframe))
        {
            case OP_CONSTANT:
                {
                    exec_vmprim(fei_vmdo_getconstant);
                }
                break;
            // unary opcode
            case OP_NEGATE:
                {
                    if(!fei_vmdo_unary(state, instruction))
                    {
                        return STATUS_RTERROR;
                    }
                }
                break;
            // literals
            case OP_NULL:
                {
                    fei_vm_stackpush(state, fei_value_makenull(state));
                }
                break;
            case OP_TRUE:
                {
                    fei_vm_stackpush(state, fei_value_makebool(state, true));
                }
                break;
            case OP_FALSE:
                {
                    fei_vm_stackpush(state, fei_value_makebool(state, false));
                }
                break;
            case OP_ADD:
            case OP_SUBTRACT:
            case OP_MULTIPLY:
            case OP_DIVIDE:
            case OP_MODULO:
            case OP_LESS:
            case OP_GREATER:
                {
                    if(!fei_vmdo_binary(state, instruction))
                    {
                        return STATUS_RTERROR;
                    }
                }
                break;
            case OP_NOT:
                {
                    exec_vmprim(fei_vmdo_logicalnot);
                }
                break;
            case OP_SWITCH_EQUAL:
                {
                    exec_vmprim(fei_vmdo_switchequal);
                }
                break;
            case OP_EQUAL:
                {
                    exec_vmprim(fei_vmdo_compare);
                }
                break;
            case OP_PRINT:
                {
                    fei_value_printvalue(state, state->iowriter_stdout, fei_vm_stackpop_inline(state), false);
                    printf("\n");
                }
                break;
            case OP_POP:
                {
                    fei_vm_stackpop_inline(state);
                }
                break;
            case OP_GET_LOCAL:
                {
                    exec_vmprim(fei_vmdo_getlocal);
                }
                break;
            case OP_SET_LOCAL:
                {
                    exec_vmprim(fei_vmdo_setlocal);
                }
                break;
            case OP_DEFINE_GLOBAL:
                {
                    exec_vmprim(fei_vmdo_defineglobal);
                }
                break;
            case OP_GET_GLOBAL:
                {
                    exec_vmprim(fei_vmdo_getglobal);
                }
                break;
            case OP_SET_GLOBAL:
                {
                    exec_vmprim(fei_vmdo_setglobal);
                }
                break;
            // upvalues set/get
            case OP_GET_UPVALUE:
                {
                    exec_vmprim(fei_vmdo_getupvalue);
                }
                break;
            case OP_SET_UPVALUE:
                {
                    exec_vmprim(fei_vmdo_setupvalue);
                }
                break;
            case OP_GET_PROPERTY:
                {
                    exec_vmprim(fei_vmdo_getproperty);
                }
                break;
            case OP_SET_PROPERTY:
                {
                    exec_vmprim(fei_vmdo_setproperty);
                }
                break;
            case OP_CLOSE_UPVALUE:
                {
                    exec_vmprim(fei_vmdo_closeupvalue);
                }
                break;
            // will always jump
            case OP_JUMP:
                {
                    exec_vmprim(fei_vmdo_jumpalways);
                }
                break;
            case OP_JUMP_IF_FALSE:// for initial if, will not jump if expression inside is true
                {
                    exec_vmprim(fei_vmdo_jumpiffalse);
                }
                break;
            case OP_LOOP:
                {
                    exec_vmprim(fei_vmdo_loop);
                }
                break;
            case OP_LOOP_IF_FALSE:
                {
                    exec_vmprim(fei_vmdo_loopiffalse);
                }
                break;
            case OP_LOOP_IF_TRUE:
                {
                    exec_vmprim(fei_vmdo_loopiftrue);
                }
                break;
            // a callstack to a funcion has the form of function name, param1, param2...
            // the top level code, or caller, also has the same function name, param1, param2... in the right order
            case OP_CALL:
                {
                    exec_vmprim(fei_vmdo_call);
                }
                break;
            // closures
            case OP_CLOSURE:
                {
                    exec_vmprim(fei_vmdo_makeclosure);
                }
                break;
            case OP_CLASS:
                {
                    // load string for the class' name and push it onto the stack
                    fei_vm_stackpush(state, fei_value_makeobject(state, fei_object_makeclass(state, READ_STRING(state, state->vmstate.topframe))));
                }
                break;
            case OP_METHOD:
                {
                    // get name of the method
                    fei_vm_classdefmethodfromstack(state, READ_STRING(state, state->vmstate.topframe));
                }
                break;
            case OP_INVOKE:
                {
                    exec_vmprim(fei_vmdo_invokemethod);
                }
                break;
            case OP_INHERIT:
                {
                    exec_vmprim(fei_vmdo_inherit);
                }
                break;
            case OP_GET_SUPER:
                {
                    exec_vmprim(fei_vmdo_getsuper);
                }
                break;
            case OP_SUPER_INVOKE:
                {
                    exec_vmprim(fei_vmdo_superinvoke);
                }
                break;
            case OP_RETURN:
                {
                    if(fei_vmdo_return(state) == false)
                    {
                        return STATUS_OK;                        
                    }
                }
                break;
        }
    }
}


