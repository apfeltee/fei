
#include "fei.h"

int fei_dbgutil_printsimpleir(FeiState* state, const char* name, int offset)
{
    (void)state;
    // print as a string, or char*
    fprintf(stderr, "%s\n", name);
    return offset + 1;
}

int fei_dbgutil_printbyteir(FeiState* state, const char* name, FeiBytecodeList* chunk, int offset)
{
    uint8_t slot;
    (void)state;
    slot = chunk->code[offset + 1];
    fprintf(stderr, "%-16s %4d\n", name, slot);
    return offset + 2;
}

int fei_dbgutil_printconstir(FeiState* state, const char* name, FeiBytecodeList* chunk, int offset)
{
    uint8_t constant;
    // pullout the constant index from the subsequent byte in the chunk
    constant = chunk->code[offset + 1];
    // print out name of the opcode, then the constant index
    fprintf(stderr, "%-16s %4d '", name, constant);
    //	display the value of the constant,  user defined function
    fei_value_printvalue(state, state->iowriter_stderr, fei_valarray_get(state, &chunk->constants, constant), true);
    fprintf(stderr, "'\n");
    //OP_RETURN is a single byte, and the other byte is the operand, hence offsets by 2
    return offset + 2;
}

int fei_dbgutil_printinvokeir(FeiState* state, const char* name, FeiBytecodeList* chunk, int offset)
{
    uint8_t constant;
    uint8_t argcount;
    // get index of the name first
    constant = chunk->code[offset + 1];
    // then get number of arguments
    argcount = chunk->code[offset + 2];
    fprintf(stderr, "%-16s (%d args) %4d", name, argcount, constant);
    // print the method
    fei_value_printvalue(state, state->iowriter_stderr, fei_valarray_get(state, &chunk->constants, constant), true);
    fprintf(stderr, "\n");
    return offset + 3;
}

int fei_dbgutil_printjumpir(FeiState* state, const char* name, int sign, FeiBytecodeList* chunk, int offset)
{
    uint16_t jump;
    (void)state;
    jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    fprintf(stderr, "%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

void fei_dbgdisas_chunk(FeiState* state, FeiBytecodeList* chunk, const char* name)
{
    int offset;
    // print a little header for debugging
    fprintf(stderr, "== %s ==\n", name);
    // for every existing instruction in the chunk
    for(offset = 0; offset < chunk->count;)
    {
        // disassemble individually, offset will be controlled from this function
        offset = fei_dbgdisas_instr(state, chunk, offset);
    }
}

int fei_dbgdisas_instr(FeiState* state, FeiBytecodeList* chunk, int offset)
{
    int j;
    int index;
    int islocal;
    uint8_t constant;
    uint8_t instruction;
    ObjFunction* function;
    // print byte offset of the given instruction, or the index
    fprintf(stderr, "%04d ", offset);
    // show source line each instruction was compiled from
    // show a | for any instruction that comes from the
    //same source as its preceding one
    if(offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1])
    {
        fprintf(stderr, "    | ");
    }
    else
    {
        fprintf(stderr, "%4d ", chunk->lines[offset]);
    }
    instruction = chunk->code[offset];// takes one byte, or an element, from the container
    switch(instruction)
    {
        case OP_CONSTANT:
            {
                // pass in chunk to get ValArray element
                return fei_dbgutil_printconstir(state, "OP_CONSTANT", chunk, offset);
            }
            break;
        // literals
        case OP_NULL:
            {
                return fei_dbgutil_printsimpleir(state, "OP_NULL", offset);
            }
            break;
        case OP_TRUE:
            return fei_dbgutil_printsimpleir(state, "OP_TRUE", offset);
        case OP_FALSE:
            return fei_dbgutil_printsimpleir(state, "OP_FALSE", offset);

        case OP_EQUAL:
            return fei_dbgutil_printsimpleir(state, "OP_EQUAL", offset);
        case OP_GREATER:
            return fei_dbgutil_printsimpleir(state, "OP_GREATER", offset);
        case OP_LESS:
            return fei_dbgutil_printsimpleir(state, "OP+LESS", offset);

        // unary
        case OP_NEGATE:
            return fei_dbgutil_printsimpleir(state, "OP_NEGATE", offset);

        // binary
        case OP_ADD:
            return fei_dbgutil_printsimpleir(state, "OP_ADD", offset);
        case OP_SUBTRACT:
            return fei_dbgutil_printsimpleir(state, "OP_MINUS", offset);
        case OP_MULTIPLY:
            return fei_dbgutil_printsimpleir(state, "OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return fei_dbgutil_printsimpleir(state, "OP_DIVIDE", offset);
        case OP_MODULO:
            return fei_dbgutil_printsimpleir(state, "OP_MODULO", offset);

        case OP_NOT:
            return fei_dbgutil_printsimpleir(state, "OP_NOT", offset);

        case OP_POP:
            return fei_dbgutil_printsimpleir(state, "OP_POP", offset);

        // names for local variables do not get carried over, hence only the slot number is shown
        case OP_GET_LOCAL:
            return fei_dbgutil_printbyteir(state, "OP_GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return fei_dbgutil_printbyteir(state, "OP_SET_LOCAL", chunk, offset);

        case OP_GET_UPVALUE:
            return fei_dbgutil_printbyteir(state, "OP_GET_UPVALUE", chunk, offset);
        case OP_SET_UPVALUE:
            return fei_dbgutil_printbyteir(state, "OP_SET_UPVALUE", chunk, offset);
        case OP_GET_PROPERTY:
            return fei_dbgutil_printconstir(state, "OP_GET_PROPERTY", chunk, offset);
        case OP_SET_PROPERTY:
            return fei_dbgutil_printconstir(state, "OP_SET_PROPERTY", chunk, offset);

        case OP_CLOSE_UPVALUE:
            return fei_dbgutil_printsimpleir(state, "OP_CLOSE_VALUE", offset);

        case OP_DEFINE_GLOBAL:
            return fei_dbgutil_printsimpleir(state, "OP_DEFINE_GLOBAL", offset);
        case OP_GET_GLOBAL:
            return fei_dbgutil_printsimpleir(state, "OP_GET_GLOBAL", offset);
        case OP_SET_GLOBAL:
            return fei_dbgutil_printsimpleir(state, "OP_SET_GLOBAL", offset);
        case OP_PRINT:
            return fei_dbgutil_printsimpleir(state, "OP_PRINT", offset);

        case OP_SWITCH_EQUAL:
            return fei_dbgutil_printsimpleir(state, "OP_SWITCH_EQUAL", offset);

        case OP_JUMP:
            return fei_dbgutil_printjumpir(state, "OP_JUMP", 1, chunk, offset);
        case OP_JUMP_IF_FALSE:
            return fei_dbgutil_printjumpir(state, "OP_JUMP_IF_FALSE", 1, chunk, offset);

        case OP_CALL:
            return fei_dbgutil_printbyteir(state, "OP_CALL", chunk, offset);

        case OP_METHOD:
            return fei_dbgutil_printconstir(state, "OP_METHOD", chunk, offset);

        case OP_INVOKE:
            return fei_dbgutil_printinvokeir(state, "OP_INVOKE", chunk, offset);

        case OP_CLOSURE:
            {
                offset++;
                constant = chunk->code[offset++];// index for Value
                fprintf(stderr, "%-16s %4d ", "OP_CLOSURE", constant);
                fei_value_printvalue(state, state->iowriter_stderr, fei_valarray_get(state, &chunk->constants, constant), true);// accessing the value using the index
                fprintf(stderr, "\n");
                function = fei_value_asfunction(fei_valarray_get(state, &chunk->constants, constant));
                for(j = 0; j < function->upvaluecount; j++)// walk through upvalues
                {
                    islocal = chunk->code[offset++];
                    index = chunk->code[offset++];
                    fprintf(stderr, "%04d	|	%s %d\n", offset - 2, islocal ? "local" : "upvalue", index);
                }
                return offset;
            }
            break;
        case OP_CLASS:
            return fei_dbgutil_printconstir(state, "OP_CLASS", chunk, offset);

        case OP_INHERIT:
            return fei_dbgutil_printsimpleir(state, "OP_INEHEIRT", offset);


        case OP_GET_SUPER:// class inheritance
            return fei_dbgutil_printconstir(state, "OP_GET_SUPER", chunk, offset);

        case OP_SUPER_INVOKE:
            {
                return fei_dbgutil_printinvokeir(state, "OP_SUPER_INVOKE", chunk, offset);
            }
            break;
        case OP_RETURN:
            {
                return fei_dbgutil_printsimpleir(state, "OP_RETURN", offset);
            }
            break;
        case OP_LOOP:
            {
                return fei_dbgutil_printjumpir(state, "OP_LOOP", -1, chunk, offset);
            }
            break;
        case OP_LOOP_IF_TRUE:
            {
                return fei_dbgutil_printjumpir(state, "OP_LOOP_IF_TRUE", -1, chunk, offset);
            }
            break;
        case OP_LOOP_IF_FALSE:
            {
                return fei_dbgutil_printjumpir(state, "OP_LOOP_IF_FALSE", -1, chunk, offset);
            }
            break;

        case OP_SETINDEX:
            {
                return fei_dbgutil_printsimpleir(state, "OP_SETINDEX", offset);
            }
            break;
        case OP_GETINDEX:
            {
                return fei_dbgutil_printsimpleir(state, "OP_GETINDEX", offset);
            }
            break;
        case OP_MAKEARRAY:
            {
                return fei_dbgutil_printsimpleir(state, "OP_MAKEARRAY", offset);
            }
            break;

        default:
            {
                fprintf(stderr, "Unknown opcode %d\n", instruction);
                return offset + 1;
            }
            break;
    }
    return -1;
}



