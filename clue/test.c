
typedef struct Value Value;

struct Value
{
    short tag;
    const char* str;
};


static Value stackvalues[100];
static Value* stackptr;

int main()
{

    Value newval = {0, "foo"};
    stackptr = stackvalues;
    *stackptr++ = newval;
}

