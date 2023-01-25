
#include "fei.h"

char* readhandle(FILE* hnd, size_t* dlen)
{
    long rawtold;
    /*
    * the value returned by ftell() may not necessarily be the same as
    * the amount that can be read.
    * since we only ever read a maximum of $toldlen, there will
    * be no memory trashing.
    */
    size_t toldlen;
    size_t actuallen;
    char* buf;
    if(fseek(hnd, 0, SEEK_END) == -1)
    {
        return NULL;
    }
    if((rawtold = ftell(hnd)) == -1)
    {
        return NULL;
    }
    toldlen = rawtold;
    if(fseek(hnd, 0, SEEK_SET) == -1)
    {
        return NULL;
    }
    buf = (char*)malloc(toldlen + 1);
    memset(buf, 0, toldlen+1);
    if(buf != NULL)
    {
        actuallen = fread(buf, sizeof(char), toldlen, hnd);
        if(dlen != NULL)
        {
            *dlen = actuallen;
        }
        return buf;
    }
    return NULL;
}

char* readfile(const char* filename, size_t* dlen)
{
    char* b;
    FILE* fh;
    if((fh = fopen(filename, "rb")) == NULL)
    {
        return NULL;
    }
    b = readhandle(fh, dlen);
    fclose(fh);
    return b;
}

#if defined(FEI_HAVE_READLINE)
void repl(FeiState* state)
{
    char* line;
    while(true)
    {
        line = readline(">> ");
        if(line == NULL)
        {
            break;
        }
        fei_vm_evalsource(state, line, strlen(line));
        add_history(line);
    }
}
#endif

void runfile(FeiState* state, const char* path)
{
    size_t len;
    char* source;
    source = readfile(path, &len);
    ResultCode result = fei_vm_evalsource(state, source, len);
    free(source);
    if(result == STATUS_SYNTAXERROR)
    {
        exit(51);
    }
    if(result == STATUS_RTERROR)
    {
        exit(61);
    }
}

enum
{
    MAX_RESTARGS = 1024,
    MAX_OPTS = 1024,
};

typedef struct Flag_t Flag_t;
typedef struct FlagContext_t FlagContext_t;
typedef struct Options_t Options_t;


struct Flag_t
{
    char flag;
    char* value;
};

struct FlagContext_t
{
    int nargc;
    int fcnt;
    int poscnt;
    char* positional[MAX_RESTARGS + 1];
    Flag_t flags[MAX_OPTS + 1];
};

struct Options_t
{
    char* debugmode;
    char* codeline;
};


#define ptyp(t) \
    fprintf(stderr, "%d\tsizeof(%s)\n", (int)sizeof(t), #t)

static bool populate_flags(int argc, int begin, char** argv, const char* expectvalue, FlagContext_t* fx)
{
    int i;
    int nextch;
    int psidx;
    int flidx;
    char* arg;
    char* nextarg;
    psidx = 0;
    flidx = 0;
    fx->fcnt = 0;
    fx->poscnt = 0;
    for(i=begin; i<argc; i++)
    {
        arg = argv[i];
        nextarg = NULL;
        if((i+1) < argc)
        {
            nextarg = argv[i+1];
        }
        if(arg[0] == '-')
        {
            fx->flags[flidx].flag = arg[1];
            fx->flags[flidx].value = NULL;
            if(strchr(expectvalue, arg[1]) != NULL)
            {
                nextch = arg[2];
                /* -e "somecode(...)" */
                /* -e is followed by text: -e"somecode(...)" */
                if(nextch != 0)
                {
                    fx->flags[flidx].value = arg + 2;
                }
                else if(nextarg != NULL)
                {
                    if(nextarg[0] != '-')
                    {
                        fx->flags[flidx].value = nextarg;
                        i++;
                    }
                }
                else
                {
                    fx->flags[flidx].value = NULL;
                }
            }
            flidx++;
        }
        else
        {
            fx->positional[psidx] = arg;
            psidx++;
        }
    }
    fx->fcnt = flidx;
    fx->poscnt = psidx;
    fx->nargc = i;
    return true;
}

static void show_help()
{
    printf("lit [options] [files]\n");
    printf("    -o --output [file]  Instead of running the file the compiled bytecode will be saved.\n");
    printf(" -O[name] [string] Enables given optimization. For the list of aviable optimizations run with -Ohelp\n");
    printf(" -D[name]  Defines given symbol.\n");
    printf(" -e --eval [string] Runs the given code string.\n");
    printf(" -p --pass [args] Passes the rest of the arguments to the script.\n");
    printf(" -i --interactive Starts an interactive shell.\n");
    printf(" -d --dump  Dumps all the bytecode chunks from the given file.\n");
    printf(" -t --time  Measures and prints the compilation timings.\n");
    printf(" -h --help  I wonder, what this option does.\n");
    printf(" If no code to run is provided, lit will try to run either main.lbc or main.lit and, if fails, default to an interactive shell will start.\n");
}

static bool parse_options(Options_t* opts, Flag_t* flags, int fcnt)
{
    int i;
    opts->codeline = NULL;
    opts->debugmode = NULL;
    for(i=0; i<fcnt; i++)
    {
        switch(flags[i].flag)
        {
            case 'h':
                {
                    show_help();
                    return false;
                }
                break;
            case 'e':
                {
                    if(flags[i].value == NULL)
                    {
                        fprintf(stderr, "flag '-e' expects a string\n");
                        return false;
                    }
                    opts->codeline = flags[i].value;
                }
                break;
            case 'd':
                {
                    if(flags[i].value == NULL)
                    {
                        fprintf(stderr, "flag '-d' expects a value. run '-h' for possible values\n");
                        return false;
                    }
                    opts->debugmode = flags[i].value;
                }
                break;
            default:
                break;
        }
    }
    return true;
}


int main(int argc, char* argv[])
{
    bool cmdfailed;
    const char* dm;
    const char* filename;
    FeiState* state;
    FlagContext_t fx;
    Options_t opts;
    cmdfailed = false;
    state = fei_state_init();
    populate_flags(argc, 1, argv, "ed", &fx);
    #if 0
    {
        ptyp(FeiValue);
        ptyp(FeiObject);
        ptyp(ObjString);
        ptyp(ObjFunction);
        ptyp(ObjClass);
        ptyp(ObjInstance);
        ptyp(ObjBoundMethod);
        ptyp(ObjUpvalue);
        ptyp(ObjClosure);
        ptyp(ObjNative);
        ptyp(FeiAstLocal);
        ptyp(FeiAstUpvalue);
        ptyp(ASTState);
        ptyp(GCState);
        ptyp(VMState);
        ptyp(FeiState);
        ptyp(FeiVMFrame);
        ptyp(FeiAstToken);
        ptyp(FeiAstLexer);
        ptyp(FeiAstParser);
        ptyp(FeiAstRule);
        ptyp(FeiAstCompiler);
        ptyp(ClassCompiler);
        ptyp(ValArray);
        ptyp(FeiBytecodeList);
        ptyp(TabEntry);
        ptyp(Table);
    }
    #endif
    if(!parse_options(&opts, fx.flags, fx.fcnt))
    {
        cmdfailed = true;
    }
    else
    {
        if(opts.debugmode != NULL)
        {
            dm = opts.debugmode;
            if(strcmp(dm, "bc") == 0)
            {
                state->config.traceinstructions = true;
            }
            /*
            else if(strcmp(dm, "ast") == 0)
            {
                state->config.dumpast = true;
            }
            */
            else
            {
                fprintf(stderr, "unrecognized dump mode '%s'\n", dm);
                cmdfailed = true;
            }
        }
    }
    if(!cmdfailed)
    {

        if((fx.poscnt > 0) || (opts.codeline != NULL))
        {
            if(opts.codeline)
            {
                fei_vm_evalsource(state, opts.codeline, strlen(opts.codeline));
            }
            else
            {
                filename = fx.positional[0];
                runfile(state, filename);
            }
        }
        else
        {
            #if defined(LIT_HAVE_READLINE)
                repl(state);
            #else
                fprintf(stderr, "no repl support compiled in\n");
            #endif
        }
    }
    fei_state_destroy(state);
    return 0;
}
