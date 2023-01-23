
#include <math.h>
#include "fei.h"

// comparison function used in VM run()
// used in ALL types of data(num, string, bools)
bool fei_value_compare(FeiState* state, FeiValue a, FeiValue b)
{
    (void)state;
    if(a.type != b.type)
    {
        return false;
    }
    switch(a.type)
    {
        case VAL_BOOL:
            {
                return fei_value_asbool(a) == fei_value_asbool(b);
            }
            break;
        case VAL_NUMBER:
            {
                if(a.isfixednumber && b.isfixednumber)
                {
                    return fei_value_asfixednumber(a) == fei_value_asfixednumber(b);
                }
                return fei_value_asfloatnumber(a) == fei_value_asfloatnumber(b);
            }
            break;
        case VAL_NULL:
            {
                // true for all nulls
                return true;
            }
            break;
        case VAL_OBJ:
            {
                // already interned, occupies the same address
                return fei_value_asobj(a) == fei_value_asobj(b);
            }
            break;
        default:
            {
            }
            break;
    }
    return false;
}
