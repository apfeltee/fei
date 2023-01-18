
#pragma once

#include <string.h>
#include <assert.h>

#define JK_DYNAMIC_ARRAY_MAX(a, b) ((a) > (b) ? (a) : (b))

#define JK_DYNAMIC_ARRAY_MIN(a, b) ((a) < (b) ? (a) : (b))


#ifndef JK_DYNAMIC_ARRAY_REALLOC
    #define JK_DYNAMIC_ARRAY_REALLOC realloc
#endif

#ifndef JK_DYNAMIC_ARRAY_MEMCPY
    #define JK_DYNAMIC_ARRAY_MEMCPY memcpy
#endif

#ifndef JK_DYNAMIC_ARRAY_MEMSET
    #define JK_DYNAMIC_ARRAY_MEMSET memset
#endif

/* clang-format off */
#define _da_if(...) (__VA_ARGS__) ?
#define _da_then(...) (__VA_ARGS__) :
#define _da_else(...) (__VA_ARGS__)


#define da_intern_getsetcount(arr) \
    ((((size_t*)(arr)) - 2)[0])

#define da_intern_getsetcapacity(arr) \
    ((((size_t*)(arr)) - 1)[0])

#define da_capacity(arr) \
    (da_intern_getsetcapacity(arr))

#define da_need_to_grow_internal(arr, n) \
    (!(arr) || (da_intern_getsetcount(arr) + (n)) >= da_intern_getsetcapacity(arr))

#define da_maybe_grow_internal(arr, n, sz) \
    (\
        _da_if(da_need_to_grow_internal((arr), (n))) \
        _da_then( \
            (*((void**)&(arr)) = da_grow_internal((void*)(arr), (n), sz)) \
        ) \
        _da_else(0)\
    )

#define da_make(arr, n, sz) \
    ( \
        da_grow_internal((arr), (n), sz) \
    )

#define da_count(arr) \
    (\
        _da_if((arr)) \
        _da_then(da_intern_getsetcount(arr)) \
        _da_else(0) \
    )

#define da_allocated(arr) \
    ( \
        _da_if((arr)) \
        _da_then(da_intern_getsetcapacity(arr)) \
        _da_else(0) \
    )

#define da_last(arr) \
    (\
        (arr)[da_intern_getsetcount(arr) - 1] \
    )

#define da_push(arr, sz, ...) \
    ( \
        da_maybe_grow_internal((arr), 1, sz), (arr)[da_intern_getsetcount(arr)++] = (__VA_ARGS__) \
    )

#define da_pushn(arr, n, sz) \
    ( \
        da_maybe_grow_internal((arr), n, sz), da_intern_getsetcount(arr) += n \
    )

#define da_pop(arr, sz) \
    ( \
        _da_if(da_count(arr)) \
        _da_then(\
            JK_DYNAMIC_ARRAY_MEMSET((arr) + (--da_intern_getsetcount(arr)), 0, sz), \
            0 \
        ) \
        _da_else(0) \
    )

#define da_popn(arr, n)                                                                                               \
    (\
        _da_if(da_count(arr))                                                                                                   \
        _da_then( \
            JK_DYNAMIC_ARRAY_MEMSET( \
                (arr) + (da_intern_getsetcount(arr) - JK_DYNAMIC_ARRAY_MIN((n), \
                da_intern_getsetcount(arr))), 0, \
                sizeof(*(arr)) * (JK_DYNAMIC_ARRAY_MIN((n), \
                da_intern_getsetcount(arr)))),                   \
            da_intern_getsetcount(arr) -= JK_DYNAMIC_ARRAY_MIN((n), da_intern_getsetcount(arr)), \
            0 \
        ) \
        _da_else(0) \
    )

#define da_destroy(arr) \
    ( \
        _da_if((arr)) \
        _da_then( \
            free(((size_t*)(arr)) - 2), \
            (arr) = NULL, \
            0\
        ) \
        _da_else(0) \
    )

#define da_clear(arr) \
    (\
        _da_if(da_count(arr)) \
        _da_then( \
            JK_DYNAMIC_ARRAY_MEMSET((arr), 0, sizeof(*(arr)) * da_intern_getsetcount(arr)), \
            da_intern_getsetcount(arr) = 0, \
            0 \
        ) \
        _da_else(0) \
    )

#define da_grow(arr, n, sz) \
    ( \
        ((arr) = da_grow_internal((arr), (n), sz)), \
        da_intern_getsetcount(arr) += (n) \
    )

#define da_remove_swap_last(arr, index) \
    ( \
        _da_if(((index) >= 0) && (index) < da_intern_getsetcount(arr)) \
        _da_then(JK_DYNAMIC_ARRAY_MEMCPY((arr) + (index), &da_last(arr), sizeof(*(arr))), --da_intern_getsetcount(arr)) \
        _da_else(0) \
    )

#define da_sizeof(arr) \
    ( \
        sizeof(*(arr)) * da_count(arr) \
    )
/* clang-format on */

static inline void* da_grow_internal(void* arr, size_t count, size_t typsz)
{
    void* res;
    size_t* ptr;
    size_t zerosz;
    size_t alloccapacity;
    size_t allocamount;
    res = NULL;
    allocamount = JK_DYNAMIC_ARRAY_MAX(2 * da_count(arr), da_count(arr) + count);
    alloccapacity = 2 * sizeof(size_t) + allocamount * typsz;
    if(arr)
    {
        ptr = (size_t*)malloc(alloccapacity);
        if(ptr)
        {
            JK_DYNAMIC_ARRAY_MEMCPY(ptr, ((size_t*)arr) - 2, da_count(arr) * typsz + 2 * sizeof(size_t));
            //free(((size_t*)arr) - 2);
            da_destroy(arr);
        }
        if(ptr)
        {
            zerosz = alloccapacity - 2 * sizeof(size_t) - ptr[0] * typsz;
            JK_DYNAMIC_ARRAY_MEMSET(((char*)ptr) + (alloccapacity - zerosz), 0, zerosz);
            res = ptr + 2;
            da_intern_getsetcapacity(res) = allocamount;
        }
        else
        {
            assert(0 && "ptr is null after reallocating");
        }
    }
    else
    {
        ptr = (size_t*)malloc(alloccapacity);
        if(ptr)
        {
            res = ptr + 2;
            JK_DYNAMIC_ARRAY_MEMSET(ptr, 0, alloccapacity);
            da_intern_getsetcount(res) = 0;
            da_intern_getsetcapacity(res) = allocamount;
        }
        else
        {
            assert(0 && "ptr is null after initial allocating");
        }
    }
    assert(res && "res is null");
    return res;
}

