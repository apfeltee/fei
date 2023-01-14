
#pragma once

#ifndef JK_DYNAMIC_ARRAY_MAX
    #define JK_DYNAMIC_ARRAY_MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

#ifndef JK_DYNAMIC_ARRAY_MIN
    #define JK_DYNAMIC_ARRAY_MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#ifndef JK_DYNAMIC_ARRAY_ASSERT
    #define JK_DYNAMIC_ARRAY_ASSERT(cond) ((cond) ? 0 : ((*(volatile int*)0) = 123), 0)
#endif

#ifndef JK_DYNAMIC_ARRAY_SIZE_T
    #ifndef __cplusplus
        #if __STDC_VERSION__ < 199901L
            #define JK_DYNAMIC_ARRAY_SIZE_T unsigned int
        #else
            #define JK_DYNAMIC_ARRAY_SIZE_T size_t
        #endif
    #endif
#endif

#ifdef __clang__
    #define JK_DYNAMIC_ARRAY_UNUSED __attribute__((__unused__))
#else
    #define JK_DYNAMIC_ARRAY_UNUSED
#endif

#ifdef JK_DYNAMIC_ARRAY_EXTERN
    #define JK_DYNAMIC_ARRAY_DEF JK_DYNAMIC_ARRAY_UNUSED extern
#else
    #define JK_DYNAMIC_ARRAY_DEF JK_DYNAMIC_ARRAY_UNUSED static
#endif

#ifndef JK_DYNAMIC_ARRAY_REALLOC
    #define JK_DYNAMIC_ARRAY_REALLOC realloc
#endif

#ifndef JK_DYNAMIC_ARRAY_MEMCPY
    #include <string.h>
    #define JK_DYNAMIC_ARRAY_MEMCPY memcpy
#endif

#ifndef JK_DYNAMIC_ARRAY_MEMSET
    #include <string.h>
    #define JK_DYNAMIC_ARRAY_MEMSET memset
#endif

#ifndef JK_DYNAMIC_ARRAY_NO_PRINTF
    #ifndef JK_DYNAMIC_ARRAY_VSNPRINTF
        #include <stdio.h>
        #define JK_DYNAMIC_ARRAY_VSNPRINTF vsnprintf
    #endif
#endif

#define da_intern_getsetcount(arr) \
    ((((JK_DYNAMIC_ARRAY_SIZE_T*)(arr)) - 2)[0])

#define da_intern_getsetcapacity(arr) \
    ((((JK_DYNAMIC_ARRAY_SIZE_T*)(arr)) - 1)[0])

#define da_capacity(arr) \
    (da_intern_getsetcapacity(arr))

#define da_need_to_grow_internal(arr, n) \
    (!(arr) || (da_intern_getsetcount(arr) + (n)) >= da_intern_getsetcapacity(arr))

#define da_maybe_grow_internal(arr, n, sz) \
    (da_need_to_grow_internal((arr), (n)) ? (*((void**)&(arr)) = da_grow_internal((void*)(arr), (n), sz)) : 0)

#define da_make(arr, n, sz) ((arr) = da_grow_internal((arr), (n), sz))

#define da_count(arr) ((arr) ? da_intern_getsetcount(arr) : 0)

#define da_allocated(arr) ((arr) ? da_intern_getsetcapacity(arr) : 0)

#define da_last(arr) ((arr)[da_intern_getsetcount(arr) - 1])

#define da_push(arr, sz, ...) (da_maybe_grow_internal((arr), 1, sz), (arr)[da_intern_getsetcount(arr)++] = (__VA_ARGS__))

#define da_pushn(arr, n, sz) (da_maybe_grow_internal((arr), n, sz), da_intern_getsetcount(arr) += n)

#define da_pop(arr, sz) \
    (da_count(arr) ? JK_DYNAMIC_ARRAY_MEMSET((arr) + (--da_intern_getsetcount(arr)), 0, sz), 0 : 0)

#define da_popn(arr, n)                                                                                               \
    (da_count(arr) ?                                                                                                  \
     JK_DYNAMIC_ARRAY_MEMSET((arr) + (da_intern_getsetcount(arr) - JK_DYNAMIC_ARRAY_MIN((n), da_intern_getsetcount(arr))), 0, \
                             sizeof(*(arr)) * (JK_DYNAMIC_ARRAY_MIN((n), da_intern_getsetcount(arr)))),                   \
     da_intern_getsetcount(arr) -= JK_DYNAMIC_ARRAY_MIN((n), da_intern_getsetcount(arr)), 0 : 0)

#define da_destroy(arr) ((arr) ? free(((JK_DYNAMIC_ARRAY_SIZE_T*)(arr)) - 2), (arr) = NULL, 0 : 0)

#define da_clear(arr) \
    (da_count(arr) ? JK_DYNAMIC_ARRAY_MEMSET((arr), 0, sizeof(*(arr)) * da_intern_getsetcount(arr)), \
     da_intern_getsetcount(arr) = 0, 0 : 0);

#define da_grow(arr, n, sz) (((arr) = da_grow_internal((arr), (n), sz)), da_intern_getsetcount(arr) += (n))

#define da_remove_swap_last(arr, index)                                                                    \
    ((((index) >= 0) && (index) < da_intern_getsetcount(arr)) ?                                                \
     (JK_DYNAMIC_ARRAY_MEMCPY((arr) + (index), &da_last(arr), sizeof(*(arr))), --da_intern_getsetcount(arr)) : \
     0)

#define da_sizeof(arr) (sizeof(*(arr)) * da_count(arr))

JK_DYNAMIC_ARRAY_DEF void* da_grow_internal(void* arr, JK_DYNAMIC_ARRAY_SIZE_T count, JK_DYNAMIC_ARRAY_SIZE_T size)
{
    void* res = 0;
    JK_DYNAMIC_ARRAY_SIZE_T allocSize;
    JK_DYNAMIC_ARRAY_SIZE_T allocCount;

    allocCount = JK_DYNAMIC_ARRAY_MAX(2 * da_count(arr), da_count(arr) + count);
    allocSize = 2 * sizeof(JK_DYNAMIC_ARRAY_SIZE_T) + allocCount * size;
    if(arr)
    {
#if 0
        JK_DYNAMIC_ARRAY_SIZE_T* ptr = (JK_DYNAMIC_ARRAY_SIZE_T*)JK_DYNAMIC_ARRAY_REALLOC(((JK_DYNAMIC_ARRAY_SIZE_T*)arr)-2, allocSize);
#else
        JK_DYNAMIC_ARRAY_SIZE_T* ptr = (JK_DYNAMIC_ARRAY_SIZE_T*)malloc(allocSize);
        if(ptr)
        {
            JK_DYNAMIC_ARRAY_MEMCPY(ptr, ((JK_DYNAMIC_ARRAY_SIZE_T*)arr) - 2,
                                    da_count(arr) * size + 2 * sizeof(JK_DYNAMIC_ARRAY_SIZE_T));
            //free(((JK_DYNAMIC_ARRAY_SIZE_T*)arr) - 2);
            da_destroy(arr);
        }
#endif
        if(ptr)
        {
            JK_DYNAMIC_ARRAY_SIZE_T zeroSize = allocSize - 2 * sizeof(JK_DYNAMIC_ARRAY_SIZE_T) - ptr[0] * size;
            JK_DYNAMIC_ARRAY_MEMSET(((char*)ptr) + (allocSize - zeroSize), 0, zeroSize);
            res = ptr + 2;
            da_intern_getsetcapacity(res) = allocCount;
        }
        else
        {
            JK_DYNAMIC_ARRAY_ASSERT(0);
        }
    }
    else
    {
#if 0
        JK_DYNAMIC_ARRAY_SIZE_T* ptr = (JK_DYNAMIC_ARRAY_SIZE_T*)JK_DYNAMIC_ARRAY_REALLOC(0, allocSize);
#else
        JK_DYNAMIC_ARRAY_SIZE_T* ptr = (JK_DYNAMIC_ARRAY_SIZE_T*)malloc(allocSize);
#endif
        if(ptr)
        {
            res = ptr + 2;
            JK_DYNAMIC_ARRAY_MEMSET(ptr, 0, allocSize);
            da_intern_getsetcount(res) = 0;
            da_intern_getsetcapacity(res) = allocCount;
        }
        else
        {
            JK_DYNAMIC_ARRAY_ASSERT(0);
        }
    }

    JK_DYNAMIC_ARRAY_ASSERT(res);
    return res;
}

