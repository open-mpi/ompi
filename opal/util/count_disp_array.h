/*
 * Copyright (c) 2024   Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2025   Stony Brook University.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef OPAL_UTIL_COUNT_DISP_ARRAY_H
#define OPAL_UTIL_COUNT_DISP_ARRAY_H

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include "opal_config.h"

/*
 * NOTE: This code chooses between 64-bit and 32-bit pointers by using the
 *       least significant bit as a flag (which is possible since these
 *       pointers will always be multiples of 4 or 8).
 */
typedef intptr_t opal_count_array_t;

/**
 * Sanity check to make sure the compiler respects the alignment assumptions
 * that allow us to use the least significant bit as a flag.
 */
_Static_assert(_Alignof(int) >= 2, "int alignment assumption violated");
_Static_assert(_Alignof(size_t) >= 2, "size_t alignment assumption violated");

#define OPAL_COUNT_ARRAY_NULL ((opal_count_array_t)0)

/**
 * Initialize an int variant of the count array.
 * Assumes is aligned to at least 2 bytes (i.e., least significant bit is 0).
 */
static inline void opal_count_array_init(opal_count_array_t *array, const int *data)
{
    assert(((intptr_t)data & 0x1L) == 0);
    *array = (intptr_t)data | 0x1L;
}

/* Initialize a bigcount variant of the count array */
static inline void opal_count_array_init_c(opal_count_array_t *array, const size_t *data)
{
    *array = (intptr_t)data;
}

#if OPAL_C_HAVE__GENERIC
#define OPAL_COUNT_ARRAY_INIT(array, data) _Generic((data), \
                                                    int *: opal_count_array_init, \
                                                    const int *: opal_count_array_init, \
                                                    size_t *: opal_count_array_init_c, \
                                                    const size_t *: opal_count_array_init_c, \
                                                    MPI_Count *: opal_count_array_init_c, \
                                                    const MPI_Count *: opal_count_array_init_c)(array, (const void *) data)
#else
#define OPAL_COUNT_ARRAY_INIT(array, data) \
    do { \
        if (sizeof(*(data)) == sizeof(int)) { \
            opal_count_array_init(array, (const int *) (data)); \
        } else if (sizeof(*(data)) == sizeof(size_t)) { \
            opal_count_array_init_c(array, (const size_t *) (data)); \
        } \
    } while (0)
#endif


static inline opal_count_array_t opal_count_array_create(const int *data)
{
    opal_count_array_t array;
    opal_count_array_init(&array, data);
    return array;
}

static inline opal_count_array_t opal_count_array_create_c(const size_t *data)
{
    opal_count_array_t array;
    opal_count_array_init_c(&array, data);
    return array;
}

static inline opal_count_array_t opal_count_array_create_with_size(const void *data, size_t size)
{
    if (size == sizeof(int)) {
        return opal_count_array_create(data);
    } else {
        return opal_count_array_create_c(data);
    }
}

#define OPAL_COUNT_ARRAY_CREATE(data) opal_count_array_create_with_size((data), sizeof(*(data)))


/* Return if the internal type is 64-bit or not */
static inline bool opal_count_array_is_64bit(opal_count_array_t array)
{
    return !(array & 0x1L) && sizeof(size_t) == 8;
}

static inline size_t opal_count_array_sizeof(opal_count_array_t array)
{
    return opal_count_array_is_64bit(array) ? sizeof(size_t) : sizeof(int);
}

static inline const void *opal_count_array_ptr(opal_count_array_t array)
{
    if (OPAL_LIKELY(array & 0x1L)){
        return (const void *)(array & ~0x1L);
    }
    return (const void *) array;
}

/* Get a count in the array at index i */
static inline size_t opal_count_array_get(opal_count_array_t array, size_t i)
{
    if (OPAL_LIKELY(array & 0x1L)){
        const int *iptr = (const int *)(array & ~0x1L);
        return (size_t)iptr[i];
    }
    return ((const size_t *)array)[i];
}

/* Set a count in the array at index i */
static inline void opal_count_array_set(opal_count_array_t array, size_t i, size_t val)
{
    if (OPAL_LIKELY(array & 0x1L)){
        int *iptr = (int *)(array & ~0x1L);
        iptr[i] = (int)val;
    } else {
        size_t *sptr = (size_t *)array;
        sptr[i] = val;
    }
}

typedef intptr_t opal_disp_array_t;

#define OPAL_DISP_ARRAY_NULL ((opal_disp_array_t)0)

/* Initialize an int variant of the disp array */
static inline void opal_disp_array_init(opal_disp_array_t *array, const int *data)
{
    *array = (intptr_t)data | 0x1L;
}

/* Initialize a bigcount variant of the disp array */
static inline void opal_disp_array_init_c(opal_disp_array_t *array, const ptrdiff_t *data)
{
    *array = (intptr_t)data;
}

#if OPAL_C_HAVE__GENERIC
#define OPAL_DISP_ARRAY_INIT(array, data) _Generic((data), \
                                                   int *: opal_disp_array_init, \
                                                   const int *: opal_disp_array_init, \
                                                   ptrdiff_t *: opal_disp_array_init_c, \
                                                   const ptrdiff_t *: opal_disp_array_init_c)(array, data)
#else
#define OPAL_DISP_ARRAY_INIT(array, data) \
    do { \
        if (sizeof(*(data)) == sizeof(int)) { \
            opal_disp_array_init(array, (const int *) (data)); \
        } else if (sizeof(*(data)) == sizeof(ptrdiff_t)) { \
            opal_disp_array_init_c(array, (const ptrdiff_t *) (data)); \
        } \
    } while(0)
#endif



static inline opal_disp_array_t opal_disp_array_create(const int *data)
{
    opal_disp_array_t array;
    opal_disp_array_init(&array, data);
    return array;
}

static inline opal_disp_array_t opal_disp_array_create_c(const ptrdiff_t *data)
{
    opal_disp_array_t array;
    opal_disp_array_init_c(&array, data);
    return array;
}

static inline opal_disp_array_t opal_disp_array_create_with_size(const void *data, size_t size)
{
    if (size == sizeof(int)) {
        return opal_disp_array_create(data);
    } else {
        return opal_disp_array_create_c(data);
    }
}

#define OPAL_DISP_ARRAY_CREATE(data) opal_disp_array_create_with_size(data, sizeof(*(data)))


/* Return if the internal type is 64-bit or not */
static inline bool opal_disp_array_is_64bit(opal_disp_array_t array)
{
    return !(array & 0x1L) && sizeof(ptrdiff_t) == 8;
}

static inline size_t opal_disp_array_sizeof(opal_disp_array_t array)
{
    return opal_disp_array_is_64bit(array) ? sizeof(ptrdiff_t) : sizeof(int);
}

/* Get a displacement in the array at index i */
static inline ptrdiff_t opal_disp_array_get(opal_disp_array_t array, size_t i)
{
    if (OPAL_LIKELY(array & 0x1L)){
        const int *iptr = (const int *)(array & ~0x1L);
        return iptr[i];
    }
    return ((const ptrdiff_t *)array)[i];
}

/* Set a displacement in the array at index i */
static inline void opal_disp_array_set(opal_disp_array_t array, size_t i, ptrdiff_t val)
{
    if (OPAL_LIKELY(array & 0x1L)){
        int *iptr = (int *)(array & ~0x1L);
        iptr[i] = (int)val;
    } else {
        ptrdiff_t *pptr = (ptrdiff_t *)array;
        pptr[i] = val;
    }
}


/* Get a direct pointer to the data */
static inline const void *opal_disp_array_ptr(opal_disp_array_t array)
{
    if (OPAL_LIKELY(array & 0x1L)){
        return (const void *)(array & ~0x1L);
    }
    return (const void *)array;
}

#endif
