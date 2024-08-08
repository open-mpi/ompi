/*
 * Copyright (c) 2024   Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef OMPI_UTIL_COUNT_DISP_ARRAY_H
#define OMPI_UTIL_COUNT_DISP_ARRAY_H

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>

/*
 * NOTE: This code chooses between 64-bit and 32-bit pointers by using the
 *       least significant bit as a flag (which is possible since these
 *       pointers will always be multiples of 4 or 8).
 */
typedef intptr_t ompi_count_array_t;

/* Initialize an int variant of the count array */
static inline void ompi_count_array_init(ompi_count_array_t *array, const int *data)
{
    *array = (intptr_t)data | 0x1L;
}

/* Initialize a bigcount variant of the count array */
static inline void ompi_count_array_init_c(ompi_count_array_t *array, const size_t *data)
{
    *array = (intptr_t)data;
}

#if OPAL_C_HAVE__GENERIC
#define OMPI_COUNT_ARRAY_INIT(array, data) _Generic((data), \
                                                    int *: ompi_count_array_init, \
                                                    const int *: ompi_count_array_init, \
                                                    size_t *: ompi_count_array_init_c, \
                                                    const size_t *: ompi_count_array_init_c)(array, data)
#else
#define OMPI_COUNT_ARRAY_INIT(array, data) \
    do { \
        if (sizeof(*(data)) == sizeof(int)) { \
            ompi_count_array_init(array, (const int *) (data)); \
        } else if (sizeof(*(data)) == sizeof(size_t)) { \
            ompi_count_array_init_c(array, (const size_t *) (data)); \
        } \
    } while (0)
#endif

/* Return if the internal type is 64-bit or not */
static inline bool ompi_count_array_is_64bit(ompi_count_array_t array)
{
    return !(array & 0x1L) && sizeof(size_t) == 8;
}

static inline const void *ompi_count_array_ptr(ompi_count_array_t array)
{
    if (OPAL_LIKELY(array & 0x1L)){
        return (const void *)(array & ~0x1L);
    }
    return (const void *) array;
}

/* Get a count in the array at index i */
static inline size_t ompi_count_array_get(ompi_count_array_t array, size_t i)
{
    if (OPAL_LIKELY(array & 0x1L)){
        const int *iptr = (const int *)(array & ~0x1L);
        return iptr[i];
    }
    return ((const size_t *)array)[i];
}

typedef intptr_t ompi_disp_array_t;

/* Initialize an int variant of the disp array */
static inline void ompi_disp_array_init(ompi_disp_array_t *array, const int *data)
{
    *array = (intptr_t)data | 0x1L;
}

/* Initialize a bigcount variant of the disp array */
static inline void ompi_disp_array_init_c(ompi_disp_array_t *array, const ptrdiff_t *data)
{
    *array = (intptr_t)data;
}

#if OPAL_C_HAVE__GENERIC
#define OMPI_DISP_ARRAY_INIT(array, data) _Generic((data), \
                                                   int *: ompi_disp_array_init, \
                                                   const int *: ompi_disp_array_init, \
                                                   ptrdiff_t *: ompi_disp_array_init_c, \
                                                   const ptrdiff_t *: ompi_disp_array_init_c)(array, data)
#else
#define OMPI_DISP_ARRAY_INIT(array, data) \
    do { \
        if (sizeof(*(data)) == sizeof(int)) { \
            ompi_disp_array_init(array, (const int *) (data)); \
        } else if (sizeof(*(data)) == sizeof(ptrdiff_t)) { \
            ompi_disp_array_init_c(array, (const ptrdiff_t *) (data)); \
        } \
    } while(0)
#endif

/* Return if the internal type is 64-bit or not */
static inline bool ompi_disp_array_is_64bit(ompi_disp_array_t array)
{
    return !(array & 0x1L) && sizeof(ptrdiff_t) == 8;
}

/* Get a displacement in the array at index i */
static inline ptrdiff_t ompi_disp_array_get(ompi_disp_array_t array, size_t i)
{
    if (OPAL_LIKELY(array & 0x1L)){
        const int *iptr = (const int *)(array & ~0x1L);
        return iptr[i];
    }
    return ((const ptrdiff_t *)array)[i];
}

/* Get a direct pointer to the data */
static inline const void *ompi_disp_array_ptr(ompi_disp_array_t array)
{
    if (OPAL_LIKELY(array & 0x1L)){
        return (const void *)(array & ~0x1L);
    }
    return (const void *)array;
}

#endif
