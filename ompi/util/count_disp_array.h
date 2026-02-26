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

#include "opal/util/count_disp_array.h"

/*
 * NOTE: This code chooses between 64-bit and 32-bit pointers by using the
 *       least significant bit as a flag (which is possible since these
 *       pointers will always be multiples of 4 or 8).
 */
typedef opal_count_array_t ompi_count_array_t;

#define OMPI_COUNT_ARRAY_NULL OPAL_COUNT_ARRAY_NULL

/* Initialize an int variant of the count array */
static inline void ompi_count_array_init(ompi_count_array_t *array, const int *data)
{
    opal_count_array_init(array, data);
}

/* Initialize a bigcount variant of the count array */
static inline void ompi_count_array_init_c(ompi_count_array_t *array, const size_t *data)
{
    opal_count_array_init_c(array, data);
}

#define OMPI_COUNT_ARRAY_INIT(array, data) OPAL_COUNT_ARRAY_INIT(array, data)


static inline ompi_count_array_t ompi_count_array_create(const int *data)
{
    return opal_count_array_create(data);
}

static inline ompi_count_array_t ompi_count_array_create_c(const size_t *data)
{
    return opal_count_array_create_c(data);
}

#define OMPI_COUNT_ARRAY_CREATE(data) OPAL_COUNT_ARRAY_CREATE(data)

/* Return if the internal type is 64-bit or not */
static inline bool ompi_count_array_is_64bit(ompi_count_array_t array)
{
    return opal_count_array_is_64bit(array);
}

static inline const void *ompi_count_array_ptr(ompi_count_array_t array)
{
    return opal_count_array_ptr(array);
}

/* Get a count in the array at index i */
static inline size_t ompi_count_array_get(ompi_count_array_t array, size_t i)
{
    return opal_count_array_get(array, i);
}

typedef opal_disp_array_t ompi_disp_array_t;

#define OMPI_DISP_ARRAY_NULL OPAL_DISP_ARRAY_NULL

/* Initialize an int variant of the disp array */
static inline void ompi_disp_array_init(ompi_disp_array_t *array, const int *data)
{
    opal_disp_array_init(array, data);
}

/* Initialize a bigcount variant of the disp array */
static inline void ompi_disp_array_init_c(ompi_disp_array_t *array, const ptrdiff_t *data)
{
    opal_disp_array_init_c(array, data);
}

#define OMPI_DISP_ARRAY_INIT(array, data) OPAL_DISP_ARRAY_INIT(array, data)

static inline ompi_disp_array_t ompi_disp_array_create(const int *data)
{
    return opal_disp_array_create(data);
}

static inline ompi_disp_array_t ompi_disp_array_create_c(const ptrdiff_t *data)
{
    return opal_disp_array_create_c(data);
}

#define OMPI_DISP_ARRAY_CREATE(data) OPAL_DISP_ARRAY_CREATE(data)

/* Return if the internal type is 64-bit or not */
static inline bool ompi_disp_array_is_64bit(ompi_disp_array_t array)
{
    return opal_disp_array_is_64bit(array);
}

/* Get a displacement in the array at index i */
static inline ptrdiff_t ompi_disp_array_get(ompi_disp_array_t array, size_t i)
{
    return opal_disp_array_get(array, i);
}

/* Get a direct pointer to the data */
static inline const void *ompi_disp_array_ptr(ompi_disp_array_t array)
{
    return opal_disp_array_ptr(array);
}

#endif
