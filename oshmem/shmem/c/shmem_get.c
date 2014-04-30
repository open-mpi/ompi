/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/spml/spml.h"

/*
 * These routines retrieve data from a contiguous data object on a remote PE. 
 * The shmem_get() routines transfer nelems elements of the data object at address source
 * on the remote PE (pe), to the data object at address target on the local PE. These routines
 * return after the data has been copied to address target on the local pe.
 */
#define SHMEM_TYPE_GET(type_name, type)    \
    void shmem##type_name##_get(type *target, const type *source, size_t nelems, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        size = nelems * sizeof(type);                               \
        rc = MCA_SPML_CALL(get(                                     \
            (void*)source,                                          \
            size,                                                   \
            (void*)target,                                          \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_char_get = pshmem_char_get
#pragma weak shmem_short_get = pshmem_short_get
#pragma weak shmem_int_get = pshmem_int_get
#pragma weak shmem_long_get = pshmem_long_get
#pragma weak shmem_longlong_get = pshmem_longlong_get
#pragma weak shmem_float_get = pshmem_float_get
#pragma weak shmem_double_get = pshmem_double_get
#pragma weak shmem_longdouble_get = pshmem_longdouble_get
#pragma weak shmem_getmem = pshmem_getmem
#pragma weak shmem_get16 = pshmem_get16
#pragma weak shmem_get32 = pshmem_get32
#pragma weak shmem_get64 = pshmem_get64
#pragma weak shmem_get128 = pshmem_get128
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_GET(_char, char)
SHMEM_TYPE_GET(_short, short)
SHMEM_TYPE_GET(_int, int)
SHMEM_TYPE_GET(_long, long)
SHMEM_TYPE_GET(_longlong, long long)
SHMEM_TYPE_GET(_float, float)
SHMEM_TYPE_GET(_double, double)
SHMEM_TYPE_GET(_longdouble, long double)

#define SHMEM_TYPE_GETMEM(name, element_size)    \
    void shmem##name(void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(get(                                     \
            (void*)source,                                          \
            size,                                                   \
            (void*)target,                                          \
            pe));                                                   \
       RUNTIME_CHECK_RC(rc);                                        \
                                                                    \
        return ;                                                    \
    }

SHMEM_TYPE_GETMEM(_getmem, 1)
SHMEM_TYPE_GETMEM(_get16, 2)
SHMEM_TYPE_GETMEM(_get32, 4)
SHMEM_TYPE_GETMEM(_get64, 8)
SHMEM_TYPE_GETMEM(_get128, 16)

SHMEM_TYPE_GETMEM(_get, sizeof(long))
