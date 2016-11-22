/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
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
#define SHMEM_TYPE_GET_NB(type_name, type)    \
    void shmem##type_name##_get_nbi(type *target, const type *source, size_t nelems, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        size = nelems * sizeof(type);                               \
        rc = MCA_SPML_CALL(get_nb(                                  \
            (void *)source,                                         \
            size,                                                   \
            (void *)target,                                         \
            pe, NULL));                                             \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_char_get_nbi = pshmem_char_get_nbi
#pragma weak shmem_short_get_nbi = pshmem_short_get_nbi
#pragma weak shmem_int_get_nbi = pshmem_int_get_nbi
#pragma weak shmem_long_get_nbi = pshmem_long_get_nbi
#pragma weak shmem_longlong_get_nbi = pshmem_longlong_get_nbi
#pragma weak shmem_float_get_nbi = pshmem_float_get_nbi
#pragma weak shmem_double_get_nbi = pshmem_double_get_nbi
#pragma weak shmem_longdouble_get_nbi = pshmem_longdouble_get_nbi
#pragma weak shmem_get8_nbi = pshmem_get8_nbi
#pragma weak shmem_get16_nbi = pshmem_get16_nbi
#pragma weak shmem_get32_nbi = pshmem_get32_nbi
#pragma weak shmem_get64_nbi = pshmem_get64_nbi
#pragma weak shmem_get128_nbi = pshmem_get128_nbi
#pragma weak shmem_getmem_nbi = pshmem_getmem_nbi
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_GET_NB(_char, char)
SHMEM_TYPE_GET_NB(_short, short)
SHMEM_TYPE_GET_NB(_int, int)
SHMEM_TYPE_GET_NB(_long, long)
SHMEM_TYPE_GET_NB(_longlong, long long)
SHMEM_TYPE_GET_NB(_float, float)
SHMEM_TYPE_GET_NB(_double, double)
SHMEM_TYPE_GET_NB(_longdouble, long double)

#define SHMEM_TYPE_GETMEM_NB(name, element_size, prefix)    \
    void prefix##name##_nbi(void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(source);                                 \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(get_nb(                                  \
            (void *)source,                                         \
            size,                                                   \
            (void *)target,                                         \
            pe, NULL));                                             \
       RUNTIME_CHECK_RC(rc);                                        \
                                                                    \
        return ;                                                    \
    }

SHMEM_TYPE_GETMEM_NB(_get8, 1, shmem)
SHMEM_TYPE_GETMEM_NB(_get16, 2, shmem)
SHMEM_TYPE_GETMEM_NB(_get32, 4, shmem)
SHMEM_TYPE_GETMEM_NB(_get64, 8, shmem)
SHMEM_TYPE_GETMEM_NB(_get128, 16, shmem)
SHMEM_TYPE_GETMEM_NB(_getmem, 1, shmem)
