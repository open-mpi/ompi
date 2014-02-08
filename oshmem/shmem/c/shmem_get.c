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
SHMEM_TYPE_GETMEM(_get32, 4)
SHMEM_TYPE_GETMEM(_get64, 8)
SHMEM_TYPE_GETMEM(_get128, 16)

#if !defined(OSHMEM_PROFILING) || (OSHMEM_PROFILING == 0)
SHMEM_TYPE_GETMEM(_get, sizeof(long))
#endif /* OSHMEM_PROFILING */
