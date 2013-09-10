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
 * These routines copy contiguous data from a local object to an object on the destination PE. 
 * These routines transfer nelems elements of the data object at address source on the calling
 * PE, to the data object at address target on the remote PE pe. These routines start the
 * remote transfer and may return before the data is delivered to the remote PE. The delivery
 * of data into the data object on the destination PE from different put calls may occur in any
 * order. Because of this, two successive put operations may deliver data out of order unless a
 * call to shmem_fence() is introduced between the two calls.
 */
#define SHMEM_TYPE_PUT(type_name, type)    \
    void shmem##type_name##_put(type *target, const type *source, size_t len, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = len * sizeof(type);                                  \
        rc = MCA_SPML_CALL(put(                                     \
            (void*)target,                                          \
            size,                                                   \
            (void*)source,                                          \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return ;                                                    \
    }

SHMEM_TYPE_PUT(_char, char)
SHMEM_TYPE_PUT(_short, short)
SHMEM_TYPE_PUT(_int, int)
SHMEM_TYPE_PUT(_long, long)
SHMEM_TYPE_PUT(_longlong, long long)
SHMEM_TYPE_PUT(_float, float)
SHMEM_TYPE_PUT(_double, double)
SHMEM_TYPE_PUT(_longdouble, long double)

#define SHMEM_TYPE_PUTMEM(name, element_size)    \
    void shmem##name(void *target, const void *source, size_t nelems, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(target);                                 \
                                                                    \
        size = nelems * element_size;                               \
        rc = MCA_SPML_CALL(put(                                     \
            (void*)target,                                          \
            size,                                                   \
            (void*)source,                                          \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return ;                                                    \
    }

SHMEM_TYPE_PUTMEM(_putmem, 1)
SHMEM_TYPE_PUTMEM(_put32, 4)
SHMEM_TYPE_PUTMEM(_put64, 8)
SHMEM_TYPE_PUTMEM(_put128, 16)

#if !defined(OSHMEM_PROFILING) || (OSHMEM_PROFILING == 0)
SHMEM_TYPE_PUTMEM(_put, sizeof(long))
#endif /* OSHMEM_PROFILING */
