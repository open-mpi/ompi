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
 * These routines provide a low latency mechanism to write basic types (short,
 * int, float, double, long) to symmetric data objects on remote PEs. 
 * The shmem_TYPE_p() routines write value to a symmetric array element or scalar
 * data object of the remote PE indicated by the parameter pe. These routines start the remote
 * transfer and may return before the data is delivered to the remote PE.
 */
#define SHMEM_TYPE_P(type_name, type)    \
    void shmem##type_name##_p(type *addr, type value, int pe) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        size_t size = 0;                                            \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
        RUNTIME_CHECK_PE(pe);                                       \
        RUNTIME_CHECK_ADDR(addr);                                   \
                                                                    \
        size = sizeof(type);                                        \
        rc = MCA_SPML_CALL(put(                                     \
            (void*)addr,                                            \
            size,                                                   \
            (void*)&value,                                          \
            pe));                                                   \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return ;                                                    \
    }

SHMEM_TYPE_P(_short, short)
SHMEM_TYPE_P(_int, int)
SHMEM_TYPE_P(_long, long)
SHMEM_TYPE_P(_longlong, long long)
SHMEM_TYPE_P(_float, float)
SHMEM_TYPE_P(_double, double)
SHMEM_TYPE_P(_longdouble, long double)
