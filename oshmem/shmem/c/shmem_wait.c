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
 * These routines force the calling PE to wait until var is no longer equal to value. 
 * A call to any shmem_wait() routine does not return until some other processor makes the
 * value at address var not equal to value.
 */
#define SHMEM_TYPE_WAIT(type_name, type, code)    \
    void shmem##type_name##_wait(type *addr, type value) \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(wait(                                    \
            (void*)addr,                                            \
            SHMEM_CMP_NE,                                           \
            (void*)&value,                                          \
            code));                                                 \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return ;                                                    \
    }

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_wait = pshmem_wait
#pragma weak shmem_short_wait = pshmem_short_wait
#pragma weak shmem_int_wait = pshmem_int_wait
#pragma weak shmem_long_wait = pshmem_long_wait
#pragma weak shmem_longlong_wait = pshmem_longlong_wait
#pragma weak shmem_int32_wait = pshmem_int32_wait
#pragma weak shmem_int64_wait = pshmem_int64_wait
#pragma weak shmem_wait_until = pshmem_wait_until
#pragma weak shmem_short_wait_until = pshmem_short_wait_until
#pragma weak shmem_int_wait_until = pshmem_int_wait_until
#pragma weak shmem_long_wait_until = pshmem_long_wait_until
#pragma weak shmem_longlong_wait_until = pshmem_longlong_wait_until
#pragma weak shmem_int32_wait_until = pshmem_int32_wait_until
#pragma weak shmem_int64_wait_until = pshmem_int64_wait_until
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_WAIT(, long, SHMEM_LONG)
SHMEM_TYPE_WAIT(_short, short, SHMEM_SHORT)
SHMEM_TYPE_WAIT(_int, int, SHMEM_INT)
SHMEM_TYPE_WAIT(_long, long, SHMEM_LONG)
SHMEM_TYPE_WAIT(_longlong, long long, SHMEM_LLONG)
SHMEM_TYPE_WAIT(_int32, int32_t, SHMEM_INT32_T)
SHMEM_TYPE_WAIT(_int64, int64_t, SHMEM_INT64_T)

#define SHMEM_TYPE_WAIT_UNTIL(type_name, type, code)    \
    void shmem##type_name##_wait_until(type *addr, int cmp, type value)   \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(wait(                                    \
            (void*)addr,                                            \
            cmp,                                                    \
            (void*)&value,                                          \
            code));                                                 \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return ;                                                    \
    }

SHMEM_TYPE_WAIT_UNTIL(, long, SHMEM_LONG)
SHMEM_TYPE_WAIT_UNTIL(_short, short, SHMEM_SHORT)
SHMEM_TYPE_WAIT_UNTIL(_int, int, SHMEM_INT)
SHMEM_TYPE_WAIT_UNTIL(_long, long, SHMEM_LONG)
SHMEM_TYPE_WAIT_UNTIL(_longlong, long long, SHMEM_LLONG)
SHMEM_TYPE_WAIT_UNTIL(_int32, int32_t, SHMEM_INT32_T)
SHMEM_TYPE_WAIT_UNTIL(_int64, int64_t, SHMEM_INT64_T)
