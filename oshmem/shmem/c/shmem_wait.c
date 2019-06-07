/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"
#include "oshmem/include/shmemx.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/spml/spml.h"

/*
 * These routines force the calling PE to wait until var is no longer equal to value.
 * A call to any shmem_wait() routine does not return until some other processor makes the
 * value at address var not equal to value.
 */
#define SHMEM_TYPE_WAIT(type_name, type, code, prefix)    \
    void prefix##type_name##_wait(type *addr, type value) \
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
#pragma weak shmemx_int32_wait = pshmemx_int32_wait
#pragma weak shmemx_int64_wait = pshmemx_int64_wait

#pragma weak shmem_short_wait_until = pshmem_short_wait_until
#pragma weak shmem_int_wait_until = pshmem_int_wait_until
#pragma weak shmem_long_wait_until = pshmem_long_wait_until
#pragma weak shmem_longlong_wait_until = pshmem_longlong_wait_until
#pragma weak shmem_ushort_wait_until = pshmem_ushort_wait_until
#pragma weak shmem_uint_wait_until = pshmem_uint_wait_until
#pragma weak shmem_ulong_wait_until = pshmem_ulong_wait_until
#pragma weak shmem_ulonglong_wait_until = pshmem_ulonglong_wait_until
#pragma weak shmem_int32_wait_until = pshmem_int32_wait_until
#pragma weak shmem_int64_wait_until = pshmem_int64_wait_until
#pragma weak shmem_uint32_wait_until = pshmem_uint32_wait_until
#pragma weak shmem_uint64_wait_until = pshmem_uint64_wait_until
#pragma weak shmem_size_wait_until = pshmem_size_wait_until
#pragma weak shmem_ptrdiff_wait_until = pshmem_ptrdiff_wait_until

#pragma weak shmemx_int32_wait_until = pshmemx_int32_wait_until
#pragma weak shmemx_int64_wait_until = pshmemx_int64_wait_until

#pragma weak shmem_short_test = pshmem_short_test
#pragma weak shmem_int_test = pshmem_int_test
#pragma weak shmem_long_test = pshmem_long_test
#pragma weak shmem_longlong_test = pshmem_longlong_test
#pragma weak shmem_ushort_test = pshmem_ushort_test
#pragma weak shmem_uint_test = pshmem_uint_test
#pragma weak shmem_ulong_test = pshmem_ulong_test
#pragma weak shmem_ulonglong_test = pshmem_ulonglong_test
#pragma weak shmem_int32_test = pshmem_int32_test
#pragma weak shmem_int64_test = pshmem_int64_test
#pragma weak shmem_uint32_test = pshmem_uint32_test
#pragma weak shmem_uint64_test = pshmem_uint64_test
#pragma weak shmem_size_test = pshmem_size_test
#pragma weak shmem_ptrdiff_test = pshmem_ptrdiff_test
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_WAIT(, volatile long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT(_short, volatile short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT(_int, volatile int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT(_long, volatile long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT(_longlong, volatile long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT(_int32, int32_t, SHMEM_INT32_T, shmemx)
SHMEM_TYPE_WAIT(_int64, int64_t, SHMEM_INT64_T, shmemx)

#define SHMEM_TYPE_WAIT_UNTIL(type_name, type, code, prefix)    \
    void prefix##type_name##_wait_until(type *addr, int cmp, type value)   \
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

SHMEM_TYPE_WAIT_UNTIL(_short, volatile short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL(_int, volatile int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT_UNTIL(_long, volatile long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT_UNTIL(_longlong, volatile long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL(_ushort, volatile unsigned short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL(_uint, volatile unsigned int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT_UNTIL(_ulong, volatile unsigned long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT_UNTIL(_ulonglong, volatile unsigned long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL(_int32, volatile int32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL(_int64, volatile int64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL(_uint32, volatile uint32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL(_uint64, volatile uint64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL(_size, volatile size_t, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL(_ptrdiff, volatile ptrdiff_t, SHMEM_LLONG, shmem)

SHMEM_TYPE_WAIT_UNTIL(_int32, int32_t, SHMEM_INT32_T, shmemx)
SHMEM_TYPE_WAIT_UNTIL(_int64, int64_t, SHMEM_INT64_T, shmemx)

#define SHMEM_TYPE_TEST(type_name, type, code, prefix)              \
    int prefix##type_name##_test(type *addr, int cmp, type value)   \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
        int out_value;                                              \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(test(                                    \
            (void*)addr,                                            \
            cmp,                                                    \
            (void*)&value,                                          \
            code, &out_value));                                     \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return out_value;                                           \
    }

SHMEM_TYPE_TEST(_short, volatile short, SHMEM_SHORT, shmem)
SHMEM_TYPE_TEST(_int, volatile int, SHMEM_INT, shmem)
SHMEM_TYPE_TEST(_long, volatile long, SHMEM_LONG, shmem)
SHMEM_TYPE_TEST(_longlong, volatile long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_TEST(_ushort, volatile unsigned short, SHMEM_SHORT, shmem)
SHMEM_TYPE_TEST(_uint, volatile unsigned int, SHMEM_INT, shmem)
SHMEM_TYPE_TEST(_ulong, volatile unsigned long, SHMEM_LONG, shmem)
SHMEM_TYPE_TEST(_ulonglong, volatile unsigned long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_TEST(_int32, volatile int32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_TEST(_int64, volatile int64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_TEST(_uint32, volatile uint32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_TEST(_uint64, volatile uint64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_TEST(_size, volatile size_t, SHMEM_LLONG, shmem)
SHMEM_TYPE_TEST(_ptrdiff, volatile ptrdiff_t, SHMEM_LLONG, shmem)
