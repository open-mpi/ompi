/*
 * Copyright (c) 2021      NVIDIA Corporation.
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


#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"

#pragma weak shmem_short_wait_until_all       = pshmem_short_wait_until_all
#pragma weak shmem_ushort_wait_until_all      = pshmem_ushort_wait_until_all
#pragma weak shmem_int_wait_until_all         = pshmem_int_wait_until_all
#pragma weak shmem_long_wait_until_all        = pshmem_long_wait_until_all
#pragma weak shmem_longlong_wait_until_all    = pshmem_longlong_wait_until_all
#pragma weak shmem_uint_wait_until_all        = pshmem_uint_wait_until_all
#pragma weak shmem_ulong_wait_until_all       = pshmem_ulong_wait_until_all
#pragma weak shmem_ulonglong_wait_until_all   = pshmem_ulonglong_wait_until_all
#pragma weak shmem_int32_wait_until_all       = pshmem_int32_wait_until_all
#pragma weak shmem_int64_wait_until_all       = pshmem_int64_wait_until_all
#pragma weak shmem_uint32_wait_until_all      = pshmem_uint32_wait_until_all
#pragma weak shmem_uint64_wait_until_all      = pshmem_uint64_wait_until_all
#pragma weak shmem_size_wait_until_all        = pshmem_size_wait_until_all
#pragma weak shmem_ptrdiff_wait_until_all     = pshmem_ptrdiff_wait_until_all


#pragma weak shmem_short_wait_until_any       = pshmem_short_wait_until_any
#pragma weak shmem_ushort_wait_until_any      = pshmem_ushort_wait_until_any
#pragma weak shmem_int_wait_until_any         = pshmem_int_wait_until_any
#pragma weak shmem_long_wait_until_any        = pshmem_long_wait_until_any
#pragma weak shmem_longlong_wait_until_any    = pshmem_longlong_wait_until_any
#pragma weak shmem_uint_wait_until_any        = pshmem_uint_wait_until_any
#pragma weak shmem_ulong_wait_until_any       = pshmem_ulong_wait_until_any
#pragma weak shmem_ulonglong_wait_until_any   = pshmem_ulonglong_wait_until_any
#pragma weak shmem_int32_wait_until_any       = pshmem_int32_wait_until_any
#pragma weak shmem_int64_wait_until_any       = pshmem_int64_wait_until_any
#pragma weak shmem_uint32_wait_until_any      = pshmem_uint32_wait_until_any
#pragma weak shmem_uint64_wait_until_any      = pshmem_uint64_wait_until_any
#pragma weak shmem_size_wait_until_any        = pshmem_size_wait_until_any
#pragma weak shmem_ptrdiff_wait_until_any     = pshmem_ptrdiff_wait_until_any


#pragma weak shmem_short_wait_until_some       = pshmem_short_wait_until_some
#pragma weak shmem_ushort_wait_until_some      = pshmem_ushort_wait_until_some
#pragma weak shmem_int_wait_until_some         = pshmem_int_wait_until_some
#pragma weak shmem_long_wait_until_some        = pshmem_long_wait_until_some
#pragma weak shmem_longlong_wait_until_some    = pshmem_longlong_wait_until_some
#pragma weak shmem_uint_wait_until_some        = pshmem_uint_wait_until_some
#pragma weak shmem_ulong_wait_until_some       = pshmem_ulong_wait_until_some
#pragma weak shmem_ulonglong_wait_until_some   = pshmem_ulonglong_wait_until_some
#pragma weak shmem_int32_wait_until_some       = pshmem_int32_wait_until_some
#pragma weak shmem_int64_wait_until_some       = pshmem_int64_wait_until_some
#pragma weak shmem_uint32_wait_until_some      = pshmem_uint32_wait_until_some
#pragma weak shmem_uint64_wait_until_some      = pshmem_uint64_wait_until_some
#pragma weak shmem_size_wait_until_some        = pshmem_size_wait_until_some
#pragma weak shmem_ptrdiff_wait_until_some     = pshmem_ptrdiff_wait_until_some


#pragma weak shmem_short_wait_until_all_vector       = pshmem_short_wait_until_all_vector
#pragma weak shmem_ushort_wait_until_all_vector      = pshmem_ushort_wait_until_all_vector
#pragma weak shmem_int_wait_until_all_vector         = pshmem_int_wait_until_all_vector
#pragma weak shmem_long_wait_until_all_vector        = pshmem_long_wait_until_all_vector
#pragma weak shmem_longlong_wait_until_all_vector    = pshmem_longlong_wait_until_all_vector
#pragma weak shmem_uint_wait_until_all_vector        = pshmem_uint_wait_until_all_vector
#pragma weak shmem_ulong_wait_until_all_vector       = pshmem_ulong_wait_until_all_vector
#pragma weak shmem_ulonglong_wait_until_all_vector   = pshmem_ulonglong_wait_until_all_vector
#pragma weak shmem_int32_wait_until_all_vector       = pshmem_int32_wait_until_all_vector
#pragma weak shmem_int64_wait_until_all_vector       = pshmem_int64_wait_until_all_vector
#pragma weak shmem_uint32_wait_until_all_vector      = pshmem_uint32_wait_until_all_vector
#pragma weak shmem_uint64_wait_until_all_vector      = pshmem_uint64_wait_until_all_vector
#pragma weak shmem_size_wait_until_all_vector        = pshmem_size_wait_until_all_vector
#pragma weak shmem_ptrdiff_wait_until_all_vector     = pshmem_ptrdiff_wait_until_all_vector


#pragma weak shmem_short_wait_until_any_vector       = pshmem_short_wait_until_any_vector
#pragma weak shmem_ushort_wait_until_any_vector      = pshmem_ushort_wait_until_any_vector
#pragma weak shmem_int_wait_until_any_vector         = pshmem_int_wait_until_any_vector
#pragma weak shmem_long_wait_until_any_vector        = pshmem_long_wait_until_any_vector
#pragma weak shmem_longlong_wait_until_any_vector    = pshmem_longlong_wait_until_any_vector
#pragma weak shmem_uint_wait_until_any_vector        = pshmem_uint_wait_until_any_vector
#pragma weak shmem_ulong_wait_until_any_vector       = pshmem_ulong_wait_until_any_vector
#pragma weak shmem_ulonglong_wait_until_any_vector   = pshmem_ulonglong_wait_until_any_vector
#pragma weak shmem_int32_wait_until_any_vector       = pshmem_int32_wait_until_any_vector
#pragma weak shmem_int64_wait_until_any_vector       = pshmem_int64_wait_until_any_vector
#pragma weak shmem_uint32_wait_until_any_vector      = pshmem_uint32_wait_until_any_vector
#pragma weak shmem_uint64_wait_until_any_vector      = pshmem_uint64_wait_until_any_vector
#pragma weak shmem_size_wait_until_any_vector        = pshmem_size_wait_until_any_vector
#pragma weak shmem_ptrdiff_wait_until_any_vector     = pshmem_ptrdiff_wait_until_any_vector


#pragma weak shmem_short_wait_until_some_vector       = pshmem_short_wait_until_some_vector
#pragma weak shmem_ushort_wait_until_some_vector      = pshmem_ushort_wait_until_some_vector
#pragma weak shmem_int_wait_until_some_vector         = pshmem_int_wait_until_some_vector
#pragma weak shmem_long_wait_until_some_vector        = pshmem_long_wait_until_some_vector
#pragma weak shmem_longlong_wait_until_some_vector    = pshmem_longlong_wait_until_some_vector
#pragma weak shmem_uint_wait_until_some_vector        = pshmem_uint_wait_until_some_vector
#pragma weak shmem_ulong_wait_until_some_vector       = pshmem_ulong_wait_until_some_vector
#pragma weak shmem_ulonglong_wait_until_some_vector   = pshmem_ulonglong_wait_until_some_vector
#pragma weak shmem_int32_wait_until_some_vector       = pshmem_int32_wait_until_some_vector
#pragma weak shmem_int64_wait_until_some_vector       = pshmem_int64_wait_until_some_vector
#pragma weak shmem_uint32_wait_until_some_vector      = pshmem_uint32_wait_until_some_vector
#pragma weak shmem_uint64_wait_until_some_vector      = pshmem_uint64_wait_until_some_vector
#pragma weak shmem_size_wait_until_some_vector        = pshmem_size_wait_until_some_vector
#pragma weak shmem_ptrdiff_wait_until_some_vector     = pshmem_ptrdiff_wait_until_some_vector


#include "oshmem/shmem/c/profile-defines.h"
#endif

#define SHMEM_TYPE_WAIT_UNTIL_ALL(type_name, type, code, prefix)    \
    void prefix##type_name##_wait_until_all(volatile type *ivars, size_t nelems, const int *status, int cmp, type value)   \
    {                                                               \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        MCA_SPML_CALL(wait_until_all(                               \
            (void*)ivars,                                           \
            cmp,                                                    \
            (void*)&value,                                          \
            nelems, status, code));                                 \
                                                                    \
        return ;                                                    \
    }


#define SHMEM_TYPE_WAIT_UNTIL_ANY(type_name, type, code, prefix)    \
    size_t prefix##type_name##_wait_until_any(volatile type *ivars, size_t nelems, const int *status, int cmp, type value)   \
    {                                                               \
        size_t rc = 0;                                              \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(wait_until_any(                          \
            (void*)ivars,                                           \
            cmp,                                                    \
            (void*)&value,                                          \
            nelems, status, code));                                 \
        RUNTIME_CHECK_IMPL_RC(rc);                                  \
                                                                    \
        return rc;                                                  \
    }


#define SHMEM_TYPE_WAIT_UNTIL_SOME(type_name, type, code, prefix)   \
    size_t  prefix##type_name##_wait_until_some(volatile type *ivars, size_t nelems, size_t *indices, const int *status, int cmp, type value)   \
    {                                                               \
        size_t rc = 0;                                              \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(wait_until_some(                         \
            (void*)ivars,                                           \
            cmp,                                                    \
            (void*)&value,                                          \
            nelems, indices, status, code));                        \
        RUNTIME_CHECK_IMPL_RC(rc);                                  \
                                                                    \
        return rc;                                                  \
    }

#define SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(type_name, type, code, prefix)    \
    size_t prefix##type_name##_wait_until_any_vector(volatile type *ivars, size_t nelems, const int *status, int cmp, type *values)   \
    {                                                               \
        size_t rc = 0;                                              \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(wait_until_any_vector(                   \
            (void*)ivars,                                           \
            cmp,                                                    \
            (void*)values,                                          \
            nelems, status, code));                                 \
        RUNTIME_CHECK_IMPL_RC(rc);                                  \
                                                                    \
        return rc;                                                  \
    }

#define SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(type_name, type, code, prefix)    \
    size_t prefix##type_name##_wait_until_some_vector(volatile type *ivars, size_t nelems, size_t *indices, const int *status, int cmp, type *values)   \
    {                                                               \
        size_t rc = 0;                                              \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(wait_until_some_vector(                  \
            (void*)ivars,                                           \
            cmp,                                                    \
            (void*)values,                                          \
            nelems, indices, status, code));                        \
        RUNTIME_CHECK_IMPL_RC(rc);                                  \
                                                                    \
        return rc;                                                  \
    }


#define SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(type_name, type, code, prefix)    \
    void prefix##type_name##_wait_until_all_vector(volatile type *ivars, size_t nelems, const int *status, int cmp, type *values)   \
    {                                                               \
        int rc = OSHMEM_SUCCESS;                                    \
                                                                    \
                                                                    \
        MCA_SPML_CALL(wait_until_all_vector(                        \
            (void*)ivars,                                           \
            cmp,                                                    \
            (void*)values,                                          \
            nelems, status, code));                                 \
                                                                    \
        return ;                                                    \
    }



SHMEM_TYPE_WAIT_UNTIL_ALL(_short,  short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_ushort,  unsigned short, SHMEM_USHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_int,  int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_long,  long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_longlong,  long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_uint,  unsigned int, SHMEM_UINT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_ulong,  unsigned long, SHMEM_ULONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_ulonglong,  unsigned long long, SHMEM_ULLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_int32,  int32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_int64,  int64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_uint32,  uint32_t, SHMEM_UINT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_uint64,  uint64_t, SHMEM_UINT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_size,  size_t, SHMEM_SIZE_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL(_ptrdiff,  ptrdiff_t, SHMEM_PTRDIFF_T, shmem)

SHMEM_TYPE_WAIT_UNTIL_ANY(_short,  short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_ushort,  unsigned short, SHMEM_USHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_int,  int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_long,  long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_longlong,  long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_uint,  unsigned int, SHMEM_UINT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_ulong,  unsigned long, SHMEM_ULONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_ulonglong,  unsigned long long, SHMEM_ULLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_int32,  int32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_int64,  int64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_uint32,  uint32_t, SHMEM_UINT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_uint64,  uint64_t, SHMEM_UINT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_size,  size_t, SHMEM_SIZE_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY(_ptrdiff,  ptrdiff_t, SHMEM_PTRDIFF_T, shmem)


SHMEM_TYPE_WAIT_UNTIL_SOME(_short,  short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_ushort,  unsigned short, SHMEM_USHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_int,  int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_long,  long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_longlong,  long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_uint,  unsigned int, SHMEM_UINT, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_ulong,  unsigned long, SHMEM_ULONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_ulonglong,  unsigned long long, SHMEM_ULLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_int32,  int32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_int64,  int64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_uint32,  uint32_t, SHMEM_UINT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_uint64,  uint64_t, SHMEM_UINT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_size,  size_t, SHMEM_SIZE_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME(_ptrdiff,  ptrdiff_t, SHMEM_PTRDIFF_T, shmem)


SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_short,  short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_ushort,  unsigned short, SHMEM_USHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_int,  int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_long,  long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_longlong,  long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_uint,  unsigned int, SHMEM_UINT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_ulong,  unsigned long, SHMEM_ULONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_ulonglong,  unsigned long long, SHMEM_ULLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_int32,  int32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_int64,  int64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_uint32,  uint32_t, SHMEM_UINT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_uint64,  uint64_t, SHMEM_UINT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_size,  size_t, SHMEM_SIZE_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ALL_VECTOR(_ptrdiff,  ptrdiff_t, SHMEM_PTRDIFF_T, shmem)



SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_short,  short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_ushort,  unsigned short, SHMEM_USHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_int,  int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_long,  long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_longlong,  long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_uint,  unsigned int, SHMEM_UINT, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_ulong,  unsigned long, SHMEM_ULONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_ulonglong,  unsigned long long, SHMEM_ULLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_int32,  int32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_int64,  int64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_uint32,  uint32_t, SHMEM_UINT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_uint64,  uint64_t, SHMEM_UINT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_size,  size_t, SHMEM_SIZE_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_ANY_VECTOR(_ptrdiff,  ptrdiff_t, SHMEM_PTRDIFF_T, shmem)



SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_short,  short, SHMEM_SHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_ushort,  unsigned short, SHMEM_USHORT, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_int,  int, SHMEM_INT, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_long,  long, SHMEM_LONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_longlong,  long long, SHMEM_LLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_uint,  unsigned int, SHMEM_UINT, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_ulong,  unsigned long, SHMEM_ULONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_ulonglong,  unsigned long long, SHMEM_ULLONG, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_int32,  int32_t, SHMEM_INT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_int64,  int64_t, SHMEM_INT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_uint32,  uint32_t, SHMEM_UINT32_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_uint64,  uint64_t, SHMEM_UINT64_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_size,  size_t, SHMEM_SIZE_T, shmem)
SHMEM_TYPE_WAIT_UNTIL_SOME_VECTOR(_ptrdiff,  ptrdiff_t, SHMEM_PTRDIFF_T, shmem)


