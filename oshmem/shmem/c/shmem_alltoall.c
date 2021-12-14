/*
 * Copyright (c) 2016-2018 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

#include "oshmem/mca/scoll/scoll.h"

#include "oshmem/proc/proc.h"

static void _shmem_alltoall(void *target,
                            const void *source,
                            ptrdiff_t dst, ptrdiff_t sst,
                            size_t nelems,
                            size_t element_size,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync);

#define SHMEM_TYPE_ALLTOALL(name, element_size)                      \
    void shmem##name(void *target,                                   \
                     const void *source,                             \
                     size_t nelems,                                  \
                     int PE_start,                                   \
                     int logPE_stride,                               \
                     int PE_size,                                    \
                     long *pSync)                                    \
{                                                                    \
    RUNTIME_CHECK_INIT();                                            \
    RUNTIME_CHECK_ADDR_SIZE(target, nelems);                         \
    RUNTIME_CHECK_ADDR_SIZE(source, nelems);                         \
                                                                     \
    _shmem_alltoall(target, source, 1, 1, nelems, element_size,      \
                       PE_start, logPE_stride, PE_size,              \
                       pSync);                                       \
}

#define SHMEM_TYPE_ALLTOALLS(name, element_size)                     \
    void shmem##name(void *target,                                   \
                     const void *source,                             \
                     ptrdiff_t dst, ptrdiff_t sst,                   \
                     size_t nelems,                                  \
                     int PE_start,                                   \
                     int logPE_stride,                               \
                     int PE_size,                                    \
                     long *pSync)                                    \
{                                                                    \
    RUNTIME_CHECK_INIT();                                            \
    RUNTIME_CHECK_ADDR_SIZE(target, nelems);                         \
    RUNTIME_CHECK_ADDR_SIZE(source, nelems);                         \
                                                                     \
    _shmem_alltoall(target, source, dst, sst, nelems, element_size,  \
                       PE_start, logPE_stride, PE_size,              \
                       pSync);                                       \
}

static void _shmem_alltoall(void *target,
                            const void *source,
                            ptrdiff_t dst, ptrdiff_t sst,
                            size_t nelems,
                            size_t element_size,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync)
{
    int rc;
    oshmem_group_t* group;

    /* Create group basing PE_start, logPE_stride and PE_size */
    group = oshmem_proc_group_create_nofail(PE_start, 1<<logPE_stride, PE_size);
    /* Call collective alltoall operation */
    rc = group->g_scoll.scoll_alltoall(group,
                                       target,
                                       source,
                                       dst,
                                       sst,
                                       nelems,
                                       element_size,
                                       pSync,
                                       SCOLL_DEFAULT_ALG);
    oshmem_proc_group_destroy(group);
    RUNTIME_CHECK_RC(rc);
}

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_alltoall32 = pshmem_alltoall32
#pragma weak shmem_alltoall64 = pshmem_alltoall64
#pragma weak shmem_alltoalls32 = pshmem_alltoalls32
#pragma weak shmem_alltoalls64 = pshmem_alltoalls64

/* Teams alltoall */
#pragma weak shmem_char_alltoall         		= pshmem_char_alltoall
#pragma weak shmem_short_alltoall         		= pshmem_short_alltoall
#pragma weak shmem_int_alltoall         	    	= pshmem_int_alltoall
#pragma weak shmem_long_alltoall         		= pshmem_long_alltoall
#pragma weak shmem_float_alltoall         		= pshmem_float_alltoall
#pragma weak shmem_double_alltoall         		= pshmem_double_alltoall
#pragma weak shmem_longlong_alltoall         		= pshmem_longlong_alltoall
#pragma weak shmem_schar_alltoall         		= pshmem_schar_alltoall
#pragma weak shmem_uchar_alltoall         		= pshmem_uchar_alltoall
#pragma weak shmem_ushort_alltoall         		= pshmem_ushort_alltoall
#pragma weak shmem_uint_alltoall         		= pshmem_uint_alltoall
#pragma weak shmem_ulong_alltoall         		= pshmem_ulong_alltoall
#pragma weak shmem_ulonglong_alltoall      		= pshmem_ulonglong_alltoall
#pragma weak shmem_longdouble_alltoall       		= pshmem_longdouble_alltoall
#pragma weak shmem_int8_alltoall         		= pshmem_int8_alltoall
#pragma weak shmem_int16_alltoall         		= pshmem_int16_alltoall
#pragma weak shmem_int32_alltoall         		= pshmem_int32_alltoall
#pragma weak shmem_int64_alltoall         		= pshmem_int64_alltoall
#pragma weak shmem_uint8_alltoall         		= pshmem_uint8_alltoall
#pragma weak shmem_uint16_alltoall         		= pshmem_uint16_alltoall
#pragma weak shmem_uint32_alltoall         		= pshmem_uint32_alltoall
#pragma weak shmem_uint64_alltoall         		= pshmem_uint64_alltoall
#pragma weak shmem_size_alltoall         		= pshmem_size_alltoall
#pragma weak shmem_ptrdiff_alltoall         		= pshmem_ptrdiff_alltoall

#pragma weak shmem_alltoallmem               		= pshmem_alltoallmem

/* Teams alltoalls */
#pragma weak shmem_char_alltoalls         		= pshmem_char_alltoalls
#pragma weak shmem_short_alltoalls         		= pshmem_short_alltoalls
#pragma weak shmem_int_alltoalls       	    	= pshmem_int_alltoalls
#pragma weak shmem_long_alltoalls         		= pshmem_long_alltoalls
#pragma weak shmem_float_alltoalls         		= pshmem_float_alltoalls
#pragma weak shmem_double_alltoalls         		= pshmem_double_alltoalls
#pragma weak shmem_longlong_alltoalls        		= pshmem_longlong_alltoalls
#pragma weak shmem_schar_alltoalls         		= pshmem_schar_alltoalls
#pragma weak shmem_uchar_alltoalls         		= pshmem_uchar_alltoalls
#pragma weak shmem_ushort_alltoalls         		= pshmem_ushort_alltoalls
#pragma weak shmem_uint_alltoalls         		= pshmem_uint_alltoalls
#pragma weak shmem_ulong_alltoalls         		= pshmem_ulong_alltoalls
#pragma weak shmem_ulonglong_alltoalls      		= pshmem_ulonglong_alltoalls
#pragma weak shmem_longdouble_alltoalls      		= pshmem_longdouble_alltoalls
#pragma weak shmem_int8_alltoalls         		= pshmem_int8_alltoalls
#pragma weak shmem_int16_alltoalls         		= pshmem_int16_alltoalls
#pragma weak shmem_int32_alltoalls         		= pshmem_int32_alltoalls
#pragma weak shmem_int64_alltoalls         		= pshmem_int64_alltoalls
#pragma weak shmem_uint8_alltoalls         		= pshmem_uint8_alltoalls
#pragma weak shmem_uint16_alltoalls         		= pshmem_uint16_alltoalls
#pragma weak shmem_uint32_alltoalls         		= pshmem_uint32_alltoalls
#pragma weak shmem_uint64_alltoalls         		= pshmem_uint64_alltoalls
#pragma weak shmem_size_alltoalls         		= pshmem_size_alltoalls
#pragma weak shmem_ptrdiff_alltoalls        		= pshmem_ptrdiff_alltoalls

#pragma weak shmem_alltoallsmem              		= pshmem_alltoallsmem

#include "oshmem/shmem/c/profile-defines.h"
#endif

SHMEM_TYPE_ALLTOALL(_alltoall32, sizeof(uint32_t))
SHMEM_TYPE_ALLTOALL(_alltoall64, sizeof(uint64_t))
SHMEM_TYPE_ALLTOALLS(_alltoalls32, sizeof(uint32_t))
SHMEM_TYPE_ALLTOALLS(_alltoalls64, sizeof(uint64_t))



#define SHMEM_TYPE_TEAM_ALLTOALL(type_name, type, code, postfix)    \
    int  shmem##type_name##postfix(shmem_team_t team, type *dest, const type *source, size_t nelems)   \
    {                                                               \
        int rc = 0;                                                 \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(team_alltoall(                           \
            team, (void*)dest, (void*)source, nelems, code));       \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return rc;                                                  \
    }


SHMEM_TYPE_TEAM_ALLTOALL(_char, char, SHMEM_CHAR, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_short, short, SHMEM_SHORT, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_int, int, SHMEM_INT, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_long, long, SHMEM_LONG, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_float, float, SHMEM_FLOAT, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_double, double, SHMEM_DOUBLE, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_longlong, long long, SHMEM_LLONG, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_schar, signed char, SHMEM_SCHAR, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_uchar, unsigned char, SHMEM_UCHAR, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_ushort, unsigned short, SHMEM_USHORT, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_uint, unsigned int, SHMEM_UINT, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_ulong, unsigned long, SHMEM_ULONG, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_ulonglong, unsigned long long, SHMEM_ULLONG, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_longdouble, long double, SHMEM_LDOUBLE, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_int8, int8_t, SHMEM_INT8_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_int16, int16_t, SHMEM_INT16_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_int32, int32_t, SHMEM_INT32_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_int64, int64_t, SHMEM_INT64_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_uint8, uint8_t, SHMEM_UINT8_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_uint16, uint16_t, SHMEM_UINT16_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_uint32, uint32_t, SHMEM_UINT32_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_uint64, uint64_t, SHMEM_UINT64_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_size, size_t, SHMEM_SIZE_T, _alltoall)
SHMEM_TYPE_TEAM_ALLTOALL(_ptrdiff, ptrdiff_t, SHMEM_PTRDIFF_T, _alltoall)

SHMEM_TYPE_TEAM_ALLTOALL(, void, SHMEM_BYTE, _alltoallmem)


#define SHMEM_TYPE_TEAM_ALLTOALLS(type_name, type, code, postfix)   \
    int  shmem##type_name##postfix(shmem_team_t team, type *dest, const type *source, ptrdiff_t dst, ptrdiff_t sst,  size_t nelems)   \
    {                                                               \
        int rc = 0;                                                 \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(team_alltoalls(                          \
            team, (void*)dest, (void*)source,                       \
                    dst, sst, nelems, code));                       \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return rc;                                                  \
    }


SHMEM_TYPE_TEAM_ALLTOALLS(_char, char, SHMEM_CHAR, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_short, short, SHMEM_SHORT, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_int, int, SHMEM_INT, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_long, long, SHMEM_LONG, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_float, float, SHMEM_FLOAT, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_double, double, SHMEM_DOUBLE, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_longlong, long long, SHMEM_LLONG, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_schar, signed char, SHMEM_SCHAR, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_uchar, unsigned char, SHMEM_UCHAR, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_ushort, unsigned short, SHMEM_USHORT, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_uint, unsigned int, SHMEM_UINT, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_ulong, unsigned long, SHMEM_ULONG, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_ulonglong, unsigned long long, SHMEM_ULLONG, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_longdouble, long double, SHMEM_LDOUBLE, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_int8, int8_t, SHMEM_INT8_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_int16, int16_t, SHMEM_INT16_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_int32, int32_t, SHMEM_INT32_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_int64, int64_t, SHMEM_INT64_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_uint8, uint8_t, SHMEM_UINT8_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_uint16, uint16_t, SHMEM_UINT16_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_uint32, uint32_t, SHMEM_UINT32_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_uint64, uint64_t, SHMEM_UINT64_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_size, size_t, SHMEM_SIZE_T, _alltoalls)
SHMEM_TYPE_TEAM_ALLTOALLS(_ptrdiff, ptrdiff_t, SHMEM_PTRDIFF_T, _alltoalls)

SHMEM_TYPE_TEAM_ALLTOALLS(, void, SHMEM_BYTE, _alltoallsmem)
