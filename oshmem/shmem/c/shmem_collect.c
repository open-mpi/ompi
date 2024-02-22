/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
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
#include "oshmem/mca/spml/spml.h"

#include "oshmem/proc/proc.h"

static void _shmem_collect(void *target,
                            const void *source,
                            size_t nbytes,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync,
                            bool nlong_type);

#define SHMEM_TYPE_COLLECT(name, element_size, nelems_type)     \
    void shmem##name( void *target,                             \
                      const void *source,                       \
                      size_t nelems,                            \
                      int PE_start,                             \
                      int logPE_stride,                         \
                      int PE_size,                              \
                      long *pSync)                              \
{                                                               \
    RUNTIME_CHECK_INIT();                                       \
    RUNTIME_CHECK_ADDR_SIZE(target, nelems);                    \
    RUNTIME_CHECK_ADDR_SIZE(source, nelems);                    \
                                                                \
    _shmem_collect( target, source, nelems * element_size,      \
                     PE_start, logPE_stride, PE_size,           \
                     pSync,                                     \
                     nelems_type);                              \
}

static void _shmem_collect(void *target,
                            const void *source,
                            size_t nbytes,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync,
                            bool array_type)
{
    int rc;
    oshmem_group_t *group;

    /* Create group basing PE_start, logPE_stride and PE_size */
    group = oshmem_proc_group_create_nofail(PE_start, 1<<logPE_stride, PE_size);
    /* Call collective broadcast operation */
    rc = group->g_scoll.scoll_collect(group,
                                      target,
                                      source,
                                      nbytes,
                                      pSync,
                                      array_type,
                                      SCOLL_DEFAULT_ALG);
    oshmem_proc_group_destroy(group);
    RUNTIME_CHECK_RC(rc);
}

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_collect32 = pshmem_collect32
#pragma weak shmem_collect64 = pshmem_collect64
#pragma weak shmem_fcollect32 = pshmem_fcollect32
#pragma weak shmem_fcollect64 = pshmem_fcollect64

/* Teams collect */
#pragma weak shmem_char_collect          		= pshmem_char_collect
#pragma weak shmem_short_collect         		= pshmem_short_collect
#pragma weak shmem_int_collect         	    	        = pshmem_int_collect
#pragma weak shmem_long_collect          		= pshmem_long_collect
#pragma weak shmem_float_collect         		= pshmem_float_collect
#pragma weak shmem_double_collect         		= pshmem_double_collect
#pragma weak shmem_longlong_collect          		= pshmem_longlong_collect
#pragma weak shmem_schar_collect         		= pshmem_schar_collect
#pragma weak shmem_uchar_collect         		= pshmem_uchar_collect
#pragma weak shmem_ushort_collect         		= pshmem_ushort_collect
#pragma weak shmem_uint_collect          		= pshmem_uint_collect
#pragma weak shmem_ulong_collect         		= pshmem_ulong_collect
#pragma weak shmem_ulonglong_collect      		= pshmem_ulonglong_collect
#pragma weak shmem_longdouble_collect        		= pshmem_longdouble_collect
#pragma weak shmem_int8_collect         	    	= pshmem_int8_collect
#pragma weak shmem_int16_collect         		= pshmem_int16_collect
#pragma weak shmem_int32_collect         		= pshmem_int32_collect
#pragma weak shmem_int64_collect         		= pshmem_int64_collect
#pragma weak shmem_uint8_collect         		= pshmem_uint8_collect
#pragma weak shmem_uint16_collect         		= pshmem_uint16_collect
#pragma weak shmem_uint32_collect         		= pshmem_uint32_collect
#pragma weak shmem_uint64_collect         		= pshmem_uint64_collect
#pragma weak shmem_size_collect            		= pshmem_size_collect
#pragma weak shmem_ptrdiff_collect         		= pshmem_ptrdiff_collect

#pragma weak shmem_collectmem                		= pshmem_collectmem


/* Teams fcollect */
#pragma weak shmem_char_fcollect         		= pshmem_char_fcollect
#pragma weak shmem_short_fcollect         		= pshmem_short_fcollect
#pragma weak shmem_int_fcollect         	    	= pshmem_int_fcollect
#pragma weak shmem_long_fcollect         		= pshmem_long_fcollect
#pragma weak shmem_float_fcollect         		= pshmem_float_fcollect
#pragma weak shmem_double_fcollect         		= pshmem_double_fcollect
#pragma weak shmem_longlong_fcollect         	        = pshmem_longlong_fcollect
#pragma weak shmem_schar_fcollect         		= pshmem_schar_fcollect
#pragma weak shmem_uchar_fcollect         		= pshmem_uchar_fcollect
#pragma weak shmem_ushort_fcollect         		= pshmem_ushort_fcollect
#pragma weak shmem_uint_fcollect         		= pshmem_uint_fcollect
#pragma weak shmem_ulong_fcollect         		= pshmem_ulong_fcollect
#pragma weak shmem_ulonglong_fcollect      		= pshmem_ulonglong_fcollect
#pragma weak shmem_longdouble_fcollect       		= pshmem_longdouble_fcollect
#pragma weak shmem_int8_fcollect         		= pshmem_int8_fcollect
#pragma weak shmem_int16_fcollect         		= pshmem_int16_fcollect
#pragma weak shmem_int32_fcollect         		= pshmem_int32_fcollect
#pragma weak shmem_int64_fcollect         		= pshmem_int64_fcollect
#pragma weak shmem_uint8_fcollect         		= pshmem_uint8_fcollect
#pragma weak shmem_uint16_fcollect         		= pshmem_uint16_fcollect
#pragma weak shmem_uint32_fcollect         		= pshmem_uint32_fcollect
#pragma weak shmem_uint64_fcollect         		= pshmem_uint64_fcollect
#pragma weak shmem_size_fcollect         		= pshmem_size_fcollect
#pragma weak shmem_ptrdiff_fcollect         	        = pshmem_ptrdiff_fcollect

#pragma weak shmem_fcollectmem               		= pshmem_fcollectmem



#include "oshmem/shmem/c/profile-defines.h"
#endif

SHMEM_TYPE_COLLECT(_collect32, sizeof(uint32_t), false)
SHMEM_TYPE_COLLECT(_collect64, sizeof(uint64_t), false)
SHMEM_TYPE_COLLECT(_fcollect32, sizeof(uint32_t), true)
SHMEM_TYPE_COLLECT(_fcollect64, sizeof(uint64_t), true)


#define SHMEM_TYPE_TEAM_COLLECT(type_name, type, code, postfix)     \
    int  shmem##type_name##postfix(shmem_team_t team, type *dest, const type *source, size_t nelems)   \
    {                                                               \
        int rc = 0;                                                 \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(team_collect(                            \
            team, (void*)dest, (void*)source,                       \
                    nelems, code));                                 \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return rc;                                                  \
    }


SHMEM_TYPE_TEAM_COLLECT(_char, char, SHMEM_CHAR, _collect)
SHMEM_TYPE_TEAM_COLLECT(_short, short, SHMEM_SHORT, _collect)
SHMEM_TYPE_TEAM_COLLECT(_int, int, SHMEM_INT, _collect)
SHMEM_TYPE_TEAM_COLLECT(_long, long, SHMEM_LONG, _collect)
SHMEM_TYPE_TEAM_COLLECT(_float, float, SHMEM_FLOAT, _collect)
SHMEM_TYPE_TEAM_COLLECT(_double, double, SHMEM_DOUBLE, _collect)
SHMEM_TYPE_TEAM_COLLECT(_longlong, long long, SHMEM_LLONG, _collect)
SHMEM_TYPE_TEAM_COLLECT(_schar, signed char, SHMEM_SCHAR, _collect)
SHMEM_TYPE_TEAM_COLLECT(_uchar, unsigned char, SHMEM_UCHAR, _collect)
SHMEM_TYPE_TEAM_COLLECT(_ushort, unsigned short, SHMEM_USHORT, _collect)
SHMEM_TYPE_TEAM_COLLECT(_uint, unsigned int, SHMEM_UINT, _collect)
SHMEM_TYPE_TEAM_COLLECT(_ulong, unsigned long, SHMEM_ULONG, _collect)
SHMEM_TYPE_TEAM_COLLECT(_ulonglong, unsigned long long, SHMEM_ULLONG, _collect)
SHMEM_TYPE_TEAM_COLLECT(_longdouble, long double, SHMEM_LDOUBLE, _collect)
SHMEM_TYPE_TEAM_COLLECT(_int8, int8_t, SHMEM_INT8_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_int16, int16_t, SHMEM_INT16_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_int32, int32_t, SHMEM_INT32_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_int64, int64_t, SHMEM_INT64_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_uint8, uint8_t, SHMEM_UINT8_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_uint16, uint16_t, SHMEM_UINT16_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_uint32, uint32_t, SHMEM_UINT32_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_uint64, uint64_t, SHMEM_UINT64_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_size, size_t, SHMEM_SIZE_T, _collect)
SHMEM_TYPE_TEAM_COLLECT(_ptrdiff, ptrdiff_t, SHMEM_PTRDIFF_T, _collect)

SHMEM_TYPE_TEAM_COLLECT(, void, SHMEM_BYTE, _collectmem)




#define SHMEM_TYPE_TEAM_FCOLLECT(type_name, type, code, postfix)    \
    int  shmem##type_name##postfix(shmem_team_t team, type *dest, const type *source,  size_t nelems)   \
    {                                                               \
        int rc = 0;                                                 \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(team_fcollect(                           \
            team, (void*)dest, (void*)source,                       \
                    nelems, code));                                 \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return rc;                                                  \
    }



SHMEM_TYPE_TEAM_FCOLLECT(_char, char, SHMEM_CHAR, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_short, short, SHMEM_SHORT, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_int, int, SHMEM_INT, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_long, long, SHMEM_LONG, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_float, float, SHMEM_FLOAT, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_double, double, SHMEM_DOUBLE, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_longlong, long long, SHMEM_LLONG, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_schar, signed char, SHMEM_SCHAR, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_uchar, unsigned char, SHMEM_UCHAR, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_ushort, unsigned short, SHMEM_USHORT, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_uint, unsigned int, SHMEM_UINT, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_ulong, unsigned long, SHMEM_ULONG, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_ulonglong, unsigned long long, SHMEM_ULLONG, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_longdouble, long double, SHMEM_LDOUBLE, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_int8, int8_t, SHMEM_INT8_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_int16, int16_t, SHMEM_INT16_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_int32, int32_t, SHMEM_INT32_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_int64, int64_t, SHMEM_INT64_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_uint8, uint8_t, SHMEM_UINT8_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_uint16, uint16_t, SHMEM_UINT16_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_uint32, uint32_t, SHMEM_UINT32_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_uint64, uint64_t, SHMEM_UINT64_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_size, size_t, SHMEM_SIZE_T, _fcollect)
SHMEM_TYPE_TEAM_FCOLLECT(_ptrdiff, ptrdiff_t, SHMEM_PTRDIFF_T, _fcollect)

SHMEM_TYPE_TEAM_FCOLLECT(, void, SHMEM_BYTE, _fcollectmem)

