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

#include "oshmem/proc/proc.h"

static void _shmem_broadcast(void *target,
                              const void *source,
                              size_t nbytes,
                              int PE_root,
                              int PE_start,
                              int logPE_stride,
                              int PE_size,
                              long *pSync);

#define SHMEM_TYPE_BROADCAST(name, element_size)                    \
    void shmem##name( void *target,                                 \
                      const void *source,                           \
                      size_t nelems,                                \
                      int PE_root,                                  \
                      int PE_start,                                 \
                      int logPE_stride,                             \
                      int PE_size,                                  \
                      long *pSync)                                  \
{                                                                   \
    RUNTIME_CHECK_INIT();                                           \
    RUNTIME_CHECK_ADDR_SIZE(target, nelems);                        \
    RUNTIME_CHECK_ADDR_SIZE(source, nelems);                        \
                                                                    \
    _shmem_broadcast( target, source, nelems * element_size,        \
                       PE_root, PE_start, logPE_stride, PE_size,    \
                       pSync);                                      \
}

static void _shmem_broadcast(void *target,
                              const void *source,
                              size_t nbytes,
                              int PE_root,
                              int PE_start,
                              int logPE_stride,
                              int PE_size,
                              long *pSync)
{
    int rc;
    oshmem_group_t *group;

    if ((0 <= PE_root) && (PE_root < PE_size)) {
        /* Create group basing PE_start, logPE_stride and PE_size */
        group = oshmem_proc_group_create_nofail(PE_start, 1 << logPE_stride, PE_size);
        if (PE_root >= group->proc_count) {
            rc = OSHMEM_ERROR;
            goto out;
        }

        /* Define actual PE using relative in active set */
        PE_root = oshmem_proc_pe_vpid(group, PE_root);

        /* Call collective broadcast operation */
        rc = group->g_scoll.scoll_broadcast(group,
                                            PE_root,
                                            target,
                                            source,
                                            nbytes,
                                            pSync,
                                            true,
                                            SCOLL_DEFAULT_ALG);
out:
        oshmem_proc_group_destroy(group);
        RUNTIME_CHECK_RC(rc);
    }
}

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_broadcast32 = pshmem_broadcast32
#pragma weak shmem_broadcast64 = pshmem_broadcast64

/* Teams broadcast */
#pragma weak shmem_char_broadcast         		= pshmem_char_broadcast
#pragma weak shmem_short_broadcast         		= pshmem_short_broadcast
#pragma weak shmem_int_broadcast        	    	= pshmem_int_broadcast
#pragma weak shmem_long_broadcast         		= pshmem_long_broadcast
#pragma weak shmem_float_broadcast         		= pshmem_float_broadcast
#pragma weak shmem_double_broadcast         		= pshmem_double_broadcast
#pragma weak shmem_longlong_broadcast        		= pshmem_longlong_broadcast
#pragma weak shmem_schar_broadcast         		= pshmem_schar_broadcast
#pragma weak shmem_uchar_broadcast         		= pshmem_uchar_broadcast
#pragma weak shmem_ushort_broadcast         		= pshmem_ushort_broadcast
#pragma weak shmem_uint_broadcast         		= pshmem_uint_broadcast
#pragma weak shmem_ulong_broadcast         		= pshmem_ulong_broadcast
#pragma weak shmem_ulonglong_broadcast      		= pshmem_ulonglong_broadcast
#pragma weak shmem_longdouble_broadcast      		= pshmem_longdouble_broadcast
#pragma weak shmem_int8_broadcast         		= pshmem_int8_broadcast
#pragma weak shmem_int16_broadcast         		= pshmem_int16_broadcast
#pragma weak shmem_int32_broadcast         		= pshmem_int32_broadcast
#pragma weak shmem_int64_broadcast         		= pshmem_int64_broadcast
#pragma weak shmem_uint8_broadcast         		= pshmem_uint8_broadcast
#pragma weak shmem_uint16_broadcast         		= pshmem_uint16_broadcast
#pragma weak shmem_uint32_broadcast         		= pshmem_uint32_broadcast
#pragma weak shmem_uint64_broadcast         		= pshmem_uint64_broadcast
#pragma weak shmem_size_broadcast         		= pshmem_size_broadcast
#pragma weak shmem_ptrdiff_broadcast        		= pshmem_ptrdiff_broadcast

#pragma weak shmem_broadcastmem              		= pshmem_broadcastmem

#include "oshmem/shmem/c/profile-defines.h"
#endif

SHMEM_TYPE_BROADCAST(_broadcast32, sizeof(uint32_t))
SHMEM_TYPE_BROADCAST(_broadcast64, sizeof(uint64_t))



#define SHMEM_TYPE_TEAM_BROADCAST(type_name, type, code, postfix)    \
    int  shmem##type_name##postfix(shmem_team_t team, type *dest, const type *source, size_t nelems, int PE_root)   \
    {                                                               \
        int rc = 0;                                                 \
                                                                    \
        RUNTIME_CHECK_INIT();                                       \
                                                                    \
        rc = MCA_SPML_CALL(team_broadcast(                          \
            team, (void*)dest, (void*)source,                       \
                    nelems, PE_root, code));                        \
        RUNTIME_CHECK_RC(rc);                                       \
                                                                    \
        return rc;                                                  \
    }


SHMEM_TYPE_TEAM_BROADCAST(_char, char, SHMEM_CHAR, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_short, short, SHMEM_SHORT, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_int, int, SHMEM_INT, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_long, long, SHMEM_LONG, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_float, float, SHMEM_FLOAT, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_double, double, SHMEM_DOUBLE, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_longlong, long long, SHMEM_LLONG, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_schar, signed char, SHMEM_SCHAR, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_uchar, unsigned char, SHMEM_UCHAR, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_ushort, unsigned short, SHMEM_USHORT, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_uint, unsigned int, SHMEM_UINT, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_ulong, unsigned long, SHMEM_ULONG, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_ulonglong, unsigned long long, SHMEM_ULLONG, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_longdouble, long double, SHMEM_LDOUBLE, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_int8, int8_t, SHMEM_INT8_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_int16, int16_t, SHMEM_INT16_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_int32, int32_t, SHMEM_INT32_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_int64, int64_t, SHMEM_INT64_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_uint8, uint8_t, SHMEM_UINT8_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_uint16, uint16_t, SHMEM_UINT16_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_uint32, uint32_t, SHMEM_UINT32_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_uint64, uint64_t, SHMEM_UINT64_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_size, size_t, SHMEM_SIZE_T, _broadcast)
SHMEM_TYPE_TEAM_BROADCAST(_ptrdiff, ptrdiff_t, SHMEM_PTRDIFF_T, _broadcast)

SHMEM_TYPE_TEAM_BROADCAST(, void, SHMEM_BYTE, _broadcastmem)
