/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/shmem/fortran/bindings.h"
#include "oshmem/include/shmem.h"
#include "oshmem/constants.h"
#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/proc/proc.h"
#include "oshmem/proc/proc_group_cache.h"
#include "oshmem/op/op.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_BROADCAST4, shmem_broadcast4)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_BROADCAST8, shmem_broadcast8)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_BROADCAST32, shmem_broadcast32)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_BROADCAST64, shmem_broadcast64)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_BROADCAST4,
        shmem_broadcast4_,
        shmem_broadcast4__,
        shmem_broadcast4_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_root, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target, source, nlong, PE_root, PE_start, logPE_stride, PE_size, pSync))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_BROADCAST8,
        shmem_broadcast8_,
        shmem_broadcast8__,
        shmem_broadcast8_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_root, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target, source, nlong, PE_root, PE_start, logPE_stride, PE_size, pSync))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_BROADCAST32,
        shmem_broadcast32_,
        shmem_broadcast32__,
        shmem_broadcast32_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_root, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target, source, nlong, PE_root, PE_start, logPE_stride, PE_size, pSync))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_BROADCAST64,
        shmem_broadcast64_,
        shmem_broadcast64__,
        shmem_broadcast64_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_root, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target, source, nlong, PE_root, PE_start, logPE_stride, PE_size, pSync))

#define SHMEM_BROADCAST(F_NAME, T_NAME, OSHMEM_GROUP_CACHE_ENABLED) void F_NAME(FORTRAN_POINTER_T target, \
    FORTRAN_POINTER_T source, \
    MPI_Fint *nlong,\
    MPI_Fint *PE_root, \
    MPI_Fint *PE_start, \
    MPI_Fint *logPE_stride, \
    MPI_Fint *PE_size, \
    FORTRAN_POINTER_T pSync)\
{\
    int rc = OSHMEM_SUCCESS;\
    oshmem_group_t*  group = NULL;\
\
    if ((0 <= OMPI_FINT_2_INT(*PE_root)) && \
        (OMPI_FINT_2_INT(*PE_root) < OMPI_FINT_2_INT(*PE_size)))\
    {\
        /* Create group basing PE_start, logPE_stride and PE_size */\
        if (OSHMEM_GROUP_CACHE_ENABLED == 0)\
        {\
            group = oshmem_proc_group_create(OMPI_FINT_2_INT(*PE_start), \
                (1 << OMPI_FINT_2_INT(*logPE_stride)), \
                OMPI_FINT_2_INT(*PE_size));\
            if (!group || (OMPI_FINT_2_INT(*PE_root) >= group->proc_count))\
            {\
                rc = OSHMEM_ERROR;\
            }\
        }\
        else\
        {\
             group = find_group_in_cache(OMPI_FINT_2_INT(*PE_start),\
                OMPI_FINT_2_INT(*logPE_stride),\
                OMPI_FINT_2_INT(*PE_size));\
            if (!group)\
            {\
                group = oshmem_proc_group_create(OMPI_FINT_2_INT(*PE_start),\
                    (1 << OMPI_FINT_2_INT(*logPE_stride)),\
                    OMPI_FINT_2_INT(*PE_size));\
                if (!group || (OMPI_FINT_2_INT(*PE_root) >= group->proc_count))\
                {\
                    rc = OSHMEM_ERROR;\
                }\
                cache_group(group,OMPI_FINT_2_INT(*PE_start),\
                    OMPI_FINT_2_INT(*logPE_stride),\
                    OMPI_FINT_2_INT(*PE_size));\
            }\
        } /* OSHMEM_GROUP_CACHE_ENABLED */\
        /* Collective operation call */\
        if ( rc == OSHMEM_SUCCESS )\
        {\
            int rel_PE_root = 0;\
            oshmem_op_t* op = T_NAME;\
\
            /* Define actual PE using relative in active set */\
            rel_PE_root = oshmem_proc_pe(group->proc_array[OMPI_FINT_2_INT(*PE_root)]);\
\
            /* Call collective broadcast operation */\
            rc = group->g_scoll.scoll_broadcast( group, \
                rel_PE_root, \
                FPTR_2_VOID_PTR(target), \
                FPTR_2_VOID_PTR(source), \
                OMPI_FINT_2_INT(*nlong) * op->dt_size, \
                FPTR_2_VOID_PTR(pSync), SCOLL_DEFAULT_ALG );\
        }\
        if (OSHMEM_GROUP_CACHE_ENABLED == 0) \
        {\
            if ( group )\
            {\
                oshmem_proc_group_destroy(group);\
            }\
        } /* OSHMEM_GROUP_CACHE_ENABLED */\
    }\
}

SHMEM_BROADCAST(shmem_broadcast4_f, oshmem_op_prod_fint4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_BROADCAST(shmem_broadcast8_f, oshmem_op_prod_fint8, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_BROADCAST(shmem_broadcast32_f, oshmem_op_prod_fint4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_BROADCAST(shmem_broadcast64_f, oshmem_op_prod_fint8, OSHMEM_GROUP_CACHE_ENABLED)
