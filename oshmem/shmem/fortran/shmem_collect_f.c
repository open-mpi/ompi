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
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_COLLECT4, shmem_collect4)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_COLLECT8, shmem_collect8)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_COLLECT32, shmem_collect32)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_COLLECT64, shmem_collect64)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_FCOLLECT4, shmem_fcollect4)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_FCOLLECT8, shmem_fcollect8)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_FCOLLECT32, shmem_fcollect32)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_FCOLLECT64, shmem_fcollect64)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_COLLECT4,
        shmem_collect4_,
        shmem_collect4__,
        shmem_collect4_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target,source,nlong,PE_start,logPE_stride,PE_size,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_COLLECT8,
        shmem_collect8_,
        shmem_collect8__,
        shmem_collect8_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target,source,nlong,PE_start,logPE_stride,PE_size,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_COLLECT32,
        shmem_collect32_,
        shmem_collect32__,
        shmem_collect32_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target,source,nlong,PE_start,logPE_stride,PE_size,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_COLLECT64,
        shmem_collect64_,
        shmem_collect64__,
        shmem_collect64_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target,source,nlong,PE_start,logPE_stride,PE_size,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_FCOLLECT4,
        shmem_fcollect4_,
        shmem_fcollect4__,
        shmem_fcollect4_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target,source,nlong,PE_start,logPE_stride,PE_size,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_FCOLLECT8,
        shmem_fcollect8_,
        shmem_fcollect8__,
        shmem_fcollect8_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target,source,nlong,PE_start,logPE_stride,PE_size,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_FCOLLECT32,
        shmem_fcollect32_,
        shmem_fcollect32__,
        shmem_fcollect32_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target,source,nlong,PE_start,logPE_stride,PE_size,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_FCOLLECT64,
        shmem_fcollect64_,
        shmem_fcollect64__,
        shmem_fcollect64_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (target,source,nlong,PE_start,logPE_stride,PE_size,pSync) )

#define SHMEM_COLLECT(F_NAME, T_NAME, OSHMEM_GROUP_CACHE_ENABLED) void F_NAME(FORTRAN_POINTER_T target, \
    FORTRAN_POINTER_T source, \
    MPI_Fint *nlong, \
    MPI_Fint *PE_start, \
    MPI_Fint * logPE_stride, \
    MPI_Fint *PE_size, \
    FORTRAN_POINTER_T pSync)\
{\
    int rc = OSHMEM_SUCCESS;\
    oshmem_group_t*  group = NULL;\
    {\
        /* Create group basing PE_start, logPE_stride and PE_size */\
        if (OSHMEM_GROUP_CACHE_ENABLED == 0)\
        {\
            group = oshmem_proc_group_create(OMPI_FINT_2_INT(*PE_start), \
                (1 << OMPI_FINT_2_INT(*logPE_stride)), \
                OMPI_FINT_2_INT(*PE_size));\
            if (!group)\
                rc = OSHMEM_ERROR;\
        }\
        else\
        {\
            group = find_group_in_cache(OMPI_FINT_2_INT(*PE_start),\
                OMPI_FINT_2_INT(*logPE_stride),\
                OMPI_FINT_2_INT(*PE_size));\
            if (!group)\
            {\
                group = oshmem_proc_group_create(OMPI_FINT_2_INT(*PE_start), \
                    (1 << OMPI_FINT_2_INT(*logPE_stride)), \
                    OMPI_FINT_2_INT(*PE_size));\
                if (!group)\
                    rc = OSHMEM_ERROR;\
                cache_group(group,OMPI_FINT_2_INT(*PE_start),\
                    OMPI_FINT_2_INT(*logPE_stride),\
                    OMPI_FINT_2_INT(*PE_size));\
            }\
        } /* OSHMEM_GROUP_CACHE_ENABLED */\
        /* Collective operation call */\
        if ( rc == OSHMEM_SUCCESS )\
        {\
            oshmem_op_t* op = T_NAME;\
            /* Call collective broadcast operation */\
            rc = group->g_scoll.scoll_collect( group, \
                FPTR_2_VOID_PTR(target), \
                FPTR_2_VOID_PTR(source), \
                OMPI_FINT_2_INT(*nlong) * op->dt_size, \
                FPTR_2_VOID_PTR(pSync), \
                false, SCOLL_DEFAULT_ALG);\
        }\
        if (OSHMEM_GROUP_CACHE_ENABLED == 0)\
        {\
            if ( rc == OSHMEM_SUCCESS )\
            {\
                oshmem_proc_group_destroy(group);\
            }\
        }/* OSHMEM_GROUP_CACHE_ENABLED */\
    }\
}

SHMEM_COLLECT(shmem_collect4_f, oshmem_op_prod_fint4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_COLLECT(shmem_collect8_f, oshmem_op_prod_fint8, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_COLLECT(shmem_collect32_f, oshmem_op_prod_fint4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_COLLECT(shmem_collect64_f, oshmem_op_prod_fint8, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_COLLECT(shmem_fcollect4_f, oshmem_op_prod_freal4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_COLLECT(shmem_fcollect8_f, oshmem_op_prod_freal8, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_COLLECT(shmem_fcollect32_f, oshmem_op_prod_freal4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_COLLECT(shmem_fcollect64_f, oshmem_op_prod_freal8, OSHMEM_GROUP_CACHE_ENABLED)
