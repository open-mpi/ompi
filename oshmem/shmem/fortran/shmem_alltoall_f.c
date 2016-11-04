/*
 * Copyright (c) 2013-2016 Mellanox Technologies, Inc.
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
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_ALLTOALL32, shmem_alltoall32)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_ALLTOALL64, shmem_alltoall64)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_ALLTOALLS32, shmem_alltoalls32)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_ALLTOALLS64, shmem_alltoalls64)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_ALLTOALL32,
        shmem_alltoall32_,
        shmem_alltoall32__,
        shmem_alltoall32_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync),
        (target, source, nlong, PE_start, logPE_stride, PE_size, pSync))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_ALLTOALL64,
        shmem_alltoall64_,
        shmem_alltoall64__,
        shmem_alltoall64_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync),
        (target, source, nlong, PE_start, logPE_stride, PE_size, pSync))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_ALLTOALLS32,
        shmem_alltoalls32_,
        shmem_alltoalls32__,
        shmem_alltoalls32_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *dst, MPI_Fint *sst, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync),
        (target, source, dst, sst, nlong, PE_start, logPE_stride, PE_size, pSync))

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_ALLTOALLS64,
        shmem_alltoalls64_,
        shmem_alltoalls64__,
        shmem_alltoalls64_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *dst, MPI_Fint *sst, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync),
        (target, source, dst, sst, nlong, PE_start, logPE_stride, PE_size, pSync))

#define SHMEM_ALLTOALL(F_NAME, T_NAME, OSHMEM_GROUP_CACHE_ENABLED) void F_NAME(FORTRAN_POINTER_T target, \
    FORTRAN_POINTER_T source, \
    MPI_Fint *nlong,\
    MPI_Fint *PE_start, \
    MPI_Fint *logPE_stride, \
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
\
            /* Call collective broadcast operation */\
            rc = group->g_scoll.scoll_alltoall( group, \
                FPTR_2_VOID_PTR(target), \
                FPTR_2_VOID_PTR(source), \
                1, \
                1, \
                OMPI_FINT_2_INT(*nlong), \
                op->dt_size, \
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

#define SHMEM_ALLTOALLS(F_NAME, T_NAME, OSHMEM_GROUP_CACHE_ENABLED) void F_NAME(FORTRAN_POINTER_T target, \
    FORTRAN_POINTER_T source, \
    MPI_Fint *dst,\
    MPI_Fint *sst,\
    MPI_Fint *nlong,\
    MPI_Fint *PE_start, \
    MPI_Fint *logPE_stride, \
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
\
            /* Call collective broadcast operation */\
            rc = group->g_scoll.scoll_alltoall( group, \
                FPTR_2_VOID_PTR(target), \
                FPTR_2_VOID_PTR(source), \
		        OMPI_FINT_2_INT(*dst), \
			    OMPI_FINT_2_INT(*sst), \
                OMPI_FINT_2_INT(*nlong), \
                op->dt_size, \
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

SHMEM_ALLTOALL(shmem_alltoall32_f, oshmem_op_prod_fint4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_ALLTOALL(shmem_alltoall64_f, oshmem_op_prod_fint8, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_ALLTOALLS(shmem_alltoalls32_f, oshmem_op_prod_fint4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_ALLTOALLS(shmem_alltoalls64_f, oshmem_op_prod_fint8, OSHMEM_GROUP_CACHE_ENABLED)
