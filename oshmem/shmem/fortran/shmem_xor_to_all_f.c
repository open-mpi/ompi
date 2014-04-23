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
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INT2_XOR_TO_ALL, shmem_int2_xor_to_all)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INT4_XOR_TO_ALL, shmem_int4_xor_to_all)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INT8_XOR_TO_ALL, shmem_int8_xor_to_all)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_COMP4_XOR_TO_ALL, shmem_comp4_xor_to_all)
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_COMP8_XOR_TO_ALL, shmem_comp8_xor_to_all)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INT2_XOR_TO_ALL,
        shmem_int2_xor_to_all_,
        shmem_int2_xor_to_all__,
        shmem_int2_xor_to_all_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync), 
        (target,source,nreduce,PE_start,logPE_stride,PE_size,pWrk,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INT4_XOR_TO_ALL,
        shmem_int4_xor_to_all_,
        shmem_int4_xor_to_all__,
        shmem_int4_xor_to_all_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync), 
        (target,source,nreduce,PE_start,logPE_stride,PE_size,pWrk,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INT8_XOR_TO_ALL,
        shmem_int8_xor_to_all_,
        shmem_int8_xor_to_all__,
        shmem_int8_xor_to_all_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync), 
        (target,source,nreduce,PE_start,logPE_stride,PE_size,pWrk,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_COMP4_XOR_TO_ALL,
        shmem_comp4_xor_to_all_,
        shmem_comp4_xor_to_all__,
        shmem_comp4_xor_to_all_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync), 
        (target,source,nreduce,PE_start,logPE_stride,PE_size,pWrk,pSync) )

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_COMP8_XOR_TO_ALL,
        shmem_comp8_xor_to_all_,
        shmem_comp8_xor_to_all__,
        shmem_comp8_xor_to_all_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync), 
        (target,source,nreduce,PE_start,logPE_stride,PE_size,pWrk,pSync) )

#define SHMEM_XOR_TO_ALL(F_NAME, T_NAME, OSHMEM_GROUP_CACHE_ENABLED) void F_NAME(FORTRAN_POINTER_T target, \
    FORTRAN_POINTER_T source, \
    MPI_Fint *nreduce,\
    MPI_Fint *PE_start,\
    MPI_Fint * logPE_stride,\
    MPI_Fint *PE_size,\
    FORTRAN_POINTER_T *pWrk,\
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
        }\
        /* Collective operation call */\
        if ( rc == OSHMEM_SUCCESS )\
        {\
            oshmem_op_t* op = T_NAME;\
            size_t size = OMPI_FINT_2_INT(*nreduce) * op->dt_size;\
\
            /* Call collective reduce operation */\
            rc = group->g_scoll.scoll_reduce( group,\
                op,\
                FPTR_2_VOID_PTR(target),\
                FPTR_2_VOID_PTR(source),\
                size,\
                FPTR_2_VOID_PTR(pSync),\
                FPTR_2_VOID_PTR(*pWrk), SCOLL_DEFAULT_ALG);\
        }\
        if (OSHMEM_GROUP_CACHE_ENABLED == 0)\
        {\
            if ( rc == OSHMEM_SUCCESS )\
            {\
                oshmem_proc_group_destroy(group);\
            }\
        }\
    }\
}

SHMEM_XOR_TO_ALL(shmem_int2_xor_to_all_f, oshmem_op_xor_fint2, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_XOR_TO_ALL(shmem_int4_xor_to_all_f, oshmem_op_xor_fint4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_XOR_TO_ALL(shmem_int8_xor_to_all_f, oshmem_op_xor_fint8, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_XOR_TO_ALL(shmem_comp4_xor_to_all_f, oshmem_op_xor_fint4, OSHMEM_GROUP_CACHE_ENABLED)
SHMEM_XOR_TO_ALL(shmem_comp8_xor_to_all_f, oshmem_op_xor_fint8, OSHMEM_GROUP_CACHE_ENABLED)
