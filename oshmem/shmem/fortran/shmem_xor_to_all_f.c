/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
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

#define SHMEM_XOR_TO_ALL(F_NAME, T_NAME) void F_NAME(FORTRAN_POINTER_T target, \
    FORTRAN_POINTER_T source, \
    MPI_Fint *nreduce,\
    MPI_Fint *PE_start,\
    MPI_Fint * logPE_stride,\
    MPI_Fint *PE_size,\
    FORTRAN_POINTER_T *pWrk,\
    FORTRAN_POINTER_T pSync)\
{\
    int rc;\
    oshmem_group_t *group;\
    /* Create group basing PE_start, logPE_stride and PE_size */\
    group = oshmem_proc_group_create_nofail(OMPI_FINT_2_INT(*PE_start), \
                                            (1 << OMPI_FINT_2_INT(*logPE_stride)), \
                                            OMPI_FINT_2_INT(*PE_size));\
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
    oshmem_proc_group_destroy(group);\
    RUNTIME_CHECK_RC(rc); \
}

SHMEM_XOR_TO_ALL(shmem_int2_xor_to_all_f, oshmem_op_xor_fint2)
SHMEM_XOR_TO_ALL(shmem_int4_xor_to_all_f, oshmem_op_xor_fint4)
SHMEM_XOR_TO_ALL(shmem_int8_xor_to_all_f, oshmem_op_xor_fint8)
SHMEM_XOR_TO_ALL(shmem_comp4_xor_to_all_f, oshmem_op_xor_fint4)
SHMEM_XOR_TO_ALL(shmem_comp8_xor_to_all_f, oshmem_op_xor_fint8)
