/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSHMEM_F77_PROTOTYPES_PSHMEM_H
#define OSHMEM_F77_PROTOTYPES_PSHMEM_H
#include "oshmem_config.h"
#include "oshmem/shmem/fortran/shmem_fortran_pointer.h"

BEGIN_C_DECLS

#define PN(ret, lower_name, upper_name, args) \
  OSHMEM_DECLSPEC ret lower_name##_f args;    \
  OSHMEM_DECLSPEC ret lower_name##_ args;     \
  OSHMEM_DECLSPEC ret lower_name##__ args;    \
  OSHMEM_DECLSPEC ret upper_name args

PN (void, pstart_pes, PSTART_PES, (MPI_Fint npes));
PN (MPI_Fint, pnum_pes, PNUM_PES, (void));
PN (MPI_Fint, pmy_pe, PMY_PE, (void));
OSHMEM_DECLSPEC MPI_Fint p_my_pe_(void);
PN (void, pshmem_barrier_all, PSHMEM_BARRIER_ALL, (void));
PN (void, pshpalloc, PSHPALLOC, (FORTRAN_POINTER_T *addr, MPI_Fint *length, MPI_Fint *errcode, MPI_Fint *abort));
PN (void, pshpdeallc, PSHPDEALLC, (FORTRAN_POINTER_T *addr, MPI_Fint *errcode, MPI_Fint *abort));
PN (void, pshpclmove, PSHPCLMOVE, (FORTRAN_POINTER_T *addr, MPI_Fint *length, MPI_Fint *status, MPI_Fint *abort));
PN (FORTRAN_POINTER_T*, pshmem_ptr, PSHMEM_PTR, (FORTRAN_POINTER_T target, MPI_Fint *pe));
PN (ompi_fortran_logical_t, pshmem_pe_accessible, PSHMEM_PE_ACCESSIBLE, (MPI_Fint *pe));
PN (MPI_Fint, pshmem_addr_accessible, PSHMEM_ADDR_ACCESSIBLE, (FORTRAN_POINTER_T addr, MPI_Fint *pe));
PN (void, pshmem_put, PSHMEM_PUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_character_put, PSHMEM_CHARACTER_PUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_complex_put, PSHMEM_COMPLEX_PUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_double_put, PSHMEM_DOUBLE_PUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_logical_put, PSHMEM_LOGICAL_PUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_integer_put, PSHMEM_INTEGER_PUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_real_put, PSHMEM_REAL_PUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_put4, PSHMEM_PUT4, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_put8, PSHMEM_PUT8, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_put32, PSHMEM_PUT32, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_put64, PSHMEM_PUT64, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_put128, PSHMEM_PUT128, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_putmem, PSHMEM_PUTMEM, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *length, MPI_Fint *pe));
PN (void, pshmem_iput4, PSHMEM_IPUT4, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iput8, PSHMEM_IPUT8, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iput32, PSHMEM_IPUT32, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iput64, PSHMEM_IPUT64, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iput128, PSHMEM_IPUT128, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_complex_iput, PSHMEM_COMPLEX_IPUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_double_iput, PSHMEM_DOUBLE_IPUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_integer_iput, PSHMEM_INTEGER_IPUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_logical_iput, PSHMEM_LOGICAL_IPUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_real_iput, PSHMEM_REAL_IPUT, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_character_get, PSHMEM_CHARACTER_GET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_complex_get, PSHMEM_COMPLEX_GET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_double_get, PSHMEM_DOUBLE_GET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_integer_get, PSHMEM_INTEGER_GET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_get4, PSHMEM_GET4, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_get8, PSHMEM_GET8, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_get32, PSHMEM_GET32, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_get64, PSHMEM_GET64, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_get128, PSHMEM_GET128, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_getmem, PSHMEM_GETMEM, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_logical_get, PSHMEM_LOGICAL_GET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_real_get, PSHMEM_REAL_GET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iget4, PSHMEM_IGET4, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iget8, PSHMEM_IGET8, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iget32, PSHMEM_IGET32, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iget64, PSHMEM_IGET64, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_iget128, PSHMEM_IGET128, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_complex_iget, PSHMEM_COMPLEX_IGET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_double_iget, PSHMEM_DOUBLE_IGET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_integer_iget, PSHMEM_INTEGER_IGET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_logical_iget, PSHMEM_LOGICAL_IGET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (void, pshmem_real_iget, PSHMEM_REAL_IGET, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *tst, MPI_Fint *sst, MPI_Fint *len, MPI_Fint *pe));
PN (MPI_Fint, pshmem_swap, PSHMEM_SWAP, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (ompi_fortran_integer4_t, pshmem_int4_swap, PSHMEM_INT4_SWAP, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (ompi_fortran_integer8_t, pshmem_int8_swap, PSHMEM_INT8_SWAP, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (ompi_fortran_real4_t, pshmem_real4_swap, PSHMEM_REAL4_SWAP, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (ompi_fortran_real8_t, pshmem_real8_swap, PSHMEM_REAL8_SWAP, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (ompi_fortran_integer4_t, pshmem_int4_cswap, PSHMEM_INT4_CSWAP, (FORTRAN_POINTER_T target, MPI_Fint *cond, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (ompi_fortran_integer8_t, pshmem_int8_cswap, PSHMEM_INT8_CSWAP, (FORTRAN_POINTER_T target, MPI_Fint *cond, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (ompi_fortran_integer4_t, pshmem_int4_fadd, PSHMEM_INT4_FADD, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (ompi_fortran_integer8_t, pshmem_int8_fadd, PSHMEM_INT8_FADD, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (void, pshmem_int4_inc, PSHMEM_INT4_INC, (FORTRAN_POINTER_T target, MPI_Fint *pe));
PN (void, pshmem_int8_inc, PSHMEM_INT8_INC, (FORTRAN_POINTER_T target, MPI_Fint *pe));
PN (ompi_fortran_integer4_t, pshmem_int4_finc, PSHMEM_INT4_FINC, (FORTRAN_POINTER_T target, MPI_Fint *pe));
PN (ompi_fortran_integer8_t, pshmem_int8_finc, PSHMEM_INT8_FINC, (FORTRAN_POINTER_T target, MPI_Fint *pe));
PN (void, pshmem_int4_add, PSHMEM_INT4_ADD, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (void, pshmem_int8_add, PSHMEM_INT8_ADD, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe));
PN (void, pshmem_int4_wait, PSHMEM_INT4_WAIT, (ompi_fortran_integer4_t *var, ompi_fortran_integer4_t *value));
PN (void, pshmem_int8_wait, PSHMEM_INT8_WAIT, (ompi_fortran_integer8_t *var, ompi_fortran_integer8_t *value));
PN (void, pshmem_wait, PSHMEM_WAIT, (MPI_Fint *var, MPI_Fint *value));
PN (void, pshmem_int4_wait_until, PSHMEM_INT4_WAIT_UNTIL, (ompi_fortran_integer4_t *var, MPI_Fint *cmp, ompi_fortran_integer4_t *value));
PN (void, pshmem_int8_wait_until, PSHMEM_INT8_WAIT_UNTIL, (ompi_fortran_integer8_t *var, MPI_Fint *cmp, ompi_fortran_integer8_t *value));
PN (void, pshmem_wait_until, PSHMEM_WAIT_UNTIL, (MPI_Fint *var, MPI_Fint *cmp, MPI_Fint *value));
PN (void, pshmem_barrier, PSHMEM_BARRIER, (MPI_Fint *PE_start, MPI_Fint *logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int2_and_to_all, PSHMEM_INT2_AND_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int4_and_to_all, PSHMEM_INT4_AND_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int8_and_to_all, PSHMEM_INT8_AND_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int2_or_to_all, PSHMEM_INT2_OR_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int4_or_to_all, PSHMEM_INT4_OR_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int8_or_to_all, PSHMEM_INT8_OR_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int2_xor_to_all, PSHMEM_INT2_XOR_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int4_xor_to_all, PSHMEM_INT4_XOR_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int8_xor_to_all, PSHMEM_INT8_XOR_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_comp4_xor_to_all, PSHMEM_COMP4_XOR_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_comp8_xor_to_all, PSHMEM_COMP8_XOR_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int2_max_to_all, PSHMEM_INT2_MAX_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int4_max_to_all, PSHMEM_INT4_MAX_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int8_max_to_all, PSHMEM_INT8_MAX_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real4_max_to_all, PSHMEM_REAL4_MAX_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real8_max_to_all, PSHMEM_REAL8_MAX_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real16_max_to_all, PSHMEM_REAL16_MAX_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int2_min_to_all, PSHMEM_INT2_MIN_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int4_min_to_all, PSHMEM_INT4_MIN_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int8_min_to_all, PSHMEM_INT8_MIN_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real4_min_to_all, PSHMEM_REAL4_MIN_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real8_min_to_all, PSHMEM_REAL8_MIN_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real16_min_to_all, PSHMEM_REAL16_MIN_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int2_sum_to_all, PSHMEM_INT2_SUM_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int4_sum_to_all, PSHMEM_INT4_SUM_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int8_sum_to_all, PSHMEM_INT8_SUM_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_comp4_sum_to_all, PSHMEM_COMP4_SUM_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_comp8_sum_to_all, PSHMEM_COMP8_SUM_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real4_sum_to_all, PSHMEM_REAL4_SUM_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real8_sum_to_all, PSHMEM_REAL8_SUM_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real16_sum_to_all, PSHMEM_REAL16_SUM_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int2_prod_to_all, PSHMEM_INT2_PROD_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int4_prod_to_all, PSHMEM_INT4_PROD_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_int8_prod_to_all, PSHMEM_INT8_PROD_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_comp4_prod_to_all, PSHMEM_COMP4_PROD_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_comp8_prod_to_all, PSHMEM_COMP8_PROD_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real4_prod_to_all, PSHMEM_REAL4_PROD_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real8_prod_to_all, PSHMEM_REAL8_PROD_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_real16_prod_to_all, PSHMEM_REAL16_PROD_TO_ALL, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nreduce, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T *pWrk, FORTRAN_POINTER_T pSync));
PN (void, pshmem_collect4, PSHMEM_COLLECT4, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_collect8, PSHMEM_COLLECT8, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_collect32, PSHMEM_COLLECT32, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_collect64, PSHMEM_COLLECT64, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_fcollect4, PSHMEM_FCOLLECT4, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_fcollect8, PSHMEM_FCOLLECT8, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_fcollect32, PSHMEM_FCOLLECT32, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_fcollect64, PSHMEM_FCOLLECT64, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_broadcast4, PSHMEM_BROADCAST4, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_root, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_broadcast8, PSHMEM_BROADCAST8, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_root, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_broadcast32, PSHMEM_BROADCAST32, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_root, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_broadcast64, PSHMEM_BROADCAST64, (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *nlong, MPI_Fint *PE_root, MPI_Fint *PE_start, MPI_Fint * logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync));
PN (void, pshmem_set_lock, PSHMEM_SET_LOCK, (FORTRAN_POINTER_T lock));
PN (void, pshmem_clear_lock, PSHMEM_CLEAR_LOCK, (FORTRAN_POINTER_T lock));
PN (MPI_Fint, pshmem_test_lock, PSHMEM_TEST_LOCK, (FORTRAN_POINTER_T lock));
PN (void, pshmem_set_cache_inv, PSHMEM_SET_CACHE_INV, (void));
PN (void, pshmem_set_cache_line_inv, PSHMEM_SET_CACHE_LINE_INV, (FORTRAN_POINTER_T target));
PN (void, pshmem_clear_cache_inv, PSHMEM_CLEAR_CACHE_INV, (void));
PN (void, pshmem_clear_cache_line_inv, PSHMEM_CLEAR_CACHE_LINE_INV, (FORTRAN_POINTER_T target));
PN (void, pshmem_udcflush, PSHMEM_UDCFLUSH, (void));
PN (void, pshmem_udcflush_line, PSHMEM_UDCFLUSH_LINE, (FORTRAN_POINTER_T target));
PN (void, pshmem_fence, PSHMEM_FENCE, (void));
PN (void, pshmem_quiet, PSHMEM_QUIET, (void));

END_C_DECLS
#endif /*OSHMEM_F77_PROTOTYPES_PSHMEM_H*/
