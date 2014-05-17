/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PSHMEM_SHMEMX_H
#define PSHMEM_SHMEMX_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Elemental put routines
 */
OSHMEM_DECLSPEC  void pshmem_char_p(char* addr, char value, int pe);
OSHMEM_DECLSPEC  void pshmem_int16_p(int16_t* addr, int16_t value, int pe);
OSHMEM_DECLSPEC  void pshmem_int32_p(int32_t* addr, int32_t value, int pe);
OSHMEM_DECLSPEC  void pshmem_int64_p(int64_t* addr, int64_t value, int pe);

/*
 * Block data put routines
 */
OSHMEM_DECLSPEC  void pshmem_put16(void *target, const void *source, size_t len, int pe);

/*
 * Strided put routines
 */
OSHMEM_DECLSPEC void pshmem_iput16(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);

/*
 * Elemental get routines
 */
OSHMEM_DECLSPEC  char pshmem_char_g(char* addr, int pe);
OSHMEM_DECLSPEC  int16_t pshmem_int16_g(int16_t* addr, int pe);
OSHMEM_DECLSPEC  int32_t pshmem_int32_g(int32_t* addr, int pe);
OSHMEM_DECLSPEC  int64_t pshmem_int64_g(int64_t* addr, int pe);

/*
 * Block data get routines
 */
OSHMEM_DECLSPEC  void pshmem_get16(void *target, const void *source, size_t len, int pe);

/*
 * Strided get routines
 */
OSHMEM_DECLSPEC void pshmem_iget16(void* target, const void* source, ptrdiff_t tst, ptrdiff_t sst,size_t len, int pe);

/*
 * Atomic operations
 */
/* Atomic swap */
OSHMEM_DECLSPEC int32_t pshmem_int32_swap(int32_t *target, int32_t value, int pe);
OSHMEM_DECLSPEC int64_t pshmem_int64_swap(int64_t *target, int64_t value, int pe);

/* Atomic conditional swap */
OSHMEM_DECLSPEC int32_t pshmem_int32_cswap(int32_t *target, int32_t cond, int32_t value, int pe);
OSHMEM_DECLSPEC int64_t pshmem_int64_cswap(int64_t *target, int64_t cond, int64_t value, int pe);

/* Atomic Fetch&Add */
OSHMEM_DECLSPEC int32_t pshmem_int32_fadd(int32_t *target, int32_t value, int pe);
OSHMEM_DECLSPEC int64_t pshmem_int64_fadd(int64_t *target, int64_t value, int pe);

/* Atomic Fetch&Inc */
OSHMEM_DECLSPEC int32_t pshmem_int32_finc(int32_t *target, int pe);
OSHMEM_DECLSPEC int64_t pshmem_int64_finc(int64_t *target, int pe);

/* Atomic Add*/
OSHMEM_DECLSPEC void pshmem_int32_add(int32_t *target, int32_t value, int pe);
OSHMEM_DECLSPEC void pshmem_int64_add(int64_t *target, int64_t value, int pe);

/* Atomic Inc */
OSHMEM_DECLSPEC void pshmem_int32_inc(int32_t *target, int pe);
OSHMEM_DECLSPEC void pshmem_int64_inc(int64_t *target, int pe);

/*
 * P2P sync routines
 */
OSHMEM_DECLSPEC  void pshmem_int16_wait(int16_t *addr, int16_t value);
OSHMEM_DECLSPEC  void pshmem_int32_wait(int32_t *addr, int32_t value);
OSHMEM_DECLSPEC  void pshmem_int64_wait(int64_t *addr, int64_t value);

OSHMEM_DECLSPEC  void pshmem_int16_wait_until(int16_t *addr, int cmp, int16_t value);
OSHMEM_DECLSPEC  void pshmem_int32_wait_until(int32_t *addr, int cmp, int32_t value);
OSHMEM_DECLSPEC  void pshmem_int64_wait_until(int64_t *addr, int cmp, int64_t value);

/*
 * Reduction routines
 */
OSHMEM_DECLSPEC void pshmem_int16_and_to_all(int16_t *target, int16_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int16_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int32_and_to_all(int32_t *target, int32_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int32_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int64_and_to_all(int64_t *target, int64_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int64_t *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_int16_or_to_all(int16_t *target, int16_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int16_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int32_or_to_all(int32_t *target, int32_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int32_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int64_or_to_all(int64_t *target, int64_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int64_t *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_int16_xor_to_all(int16_t *target, int16_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int16_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int32_xor_to_all(int32_t *target, int32_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int32_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int64_xor_to_all(int64_t *target, int64_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int64_t *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_int16_max_to_all(int16_t *target, int16_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int16_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int32_max_to_all(int32_t *target, int32_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int32_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int64_max_to_all(int64_t *target, int64_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int64_t *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_int16_min_to_all(int16_t *target, int16_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int16_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int32_min_to_all(int32_t *target, int32_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int32_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int64_min_to_all(int64_t *target, int64_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int64_t *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_int16_sum_to_all(int16_t *target, int16_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int16_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int32_sum_to_all(int32_t *target, int32_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int32_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int64_sum_to_all(int64_t *target, int64_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int64_t *pWrk, long *pSync);

OSHMEM_DECLSPEC void pshmem_int16_prod_to_all(int16_t *target, int16_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int16_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int32_prod_to_all(int32_t *target, int32_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int32_t *pWrk, long *pSync);
OSHMEM_DECLSPEC void pshmem_int64_prod_to_all(int64_t *target, int64_t *source, int nreduce, int PE_start, int logPE_stride, int PE_size, int64_t *pWrk, long *pSync);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* PSHMEM_SHMEMX_H */
