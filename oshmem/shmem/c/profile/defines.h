/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OSHMEM_C_PROFILE_DEFINES_H
#define OSHMEM_C_PROFILE_DEFINES_H
/*
 * This file is included in the top directory only if 
 * profiling is required. Once profiling is required,
 * this file will replace all shmem_* symbols with 
 * pshmem_* symbols
 */

/*
 * Initialization routines
 */
#define start_pes                   pstart_pes

/*
 * Finalization routines
 */
#define shmem_finalize              pshmem_finalize


/* 
 * Query routines 
 */
#define _num_pes                    p_num_pes
#define _my_pe                      p_my_pe


/* 
 * Accessability routines 
 */
#define shmem_pe_accessible         pshmem_pe_accessible
#define shmem_addr_accessible       pshmem_addr_accessible

/* 
 * Symmetric heap routines 
 */
#define shmalloc                    pshmalloc
#define shmemalign                  pshmemalign
#define shrealloc                   pshrealloc
#define shfree                      pshfree

/* 
 * Remote pointer operations 
 */
#define shmem_ptr                   pshmem_ptr

/* 
 * Elemental put routines 
 */
#define shmem_char_p                pshmem_char_p
#define shmem_short_p               pshmem_short_p
#define shmem_int_p                 pshmem_int_p
#define shmem_long_p                pshmem_long_p
#define shmem_float_p               pshmem_float_p
#define shmem_double_p              pshmem_double_p
#define shmem_longlong_p            pshmem_longlong_p
#define shmem_longdouble_p          pshmem_longdouble_p
#define shmem_int16_p               pshmem_int16_p
#define shmem_int32_p               pshmem_int32_p
#define shmem_int64_p               pshmem_int64_p

/* 
 * Block data put routines 
 */
#define shmem_char_put              pshmem_char_put
#define shmem_short_put             pshmem_short_put
#define shmem_int_put               pshmem_int_put
#define shmem_long_put              pshmem_long_put
#define shmem_float_put             pshmem_float_put
#define shmem_double_put            pshmem_double_put
#define shmem_longlong_put          pshmem_longlong_put
#define shmem_longdouble_put        pshmem_longdouble_put
#define shmem_put16                 pshmem_put16
#define shmem_put32                 pshmem_put32
#define shmem_put64                 pshmem_put64
#define shmem_put128                pshmem_put128
#define shmem_putmem                pshmem_putmem

/* 
 * Strided put routines 
 */
#define shmem_int_iput              pshmem_int_iput
#define shmem_short_iput            pshmem_short_iput
#define shmem_float_iput            pshmem_float_iput
#define shmem_double_iput           pshmem_double_iput
#define shmem_longlong_iput         pshmem_longlong_iput
#define shmem_longdouble_iput       pshmem_longdouble_iput
#define shmem_long_iput             pshmem_long_iput
#define shmem_iput16                pshmem_iput16
#define shmem_iput32                pshmem_iput32
#define shmem_iput64                pshmem_iput64
#define shmem_iput128               pshmem_iput128

/* 
 * Elemental get routines 
 */
#define shmem_char_g                pshmem_char_g
#define shmem_short_g               pshmem_short_g
#define shmem_int_g                 pshmem_int_g
#define shmem_long_g                pshmem_long_g
#define shmem_float_g               pshmem_float_g
#define shmem_double_g              pshmem_double_g
#define shmem_longlong_g            pshmem_longlong_g
#define shmem_longdouble_g          pshmem_longdouble_g
#define shmem_int16_g               pshmem_int16_g
#define shmem_int32_g               pshmem_int32_g
#define shmem_int64_g               pshmem_int64_g

/* 
 * Block data get routines 
 */
#define shmem_char_get              pshmem_char_get
#define shmem_short_get             pshmem_short_get
#define shmem_int_get               pshmem_int_get
#define shmem_long_get              pshmem_long_get
#define shmem_float_get             pshmem_float_get
#define shmem_double_get            pshmem_double_get
#define shmem_longlong_get          pshmem_longlong_get
#define shmem_longdouble_get        pshmem_longdouble_get
#define shmem_get16                 pshmem_get16
#define shmem_get32                 pshmem_get32
#define shmem_get64                 pshmem_get64
#define shmem_get128                pshmem_get128
#define shmem_getmem                pshmem_getmem

/* 
 * Strided get routines 
 */
#define shmem_int_iget              pshmem_int_iget
#define shmem_short_iget            pshmem_short_iget
#define shmem_float_iget            pshmem_float_iget
#define shmem_double_iget           pshmem_double_iget
#define shmem_longlong_iget         pshmem_longlong_iget
#define shmem_longdouble_iget       pshmem_longdouble_iget
#define shmem_long_iget             pshmem_long_iget
#define shmem_iget16                pshmem_iget16
#define shmem_iget32                pshmem_iget32
#define shmem_iget64                pshmem_iget64
#define shmem_iget128               pshmem_iget128

/* 
 * Atomic operations 
 */
/* Atomic swap */
#define shmem_swap                  pshmem_swap
#define shmem_double_swap           pshmem_double_swap
#define shmem_float_swap            pshmem_float_swap
#define shmem_int_swap              pshmem_int_swap
#define shmem_long_swap             pshmem_long_swap
#define shmem_longlong_swap         pshmem_longlong_swap
#define shmem_int32_swap            pshmem_int32_swap
#define shmem_int64_swap            pshmem_int64_swap


/* Atomic conditional swap */
#define shmem_int_cswap             pshmem_int_cswap
#define shmem_long_cswap            pshmem_long_cswap
#define shmem_longlong_cswap        pshmem_longlong_cswap
#define shmem_int32_cswap           pshmem_int32_cswap
#define shmem_int64_cswap           pshmem_int64_cswap


/* Atomic Fetch&Add */
#define shmem_int_fadd              pshmem_int_fadd
#define shmem_long_fadd             pshmem_long_fadd
#define shmem_longlong_fadd         pshmem_longlong_fadd
#define shmem_int32_fadd            pshmem_int32_fadd
#define shmem_int64_fadd            pshmem_int64_fadd

/* Atomic Fetch&Inc */
#define shmem_int_finc              pshmem_int_finc
#define shmem_long_finc             pshmem_long_finc
#define shmem_longlong_finc         pshmem_longlong_finc
#define shmem_int32_finc            pshmem_int32_finc
#define shmem_int64_finc            pshmem_int64_finc

/* Atomic Add*/
#define shmem_int_add               pshmem_int_add
#define shmem_long_add              pshmem_long_add
#define shmem_longlong_add          pshmem_longlong_add
#define shmem_int32_add             pshmem_int32_add
#define shmem_int64_add             pshmem_int64_add

/* Atomic Inc */
#define shmem_int_inc               pshmem_int_inc
#define shmem_long_inc              pshmem_long_inc
#define shmem_longlong_inc          pshmem_longlong_inc
#define shmem_int32_inc             pshmem_int32_inc
#define shmem_int64_inc             pshmem_int64_inc

/* 
 * Lock functions 
 */
#define shmem_set_lock              pshmem_set_lock
#define shmem_clear_lock            pshmem_clear_lock
#define shmem_test_lock             pshmem_test_lock

/* 
 * P2P sync routines 
 */
#define shmem_short_wait            pshmem_short_wait
#define shmem_int_wait              pshmem_int_wait
#define shmem_long_wait             pshmem_long_wait
#define shmem_longlong_wait         pshmem_longlong_wait
#define shmem_wait                  pshmem_wait
#define shmem_int16_wait            pshmem_int16_wait
#define shmem_int32_wait            pshmem_int32_wait
#define shmem_int64_wait            pshmem_int64_wait

#define shmem_short_wait_until      pshmem_short_wait_until
#define shmem_int_wait_until        pshmem_int_wait_until
#define shmem_long_wait_until       pshmem_long_wait_until
#define shmem_longlong_wait_until   pshmem_longlong_wait_until
#define shmem_wait_until            pshmem_wait_until
#define shmem_int16_wait_until      pshmem_int16_wait_until
#define shmem_int32_wait_until      pshmem_int32_wait_until
#define shmem_int64_wait_until      pshmem_int64_wait_until

/* 
 * Barrier sync routines 
 */
#define shmem_barrier               pshmem_barrier
#define shmem_barrier_all           pshmem_barrier_all
#define shmem_fence                 pshmem_fence
#define shmem_quiet                 pshmem_quiet

/* 
 * Collective routines 
 */
#define shmem_broadcast32           pshmem_broadcast32
#define shmem_broadcast64           pshmem_broadcast64
#define shmem_broadcast             pshmem_broadcast
#define shmem_collect32             pshmem_collect32
#define shmem_collect64             pshmem_collect64
#define shmem_fcollect32            pshmem_fcollect32
#define shmem_fcollect64            pshmem_fcollect64

/* 
 * Reduction routines 
 */
#define shmem_short_and_to_all      pshmem_short_and_to_all
#define shmem_int_and_to_all        pshmem_int_and_to_all
#define shmem_long_and_to_all       pshmem_long_and_to_all
#define shmem_longlong_and_to_all   pshmem_longlong_and_to_all
#define shmem_int16_and_to_all      pshmem_int16_and_to_all
#define shmem_int32_and_to_all      pshmem_int32_and_to_all
#define shmem_int64_and_to_all      pshmem_int64_and_to_all

#define shmem_short_or_to_all       pshmem_short_or_to_all
#define shmem_int_or_to_all         pshmem_int_or_to_all
#define shmem_long_or_to_all        pshmem_long_or_to_all
#define shmem_longlong_or_to_all    pshmem_longlong_or_to_all
#define shmem_int16_or_to_all      pshmem_int16_or_to_all
#define shmem_int32_or_to_all      pshmem_int32_or_to_all
#define shmem_int64_or_to_all      pshmem_int64_or_to_all

#define shmem_short_xor_to_all      pshmem_short_xor_to_all
#define shmem_int_xor_to_all        pshmem_int_xor_to_all
#define shmem_long_xor_to_all       pshmem_long_xor_to_all
#define shmem_longlong_xor_to_all   pshmem_longlong_xor_to_all
#define shmem_int16_xor_to_all      pshmem_int16_xor_to_all
#define shmem_int32_xor_to_all      pshmem_int32_xor_to_all
#define shmem_int64_xor_to_all      pshmem_int64_xor_to_all

#define shmem_short_max_to_all      pshmem_short_max_to_all
#define shmem_int_max_to_all        pshmem_int_max_to_all
#define shmem_long_max_to_all       pshmem_long_max_to_all
#define shmem_longlong_max_to_all   pshmem_longlong_max_to_all
#define shmem_float_max_to_all      pshmem_float_max_to_all
#define shmem_double_max_to_all     pshmem_double_max_to_all
#define shmem_longdouble_max_to_all pshmem_longdouble_max_to_all
#define shmem_int16_max_to_all      pshmem_int16_max_to_all
#define shmem_int32_max_to_all      pshmem_int32_max_to_all
#define shmem_int64_max_to_all      pshmem_int64_max_to_all

#define shmem_short_min_to_all      pshmem_short_min_to_all
#define shmem_int_min_to_all        pshmem_int_min_to_all
#define shmem_long_min_to_all       pshmem_long_min_to_all
#define shmem_longlong_min_to_all   pshmem_longlong_min_to_all
#define shmem_float_min_to_all      pshmem_float_min_to_all
#define shmem_double_min_to_all     pshmem_double_min_to_all
#define shmem_longdouble_min_to_all pshmem_longdouble_min_to_all
#define shmem_int16_min_to_all      pshmem_int16_min_to_all
#define shmem_int32_min_to_all      pshmem_int32_min_to_all
#define shmem_int64_min_to_all      pshmem_int64_min_to_all

#define shmem_short_sum_to_all      pshmem_short_sum_to_all
#define shmem_int_sum_to_all        pshmem_int_sum_to_all
#define shmem_long_sum_to_all       pshmem_long_sum_to_all
#define shmem_longlong_sum_to_all   pshmem_longlong_sum_to_all
#define shmem_float_sum_to_all      pshmem_float_sum_to_all
#define shmem_double_sum_to_all     pshmem_double_sum_to_all
#define shmem_longdouble_sum_to_all pshmem_longdouble_sum_to_all
#define shmem_complexf_sum_to_all   pshmem_complexf_sum_to_all
#define shmem_complexd_sum_to_all   pshmem_complexd_sum_to_all
#define shmem_int16_sum_to_all      pshmem_int16_sum_to_all
#define shmem_int32_sum_to_all      pshmem_int32_sum_to_all
#define shmem_int64_sum_to_all      pshmem_int64_sum_to_all

#define shmem_short_prod_to_all     pshmem_short_prod_to_all
#define shmem_int_prod_to_all       pshmem_int_prod_to_all
#define shmem_long_prod_to_all      pshmem_long_prod_to_all
#define shmem_longlong_prod_to_all  pshmem_longlong_prod_to_all
#define shmem_float_prod_to_all     pshmem_float_prod_to_all
#define shmem_double_prod_to_all    pshmem_double_prod_to_all
#define shmem_longdouble_prod_to_all pshmem_longdouble_prod_to_all
#define shmem_complexf_prod_to_all  pshmem_complexf_prod_to_all
#define shmem_complexd_prod_to_all  pshmem_complexd_prod_to_all
#define shmem_int16_prod_to_all      pshmem_int16_prod_to_all
#define shmem_int32_prod_to_all      pshmem_int32_prod_to_all
#define shmem_int64_prod_to_all      pshmem_int64_prod_to_all

/*
 * Platform specific cache management routines
 */
#define shmem_udcflush              pshmem_udcflush
#define shmem_udcflush_line         pshmem_udcflush_line
#define shmem_set_cache_inv         pshmem_set_cache_inv
#define shmem_set_cache_line_inv    pshmem_set_cache_line_inv
#define shmem_clear_cache_inv       pshmem_clear_cache_inv
#define shmem_clear_cache_line_inv  pshmem_clear_cache_line_inv

#endif /* OSHMEM_C_PROFILE_DEFINES_H */
