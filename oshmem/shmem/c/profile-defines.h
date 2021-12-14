/*
 * Copyright (c) 2013-2017 Mellanox Technologies, Inc.
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
#define shmem_init                   pshmem_init
#define shmem_init_thread            pshmem_init_thread
#define start_pes                    pstart_pes /* shmem-compat.h */

/*
 * Finalization routines
 */
#define shmem_finalize               pshmem_finalize
#define shmem_global_exit            pshmem_global_exit

/*
 * Query routines
 */
#define shmem_n_pes                  pshmem_n_pes
#define shmem_query_thread           pshmem_query_thread
#define shmem_my_pe                  pshmem_my_pe
#define _num_pes                     p_num_pes /* shmem-compat.h */
#define _my_pe                       p_my_pe /* shmem-compat.h */

/*
 * Accessability routines
 */
#define shmem_pe_accessible          pshmem_pe_accessible
#define shmem_addr_accessible        pshmem_addr_accessible

/*
 * Symmetric heap routines
 */
#define shmem_malloc                 pshmem_malloc
#define shmem_calloc                 pshmem_calloc
#define shmem_align                  pshmem_align
#define shmem_realloc                pshmem_realloc
#define shmem_malloc_with_hints      pshmem_malloc_with_hints
#define shmem_free                   pshmem_free
#define shmalloc                     pshmalloc /* shmem-compat.h */
#define shmemalign                   pshmemalign /* shmem-compat.h */
#define shrealloc                    pshrealloc /* shmem-compat.h */
#define shfree                       pshfree /* shmem-compat.h */

/*
 * Remote pointer operations
 */
#define shmem_ptr                    pshmem_ptr

/*
 * Communication context operations
 */
#define shmem_ctx_create             pshmem_ctx_create
#define shmem_ctx_destroy            pshmem_ctx_destroy


/*
 * Team management routines
 */
#define shmem_team_sync              pshmem_team_sync
#define shmem_team_my_pe             pshmem_team_my_pe
#define shmem_team_n_pes             pshmem_team_n_pes
#define shmem_team_get_config        pshmem_team_get_config
#define shmem_team_translate_pe      pshmem_team_translate_pe
#define shmem_team_split_strided     pshmem_team_split_strided
#define shmem_team_split_2d          pshmem_team_split_2d
#define shmem_team_destroy           pshmem_team_destroy
#define shmem_ctx_get_team           pshmem_ctx_get_team
#define shmem_team_create_ctx        pshmem_team_create_ctx

/*
 * Teams-based Collectives
 */

/* Teams alltoall */
#define shmem_char_alltoall         	pshmem_char_alltoall
#define shmem_short_alltoall         	pshmem_short_alltoall
#define shmem_int_alltoall         	    pshmem_int_alltoall
#define shmem_long_alltoall         	pshmem_long_alltoall
#define shmem_float_alltoall         	pshmem_float_alltoall
#define shmem_double_alltoall         	pshmem_double_alltoall
#define shmem_longlong_alltoall         pshmem_longlong_alltoall
#define shmem_schar_alltoall         	pshmem_schar_alltoall
#define shmem_uchar_alltoall         	pshmem_uchar_alltoall
#define shmem_ushort_alltoall         	pshmem_ushort_alltoall
#define shmem_uint_alltoall         	pshmem_uint_alltoall
#define shmem_ulong_alltoall         	pshmem_ulong_alltoall
#define shmem_ulonglong_alltoall      	pshmem_ulonglong_alltoall
#define shmem_longdouble_alltoall       pshmem_longdouble_alltoall
#define shmem_int8_alltoall         	pshmem_int8_alltoall
#define shmem_int16_alltoall         	pshmem_int16_alltoall
#define shmem_int32_alltoall         	pshmem_int32_alltoall
#define shmem_int64_alltoall         	pshmem_int64_alltoall
#define shmem_uint8_alltoall         	pshmem_uint8_alltoall
#define shmem_uint16_alltoall         	pshmem_uint16_alltoall
#define shmem_uint32_alltoall         	pshmem_uint32_alltoall
#define shmem_uint64_alltoall         	pshmem_uint64_alltoall
#define shmem_size_alltoall         	pshmem_size_alltoall
#define shmem_ptrdiff_alltoall         pshmem_ptrdiff_alltoall

#define shmem_alltoallmem               pshmem_alltoallmem


/* Teams alltoalls */
#define shmem_char_alltoalls         	pshmem_char_alltoalls
#define shmem_short_alltoalls         	pshmem_short_alltoalls
#define shmem_int_alltoalls            pshmem_int_alltoalls
#define shmem_long_alltoalls         	pshmem_long_alltoalls
#define shmem_float_alltoalls         	pshmem_float_alltoalls
#define shmem_double_alltoalls         pshmem_double_alltoalls
#define shmem_longlong_alltoalls       pshmem_longlong_alltoalls
#define shmem_schar_alltoalls         	pshmem_schar_alltoalls
#define shmem_uchar_alltoalls         	pshmem_uchar_alltoalls
#define shmem_ushort_alltoalls         pshmem_ushort_alltoalls
#define shmem_uint_alltoalls         	pshmem_uint_alltoalls
#define shmem_ulong_alltoalls         	pshmem_ulong_alltoalls
#define shmem_ulonglong_alltoalls      pshmem_ulonglong_alltoalls
#define shmem_longdouble_alltoalls     pshmem_longdouble_alltoalls
#define shmem_int8_alltoalls         	pshmem_int8_alltoalls
#define shmem_int16_alltoalls         	pshmem_int16_alltoalls
#define shmem_int32_alltoalls         	pshmem_int32_alltoalls
#define shmem_int64_alltoalls         	pshmem_int64_alltoalls
#define shmem_uint8_alltoalls         	pshmem_uint8_alltoalls
#define shmem_uint16_alltoalls         pshmem_uint16_alltoalls
#define shmem_uint32_alltoalls         pshmem_uint32_alltoalls
#define shmem_uint64_alltoalls         pshmem_uint64_alltoalls
#define shmem_size_alltoalls         	pshmem_size_alltoalls
#define shmem_ptrdiff_alltoalls        pshmem_ptrdiff_alltoalls

#define shmem_alltoallsmem             pshmem_alltoallsmem


/* Teams broadcast */
#define shmem_char_broadcast         	pshmem_char_broadcast
#define shmem_short_broadcast         	pshmem_short_broadcast
#define shmem_int_broadcast        	pshmem_int_broadcast
#define shmem_long_broadcast         	pshmem_long_broadcast
#define shmem_float_broadcast         	pshmem_float_broadcast
#define shmem_double_broadcast         pshmem_double_broadcast
#define shmem_longlong_broadcast       pshmem_longlong_broadcast
#define shmem_schar_broadcast         	pshmem_schar_broadcast
#define shmem_uchar_broadcast         	pshmem_uchar_broadcast
#define shmem_ushort_broadcast         pshmem_ushort_broadcast
#define shmem_uint_broadcast         	pshmem_uint_broadcast
#define shmem_ulong_broadcast         	pshmem_ulong_broadcast
#define shmem_ulonglong_broadcast      pshmem_ulonglong_broadcast
#define shmem_longdouble_broadcast     pshmem_longdouble_broadcast
#define shmem_int8_broadcast         	pshmem_int8_broadcast
#define shmem_int16_broadcast         	pshmem_int16_broadcast
#define shmem_int32_broadcast         	pshmem_int32_broadcast
#define shmem_int64_broadcast         	pshmem_int64_broadcast
#define shmem_uint8_broadcast         	pshmem_uint8_broadcast
#define shmem_uint16_broadcast         pshmem_uint16_broadcast
#define shmem_uint32_broadcast         pshmem_uint32_broadcast
#define shmem_uint64_broadcast         pshmem_uint64_broadcast
#define shmem_size_broadcast         	pshmem_size_broadcast
#define shmem_ptrdiff_broadcast        pshmem_ptrdiff_broadcast

#define shmem_broadcastmem             pshmem_broadcastmem


/* Teams collect */
#define shmem_char_collect          	pshmem_char_collect
#define shmem_short_collect         	pshmem_short_collect
#define shmem_int_collect         	pshmem_int_collect
#define shmem_long_collect          	pshmem_long_collect
#define shmem_float_collect         	pshmem_float_collect
#define shmem_double_collect         	pshmem_double_collect
#define shmem_longlong_collect         pshmem_longlong_collect
#define shmem_schar_collect         	pshmem_schar_collect
#define shmem_uchar_collect         	pshmem_uchar_collect
#define shmem_ushort_collect         	pshmem_ushort_collect
#define shmem_uint_collect          	pshmem_uint_collect
#define shmem_ulong_collect         	pshmem_ulong_collect
#define shmem_ulonglong_collect      	pshmem_ulonglong_collect
#define shmem_longdouble_collect       pshmem_longdouble_collect
#define shmem_int8_collect         	pshmem_int8_collect
#define shmem_int16_collect         	pshmem_int16_collect
#define shmem_int32_collect         	pshmem_int32_collect
#define shmem_int64_collect         	pshmem_int64_collect
#define shmem_uint8_collect         	pshmem_uint8_collect
#define shmem_uint16_collect         	pshmem_uint16_collect
#define shmem_uint32_collect         	pshmem_uint32_collect
#define shmem_uint64_collect         	pshmem_uint64_collect
#define shmem_size_collect            	pshmem_size_collect
#define shmem_ptrdiff_collect         	pshmem_ptrdiff_collect

#define shmem_collectmem                pshmem_collectmem


/* Teams fcollect */
#define shmem_char_fcollect         	pshmem_char_fcollect
#define shmem_short_fcollect         	pshmem_short_fcollect
#define shmem_int_fcollect         	pshmem_int_fcollect
#define shmem_long_fcollect         	pshmem_long_fcollect
#define shmem_float_fcollect         	pshmem_float_fcollect
#define shmem_double_fcollect         	pshmem_double_fcollect
#define shmem_longlong_fcollect        pshmem_longlong_fcollect
#define shmem_schar_fcollect         	pshmem_schar_fcollect
#define shmem_uchar_fcollect         	pshmem_uchar_fcollect
#define shmem_ushort_fcollect         	pshmem_ushort_fcollect
#define shmem_uint_fcollect         	pshmem_uint_fcollect
#define shmem_ulong_fcollect         	pshmem_ulong_fcollect
#define shmem_ulonglong_fcollect      	pshmem_ulonglong_fcollect
#define shmem_longdouble_fcollect      pshmem_longdouble_fcollect
#define shmem_int8_fcollect         	pshmem_int8_fcollect
#define shmem_int16_fcollect         	pshmem_int16_fcollect
#define shmem_int32_fcollect         	pshmem_int32_fcollect
#define shmem_int64_fcollect         	pshmem_int64_fcollect
#define shmem_uint8_fcollect         	pshmem_uint8_fcollect
#define shmem_uint16_fcollect         	pshmem_uint16_fcollect
#define shmem_uint32_fcollect         	pshmem_uint32_fcollect
#define shmem_uint64_fcollect         	pshmem_uint64_fcollect
#define shmem_size_fcollect         	pshmem_size_fcollect
#define shmem_ptrdiff_fcollect         pshmem_ptrdiff_fcollect

#define shmem_fcollectmem               pshmem_fcollectmem


/* Teams reduction: AND */
#define shmem_uchar_and_reduce          pshmem_uchar_and_reduce
#define shmem_ushort_and_reduce         pshmem_ushort_and_reduce
#define shmem_uint_and_reduce           pshmem_uint_and_reduce
#define shmem_ulong_and_reduce          pshmem_ulong_and_reduce
#define shmem_ulonglong_and_reduce      pshmem_ulonglong_and_reduce
#define shmem_int_and_reduce            pshmem_int_and_reduce
#define shmem_longlong_and_reduce       pshmem_longlong_and_reduce
#define shmem_int8_and_reduce           pshmem_int8_and_reduce
#define shmem_int16_and_reduce          pshmem_int16_and_reduce
#define shmem_int32_and_reduce          pshmem_int32_and_reduce
#define shmem_int64_and_reduce          pshmem_int64_and_reduce
#define shmem_uint8_and_reduce          pshmem_uint8_and_reduce
#define shmem_uint16_and_reduce         pshmem_uint16_and_reduce
#define shmem_uint32_and_reduce         pshmem_uint32_and_reduce
#define shmem_uint64_and_reduce         pshmem_uint64_and_reduce
#define shmem_size_and_reduce           pshmem_size_and_reduce


/* Teams reduction: OR */
#define shmem_uchar_or_reduce          pshmem_uchar_or_reduce
#define shmem_ushort_or_reduce         pshmem_ushort_or_reduce
#define shmem_uint_or_reduce           pshmem_uint_or_reduce
#define shmem_ulong_or_reduce          pshmem_ulong_or_reduce
#define shmem_ulonglong_or_reduce      pshmem_ulonglong_or_reduce
#define shmem_int8_or_reduce           pshmem_int8_or_reduce
#define shmem_int16_or_reduce          pshmem_int16_or_reduce
#define shmem_int32_or_reduce          pshmem_int32_or_reduce
#define shmem_int64_or_reduce          pshmem_int64_or_reduce
#define shmem_uint8_or_reduce          pshmem_uint8_or_reduce
#define shmem_uint16_or_reduce         pshmem_uint16_or_reduce
#define shmem_uint32_or_reduce         pshmem_uint32_or_reduce
#define shmem_uint64_or_reduce         pshmem_uint64_or_reduce
#define shmem_size_or_reduce           pshmem_size_or_reduce


/* Teams reduction: XOR */
#define shmem_uchar_xor_reduce          pshmem_uchar_xor_reduce
#define shmem_ushort_xor_reduce         pshmem_ushort_xor_reduce
#define shmem_uint_xor_reduce           pshmem_uint_xor_reduce
#define shmem_ulong_xor_reduce          pshmem_ulong_xor_reduce
#define shmem_ulonglong_xor_reduce      pshmem_ulonglong_xor_reduce
#define shmem_int8_xor_reduce           pshmem_int8_xor_reduce
#define shmem_int16_xor_reduce          pshmem_int16_xor_reduce
#define shmem_int32_xor_reduce          pshmem_int32_xor_reduce
#define shmem_int64_xor_reduce          pshmem_int64_xor_reduce
#define shmem_uint8_xor_reduce          pshmem_uint8_xor_reduce
#define shmem_uint16_xor_reduce         pshmem_uint16_xor_reduce
#define shmem_uint32_xor_reduce         pshmem_uint32_xor_reduce
#define shmem_uint64_xor_reduce         pshmem_uint64_xor_reduce
#define shmem_size_xor_reduce           pshmem_size_xor_reduce


/* Teams reduction: MAX */
#define shmem_char_max_reduce           pshmem_char_max_reduce
#define shmem_short_max_reduce          pshmem_short_max_reduce
#define shmem_int_max_reduce            pshmem_int_max_reduce
#define shmem_long_max_reduce           pshmem_long_max_reduce
#define shmem_float_max_reduce          pshmem_float_max_reduce
#define shmem_double_max_reduce         pshmem_double_max_reduce
#define shmem_longlong_max_reduce       pshmem_longlong_max_reduce
#define shmem_schar_max_reduce          pshmem_schar_max_reduce
#define shmem_longdouble_max_reduce     pshmem_longdouble_max_reduce
#define shmem_ptrdiff_max_reduce        pshmem_ptrdiff_max_reduce
#define shmem_uchar_max_reduce          pshmem_uchar_max_reduce
#define shmem_ushort_max_reduce         pshmem_ushort_max_reduce
#define shmem_uint_max_reduce           pshmem_uint_max_reduce
#define shmem_ulong_max_reduce          pshmem_ulong_max_reduce
#define shmem_ulonglong_max_reduce      pshmem_ulonglong_max_reduce
#define shmem_int8_max_reduce           pshmem_int8_max_reduce
#define shmem_int16_max_reduce          pshmem_int16_max_reduce
#define shmem_int32_max_reduce          pshmem_int32_max_reduce
#define shmem_int64_max_reduce          pshmem_int64_max_reduce
#define shmem_uint8_max_reduce          pshmem_uint8_max_reduce
#define shmem_uint16_max_reduce         pshmem_uint16_max_reduce
#define shmem_uint32_max_reduce         pshmem_uint32_max_reduce
#define shmem_uint64_max_reduce         pshmem_uint64_max_reduce
#define shmem_size_max_reduce           pshmem_size_max_reduce


/* Teams reduction: MIN */
#define shmem_char_min_reduce           pshmem_char_min_reduce
#define shmem_short_min_reduce          pshmem_short_min_reduce
#define shmem_int_min_reduce            pshmem_int_min_reduce
#define shmem_long_min_reduce           pshmem_long_min_reduce
#define shmem_float_min_reduce          pshmem_float_min_reduce
#define shmem_double_min_reduce         pshmem_double_min_reduce
#define shmem_longlong_min_reduce       pshmem_longlong_min_reduce
#define shmem_schar_min_reduce          pshmem_schar_min_reduce
#define shmem_longdouble_min_reduce     pshmem_longdouble_min_reduce
#define shmem_ptrdiff_min_reduce        pshmem_ptrdiff_min_reduce
#define shmem_uchar_min_reduce          pshmem_uchar_min_reduce
#define shmem_ushort_min_reduce         pshmem_ushort_min_reduce
#define shmem_uint_min_reduce           pshmem_uint_min_reduce
#define shmem_ulong_min_reduce          pshmem_ulong_min_reduce
#define shmem_ulonglong_min_reduce      pshmem_ulonglong_min_reduce
#define shmem_int8_min_reduce           pshmem_int8_min_reduce
#define shmem_int16_min_reduce          pshmem_int16_min_reduce
#define shmem_int32_min_reduce          pshmem_int32_min_reduce
#define shmem_int64_min_reduce          pshmem_int64_min_reduce
#define shmem_uint8_min_reduce          pshmem_uint8_min_reduce
#define shmem_uint16_min_reduce         pshmem_uint16_min_reduce
#define shmem_uint32_min_reduce         pshmem_uint32_min_reduce
#define shmem_uint64_min_reduce         pshmem_uint64_min_reduce
#define shmem_size_min_reduce           pshmem_size_min_reduce


/* Teams reduction: SUM */
#define shmem_char_sum_reduce           pshmem_char_sum_reduce
#define shmem_short_sum_reduce          pshmem_short_sum_reduce
#define shmem_int_sum_reduce            pshmem_int_sum_reduce
#define shmem_long_sum_reduce           pshmem_long_sum_reduce
#define shmem_float_sum_reduce          pshmem_float_sum_reduce
#define shmem_double_sum_reduce         pshmem_double_sum_reduce
#define shmem_longlong_sum_reduce       pshmem_longlong_sum_reduce
#define shmem_schar_sum_reduce          pshmem_schar_sum_reduce
#define shmem_longdouble_sum_reduce     pshmem_longdouble_sum_reduce
#define shmem_ptrdiff_sum_reduce        pshmem_ptrdiff_sum_reduce
#define shmem_uchar_sum_reduce          pshmem_uchar_sum_reduce
#define shmem_ushort_sum_reduce         pshmem_ushort_sum_reduce
#define shmem_uint_sum_reduce           pshmem_uint_sum_reduce
#define shmem_ulong_sum_reduce          pshmem_ulong_sum_reduce
#define shmem_ulonglong_sum_reduce      pshmem_ulonglong_sum_reduce
#define shmem_int8_sum_reduce           pshmem_int8_sum_reduce
#define shmem_int16_sum_reduce          pshmem_int16_sum_reduce
#define shmem_int32_sum_reduce          pshmem_int32_sum_reduce
#define shmem_int64_sum_reduce          pshmem_int64_sum_reduce
#define shmem_uint8_sum_reduce          pshmem_uint8_sum_reduce
#define shmem_uint16_sum_reduce         pshmem_uint16_sum_reduce
#define shmem_uint32_sum_reduce         pshmem_uint32_sum_reduce
#define shmem_uint64_sum_reduce         pshmem_uint64_sum_reduce
#define shmem_size_sum_reduce           pshmem_size_sum_reduce
#define shmem_complexd_sum_reduce       pshmem_complexd_sum_reduce
#define shmem_complexf_sum_reduce       pshmem_complexf_sum_reduce


/* Teams reduction: PROD */
#define shmem_char_prod_reduce           pshmem_char_prod_reduce
#define shmem_short_prod_reduce          pshmem_short_prod_reduce
#define shmem_int_prod_reduce            pshmem_int_prod_reduce
#define shmem_long_prod_reduce           pshmem_long_prod_reduce
#define shmem_float_prod_reduce          pshmem_float_prod_reduce
#define shmem_double_prod_reduce         pshmem_double_prod_reduce
#define shmem_longlong_prod_reduce       pshmem_longlong_prod_reduce
#define shmem_schar_prod_reduce          pshmem_schar_prod_reduce
#define shmem_longdouble_prod_reduce     pshmem_longdouble_prod_reduce
#define shmem_ptrdiff_prod_reduce        pshmem_ptrdiff_prod_reduce
#define shmem_uchar_prod_reduce          pshmem_uchar_prod_reduce
#define shmem_ushort_prod_reduce         pshmem_ushort_prod_reduce
#define shmem_uint_prod_reduce           pshmem_uint_prod_reduce
#define shmem_ulong_prod_reduce          pshmem_ulong_prod_reduce
#define shmem_ulonglong_prod_reduce      pshmem_ulonglong_prod_reduce
#define shmem_int8_prod_reduce           pshmem_int8_prod_reduce
#define shmem_int16_prod_reduce          pshmem_int16_prod_reduce
#define shmem_int32_prod_reduce          pshmem_int32_prod_reduce
#define shmem_int64_prod_reduce          pshmem_int64_prod_reduce
#define shmem_uint8_prod_reduce          pshmem_uint8_prod_reduce
#define shmem_uint16_prod_reduce         pshmem_uint16_prod_reduce
#define shmem_uint32_prod_reduce         pshmem_uint32_prod_reduce
#define shmem_uint64_prod_reduce         pshmem_uint64_prod_reduce
#define shmem_size_prod_reduce           pshmem_size_prod_reduce
#define shmem_complexd_prod_reduce       pshmem_complexd_prod_reduce
#define shmem_complexf_prod_reduce       pshmem_complexf_prod_reduce

/*
 * Elemental put routines
 */
#define shmem_ctx_char_p             pshmem_ctx_char_p
#define shmem_ctx_short_p            pshmem_ctx_short_p
#define shmem_ctx_int_p              pshmem_ctx_int_p
#define shmem_ctx_long_p             pshmem_ctx_long_p
#define shmem_ctx_float_p            pshmem_ctx_float_p
#define shmem_ctx_double_p           pshmem_ctx_double_p
#define shmem_ctx_longlong_p         pshmem_ctx_longlong_p
#define shmem_ctx_schar_p            pshmem_ctx_schar_p
#define shmem_ctx_uchar_p            pshmem_ctx_uchar_p
#define shmem_ctx_ushort_p           pshmem_ctx_ushort_p
#define shmem_ctx_uint_p             pshmem_ctx_uint_p
#define shmem_ctx_ulong_p            pshmem_ctx_ulong_p
#define shmem_ctx_ulonglong_p        pshmem_ctx_ulonglong_p
#define shmem_ctx_longdouble_p       pshmem_ctx_longdouble_p
#define shmem_ctx_int8_p             pshmem_ctx_int8_p
#define shmem_ctx_int16_p            pshmem_ctx_int16_p
#define shmem_ctx_int32_p            pshmem_ctx_int32_p
#define shmem_ctx_int64_p            pshmem_ctx_int64_p
#define shmem_ctx_uint8_p            pshmem_ctx_uint8_p
#define shmem_ctx_uint16_p           pshmem_ctx_uint16_p
#define shmem_ctx_uint32_p           pshmem_ctx_uint32_p
#define shmem_ctx_uint64_p           pshmem_ctx_uint64_p
#define shmem_ctx_size_p             pshmem_ctx_size_p
#define shmem_ctx_ptrdiff_p          pshmem_ctx_ptrdiff_p

#define shmem_char_p                 pshmem_char_p
#define shmem_short_p                pshmem_short_p
#define shmem_int_p                  pshmem_int_p
#define shmem_long_p                 pshmem_long_p
#define shmem_float_p                pshmem_float_p
#define shmem_double_p               pshmem_double_p
#define shmem_longlong_p             pshmem_longlong_p
#define shmem_schar_p                pshmem_schar_p
#define shmem_uchar_p                pshmem_uchar_p
#define shmem_ushort_p               pshmem_ushort_p
#define shmem_uint_p                 pshmem_uint_p
#define shmem_ulong_p                pshmem_ulong_p
#define shmem_ulonglong_p            pshmem_ulonglong_p
#define shmem_longdouble_p           pshmem_longdouble_p
#define shmem_int8_p                 pshmem_int8_p
#define shmem_int16_p                pshmem_int16_p
#define shmem_int32_p                pshmem_int32_p
#define shmem_int64_p                pshmem_int64_p
#define shmem_uint8_p                pshmem_uint8_p
#define shmem_uint16_p               pshmem_uint16_p
#define shmem_uint32_p               pshmem_uint32_p
#define shmem_uint64_p               pshmem_uint64_p
#define shmem_size_p                 pshmem_size_p
#define shmem_ptrdiff_p              pshmem_ptrdiff_p

#define shmemx_int16_p               pshmemx_int16_p
#define shmemx_int32_p               pshmemx_int32_p
#define shmemx_int64_p               pshmemx_int64_p

/*
 * Signaled put routines
 */
#define shmem_ctx_char_put_signal             pshmem_ctx_char_put_signal
#define shmem_ctx_short_put_signal            pshmem_ctx_short_put_signal
#define shmem_ctx_int_put_signal              pshmem_ctx_int_put_signal
#define shmem_ctx_long_put_signal             pshmem_ctx_long_put_signal
#define shmem_ctx_float_put_signal            pshmem_ctx_float_put_signal
#define shmem_ctx_double_put_signal           pshmem_ctx_double_put_signal
#define shmem_ctx_longlong_put_signal         pshmem_ctx_longlong_put_signal
#define shmem_ctx_schar_put_signal            pshmem_ctx_schar_put_signal
#define shmem_ctx_uchar_put_signal            pshmem_ctx_uchar_put_signal
#define shmem_ctx_ushort_put_signal           pshmem_ctx_ushort_put_signal
#define shmem_ctx_uint_put_signal             pshmem_ctx_uint_put_signal
#define shmem_ctx_ulong_put_signal            pshmem_ctx_ulong_put_signal
#define shmem_ctx_ulonglong_put_signal        pshmem_ctx_ulonglong_put_signal
#define shmem_ctx_longdouble_put_signal       pshmem_ctx_longdouble_put_signal
#define shmem_ctx_int8_put_signal             pshmem_ctx_int8_put_signal
#define shmem_ctx_int16_put_signal            pshmem_ctx_int16_put_signal
#define shmem_ctx_int32_put_signal            pshmem_ctx_int32_put_signal
#define shmem_ctx_int64_put_signal            pshmem_ctx_int64_put_signal
#define shmem_ctx_uint8_put_signal            pshmem_ctx_uint8_put_signal
#define shmem_ctx_uint16_put_signal           pshmem_ctx_uint16_put_signal
#define shmem_ctx_uint32_put_signal           pshmem_ctx_uint32_put_signal
#define shmem_ctx_uint64_put_signal           pshmem_ctx_uint64_put_signal
#define shmem_ctx_size_put_signal             pshmem_ctx_size_put_signal
#define shmem_ctx_ptrdiff_put_signal          pshmem_ctx_ptrdiff_put_signal

#define shmem_char_put_signal                 pshmem_char_put_signal
#define shmem_short_put_signal                pshmem_short_put_signal
#define shmem_int_put_signal                  pshmem_int_put_signal
#define shmem_long_put_signal                 pshmem_long_put_signal
#define shmem_float_put_signal                pshmem_float_put_signal
#define shmem_double_put_signal               pshmem_double_put_signal
#define shmem_longlong_put_signal             pshmem_longlong_put_signal
#define shmem_schar_put_signal                pshmem_schar_put_signal
#define shmem_uchar_put_signal                pshmem_uchar_put_signal
#define shmem_ushort_put_signal               pshmem_ushort_put_signal
#define shmem_uint_put_signal                 pshmem_uint_put_signal
#define shmem_ulong_put_signal                pshmem_ulong_put_signal
#define shmem_ulonglong_put_signal            pshmem_ulonglong_put_signal
#define shmem_longdouble_put_signal           pshmem_longdouble_put_signal
#define shmem_int8_put_signal                 pshmem_int8_put_signal
#define shmem_int16_put_signal                pshmem_int16_put_signal
#define shmem_int32_put_signal                pshmem_int32_put_signal
#define shmem_int64_put_signal                pshmem_int64_put_signal
#define shmem_uint8_put_signal                pshmem_uint8_put_signal
#define shmem_uint16_put_signal               pshmem_uint16_put_signal
#define shmem_uint32_put_signal               pshmem_uint32_put_signal
#define shmem_uint64_put_signal               pshmem_uint64_put_signal
#define shmem_size_put_signal                 pshmem_size_put_signal
#define shmem_ptrdiff_put_signal              pshmem_ptrdiff_put_signal

#define shmem_put8_signal                     pshmem_put8_signal
#define shmem_put16_signal                    pshmem_put16_signal
#define shmem_put32_signal                    pshmem_put32_signal
#define shmem_put64_signal                    pshmem_put64_signal
#define shmem_put128_signal                   pshmem_put128_signal

#define shmem_ctx_put8_signal                 pshmem_ctx_put8_signal
#define shmem_ctx_put16_signal                pshmem_ctx_put16_signal
#define shmem_ctx_put32_signal                pshmem_ctx_put32_signal
#define shmem_ctx_put64_signal                pshmem_ctx_put64_signal
#define shmem_ctx_put128_signal               pshmem_ctx_put128_signal

#define shmem_putmem_signal                  pshmem_putmem_signal
#define shmem_ctx_putmem_signal              pshmem_ctx_putmem_signal

/*
 * Nonblocking signaled put routines
 */
#define shmem_ctx_char_put_signal_nbi             pshmem_ctx_char_put_signal_nbi
#define shmem_ctx_short_put_signal_nbi            pshmem_ctx_short_put_signal_nbi
#define shmem_ctx_int_put_signal_nbi              pshmem_ctx_int_put_signal_nbi
#define shmem_ctx_long_put_signal_nbi             pshmem_ctx_long_put_signal_nbi
#define shmem_ctx_float_put_signal_nbi            pshmem_ctx_float_put_signal_nbi
#define shmem_ctx_double_put_signal_nbi           pshmem_ctx_double_put_signal_nbi
#define shmem_ctx_longlong_put_signal_nbi         pshmem_ctx_longlong_put_signal_nbi
#define shmem_ctx_schar_put_signal_nbi            pshmem_ctx_schar_put_signal_nbi
#define shmem_ctx_uchar_put_signal_nbi            pshmem_ctx_uchar_put_signal_nbi
#define shmem_ctx_ushort_put_signal_nbi           pshmem_ctx_ushort_put_signal_nbi
#define shmem_ctx_uint_put_signal_nbi             pshmem_ctx_uint_put_signal_nbi
#define shmem_ctx_ulong_put_signal_nbi            pshmem_ctx_ulong_put_signal_nbi
#define shmem_ctx_ulonglong_put_signal_nbi        pshmem_ctx_ulonglong_put_signal_nbi
#define shmem_ctx_longdouble_put_signal_nbi       pshmem_ctx_longdouble_put_signal_nbi
#define shmem_ctx_int8_put_signal_nbi             pshmem_ctx_int8_put_signal_nbi
#define shmem_ctx_int16_put_signal_nbi            pshmem_ctx_int16_put_signal_nbi
#define shmem_ctx_int32_put_signal_nbi            pshmem_ctx_int32_put_signal_nbi
#define shmem_ctx_int64_put_signal_nbi            pshmem_ctx_int64_put_signal_nbi
#define shmem_ctx_uint8_put_signal_nbi            pshmem_ctx_uint8_put_signal_nbi
#define shmem_ctx_uint16_put_signal_nbi           pshmem_ctx_uint16_put_signal_nbi
#define shmem_ctx_uint32_put_signal_nbi           pshmem_ctx_uint32_put_signal_nbi
#define shmem_ctx_uint64_put_signal_nbi           pshmem_ctx_uint64_put_signal_nbi
#define shmem_ctx_size_put_signal_nbi             pshmem_ctx_size_put_signal_nbi
#define shmem_ctx_ptrdiff_put_signal_nbi          pshmem_ctx_ptrdiff_put_signal_nbi

#define shmem_char_put_signal_nbi                 pshmem_char_put_signal_nbi
#define shmem_short_put_signal_nbi                pshmem_short_put_signal_nbi
#define shmem_int_put_signal_nbi                  pshmem_int_put_signal_nbi
#define shmem_long_put_signal_nbi                 pshmem_long_put_signal_nbi
#define shmem_float_put_signal_nbi                pshmem_float_put_signal_nbi
#define shmem_double_put_signal_nbi               pshmem_double_put_signal_nbi
#define shmem_longlong_put_signal_nbi             pshmem_longlong_put_signal_nbi
#define shmem_schar_put_signal_nbi                pshmem_schar_put_signal_nbi
#define shmem_uchar_put_signal_nbi                pshmem_uchar_put_signal_nbi
#define shmem_ushort_put_signal_nbi               pshmem_ushort_put_signal_nbi
#define shmem_uint_put_signal_nbi                 pshmem_uint_put_signal_nbi
#define shmem_ulong_put_signal_nbi                pshmem_ulong_put_signal_nbi
#define shmem_ulonglong_put_signal_nbi            pshmem_ulonglong_put_signal_nbi
#define shmem_longdouble_put_signal_nbi           pshmem_longdouble_put_signal_nbi
#define shmem_int8_put_signal_nbi                 pshmem_int8_put_signal_nbi
#define shmem_int16_put_signal_nbi                pshmem_int16_put_signal_nbi
#define shmem_int32_put_signal_nbi                pshmem_int32_put_signal_nbi
#define shmem_int64_put_signal_nbi                pshmem_int64_put_signal_nbi
#define shmem_uint8_put_signal_nbi                pshmem_uint8_put_signal_nbi
#define shmem_uint16_put_signal_nbi               pshmem_uint16_put_signal_nbi
#define shmem_uint32_put_signal_nbi               pshmem_uint32_put_signal_nbi
#define shmem_uint64_put_signal_nbi               pshmem_uint64_put_signal_nbi
#define shmem_size_put_signal_nbi                 pshmem_size_put_signal_nbi
#define shmem_ptrdiff_put_signal_nbi              pshmem_ptrdiff_put_signal_nbi

#define shmem_put8_signal_nbi                     pshmem_put8_signal_nbi
#define shmem_put16_signal_nbi                    pshmem_put16_signal_nbi
#define shmem_put32_signal_nbi                    pshmem_put32_signal_nbi
#define shmem_put64_signal_nbi                    pshmem_put64_signal_nbi
#define shmem_put128_signal_nbi                   pshmem_put128_signal_nbi

#define shmem_ctx_put8_signal_nbi                 pshmem_ctx_put8_signal_nbi
#define shmem_ctx_put16_signal_nbi                pshmem_ctx_put16_signal_nbi
#define shmem_ctx_put32_signal_nbi                pshmem_ctx_put32_signal_nbi
#define shmem_ctx_put64_signal_nbi                pshmem_ctx_put64_signal_nbi
#define shmem_ctx_put128_signal_nbi               pshmem_ctx_put128_signal_nbi

#define shmem_putmem_signal_nbi                  pshmem_putmem_signal_nbi
#define shmem_ctx_putmem_signal_nbi              pshmem_ctx_putmem_signal_nbi

#define shmem_signal_fetch                       pshmem_signal_fetch

/*
 * Block data put routines
 */
#define shmem_ctx_char_put           pshmem_ctx_char_put
#define shmem_ctx_short_put          pshmem_ctx_short_put
#define shmem_ctx_int_put            pshmem_ctx_int_put
#define shmem_ctx_long_put           pshmem_ctx_long_put
#define shmem_ctx_float_put          pshmem_ctx_float_put
#define shmem_ctx_double_put         pshmem_ctx_double_put
#define shmem_ctx_longlong_put       pshmem_ctx_longlong_put
#define shmem_ctx_schar_put          pshmem_ctx_schar_put
#define shmem_ctx_uchar_put          pshmem_ctx_uchar_put
#define shmem_ctx_ushort_put         pshmem_ctx_ushort_put
#define shmem_ctx_uint_put           pshmem_ctx_uint_put
#define shmem_ctx_ulong_put          pshmem_ctx_ulong_put
#define shmem_ctx_ulonglong_put      pshmem_ctx_ulonglong_put
#define shmem_ctx_longdouble_put     pshmem_ctx_longdouble_put
#define shmem_ctx_int8_put           pshmem_ctx_int8_put
#define shmem_ctx_int16_put          pshmem_ctx_int16_put
#define shmem_ctx_int32_put          pshmem_ctx_int32_put
#define shmem_ctx_int64_put          pshmem_ctx_int64_put
#define shmem_ctx_uint8_put          pshmem_ctx_uint8_put
#define shmem_ctx_uint16_put         pshmem_ctx_uint16_put
#define shmem_ctx_uint32_put         pshmem_ctx_uint32_put
#define shmem_ctx_uint64_put         pshmem_ctx_uint64_put
#define shmem_ctx_size_put           pshmem_ctx_size_put
#define shmem_ctx_ptrdiff_put        pshmem_ctx_ptrdiff_put

#define shmem_char_put               pshmem_char_put /* shmem-compat.h */
#define shmem_short_put              pshmem_short_put
#define shmem_int_put                pshmem_int_put
#define shmem_long_put               pshmem_long_put
#define shmem_float_put              pshmem_float_put
#define shmem_double_put             pshmem_double_put
#define shmem_longlong_put           pshmem_longlong_put
#define shmem_schar_put              pshmem_schar_put
#define shmem_uchar_put              pshmem_uchar_put
#define shmem_ushort_put             pshmem_ushort_put
#define shmem_uint_put               pshmem_uint_put
#define shmem_ulong_put              pshmem_ulong_put
#define shmem_ulonglong_put          pshmem_ulonglong_put
#define shmem_longdouble_put         pshmem_longdouble_put
#define shmem_int8_put               pshmem_int8_put
#define shmem_int16_put              pshmem_int16_put
#define shmem_int32_put              pshmem_int32_put
#define shmem_int64_put              pshmem_int64_put
#define shmem_uint8_put              pshmem_uint8_put
#define shmem_uint16_put             pshmem_uint16_put
#define shmem_uint32_put             pshmem_uint32_put
#define shmem_uint64_put             pshmem_uint64_put
#define shmem_size_put               pshmem_size_put
#define shmem_ptrdiff_put            pshmem_ptrdiff_put

#define shmem_ctx_put8               pshmem_ctx_put8
#define shmem_ctx_put16              pshmem_ctx_put16
#define shmem_ctx_put32              pshmem_ctx_put32
#define shmem_ctx_put64              pshmem_ctx_put64
#define shmem_ctx_put128             pshmem_ctx_put128
#define shmem_ctx_putmem             pshmem_ctx_putmem

#define shmem_put8                   pshmem_put8
#define shmem_put16                  pshmem_put16
#define shmem_put32                  pshmem_put32
#define shmem_put64                  pshmem_put64
#define shmem_put128                 pshmem_put128
#define shmem_putmem                 pshmem_putmem


/*
 * Strided put routines
 */
#define shmem_ctx_char_iput           pshmem_ctx_char_iput
#define shmem_ctx_short_iput          pshmem_ctx_short_iput
#define shmem_ctx_int_iput            pshmem_ctx_int_iput
#define shmem_ctx_long_iput           pshmem_ctx_long_iput
#define shmem_ctx_float_iput          pshmem_ctx_float_iput
#define shmem_ctx_double_iput         pshmem_ctx_double_iput
#define shmem_ctx_longlong_iput       pshmem_ctx_longlong_iput
#define shmem_ctx_schar_iput          pshmem_ctx_schar_iput
#define shmem_ctx_uchar_iput          pshmem_ctx_uchar_iput
#define shmem_ctx_ushort_iput         pshmem_ctx_ushort_iput
#define shmem_ctx_uint_iput           pshmem_ctx_uint_iput
#define shmem_ctx_ulong_iput          pshmem_ctx_ulong_iput
#define shmem_ctx_ulonglong_iput      pshmem_ctx_ulonglong_iput
#define shmem_ctx_longdouble_iput     pshmem_ctx_longdouble_iput
#define shmem_ctx_int8_iput           pshmem_ctx_int8_iput
#define shmem_ctx_int16_iput          pshmem_ctx_int16_iput
#define shmem_ctx_int32_iput          pshmem_ctx_int32_iput
#define shmem_ctx_int64_iput          pshmem_ctx_int64_iput
#define shmem_ctx_uint8_iput          pshmem_ctx_uint8_iput
#define shmem_ctx_uint16_iput         pshmem_ctx_uint16_iput
#define shmem_ctx_uint32_iput         pshmem_ctx_uint32_iput
#define shmem_ctx_uint64_iput         pshmem_ctx_uint64_iput
#define shmem_ctx_size_iput           pshmem_ctx_size_iput
#define shmem_ctx_ptrdiff_iput        pshmem_ctx_ptrdiff_iput

#define shmem_char_iput               pshmem_char_iput
#define shmem_short_iput              pshmem_short_iput
#define shmem_int_iput                pshmem_int_iput
#define shmem_long_iput               pshmem_long_iput
#define shmem_float_iput              pshmem_float_iput
#define shmem_double_iput             pshmem_double_iput
#define shmem_longlong_iput           pshmem_longlong_iput
#define shmem_schar_iput              pshmem_schar_iput
#define shmem_uchar_iput              pshmem_uchar_iput
#define shmem_ushort_iput             pshmem_ushort_iput
#define shmem_uint_iput               pshmem_uint_iput
#define shmem_ulong_iput              pshmem_ulong_iput
#define shmem_ulonglong_iput          pshmem_ulonglong_iput
#define shmem_longdouble_iput         pshmem_longdouble_iput
#define shmem_int8_iput               pshmem_int8_iput
#define shmem_int16_iput              pshmem_int16_iput
#define shmem_int32_iput              pshmem_int32_iput
#define shmem_int64_iput              pshmem_int64_iput
#define shmem_uint8_iput              pshmem_uint8_iput
#define shmem_uint16_iput             pshmem_uint16_iput
#define shmem_uint32_iput             pshmem_uint32_iput
#define shmem_uint64_iput             pshmem_uint64_iput
#define shmem_size_iput               pshmem_size_iput
#define shmem_ptrdiff_iput            pshmem_ptrdiff_iput

#define shmem_ctx_iput8              pshmem_ctx_iput8
#define shmem_ctx_iput16             pshmem_ctx_iput16
#define shmem_ctx_iput32             pshmem_ctx_iput32
#define shmem_ctx_iput64             pshmem_ctx_iput64
#define shmem_ctx_iput128            pshmem_ctx_iput128

#define shmem_iput8                  pshmem_iput8
#define shmem_iput16                 pshmem_iput16
#define shmem_iput32                 pshmem_iput32
#define shmem_iput64                 pshmem_iput64
#define shmem_iput128                pshmem_iput128

/*
 * Non-block data put routines
 */
#define shmem_ctx_char_put_nbi           pshmem_ctx_char_put_nbi
#define shmem_ctx_short_put_nbi          pshmem_ctx_short_put_nbi
#define shmem_ctx_int_put_nbi            pshmem_ctx_int_put_nbi
#define shmem_ctx_long_put_nbi           pshmem_ctx_long_put_nbi
#define shmem_ctx_float_put_nbi          pshmem_ctx_float_put_nbi
#define shmem_ctx_double_put_nbi         pshmem_ctx_double_put_nbi
#define shmem_ctx_longlong_put_nbi       pshmem_ctx_longlong_put_nbi
#define shmem_ctx_schar_put_nbi          pshmem_ctx_schar_put_nbi
#define shmem_ctx_uchar_put_nbi          pshmem_ctx_uchar_put_nbi
#define shmem_ctx_ushort_put_nbi         pshmem_ctx_ushort_put_nbi
#define shmem_ctx_uint_put_nbi           pshmem_ctx_uint_put_nbi
#define shmem_ctx_ulong_put_nbi          pshmem_ctx_ulong_put_nbi
#define shmem_ctx_ulonglong_put_nbi      pshmem_ctx_ulonglong_put_nbi
#define shmem_ctx_longdouble_put_nbi     pshmem_ctx_longdouble_put_nbi
#define shmem_ctx_int8_put_nbi           pshmem_ctx_int8_put_nbi
#define shmem_ctx_int16_put_nbi          pshmem_ctx_int16_put_nbi
#define shmem_ctx_int32_put_nbi          pshmem_ctx_int32_put_nbi
#define shmem_ctx_int64_put_nbi          pshmem_ctx_int64_put_nbi
#define shmem_ctx_uint8_put_nbi          pshmem_ctx_uint8_put_nbi
#define shmem_ctx_uint16_put_nbi         pshmem_ctx_uint16_put_nbi
#define shmem_ctx_uint32_put_nbi         pshmem_ctx_uint32_put_nbi
#define shmem_ctx_uint64_put_nbi         pshmem_ctx_uint64_put_nbi
#define shmem_ctx_size_put_nbi           pshmem_ctx_size_put_nbi
#define shmem_ctx_ptrdiff_put_nbi        pshmem_ctx_ptrdiff_put_nbi

#define shmem_char_put_nbi               pshmem_char_put_nbi
#define shmem_short_put_nbi              pshmem_short_put_nbi
#define shmem_int_put_nbi                pshmem_int_put_nbi
#define shmem_long_put_nbi               pshmem_long_put_nbi
#define shmem_float_put_nbi              pshmem_float_put_nbi
#define shmem_double_put_nbi             pshmem_double_put_nbi
#define shmem_longlong_put_nbi           pshmem_longlong_put_nbi
#define shmem_schar_put_nbi              pshmem_schar_put_nbi
#define shmem_uchar_put_nbi              pshmem_uchar_put_nbi
#define shmem_ushort_put_nbi             pshmem_ushort_put_nbi
#define shmem_uint_put_nbi               pshmem_uint_put_nbi
#define shmem_ulong_put_nbi              pshmem_ulong_put_nbi
#define shmem_ulonglong_put_nbi          pshmem_ulonglong_put_nbi
#define shmem_longdouble_put_nbi         pshmem_longdouble_put_nbi
#define shmem_int8_put_nbi               pshmem_int8_put_nbi
#define shmem_int16_put_nbi              pshmem_int16_put_nbi
#define shmem_int32_put_nbi              pshmem_int32_put_nbi
#define shmem_int64_put_nbi              pshmem_int64_put_nbi
#define shmem_uint8_put_nbi              pshmem_uint8_put_nbi
#define shmem_uint16_put_nbi             pshmem_uint16_put_nbi
#define shmem_uint32_put_nbi             pshmem_uint32_put_nbi
#define shmem_uint64_put_nbi             pshmem_uint64_put_nbi
#define shmem_size_put_nbi               pshmem_size_put_nbi
#define shmem_ptrdiff_put_nbi            pshmem_ptrdiff_put_nbi

#define shmem_ctx_put8_nbi           pshmem_ctx_put8_nbi
#define shmem_ctx_put16_nbi          pshmem_ctx_put16_nbi
#define shmem_ctx_put32_nbi          pshmem_ctx_put32_nbi
#define shmem_ctx_put64_nbi          pshmem_ctx_put64_nbi
#define shmem_ctx_put128_nbi         pshmem_ctx_put128_nbi
#define shmem_ctx_putmem_nbi         pshmem_ctx_putmem_nbi

#define shmem_put8_nbi               pshmem_put8_nbi
#define shmem_put16_nbi              pshmem_put16_nbi
#define shmem_put32_nbi              pshmem_put32_nbi
#define shmem_put64_nbi              pshmem_put64_nbi
#define shmem_put128_nbi             pshmem_put128_nbi
#define shmem_putmem_nbi             pshmem_putmem_nbi

/*
 * Elemental get routines
 */
#define shmem_ctx_char_g             pshmem_ctx_char_g
#define shmem_ctx_short_g            pshmem_ctx_short_g
#define shmem_ctx_int_g              pshmem_ctx_int_g
#define shmem_ctx_long_g             pshmem_ctx_long_g
#define shmem_ctx_float_g            pshmem_ctx_float_g
#define shmem_ctx_double_g           pshmem_ctx_double_g
#define shmem_ctx_longlong_g         pshmem_ctx_longlong_g
#define shmem_ctx_schar_g            pshmem_ctx_schar_g
#define shmem_ctx_uchar_g            pshmem_ctx_uchar_g
#define shmem_ctx_ushort_g           pshmem_ctx_ushort_g
#define shmem_ctx_uint_g             pshmem_ctx_uint_g
#define shmem_ctx_ulong_g            pshmem_ctx_ulong_g
#define shmem_ctx_ulonglong_g        pshmem_ctx_ulonglong_g
#define shmem_ctx_longdouble_g       pshmem_ctx_longdouble_g
#define shmem_ctx_int8_g             pshmem_ctx_int8_g
#define shmem_ctx_int16_g            pshmem_ctx_int16_g
#define shmem_ctx_int32_g            pshmem_ctx_int32_g
#define shmem_ctx_int64_g            pshmem_ctx_int64_g
#define shmem_ctx_uint8_g            pshmem_ctx_uint8_g
#define shmem_ctx_uint16_g           pshmem_ctx_uint16_g
#define shmem_ctx_uint32_g           pshmem_ctx_uint32_g
#define shmem_ctx_uint64_g           pshmem_ctx_uint64_g
#define shmem_ctx_size_g             pshmem_ctx_size_g
#define shmem_ctx_ptrdiff_g          pshmem_ctx_ptrdiff_g

#define shmem_char_g                 pshmem_char_g
#define shmem_short_g                pshmem_short_g
#define shmem_int_g                  pshmem_int_g
#define shmem_long_g                 pshmem_long_g
#define shmem_float_g                pshmem_float_g
#define shmem_double_g               pshmem_double_g
#define shmem_longlong_g             pshmem_longlong_g
#define shmem_schar_g                pshmem_schar_g
#define shmem_uchar_g                pshmem_uchar_g
#define shmem_ushort_g               pshmem_ushort_g
#define shmem_uint_g                 pshmem_uint_g
#define shmem_ulong_g                pshmem_ulong_g
#define shmem_ulonglong_g            pshmem_ulonglong_g
#define shmem_longdouble_g           pshmem_longdouble_g
#define shmem_int8_g                 pshmem_int8_g
#define shmem_int16_g                pshmem_int16_g
#define shmem_int32_g                pshmem_int32_g
#define shmem_int64_g                pshmem_int64_g
#define shmem_uint8_g                pshmem_uint8_g
#define shmem_uint16_g               pshmem_uint16_g
#define shmem_uint32_g               pshmem_uint32_g
#define shmem_uint64_g               pshmem_uint64_g
#define shmem_size_g                 pshmem_size_g
#define shmem_ptrdiff_g              pshmem_ptrdiff_g

#define shmemx_int16_g               pshmemx_int16_g
#define shmemx_int32_g               pshmemx_int32_g
#define shmemx_int64_g               pshmemx_int64_g

/*
 * Block data get routines
 */
#define shmem_ctx_char_get           pshmem_ctx_char_get
#define shmem_ctx_short_get          pshmem_ctx_short_get
#define shmem_ctx_int_get            pshmem_ctx_int_get
#define shmem_ctx_long_get           pshmem_ctx_long_get
#define shmem_ctx_float_get          pshmem_ctx_float_get
#define shmem_ctx_double_get         pshmem_ctx_double_get
#define shmem_ctx_longlong_get       pshmem_ctx_longlong_get
#define shmem_ctx_schar_get          pshmem_ctx_schar_get
#define shmem_ctx_uchar_get          pshmem_ctx_uchar_get
#define shmem_ctx_ushort_get         pshmem_ctx_ushort_get
#define shmem_ctx_uint_get           pshmem_ctx_uint_get
#define shmem_ctx_ulong_get          pshmem_ctx_ulong_get
#define shmem_ctx_ulonglong_get      pshmem_ctx_ulonglong_get
#define shmem_ctx_longdouble_get     pshmem_ctx_longdouble_get
#define shmem_ctx_int8_get           pshmem_ctx_int8_get
#define shmem_ctx_int16_get          pshmem_ctx_int16_get
#define shmem_ctx_int32_get          pshmem_ctx_int32_get
#define shmem_ctx_int64_get          pshmem_ctx_int64_get
#define shmem_ctx_uint8_get          pshmem_ctx_uint8_get
#define shmem_ctx_uint16_get         pshmem_ctx_uint16_get
#define shmem_ctx_uint32_get         pshmem_ctx_uint32_get
#define shmem_ctx_uint64_get         pshmem_ctx_uint64_get
#define shmem_ctx_size_get           pshmem_ctx_size_get
#define shmem_ctx_ptrdiff_get        pshmem_ctx_ptrdiff_get

#define shmem_char_get               pshmem_char_get /* shmem-compat.h */
#define shmem_short_get              pshmem_short_get
#define shmem_int_get                pshmem_int_get
#define shmem_long_get               pshmem_long_get
#define shmem_float_get              pshmem_float_get
#define shmem_double_get             pshmem_double_get
#define shmem_longlong_get           pshmem_longlong_get
#define shmem_schar_get              pshmem_schar_get
#define shmem_uchar_get              pshmem_uchar_get
#define shmem_ushort_get             pshmem_ushort_get
#define shmem_uint_get               pshmem_uint_get
#define shmem_ulong_get              pshmem_ulong_get
#define shmem_ulonglong_get          pshmem_ulonglong_get
#define shmem_longdouble_get         pshmem_longdouble_get
#define shmem_int8_get               pshmem_int8_get
#define shmem_int16_get              pshmem_int16_get
#define shmem_int32_get              pshmem_int32_get
#define shmem_int64_get              pshmem_int64_get
#define shmem_uint8_get              pshmem_uint8_get
#define shmem_uint16_get             pshmem_uint16_get
#define shmem_uint32_get             pshmem_uint32_get
#define shmem_uint64_get             pshmem_uint64_get
#define shmem_size_get               pshmem_size_get
#define shmem_ptrdiff_get            pshmem_ptrdiff_get

#define shmem_ctx_get8               pshmem_ctx_get8
#define shmem_ctx_get16              pshmem_ctx_get16
#define shmem_ctx_get32              pshmem_ctx_get32
#define shmem_ctx_get64              pshmem_ctx_get64
#define shmem_ctx_get128             pshmem_ctx_get128
#define shmem_ctx_getmem             pshmem_ctx_getmem

#define shmem_get8                   pshmem_get8
#define shmem_get16                  pshmem_get16
#define shmem_get32                  pshmem_get32
#define shmem_get64                  pshmem_get64
#define shmem_get128                 pshmem_get128
#define shmem_getmem                 pshmem_getmem

/*
 * Strided get routines
 */
#define shmem_ctx_char_iget           pshmem_ctx_char_iget
#define shmem_ctx_short_iget          pshmem_ctx_short_iget
#define shmem_ctx_int_iget            pshmem_ctx_int_iget
#define shmem_ctx_long_iget           pshmem_ctx_long_iget
#define shmem_ctx_float_iget          pshmem_ctx_float_iget
#define shmem_ctx_double_iget         pshmem_ctx_double_iget
#define shmem_ctx_longlong_iget       pshmem_ctx_longlong_iget
#define shmem_ctx_schar_iget          pshmem_ctx_schar_iget
#define shmem_ctx_uchar_iget          pshmem_ctx_uchar_iget
#define shmem_ctx_ushort_iget         pshmem_ctx_ushort_iget
#define shmem_ctx_uint_iget           pshmem_ctx_uint_iget
#define shmem_ctx_ulong_iget          pshmem_ctx_ulong_iget
#define shmem_ctx_ulonglong_iget      pshmem_ctx_ulonglong_iget
#define shmem_ctx_longdouble_iget     pshmem_ctx_longdouble_iget
#define shmem_ctx_int8_iget           pshmem_ctx_int8_iget
#define shmem_ctx_int16_iget          pshmem_ctx_int16_iget
#define shmem_ctx_int32_iget          pshmem_ctx_int32_iget
#define shmem_ctx_int64_iget          pshmem_ctx_int64_iget
#define shmem_ctx_uint8_iget          pshmem_ctx_uint8_iget
#define shmem_ctx_uint16_iget         pshmem_ctx_uint16_iget
#define shmem_ctx_uint32_iget         pshmem_ctx_uint32_iget
#define shmem_ctx_uint64_iget         pshmem_ctx_uint64_iget
#define shmem_ctx_size_iget           pshmem_ctx_size_iget
#define shmem_ctx_ptrdiff_iget        pshmem_ctx_ptrdiff_iget

#define shmem_char_iget               pshmem_char_iget
#define shmem_short_iget              pshmem_short_iget
#define shmem_int_iget                pshmem_int_iget
#define shmem_long_iget               pshmem_long_iget
#define shmem_float_iget              pshmem_float_iget
#define shmem_double_iget             pshmem_double_iget
#define shmem_longlong_iget           pshmem_longlong_iget
#define shmem_schar_iget              pshmem_schar_iget
#define shmem_uchar_iget              pshmem_uchar_iget
#define shmem_ushort_iget             pshmem_ushort_iget
#define shmem_uint_iget               pshmem_uint_iget
#define shmem_ulong_iget              pshmem_ulong_iget
#define shmem_ulonglong_iget          pshmem_ulonglong_iget
#define shmem_longdouble_iget         pshmem_longdouble_iget
#define shmem_int8_iget               pshmem_int8_iget
#define shmem_int16_iget              pshmem_int16_iget
#define shmem_int32_iget              pshmem_int32_iget
#define shmem_int64_iget              pshmem_int64_iget
#define shmem_uint8_iget              pshmem_uint8_iget
#define shmem_uint16_iget             pshmem_uint16_iget
#define shmem_uint32_iget             pshmem_uint32_iget
#define shmem_uint64_iget             pshmem_uint64_iget
#define shmem_size_iget               pshmem_size_iget
#define shmem_ptrdiff_iget            pshmem_ptrdiff_iget

#define shmem_ctx_iget8              pshmem_ctx_iget8
#define shmem_ctx_iget16             pshmem_ctx_iget16
#define shmem_ctx_iget32             pshmem_ctx_iget32
#define shmem_ctx_iget64             pshmem_ctx_iget64
#define shmem_ctx_iget128            pshmem_ctx_iget128

#define shmem_iget8                  pshmem_iget8
#define shmem_iget16                 pshmem_iget16
#define shmem_iget32                 pshmem_iget32
#define shmem_iget64                 pshmem_iget64
#define shmem_iget128                pshmem_iget128

/*
 * Non-block data get routines
 */
#define shmem_ctx_char_get_nbi           pshmem_ctx_char_get_nbi
#define shmem_ctx_short_get_nbi          pshmem_ctx_short_get_nbi
#define shmem_ctx_int_get_nbi            pshmem_ctx_int_get_nbi
#define shmem_ctx_long_get_nbi           pshmem_ctx_long_get_nbi
#define shmem_ctx_float_get_nbi          pshmem_ctx_float_get_nbi
#define shmem_ctx_double_get_nbi         pshmem_ctx_double_get_nbi
#define shmem_ctx_longlong_get_nbi       pshmem_ctx_longlong_get_nbi
#define shmem_ctx_schar_get_nbi          pshmem_ctx_schar_get_nbi
#define shmem_ctx_uchar_get_nbi          pshmem_ctx_uchar_get_nbi
#define shmem_ctx_ushort_get_nbi         pshmem_ctx_ushort_get_nbi
#define shmem_ctx_uint_get_nbi           pshmem_ctx_uint_get_nbi
#define shmem_ctx_ulong_get_nbi          pshmem_ctx_ulong_get_nbi
#define shmem_ctx_ulonglong_get_nbi      pshmem_ctx_ulonglong_get_nbi
#define shmem_ctx_longdouble_get_nbi     pshmem_ctx_longdouble_get_nbi
#define shmem_ctx_int8_get_nbi           pshmem_ctx_int8_get_nbi
#define shmem_ctx_int16_get_nbi          pshmem_ctx_int16_get_nbi
#define shmem_ctx_int32_get_nbi          pshmem_ctx_int32_get_nbi
#define shmem_ctx_int64_get_nbi          pshmem_ctx_int64_get_nbi
#define shmem_ctx_uint8_get_nbi          pshmem_ctx_uint8_get_nbi
#define shmem_ctx_uint16_get_nbi         pshmem_ctx_uint16_get_nbi
#define shmem_ctx_uint32_get_nbi         pshmem_ctx_uint32_get_nbi
#define shmem_ctx_uint64_get_nbi         pshmem_ctx_uint64_get_nbi
#define shmem_ctx_size_get_nbi           pshmem_ctx_size_get_nbi
#define shmem_ctx_ptrdiff_get_nbi        pshmem_ctx_ptrdiff_get_nbi

#define shmem_char_get_nbi               pshmem_char_get_nbi
#define shmem_short_get_nbi              pshmem_short_get_nbi
#define shmem_int_get_nbi                pshmem_int_get_nbi
#define shmem_long_get_nbi               pshmem_long_get_nbi
#define shmem_float_get_nbi              pshmem_float_get_nbi
#define shmem_double_get_nbi             pshmem_double_get_nbi
#define shmem_longlong_get_nbi           pshmem_longlong_get_nbi
#define shmem_schar_get_nbi              pshmem_schar_get_nbi
#define shmem_uchar_get_nbi              pshmem_uchar_get_nbi
#define shmem_ushort_get_nbi             pshmem_ushort_get_nbi
#define shmem_uint_get_nbi               pshmem_uint_get_nbi
#define shmem_ulong_get_nbi              pshmem_ulong_get_nbi
#define shmem_ulonglong_get_nbi          pshmem_ulonglong_get_nbi
#define shmem_longdouble_get_nbi         pshmem_longdouble_get_nbi
#define shmem_int8_get_nbi               pshmem_int8_get_nbi
#define shmem_int16_get_nbi              pshmem_int16_get_nbi
#define shmem_int32_get_nbi              pshmem_int32_get_nbi
#define shmem_int64_get_nbi              pshmem_int64_get_nbi
#define shmem_uint8_get_nbi              pshmem_uint8_get_nbi
#define shmem_uint16_get_nbi             pshmem_uint16_get_nbi
#define shmem_uint32_get_nbi             pshmem_uint32_get_nbi
#define shmem_uint64_get_nbi             pshmem_uint64_get_nbi
#define shmem_size_get_nbi               pshmem_size_get_nbi
#define shmem_ptrdiff_get_nbi            pshmem_ptrdiff_get_nbi

#define shmem_ctx_get8_nbi           pshmem_ctx_get8_nbi
#define shmem_ctx_get16_nbi          pshmem_ctx_get16_nbi
#define shmem_ctx_get32_nbi          pshmem_ctx_get32_nbi
#define shmem_ctx_get64_nbi          pshmem_ctx_get64_nbi
#define shmem_ctx_get128_nbi         pshmem_ctx_get128_nbi
#define shmem_ctx_getmem_nbi         pshmem_ctx_getmem_nbi

#define shmem_get8_nbi               pshmem_get8_nbi
#define shmem_get16_nbi              pshmem_get16_nbi
#define shmem_get32_nbi              pshmem_get32_nbi
#define shmem_get64_nbi              pshmem_get64_nbi
#define shmem_get128_nbi             pshmem_get128_nbi
#define shmem_getmem_nbi             pshmem_getmem_nbi

/*
 * Atomic operations
 */
/* Atomic swap */
#define shmem_ctx_double_atomic_swap pshmem_ctx_double_atomic_swap
#define shmem_ctx_float_atomic_swap  pshmem_ctx_float_atomic_swap
#define shmem_ctx_int_atomic_swap    pshmem_ctx_int_atomic_swap
#define shmem_ctx_long_atomic_swap   pshmem_ctx_long_atomic_swap
#define shmem_ctx_longlong_atomic_swap pshmem_ctx_longlong_atomic_swap
#define shmem_ctx_uint_atomic_swap   pshmem_ctx_uint_atomic_swap
#define shmem_ctx_ulong_atomic_swap  pshmem_ctx_ulong_atomic_swap
#define shmem_ctx_ulonglong_atomic_swap pshmem_ctx_ulonglong_atomic_swap
#define shmem_ctx_int32_atomic_swap    pshmem_ctx_int32_atomic_swap
#define shmem_ctx_int64_atomic_swap    pshmem_ctx_int64_atomic_swap
#define shmem_ctx_uint32_atomic_swap    pshmem_ctx_uint32_atomic_swap
#define shmem_ctx_uint64_atomic_swap    pshmem_ctx_uint64_atomic_swap
#define shmem_ctx_size_atomic_swap    pshmem_ctx_size_atomic_swap
#define shmem_ctx_ptrdiff_atomic_swap    pshmem_ctx_ptrdiff_atomic_swap

#define shmem_double_atomic_swap     pshmem_double_atomic_swap
#define shmem_float_atomic_swap      pshmem_float_atomic_swap
#define shmem_int_atomic_swap        pshmem_int_atomic_swap
#define shmem_long_atomic_swap       pshmem_long_atomic_swap
#define shmem_longlong_atomic_swap   pshmem_longlong_atomic_swap
#define shmem_uint_atomic_swap       pshmem_uint_atomic_swap
#define shmem_ulong_atomic_swap      pshmem_ulong_atomic_swap
#define shmem_ulonglong_atomic_swap  pshmem_ulonglong_atomic_swap
#define shmem_int32_atomic_swap    pshmem_int32_atomic_swap
#define shmem_int64_atomic_swap    pshmem_int64_atomic_swap
#define shmem_uint32_atomic_swap    pshmem_uint32_atomic_swap
#define shmem_uint64_atomic_swap    pshmem_uint64_atomic_swap
#define shmem_size_atomic_swap    pshmem_size_atomic_swap
#define shmem_ptrdiff_atomic_swap    pshmem_ptrdiff_atomic_swap

#define shmem_double_swap            pshmem_double_swap
#define shmem_float_swap             pshmem_float_swap
#define shmem_int_swap               pshmem_int_swap
#define shmem_long_swap              pshmem_long_swap
#define shmem_longlong_swap          pshmem_longlong_swap

#define shmemx_int32_swap            pshmemx_int32_swap
#define shmemx_int64_swap            pshmemx_int64_swap

/* Atomic set */
#define shmem_ctx_double_atomic_set pshmem_ctx_double_atomic_set
#define shmem_ctx_float_atomic_set  pshmem_ctx_float_atomic_set
#define shmem_ctx_int_atomic_set    pshmem_ctx_int_atomic_set
#define shmem_ctx_long_atomic_set   pshmem_ctx_long_atomic_set
#define shmem_ctx_longlong_atomic_set pshmem_ctx_longlong_atomic_set
#define shmem_ctx_uint_atomic_set   pshmem_ctx_uint_atomic_set
#define shmem_ctx_ulong_atomic_set  pshmem_ctx_ulong_atomic_set
#define shmem_ctx_ulonglong_atomic_set pshmem_ctx_ulonglong_atomic_set
#define shmem_ctx_int32_atomic_set  pshmem_ctx_int32_atomic_set
#define shmem_ctx_int64_atomic_set  pshmem_ctx_int64_atomic_set
#define shmem_ctx_uint32_atomic_set  pshmem_ctx_uint32_atomic_set
#define shmem_ctx_uint64_atomic_set  pshmem_ctx_uint64_atomic_set
#define shmem_ctx_size_atomic_set  pshmem_ctx_size_atomic_set
#define shmem_ctx_ptrdiff_atomic_set  pshmem_ctx_ptrdiff_atomic_set

#define shmem_double_atomic_set     pshmem_double_atomic_set
#define shmem_float_atomic_set      pshmem_float_atomic_set
#define shmem_int_atomic_set        pshmem_int_atomic_set
#define shmem_long_atomic_set       pshmem_long_atomic_set
#define shmem_longlong_atomic_set   pshmem_longlong_atomic_set
#define shmem_uint_atomic_set       pshmem_uint_atomic_set
#define shmem_ulong_atomic_set      pshmem_ulong_atomic_set
#define shmem_ulonglong_atomic_set  pshmem_ulonglong_atomic_set
#define shmem_int32_atomic_set      pshmem_int32_atomic_set
#define shmem_int64_atomic_set      pshmem_int64_atomic_set
#define shmem_uint32_atomic_set     pshmem_uint32_atomic_set
#define shmem_uint64_atomic_set     pshmem_uint64_atomic_set
#define shmem_size_atomic_set       pshmem_size_atomic_set
#define shmem_ptrdiff_atomic_set    pshmem_ptrdiff_atomic_set

#define shmem_double_set            pshmem_double_set
#define shmem_float_set             pshmem_float_set
#define shmem_int_set               pshmem_int_set
#define shmem_long_set              pshmem_long_set
#define shmem_longlong_set          pshmem_longlong_set

#define shmemx_int32_set            pshmemx_int32_set
#define shmemx_int64_set            pshmemx_int64_set

/* Atomic conditional swap */
#define shmem_ctx_int_atomic_compare_swap   pshmem_ctx_int_atomic_compare_swap
#define shmem_ctx_long_atomic_compare_swap  pshmem_ctx_long_atomic_compare_swap
#define shmem_ctx_longlong_atomic_compare_swap pshmem_ctx_longlong_atomic_compare_swap
#define shmem_ctx_uint_atomic_compare_swap  pshmem_ctx_uint_atomic_compare_swap
#define shmem_ctx_ulong_atomic_compare_swap pshmem_ctx_ulong_atomic_compare_swap
#define shmem_ctx_ulonglong_atomic_compare_swap pshmem_ctx_ulonglong_atomic_compare_swap
#define shmem_ctx_int32_atomic_compare_swap   pshmem_ctx_int32_atomic_compare_swap
#define shmem_ctx_int64_atomic_compare_swap   pshmem_ctx_int64_atomic_compare_swap
#define shmem_ctx_uint32_atomic_compare_swap   pshmem_ctx_uint32_atomic_compare_swap
#define shmem_ctx_uint64_atomic_compare_swap   pshmem_ctx_uint64_atomic_compare_swap
#define shmem_ctx_size_atomic_compare_swap   pshmem_ctx_size_atomic_compare_swap
#define shmem_ctx_ptrdiff_atomic_compare_swap   pshmem_ctx_ptrdiff_atomic_compare_swap

#define shmem_int_atomic_compare_swap       pshmem_int_atomic_compare_swap
#define shmem_long_atomic_compare_swap      pshmem_long_atomic_compare_swap
#define shmem_longlong_atomic_compare_swap  pshmem_longlong_atomic_compare_swap
#define shmem_uint_atomic_compare_swap      pshmem_uint_atomic_compare_swap
#define shmem_ulong_atomic_compare_swap     pshmem_ulong_atomic_compare_swap
#define shmem_ulonglong_atomic_compare_swap pshmem_ulonglong_atomic_compare_swap
#define shmem_int32_atomic_compare_swap   pshmem_int32_atomic_compare_swap
#define shmem_int64_atomic_compare_swap   pshmem_int64_atomic_compare_swap
#define shmem_uint32_atomic_compare_swap   pshmem_uint32_atomic_compare_swap
#define shmem_uint64_atomic_compare_swap   pshmem_uint64_atomic_compare_swap
#define shmem_size_atomic_compare_swap   pshmem_size_atomic_compare_swap
#define shmem_ptrdiff_atomic_compare_swap   pshmem_ptrdiff_atomic_compare_swap

#define shmem_int_cswap              pshmem_int_cswap
#define shmem_long_cswap             pshmem_long_cswap
#define shmem_longlong_cswap         pshmem_longlong_cswap

#define shmemx_int32_cswap           pshmemx_int32_cswap
#define shmemx_int64_cswap           pshmemx_int64_cswap

/* Atomic Fetch&Add */
#define shmem_ctx_int_atomic_fetch_add       pshmem_ctx_int_atomic_fetch_add
#define shmem_ctx_long_atomic_fetch_add      pshmem_ctx_long_atomic_fetch_add
#define shmem_ctx_longlong_atomic_fetch_add  pshmem_ctx_longlong_atomic_fetch_add
#define shmem_ctx_uint_atomic_fetch_add      pshmem_ctx_uint_atomic_fetch_add
#define shmem_ctx_ulong_atomic_fetch_add     pshmem_ctx_ulong_atomic_fetch_add
#define shmem_ctx_ulonglong_atomic_fetch_add pshmem_ctx_ulonglong_atomic_fetch_add
#define shmem_ctx_int32_atomic_fetch_add   pshmem_ctx_int32_atomic_fetch_add
#define shmem_ctx_int64_atomic_fetch_add   pshmem_ctx_int64_atomic_fetch_add
#define shmem_ctx_uint32_atomic_fetch_add   pshmem_ctx_uint32_atomic_fetch_add
#define shmem_ctx_uint64_atomic_fetch_add   pshmem_ctx_uint64_atomic_fetch_add
#define shmem_ctx_size_atomic_fetch_add   pshmem_ctx_size_atomic_fetch_add
#define shmem_ctx_ptrdiff_atomic_fetch_add   pshmem_ctx_ptrdiff_atomic_fetch_add

#define shmem_int_atomic_fetch_add           pshmem_int_atomic_fetch_add
#define shmem_long_atomic_fetch_add          pshmem_long_atomic_fetch_add
#define shmem_longlong_atomic_fetch_add      pshmem_longlong_atomic_fetch_add
#define shmem_uint_atomic_fetch_add          pshmem_uint_atomic_fetch_add
#define shmem_ulong_atomic_fetch_add         pshmem_ulong_atomic_fetch_add
#define shmem_ulonglong_atomic_fetch_add     pshmem_ulonglong_atomic_fetch_add
#define shmem_int32_atomic_fetch_add   pshmem_int32_atomic_fetch_add
#define shmem_int64_atomic_fetch_add   pshmem_int64_atomic_fetch_add
#define shmem_uint32_atomic_fetch_add   pshmem_uint32_atomic_fetch_add
#define shmem_uint64_atomic_fetch_add   pshmem_uint64_atomic_fetch_add
#define shmem_size_atomic_fetch_add   pshmem_size_atomic_fetch_add
#define shmem_ptrdiff_atomic_fetch_add   pshmem_ptrdiff_atomic_fetch_add

#define shmem_int_fadd                       pshmem_int_fadd
#define shmem_long_fadd                      pshmem_long_fadd
#define shmem_longlong_fadd                  pshmem_longlong_fadd

#define shmemx_int32_fadd                    pshmemx_int32_fadd
#define shmemx_int64_fadd                    pshmemx_int64_fadd

/* Atomic Fetch&And */
#define shmem_int_atomic_fetch_and        pshmem_int_atomic_fetch_and
#define shmem_long_atomic_fetch_and       pshmem_long_atomic_fetch_and
#define shmem_longlong_atomic_fetch_and   pshmem_longlong_atomic_fetch_and
#define shmem_uint_atomic_fetch_and       pshmem_uint_atomic_fetch_and
#define shmem_ulong_atomic_fetch_and      pshmem_ulong_atomic_fetch_and
#define shmem_ulonglong_atomic_fetch_and  pshmem_ulonglong_atomic_fetch_and
#define shmem_int32_atomic_fetch_and      pshmem_int32_atomic_fetch_and
#define shmem_int64_atomic_fetch_and      pshmem_int64_atomic_fetch_and
#define shmem_uint32_atomic_fetch_and     pshmem_uint32_atomic_fetch_and
#define shmem_uint64_atomic_fetch_and     pshmem_uint64_atomic_fetch_and

#define shmem_ctx_int_atomic_fetch_and    pshmem_ctx_int_atomic_fetch_and
#define shmem_ctx_long_atomic_fetch_and   pshmem_ctx_long_atomic_fetch_and
#define shmem_ctx_longlong_atomic_fetch_and pshmem_ctx_longlong_atomic_fetch_and
#define shmem_ctx_uint_atomic_fetch_and   pshmem_ctx_uint_atomic_fetch_and
#define shmem_ctx_ulong_atomic_fetch_and  pshmem_ctx_ulong_atomic_fetch_and
#define shmem_ctx_ulonglong_atomic_fetch_and pshmem_ctx_ulonglong_atomic_fetch_and
#define shmem_ctx_int32_atomic_fetch_and  pshmem_ctx_int32_atomic_fetch_and
#define shmem_ctx_int64_atomic_fetch_and  pshmem_ctx_int64_atomic_fetch_and
#define shmem_ctx_uint32_atomic_fetch_and pshmem_ctx_uint32_atomic_fetch_and
#define shmem_ctx_uint64_atomic_fetch_and pshmem_ctx_uint64_atomic_fetch_and

#define shmemx_int32_atomic_fetch_and     pshmemx_int32_atomic_fetch_and
#define shmemx_int64_atomic_fetch_and     pshmemx_int64_atomic_fetch_and
#define shmemx_uint32_atomic_fetch_and    pshmemx_uint32_atomic_fetch_and
#define shmemx_uint64_atomic_fetch_and    pshmemx_uint64_atomic_fetch_and

/* Atomic Fetch&Or */
#define shmem_int_atomic_fetch_or         pshmem_int_atomic_fetch_or
#define shmem_long_atomic_fetch_or        pshmem_long_atomic_fetch_or
#define shmem_longlong_atomic_fetch_or    pshmem_longlong_atomic_fetch_or
#define shmem_uint_atomic_fetch_or        pshmem_uint_atomic_fetch_or
#define shmem_ulong_atomic_fetch_or       pshmem_ulong_atomic_fetch_or
#define shmem_ulonglong_atomic_fetch_or   pshmem_ulonglong_atomic_fetch_or
#define shmem_int32_atomic_fetch_or       pshmem_int32_atomic_fetch_or
#define shmem_int64_atomic_fetch_or       pshmem_int64_atomic_fetch_or
#define shmem_uint32_atomic_fetch_or      pshmem_uint32_atomic_fetch_or
#define shmem_uint64_atomic_fetch_or      pshmem_uint64_atomic_fetch_or

#define shmem_ctx_int_atomic_fetch_or     pshmem_ctx_int_atomic_fetch_or
#define shmem_ctx_long_atomic_fetch_or    pshmem_ctx_long_atomic_fetch_or
#define shmem_ctx_longlong_atomic_fetch_or pshmem_ctx_longlong_atomic_fetch_or
#define shmem_ctx_uint_atomic_fetch_or    pshmem_ctx_uint_atomic_fetch_or
#define shmem_ctx_ulong_atomic_fetch_or   pshmem_ctx_ulong_atomic_fetch_or
#define shmem_ctx_ulonglong_atomic_fetch_or pshmem_ctx_ulonglong_atomic_fetch_or
#define shmem_ctx_int32_atomic_fetch_or   pshmem_ctx_int32_atomic_fetch_or
#define shmem_ctx_int64_atomic_fetch_or   pshmem_ctx_int64_atomic_fetch_or
#define shmem_ctx_uint32_atomic_fetch_or  pshmem_ctx_uint32_atomic_fetch_or
#define shmem_ctx_uint64_atomic_fetch_or  pshmem_ctx_uint64_atomic_fetch_or

#define shmemx_int32_atomic_fetch_or      pshmemx_int32_atomic_fetch_or
#define shmemx_int64_atomic_fetch_or      pshmemx_int64_atomic_fetch_or
#define shmemx_uint32_atomic_fetch_or     pshmemx_uint32_atomic_fetch_or
#define shmemx_uint64_atomic_fetch_or     pshmemx_uint64_atomic_fetch_or

/* Atomic Fetch&Xor */
#define shmem_int_atomic_fetch_xor        pshmem_int_atomic_fetch_xor
#define shmem_long_atomic_fetch_xor       pshmem_long_atomic_fetch_xor
#define shmem_longlong_atomic_fetch_xor   pshmem_longlong_atomic_fetch_xor
#define shmem_uint_atomic_fetch_xor       pshmem_uint_atomic_fetch_xor
#define shmem_ulong_atomic_fetch_xor      pshmem_ulong_atomic_fetch_xor
#define shmem_ulonglong_atomic_fetch_xor  pshmem_ulonglong_atomic_fetch_xor
#define shmem_int32_atomic_fetch_xor      pshmem_int32_atomic_fetch_xor
#define shmem_int64_atomic_fetch_xor      pshmem_int64_atomic_fetch_xor
#define shmem_uint32_atomic_fetch_xor     pshmem_uint32_atomic_fetch_xor
#define shmem_uint64_atomic_fetch_xor     pshmem_uint64_atomic_fetch_xor

#define shmem_ctx_int_atomic_fetch_xor    pshmem_ctx_int_atomic_fetch_xor
#define shmem_ctx_long_atomic_fetch_xor   pshmem_ctx_long_atomic_fetch_xor
#define shmem_ctx_longlong_atomic_fetch_xor pshmem_ctx_longlong_atomic_fetch_xor
#define shmem_ctx_uint_atomic_fetch_xor   pshmem_ctx_uint_atomic_fetch_xor
#define shmem_ctx_ulong_atomic_fetch_xor  pshmem_ctx_ulong_atomic_fetch_xor
#define shmem_ctx_ulonglong_atomic_fetch_xor pshmem_ctx_ulonglong_atomic_fetch_xor
#define shmem_ctx_int32_atomic_fetch_xor  pshmem_ctx_int32_atomic_fetch_xor
#define shmem_ctx_int64_atomic_fetch_xor  pshmem_ctx_int64_atomic_fetch_xor
#define shmem_ctx_uint32_atomic_fetch_xor pshmem_ctx_uint32_atomic_fetch_xor
#define shmem_ctx_uint64_atomic_fetch_xor pshmem_ctx_uint64_atomic_fetch_xor

#define shmemx_int32_atomic_fetch_xor     pshmemx_int32_atomic_fetch_xor
#define shmemx_int64_atomic_fetch_xor     pshmemx_int64_atomic_fetch_xor
#define shmemx_uint32_atomic_fetch_xor    pshmemx_uint32_atomic_fetch_xor
#define shmemx_uint64_atomic_fetch_xor    pshmemx_uint64_atomic_fetch_xor

/* Atomic Fetch */
#define shmem_ctx_double_atomic_fetch pshmem_ctx_double_atomic_fetch
#define shmem_ctx_float_atomic_fetch  pshmem_ctx_float_atomic_fetch
#define shmem_ctx_int_atomic_fetch    pshmem_ctx_int_atomic_fetch
#define shmem_ctx_long_atomic_fetch   pshmem_ctx_long_atomic_fetch
#define shmem_ctx_longlong_atomic_fetch pshmem_ctx_longlong_atomic_fetch
#define shmem_ctx_uint_atomic_fetch   pshmem_ctx_uint_atomic_fetch
#define shmem_ctx_ulong_atomic_fetch  pshmem_ctx_ulong_atomic_fetch
#define shmem_ctx_ulonglong_atomic_fetch pshmem_ctx_ulonglong_atomic_fetch
#define shmem_ctx_int32_atomic_fetch    pshmem_ctx_int32_atomic_fetch
#define shmem_ctx_int64_atomic_fetch    pshmem_ctx_int64_atomic_fetch
#define shmem_ctx_uint32_atomic_fetch    pshmem_ctx_uint32_atomic_fetch
#define shmem_ctx_uint64_atomic_fetch    pshmem_ctx_uint64_atomic_fetch
#define shmem_ctx_size_atomic_fetch    pshmem_ctx_size_atomic_fetch
#define shmem_ctx_ptrdiff_atomic_fetch    pshmem_ctx_ptrdiff_atomic_fetch

#define shmem_double_atomic_fetch     pshmem_double_atomic_fetch
#define shmem_float_atomic_fetch      pshmem_float_atomic_fetch
#define shmem_int_atomic_fetch        pshmem_int_atomic_fetch
#define shmem_long_atomic_fetch       pshmem_long_atomic_fetch
#define shmem_longlong_atomic_fetch   pshmem_longlong_atomic_fetch
#define shmem_uint_atomic_fetch       pshmem_uint_atomic_fetch
#define shmem_ulong_atomic_fetch      pshmem_ulong_atomic_fetch
#define shmem_ulonglong_atomic_fetch  pshmem_ulonglong_atomic_fetch
#define shmem_int32_atomic_fetch    pshmem_int32_atomic_fetch
#define shmem_int64_atomic_fetch    pshmem_int64_atomic_fetch
#define shmem_uint32_atomic_fetch    pshmem_uint32_atomic_fetch
#define shmem_uint64_atomic_fetch    pshmem_uint64_atomic_fetch
#define shmem_size_atomic_fetch    pshmem_size_atomic_fetch
#define shmem_ptrdiff_atomic_fetch    pshmem_ptrdiff_atomic_fetch

#define shmem_double_fetch            pshmem_double_fetch
#define shmem_float_fetch             pshmem_float_fetch
#define shmem_int_fetch               pshmem_int_fetch
#define shmem_long_fetch              pshmem_long_fetch
#define shmem_longlong_fetch          pshmem_longlong_fetch

#define shmemx_int32_fetch            pshmemx_int32_fetch
#define shmemx_int64_fetch            pshmemx_int64_fetch

/* Atomic Fetch&Inc */
#define shmem_ctx_int_atomic_fetch_inc    pshmem_ctx_int_atomic_fetch_inc
#define shmem_ctx_long_atomic_fetch_inc   pshmem_ctx_long_atomic_fetch_inc
#define shmem_ctx_longlong_atomic_fetch_inc pshmem_ctx_longlong_atomic_fetch_inc
#define shmem_ctx_uint_atomic_fetch_inc    pshmem_ctx_uint_atomic_fetch_inc
#define shmem_ctx_ulong_atomic_fetch_inc   pshmem_ctx_ulong_atomic_fetch_inc
#define shmem_ctx_ulonglong_atomic_fetch_inc pshmem_ctx_ulonglong_atomic_fetch_inc
#define shmem_ctx_int32_atomic_fetch_inc    pshmem_ctx_int32_atomic_fetch_inc
#define shmem_ctx_int64_atomic_fetch_inc    pshmem_ctx_int64_atomic_fetch_inc
#define shmem_ctx_uint32_atomic_fetch_inc    pshmem_ctx_uint32_atomic_fetch_inc
#define shmem_ctx_uint64_atomic_fetch_inc    pshmem_ctx_uint64_atomic_fetch_inc
#define shmem_ctx_size_atomic_fetch_inc    pshmem_ctx_size_atomic_fetch_inc
#define shmem_ctx_ptrdiff_atomic_fetch_inc    pshmem_ctx_ptrdiff_atomic_fetch_inc

#define shmem_uint_atomic_fetch_inc        pshmem_uint_atomic_fetch_inc
#define shmem_ulong_atomic_fetch_inc       pshmem_ulong_atomic_fetch_inc
#define shmem_ulonglong_atomic_fetch_inc   pshmem_ulonglong_atomic_fetch_inc
#define shmem_int_atomic_fetch_inc        pshmem_int_atomic_fetch_inc
#define shmem_long_atomic_fetch_inc       pshmem_long_atomic_fetch_inc
#define shmem_longlong_atomic_fetch_inc   pshmem_longlong_atomic_fetch_inc
#define shmem_int32_atomic_fetch_inc    pshmem_int32_atomic_fetch_inc
#define shmem_int64_atomic_fetch_inc    pshmem_int64_atomic_fetch_inc
#define shmem_uint32_atomic_fetch_inc    pshmem_uint32_atomic_fetch_inc
#define shmem_uint64_atomic_fetch_inc    pshmem_uint64_atomic_fetch_inc
#define shmem_size_atomic_fetch_inc    pshmem_size_atomic_fetch_inc
#define shmem_ptrdiff_atomic_fetch_inc    pshmem_ptrdiff_atomic_fetch_inc



#define shmem_int_finc               pshmem_int_finc
#define shmem_long_finc              pshmem_long_finc
#define shmem_longlong_finc          pshmem_longlong_finc

#define shmemx_int32_finc            pshmemx_int32_finc
#define shmemx_int64_finc            pshmemx_int64_finc

/* Atomic Add */
#define shmem_ctx_int_atomic_add     pshmem_ctx_int_atomic_add
#define shmem_ctx_long_atomic_add    pshmem_ctx_long_atomic_add
#define shmem_ctx_longlong_atomic_add pshmem_ctx_longlong_atomic_add
#define shmem_ctx_uint_atomic_add    pshmem_ctx_uint_atomic_add
#define shmem_ctx_ulong_atomic_add   pshmem_ctx_ulong_atomic_add
#define shmem_ctx_ulonglong_atomic_add pshmem_ctx_ulonglong_atomic_add
#define shmem_ctx_int32_atomic_add     pshmem_ctx_int32_atomic_add
#define shmem_ctx_int64_atomic_add     pshmem_ctx_int64_atomic_add
#define shmem_ctx_uint32_atomic_add     pshmem_ctx_uint32_atomic_add
#define shmem_ctx_uint64_atomic_add     pshmem_ctx_uint64_atomic_add
#define shmem_ctx_size_atomic_add     pshmem_ctx_size_atomic_add
#define shmem_ctx_ptrdiff_atomic_add     pshmem_ctx_ptrdiff_atomic_add

#define shmem_int_atomic_add         pshmem_int_atomic_add
#define shmem_long_atomic_add        pshmem_long_atomic_add
#define shmem_longlong_atomic_add    pshmem_longlong_atomic_add
#define shmem_uint_atomic_add        pshmem_uint_atomic_add
#define shmem_ulong_atomic_add       pshmem_ulong_atomic_add
#define shmem_ulonglong_atomic_add   pshmem_ulonglong_atomic_add
#define shmem_int32_atomic_add     pshmem_int32_atomic_add
#define shmem_int64_atomic_add     pshmem_int64_atomic_add
#define shmem_uint32_atomic_add     pshmem_uint32_atomic_add
#define shmem_uint64_atomic_add     pshmem_uint64_atomic_add
#define shmem_size_atomic_add     pshmem_size_atomic_add
#define shmem_ptrdiff_atomic_add     pshmem_ptrdiff_atomic_add

#define shmem_int_add                pshmem_int_add
#define shmem_long_add               pshmem_long_add
#define shmem_longlong_add           pshmem_longlong_add

#define shmemx_int32_add             pshmemx_int32_add
#define shmemx_int64_add             pshmemx_int64_add

/* Atomic And */
#define shmem_int_atomic_and         pshmem_int_atomic_and
#define shmem_long_atomic_and        pshmem_long_atomic_and
#define shmem_longlong_atomic_and    pshmem_longlong_atomic_and
#define shmem_uint_atomic_and        pshmem_uint_atomic_and
#define shmem_ulong_atomic_and       pshmem_ulong_atomic_and
#define shmem_ulonglong_atomic_and   pshmem_ulonglong_atomic_and
#define shmem_int32_atomic_and       pshmem_int32_atomic_and
#define shmem_int64_atomic_and       pshmem_int64_atomic_and
#define shmem_uint32_atomic_and      pshmem_uint32_atomic_and
#define shmem_uint64_atomic_and      pshmem_uint64_atomic_and

#define shmem_ctx_int_atomic_and     pshmem_ctx_int_atomic_and
#define shmem_ctx_long_atomic_and    pshmem_ctx_long_atomic_and
#define shmem_ctx_longlong_atomic_and pshmem_ctx_longlong_atomic_and
#define shmem_ctx_uint_atomic_and    pshmem_ctx_uint_atomic_and
#define shmem_ctx_ulong_atomic_and   pshmem_ctx_ulong_atomic_and
#define shmem_ctx_ulonglong_atomic_and pshmem_ctx_ulonglong_atomic_and
#define shmem_ctx_int32_atomic_and   pshmem_ctx_int32_atomic_and
#define shmem_ctx_int64_atomic_and   pshmem_ctx_int64_atomic_and
#define shmem_ctx_uint32_atomic_and  pshmem_ctx_uint32_atomic_and
#define shmem_ctx_uint64_atomic_and  pshmem_ctx_uint64_atomic_and

#define shmemx_int32_atomic_and      pshmemx_int32_atomic_and
#define shmemx_int64_atomic_and      pshmemx_int64_atomic_and

#define shmemx_uint32_atomic_and     pshmemx_uint32_atomic_and
#define shmemx_uint64_atomic_and     pshmemx_uint64_atomic_and

/* Atomic Or */
#define shmem_int_atomic_or          pshmem_int_atomic_or
#define shmem_long_atomic_or         pshmem_long_atomic_or
#define shmem_longlong_atomic_or     pshmem_longlong_atomic_or
#define shmem_uint_atomic_or         pshmem_uint_atomic_or
#define shmem_ulong_atomic_or        pshmem_ulong_atomic_or
#define shmem_ulonglong_atomic_or    pshmem_ulonglong_atomic_or
#define shmem_int32_atomic_or        pshmem_int32_atomic_or
#define shmem_int64_atomic_or        pshmem_int64_atomic_or
#define shmem_uint32_atomic_or       pshmem_uint32_atomic_or
#define shmem_uint64_atomic_or       pshmem_uint64_atomic_or

#define shmem_ctx_int_atomic_or      pshmem_ctx_int_atomic_or
#define shmem_ctx_long_atomic_or     pshmem_ctx_long_atomic_or
#define shmem_ctx_longlong_atomic_or pshmem_ctx_longlong_atomic_or
#define shmem_ctx_uint_atomic_or     pshmem_ctx_uint_atomic_or
#define shmem_ctx_ulong_atomic_or    pshmem_ctx_ulong_atomic_or
#define shmem_ctx_ulonglong_atomic_or pshmem_ctx_ulonglong_atomic_or
#define shmem_ctx_int32_atomic_or    pshmem_ctx_int32_atomic_or
#define shmem_ctx_int64_atomic_or    pshmem_ctx_int64_atomic_or
#define shmem_ctx_uint32_atomic_or   pshmem_ctx_uint32_atomic_or
#define shmem_ctx_uint64_atomic_or   pshmem_ctx_uint64_atomic_or

#define shmemx_int32_atomic_or       pshmemx_int32_atomic_or
#define shmemx_int64_atomic_or       pshmemx_int64_atomic_or

#define shmemx_uint32_atomic_or      pshmemx_uint32_atomic_or
#define shmemx_uint64_atomic_or      pshmemx_uint64_atomic_or

/* Atomic Xor */
#define shmem_int_atomic_xor         pshmem_int_atomic_xor
#define shmem_long_atomic_xor        pshmem_long_atomic_xor
#define shmem_longlong_atomic_xor    pshmem_longlong_atomic_xor
#define shmem_uint_atomic_xor        pshmem_uint_atomic_xor
#define shmem_ulong_atomic_xor       pshmem_ulong_atomic_xor
#define shmem_ulonglong_atomic_xor   pshmem_ulonglong_atomic_xor
#define shmem_int32_atomic_xor       pshmem_int32_atomic_xor
#define shmem_int64_atomic_xor       pshmem_int64_atomic_xor
#define shmem_uint32_atomic_xor      pshmem_uint32_atomic_xor
#define shmem_uint64_atomic_xor      pshmem_uint64_atomic_xor

#define shmem_ctx_int_atomic_xor     pshmem_ctx_int_atomic_xor
#define shmem_ctx_long_atomic_xor    pshmem_ctx_long_atomic_xor
#define shmem_ctx_longlong_atomic_xor pshmem_ctx_longlong_atomic_xor
#define shmem_ctx_uint_atomic_xor    pshmem_ctx_uint_atomic_xor
#define shmem_ctx_ulong_atomic_xor   pshmem_ctx_ulong_atomic_xor
#define shmem_ctx_ulonglong_atomic_xor pshmem_ctx_ulonglong_atomic_xor
#define shmem_ctx_int32_atomic_xor   pshmem_ctx_int32_atomic_xor
#define shmem_ctx_int64_atomic_xor   pshmem_ctx_int64_atomic_xor
#define shmem_ctx_uint32_atomic_xor  pshmem_ctx_uint32_atomic_xor
#define shmem_ctx_uint64_atomic_xor  pshmem_ctx_uint64_atomic_xor

#define shmemx_int32_atomic_xor      pshmemx_int32_atomic_xor
#define shmemx_int64_atomic_xor      pshmemx_int64_atomic_xor

#define shmemx_uint32_atomic_xor     pshmemx_uint32_atomic_xor
#define shmemx_uint64_atomic_xor     pshmemx_uint64_atomic_xor

/* Atomic Inc */
#define shmem_ctx_int_atomic_inc     pshmem_ctx_int_atomic_inc
#define shmem_ctx_long_atomic_inc    pshmem_ctx_long_atomic_inc
#define shmem_ctx_longlong_atomic_inc pshmem_ctx_longlong_atomic_inc
#define shmem_ctx_uint_atomic_inc    pshmem_ctx_uint_atomic_inc
#define shmem_ctx_ulong_atomic_inc   pshmem_ctx_ulong_atomic_inc
#define shmem_ctx_ulonglong_atomic_inc pshmem_ctx_ulonglong_atomic_inc
#define shmem_ctx_int32_atomic_inc     pshmem_ctx_int32_atomic_inc
#define shmem_ctx_int64_atomic_inc     pshmem_ctx_int64_atomic_inc
#define shmem_ctx_uint32_atomic_inc     pshmem_ctx_uint32_atomic_inc
#define shmem_ctx_uint64_atomic_inc     pshmem_ctx_uint64_atomic_inc
#define shmem_ctx_size_atomic_inc     pshmem_ctx_size_atomic_inc
#define shmem_ctx_ptrdiff_atomic_inc     pshmem_ctx_ptrdiff_atomic_inc

#define shmem_int_atomic_inc         pshmem_int_atomic_inc
#define shmem_long_atomic_inc        pshmem_long_atomic_inc
#define shmem_longlong_atomic_inc    pshmem_longlong_atomic_inc
#define shmem_uint_atomic_inc        pshmem_uint_atomic_inc
#define shmem_ulong_atomic_inc       pshmem_ulong_atomic_inc
#define shmem_ulonglong_atomic_inc   pshmem_ulonglong_atomic_inc
#define shmem_int32_atomic_inc     pshmem_int32_atomic_inc
#define shmem_int64_atomic_inc     pshmem_int64_atomic_inc
#define shmem_uint32_atomic_inc     pshmem_uint32_atomic_inc
#define shmem_uint64_atomic_inc     pshmem_uint64_atomic_inc
#define shmem_size_atomic_inc     pshmem_size_atomic_inc
#define shmem_ptrdiff_atomic_inc     pshmem_ptrdiff_atomic_inc

#define shmem_int_inc                pshmem_int_inc
#define shmem_long_inc               pshmem_long_inc
#define shmem_longlong_inc           pshmem_longlong_inc

#define shmemx_int32_inc             pshmemx_int32_inc
#define shmemx_int64_inc             pshmemx_int64_inc

/* Nonblocking Atomic Fetch */
#define shmem_ctx_double_atomic_fetch_nbi           pshmem_ctx_double_atomic_fetch_nbi
#define shmem_ctx_float_atomic_fetch_nbi            pshmem_ctx_float_atomic_fetch_nbi
#define shmem_ctx_int_atomic_fetch_nbi              pshmem_ctx_int_atomic_fetch_nbi
#define shmem_ctx_long_atomic_fetch_nbi             pshmem_ctx_long_atomic_fetch_nbi
#define shmem_ctx_longlong_atomic_fetch_nbi         pshmem_ctx_longlong_atomic_fetch_nbi
#define shmem_ctx_uint_atomic_fetch_nbi             pshmem_ctx_uint_atomic_fetch_nbi
#define shmem_ctx_ulong_atomic_fetch_nbi            pshmem_ctx_ulong_atomic_fetch_nbi
#define shmem_ctx_ulonglong_atomic_fetch_nbi        pshmem_ctx_ulonglong_atomic_fetch_nbi
#define shmem_ctx_int32_atomic_fetch_nbi            pshmem_ctx_int32_atomic_fetch_nbi
#define shmem_ctx_int64_atomic_fetch_nbi            pshmem_ctx_int64_atomic_fetch_nbi
#define shmem_ctx_uint32_atomic_fetch_nbi           pshmem_ctx_uint32_atomic_fetch_nbi
#define shmem_ctx_uint64_atomic_fetch_nbi           pshmem_ctx_uint64_atomic_fetch_nbi
#define shmem_ctx_size_atomic_fetch_nbi             pshmem_ctx_size_atomic_fetch_nbi
#define shmem_ctx_ptrdiff_atomic_fetch_nbi          pshmem_ctx_ptrdiff_atomic_fetch_nbi

#define shmem_double_atomic_fetch_nbi     		pshmem_double_atomic_fetch_nbi
#define shmem_float_atomic_fetch_nbi      		pshmem_float_atomic_fetch_nbi
#define shmem_int_atomic_fetch_nbi        		pshmem_int_atomic_fetch_nbi
#define shmem_long_atomic_fetch_nbi       		pshmem_long_atomic_fetch_nbi
#define shmem_longlong_atomic_fetch_nbi   		pshmem_longlong_atomic_fetch_nbi
#define shmem_uint_atomic_fetch_nbi       		pshmem_uint_atomic_fetch_nbi
#define shmem_ulong_atomic_fetch_nbi      		pshmem_ulong_atomic_fetch_nbi
#define shmem_ulonglong_atomic_fetch_nbi  		pshmem_ulonglong_atomic_fetch_nbi
#define shmem_int32_atomic_fetch_nbi      		pshmem_int32_atomic_fetch_nbi
#define shmem_int64_atomic_fetch_nbi      		pshmem_int64_atomic_fetch_nbi
#define shmem_uint32_atomic_fetch_nbi     		pshmem_uint32_atomic_fetch_nbi
#define shmem_uint64_atomic_fetch_nbi     		pshmem_uint64_atomic_fetch_nbi
#define shmem_size_atomic_fetch_nbi       		pshmem_size_atomic_fetch_nbi
#define shmem_ptrdiff_atomic_fetch_nbi    		pshmem_ptrdiff_atomic_fetch_nbi


/* Nonblocking Atomic Compare Swap */
#define shmem_ctx_int_atomic_compare_swap_nbi               pshmem_ctx_int_atomic_compare_swap_nbi
#define shmem_ctx_long_atomic_compare_swap_nbi              pshmem_ctx_long_atomic_compare_swap_nbi
#define shmem_ctx_longlong_atomic_compare_swap_nbi          pshmem_ctx_longlong_atomic_compare_swap_nbi
#define shmem_ctx_uint_atomic_compare_swap_nbi              pshmem_ctx_uint_atomic_compare_swap_nbi
#define shmem_ctx_ulong_atomic_compare_swap_nbi             pshmem_ctx_ulong_atomic_compare_swap_nbi
#define shmem_ctx_ulonglong_atomic_compare_swap_nbi         pshmem_ctx_ulonglong_atomic_compare_swap_nbi
#define shmem_ctx_int32_atomic_compare_swap_nbi             pshmem_ctx_int32_atomic_compare_swap_nbi
#define shmem_ctx_int64_atomic_compare_swap_nbi             pshmem_ctx_int64_atomic_compare_swap_nbi
#define shmem_ctx_uint32_atomic_compare_swap_nbi            pshmem_ctx_uint32_atomic_compare_swap_nbi
#define shmem_ctx_uint64_atomic_compare_swap_nbi            pshmem_ctx_uint64_atomic_compare_swap_nbi
#define shmem_ctx_size_atomic_compare_swap_nbi              pshmem_ctx_size_atomic_compare_swap_nbi
#define shmem_ctx_ptrdiff_atomic_compare_swap_nbi           pshmem_ctx_ptrdiff_atomic_compare_swap_nbi

#define shmem_int_atomic_compare_swap_nbi        	pshmem_int_atomic_compare_swap_nbi
#define shmem_long_atomic_compare_swap_nbi       	pshmem_long_atomic_compare_swap_nbi
#define shmem_longlong_atomic_compare_swap_nbi   	pshmem_longlong_atomic_compare_swap_nbi
#define shmem_uint_atomic_compare_swap_nbi       	pshmem_uint_atomic_compare_swap_nbi
#define shmem_ulong_atomic_compare_swap_nbi      	pshmem_ulong_atomic_compare_swap_nbi
#define shmem_ulonglong_atomic_compare_swap_nbi        pshmem_ulonglong_atomic_compare_swap_nbi
#define shmem_int32_atomic_compare_swap_nbi      	pshmem_int32_atomic_compare_swap_nbi
#define shmem_int64_atomic_compare_swap_nbi      	pshmem_int64_atomic_compare_swap_nbi
#define shmem_uint32_atomic_compare_swap_nbi     	pshmem_uint32_atomic_compare_swap_nbi
#define shmem_uint64_atomic_compare_swap_nbi     	pshmem_uint64_atomic_compare_swap_nbi
#define shmem_size_atomic_compare_swap_nbi       	pshmem_size_atomic_compare_swap_nbi
#define shmem_ptrdiff_atomic_compare_swap_nbi    	pshmem_ptrdiff_atomic_compare_swap_nbi


/* Nonblocking Atomic Swap */
#define shmem_ctx_double_atomic_swap_nbi            pshmem_ctx_double_atomic_swap_nbi
#define shmem_ctx_float_atomic_swap_nbi             pshmem_ctx_float_atomic_swap_nbi
#define shmem_ctx_int_atomic_swap_nbi               pshmem_ctx_int_atomic_swap_nbi
#define shmem_ctx_long_atomic_swap_nbi              pshmem_ctx_long_atomic_swap_nbi
#define shmem_ctx_longlong_atomic_swap_nbi          pshmem_ctx_longlong_atomic_swap_nbi
#define shmem_ctx_uint_atomic_swap_nbi              pshmem_ctx_uint_atomic_swap_nbi
#define shmem_ctx_ulong_atomic_swap_nbi             pshmem_ctx_ulong_atomic_swap_nbi
#define shmem_ctx_ulonglong_atomic_swap_nbi         pshmem_ctx_ulonglong_atomic_swap_nbi
#define shmem_ctx_int32_atomic_swap_nbi             pshmem_ctx_int32_atomic_swap_nbi
#define shmem_ctx_int64_atomic_swap_nbi             pshmem_ctx_int64_atomic_swap_nbi
#define shmem_ctx_uint32_atomic_swap_nbi            pshmem_ctx_uint32_atomic_swap_nbi
#define shmem_ctx_uint64_atomic_swap_nbi            pshmem_ctx_uint64_atomic_swap_nbi
#define shmem_ctx_size_atomic_swap_nbi              pshmem_ctx_size_atomic_swap_nbi
#define shmem_ctx_ptrdiff_atomic_swap_nbi           pshmem_ctx_ptrdiff_atomic_swap_nbi

#define shmem_double_atomic_swap_nbi     	        pshmem_double_atomic_swap_nbi
#define shmem_float_atomic_swap_nbi      	        pshmem_float_atomic_swap_nbi
#define shmem_int_atomic_swap_nbi        	        pshmem_int_atomic_swap_nbi
#define shmem_long_atomic_swap_nbi       	        pshmem_long_atomic_swap_nbi
#define shmem_longlong_atomic_swap_nbi   	        pshmem_longlong_atomic_swap_nbi
#define shmem_uint_atomic_swap_nbi       		pshmem_uint_atomic_swap_nbi
#define shmem_ulong_atomic_swap_nbi      		pshmem_ulong_atomic_swap_nbi
#define shmem_ulonglong_atomic_swap_nbi  		pshmem_ulonglong_atomic_swap_nbi
#define shmem_int32_atomic_swap_nbi      		pshmem_int32_atomic_swap_nbi
#define shmem_int64_atomic_swap_nbi      		pshmem_int64_atomic_swap_nbi
#define shmem_uint32_atomic_swap_nbi     		pshmem_uint32_atomic_swap_nbi
#define shmem_uint64_atomic_swap_nbi     		pshmem_uint64_atomic_swap_nbi
#define shmem_size_atomic_swap_nbi       		pshmem_size_atomic_swap_nbi
#define shmem_ptrdiff_atomic_swap_nbi    		pshmem_ptrdiff_atomic_swap_nbi


/* Nonblocking Atomic Fetch and Increment */
#define shmem_ctx_int_atomic_fetch_inc_nbi               pshmem_ctx_int_atomic_fetch_inc_nbi
#define shmem_ctx_long_atomic_fetch_inc_nbi              pshmem_ctx_long_atomic_fetch_inc_nbi
#define shmem_ctx_longlong_atomic_fetch_inc_nbi          pshmem_ctx_longlong_atomic_fetch_inc_nbi
#define shmem_ctx_uint_atomic_fetch_inc_nbi              pshmem_ctx_uint_atomic_fetch_inc_nbi
#define shmem_ctx_ulong_atomic_fetch_inc_nbi             pshmem_ctx_ulong_atomic_fetch_inc_nbi
#define shmem_ctx_ulonglong_atomic_fetch_inc_nbi         pshmem_ctx_ulonglong_atomic_fetch_inc_nbi
#define shmem_ctx_int32_atomic_fetch_inc_nbi             pshmem_ctx_int32_atomic_fetch_inc_nbi
#define shmem_ctx_int64_atomic_fetch_inc_nbi             pshmem_ctx_int64_atomic_fetch_inc_nbi
#define shmem_ctx_uint32_atomic_fetch_inc_nbi            pshmem_ctx_uint32_atomic_fetch_inc_nbi
#define shmem_ctx_uint64_atomic_fetch_inc_nbi            pshmem_ctx_uint64_atomic_fetch_inc_nbi
#define shmem_ctx_size_atomic_fetch_inc_nbi              pshmem_ctx_size_atomic_fetch_inc_nbi
#define shmem_ctx_ptrdiff_atomic_fetch_inc_nbi           pshmem_ctx_ptrdiff_atomic_fetch_inc_nbi

#define shmem_int_atomic_fetch_inc_nbi        			 pshmem_int_atomic_fetch_inc_nbi
#define shmem_long_atomic_fetch_inc_nbi       			 pshmem_long_atomic_fetch_inc_nbi
#define shmem_longlong_atomic_fetch_inc_nbi   			 pshmem_longlong_atomic_fetch_inc_nbi
#define shmem_uint_atomic_fetch_inc_nbi       			 pshmem_uint_atomic_fetch_inc_nbi
#define shmem_ulong_atomic_fetch_inc_nbi      			 pshmem_ulong_atomic_fetch_inc_nbi
#define shmem_ulonglong_atomic_fetch_inc_nbi  			 pshmem_ulonglong_atomic_fetch_inc_nbi
#define shmem_int32_atomic_fetch_inc_nbi      			 pshmem_int32_atomic_fetch_inc_nbi
#define shmem_int64_atomic_fetch_inc_nbi      			 pshmem_int64_atomic_fetch_inc_nbi
#define shmem_uint32_atomic_fetch_inc_nbi     			 pshmem_uint32_atomic_fetch_inc_nbi
#define shmem_uint64_atomic_fetch_inc_nbi     			 pshmem_uint64_atomic_fetch_inc_nbi
#define shmem_size_atomic_fetch_inc_nbi       			 pshmem_size_atomic_fetch_inc_nbi
#define shmem_ptrdiff_atomic_fetch_inc_nbi    			 pshmem_ptrdiff_atomic_fetch_inc_nbi


/* Nonblocking Atomic Fetch and Add */
#define shmem_ctx_int_atomic_fetch_add_nbi               pshmem_ctx_int_atomic_fetch_add_nbi
#define shmem_ctx_long_atomic_fetch_add_nbi              pshmem_ctx_long_atomic_fetch_add_nbi
#define shmem_ctx_longlong_atomic_fetch_add_nbi          pshmem_ctx_longlong_atomic_fetch_add_nbi
#define shmem_ctx_uint_atomic_fetch_add_nbi              pshmem_ctx_uint_atomic_fetch_add_nbi
#define shmem_ctx_ulong_atomic_fetch_add_nbi             pshmem_ctx_ulong_atomic_fetch_add_nbi
#define shmem_ctx_ulonglong_atomic_fetch_add_nbi         pshmem_ctx_ulonglong_atomic_fetch_add_nbi
#define shmem_ctx_int32_atomic_fetch_add_nbi             pshmem_ctx_int32_atomic_fetch_add_nbi
#define shmem_ctx_int64_atomic_fetch_add_nbi             pshmem_ctx_int64_atomic_fetch_add_nbi
#define shmem_ctx_uint32_atomic_fetch_add_nbi            pshmem_ctx_uint32_atomic_fetch_add_nbi
#define shmem_ctx_uint64_atomic_fetch_add_nbi            pshmem_ctx_uint64_atomic_fetch_add_nbi
#define shmem_ctx_size_atomic_fetch_add_nbi              pshmem_ctx_size_atomic_fetch_add_nbi
#define shmem_ctx_ptrdiff_atomic_fetch_add_nbi           pshmem_ctx_ptrdiff_atomic_fetch_add_nbi

#define shmem_int_atomic_fetch_add_nbi        			 pshmem_int_atomic_fetch_add_nbi
#define shmem_long_atomic_fetch_add_nbi       			 pshmem_long_atomic_fetch_add_nbi
#define shmem_longlong_atomic_fetch_add_nbi   			 pshmem_longlong_atomic_fetch_add_nbi
#define shmem_uint_atomic_fetch_add_nbi       			 pshmem_uint_atomic_fetch_add_nbi
#define shmem_ulong_atomic_fetch_add_nbi      			 pshmem_ulong_atomic_fetch_add_nbi
#define shmem_ulonglong_atomic_fetch_add_nbi  			 pshmem_ulonglong_atomic_fetch_add_nbi
#define shmem_int32_atomic_fetch_add_nbi      			 pshmem_int32_atomic_fetch_add_nbi
#define shmem_int64_atomic_fetch_add_nbi      			 pshmem_int64_atomic_fetch_add_nbi
#define shmem_uint32_atomic_fetch_add_nbi     			 pshmem_uint32_atomic_fetch_add_nbi
#define shmem_uint64_atomic_fetch_add_nbi     			 pshmem_uint64_atomic_fetch_add_nbi
#define shmem_size_atomic_fetch_add_nbi       			 pshmem_size_atomic_fetch_add_nbi
#define shmem_ptrdiff_atomic_fetch_add_nbi    			 pshmem_ptrdiff_atomic_fetch_add_nbi


/* Nonblocking Atomic Fetch and And */
#define shmem_ctx_uint_atomic_fetch_and_nbi              pshmem_ctx_uint_atomic_fetch_and_nbi
#define shmem_ctx_ulong_atomic_fetch_and_nbi             pshmem_ctx_ulong_atomic_fetch_and_nbi
#define shmem_ctx_ulonglong_atomic_fetch_and_nbi         pshmem_ctx_ulonglong_atomic_fetch_and_nbi
#define shmem_ctx_int32_atomic_fetch_and_nbi             pshmem_ctx_int32_atomic_fetch_and_nbi
#define shmem_ctx_int64_atomic_fetch_and_nbi             pshmem_ctx_int64_atomic_fetch_and_nbi
#define shmem_ctx_uint32_atomic_fetch_and_nbi            pshmem_ctx_uint32_atomic_fetch_and_nbi
#define shmem_ctx_uint64_atomic_fetch_and_nbi            pshmem_ctx_uint64_atomic_fetch_and_nbi

#define shmem_uint_atomic_fetch_and_nbi       			 pshmem_uint_atomic_fetch_and_nbi
#define shmem_ulong_atomic_fetch_and_nbi      			 pshmem_ulong_atomic_fetch_and_nbi
#define shmem_ulonglong_atomic_fetch_and_nbi  			 pshmem_ulonglong_atomic_fetch_and_nbi
#define shmem_int32_atomic_fetch_and_nbi      			 pshmem_int32_atomic_fetch_and_nbi
#define shmem_int64_atomic_fetch_and_nbi      			 pshmem_int64_atomic_fetch_and_nbi
#define shmem_uint32_atomic_fetch_and_nbi     			 pshmem_uint32_atomic_fetch_and_nbi
#define shmem_uint64_atomic_fetch_and_nbi     			 pshmem_uint64_atomic_fetch_and_nbi


/* Nonblocking Atomic Fetch and OR */
#define shmem_ctx_uint_atomic_fetch_or_nbi              pshmem_ctx_uint_atomic_fetch_or_nbi
#define shmem_ctx_ulong_atomic_fetch_or_nbi             pshmem_ctx_ulong_atomic_fetch_or_nbi
#define shmem_ctx_ulonglong_atomic_fetch_or_nbi         pshmem_ctx_ulonglong_atomic_fetch_or_nbi
#define shmem_ctx_int32_atomic_fetch_or_nbi             pshmem_ctx_int32_atomic_fetch_or_nbi
#define shmem_ctx_int64_atomic_fetch_or_nbi             pshmem_ctx_int64_atomic_fetch_or_nbi
#define shmem_ctx_uint32_atomic_fetch_or_nbi            pshmem_ctx_uint32_atomic_fetch_or_nbi
#define shmem_ctx_uint64_atomic_fetch_or_nbi            pshmem_ctx_uint64_atomic_fetch_or_nbi

#define shmem_uint_atomic_fetch_or_nbi       			pshmem_uint_atomic_fetch_or_nbi
#define shmem_ulong_atomic_fetch_or_nbi      			pshmem_ulong_atomic_fetch_or_nbi
#define shmem_ulonglong_atomic_fetch_or_nbi  			pshmem_ulonglong_atomic_fetch_or_nbi
#define shmem_int32_atomic_fetch_or_nbi      			pshmem_int32_atomic_fetch_or_nbi
#define shmem_int64_atomic_fetch_or_nbi      			pshmem_int64_atomic_fetch_or_nbi
#define shmem_uint32_atomic_fetch_or_nbi     			pshmem_uint32_atomic_fetch_or_nbi
#define shmem_uint64_atomic_fetch_or_nbi     			pshmem_uint64_atomic_fetch_or_nbi


/* Nonblocking Atomic Fetch and XOR */
#define shmem_ctx_uint_atomic_fetch_xor_nbi              pshmem_ctx_uint_atomic_fetch_xor_nbi
#define shmem_ctx_ulong_atomic_fetch_xor_nbi             pshmem_ctx_ulong_atomic_fetch_xor_nbi
#define shmem_ctx_ulonglong_atomic_fetch_xor_nbi         pshmem_ctx_ulonglong_atomic_fetch_xor_nbi
#define shmem_ctx_int32_atomic_fetch_xor_nbi             pshmem_ctx_int32_atomic_fetch_xor_nbi
#define shmem_ctx_int64_atomic_fetch_xor_nbi             pshmem_ctx_int64_atomic_fetch_xor_nbi
#define shmem_ctx_uint32_atomic_fetch_xor_nbi            pshmem_ctx_uint32_atomic_fetch_xor_nbi
#define shmem_ctx_uint64_atomic_fetch_xor_nbi            pshmem_ctx_uint64_atomic_fetch_xor_nbi

#define shmem_uint_atomic_fetch_xor_nbi       			 pshmem_uint_atomic_fetch_xor_nbi
#define shmem_ulong_atomic_fetch_xor_nbi      			 pshmem_ulong_atomic_fetch_xor_nbi
#define shmem_ulonglong_atomic_fetch_xor_nbi  			 pshmem_ulonglong_atomic_fetch_xor_nbi
#define shmem_int32_atomic_fetch_xor_nbi      			 pshmem_int32_atomic_fetch_xor_nbi
#define shmem_int64_atomic_fetch_xor_nbi      			 pshmem_int64_atomic_fetch_xor_nbi
#define shmem_uint32_atomic_fetch_xor_nbi     			 pshmem_uint32_atomic_fetch_xor_nbi
#define shmem_uint64_atomic_fetch_xor_nbi     			 pshmem_uint64_atomic_fetch_xor_nbi

/*
 * Control of profile
 */

#define shmem_pcontrol               pshmem_pcontrol

/*
 * Lock functions
 */
#define shmem_set_lock               pshmem_set_lock
#define shmem_clear_lock             pshmem_clear_lock
#define shmem_test_lock              pshmem_test_lock

/*
 * P2P sync routines
 */
#define shmem_short_wait             pshmem_short_wait
#define shmem_int_wait               pshmem_int_wait
#define shmem_long_wait              pshmem_long_wait
#define shmem_longlong_wait          pshmem_longlong_wait
#define shmem_wait                   pshmem_wait
#define shmemx_int32_wait            pshmemx_int32_wait
#define shmemx_int64_wait            pshmemx_int64_wait

#define shmem_short_wait_until       pshmem_short_wait_until
#define shmem_int_wait_until         pshmem_int_wait_until
#define shmem_long_wait_until        pshmem_long_wait_until
#define shmem_longlong_wait_until    pshmem_longlong_wait_until
#define shmem_ushort_wait_until      pshmem_ushort_wait_until
#define shmem_uint_wait_until        pshmem_uint_wait_until
#define shmem_ulong_wait_until       pshmem_ulong_wait_until
#define shmem_ulonglong_wait_until   pshmem_ulonglong_wait_until
#define shmem_int32_wait_until       pshmem_int32_wait_until
#define shmem_int64_wait_until       pshmem_int64_wait_until
#define shmem_uint32_wait_until      pshmem_uint32_wait_until
#define shmem_uint64_wait_until      pshmem_uint64_wait_until
#define shmem_size_wait_until        pshmem_size_wait_until
#define shmem_ptrdiff_wait_until     pshmem_ptrdiff_wait_until

#define shmemx_int32_wait_until      pshmemx_int32_wait_until
#define shmemx_int64_wait_until      pshmemx_int64_wait_until

#define shmem_short_wait_until_all       pshmem_short_wait_until_all
#define shmem_ushort_wait_until_all      pshmem_ushort_wait_until_all
#define shmem_int_wait_until_all         pshmem_int_wait_until_all
#define shmem_long_wait_until_all        pshmem_long_wait_until_all
#define shmem_longlong_wait_until_all    pshmem_longlong_wait_until_all
#define shmem_uint_wait_until_all        pshmem_uint_wait_until_all
#define shmem_ulong_wait_until_all       pshmem_ulong_wait_until_all
#define shmem_ulonglong_wait_until_all   pshmem_ulonglong_wait_until_all
#define shmem_int32_wait_until_all       pshmem_int32_wait_until_all
#define shmem_int64_wait_until_all       pshmem_int64_wait_until_all
#define shmem_uint32_wait_until_all      pshmem_uint32_wait_until_all
#define shmem_uint64_wait_until_all      pshmem_uint64_wait_until_all
#define shmem_size_wait_until_all        pshmem_size_wait_until_all
#define shmem_ptrdiff_wait_until_all     pshmem_ptrdiff_wait_until_all


#define shmem_short_wait_until_any       pshmem_short_wait_until_any
#define shmem_ushort_wait_until_any      pshmem_ushort_wait_until_any
#define shmem_int_wait_until_any         pshmem_int_wait_until_any
#define shmem_long_wait_until_any        pshmem_long_wait_until_any
#define shmem_longlong_wait_until_any    pshmem_longlong_wait_until_any
#define shmem_uint_wait_until_any        pshmem_uint_wait_until_any
#define shmem_ulong_wait_until_any       pshmem_ulong_wait_until_any
#define shmem_ulonglong_wait_until_any   pshmem_ulonglong_wait_until_any
#define shmem_int32_wait_until_any       pshmem_int32_wait_until_any
#define shmem_int64_wait_until_any       pshmem_int64_wait_until_any
#define shmem_uint32_wait_until_any      pshmem_uint32_wait_until_any
#define shmem_uint64_wait_until_any      pshmem_uint64_wait_until_any
#define shmem_size_wait_until_any        pshmem_size_wait_until_any
#define shmem_ptrdiff_wait_until_any     pshmem_ptrdiff_wait_until_any


#define shmem_short_wait_until_some       pshmem_short_wait_until_some
#define shmem_ushort_wait_until_some      pshmem_ushort_wait_until_some
#define shmem_int_wait_until_some         pshmem_int_wait_until_some
#define shmem_long_wait_until_some        pshmem_long_wait_until_some
#define shmem_longlong_wait_until_some    pshmem_longlong_wait_until_some
#define shmem_uint_wait_until_some        pshmem_uint_wait_until_some
#define shmem_ulong_wait_until_some       pshmem_ulong_wait_until_some
#define shmem_ulonglong_wait_until_some   pshmem_ulonglong_wait_until_some
#define shmem_int32_wait_until_some       pshmem_int32_wait_until_some
#define shmem_int64_wait_until_some       pshmem_int64_wait_until_some
#define shmem_uint32_wait_until_some      pshmem_uint32_wait_until_some
#define shmem_uint64_wait_until_some      pshmem_uint64_wait_until_some
#define shmem_size_wait_until_some        pshmem_size_wait_until_some
#define shmem_ptrdiff_wait_until_some     pshmem_ptrdiff_wait_until_some


#define shmem_short_wait_until_all_vector       pshmem_short_wait_until_all_vector
#define shmem_ushort_wait_until_all_vector      pshmem_ushort_wait_until_all_vector
#define shmem_int_wait_until_all_vector         pshmem_int_wait_until_all_vector
#define shmem_long_wait_until_all_vector        pshmem_long_wait_until_all_vector
#define shmem_longlong_wait_until_all_vector    pshmem_longlong_wait_until_all_vector
#define shmem_uint_wait_until_all_vector        pshmem_uint_wait_until_all_vector
#define shmem_ulong_wait_until_all_vector       pshmem_ulong_wait_until_all_vector
#define shmem_ulonglong_wait_until_all_vector   pshmem_ulonglong_wait_until_all_vector
#define shmem_int32_wait_until_all_vector       pshmem_int32_wait_until_all_vector
#define shmem_int64_wait_until_all_vector       pshmem_int64_wait_until_all_vector
#define shmem_uint32_wait_until_all_vector      pshmem_uint32_wait_until_all_vector
#define shmem_uint64_wait_until_all_vector      pshmem_uint64_wait_until_all_vector
#define shmem_size_wait_until_all_vector        pshmem_size_wait_until_all_vector
#define shmem_ptrdiff_wait_until_all_vector     pshmem_ptrdiff_wait_until_all_vector


#define shmem_short_wait_until_any_vector       pshmem_short_wait_until_any_vector
#define shmem_ushort_wait_until_any_vector      pshmem_ushort_wait_until_any_vector
#define shmem_int_wait_until_any_vector         pshmem_int_wait_until_any_vector
#define shmem_long_wait_until_any_vector        pshmem_long_wait_until_any_vector
#define shmem_longlong_wait_until_any_vector    pshmem_longlong_wait_until_any_vector
#define shmem_uint_wait_until_any_vector        pshmem_uint_wait_until_any_vector
#define shmem_ulong_wait_until_any_vector       pshmem_ulong_wait_until_any_vector
#define shmem_ulonglong_wait_until_any_vector   pshmem_ulonglong_wait_until_any_vector
#define shmem_int32_wait_until_any_vector       pshmem_int32_wait_until_any_vector
#define shmem_int64_wait_until_any_vector       pshmem_int64_wait_until_any_vector
#define shmem_uint32_wait_until_any_vector      pshmem_uint32_wait_until_any_vector
#define shmem_uint64_wait_until_any_vector      pshmem_uint64_wait_until_any_vector
#define shmem_size_wait_until_any_vector        pshmem_size_wait_until_any_vector
#define shmem_ptrdiff_wait_until_any_vector     pshmem_ptrdiff_wait_until_any_vector


#define shmem_short_wait_until_some_vector       pshmem_short_wait_until_some_vector
#define shmem_ushort_wait_until_some_vector      pshmem_ushort_wait_until_some_vector
#define shmem_int_wait_until_some_vector         pshmem_int_wait_until_some_vector
#define shmem_long_wait_until_some_vector        pshmem_long_wait_until_some_vector
#define shmem_longlong_wait_until_some_vector    pshmem_longlong_wait_until_some_vector
#define shmem_uint_wait_until_some_vector        pshmem_uint_wait_until_some_vector
#define shmem_ulong_wait_until_some_vector       pshmem_ulong_wait_until_some_vector
#define shmem_ulonglong_wait_until_some_vector   pshmem_ulonglong_wait_until_some_vector
#define shmem_int32_wait_until_some_vector       pshmem_int32_wait_until_some_vector
#define shmem_int64_wait_until_some_vector       pshmem_int64_wait_until_some_vector
#define shmem_uint32_wait_until_some_vector      pshmem_uint32_wait_until_some_vector
#define shmem_uint64_wait_until_some_vector      pshmem_uint64_wait_until_some_vector
#define shmem_size_wait_until_some_vector        pshmem_size_wait_until_some_vector
#define shmem_ptrdiff_wait_until_some_vector     pshmem_ptrdiff_wait_until_some_vector


#define shmem_short_test             pshmem_short_test
#define shmem_int_test               pshmem_int_test
#define shmem_long_test              pshmem_long_test
#define shmem_longlong_test          pshmem_longlong_test
#define shmem_ushort_test            pshmem_ushort_test
#define shmem_uint_test              pshmem_uint_test
#define shmem_ulong_test             pshmem_ulong_test
#define shmem_ulonglong_test         pshmem_ulonglong_test
#define shmem_int32_test             pshmem_int32_test
#define shmem_int64_test             pshmem_int64_test
#define shmem_uint32_test            pshmem_uint32_test
#define shmem_uint64_test            pshmem_uint64_test
#define shmem_size_test              pshmem_size_test
#define shmem_ptrdiff_test           pshmem_ptrdiff_test


#define shmem_short_test_all       pshmem_short_test_all
#define shmem_ushort_test_all      pshmem_ushort_test_all
#define shmem_int_test_all         pshmem_int_test_all
#define shmem_long_test_all        pshmem_long_test_all
#define shmem_longlong_test_all    pshmem_longlong_test_all
#define shmem_uint_test_all        pshmem_uint_test_all
#define shmem_ulong_test_all       pshmem_ulong_test_all
#define shmem_ulonglong_test_all   pshmem_ulonglong_test_all
#define shmem_int32_test_all       pshmem_int32_test_all
#define shmem_int64_test_all       pshmem_int64_test_all
#define shmem_uint32_test_all      pshmem_uint32_test_all
#define shmem_uint64_test_all      pshmem_uint64_test_all
#define shmem_size_test_all        pshmem_size_test_all
#define shmem_ptrdiff_test_all     pshmem_ptrdiff_test_all


#define shmem_short_test_any       pshmem_short_test_any
#define shmem_ushort_test_any      pshmem_ushort_test_any
#define shmem_int_test_any         pshmem_int_test_any
#define shmem_long_test_any        pshmem_long_test_any
#define shmem_longlong_test_any    pshmem_longlong_test_any
#define shmem_uint_test_any        pshmem_uint_test_any
#define shmem_ulong_test_any       pshmem_ulong_test_any
#define shmem_ulonglong_test_any   pshmem_ulonglong_test_any
#define shmem_int32_test_any       pshmem_int32_test_any
#define shmem_int64_test_any       pshmem_int64_test_any
#define shmem_uint32_test_any      pshmem_uint32_test_any
#define shmem_uint64_test_any      pshmem_uint64_test_any
#define shmem_size_test_any        pshmem_size_test_any
#define shmem_ptrdiff_test_any     pshmem_ptrdiff_test_any



#define shmem_short_test_some       pshmem_short_test_some
#define shmem_ushort_test_some      pshmem_ushort_test_some
#define shmem_int_test_some         pshmem_int_test_some
#define shmem_long_test_some        pshmem_long_test_some
#define shmem_longlong_test_some    pshmem_longlong_test_some
#define shmem_uint_test_some        pshmem_uint_test_some
#define shmem_ulong_test_some       pshmem_ulong_test_some
#define shmem_ulonglong_test_some   pshmem_ulonglong_test_some
#define shmem_int32_test_some       pshmem_int32_test_some
#define shmem_int64_test_some       pshmem_int64_test_some
#define shmem_uint32_test_some      pshmem_uint32_test_some
#define shmem_uint64_test_some      pshmem_uint64_test_some
#define shmem_size_test_some        pshmem_size_test_some
#define shmem_ptrdiff_test_some     pshmem_ptrdiff_test_some


#define shmem_short_test_all_vector       pshmem_short_test_all_vector
#define shmem_ushort_test_all_vector      pshmem_ushort_test_all_vector
#define shmem_int_test_all_vector         pshmem_int_test_all_vector
#define shmem_long_test_all_vector        pshmem_long_test_all_vector
#define shmem_longlong_test_all_vector    pshmem_longlong_test_all_vector
#define shmem_uint_test_all_vector        pshmem_uint_test_all_vector
#define shmem_ulong_test_all_vector       pshmem_ulong_test_all_vector
#define shmem_ulonglong_test_all_vector   pshmem_ulonglong_test_all_vector
#define shmem_int32_test_all_vector       pshmem_int32_test_all_vector
#define shmem_int64_test_all_vector       pshmem_int64_test_all_vector
#define shmem_uint32_test_all_vector      pshmem_uint32_test_all_vector
#define shmem_uint64_test_all_vector      pshmem_uint64_test_all_vector
#define shmem_size_test_all_vector        pshmem_size_test_all_vector
#define shmem_ptrdiff_test_all_vector     pshmem_ptrdiff_test_all_vector


#define shmem_short_test_any_vector       pshmem_short_test_any_vector
#define shmem_ushort_test_any_vector      pshmem_ushort_test_any_vector
#define shmem_int_test_any_vector         pshmem_int_test_any_vector
#define shmem_long_test_any_vector        pshmem_long_test_any_vector
#define shmem_longlong_test_any_vector    pshmem_longlong_test_any_vector
#define shmem_uint_test_any_vector        pshmem_uint_test_any_vector
#define shmem_ulong_test_any_vector       pshmem_ulong_test_any_vector
#define shmem_ulonglong_test_any_vector   pshmem_ulonglong_test_any_vector
#define shmem_int32_test_any_vector       pshmem_int32_test_any_vector
#define shmem_int64_test_any_vector       pshmem_int64_test_any_vector
#define shmem_uint32_test_any_vector      pshmem_uint32_test_any_vector
#define shmem_uint64_test_any_vector      pshmem_uint64_test_any_vector
#define shmem_size_test_any_vector        pshmem_size_test_any_vector
#define shmem_ptrdiff_test_any_vector     pshmem_ptrdiff_test_any_vector


#define shmem_short_test_some_vector       pshmem_short_test_some_vector
#define shmem_ushort_test_some_vector      pshmem_ushort_test_some_vector
#define shmem_int_test_some_vector         pshmem_int_test_some_vector
#define shmem_long_test_some_vector        pshmem_long_test_some_vector
#define shmem_longlong_test_some_vector    pshmem_longlong_test_some_vector
#define shmem_uint_test_some_vector        pshmem_uint_test_some_vector
#define shmem_ulong_test_some_vector       pshmem_ulong_test_some_vector
#define shmem_ulonglong_test_some_vector   pshmem_ulonglong_test_some_vector
#define shmem_int32_test_some_vector       pshmem_int32_test_some_vector
#define shmem_int64_test_some_vector       pshmem_int64_test_some_vector
#define shmem_uint32_test_some_vector      pshmem_uint32_test_some_vector
#define shmem_uint64_test_some_vector      pshmem_uint64_test_some_vector
#define shmem_size_test_some_vector        pshmem_size_test_some_vector
#define shmem_ptrdiff_test_some_vector     pshmem_ptrdiff_test_some_vector

/*
 * Barrier sync routines
 */
#define shmem_barrier                pshmem_barrier
#define shmem_barrier_all            pshmem_barrier_all
#define shmem_sync_all               pshmem_sync_all
#define shmem_sync_deprecated        pshmem_sync_deprecated
#define shmem_fence                  pshmem_fence
#define shmem_ctx_fence              pshmem_ctx_fence
#define shmem_quiet                  pshmem_quiet
#define shmem_ctx_quiet              pshmem_ctx_quiet

/*
 * Collective routines
 */
#define shmem_broadcast32            pshmem_broadcast32
#define shmem_broadcast64            pshmem_broadcast64
#define shmem_collect32              pshmem_collect32
#define shmem_collect64              pshmem_collect64
#define shmem_fcollect32             pshmem_fcollect32
#define shmem_fcollect64             pshmem_fcollect64

/*
 * Reduction routines
 */
#define shmem_short_and_to_all       pshmem_short_and_to_all
#define shmem_int_and_to_all         pshmem_int_and_to_all
#define shmem_long_and_to_all        pshmem_long_and_to_all
#define shmem_longlong_and_to_all    pshmem_longlong_and_to_all
#define shmemx_int16_and_to_all      pshmemx_int16_and_to_all
#define shmemx_int32_and_to_all      pshmemx_int32_and_to_all
#define shmemx_int64_and_to_all      pshmemx_int64_and_to_all

#define shmem_short_or_to_all        pshmem_short_or_to_all
#define shmem_int_or_to_all          pshmem_int_or_to_all
#define shmem_long_or_to_all         pshmem_long_or_to_all
#define shmem_longlong_or_to_all     pshmem_longlong_or_to_all
#define shmemx_int16_or_to_all       pshmemx_int16_or_to_all
#define shmemx_int32_or_to_all       pshmemx_int32_or_to_all
#define shmemx_int64_or_to_all       pshmemx_int64_or_to_all

#define shmem_short_xor_to_all       pshmem_short_xor_to_all
#define shmem_int_xor_to_all         pshmem_int_xor_to_all
#define shmem_long_xor_to_all        pshmem_long_xor_to_all
#define shmem_longlong_xor_to_all    pshmem_longlong_xor_to_all
#define shmemx_int16_xor_to_all      pshmemx_int16_xor_to_all
#define shmemx_int32_xor_to_all      pshmemx_int32_xor_to_all
#define shmemx_int64_xor_to_all      pshmemx_int64_xor_to_all

#define shmem_short_max_to_all       pshmem_short_max_to_all
#define shmem_int_max_to_all         pshmem_int_max_to_all
#define shmem_long_max_to_all        pshmem_long_max_to_all
#define shmem_longlong_max_to_all    pshmem_longlong_max_to_all
#define shmem_float_max_to_all       pshmem_float_max_to_all
#define shmem_double_max_to_all      pshmem_double_max_to_all
#define shmem_longdouble_max_to_all  pshmem_longdouble_max_to_all
#define shmemx_int16_max_to_all      pshmemx_int16_max_to_all
#define shmemx_int32_max_to_all      pshmemx_int32_max_to_all
#define shmemx_int64_max_to_all      pshmemx_int64_max_to_all

#define shmem_short_min_to_all       pshmem_short_min_to_all
#define shmem_int_min_to_all         pshmem_int_min_to_all
#define shmem_long_min_to_all        pshmem_long_min_to_all
#define shmem_longlong_min_to_all    pshmem_longlong_min_to_all
#define shmem_float_min_to_all       pshmem_float_min_to_all
#define shmem_double_min_to_all      pshmem_double_min_to_all
#define shmem_longdouble_min_to_all  pshmem_longdouble_min_to_all
#define shmemx_int16_min_to_all      pshmemx_int16_min_to_all
#define shmemx_int32_min_to_all      pshmemx_int32_min_to_all
#define shmemx_int64_min_to_all      pshmemx_int64_min_to_all

#define shmem_short_sum_to_all       pshmem_short_sum_to_all
#define shmem_int_sum_to_all         pshmem_int_sum_to_all
#define shmem_long_sum_to_all        pshmem_long_sum_to_all
#define shmem_longlong_sum_to_all    pshmem_longlong_sum_to_all
#define shmem_float_sum_to_all       pshmem_float_sum_to_all
#define shmem_double_sum_to_all      pshmem_double_sum_to_all
#define shmem_longdouble_sum_to_all  pshmem_longdouble_sum_to_all
#define shmem_complexf_sum_to_all    pshmem_complexf_sum_to_all
#define shmem_complexd_sum_to_all    pshmem_complexd_sum_to_all
#define shmemx_int16_sum_to_all      pshmemx_int16_sum_to_all
#define shmemx_int32_sum_to_all      pshmemx_int32_sum_to_all
#define shmemx_int64_sum_to_all      pshmemx_int64_sum_to_all

#define shmem_short_prod_to_all      pshmem_short_prod_to_all
#define shmem_int_prod_to_all        pshmem_int_prod_to_all
#define shmem_long_prod_to_all       pshmem_long_prod_to_all
#define shmem_longlong_prod_to_all   pshmem_longlong_prod_to_all
#define shmem_float_prod_to_all      pshmem_float_prod_to_all
#define shmem_double_prod_to_all     pshmem_double_prod_to_all
#define shmem_longdouble_prod_to_all pshmem_longdouble_prod_to_all
#define shmem_complexf_prod_to_all   pshmem_complexf_prod_to_all
#define shmem_complexd_prod_to_all   pshmem_complexd_prod_to_all
#define shmemx_int16_prod_to_all     pshmemx_int16_prod_to_all
#define shmemx_int32_prod_to_all     pshmemx_int32_prod_to_all
#define shmemx_int64_prod_to_all     pshmemx_int64_prod_to_all

/*
 * Alltoall routines
 */
#define shmem_alltoall32             pshmem_alltoall32
#define shmem_alltoall64             pshmem_alltoall64
#define shmem_alltoalls32            pshmem_alltoalls32
#define shmem_alltoalls64            pshmem_alltoalls64

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
