/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>

#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_constructors.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/runtime/opal.h"
#include "opal/util/arch.h"
#include "opal/util/output.h"

/* by default the debugging is turned off */
int opal_datatype_dfd = -1;
bool opal_ddt_unpack_debug = false;
bool opal_ddt_pack_debug = false;
bool opal_ddt_position_debug = false;
bool opal_ddt_copy_debug = false;
bool opal_ddt_raw_debug = false;
int opal_ddt_verbose = -1; /* Has the datatype verbose it's own output stream */
/* The missed-optimization check walks optimized descriptions and is meant for tuning diagnostics. */
bool opal_datatype_check_missed_optimizations = false;

/* Default pack/unpack/optimize policy thresholds, tunable at launch through the expert MCA
 * parameters registered below (see opal_datatype_register_params).
 *
 * These are a provisional cost model, not a proven optimum: the values were measured on an Apple
 * M3 Pro and revalidated on an Intel Xeon Platinum 8580, and are almost certainly not optimal on
 * other microarchitectures. Do not change them without vetting on the target hardware with
 * contrib/datatype/tune_datatype.py, which drives the ompi/test/datatype/pack_description_sweep
 * benchmark that produces candidate values. */
opal_datatype_config_t opal_datatype_config = {
    .pack = {
        .max_vectorized_blocklen = 64,
        .l1_cache_lines = 512,
        /* On x86 a tuned memcpy beats the runtime-count typed loop for small gappy blocks, so
         * default the gate on; other microarchitectures (e.g. arm64, where the typed loop wins)
         * default it off. Retune per target with contrib/datatype/tune_datatype.py. */
#if defined(__x86_64__) || defined(__i386__) || defined(_M_X64) || defined(_M_IX86)
        .min_typed_block_bytes = 64,
#else
        .min_typed_block_bytes = 0,
#endif
    },
    .unpack = {
        .max_vectorized_block_bytes = 1024,
        .always_typed_block_bytes = 64,
        .compact_memcpy_max_bytes = 256,
        .min_scatter_gap_bytes = 96,
        .small_fragment_bytes = 4 * 1024,
        .large_fragment_bytes = 128 * 1024,
    },
    .optimize = {
        .max_desc_growth = 10,
        .loop_unroll_max_items = 8,
        .loop_unroll_max_data_bytes = 128,
        .preserve_type = true,
    },
};

/* Using this macro implies that at this point _all_ information needed
 * to fill up the datatype are known.
 * We fill all the static information, the pointer to desc.desc is setup
 * into an array, which is initialized at runtime.
 * Everything is constant.
 */
OPAL_DECLSPEC const opal_datatype_t opal_datatype_empty = OPAL_DATATYPE_INITIALIZER_EMPTY(
    OPAL_DATATYPE_FLAG_CONTIGUOUS);

OPAL_DECLSPEC const opal_datatype_t opal_datatype_loop = OPAL_DATATYPE_INITIALIZER_LOOP(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_end_loop = OPAL_DATATYPE_INITIALIZER_END_LOOP(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_lb = OPAL_DATATYPE_INITIALIZER_LB(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_ub = OPAL_DATATYPE_INITIALIZER_UB(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int1 = OPAL_DATATYPE_INITIALIZER_INT1(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int2 = OPAL_DATATYPE_INITIALIZER_INT2(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int4 = OPAL_DATATYPE_INITIALIZER_INT4(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int8 = OPAL_DATATYPE_INITIALIZER_INT8(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_int16 = OPAL_DATATYPE_INITIALIZER_INT16(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint1 = OPAL_DATATYPE_INITIALIZER_UINT1(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint2 = OPAL_DATATYPE_INITIALIZER_UINT2(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint4 = OPAL_DATATYPE_INITIALIZER_UINT4(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint8 = OPAL_DATATYPE_INITIALIZER_UINT8(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_uint16 = OPAL_DATATYPE_INITIALIZER_UINT16(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float2 = OPAL_DATATYPE_INITIALIZER_FLOAT2(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float4 = OPAL_DATATYPE_INITIALIZER_FLOAT4(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float8 = OPAL_DATATYPE_INITIALIZER_FLOAT8(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float12 = OPAL_DATATYPE_INITIALIZER_FLOAT12(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float16 = OPAL_DATATYPE_INITIALIZER_FLOAT16(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_short_float_complex
    = OPAL_DATATYPE_INITIALIZER_SHORT_FLOAT_COMPLEX(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float_complex
    = OPAL_DATATYPE_INITIALIZER_FLOAT_COMPLEX(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_double_complex
    = OPAL_DATATYPE_INITIALIZER_DOUBLE_COMPLEX(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_long_double_complex
    = OPAL_DATATYPE_INITIALIZER_LONG_DOUBLE_COMPLEX(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_float128_complex
    = OPAL_DATATYPE_INITIALIZER_FLOAT128_COMPLEX(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_bool = OPAL_DATATYPE_INITIALIZER_BOOL(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_wchar = OPAL_DATATYPE_INITIALIZER_WCHAR(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_long =  OPAL_DATATYPE_INITIALIZER_LONG(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_unsigned_long =  OPAL_DATATYPE_INITIALIZER_UNSIGNED_LONG(0);
OPAL_DECLSPEC const opal_datatype_t opal_datatype_unavailable
    = OPAL_DATATYPE_INITIALIZER_UNAVAILABLE_NAMED(UNAVAILABLE, 0);

OPAL_DECLSPEC dt_elem_desc_t opal_datatype_predefined_elem_desc[2 * OPAL_DATATYPE_MAX_PREDEFINED]
    = {{{{0}}}};
/*
 * NOTE: The order of this array *MUST* match the order in opal_datatype_basicDatatypes
 * (use of designated initializers should relax this restrictions some)
 */
OPAL_DECLSPEC const size_t opal_datatype_local_sizes[OPAL_DATATYPE_MAX_PREDEFINED] = {
    [OPAL_DATATYPE_INT1] = sizeof(int8_t),
    [OPAL_DATATYPE_INT2] = sizeof(int16_t),
    [OPAL_DATATYPE_INT4] = sizeof(int32_t),
    [OPAL_DATATYPE_INT8] = sizeof(int64_t),
    [OPAL_DATATYPE_INT16] = 16,                    /* sizeof (int128_t) */
    [OPAL_DATATYPE_UINT1] = sizeof(uint8_t),
    [OPAL_DATATYPE_UINT2] = sizeof(uint16_t),
    [OPAL_DATATYPE_UINT4] = sizeof(uint32_t),
    [OPAL_DATATYPE_UINT8] = sizeof(uint64_t),
    [OPAL_DATATYPE_UINT16] = 16,                   /* sizeof (uint128_t) */
    [OPAL_DATATYPE_FLOAT2] = 2,                    /* sizeof (float2) */
    [OPAL_DATATYPE_FLOAT4] = 4,                    /* sizeof (float4) */
    [OPAL_DATATYPE_FLOAT8] = 8,                    /* sizeof (float8) */
    [OPAL_DATATYPE_FLOAT12] = OPAL_SIZEOF_FLOAT12, /* sizeof (float12) */
    [OPAL_DATATYPE_FLOAT16] = OPAL_SIZEOF_FLOAT16, /* sizeof (float16) */
#if defined(HAVE_SHORT_FLOAT__COMPLEX)
    [OPAL_DATATYPE_SHORT_FLOAT_COMPLEX] = sizeof(short float _Complex),
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
    [OPAL_DATATYPE_SHORT_FLOAT_COMPLEX] = sizeof(opal_short_float_complex_t),
#else
    [OPAL_DATATYPE_SHORT_FLOAT_COMPLEX] = 4, /* typical sizeof(short float _Complex) */
#endif
    [OPAL_DATATYPE_FLOAT_COMPLEX] = sizeof(float _Complex),
    [OPAL_DATATYPE_DOUBLE_COMPLEX] = sizeof(double _Complex),
    [OPAL_DATATYPE_LONG_DOUBLE_COMPLEX] = sizeof(long double _Complex),
    [OPAL_DATATYPE_BOOL] = sizeof(_Bool),
    [OPAL_DATATYPE_WCHAR] = sizeof(wchar_t),
    [OPAL_DATATYPE_LONG] = sizeof(long),
    [OPAL_DATATYPE_UNSIGNED_LONG] = sizeof(unsigned long),
#if defined(HAVE__FLOAT128) && defined(HAVE__FLOAT128__COMPLEX)
    [OPAL_DATATYPE_FLOAT128_COMPLEX] = sizeof(_Float128 _Complex),
#elif defined(HAVE___FLOAT128) && defined(HAVE___FLOAT128__COMPLEX)
    [OPAL_DATATYPE_FLOAT128_COMPLEX] = sizeof(__float128 _Complex),
#else
    [OPAL_DATATYPE_FLOAT128_COMPLEX] = 32, /* typical sizeof(_Float128 _Complex) */
#endif
};

/* The partial-element staging buffers in opal_unpack_partial_predefined() (opal_datatype_unpack.h)
 * are sized to OPAL_DATATYPE_MAX_PREDEFINED_SIZE. Guarantee at compile time that the widest
 * predefined types copied through them still fit; if a wider predefined type is ever added to the
 * table above, bump OPAL_DATATYPE_MAX_PREDEFINED_SIZE to match. */
_Static_assert(sizeof(long double _Complex) <= OPAL_DATATYPE_MAX_PREDEFINED_SIZE,
               "OPAL_DATATYPE_MAX_PREDEFINED_SIZE too small for long double _Complex");
_Static_assert(OPAL_SIZEOF_FLOAT16 <= OPAL_DATATYPE_MAX_PREDEFINED_SIZE,
               "OPAL_DATATYPE_MAX_PREDEFINED_SIZE too small for FLOAT16");
#if defined(HAVE__FLOAT128) && defined(HAVE__FLOAT128__COMPLEX)
_Static_assert(sizeof(_Float128 _Complex) <= OPAL_DATATYPE_MAX_PREDEFINED_SIZE,
               "OPAL_DATATYPE_MAX_PREDEFINED_SIZE too small for _Float128 _Complex");
#elif defined(HAVE___FLOAT128) && defined(HAVE___FLOAT128__COMPLEX)
_Static_assert(sizeof(__float128 _Complex) <= OPAL_DATATYPE_MAX_PREDEFINED_SIZE,
               "OPAL_DATATYPE_MAX_PREDEFINED_SIZE too small for __float128 _Complex");
#endif

/*
 * NOTE: The order of this array *MUST* match what is listed in datatype.h
 * (use of designated initializers should relax this restrictions some)
 */
OPAL_DECLSPEC const opal_datatype_t *opal_datatype_basicDatatypes[OPAL_DATATYPE_MAX_PREDEFINED] = {
    [OPAL_DATATYPE_LOOP] = &opal_datatype_loop,
    [OPAL_DATATYPE_END_LOOP] = &opal_datatype_end_loop,
    [OPAL_DATATYPE_LB] = &opal_datatype_lb,
    [OPAL_DATATYPE_UB] = &opal_datatype_ub,
    [OPAL_DATATYPE_INT1] = &opal_datatype_int1,
    [OPAL_DATATYPE_INT2] = &opal_datatype_int2,
    [OPAL_DATATYPE_INT4] = &opal_datatype_int4,
    [OPAL_DATATYPE_INT8] = &opal_datatype_int8,
    [OPAL_DATATYPE_INT16] = &opal_datatype_int16, /* Yes, double-machine word integers are available
                                                   */
    [OPAL_DATATYPE_UINT1] = &opal_datatype_uint1,
    [OPAL_DATATYPE_UINT2] = &opal_datatype_uint2,
    [OPAL_DATATYPE_UINT4] = &opal_datatype_uint4,
    [OPAL_DATATYPE_UINT8] = &opal_datatype_uint8,
    [OPAL_DATATYPE_UINT16] = &opal_datatype_uint16, /* Yes, double-machine word integers are
                                                       available */
    [OPAL_DATATYPE_FLOAT2] = &opal_datatype_float2,
    [OPAL_DATATYPE_FLOAT4] = &opal_datatype_float4,
    [OPAL_DATATYPE_FLOAT8] = &opal_datatype_float8,
    [OPAL_DATATYPE_FLOAT12] = &opal_datatype_float12,
    [OPAL_DATATYPE_FLOAT16] = &opal_datatype_float16,
    [OPAL_DATATYPE_SHORT_FLOAT_COMPLEX] = &opal_datatype_short_float_complex,
    [OPAL_DATATYPE_FLOAT_COMPLEX] = &opal_datatype_float_complex,
    [OPAL_DATATYPE_DOUBLE_COMPLEX] = &opal_datatype_double_complex,
    [OPAL_DATATYPE_LONG_DOUBLE_COMPLEX] = &opal_datatype_long_double_complex,
    [OPAL_DATATYPE_BOOL] = &opal_datatype_bool,
    [OPAL_DATATYPE_WCHAR] = &opal_datatype_wchar,
    [OPAL_DATATYPE_LONG] = &opal_datatype_long,
    [OPAL_DATATYPE_UNSIGNED_LONG] = &opal_datatype_unsigned_long,
    [OPAL_DATATYPE_FLOAT128_COMPLEX] = &opal_datatype_float128_complex,
    [OPAL_DATATYPE_UNAVAILABLE] = &opal_datatype_unavailable,
};

int opal_datatype_register_params(void)
{
    int ret;

#if OPAL_ENABLE_DEBUG
    ret = mca_base_var_register(
        "opal", "mpi", NULL, "ddt_unpack_debug",
        "Whether to output debugging information in the ddt unpack functions (nonzero = enabled)",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &opal_ddt_unpack_debug);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register(
        "opal", "mpi", NULL, "ddt_pack_debug",
        "Whether to output debugging information in the ddt pack functions (nonzero = enabled)",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &opal_ddt_pack_debug);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register(
        "opal", "mpi", NULL, "ddt_raw_debug",
        "Whether to output debugging information in the ddt raw functions (nonzero = enabled)",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &opal_ddt_raw_debug);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register(
        "opal", "mpi", NULL, "ddt_position_debug",
        "Non zero lead to output generated by the datatype position functions",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &opal_ddt_position_debug);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register(
        "opal", "mpi", NULL, "ddt_copy_debug",
        "Whether to output debugging information in the ddt copy functions (nonzero = enabled)",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &opal_ddt_copy_debug);
    if (0 > ret) {
        return ret;
    }

    ret = mca_base_var_register("opal", "opal", NULL, "ddt_verbose",
                                "Set level of opal datatype verbosity", MCA_BASE_VAR_TYPE_INT, NULL,
                                0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_8,
                                MCA_BASE_VAR_SCOPE_LOCAL, &opal_ddt_verbose);
    if (0 > ret) {
        return ret;
    }

#endif /* OPAL_ENABLE_DEBUG */

    ret = mca_base_var_register(
        "opal", "opal", NULL, "datatype_check_missed_optimizations",
        "Whether to scan committed datatypes for copy ranges that the optimizer did not merge. "
        "The report is written to the datatype output stream at verbosity 10, so it also requires "
        "opal_ddt_verbose >= 10 to be visible",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_9,
        MCA_BASE_VAR_SCOPE_LOCAL, &opal_datatype_check_missed_optimizations);
    if (0 > ret) {
        return ret;
    }

    /* Expert pack/unpack/optimize tuning thresholds. These gate the medium-block typed
     * movers and the descriptor optimizer and are a provisional cost model (see
     * opal_datatype_internal.h); the pack/unpack ones are read once per block run (not
     * per element) and the optimize ones at commit time, so the runtime indirection is
     * off the hot path. Exposed at OPAL_INFO_LVL_9 because retuning them requires the
     * dedicated benchmark (contrib/datatype/tune_datatype.py), not guesswork. */
    static const struct {
        const char *name;
        const char *help;
        size_t *storage;
    } thresholds[] = {
        {"datatype_pack_max_vectorized_blocklen",
         "Expert tuning: element block length (in elements) above which the pack engine stops "
         "using the medium-block typed copy loop and falls back to the predefined mover",
         &opal_datatype_config.pack.max_vectorized_blocklen},
        {"datatype_pack_l1_cache_lines",
         "Expert tuning: estimated number of L1 data cache lines; combined with the runtime cache "
         "line size to decide when a pack fragment is too large for the typed copy loop",
         &opal_datatype_config.pack.l1_cache_lines},
        {"datatype_pack_min_typed_block_bytes",
         "Expert tuning: contiguous per-block size (bytes) below which a strided/gappy pack uses "
         "the byte memcpy path instead of the typed inline mover (0 disables the gate)",
         &opal_datatype_config.pack.min_typed_block_bytes},
        {"datatype_unpack_max_vectorized_block_bytes",
         "Expert tuning: maximum block size (bytes) eligible for the medium-block typed unpack loop",
         &opal_datatype_config.unpack.max_vectorized_block_bytes},
        {"datatype_unpack_always_typed_block_bytes",
         "Expert tuning: block size (bytes) at or below which the typed unpack loop is always used",
         &opal_datatype_config.unpack.always_typed_block_bytes},
        {"datatype_unpack_compact_memcpy_max_bytes",
         "Expert tuning: upper block-size bound (bytes) of the compact-scatter region routed to the "
         "callback mover",
         &opal_datatype_config.unpack.compact_memcpy_max_bytes},
        {"datatype_unpack_min_scatter_gap_bytes",
         "Expert tuning: minimum inter-block gap (bytes) below which a medium unpack block is "
         "treated as a compact scatter",
         &opal_datatype_config.unpack.min_scatter_gap_bytes},
        {"datatype_unpack_small_fragment_bytes",
         "Expert tuning: lower iovec-length bound (bytes) of the compact-scatter callback region",
         &opal_datatype_config.unpack.small_fragment_bytes},
        {"datatype_unpack_large_fragment_bytes",
         "Expert tuning: upper iovec-length bound (bytes) of the compact-scatter callback region",
         &opal_datatype_config.unpack.large_fragment_bytes},
        {"datatype_optimize_max_desc_growth",
         "Expert tuning: maximum factor by which the descriptor optimizer may grow a datatype "
         "description (relative to its committed entry count) while unrolling loop boundaries",
         &opal_datatype_config.optimize.max_desc_growth},
        {"datatype_optimize_loop_unroll_max_items",
         "Expert tuning: maximum loop-body item count the optimizer will unroll; loops with more "
         "body items are left as-is",
         &opal_datatype_config.optimize.loop_unroll_max_items},
        {"datatype_optimize_loop_unroll_max_data_bytes",
         "Expert tuning: maximum per-item byte size the optimizer considers cheap enough to unroll",
         &opal_datatype_config.optimize.loop_unroll_max_data_bytes},
    };
    for (size_t i = 0; i < sizeof(thresholds) / sizeof(thresholds[0]); ++i) {
        ret = mca_base_var_register("opal", "opal", NULL, thresholds[i].name, thresholds[i].help,
                                    MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0, OPAL_INFO_LVL_9,
                                    MCA_BASE_VAR_SCOPE_LOCAL, thresholds[i].storage);
        if (0 > ret) {
            return ret;
        }
    }

    ret = mca_base_var_register(
        "opal", "opal", NULL, "datatype_optimize_preserve_type",
        "Expert tuning: when true, the descriptor optimizer copies a fused mixed-type region "
        "through the widest safe unsigned integer type; when false it degrades such regions to "
        "byte (UINT1) copies. Read at datatype-commit time",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
        &opal_datatype_config.optimize.preserve_type);
    if (0 > ret) {
        return ret;
    }

    return OPAL_SUCCESS;
}

static void opal_datatype_finalize(void)
{
    /* As the synonyms are just copies of the internal data we should not free them.
     * Anyway they are over the limit of OPAL_DATATYPE_MAX_PREDEFINED so they will never get freed.
     */

    /* As they are statically allocated they cannot be released. But we
     * can call OBJ_DESTRUCT, just to free all internally allocated resources.
     */
    /* clear all master convertors */
    opal_convertor_destroy_masters();

    opal_output_close(opal_datatype_dfd);
    opal_datatype_dfd = -1;
}

int32_t opal_datatype_init(void)
{
    const opal_datatype_t *datatype;
    int32_t i;

    /**
     * Force the initialization of the opal_datatype_t class. This will allow us to
     * call OBJ_DESTRUCT without going too deep in the initialization process.
     */
    opal_class_initialize(OBJ_CLASS(opal_datatype_t));
    for (i = OPAL_DATATYPE_FIRST_TYPE; i < OPAL_DATATYPE_MAX_PREDEFINED; i++) {
        datatype = opal_datatype_basicDatatypes[i];

        /* All of the predefined OPAL types don't have any GAPS! */
        datatype->desc.desc[0].elem.common.flags = OPAL_DATATYPE_FLAG_PREDEFINED
                                                   | OPAL_DATATYPE_FLAG_DATA
                                                   | OPAL_DATATYPE_FLAG_CONTIGUOUS
                                                   | OPAL_DATATYPE_FLAG_NO_GAPS;
        datatype->desc.desc[0].elem.common.type = i;
        datatype->desc.desc[0].elem.count = 1;
        datatype->desc.desc[0].elem.blocklen = 1;
        datatype->desc.desc[0].elem.disp = 0;
        datatype->desc.desc[0].elem.extent = datatype->size;

        datatype->desc.desc[1].end_loop.common.flags = 0;
        datatype->desc.desc[1].end_loop.common.type = OPAL_DATATYPE_END_LOOP;
        datatype->desc.desc[1].end_loop.items = 1;
        datatype->desc.desc[1].end_loop.first_elem_disp = datatype->desc.desc[0].elem.disp;
        datatype->desc.desc[1].end_loop.size = datatype->size;
    }

    /* Enable a private output stream for datatype */
    if (opal_ddt_verbose > 0) {
        opal_datatype_dfd = opal_output_open(NULL);
        opal_output_set_verbosity(opal_datatype_dfd, opal_ddt_verbose);
    }

    opal_finalize_register_cleanup(opal_datatype_finalize);

    /* Sanity check*/
    assert(opal_datatype_local_sizes[OPAL_DATATYPE_FLOAT12] == opal_datatype_float12.size);

    return OPAL_SUCCESS;
}

#if OPAL_ENABLE_DEBUG
/*
 * Set a breakpoint to this function in your favorite debugger
 * to make it stop on all pack and unpack errors.
 */
int opal_datatype_safeguard_pointer_debug_breakpoint(const void *actual_ptr, int length,
                                                     const void *initial_ptr,
                                                     const opal_datatype_t *pData, int count)
{
    return 0;
}
#endif /* OPAL_ENABLE_DEBUG */
