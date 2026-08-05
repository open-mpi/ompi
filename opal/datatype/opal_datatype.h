/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2026      Stony Brook University.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * opal_datatype_t interface for OPAL internal data type representation
 *
 * opal_datatype_t is a class which represents contiguous or
 * non-contiguous data together with constituent type-related
 * information.
 */

#ifndef OPAL_DATATYPE_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_H_HAS_BEEN_INCLUDED

#include "opal_config.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "opal/class/opal_object.h"

BEGIN_C_DECLS

/*
 * If there are more basic datatypes than the number of bytes in the int type
 * the bdt_used field of the data description struct should be changed to long.
 *
 * This must match the same definition as in opal_datatype_internal.h
 */
#if !defined(OPAL_DATATYPE_MAX_PREDEFINED)
#    define OPAL_DATATYPE_MAX_PREDEFINED 29
#endif
/*
 * Size (in bytes) of the largest predefined datatype element. The partial-element staging buffers
 * in opal_unpack_partial_predefined() (opal_datatype_unpack.h) are sized to this bound so a
 * fragment boundary landing inside a predefined element can be staged on the stack. The widest
 * predefined types are the 32-byte complex forms (long double _Complex, _Float128 _Complex); a
 * _Static_assert next to opal_datatype_local_sizes[] in opal_datatype_module.c enforces at compile
 * time that no predefined type exceeds this value.
 */
#define OPAL_DATATYPE_MAX_PREDEFINED_SIZE 32
/*
 * Upper limit of the number of _Basic_ datatypes supported (in order to
 * not change setup and usage of the predefined datatypes).
 *
 * BEWARE: This constant should reflect whatever the upper layer needs.
 */
#define OPAL_DATATYPE_MAX_SUPPORTED 64

/* flags for the datatypes. */
#define OPAL_DATATYPE_FLAG_UNAVAILABLE \
    0x00000001u /**< datatypes unavailable on the build (OS or compiler dependent) */
#define OPAL_DATATYPE_FLAG_PREDEFINED \
    0x00000002u /**< cannot be removed: initial and predefined datatypes */
#define OPAL_DATATYPE_FLAG_COMMITTED  0x00000004u /**< ready to be used for a send/recv operation */
#define OPAL_DATATYPE_FLAG_OVERLAP    0x00000008u /**< datatype is unpropper for a recv operation */
/*
 * For datatype descriptors this flag describes the whole datatype layout.  A
 * LOOP descriptor may also carry it, but that is a construction artifact: in a
 * homogeneous optimized descriptor such loops should have been collapsed
 * already, and in a heterogeneous path the loop body typemap must still be
 * traversed. Do not use a contiguous LOOP marker as a generic byte-copy
 * optimization.
 */
#define OPAL_DATATYPE_FLAG_CONTIGUOUS 0x00000010u /**< contiguous datatype */
#define OPAL_DATATYPE_FLAG_NO_GAPS    0x00000020u /**< no gaps around the datatype, aka OPAL_DATATYPE_FLAG_CONTIGUOUS and extent == size */
#define OPAL_DATATYPE_FLAG_USER_LB 0x00000040u /**< has a user defined LB */
#define OPAL_DATATYPE_FLAG_USER_UB 0x00000080u /**< has a user defined UB */
#define OPAL_DATATYPE_FLAG_DATA    0x00000100u /**< data or control structure */
/*
 * We should make the difference here between the predefined contiguous and non contiguous
 * datatypes. The OPAL_DATATYPE_FLAG_BASIC is held by all predefined contiguous datatypes.
 */
#define OPAL_DATATYPE_FLAG_BASIC                                                                \
    (OPAL_DATATYPE_FLAG_PREDEFINED | OPAL_DATATYPE_FLAG_CONTIGUOUS | OPAL_DATATYPE_FLAG_NO_GAPS \
     | OPAL_DATATYPE_FLAG_DATA | OPAL_DATATYPE_FLAG_COMMITTED)
/*
 * The per-element descriptor flag field (ddt_elem_id_description.flags in
 * opal_datatype_internal.h) is only 16 bits wide.  opal_datatype_add() copies a predefined type's
 * datatype-level flags down into an element, so only the element-meaningful OPAL flags below may be
 * let through; everything else is masked out on the way in.  By convention any layer above OPAL
 * places its own datatype flags in the top 16 bits of the flags word, so they can never reach an
 * element and are naturally excluded by the mask.  The one datatype-level bit that still lands
 * inside the 16-bit range is the optimizer-private OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED (bit 9); it
 * is intentionally left out of this mask (enforced by a _Static_assert in opal_datatype_add.c) so it
 * is not confused with an incoming element flag.
 */
#define OPAL_DATATYPE_FLAG_ELEM_MASK                                                            \
    (OPAL_DATATYPE_FLAG_UNAVAILABLE | OPAL_DATATYPE_FLAG_PREDEFINED | OPAL_DATATYPE_FLAG_COMMITTED \
     | OPAL_DATATYPE_FLAG_OVERLAP | OPAL_DATATYPE_FLAG_CONTIGUOUS | OPAL_DATATYPE_FLAG_NO_GAPS   \
     | OPAL_DATATYPE_FLAG_USER_LB | OPAL_DATATYPE_FLAG_USER_UB | OPAL_DATATYPE_FLAG_DATA)
/*
 * If during the datatype optimization process we collapse contiguous elements with
 * different types, we cannot use this optimized description for any communication
 * in a heterogeneous setting, especially not for the external32 support.
 *
 * A datatype with this flag cannot use the optimized description in heterogeneous
 * setups.
 *
 * This and OPAL_DATATYPE_FLAG_COUNT_OPTIMIZABLE below are datatype-only "shape hints": they live
 * above bit 15, outside CONVERTOR_DATATYPE_MASK, so OPAL_CONVERTOR_PREPARE() never copies them into
 * a convertor's flags, and they are read only from opal_datatype_t::flags. They occupy the two
 * lowest bits above the data-flag region; the convertor control flags (opal_convertor.h) pack
 * downward from bit 31 and deliberately leave these two free. A _Static_assert there enforces the
 * two sets stay disjoint, so a mistaken test of the wrong field cannot read as an active convertor
 * bit.
 */
#define OPAL_DATATYPE_OPTIMIZED_RESTRICTED 0x00010000u
/*
 * The last fragment of one full datatype instance is adjacent to the first
 * fragment of the next instance. Commit-time optimization cannot fold this into
 * a count-1 description, but convertor setup can use the hint for count > 1.
 */
#define OPAL_DATATYPE_FLAG_COUNT_OPTIMIZABLE 0x00020000u

/*
 * Optimization controls.  Keep these as a mask so upper layers can disable
 * transformations whose cost model differs for a given pack/unpack path.
 */
#define OPAL_DATATYPE_OPTIMIZE_ADJACENT_FUSION 0x00000001u
#define OPAL_DATATYPE_OPTIMIZE_LOOP_BOUNDARY   0x00000002u
#define OPAL_DATATYPE_OPTIMIZE_LOOP_UNROLL     0x00000004u
#define OPAL_DATATYPE_OPTIMIZE_ALL             UINT32_MAX

/**
 * The number of supported entries in the data-type definition and the
 * associated type.
 */
#define MAX_DT_COMPONENT_COUNT UINT_MAX
typedef size_t opal_datatype_count_t;

typedef union dt_elem_desc dt_elem_desc_t;

struct dt_type_desc_t {
    opal_datatype_count_t length; /**< the maximum number of elements in the description array */
    opal_datatype_count_t used;   /**< the number of used elements in the description array */
    dt_elem_desc_t *desc;
};
typedef struct dt_type_desc_t dt_type_desc_t;

/*
 * The datatype description.
 */
struct opal_datatype_t {
    opal_object_t super; /**< basic superclass */
    uint32_t flags;      /**< datatype flags, including shape hints above bit 15 */
    uint32_t bdt_used;   /**< bitset of which basic datatypes are used in the data description */
    size_t size;         /**< total size in bytes of the memory used by the data if
                              the data is put on a contiguous buffer */
    ptrdiff_t true_lb;   /**< the true lb of the data without user defined lb and ub */
    ptrdiff_t true_ub;   /**< the true ub of the data without user defined lb and ub */
    ptrdiff_t lb;        /**< lower bound in memory */
    ptrdiff_t ub;        /**< upper bound in memory */
    /* --- cacheline 1 boundary (64 bytes) --- */
    size_t nbElems; /**< total number of elements inside the datatype */
    uint16_t id;    /**< data id, normally the index in the data array. */
    uint16_t align; /**< data should be aligned to */
    uint32_t loops; /**< number of loops on the internal type stack */

    /* Attribute fields */
    char name[OPAL_MAX_OBJECT_NAME]; /**< name of the datatype */
    dt_type_desc_t desc;             /**< the data description */
    dt_type_desc_t opt_desc; /**< short description of the data used when conversion is useless
                                  or in the send case (without conversion) */

    size_t *ptypes; /**< array of basic predefined types that facilitate the computing
                         of the remote size in heterogeneous environments. The length of the
                         array is dependent on the maximum number of predefined datatypes of
                         all language interfaces (because Fortran is not known at the OPAL
                         layer). This field should never be initialized in homogeneous
                         environments */

    /*
     * Layout note for the default LP64 configuration with
     * OPAL_MAX_OBJECT_NAME == 64: this struct is 200 bytes, aligned to 8
     * bytes, and ptypes ends at offset 200.  The 32-bit flags field is paid
     * for by keeping id and align as adjacent 16-bit fields, so there is no
     * trailing padding in the C object to reuse; the object only occupies the
     * first 8 bytes of its fourth 64-byte cacheline.
     */
};

typedef struct opal_datatype_t opal_datatype_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_datatype_t);

OPAL_DECLSPEC extern const opal_datatype_t
    *opal_datatype_basicDatatypes[OPAL_DATATYPE_MAX_PREDEFINED];
OPAL_DECLSPEC extern const size_t opal_datatype_local_sizes[OPAL_DATATYPE_MAX_PREDEFINED];

/* Local Architecture as provided by opal_arch_compute_local_id() */
OPAL_DECLSPEC extern uint32_t opal_local_arch;

/*
 * The OPAL-layer's Basic datatypes themselves.
 */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_empty;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_loop;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_end_loop;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_lb;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_ub;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int1;    /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int2;    /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int4;    /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int8;    /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_int16;   /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint1;   /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint2;   /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint4;   /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint8;   /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_uint16;  /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float2;  /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float4;  /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float8;  /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float12; /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float16; /* in bytes */
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_short_float_complex;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float_complex;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_double_complex;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_long_double_complex;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_float128_complex;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_bool;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_wchar;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_long;
OPAL_DECLSPEC extern const opal_datatype_t opal_datatype_unsigned_long;

/*
 * Functions exported externally
 */
int opal_datatype_register_params(void);
OPAL_DECLSPEC int32_t opal_datatype_init(void);
OPAL_DECLSPEC opal_datatype_t *opal_datatype_create(ssize_t expectedSize);
OPAL_DECLSPEC int32_t opal_datatype_create_desc(opal_datatype_t *datatype, ssize_t expectedSize);
OPAL_DECLSPEC int32_t opal_datatype_commit(opal_datatype_t *pData);
OPAL_DECLSPEC int32_t opal_datatype_destroy(opal_datatype_t **);
OPAL_DECLSPEC int32_t opal_datatype_is_monotonic(opal_datatype_t *type);

static inline int32_t opal_datatype_is_committed(const opal_datatype_t *type)
{
    return ((type->flags & OPAL_DATATYPE_FLAG_COMMITTED) == OPAL_DATATYPE_FLAG_COMMITTED);
}

static inline int32_t opal_datatype_is_overlapped(const opal_datatype_t *type)
{
    return ((type->flags & OPAL_DATATYPE_FLAG_OVERLAP) == OPAL_DATATYPE_FLAG_OVERLAP);
}

static inline int32_t opal_datatype_is_valid(const opal_datatype_t *type)
{
    return !((type->flags & OPAL_DATATYPE_FLAG_UNAVAILABLE) == OPAL_DATATYPE_FLAG_UNAVAILABLE);
}

static inline int32_t opal_datatype_is_predefined(const opal_datatype_t *type)
{
    return (type->flags & OPAL_DATATYPE_FLAG_PREDEFINED);
}

/*
 * This function return true (1) if the datatype representation depending on the count
 * is contiguous in the memory. And false (0) otherwise.
 */
static inline int32_t opal_datatype_is_contiguous_memory_layout(const opal_datatype_t *datatype,
                                                                int32_t count)
{
    if (!(datatype->flags & OPAL_DATATYPE_FLAG_CONTIGUOUS))
        return 0;
    if ((count == 1) || (datatype->flags & OPAL_DATATYPE_FLAG_NO_GAPS))
        return 1;
    return 0;
}

OPAL_DECLSPEC void opal_datatype_dump(const opal_datatype_t *pData);

/* data creation functions */

/**
 * Create a duplicate of the source datatype.
 */
OPAL_DECLSPEC int32_t opal_datatype_clone(const opal_datatype_t *src_type,
                                          opal_datatype_t *dest_type);
/**
 * A contiguous array of identical datatypes.
 */
OPAL_DECLSPEC int32_t opal_datatype_create_contiguous(int count, const opal_datatype_t *oldType,
                                                      opal_datatype_t **newType);
/*
 * Build the optimized descriptor for a datatype created by
 * opal_datatype_create_contiguous(count, oldType), using oldType's already
 * optimized descriptor as the loop body.  The regular descriptor is left as
 * the true contiguous-constructor description.  optimization_mask controls
 * which transforms are allowed while building the optimized descriptor.
 */
OPAL_DECLSPEC int32_t opal_datatype_optimize_from_contiguous(opal_datatype_t *pData,
                                                             const opal_datatype_t *oldType,
                                                             size_t count,
                                                             uint32_t optimization_mask);
/**
 * Add a new datatype to the base type description. The count is the number
 * repetitions of the same element to be added, and the extent is the extent
 * of each element. The displacement is the initial displacement of the
 * first element.
 */
OPAL_DECLSPEC int32_t opal_datatype_add(opal_datatype_t *pdtBase, const opal_datatype_t *pdtAdd,
                                        size_t count, ptrdiff_t disp, ptrdiff_t extent);

/**
 * Alter the lb and extent of an existing datatype in place.
 */
OPAL_DECLSPEC int32_t opal_datatype_resize(opal_datatype_t *type, ptrdiff_t lb, ptrdiff_t extent);

static inline int32_t opal_datatype_type_lb(const opal_datatype_t *pData, ptrdiff_t *disp)
{
    *disp = pData->lb;
    return 0;
}

static inline int32_t opal_datatype_type_ub(const opal_datatype_t *pData, ptrdiff_t *disp)
{
    *disp = pData->ub;
    return 0;
}

static inline int32_t opal_datatype_type_size(const opal_datatype_t *pData, size_t *size)
{
    *size = pData->size;
    return 0;
}

static inline int32_t opal_datatype_type_extent(const opal_datatype_t *pData, ptrdiff_t *extent)
{
    *extent = pData->ub - pData->lb;
    return 0;
}

static inline int32_t opal_datatype_get_extent(const opal_datatype_t *pData, ptrdiff_t *lb,
                                               ptrdiff_t *extent)
{
    *lb = pData->lb;
    *extent = pData->ub - pData->lb;
    return 0;
}

static inline int32_t opal_datatype_get_true_extent(const opal_datatype_t *pData,
                                                    ptrdiff_t *true_lb, ptrdiff_t *true_extent)
{
    *true_lb = pData->true_lb;
    *true_extent = (pData->true_ub - pData->true_lb);
    return 0;
}

OPAL_DECLSPEC ssize_t opal_datatype_get_element_count(const opal_datatype_t *pData, size_t iSize);
OPAL_DECLSPEC int32_t opal_datatype_set_element_count(const opal_datatype_t *pData, size_t count,
                                                      size_t *length);
OPAL_DECLSPEC int32_t opal_datatype_copy_content_same_ddt(const opal_datatype_t *pData,
                                                          int32_t count, char *pDestBuf,
                                                          char *pSrcBuf);

OPAL_DECLSPEC int opal_datatype_compute_ptypes(opal_datatype_t *datatype);

/*
 * Compute the size of the datatype using a specific set of predefined type sizes.
 * This function allows to compute the size of a packed buffer without creating
 * a fully fledged specialized convertor for the remote peer.
 */
OPAL_DECLSPEC size_t
opal_datatype_compute_remote_size(const opal_datatype_t *pData,
                                  const size_t *sizes);

/* Compute the span in memory of count datatypes. This function help with temporary
 * memory allocations for receiving already typed data (such as those used for reduce
 * operations). This span is the distance between the minimum and the maximum byte
 * in the memory layout of count datatypes, or in other terms the memory needed to
 * allocate count times the datatype without the gap in the beginning and at the end.
 *
 * Returns: the memory span of count repetition of the datatype, and in the gap
 *          argument, the number of bytes of the gap at the beginning.
 */
static inline ptrdiff_t opal_datatype_span(const opal_datatype_t *pData, size_t count,
                                           ptrdiff_t *gap)
{
    if (OPAL_UNLIKELY(0 == pData->size) || (0 == count)) {
        *gap = 0;
        return 0;
    }
    *gap = pData->true_lb;
    ptrdiff_t extent = (pData->ub - pData->lb);
    ptrdiff_t true_extent = (pData->true_ub - pData->true_lb);
    return true_extent + extent * (count - 1);
}

#if OPAL_ENABLE_DEBUG
/*
 * Set a breakpoint to this function in your favorite debugger
 * to make it stop on all pack and unpack errors.
 */
OPAL_DECLSPEC int opal_datatype_safeguard_pointer_debug_breakpoint(const void *actual_ptr,
                                                                   int length,
                                                                   const void *initial_ptr,
                                                                   const opal_datatype_t *pData,
                                                                   int count);
#endif /* OPAL_ENABLE_DEBUG */

END_C_DECLS
#endif /* OPAL_DATATYPE_H_HAS_BEEN_INCLUDED */
