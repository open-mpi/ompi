/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2023, 2026 Jeffrey M. Squyres.  All rights reserved.
 * Copyright (c) 2024-2026 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/info/info.h"
#include "ompi/instance/instance.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/class/opal_cstring.h"
#include "opal/util/bit_ops.h"
#include "opal/include/opal_stdint.h"

static ompi_info_t *abi_fortran_info_from_user = NULL;

typedef struct {
    int64_t true_value;
    int64_t false_value;
#if HAVE_OPAL_INT128_T
    opal_int128_t true_value128;
    opal_int128_t false_value128;
#else
    int64_t true_value128;
    int64_t false_value128;
#endif
    bool is_set;
    bool using_int128;
} ompi_abi_fortran_user_bool_info_t;

static ompi_abi_fortran_user_bool_info_t user_logicals[5];

static int countbits32(unsigned int n) {
    int count = 0;
    while (n) {
        n &= (n - 1); // Clear the least significant bit set
        count++;
    }
    return count;
}
#if !OMPI_BUILD_FORTRAN_BINDINGS
/*
 * Only the no-Fortran-bindings build registers this finalize callback (it
 * is the sole consumer of abi_fortran_info_from_user via
 * MPI_Abi_set_fortran_info).  Guard the definition so a Fortran-enabled
 * build, which never references it, does not warn about an unused
 * function.
 */
static int ompi_abi_fortran_finalize(void)
{
    if (NULL != abi_fortran_info_from_user) {
        ompi_info_free(&abi_fortran_info_from_user);
        abi_fortran_info_from_user = NULL;
    }
    return OMPI_SUCCESS;
}
#endif /* !OMPI_BUILD_FORTRAN_BINDINGS */
        
OMPI_HIDDEN int ompi_abi_get_fortran_info(ompi_info_t **info)
{
    ompi_info_t *newinfo = NULL;
    int ret = MPI_SUCCESS;
#if OMPI_BUILD_FORTRAN_BINDINGS
    char tmp[16];
    const char true_str[]="true";
    const char false_str[]="false";
    const char *cptr;

    newinfo = ompi_info_allocate ();
    if (NULL == newinfo) {
        return MPI_ERR_NO_MEM;
    }

#if OMPI_SIZEOF_FORTRAN_LOGICAL
    snprintf(tmp, 16, "%d", OMPI_SIZEOF_FORTRAN_LOGICAL);
    ret = opal_info_set(&newinfo->super, "mpi_logical_size", tmp);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
#endif

#if OMPI_SIZEOF_FORTRAN_INTEGER
    snprintf(tmp, 16, "%d", OMPI_SIZEOF_FORTRAN_INTEGER);
    ret = opal_info_set(&newinfo->super, "mpi_integer_size", tmp);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
#endif

#if OMPI_SIZEOF_FORTRAN_REAL
    snprintf(tmp, 16, "%d", OMPI_SIZEOF_FORTRAN_REAL);
    ret = opal_info_set(&newinfo->super, "mpi_real_size", tmp);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
#endif

#if OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION
    snprintf(tmp, 16, "%d", OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION);
    ret = opal_info_set(&newinfo->super, "mpi_double_precision_size", tmp);
    if (OPAL_SUCCESS != ret) {
        goto err_cleanup;
    }
#endif

#define OMPI_DATATYPE_SET_SUPPORTED(type, TYPE) do { \
    cptr = (OMPI_DATATYPE_##TYPE != OMPI_DATATYPE_MPI_UNAVAILABLE) ? true_str : false_str; \
    ret = opal_info_set(&newinfo->super, #type "_supported", cptr); \
    if (OPAL_SUCCESS != ret) goto err_cleanup; } while (0)

    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical1, MPI_LOGICAL1);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical2, MPI_LOGICAL2);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical4, MPI_LOGICAL4);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical8, MPI_LOGICAL8);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_logical16, MPI_LOGICAL16);

    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer1, MPI_INTEGER1);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer2, MPI_INTEGER2);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer4, MPI_INTEGER4);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer8, MPI_INTEGER8);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_integer16, MPI_INTEGER16);

    OMPI_DATATYPE_SET_SUPPORTED(mpi_real2, MPI_REAL2);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_real4, MPI_REAL4);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_real8, MPI_REAL8);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_real16, MPI_REAL16);

    OMPI_DATATYPE_SET_SUPPORTED(mpi_complex4, MPI_COMPLEX4);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_complex8, MPI_COMPLEX8);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_complex16, MPI_COMPLEX16);
    OMPI_DATATYPE_SET_SUPPORTED(mpi_complex32, MPI_COMPLEX32);

    OMPI_DATATYPE_SET_SUPPORTED(mpi_double_complex, MPI_DOUBLE_COMPLEX);

#undef OMPI_DATATYPE_SET_SUPPORTED
    
    *info = newinfo;

#else

    /*
     * If the user previously set fortran info via
     * MPI_Abi_set_fortran_info, dup it into the returned info.
     * Otherwise the implementation does not know the Fortran
     * properties, so per the MPI-5.0 standard return MPI_INFO_NULL
     * to tell the application it must set them itself.
     */
    if (NULL != abi_fortran_info_from_user) {
        newinfo = ompi_info_allocate();
        if (NULL == newinfo) {
            ret = MPI_ERR_NO_MEM;
            goto err_cleanup;
        }
        ret = ompi_info_dup(abi_fortran_info_from_user, &newinfo);
        if (OPAL_SUCCESS != ret) {
            goto err_cleanup;
        }
        *info = newinfo;
    } else {
        *info = MPI_INFO_NULL;
    }

#endif /*  OMPI_BUILD_FORTRAN_BINDINGS */

    return ret;

err_cleanup:
    if (NULL != newinfo) {
        ompi_info_free(&newinfo);
    }
    if (NULL != abi_fortran_info_from_user) {
        ompi_info_free(&abi_fortran_info_from_user);
    }

    /*
     * The failures funneled here carry OPAL error codes (negative
     * values), which are not valid MPI error classes; translate them
     * before they surface as the return value of
     * MPI_Abi_get_fortran_info.
     */
    if (ret < 0) {
        ret = (OPAL_ERR_OUT_OF_RESOURCE == ret) ? MPI_ERR_NO_MEM : MPI_ERR_INTERN;
    }

    return ret;
}

#if !OMPI_BUILD_FORTRAN_BINDINGS

/*
 * The set of predefined Fortran datatypes we can make available at
 * runtime from an application-supplied info object.  Each entry names the
 * predefined OMPI datatype object, the OMPI language flag describing its
 * data class (INT / FLOAT / COMPLEX), and -- for the fixed-size optional
 * types -- the info key that toggles support and the inherent size the
 * type must have when supported.
 */
typedef enum {
    OMPI_ABI_FORTRAN_CLASS_INT,      /* INTEGER kinds: integer-sized, DATA_INT */
    OMPI_ABI_FORTRAN_CLASS_LOGICAL,  /* LOGICAL/CHARACTER: integer-sized, no data flag */
    OMPI_ABI_FORTRAN_CLASS_FLOAT,
    OMPI_ABI_FORTRAN_CLASS_COMPLEX
} ompi_abi_fortran_class_t;

/*
 * Resolve a requested (class, byte size) to the matching OPAL base
 * datatype, or NULL when this build has no base type of that exact size
 * and class.  These OPAL base datatypes carry statically-allocated
 * descriptors that live for the lifetime of the process, so the pointers
 * copied out of them in ompi_abi_fortran_make_available() remain valid.
 *
 * A candidate is returned only if it is actually usable in this build:
 * some OPAL base types (e.g. opal_datatype_int16 without __int128, or
 * opal_datatype_float16 on a platform whose widest float is 8 bytes) are
 * compiled as "unavailable" with size 0, and aliasing a Fortran type onto
 * one of those would produce a bogus size-0 "available" datatype instead
 * of the MPI_UNDEFINED the standard requires.  Reject any candidate that
 * is unavailable or whose size does not match the request.
 */
static const opal_datatype_t *ompi_abi_fortran_base_type(ompi_abi_fortran_class_t cls,
                                                         int size)
{
    const opal_datatype_t *candidate = NULL;

    switch (cls) {
    case OMPI_ABI_FORTRAN_CLASS_INT:
    case OMPI_ABI_FORTRAN_CLASS_LOGICAL:
        switch (size) {
        case 1:  candidate = &opal_datatype_int1;  break;
        case 2:  candidate = &opal_datatype_int2;  break;
        case 4:  candidate = &opal_datatype_int4;  break;
        case 8:  candidate = &opal_datatype_int8;  break;
        case 16: candidate = &opal_datatype_int16; break;
        default: return NULL;
        }
        break;
    case OMPI_ABI_FORTRAN_CLASS_FLOAT:
        switch (size) {
        case 2:  candidate = &opal_datatype_float2;  break;
        case 4:  candidate = &opal_datatype_float4;  break;
        case 8:  candidate = &opal_datatype_float8;  break;
        case 12: candidate = &opal_datatype_float12; break;
        case 16: candidate = &opal_datatype_float16; break;
        default: return NULL;
        }
        break;
    case OMPI_ABI_FORTRAN_CLASS_COMPLEX:
        /*
         * Fortran COMPLEX of N bytes is a pair of N/2-byte reals.  Match
         * the OPAL complex base type whose size is exactly N.  The size
         * comparisons here also naturally skip any complex base type that
         * is unavailable (size 0).
         */
        if ((int)opal_datatype_float_complex.size == size) {
            candidate = &opal_datatype_float_complex;
        } else if ((int)opal_datatype_double_complex.size == size) {
            candidate = &opal_datatype_double_complex;
        } else if ((int)opal_datatype_long_double_complex.size == size) {
            candidate = &opal_datatype_long_double_complex;
        } else if ((int)opal_datatype_short_float_complex.size == size) {
            candidate = &opal_datatype_short_float_complex;
        } else {
            return NULL;
        }
        break;
    default:
        return NULL;
    }

    /*
     * Reject a candidate this build compiled as unavailable, or whose
     * size does not match the request (the int/float candidates are
     * selected by enum slot, not by their configured size).
     */
    if (!opal_datatype_is_valid(candidate) || (int)candidate->size != size) {
        return NULL;
    }
    return candidate;
}

/*
 * Return the OMPI data-class flag for a given Fortran type class.
 */
static uint32_t ompi_abi_fortran_class_flag(ompi_abi_fortran_class_t cls)
{
    switch (cls) {
    case OMPI_ABI_FORTRAN_CLASS_INT:     return OMPI_DATATYPE_FLAG_DATA_INT;
    case OMPI_ABI_FORTRAN_CLASS_LOGICAL: return 0;
    case OMPI_ABI_FORTRAN_CLASS_FLOAT:   return OMPI_DATATYPE_FLAG_DATA_FLOAT;
    case OMPI_ABI_FORTRAN_CLASS_COMPLEX: return OMPI_DATATYPE_FLAG_DATA_COMPLEX;
    default:                             return 0;
    }
}

/*
 * Make a currently-unavailable predefined Fortran datatype available by
 * aliasing it onto an OPAL base datatype, exactly as a Fortran-enabled
 * build does statically via OMPI_DATATYPE_INITIALIZER_FORTRAN().  We share
 * the base type's static descriptor pointers rather than cloning them: the
 * base types are process-lifetime constants and predefined datatypes are
 * never destructed, so no allocation is required.
 */
static void ompi_abi_fortran_make_available(ompi_datatype_t *dst,
                                            const opal_datatype_t *src,
                                            ompi_abi_fortran_class_t cls)
{
    dst->super.flags    = OPAL_DATATYPE_FLAG_BASIC |
                          OMPI_DATATYPE_FLAG_PREDEFINED |
                          OMPI_DATATYPE_FLAG_ANALYZED |
                          OMPI_DATATYPE_FLAG_MONOTONIC |
                          OMPI_DATATYPE_FLAG_DATA_FORTRAN |
                          ompi_abi_fortran_class_flag(cls);
    dst->super.id       = src->id;
    dst->super.bdt_used = src->bdt_used;
    dst->super.size     = src->size;
    dst->super.true_lb  = 0;
    dst->super.true_ub  = src->size;
    dst->super.lb       = 0;
    dst->super.ub       = src->size;
    dst->super.align    = src->align;
    dst->super.nbElems  = 1;
    dst->super.desc     = src->desc;
    dst->super.opt_desc = src->opt_desc;
    dst->super.ptypes   = src->ptypes;
}

/*
 * Read an integer-valued info key.  On success stores the value in *out and
 * sets *found.  Returns MPI_ERR_INFO if the key is present but not a valid
 * positive integer.
 */
static int ompi_abi_fortran_get_int(ompi_info_t *info, const char *key,
                                    int *out, bool *found)
{
    opal_cstring_t *value = NULL;
    int flag = 0;
    int ret;

    *found = false;
    ret = ompi_info_get(info, key, &value, &flag);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }
    if (!flag) {
        return MPI_SUCCESS;
    }

    ret = opal_cstring_to_int(value, out);
    OBJ_RELEASE(value);
    if (OPAL_SUCCESS != ret || *out <= 0) {
        return MPI_ERR_INFO;
    }
    *found = true;
    return MPI_SUCCESS;
}

/*
 * Staging description for one default-sized Fortran type parsed from the
 * info object before any global datatype is mutated.
 */
typedef struct {
    ompi_datatype_t *dt;
    ompi_abi_fortran_class_t cls;
    const opal_datatype_t *base; /* resolved OPAL base type, or NULL if key absent */
} ompi_abi_fortran_default_t;

/*
 * Description of one optional fixed-size Fortran type: the predefined
 * datatype, its data class, the info key that toggles support, and the
 * inherent byte size the type must have when supported.
 */
typedef struct {
    ompi_datatype_t *dt;
    ompi_abi_fortran_class_t cls;
    const char *key;
    int size;
} ompi_abi_fortran_optional_t;

OMPI_HIDDEN int ompi_abi_set_fortran_info(ompi_info_t *info)
{
    int ret = MPI_SUCCESS;
    static bool already_called = false;
    int i;

    /*
     * According to the MPI-5.0 standard only the first call affects the
     * state of the library; subsequent calls must return MPI_ERR_ABI.
     * The latch is set only on full success (see the end of this
     * function), so a call that fails validation leaves the library
     * unchanged and does not prevent a later, correct call.
     */
    if (true == already_called) {
        return MPI_ERR_ABI;
    }

    /*
     * Parse and validate the entire info object into local staging
     * structures before mutating any global datatype, so that a bad info
     * object leaves all library state untouched.
     */
    int logical_size = 0, integer_size = 0, real_size = 0, dblprec_size = 0;
    int character_size = 1; /* Fortran CHARACTER is always one byte */
    bool have_logical = false, have_integer = false;
    bool have_real = false, have_dblprec = false, have_character = false;

    ret = ompi_abi_fortran_get_int(info, "mpi_logical_size", &logical_size, &have_logical);
    if (MPI_SUCCESS != ret) { return ret; }
    ret = ompi_abi_fortran_get_int(info, "mpi_integer_size", &integer_size, &have_integer);
    if (MPI_SUCCESS != ret) { return ret; }
    ret = ompi_abi_fortran_get_int(info, "mpi_real_size", &real_size, &have_real);
    if (MPI_SUCCESS != ret) { return ret; }
    ret = ompi_abi_fortran_get_int(info, "mpi_double_precision_size", &dblprec_size,
                                   &have_dblprec);
    if (MPI_SUCCESS != ret) { return ret; }
    /* mpi_character_size is not a standard key, but honor it if present. */
    ret = ompi_abi_fortran_get_int(info, "mpi_character_size", &character_size,
                                   &have_character);
    if (MPI_SUCCESS != ret) { return ret; }

    ompi_abi_fortran_default_t defaults[] = {
        { &ompi_mpi_character.dt, OMPI_ABI_FORTRAN_CLASS_LOGICAL, NULL },
        { &ompi_mpi_logical.dt,   OMPI_ABI_FORTRAN_CLASS_LOGICAL, NULL },
        { &ompi_mpi_integer.dt,   OMPI_ABI_FORTRAN_CLASS_INT,     NULL },
        { &ompi_mpi_real.dt,      OMPI_ABI_FORTRAN_CLASS_FLOAT,   NULL },
        { &ompi_mpi_dblprec.dt,   OMPI_ABI_FORTRAN_CLASS_FLOAT,   NULL },
    };
    int default_sizes[] = { character_size, logical_size, integer_size,
                            real_size, dblprec_size };
    /*
     * CHARACTER is a fundamental Fortran type that is always one byte, so
     * make it available whenever Fortran info is set, whether or not the
     * (non-standard) mpi_character_size key was supplied.  The other four
     * default types are made available only when their size key is present.
     */
    (void) have_character;
    bool default_present[] = { true, have_logical, have_integer,
                               have_real, have_dblprec };

    /*
     * Resolve each requested default-sized type to its OPAL base type.  A
     * present key whose size cannot be satisfied by this build is an error.
     */
    for (i = 0; i < (int)(sizeof(defaults) / sizeof(defaults[0])); i++) {
        if (!default_present[i]) {
            continue;
        }
        defaults[i].base = ompi_abi_fortran_base_type(defaults[i].cls,
                                                      default_sizes[i]);
        if (NULL == defaults[i].base) {
            return MPI_ERR_INFO;
        }
    }

    ompi_abi_fortran_optional_t optionals[] = {
        { &ompi_mpi_logical1.dt,  OMPI_ABI_FORTRAN_CLASS_LOGICAL, "mpi_logical1_supported",  1 },
        { &ompi_mpi_logical2.dt,  OMPI_ABI_FORTRAN_CLASS_LOGICAL, "mpi_logical2_supported",  2 },
        { &ompi_mpi_logical4.dt,  OMPI_ABI_FORTRAN_CLASS_LOGICAL, "mpi_logical4_supported",  4 },
        { &ompi_mpi_logical8.dt,  OMPI_ABI_FORTRAN_CLASS_LOGICAL, "mpi_logical8_supported",  8 },
        { &ompi_mpi_logical16.dt, OMPI_ABI_FORTRAN_CLASS_LOGICAL, "mpi_logical16_supported", 16 },
        { &ompi_mpi_integer1.dt,  OMPI_ABI_FORTRAN_CLASS_INT,     "mpi_integer1_supported",  1 },
        { &ompi_mpi_integer2.dt,  OMPI_ABI_FORTRAN_CLASS_INT,     "mpi_integer2_supported",  2 },
        { &ompi_mpi_integer4.dt,  OMPI_ABI_FORTRAN_CLASS_INT,     "mpi_integer4_supported",  4 },
        { &ompi_mpi_integer8.dt,  OMPI_ABI_FORTRAN_CLASS_INT,     "mpi_integer8_supported",  8 },
        { &ompi_mpi_integer16.dt, OMPI_ABI_FORTRAN_CLASS_INT,     "mpi_integer16_supported", 16 },
        { &ompi_mpi_real2.dt,     OMPI_ABI_FORTRAN_CLASS_FLOAT,   "mpi_real2_supported",     2 },
        { &ompi_mpi_real4.dt,     OMPI_ABI_FORTRAN_CLASS_FLOAT,   "mpi_real4_supported",     4 },
        { &ompi_mpi_real8.dt,     OMPI_ABI_FORTRAN_CLASS_FLOAT,   "mpi_real8_supported",     8 },
        { &ompi_mpi_real16.dt,    OMPI_ABI_FORTRAN_CLASS_FLOAT,   "mpi_real16_supported",    16 },
        { &ompi_mpi_complex4.dt,  OMPI_ABI_FORTRAN_CLASS_COMPLEX, "mpi_complex4_supported",  4 },
        { &ompi_mpi_complex8.dt,  OMPI_ABI_FORTRAN_CLASS_COMPLEX, "mpi_complex8_supported",  8 },
        { &ompi_mpi_complex16.dt, OMPI_ABI_FORTRAN_CLASS_COMPLEX, "mpi_complex16_supported", 16 },
        { &ompi_mpi_complex32.dt, OMPI_ABI_FORTRAN_CLASS_COMPLEX, "mpi_complex32_supported", 32 },
        { &ompi_mpi_dblcplex.dt,  OMPI_ABI_FORTRAN_CLASS_COMPLEX, "mpi_double_complex_supported", 16 },
    };
    const opal_datatype_t *optional_base[sizeof(optionals) / sizeof(optionals[0])];
    bool optional_set[sizeof(optionals) / sizeof(optionals[0])];

    /*
     * Resolve each optional type whose *_supported key is true.
     *
     * The mpi_<type><n>_supported keys describe what the *Fortran
     * compiler* supports (abi-rendered.tex / MPI-5.0 sec. "Fortran Type
     * Registration"), not a promise that this C-backed build can realize
     * that datatype.  When the key is true but this build has no
     * underlying type of the required size (e.g. mpi_real16_supported on a
     * platform whose widest float is 8 bytes), we accept the key and leave
     * the datatype unavailable rather than failing the whole call -- a
     * single unrealizable optional type must not sink the realizable
     * defaults and optionals alongside it.  This is consistent with the
     * standard, which has the application discover actual availability via
     * MPI_Type_size() returning MPI_UNDEFINED, not via the return code of
     * this routine.  See the MPI_Abi_set_fortran_info man page.
     */
    for (i = 0; i < (int)(sizeof(optionals) / sizeof(optionals[0])); i++) {
        bool supported = false;
        int flag = 0;

        optional_base[i] = NULL;
        optional_set[i] = false;

        ret = ompi_info_get_bool(info, optionals[i].key, &supported, &flag);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
        if (flag && supported) {
            optional_base[i] = ompi_abi_fortran_base_type(optionals[i].cls,
                                                          optionals[i].size);
            optional_set[i] = (NULL != optional_base[i]);
        }
    }

    /*
     * Validation complete.  Apply the staged changes: all failures above
     * returned without touching a single global datatype.
     */
    for (i = 0; i < (int)(sizeof(defaults) / sizeof(defaults[0])); i++) {
        if (NULL != defaults[i].base) {
            ompi_abi_fortran_make_available(defaults[i].dt, defaults[i].base,
                                            defaults[i].cls);
        }
    }
    for (i = 0; i < (int)(sizeof(optionals) / sizeof(optionals[0])); i++) {
        if (optional_set[i]) {
            ompi_abi_fortran_make_available(optionals[i].dt, optional_base[i],
                                            optionals[i].cls);
        }
    }

    /*
     * Remember the info the user supplied so a subsequent
     * MPI_Abi_get_fortran_info can return it.  ompi_info_dup() copies into
     * a caller-allocated object, so allocate it first.
     */
    abi_fortran_info_from_user = ompi_info_allocate();
    if (NULL == abi_fortran_info_from_user) {
        return MPI_ERR_NO_MEM;
    }
    ret = ompi_info_dup(info, &abi_fortran_info_from_user);
    if (MPI_SUCCESS != ret) {
        ompi_info_free(&abi_fortran_info_from_user);
        if (ret < 0) {
            ret = (OPAL_ERR_OUT_OF_RESOURCE == ret) ? MPI_ERR_NO_MEM : MPI_ERR_INTERN;
        }
        return ret;
    }

    ompi_mpi_instance_append_finalize(ompi_abi_fortran_finalize);
    already_called = true;

    return MPI_SUCCESS;
}

#else /* OMPI_BUILD_FORTRAN_BINDINGS */

OMPI_HIDDEN int ompi_abi_set_fortran_info(ompi_info_t *info)
{
    /*
     * This build knows the Fortran compiler's type properties, so the
     * implementation already set them and MPI_Abi_get_fortran_info returns
     * a populated info object.  Per the MPI-5.0 standard the application
     * cannot override them: every call returns MPI_ERR_ABI.
     */
    (void) info;
    return MPI_ERR_ABI;
}

#endif /* OMPI_BUILD_FORTRAN_BINDINGS */

OMPI_HIDDEN int ompi_abi_get_fortran_booleans(int logical_size, void *logical_true, void *logical_false, int *is_set)
{
    int ret = MPI_SUCCESS;

    /*
     * logical_size is the size in bytes of a Fortran logical and must be a
     * power of two.  Reject anything else with MPI_ERR_ARG up front so the
     * documented contract holds in every build: the Fortran-enabled path
     * below would otherwise fall through its switch to "unavailable" and
     * return MPI_SUCCESS with *is_set == 0, which is inconsistent both with
     * the documentation and with MPI_Abi_set_fortran_booleans (which rejects
     * such sizes).
     */
    if (countbits32((unsigned int)logical_size) != 1) {
        return MPI_ERR_ARG;
    }

#if  OMPI_HAVE_FORTRAN_LOGICAL
    bool unavailable = false;
    bool use_int8_t = false, use_int16_t = false, use_int32_t = false, use_int64_t = false;
    /*
     * handle special case of "default" LOGICAL size.
     */
    if (OMPI_SIZEOF_FORTRAN_LOGICAL == logical_size) {
        switch(OMPI_SIZEOF_FORTRAN_LOGICAL) {
            case 1:
                use_int8_t = true;
                break;
            case 2:
                use_int16_t = true;
                break;
            case 4:
                use_int32_t = true;
                break;
            case 8:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
        }
    } else {
    switch (logical_size) {
        case OMPI_SIZEOF_FORTRAN_LOGICAL1:
            switch (OMPI_DATATYPE_MPI_LOGICAL1) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
             }
            break;
        case OMPI_SIZEOF_FORTRAN_LOGICAL2:
            switch (OMPI_DATATYPE_MPI_LOGICAL2) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
            }
            break;
        case OMPI_SIZEOF_FORTRAN_LOGICAL4:
            switch (OMPI_DATATYPE_MPI_LOGICAL4) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
            }
            break;
        case OMPI_SIZEOF_FORTRAN_LOGICAL8:
            switch (OMPI_DATATYPE_MPI_LOGICAL8) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
            }
            break;
        case OMPI_SIZEOF_FORTRAN_LOGICAL16:
            switch (OMPI_DATATYPE_MPI_LOGICAL16) {
            case OMPI_DATATYPE_MPI_INT8_T:
                use_int8_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT16_T:
                use_int16_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT32_T:
                use_int32_t = true;
                break;
            case OMPI_DATATYPE_MPI_INT64_T:
                use_int64_t = true;
                break;
            default:
                unavailable = true;
                break;
            }
            break;
        default:
            unavailable = true;
        }
    }

    if (true == unavailable) {
        *is_set = 0;
    } else {
        *is_set = 1;
        if (true == use_int8_t) {
            *(int8_t *)logical_true = (int8_t)OMPI_FORTRAN_VALUE_TRUE;
            *(int8_t *)logical_false = (int8_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int16_t) {
            *(int16_t *)logical_true = (int16_t)OMPI_FORTRAN_VALUE_TRUE;
            *(int16_t *)logical_false = (int16_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int32_t) {
            *(int32_t *)logical_true = (int32_t)OMPI_FORTRAN_VALUE_TRUE;
            *(int32_t *)logical_false = (int32_t)OMPI_FORTRAN_VALUE_FALSE;
        } else if (true == use_int64_t) {
            *(int64_t *)logical_true = (int64_t)OMPI_FORTRAN_VALUE_TRUE;
            *(int64_t *)logical_false = (int64_t)OMPI_FORTRAN_VALUE_FALSE;
        }
    }

#else

/*
 * OMPI wasn't built with fortran bindings support so 
 * see if the user set something with MPI_ABI_set_fortran_boleans
 */
    int logical_size_pow2;

    /*
     * logical_size is a power-of-two byte count (1, 2, 4, 8, 16); its single
     * set bit gives the index (0..4) into user_logicals[].  Scan bits [0,7]
     * so the shift inside opal_hibit() stays well-defined -- opal_hibit(x, 0)
     * masks out every bit and always returns -1.  Reject any out-of-range
     * result before using it as an array index.  (This guard previously read
     * "4 > logical_size_pow2", which both relied on the broken -1 result and
     * was inverted relative to the set path, which stores indices 0..4.)
     */
    logical_size_pow2 = opal_hibit(logical_size, 8);

    if (logical_size_pow2 < 0 || logical_size_pow2 > 4) {
        return MPI_ERR_ARG;
    }

    if (false == user_logicals[logical_size_pow2].is_set) {
        *is_set = 0;
    } else { 
        switch(logical_size_pow2) {
        case 0:
            *(int8_t *)logical_true = (int8_t)user_logicals[logical_size_pow2].true_value;
            *(int8_t *)logical_false = (int8_t)user_logicals[logical_size_pow2].false_value;
            *is_set = 1;
            break;
        case 1:
            *(int16_t *)logical_true = (int16_t)user_logicals[logical_size_pow2].true_value;
            *(int16_t *)logical_false = (int16_t)user_logicals[logical_size_pow2].false_value;
            *is_set = 1;
            break;
        case 2:
            *(int32_t *)logical_true = (int32_t)user_logicals[logical_size_pow2].true_value;
            *(int32_t *)logical_false = (int32_t)user_logicals[logical_size_pow2].false_value;
            *is_set = 1;
            break;
        case 3:
            *(int64_t *)logical_true = (int64_t)user_logicals[logical_size_pow2].true_value;
            *(int64_t *)logical_false = (int64_t)user_logicals[logical_size_pow2].false_value;
            *is_set = 1;
            break;
#if HAVE_OPAL_INT128_T
        case 4:
            *(opal_int128_t *)logical_true = (opal_int128_t)user_logicals[logical_size_pow2].true_value128;
            *(opal_int128_t *)logical_false = (opal_int128_t)user_logicals[logical_size_pow2].false_value128;
            *is_set = 1;
            break;
#endif
        default:
            ret = MPI_ERR_ARG;
            break;
        }
    }

#endif  /* OMPI_HAVE_FORTRAN_LOGICAL */
    return ret;
}

OMPI_HIDDEN int ompi_abi_set_fortran_booleans(int logical_size, void *logical_true, void *logical_false)
{
    int ret=MPI_SUCCESS;
    static bool already_called = false;

    /*
     * According to the MPI-5.0 standard this function can only be called
     * once.  Only a call that actually configures the logicals counts against
     * that limit, so the one-shot latch is set at the very end, after all
     * argument validation has passed -- a call that fails validation stores
     * nothing and must not prevent a later, correct call from succeeding.
     */
    if (true == already_called) {
        return MPI_ERR_ABI;
    }

#if OMPI_HAVE_FORTRAN_LOGICAL
    /*
     * Mirror ompi_abi_set_fortran_info(): when this build knows the
     * Fortran compiler's LOGICAL representation,
     * ompi_abi_get_fortran_booleans() returns the compiler-derived
     * values and never consults user_logicals[].  Accepting a
     * registration here would let the set "succeed" while the get
     * silently returns different values, so reject the call instead.
     */
    return MPI_ERR_ABI;
#endif

    int logical_size_pow2;
    int64_t logical_true64 = 0;
    int64_t logical_false64 = 0;
#if HAVE_OPAL_INT128_T
    opal_int128_t logical_true128 = 0;
    opal_int128_t logical_false128 = 0;
#else
    int64_t logical_true128 = 0;
    int64_t logical_false128 = 0;
#endif
    bool using128 = false;

    /* check logical size to be pow2 */

    if(countbits32((unsigned int)logical_size) != 1) {
        return MPI_ERR_ARG;
    }

    /*
     * logical_size is a power-of-two byte count (1, 2, 4, 8, 16); its single
     * set bit gives the index (0..4) into user_logicals[].  Scan bits [0,7]
     * so the shift inside opal_hibit() stays well-defined -- opal_hibit(x, 0)
     * masks out every bit and always returns -1.  Any out-of-range size is
     * rejected here before it can be used as an array index.
     */
    logical_size_pow2 = opal_hibit(logical_size, 8);
    if (logical_size_pow2 < 0 || logical_size_pow2 > 4) {
        return MPI_ERR_ARG;
    }

    switch(logical_size_pow2) {
    case 0:
        logical_true64 = (int64_t)*(int8_t *)logical_true;
        logical_false64 = (int64_t)*(int8_t *)logical_false;
        break;
    case 1:
        logical_true64 = (int64_t)*(int16_t *)logical_true;
        logical_false64 = (int64_t)*(int16_t *)logical_false;
        break;
    case 2:
        logical_true64 = (int64_t)*(int32_t *)logical_true;
        logical_false64 = (int64_t)*(int32_t *)logical_false;
        break;
    case 3:
        logical_true64 = (int64_t)*(int64_t *)logical_true;
        logical_false64 = (int64_t)*(int64_t *)logical_false;
        break;
#if HAVE_OPAL_INT128_T
    case 4:
        logical_true128 = (opal_int128_t)*(opal_int128_t *)logical_true;
        logical_false128 = (opal_int128_t)*(opal_int128_t *)logical_false;
        using128 = true;
        break;
#endif
    default:
        /* in-range index with no handler (e.g. size 16 without int128
         * support): reject before the array write below */
        return MPI_ERR_ARG;
    }

#if !HAVE_OPAL_INT128_T
    assert(false == using128);
#endif

    if (true == using128) {
        user_logicals[logical_size_pow2].true_value128 = logical_true128;
        user_logicals[logical_size_pow2].false_value128 = logical_false128;
        user_logicals[logical_size_pow2].is_set = true;
        user_logicals[logical_size_pow2].using_int128 = true;
    } else { 
        user_logicals[logical_size_pow2].true_value = logical_true64;
        user_logicals[logical_size_pow2].false_value = logical_false64;
        user_logicals[logical_size_pow2].is_set = true;
    }

    /* The logicals were configured successfully: latch so that any later
     * call is rejected with MPI_ERR_ABI per the MPI-5.0 "at most once"
     * contract. */
    already_called = true;

    return ret;
}

