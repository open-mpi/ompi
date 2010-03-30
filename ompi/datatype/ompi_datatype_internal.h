/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * ompi_datatype_t interface for OMPI internal data type representation
 *
 * ompi_datatype_t is a class which represents contiguous or
 * non-contiguous data together with constituent type-related
 * information.
 */

#ifndef OMPI_DATATYPE_INTERNAL_H
#define OMPI_DATATYPE_INTERNAL_H

#include "opal/datatype/opal_datatype_internal.h"

/*
 * This is the OMPI-layered numbering of ALL supported MPI types
 * (derived from the old DT_ names).
 * NOTE: These numbers have to match the d_f_to_c_index.
 */
#define OMPI_DATATYPE_MPI_NULL                    0x00
#define OMPI_DATATYPE_MPI_LB                      0x00
#define OMPI_DATATYPE_MPI_UB                      0x01
#define OMPI_DATATYPE_MPI_CHAR                    0x02
#define OMPI_DATATYPE_MPI_SIGNED_CHAR             0x03   /* Changed order */
#define OMPI_DATATYPE_MPI_UNSIGNED_CHAR           0x04   /* Changed order */
#define OMPI_DATATYPE_MPI_BYTE                    0x05
#define OMPI_DATATYPE_MPI_SHORT                   0x06
#define OMPI_DATATYPE_MPI_UNSIGNED_SHORT          0x07
#define OMPI_DATATYPE_MPI_INT                     0x08
#define OMPI_DATATYPE_MPI_UNSIGNED_INT            0x09
#define OMPI_DATATYPE_MPI_LONG                    0x0A
#define OMPI_DATATYPE_MPI_UNSIGNED_LONG           0x0B
#define OMPI_DATATYPE_MPI_LONG_LONG               0x0C   /* Was LONG_LONG_INT */
#define OMPI_DATATYPE_MPI_UNSIGNED_LONG_LONG      0x0D
#define OMPI_DATATYPE_MPI_FLOAT                   0x0E
#define OMPI_DATATYPE_MPI_DOUBLE                  0x0F
#define OMPI_DATATYPE_MPI_LONG_DOUBLE             0x10
#define OMPI_DATATYPE_MPI_COMPLEX8                0x11
#define OMPI_DATATYPE_MPI_COMPLEX16               0x12
#define OMPI_DATATYPE_MPI_COMPLEX32               0x13
#define OMPI_DATATYPE_MPI_WCHAR                   0x14
#define OMPI_DATATYPE_MPI_PACKED                  0x15

#define OMPI_DATATYPE_MPI_BOOL                    0x16   /* Was CXX_BOOL */

#define OMPI_DATATYPE_MPI_LOGICAL                 0x17
#define OMPI_DATATYPE_MPI_CHARACTER               0x18   /* Changed */
#define OMPI_DATATYPE_MPI_INTEGER                 0x19
#define OMPI_DATATYPE_MPI_REAL                    0x1A
#define OMPI_DATATYPE_MPI_DOUBLE_PRECISION        0x1B   /* Was DBLPREC */

/*
 * Derived datatypes supposely contiguous
 */
#define OMPI_DATATYPE_MPI_COMPLEX                 0x1C   /* Was COMPLEX_FLOAT */
#define OMPI_DATATYPE_MPI_DOUBLE_COMPLEX          0x1D   /* Was COMPLEX_DOUBLE */
#define OMPI_DATATYPE_MPI_LONG_DOUBLE_COMPLEX     0x1E   /* Was COMPLEX_LONG_DOUBLE */
#define OMPI_DATATYPE_MPI_2INT                    0x1F
#define OMPI_DATATYPE_MPI_2INTEGER                0x20
#define OMPI_DATATYPE_MPI_2REAL                   0x21
#define OMPI_DATATYPE_MPI_2DBLPREC                0x22
#define OMPI_DATATYPE_MPI_2COMPLEX                0x23
#define OMPI_DATATYPE_MPI_2DOUBLE_COMPLEX         0x24
/*
 * Derived datatypes which will definitively be non contiguous on some architectures.
 */
#define OMPI_DATATYPE_MPI_FLOAT_INT               0x25
#define OMPI_DATATYPE_MPI_DOUBLE_INT              0x26
#define OMPI_DATATYPE_MPI_LONG_DOUBLE_INT         0x27
#define OMPI_DATATYPE_MPI_LONG_INT                0x28
#define OMPI_DATATYPE_MPI_SHORT_INT               0x29
#define OMPI_DATATYPE_MPI_UNAVAILABLE             0x2A

#define OMPI_DATATYPE_MPI_MAX_PREDEFINED          0x2B

/*
 * Optional Fortran datatypes, these map to representable types
 * in the lower layer, aka as other Fortran types have to map to C types,
 * additionally, if the type has the same size as the mandatory
 * Fortran type, map to this one.
 */
/* LOGICAL */
#if OMPI_SIZEOF_FORTRAN_LOGICAL1 == OMPI_SIZEOF_FORTRAN_LOGICAL
#  define OMPI_DATATYPE_MPI_LOGICAL1              OMPI_DATATYPE_MPI_LOGICAL
#elif OMPI_SIZEOF_FORTRAN_LOGICAL1 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_LOGICAL1              OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_LOGICAL1 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_LOGICAL1              OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_LOGICAL1 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_LOGICAL1              OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_LOGICAL1 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_LOGICAL1              OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_LOGICAL1              OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_LOGICAL2 == OMPI_SIZEOF_FORTRAN_LOGICAL
#  define OMPI_DATATYPE_MPI_LOGICAL2              OMPI_DATATYPE_MPI_LOGICAL
#elif OMPI_SIZEOF_FORTRAN_LOGICAL2 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_LOGICAL2              OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_LOGICAL2 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_LOGICAL2              OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_LOGICAL2 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_LOGICAL2              OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_LOGICAL2 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_LOGICAL2              OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_LOGICAL2              OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_LOGICAL4 == OMPI_SIZEOF_FORTRAN_LOGICAL
#  define OMPI_DATATYPE_MPI_LOGICAL4              OMPI_DATATYPE_MPI_LOGICAL
#elif OMPI_SIZEOF_FORTRAN_LOGICAL4 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_LOGICAL4              OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_LOGICAL4 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_LOGICAL4              OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_LOGICAL4 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_LOGICAL4              OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_LOGICAL4 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_LOGICAL4              OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_LOGICAL4              OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_LOGICAL8 == OMPI_SIZEOF_FORTRAN_LOGICAL
#  define OMPI_DATATYPE_MPI_LOGICAL8              OMPI_DATATYPE_MPI_LOGICAL
#elif OMPI_SIZEOF_FORTRAN_LOGICAL8 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_LOGICAL8              OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_LOGICAL8 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_LOGICAL8              OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_LOGICAL8 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_LOGICAL8              OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_LOGICAL8 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_LOGICAL8              OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_LOGICAL8              OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

/* INTEGER */
#if OMPI_SIZEOF_FORTRAN_INTEGER1 == OMPI_SIZEOF_FORTRAN_INTEGER
#  define OMPI_DATATYPE_MPI_INTEGER1              OMPI_DATATYPE_MPI_INTEGER
#elif OMPI_SIZEOF_FORTRAN_INTEGER1 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_INTEGER1              OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_INTEGER1 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_INTEGER1              OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_INTEGER1 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_INTEGER1              OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_INTEGER1 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_INTEGER1              OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_INTEGER1              OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_INTEGER2 == OMPI_SIZEOF_FORTRAN_INTEGER
#  define OMPI_DATATYPE_MPI_INTEGER2              OMPI_DATATYPE_MPI_INTEGER
#elif OMPI_SIZEOF_FORTRAN_INTEGER2 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_INTEGER2              OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_INTEGER2 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_INTEGER2              OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_INTEGER2 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_INTEGER2              OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_INTEGER2 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_INTEGER2              OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_INTEGER2              OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_INTEGER4 == OMPI_SIZEOF_FORTRAN_INTEGER
#  define OMPI_DATATYPE_MPI_INTEGER4              OMPI_DATATYPE_MPI_INTEGER
#elif OMPI_SIZEOF_FORTRAN_INTEGER4 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_INTEGER4              OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_INTEGER4 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_INTEGER4              OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_INTEGER4 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_INTEGER4              OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_INTEGER4 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_INTEGER4              OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_INTEGER4              OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_INTEGER8 == OMPI_SIZEOF_FORTRAN_INTEGER
#  define OMPI_DATATYPE_MPI_INTEGER8              OMPI_DATATYPE_MPI_INTEGER
#elif OMPI_SIZEOF_FORTRAN_INTEGER8 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_INTEGER8              OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_INTEGER8 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_INTEGER8              OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_INTEGER8 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_INTEGER8              OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_INTEGER8 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_INTEGER8              OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_INTEGER8              OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_INTEGER16 == OMPI_SIZEOF_FORTRAN_INTEGER
#  define OMPI_DATATYPE_MPI_INTEGER16             OMPI_DATATYPE_MPI_INTEGER
#elif OMPI_SIZEOF_FORTRAN_INTEGER16 == SIZEOF_CHAR
#  define OMPI_DATATYPE_MPI_INTEGER16             OMPI_DATATYPE_MPI_CHAR
#elif OMPI_SIZEOF_FORTRAN_INTEGER16 == SIZEOF_SHORT
#  define OMPI_DATATYPE_MPI_INTEGER16             OMPI_DATATYPE_MPI_SHORT
#elif OMPI_SIZEOF_FORTRAN_INTEGER16 == SIZEOF_INT
#  define OMPI_DATATYPE_MPI_INTEGER16             OMPI_DATATYPE_MPI_INT
#elif OMPI_SIZEOF_FORTRAN_INTEGER16 == SIZEOF_LONG
#  define OMPI_DATATYPE_MPI_INTEGER16             OMPI_DATATYPE_MPI_LONG
#else
#  define OMPI_DATATYPE_MPI_INTEGER16             OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

/* REAL */
#if OMPI_SIZEOF_FORTRAN_REAL2 == OMPI_SIZEOF_FORTRAN_REAL
#  define OMPI_DATATYPE_MPI_REAL2                 OMPI_DATATYPE_MPI_REAL
#elif OMPI_SIZEOF_FORTRAN_REAL2 == SIZEOF_FLOAT
#  define OMPI_DATATYPE_MPI_REAL2                 OMPI_DATATYPE_MPI_FLOAT
#elif OMPI_SIZEOF_FORTRAN_REAL2 == SIZEOF_DOUBLE
#  define OMPI_DATATYPE_MPI_REAL2                 OMPI_DATATYPE_MPI_DOUBLE
#elif OMPI_SIZEOF_FORTRAN_REAL2 == SIZEOF_LONG_DOUBLE
#  define OMPI_DATATYPE_MPI_REAL2                 OMPI_DATATYPE_MPI_LONG_DOUBLE
#else
#  define OMPI_DATATYPE_MPI_REAL2                 OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_REAL4 == OMPI_SIZEOF_FORTRAN_REAL
#  define OMPI_DATATYPE_MPI_REAL4                 OMPI_DATATYPE_MPI_REAL
#elif OMPI_SIZEOF_FORTRAN_REAL4 == SIZEOF_FLOAT
#  define OMPI_DATATYPE_MPI_REAL4                 OMPI_DATATYPE_MPI_FLOAT
#elif OMPI_SIZEOF_FORTRAN_REAL4 == SIZEOF_DOUBLE
#  define OMPI_DATATYPE_MPI_REAL4                 OMPI_DATATYPE_MPI_DOUBLE
#elif OMPI_SIZEOF_FORTRAN_REAL4 == SIZEOF_LONG_DOUBLE
#  define OMPI_DATATYPE_MPI_REAL4                 OMPI_DATATYPE_MPI_LONG_DOUBLE
#else
#  define OMPI_DATATYPE_MPI_REAL4                 OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_REAL8 == OMPI_SIZEOF_FORTRAN_REAL
#  define OMPI_DATATYPE_MPI_REAL8                 OMPI_DATATYPE_MPI_REAL
#elif OMPI_SIZEOF_FORTRAN_REAL8 == SIZEOF_FLOAT
#  define OMPI_DATATYPE_MPI_REAL8                 OMPI_DATATYPE_MPI_FLOAT
#elif OMPI_SIZEOF_FORTRAN_REAL8 == SIZEOF_DOUBLE
#  define OMPI_DATATYPE_MPI_REAL8                 OMPI_DATATYPE_MPI_DOUBLE
#elif OMPI_SIZEOF_FORTRAN_REAL8 == SIZEOF_LONG_DOUBLE
#  define OMPI_DATATYPE_MPI_REAL8                 OMPI_DATATYPE_MPI_LONG_DOUBLE
#else
#  define OMPI_DATATYPE_MPI_REAL8                 OMPI_DATATYPE_MPI_UNAVAILABLE
#endif

#if OMPI_SIZEOF_FORTRAN_REAL16 == OMPI_SIZEOF_FORTRAN_REAL
#  define OMPI_DATATYPE_MPI_REAL16                OMPI_DATATYPE_MPI_REAL
#elif OMPI_SIZEOF_FORTRAN_REAL16 == SIZEOF_FLOAT
#  define OMPI_DATATYPE_MPI_REAL16                OMPI_DATATYPE_MPI_FLOAT
#elif OMPI_SIZEOF_FORTRAN_REAL16 == SIZEOF_DOUBLE
#  define OMPI_DATATYPE_MPI_REAL16                OMPI_DATATYPE_MPI_DOUBLE
#elif OMPI_SIZEOF_FORTRAN_REAL16 == SIZEOF_LONG_DOUBLE
#  define OMPI_DATATYPE_MPI_REAL16                OMPI_DATATYPE_MPI_LONG_DOUBLE
#else
#  define OMPI_DATATYPE_MPI_REAL16                OMPI_DATATYPE_MPI_UNAVAILABLE
#endif


OMPI_DECLSPEC extern union dt_elem_desc ompi_datatype_predefined_elem_desc[2 * OMPI_DATATYPE_MPI_MAX_PREDEFINED];
extern const ompi_datatype_t* ompi_datatype_basicDatatypes[OMPI_DATATYPE_MPI_MAX_PREDEFINED];

/* There 3 types of predefined data types.
 * - the basic one composed by just one basic datatype which are
 *   definitively contiguous
 * - the derived ones where the same basic type is used multiple times.
 *   They should be most of the time contiguous.
 * - and finally the derived one where multiple basic types are used.
 *   Depending on the architecture they can be contiguous or not.
 *
 * At this level we do not care from which language the datatype came from
 * (C, C++ or FORTRAN), we only focus on their internal representation in
 * the host memory.
 */

#define OMPI_DATATYPE_EMPTY_DATA(NAME)                                               \
    OMPI_DATATYPE_MPI_ ## NAME /*id*/,                                               \
    0 /*d_f_to_c_index*/,                                                            \
    NULL /*d_keyhash*/,                                                              \
    NULL /*args*/,                                                                   \
    NULL /*packed_description*/,                                                     \
    "MPI_" # NAME /*name*/

#define OMPI_DATATYPE_INITIALIZER_UNAVAILABLE(FLAGS)                                 \
    OPAL_DATATYPE_INITIALIZER_UNAVAILABLE(FLAGS)

#define OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE( TYPE, NAME, FLAGS )                \
    { /*ompi_predefined_datatype_t*/                                                 \
        { /* ompi_datatype_t */                                                      \
            OMPI_DATATYPE_INITIALIZER_ ## TYPE (OMPI_DATATYPE_FLAG_PREDEFINED |      \
                                                (FLAGS)) /*super*/,                  \
            OMPI_DATATYPE_EMPTY_DATA(NAME) /*id,d_f_to_c_index,d_keyhash,args,packed_description,name*/ \
        },                                                                           \
        {0, } /* padding */                                                          \
    }
/*
 * Two macros for convenience
 */
#define OMPI_DATATYPE_INIT_PREDEFINED( NAME, FLAGS )                                 \
    OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE( NAME, NAME, FLAGS )
#define OMPI_DATATYPE_INIT_UNAVAILABLE( NAME, FLAGS )                                \
    OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE( UNAVAILABLE, NAME, FLAGS )

/*
 * Initilization for these types is deferred until runtime.
 *
 * Using this macro implies that at this point not all informations needed
 * to fill up the datatype are known. We fill them with zeros and then later
 * when the datatype engine will be initialized we complete with the
 * correct information. This macro should be used for all composed types.
 */
#define OMPI_DATATYPE_INIT_DEFER(NAME, FLAGS)                                        \
    OMPI_DATATYPE_INIT_UNAVAILABLE(NAME, FLAGS)


#if OMPI_WANT_F77_BINDINGS
/*
 * For Fortran, we need to pass information, such as ALIGNMENT and SIZE as well
 * Therefore, for initialization at compile-time, pass this data as well.
 *
 * However, there is no underlying OPAL-TYPE, therefore we just pass NAME, SIZE,
 * ALIGN and the FLAGS. Additionally, ONLY for Fortran we need the
 * ompi_datatype_predefined_elem_desc for the additional types.
 */
#define OMPI_DATATYPE_INIT_DESC_PREDEFINED(TYPE, SIZE)                               \
    {                                                                                \
        1 /*length*/, 1 /*used*/,                                                    \
        &(ompi_datatype_predefined_elem_desc[2 * OPAL_DATATYPE_ ## TYPE ## SIZE]) /*desc*/ \
    }

/*
 * Fortran types are based on the underlying OPAL types: They share the ID -- however,
 * the alignment is overwritten.
 */
#define OMPI_DATATYPE_INITIALIZER_FORTRAN( TYPE, NAME, SIZE, ALIGN, FLAGS )          \
    {                                                                                \
        OPAL_OBJ_STATIC_INIT(opal_datatype_t),                                       \
        OPAL_DATATYPE_FLAG_BASIC |                                                   \
            OMPI_DATATYPE_FLAG_PREDEFINED |                                          \
            OMPI_DATATYPE_FLAG_DATA_FORTRAN | (FLAGS) /*flag*/,                      \
        OPAL_DATATYPE_ ## TYPE ## SIZE /*id*/,                                       \
        (((uint32_t)1)<<(OPAL_DATATYPE_ ## TYPE ## SIZE)) /*bdt_used*/,              \
        SIZE /*size*/,                                                               \
        0 /*true_lb*/, SIZE /*true_ub*/, 0 /*lb*/, SIZE /*ub*/,                      \
        (ALIGN) /*align*/,                                                           \
        1 /*nbElems*/,                                                               \
        OPAL_DATATYPE_INIT_NAME(TYPE ## SIZE) /*name*/,                              \
        OMPI_DATATYPE_INIT_DESC_PREDEFINED(TYPE, SIZE) /*desc*/,                     \
        OMPI_DATATYPE_INIT_DESC_PREDEFINED(TYPE, SIZE) /*opt_desc*/,                 \
        OPAL_DATATYPE_INIT_BTYPES_ARRAY_ ## TYPE ## SIZE /*btypes*/                  \
    }

#define OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN( TYPE, NAME, SIZE, ALIGN, FLAGS ) \
    { /*ompi_predefined_datatype_t*/                                                 \
        { /*ompi_datatype_t*/                                                        \
            OMPI_DATATYPE_INITIALIZER_FORTRAN( TYPE, NAME, SIZE, ALIGN, FLAGS) /*super*/, \
            OMPI_DATATYPE_EMPTY_DATA(NAME) /*id,d_f_to_c_index,d_keyhash,args,packed_description,name*/ \
        },                                                                           \
        {0, } /* padding */                                                          \
    }
#else
#define OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE_FORTRAN( TYPE, NAME, SIZE, ALIGN, FLAGS ) \
    OMPI_DATATYPE_INIT_PREDEFINED_BASIC_TYPE( UNAVAILABLE, NAME, FLAGS )
#endif


/*
 * OMPI-Versions of Initializer mapped onto OPAL-Types
 */

#define OMPI_DATATYPE_INITIALIZER_LB                  OPAL_DATATYPE_INITIALIZER_LB
#define OMPI_DATATYPE_INITIALIZER_UB                  OPAL_DATATYPE_INITIALIZER_UB
#define OMPI_DATATYPE_INITIALIZER_CHAR                OPAL_DATATYPE_INITIALIZER_INT1
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_CHAR       OPAL_DATATYPE_INITIALIZER_UINT1
#define OMPI_DATATYPE_INITIALIZER_SIGNED_CHAR         OPAL_DATATYPE_INITIALIZER_INT1
#define OMPI_DATATYPE_INITIALIZER_BYTE                OPAL_DATATYPE_INITIALIZER_UINT1

#if SIZEOF_SHORT == 2
#define OMPI_DATATYPE_INITIALIZER_SHORT               OPAL_DATATYPE_INITIALIZER_INT2
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_SHORT      OPAL_DATATYPE_INITIALIZER_UINT2
#elif SIZEOF_SHORT == 4
#define OMPI_DATATYPE_INITIALIZER_SHORT               OPAL_DATATYPE_INITIALIZER_INT4
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_SHORT      OPAL_DATATYPE_INITIALIZER_UINT4
#elif SIZEOF_SHORT == 8
#define OMPI_DATATYPE_INITIALIZER_SHORT               OPAL_DATATYPE_INITIALIZER_INT8
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_SHORT      OPAL_DATATYPE_INITIALIZER_UINT8
#endif

/*
 * Unfortunately, the following does not work:
 *
#define OMPI_DATATYPE_INITIALIZER_INT                 OPAL_DATATYPE_INITIALIZER_INT ## SIZEOF_INT
#define OMPI_DATATYPE_INITIALIZER_UINT                OPAL_DATATYPE_INITIALIZER_UINT ## SIZEOF_INT
#define OMPI_DATATYPE_INT                             OPAL_DATATYPE_INT ## SIZEOF_INT
#define OMPI_DATATYPE_UNSIGNED_INT                    OPAL_DATATYPE_UINT ## SIZEOF_INT
 * therefore do it the long way
 */

#if SIZEOF_INT == 2
#define OMPI_DATATYPE_INITIALIZER_INT                 OPAL_DATATYPE_INITIALIZER_INT2
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_INT        OPAL_DATATYPE_INITIALIZER_UINT2
#elif SIZEOF_INT == 4
#define OMPI_DATATYPE_INITIALIZER_INT                 OPAL_DATATYPE_INITIALIZER_INT4
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_INT        OPAL_DATATYPE_INITIALIZER_UINT4
#elif SIZEOF_INT == 8
#define OMPI_DATATYPE_INITIALIZER_INT                 OPAL_DATATYPE_INITIALIZER_INT8
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_INT        OPAL_DATATYPE_INITIALIZER_UINT8
#endif

#if SIZEOF_LONG == 4
#define OMPI_DATATYPE_INITIALIZER_LONG                OPAL_DATATYPE_INITIALIZER_INT4
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_LONG       OPAL_DATATYPE_INITIALIZER_UINT4
#elif SIZEOF_LONG == 8
#define OMPI_DATATYPE_INITIALIZER_LONG                OPAL_DATATYPE_INITIALIZER_INT8
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_LONG       OPAL_DATATYPE_INITIALIZER_UINT8
#elif SIZEOF_LONG == 16
#define OMPI_DATATYPE_INITIALIZER_LONG                OPAL_DATATYPE_INITIALIZER_INT16
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_LONG       OPAL_DATATYPE_INITIALIZER_UINT16
#endif


#ifdef HAVE_LONG_LONG
#if SIZEOF_LONG_LONG == 4
#define OMPI_DATATYPE_INITIALIZER_LONG_LONG           OPAL_DATATYPE_INITIALIZER_INT4
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_LONG_LONG  OPAL_DATATYPE_INITIALIZER_UINT4
#elif SIZEOF_LONG_LONG == 8
#define OMPI_DATATYPE_INITIALIZER_LONG_LONG           OPAL_DATATYPE_INITIALIZER_INT8
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_LONG_LONG  OPAL_DATATYPE_INITIALIZER_UINT8
#elif SIZEOF_LONG_LONG == 16
#define OMPI_DATATYPE_INITIALIZER_LONG_LONG           OPAL_DATATYPE_INITIALIZER_INT16
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_LONG_LONG  OPAL_DATATYPE_INITIALIZER_UINT16
#endif

#else /* HAVE_LONG_LONG */

#define OMPI_DATATYPE_INITIALIZER_LONG_LONG           OPAL_DATATYPE_INIT_UNAVAILABLE (LONG_LONG, OMPI_DATATYPE_FLAG_DATA_C)
#define OMPI_DATATYPE_INITIALIZER_UNSIGNED_LONG_LONG  OPAL_DATATYPE_INIT_UNAVAILABLE (UNSIGNED_LONG_LONG, OMPI_DATATYPE_FLAG_DATA_C)

#endif /* HAVE_LONG_LONG */

#if SIZEOF_FLOAT == 2
#define OMPI_DATATYPE_INITIALIZER_FLOAT               OPAL_DATATYPE_INITIALIZER_FLOAT2
#elif SIZEOF_FLOAT == 4
#define OMPI_DATATYPE_INITIALIZER_FLOAT               OPAL_DATATYPE_INITIALIZER_FLOAT4
#elif SIZEOF_FLOAT == 8
#define OMPI_DATATYPE_INITIALIZER_FLOAT               OPAL_DATATYPE_INITIALIZER_FLOAT8
#endif


#if SIZEOF_DOUBLE == 4
#define OMPI_DATATYPE_INITIALIZER_DOUBLE              OPAL_DATATYPE_INITIALIZER_FLOAT4
#elif SIZEOF_DOUBLE == 8
#define OMPI_DATATYPE_INITIALIZER_DOUBLE              OPAL_DATATYPE_INITIALIZER_FLOAT8
#elif SIZEOF_DOUBLE == 12
#define OMPI_DATATYPE_INITIALIZER_DOUBLE              OPAL_DATATYPE_INITIALIZER_FLOAT12
#elif SIZEOF_DOUBLE == 16
#define OMPI_DATATYPE_INITIALIZER_DOUBLE              OPAL_DATATYPE_INITIALIZER_FLOAT16
#endif


#ifdef HAVE_LONG_DOUBLE
#if SIZEOF_LONG_DOUBLE == 4
#define OMPI_DATATYPE_INITIALIZER_LONG_DOUBLE         OPAL_DATATYPE_INITIALIZER_FLOAT4
#elif SIZEOF_LONG_DOUBLE == 8
#define OMPI_DATATYPE_INITIALIZER_LONG_DOUBLE         OPAL_DATATYPE_INITIALIZER_FLOAT8
#elif SIZEOF_LONG_DOUBLE == 12
#define OMPI_DATATYPE_INITIALIZER_LONG_DOUBLE         OPAL_DATATYPE_INITIALIZER_FLOAT12
#elif SIZEOF_LONG_DOUBLE == 16
#define OMPI_DATATYPE_INITIALIZER_LONG_DOUBLE         OPAL_DATATYPE_INITIALIZER_FLOAT16
#endif

#else /* HAVE_LONG_DOUBLE */

#define OMPI_DATATYPE_INITIALIZER_LONG_DOUBLE         OMPI_DATATYPE_INIT_UNAVAILABLE(LONG_DOUBLE, OMPI_DATATYPE_FLAG_DATA_C)

#endif  /* HAVE_LONG_DOUBLE */

#define OMPI_DATATYPE_INITIALIZER_PACKED              OPAL_DATATYPE_INITIALIZER_UINT1

#define OMPI_DATATYPE_INITIALIZER_BOOL                OPAL_DATATYPE_INITIALIZER_BOOL

#define OMPI_DATATYPE_INITIALIZER_WCHAR               OPAL_DATATYPE_INITIALIZER_WCHAR

/*
 * Following are the structured types, that cannot be represented
 * by one single OPAL basic type
 */
#define OMPI_DATATYPE_FIRST_TYPE                      OPAL_DATATYPE_MAX_PREDEFINED
#define OMPI_DATATYPE_COMPLEX                         (OMPI_DATATYPE_FIRST_TYPE+0)   /* Equal to OMPI_DATATYPE_FIRST_TYPE */
#define OMPI_DATATYPE_DOUBLE_COMPLEX                  (OMPI_DATATYPE_FIRST_TYPE+1)
#define OMPI_DATATYPE_LONG_DOUBLE_COMPLEX             (OMPI_DATATYPE_FIRST_TYPE+2)

#define OMPI_DATATYPE_COMPLEX8                        (OMPI_DATATYPE_FIRST_TYPE+3)
#define OMPI_DATATYPE_COMPLEX16                       (OMPI_DATATYPE_FIRST_TYPE+4)
#define OMPI_DATATYPE_COMPLEX32                       (OMPI_DATATYPE_FIRST_TYPE+5)

/*
 * Derived datatypes supposely contiguous
 */
#define OMPI_DATATYPE_2INT                            (OMPI_DATATYPE_FIRST_TYPE+6)
#define OMPI_DATATYPE_2INTEGER                        (OMPI_DATATYPE_FIRST_TYPE+7)
#define OMPI_DATATYPE_2REAL                           (OMPI_DATATYPE_FIRST_TYPE+8)
#define OMPI_DATATYPE_2DBLPREC                        (OMPI_DATATYPE_FIRST_TYPE+9)
#define OMPI_DATATYPE_2COMPLEX                        (OMPI_DATATYPE_FIRST_TYPE+10)
#define OMPI_DATATYPE_2DOUBLE_COMPLEX                 (OMPI_DATATYPE_FIRST_TYPE+11)
/*
 * Derived datatypes which will definitively be non contiguous on some architectures.
 */
#define OMPI_DATATYPE_FLOAT_INT                       (OMPI_DATATYPE_FIRST_TYPE+12)
#define OMPI_DATATYPE_DOUBLE_INT                      (OMPI_DATATYPE_FIRST_TYPE+13)
#define OMPI_DATATYPE_LONG_DOUBLE_INT                 (OMPI_DATATYPE_FIRST_TYPE+14)
#define OMPI_DATATYPE_SHORT_INT                       (OMPI_DATATYPE_FIRST_TYPE+15)
#define OMPI_DATATYPE_LONG_INT                        (OMPI_DATATYPE_FIRST_TYPE+16)

/* Used locally as a nice marker */
#define OMPI_DATATYPE_UNAVAILABLE                     (OMPI_DATATYPE_FIRST_TYPE+17)

/* If the number of basic datatype should change update OMPI_DATATYPE_MAX_PREDEFINED in ompi_datatype.h */
#if OMPI_DATATYPE_MAX_PREDEFINED <= OMPI_DATATYPE_UNAVAILABLE
#error OMPI_DATATYPE_MAX_PREDEFINED should be updated to the next value after the OMPI_DATATYPE_UNAVAILABLE define
#endif  /* safe check for max predefined datatypes. */

#endif /* OMPI_DATATYPE_INTERNAL_H */
