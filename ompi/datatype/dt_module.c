/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/datatype_internal.h"
#include "ompi/datatype/convertor_internal.h"

#if OMPI_ENABLE_DEBUG
#include "opal/mca/base/mca_base_param.h"
int ompi_unpack_debug   = 0;
int ompi_pack_debug     = 0;
int ompi_copy_debug     = 0;
int ompi_position_debug = 0;
#endif  /* OMPI_ENABLE_DEBUG */

extern size_t ompi_datatype_memcpy_block_size;

/* by default the debuging is turned off */
int ompi_ddt_dfd = -1;

/* other fields starting after bdt_used (index of DT_LOOP should be ONE) */
#define ZERO_DDT_ARRAY { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,             \
            0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define EMPTY_DATA(NAME) NULL, 0, "MPI_" # NAME, {0, 0, NULL}, {0, 0, NULL}, NULL, NULL, ZERO_DDT_ARRAY
#define BASEOBJ_DATA { OBJ_CLASS(ompi_datatype_t), 1 }

/* Using this macro implies that at this point not all informations needed
 * to fill up the datatype are known. We fill them with zeros and then later
 * when the datatype engine will be initialized we complete with the
 * correct information. This macro should be used for all composed types.
 */
#define INIT_BASIC_TYPE( TYPE, NAME )                                        \
    { BASEOBJ_DATA, 0/*size*/, 0 /*align*/, 0/*true_lb*/, 0/*true_ub*/,      \
            0/*lb*/, 0/*ub*/, DT_FLAG_PREDEFINED, TYPE, 1,                   \
      (((uint64_t)1)<<(TYPE)), EMPTY_DATA(NAME) }

#define INIT_BASIC_DATA( TYPE, ALIGN, NAME, FLAGS )                                \
    { BASEOBJ_DATA, sizeof(TYPE), ALIGN, 0, sizeof(TYPE),                          \
            0, sizeof(TYPE), DT_FLAG_BASIC | (FLAGS),                              \
            DT_##NAME, 1, (((uint64_t)1)<<(DT_##NAME)), EMPTY_DATA(NAME) }

#define INIT_BASIC_DATA_WITH_NAME( TYPE, ALIGN, INTERNAL_NAME, NAME, FLAGS )       \
    { BASEOBJ_DATA, sizeof(TYPE), ALIGN, 0, sizeof(TYPE),                          \
            0, sizeof(TYPE), DT_FLAG_BASIC | (FLAGS),                              \
            DT_##INTERNAL_NAME, 1, (((uint64_t)1)<<(DT_##INTERNAL_NAME)),          \
            EMPTY_DATA(NAME) }

#define INIT_UNAVAILABLE_DATA( NAME )                                           \
    { BASEOBJ_DATA, 0, 0, 0, 0, 0, 0, DT_FLAG_UNAVAILABLE | DT_FLAG_PREDEFINED, \
            DT_UNAVAILABLE, 1, 0, EMPTY_DATA( "UNAVAILABLE_" # NAME ) }

/* The upper bound and the true UB are set to the size of the datatype.
 * If it's not the case then they should be modified in the initialization
 * function.
 */
#if OMPI_WANT_F77_BINDINGS
#define INIT_BASIC_FORTRAN_TYPE( TYPE, NAME, SIZE, ALIGN, FLAGS )                                  \
    { BASEOBJ_DATA, SIZE, ALIGN, 0/*true_lb*/, SIZE/*true_ub*/,                                    \
            0/*lb*/, SIZE/*ub*/,                                                                   \
            DT_FLAG_BASIC | DT_FLAG_DATA_FORTRAN | (FLAGS), \
            (TYPE), 1, (((uint64_t)1)<<(TYPE)), EMPTY_DATA(NAME) }
#else
#define INIT_BASIC_FORTRAN_TYPE( TYPE, NAME, SIZE, ALIGN, FLAGS )                                          \
    INIT_BASIC_TYPE( TYPE, NAME )
#endif  /* OMPI_WANT_F77_BINDINGS */

/**
 * This is the number of predefined datatypes. It is different than the MAX_PREDEFINED
 * as it include all the optional datatypes (such as MPI_INTEGER?, MPI_REAL?).
 */
int32_t ompi_ddt_number_of_predefined_data = 0;

OMPI_DECLSPEC ompi_datatype_t ompi_mpi_datatype_null =
    { BASEOBJ_DATA, 0, 0, 0, 0,
      0, 0, DT_FLAG_PREDEFINED, 0, 1,
      ((long long)0), EMPTY_DATA(DATATYPE_NULL) };

OMPI_DECLSPEC ompi_datatype_t ompi_mpi_loop = INIT_BASIC_TYPE( DT_LOOP, LOOP );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_end_loop = INIT_BASIC_TYPE( DT_END_LOOP, END_LOOP );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_ub = INIT_BASIC_TYPE( DT_UB, UB );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_lb = INIT_BASIC_TYPE( DT_LB, LB );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_char = INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, CHAR, DT_FLAG_DATA_C );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_character = INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, CHARACTER, DT_FLAG_DATA_FORTRAN );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_unsigned_char = INIT_BASIC_DATA( unsigned char, OMPI_ALIGNMENT_CHAR, UNSIGNED_CHAR, DT_FLAG_DATA_C );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_signed_char = INIT_BASIC_DATA( signed char, OMPI_ALIGNMENT_CHAR, SIGNED_CHAR, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_byte = INIT_BASIC_DATA( unsigned char, OMPI_ALIGNMENT_CHAR, BYTE, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_short = INIT_BASIC_DATA( short, OMPI_ALIGNMENT_SHORT, SHORT, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_unsigned_short = INIT_BASIC_DATA( unsigned short, OMPI_ALIGNMENT_SHORT, UNSIGNED_SHORT, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_int = INIT_BASIC_DATA( int, OMPI_ALIGNMENT_INT, INT, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_unsigned = INIT_BASIC_DATA_WITH_NAME( unsigned int, OMPI_ALIGNMENT_INT, UNSIGNED_INT, UNSIGNED, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_long = INIT_BASIC_DATA( long, OMPI_ALIGNMENT_LONG, LONG, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_unsigned_long = INIT_BASIC_DATA( unsigned long, OMPI_ALIGNMENT_LONG, UNSIGNED_LONG, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
#if HAVE_LONG_LONG
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_long_long_int = INIT_BASIC_DATA_WITH_NAME( long long, OMPI_ALIGNMENT_LONG_LONG, LONG_LONG_INT, LONG_LONG, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_unsigned_long_long = INIT_BASIC_DATA( unsigned long long, OMPI_ALIGNMENT_LONG_LONG, UNSIGNED_LONG_LONG, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_long_long_int = INIT_UNAVAILABLE_DATA( LONG_LONG_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_unsigned_long_long = INIT_UNAVAILABLE_DATA( UNIGNED_LONG_LONG );
#endif  /* HAVE_LONG_LONG */
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_float = INIT_BASIC_DATA( float, OMPI_ALIGNMENT_FLOAT, FLOAT, DT_FLAG_DATA_C | DT_FLAG_DATA_FLOAT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_double = INIT_BASIC_DATA( double, OMPI_ALIGNMENT_DOUBLE, DOUBLE, DT_FLAG_DATA_C | DT_FLAG_DATA_FLOAT );
#if HAVE_LONG_DOUBLE
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_long_double = INIT_BASIC_DATA( long double, OMPI_ALIGNMENT_LONG_DOUBLE, LONG_DOUBLE, DT_FLAG_DATA_C | DT_FLAG_DATA_FLOAT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_long_double = INIT_UNAVAILABLE_DATA( LONG_DOUBLE );
#endif  /* HAVE_LONG_DOUBLE */
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_packed = INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, PACKED, 0 );
#if OMPI_ALIGNMENT_WCHAR != 0
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_wchar = INIT_BASIC_DATA( wchar_t, OMPI_ALIGNMENT_WCHAR, WCHAR, DT_FLAG_DATA_C );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_wchar = INIT_UNAVAILABLE_DATA( WCHAR );
#endif  /* FTMPI_HAVE_WCHAR_T */

OMPI_DECLSPEC ompi_datatype_t ompi_mpi_cxx_bool = INIT_BASIC_DATA( bool, OMPI_ALIGNMENT_CXX_BOOL, CXX_BOOL, DT_FLAG_DATA_CPP );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_logic = INIT_BASIC_FORTRAN_TYPE( DT_LOGIC, LOGIC, OMPI_SIZEOF_FORTRAN_LOGICAL, OMPI_ALIGNMENT_FORTRAN_LOGICAL, 0 );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer = INIT_BASIC_FORTRAN_TYPE( DT_INTEGER, INTEGER, OMPI_SIZEOF_FORTRAN_INTEGER, OMPI_ALIGNMENT_FORTRAN_INTEGER, DT_FLAG_DATA_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_real = INIT_BASIC_FORTRAN_TYPE( DT_REAL, REAL, OMPI_SIZEOF_FORTRAN_REAL, OMPI_ALIGNMENT_FORTRAN_REAL, DT_FLAG_DATA_FLOAT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_dblprec = INIT_BASIC_FORTRAN_TYPE( DT_DBLPREC, DOUBLE_PRECISION, OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION, OMPI_ALIGNMENT_FORTRAN_DOUBLE_PRECISION, DT_FLAG_DATA_FLOAT );

#if HAVE_LONG_DOUBLE
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_ldblcplex = INIT_BASIC_DATA( ompi_complex_long_double_t, OMPI_ALIGNMENT_LONG_DOUBLE, COMPLEX_LONG_DOUBLE, DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_COMPLEX );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_ldblcplex = INIT_UNAVAILABLE_DATA( COMPLEX_LONG_DOUBLE );
#endif  /* HAVE_LONG_DOUBLE */

OMPI_DECLSPEC ompi_datatype_t ompi_mpi_cplex = INIT_BASIC_DATA( ompi_complex_float_t, OMPI_ALIGNMENT_FLOAT, COMPLEX_FLOAT, DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_COMPLEX );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_dblcplex = INIT_BASIC_DATA( ompi_complex_double_t, OMPI_ALIGNMENT_DOUBLE, COMPLEX_DOUBLE, DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_COMPLEX );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_float_int = INIT_BASIC_TYPE( DT_FLOAT_INT, FLOAT_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_double_int = INIT_BASIC_TYPE( DT_DOUBLE_INT, DOUBLE_INT );
#if HAVE_LONG_DOUBLE
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_longdbl_int = INIT_BASIC_TYPE( DT_LONG_DOUBLE_INT, LONG_DOUBLE_INT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_longdbl_int = INIT_UNAVAILABLE_DATA( LONG_DOUBLE_INT );
#endif  /* HAVE_LONG_DOUBLE */
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_long_int = INIT_BASIC_TYPE( DT_LONG_INT, LONG_INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_short_int = INIT_BASIC_TYPE( DT_SHORT_INT, SHORT_INT );

OMPI_DECLSPEC ompi_datatype_t ompi_mpi_2int = INIT_BASIC_TYPE( DT_2INT, 2INT );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_2real = INIT_BASIC_TYPE( DT_2REAL, 2REAL );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_2dblprec = INIT_BASIC_TYPE( DT_2DBLPREC, 2DBLPREC );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_2integer = INIT_BASIC_TYPE( DT_2INTEGER, 2INTEGER );

OMPI_DECLSPEC ompi_datatype_t ompi_mpi_cxx_cplex = INIT_BASIC_DATA( ompi_complex_float_t, OMPI_ALIGNMENT_FLOAT, COMPLEX_FLOAT, DT_FLAG_DATA_CPP | DT_FLAG_DATA_COMPLEX );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_cxx_dblcplex = INIT_BASIC_DATA( ompi_complex_double_t, OMPI_ALIGNMENT_DOUBLE, COMPLEX_DOUBLE, DT_FLAG_DATA_CPP | DT_FLAG_DATA_COMPLEX );
#if HAVE_LONG_DOUBLE
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_cxx_ldblcplex = INIT_BASIC_DATA( ompi_complex_long_double_t, OMPI_ALIGNMENT_LONG_DOUBLE, COMPLEX_LONG_DOUBLE, DT_FLAG_DATA_CPP | DT_FLAG_DATA_COMPLEX );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_cxx_ldblcplex = INIT_UNAVAILABLE_DATA( COMPLEX_LONG_DOUBLE );
#endif  /* HAVE_LONG_DOUBLE */

OMPI_DECLSPEC ompi_datatype_t ompi_mpi_2cplex = INIT_BASIC_TYPE( DT_2COMPLEX, 2COMPLEX );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_2dblcplex = INIT_BASIC_TYPE( DT_2DOUBLE_COMPLEX, 2DOUBLE_COMPLEX );
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_unavailable = INIT_UNAVAILABLE_DATA( UNAVAILABLE );

#if OMPI_HAVE_FORTRAN_REAL4
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_real4 = INIT_BASIC_FORTRAN_TYPE( DT_FLOAT, REAL4, OMPI_SIZEOF_FORTRAN_REAL4, OMPI_ALIGNMENT_FORTRAN_REAL4, DT_FLAG_DATA_FLOAT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_real4 = INIT_UNAVAILABLE_DATA( REAL4 );
#endif
#if OMPI_HAVE_FORTRAN_REAL8
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_real8 = INIT_BASIC_FORTRAN_TYPE( DT_DOUBLE, REAL8, OMPI_SIZEOF_FORTRAN_REAL8, OMPI_ALIGNMENT_FORTRAN_REAL8, DT_FLAG_DATA_FLOAT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_real8 = INIT_UNAVAILABLE_DATA( REAL8 );
#endif
#if OMPI_HAVE_FORTRAN_REAL16
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_real16 = INIT_BASIC_FORTRAN_TYPE( DT_LONG_DOUBLE, REAL16, OMPI_SIZEOF_FORTRAN_REAL16, OMPI_ALIGNMENT_FORTRAN_REAL16, DT_FLAG_DATA_FLOAT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_real16 = INIT_UNAVAILABLE_DATA( REAL16 );
#endif

#if OMPI_HAVE_FORTRAN_INTEGER1
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer1 = INIT_BASIC_FORTRAN_TYPE( DT_CHAR, INTEGER1, OMPI_SIZEOF_FORTRAN_INTEGER1, OMPI_ALIGNMENT_FORTRAN_INTEGER1, DT_FLAG_DATA_INT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer1 = INIT_UNAVAILABLE_DATA( INTEGER1 );
#endif
#if OMPI_HAVE_FORTRAN_INTEGER2
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer2 = INIT_BASIC_FORTRAN_TYPE( DT_SHORT, INTEGER2, OMPI_SIZEOF_FORTRAN_INTEGER2, OMPI_ALIGNMENT_FORTRAN_INTEGER2, DT_FLAG_DATA_INT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer2 = INIT_UNAVAILABLE_DATA( INTEGER2 );
#endif
#if OMPI_HAVE_FORTRAN_INTEGER4
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer4 = INIT_BASIC_FORTRAN_TYPE( DT_INT, INTEGER4, OMPI_SIZEOF_FORTRAN_INTEGER4, OMPI_ALIGNMENT_FORTRAN_INTEGER4, DT_FLAG_DATA_INT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer4 = INIT_UNAVAILABLE_DATA( INTEGER4 );
#endif
#if OMPI_HAVE_FORTRAN_INTEGER8
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer8 = INIT_BASIC_FORTRAN_TYPE( DT_LONG_LONG_INT, INTEGER8, OMPI_SIZEOF_FORTRAN_INTEGER8, OMPI_ALIGNMENT_FORTRAN_INTEGER8, DT_FLAG_DATA_INT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer8 = INIT_UNAVAILABLE_DATA( INTEGER8 );
#endif
#if OMPI_HAVE_FORTRAN_INTEGER16
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer16 = INIT_BASIC_FORTRAN_TYPE( DT_LONG_LONG_INT, INTEGER16, OMPI_SIZEOF_FORTRAN_INTEGER16, OMPI_ALIGNMENT_FORTRAN_INTEGER16, DT_FLAG_DATA_INT );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_integer16 = INIT_UNAVAILABLE_DATA( INTEGER16 );
#endif

#if OMPI_HAVE_FORTRAN_REAL4 && OMPI_HAVE_FORTRAN_COMPLEX8
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_complex8 = INIT_BASIC_FORTRAN_TYPE( DT_COMPLEX_FLOAT, COMPLEX8, OMPI_SIZEOF_FORTRAN_COMPLEX, OMPI_ALIGNMENT_FORTRAN_REAL, DT_FLAG_DATA_COMPLEX );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_complex8 = INIT_UNAVAILABLE_DATA( COMPLEX8 );
#endif
#if OMPI_HAVE_FORTRAN_REAL8 && OMPI_HAVE_FORTRAN_COMPLEX16
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_complex16 = INIT_BASIC_FORTRAN_TYPE( DT_COMPLEX_DOUBLE, COMPLEX16, OMPI_SIZEOF_FORTRAN_COMPLEX16, OMPI_ALIGNMENT_FORTRAN_COMPLEX16, DT_FLAG_DATA_COMPLEX );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_complex16 = INIT_UNAVAILABLE_DATA( COMPLEX16 );
#endif
#if OMPI_HAVE_FORTRAN_REAL16 && OMPI_HAVE_FORTRAN_COMPLEX32
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_complex32 = INIT_BASIC_FORTRAN_TYPE( DT_COMPLEX_LONG_DOUBLE, COMPLEX32, OMPI_SIZEOF_FORTRAN_COMPLEX32, OMPI_ALIGNMENT_FORTRAN_COMPLEX32, DT_FLAG_DATA_COMPLEX );
#else
OMPI_DECLSPEC ompi_datatype_t ompi_mpi_complex32 = INIT_UNAVAILABLE_DATA( COMPLEX32 );
#endif

/*
 * NOTE: The order of this array *MUST* match what is listed in
 * datatype_internal.h
 */
const ompi_datatype_t* ompi_ddt_basicDatatypes[DT_MAX_PREDEFINED] = {
    &ompi_mpi_loop,
    &ompi_mpi_end_loop,
    &ompi_mpi_lb,
    &ompi_mpi_ub,
    &ompi_mpi_char,
    &ompi_mpi_character,
    &ompi_mpi_unsigned_char,
    &ompi_mpi_signed_char,
    &ompi_mpi_byte,
    &ompi_mpi_short,
    &ompi_mpi_unsigned_short,
    &ompi_mpi_int,
    &ompi_mpi_unsigned,
    &ompi_mpi_long,
    &ompi_mpi_unsigned_long,
    &ompi_mpi_long_long_int,
    &ompi_mpi_unsigned_long_long,
    &ompi_mpi_float,
    &ompi_mpi_double,
    &ompi_mpi_long_double,
    &ompi_mpi_packed,
    &ompi_mpi_wchar,
    &ompi_mpi_cxx_bool,
    &ompi_mpi_logic,
    &ompi_mpi_integer,
    &ompi_mpi_real,
    &ompi_mpi_dblprec,
    &ompi_mpi_cplex,
    &ompi_mpi_dblcplex,
    &ompi_mpi_ldblcplex,
    &ompi_mpi_2int,
    &ompi_mpi_2integer,
    &ompi_mpi_2real,
    &ompi_mpi_2dblprec,
    &ompi_mpi_2cplex,
    &ompi_mpi_2dblcplex,
    &ompi_mpi_float_int,
    &ompi_mpi_double_int,
    &ompi_mpi_longdbl_int,
    &ompi_mpi_long_int,
    &ompi_mpi_short_int,
    &ompi_mpi_unavailable
};

ompi_pointer_array_t *ompi_datatype_f_to_c_table = NULL;

size_t ompi_ddt_local_sizes[DT_MAX_PREDEFINED];

#define COPY_DATA_DESC( PDST, PSRC )                                    \
    do {                                                                \
        (PDST)->size     = (PSRC)->size;                                \
        (PDST)->true_lb  = (PSRC)->true_lb;                             \
        (PDST)->true_ub  = (PSRC)->true_ub;                             \
        (PDST)->align    = (PSRC)->align;                               \
        (PDST)->lb       = (PSRC)->lb;                                  \
        (PDST)->ub       = (PSRC)->ub;                                  \
        (PDST)->flags    = (PSRC)->flags;                               \
        (PDST)->id       = (PSRC)->id;                                  \
        (PDST)->nbElems  = (PSRC)->nbElems;                             \
        (PDST)->bdt_used = (PSRC)->bdt_used;                            \
        if( NULL != (PDST)->desc.desc )                                 \
            free( (PDST)->desc.desc );                                  \
        /* Don't re-assing the (PDST)->desc because we need the pointer \
         * value in the following if statement. In the case of          \
         * predefined datatypes both descriptors point to the same      \
         * memory and if we free the memory twice bad things happen.*/  \
        if( (NULL != (PDST)->opt_desc.desc) &&                          \
            ((PDST)->opt_desc.desc != (PDST)->desc.desc) )              \
            free( (PDST)->opt_desc.desc );                              \
        (PDST)->desc     = (PSRC)->desc;                                \
        (PDST)->opt_desc = (PSRC)->opt_desc;                            \
        (PDST)->packed_description = (PSRC)->packed_description;        \
        (PSRC)->packed_description = NULL;                              \
        memcpy( (PDST)->btypes, (PSRC)->btypes,                         \
                DT_MAX_PREDEFINED * sizeof(uint32_t) );                 \
    } while(0)

#define DECLARE_MPI2_COMPOSED_STRUCT_DDT( PDATA, MPIDDT, MPIDDTNAME, type1, type2, MPIType1, MPIType2, FLAGS) \
    do {                                                                \
        struct { type1 v1; type2 v2; } s[2];                            \
        ompi_datatype_t* types[2];                                      \
        ompi_datatype_t* ptype;                                         \
        int bLength[2] = {1, 1};                                        \
        MPI_Aint base, displ[2];                                       \
                                                                        \
        types[0] = (ompi_datatype_t*)ompi_ddt_basicDatatypes[MPIType1]; \
        types[1] = (ompi_datatype_t*)ompi_ddt_basicDatatypes[MPIType2]; \
        base = (ptrdiff_t)(&(s[0]));                                    \
        displ[0] = (ptrdiff_t)(&(s[0].v1));                             \
        displ[0] -= base;                                               \
        displ[1] = (ptrdiff_t)(&(s[0].v2));                             \
        displ[1] -= base;                                               \
                                                                        \
        ompi_ddt_create_struct( 2, bLength, displ, types, &ptype );     \
        displ[0] = (ptrdiff_t)(&(s[1]));                                \
        displ[0] -= base;                                               \
        if( displ[0] != (displ[1] + (ptrdiff_t)sizeof(type2)) )         \
            ptype->ub = displ[0];  /* force a new extent for the datatype */ \
        ptype->flags |= (FLAGS);                                        \
        ptype->id = MPIDDT;                                             \
        ompi_ddt_commit( &ptype );                                      \
        COPY_DATA_DESC( PDATA, ptype );                                 \
        (PDATA)->flags |= DT_FLAG_PREDEFINED;                           \
        ptype->desc.desc = NULL;                                        \
        ptype->opt_desc.desc = NULL;                                    \
        OBJ_RELEASE( ptype );                                           \
        strncpy( (PDATA)->name, MPIDDTNAME, MPI_MAX_OBJECT_NAME );      \
    } while(0)

#define DECLARE_MPI2_COMPOSED_BLOCK_DDT( PDATA, MPIDDT, MPIDDTNAME, MPIType, FLAGS ) \
    do {                                                                             \
        ompi_datatype_t *ptype;                                                      \
        ompi_ddt_create_contiguous( 2, ompi_ddt_basicDatatypes[MPIType], &ptype );   \
        ptype->flags |= (FLAGS);                                                     \
        ptype->id = (MPIDDT);                                                        \
        ompi_ddt_commit( &ptype );                                                   \
        COPY_DATA_DESC( (PDATA), ptype );                                            \
        (PDATA)->flags |= DT_FLAG_PREDEFINED;                                        \
        ptype->desc.desc = NULL;                                                     \
        ptype->opt_desc.desc = NULL;                                                 \
        OBJ_RELEASE( ptype );                                                        \
        strncpy( (PDATA)->name, (MPIDDTNAME), MPI_MAX_OBJECT_NAME );                 \
    } while(0)

#define DECLARE_MPI_SYNONYM_DDT( PDATA, MPIDDTNAME, PORIGDDT)           \
    do {                                                                \
        /* just memcpy as it's easier this way */                       \
        memcpy( (PDATA), (PORIGDDT), sizeof(ompi_datatype_t) );         \
        strncpy( (PDATA)->name, MPIDDTNAME, MPI_MAX_OBJECT_NAME );      \
        /* forget the language flag */                                  \
        (PDATA)->flags &= ~DT_FLAG_DATA_LANGUAGE;                       \
    } while(0)

int ompi_ddt_register_params(void)
{
#if OMPI_ENABLE_DEBUG
    mca_base_param_reg_int_name( "mpi", "ddt_unpack_debug",
                                 "Whether to output debugging information in the ddt unpack functions (nonzero = enabled)",
                                 false, false,
                                 ompi_unpack_debug, &ompi_unpack_debug );
    mca_base_param_reg_int_name( "mpi", "ddt_pack_debug", 
                                 "Whether to output debugging information in the ddt pack functions (nonzero = enabled)",
                                 false, false,
                                 ompi_pack_debug, &ompi_pack_debug );
    mca_base_param_reg_int_name( "mpi", "ddt_position_debug",
                                 "Non zero lead to output generated by the datatype position functions",
                                 false, false, 0, &ompi_position_debug );

    mca_base_param_reg_int_name( "mpi", "ddt_copy_debug",
                                 "Whether to output debugging information in the ddt copy functions (nonzero = enabled)",
                                 false, false, 
                                 ompi_copy_debug, &ompi_copy_debug );
#endif  /* OMPI_ENABLE_DEBUG */
    return OMPI_SUCCESS;
}

int32_t ompi_ddt_init( void )
{
    int32_t i;

    for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
        ompi_datatype_t* datatype = (ompi_datatype_t*)ompi_ddt_basicDatatypes[i];

        datatype->desc.desc = (dt_elem_desc_t*)malloc(2*sizeof(dt_elem_desc_t));
        datatype->desc.desc[0].elem.common.flags = DT_FLAG_PREDEFINED | DT_FLAG_DATA | DT_FLAG_CONTIGUOUS;
        datatype->desc.desc[0].elem.common.type  = i;
        datatype->desc.desc[0].elem.count        = 1;
        datatype->desc.desc[0].elem.disp         = 0;
        datatype->desc.desc[0].elem.extent       = datatype->size;

        datatype->desc.desc[1].end_loop.common.flags    = 0;
        datatype->desc.desc[1].end_loop.common.type     = DT_END_LOOP;
        datatype->desc.desc[1].end_loop.items           = 1;
        datatype->desc.desc[1].end_loop.first_elem_disp = datatype->desc.desc[0].elem.disp;
        datatype->desc.desc[1].end_loop.size            = datatype->size;

        datatype->desc.length       = 1;
        datatype->desc.used         = 1;
        /* By default the optimized descritption is the same as the default
         * description for predefined datatypes.
         */
        datatype->opt_desc          = datatype->desc;

        datatype->btypes[i]         = 1;
        /* Check if the data contain gaps */
        if( (datatype->ub - datatype->lb) == (ptrdiff_t)datatype->size ) {
            datatype->desc.desc[0].elem.common.flags |= DT_FLAG_NO_GAPS;
        }
    }

    /* Create the f2c translation table */
    ompi_datatype_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
    if (NULL == ompi_datatype_f_to_c_table) {
        return OMPI_ERROR;
    }
    /* All temporary datatypes created on the following statement will get registered
     * on the f2c table. But as they get destroyed they will (hopefully) get unregistered
     * so later when we start registering the real datatypes they will get the index
     * in mpif.h
     */

    /* the 2 complex datatypes (float and double) */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_cplex, DT_COMPLEX_FLOAT, "MPI_COMPLEX",
				      float, float, DT_FLOAT, DT_FLOAT,
				      DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_dblcplex, DT_COMPLEX_DOUBLE, "MPI_DOUBLE_COMPLEX",
				      double, double, DT_DOUBLE, DT_DOUBLE,
				      DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT );
#if HAVE_LONG_DOUBLE
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_ldblcplex, DT_COMPLEX_LONG_DOUBLE, "MPI_LONG_DOUBLE_COMPLEX",
				      long double, long double, DT_LONG_DOUBLE, DT_LONG_DOUBLE,
				      DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT );
#endif  /* HAVE_LONG_DOUBLE */

    /* Now the predefined MPI2 datatypes (they should last forever!) */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_float_int, DT_FLOAT_INT, "MPI_FLOAT_INT",
				      float, int, DT_FLOAT, DT_INT, DT_FLAG_DATA_C );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_double_int, DT_DOUBLE_INT, "MPI_DOUBLE_INT",
				      double, int, DT_DOUBLE, DT_INT, DT_FLAG_DATA_C );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_long_int, DT_LONG_INT, "MPI_LONG_INT",
				      long, int, DT_LONG, DT_INT, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_short_int, DT_SHORT_INT, "MPI_SHORT_INT",
				      short, int, DT_SHORT, DT_INT, DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_longdbl_int, DT_LONG_DOUBLE_INT, "MPI_LONG_DOUBLE_INT",
				      long double, int, DT_LONG_DOUBLE, DT_INT, DT_FLAG_DATA_C );

    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2int, DT_2INT, "MPI_2INT", DT_INT,
				     DT_FLAG_DATA_C | DT_FLAG_DATA_INT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2integer, DT_2INTEGER, "MPI_2INTEGER", DT_INT,
				     DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_INT);
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2real, DT_2REAL, "MPI_2REAL", DT_FLOAT,
				     DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2dblprec, DT_2DBLPREC, "MPI_2DOUBLE_PRECISION", DT_DOUBLE,
				     DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2cplex, DT_2COMPLEX, "MPI_2COMPLEX", DT_COMPLEX_FLOAT,
				     DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT);
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2dblcplex, DT_2DOUBLE_COMPLEX, "MPI_2DOUBLE_COMPLEX",
                                     DT_COMPLEX_DOUBLE, DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT );

    for( i = 0; i < DT_MAX_PREDEFINED; ++i ) {
        ompi_ddt_local_sizes[i] = ompi_ddt_basicDatatypes[i]->size;
    }

    /* Copy the desc pointer from the <DT_MAX_PREDEFINED datatypes to
       the synonym types */

    /* C++ complex types */
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_cxx_cplex, "MPI_CXX_COMPLEX", &ompi_mpi_cplex );
    ompi_mpi_cxx_cplex.flags |= DT_FLAG_DATA_CPP | DT_FLAG_DATA_COMPLEX;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_cxx_dblcplex, "MPI_CXX_DOUBLE_COMPLEX", &ompi_mpi_dblcplex );
    ompi_mpi_cxx_dblcplex.flags |= DT_FLAG_DATA_CPP | DT_FLAG_DATA_COMPLEX;
#if HAVE_LONG_DOUBLE
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_cxx_ldblcplex, "MPI_CXX_LONG_DOUBLE_COMPLEX", &ompi_mpi_ldblcplex );
    ompi_mpi_cxx_ldblcplex.flags |= DT_FLAG_DATA_CPP | DT_FLAG_DATA_COMPLEX;
#endif  /* HAVE_LONG_DOUBLE */

    /* FORTRAN types */
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_real4, "MPI_REAL4", &ompi_mpi_float );
    ompi_mpi_real4.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_real8, "MPI_REAL8", &ompi_mpi_double );
    ompi_mpi_real8.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_real16, "MPI_REAL16", &ompi_mpi_long_double );
    ompi_mpi_real16.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_FLOAT;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_integer1, "MPI_INTEGER1", &ompi_mpi_char );
    ompi_mpi_integer1.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_INT;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_integer2, "MPI_INTEGER2", &ompi_mpi_short );
    ompi_mpi_integer2.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_INT;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_integer4, "MPI_INTEGER4", &ompi_mpi_int );
    ompi_mpi_integer4.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_INT;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_integer8, "MPI_INTEGER8", &ompi_mpi_long_long_int );
    ompi_mpi_integer8.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_INT;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_integer16, "MPI_INTEGER16", &ompi_mpi_unavailable );
    ompi_mpi_integer16.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_INT;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_complex8, "MPI_COMPLEX8", &ompi_mpi_cplex );
    ompi_mpi_complex8.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_COMPLEX;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_complex16, "MPI_COMPLEX16", &ompi_mpi_dblcplex );
    ompi_mpi_complex16.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_COMPLEX;
    DECLARE_MPI_SYNONYM_DDT( &ompi_mpi_complex32, "MPI_COMPLEX32", &ompi_mpi_ldblcplex );
    ompi_mpi_complex32.flags |= DT_FLAG_DATA_FORTRAN | DT_FLAG_DATA_COMPLEX;

    /* Start to populate the f2c index translation table */

    /* The order of the data registration should be the same as the
     * one in the mpif.h file. Any modification here should be
     * reflected there !!!  Do the Fortran types first so that mpif.h
     * can have consecutive, dense numbers. */ 

    /* This macro makes everything significantly easier to read below.
       All hail the moog!  :-) */

#define MOOG(name)                                                      \
    {                                                                   \
        ompi_mpi_##name.d_f_to_c_index =                                \
            ompi_pointer_array_add(ompi_datatype_f_to_c_table, &ompi_mpi_##name); \
        if( ompi_ddt_number_of_predefined_data < (ompi_mpi_##name).d_f_to_c_index ) \
            ompi_ddt_number_of_predefined_data = (ompi_mpi_##name).d_f_to_c_index; \
    }

    MOOG(datatype_null);
    MOOG(byte);
    MOOG(packed);
    MOOG(ub);
    MOOG(lb);
    MOOG(character);
    MOOG(logic);
    MOOG(integer);
    MOOG(integer1);
    MOOG(integer2);
    MOOG(integer4);
    MOOG(integer8);
    MOOG(integer16);
    MOOG(real);
    MOOG(real4);
    MOOG(real8);
    MOOG(real16);
    MOOG(dblprec);
    MOOG(cplex);
    MOOG(complex8);
    MOOG(complex16);
    MOOG(complex32);
    MOOG(dblcplex);
    MOOG(2real);
    MOOG(2dblprec);
    MOOG(2integer);
    MOOG(2cplex);
    MOOG(2dblcplex);

    /* Now the C types */

    MOOG(wchar);
    MOOG(char);
    MOOG(unsigned_char);
    MOOG(signed_char);
    MOOG(short);
    MOOG(unsigned_short);
    MOOG(int);
    MOOG(unsigned);
    MOOG(long);
    MOOG(unsigned_long);
    MOOG(long_long_int);
    MOOG(unsigned_long_long);

    MOOG(float);
    MOOG(double);
    MOOG(long_double);

    MOOG(float_int);
    MOOG(double_int);
    MOOG(longdbl_int);
    MOOG(long_int);
    MOOG(2int);
    MOOG(short_int);

    /* C++ types */

    MOOG(cxx_bool);
    MOOG(cxx_cplex);
    MOOG(cxx_dblcplex);
    MOOG(cxx_ldblcplex);

    for( i = 0; i < ompi_mpi_cxx_ldblcplex.d_f_to_c_index; i++ ) {
        ompi_datatype_t* datatype = (ompi_datatype_t*)ompi_pointer_array_get_item( ompi_datatype_f_to_c_table, i );

        if( (datatype->ub - datatype->lb) == (ptrdiff_t)datatype->size ) {
            datatype->flags |= DT_FLAG_NO_GAPS;
        } else {
            datatype->flags &= ~DT_FLAG_NO_GAPS;
        }
    }

    ompi_ddt_default_convertors_init();
    return OMPI_SUCCESS;
}

int32_t ompi_ddt_finalize( void )
{
    int i;

    /* As the synonyms are just copies of the internal data we should not free them.
     * Anyway they are over the limit of DT_MAX_PREDEFINED so they will never get freed.
     */

    /* As they are statically allocated they cannot be released. But we
     * can call OBJ_DESTRUCT, just to free all internally allocated ressources.
     */
    for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
        OBJ_DESTRUCT( ompi_ddt_basicDatatypes[i] );
    }

    /* Get rid of the Fortran2C translation table */
    OBJ_RELEASE(ompi_datatype_f_to_c_table);

#if defined(VERBOSE)
    if( ompi_ddt_dfd != -1 )
        opal_output_close( ompi_ddt_dfd );
    ompi_ddt_dfd = -1;
#endif  /* VERBOSE */

    /* release the local convertors (external32 and local) */
    ompi_ddt_default_convertors_fini();

    /* clear all master convertors */
    ompi_convertor_destroy_masters();

    return OMPI_SUCCESS;
}

#if OMPI_ENABLE_DEBUG
/*
 * Set a breakpoint to this function in your favorite debugger
 * to make it stopping on all pack and unpack errors.
 */
int ompi_ddt_safeguard_pointer_debug_breakpoint( const void* actual_ptr, int length,
                                                 const void* initial_ptr,
                                                 const ompi_datatype_t* pData,
                                                 int count )
{
    return 0;
}
#endif  /* OMPI_ENABLE_DEBUG */

/********************************************************
 * Data dumping functions
 ********************************************************/

static int _dump_data_flags( unsigned short usflags, char* ptr, size_t length )
{
    if( length < 21 ) return 0;
    sprintf( ptr, "-----------[---][---]" );  /* set everything to - */
    if( usflags & DT_FLAG_DESTROYED )                ptr[0]  = 'd';
    if( usflags & DT_FLAG_COMMITED )                 ptr[1]  = 'c';
    if( usflags & DT_FLAG_CONTIGUOUS )               ptr[2]  = 'C';
    if( usflags & DT_FLAG_OVERLAP )                  ptr[3]  = 'o';
    if( usflags & DT_FLAG_USER_LB )                  ptr[4]  = 'l';
    if( usflags & DT_FLAG_USER_UB )                  ptr[5]  = 'u';
    if( usflags & DT_FLAG_PREDEFINED )               ptr[6]  = 'P';
    if( !(usflags & DT_FLAG_NO_GAPS) )               ptr[7]  = 'G';
    if( usflags & DT_FLAG_DATA )                     ptr[8]  = 'D';
    if( (usflags & DT_FLAG_BASIC) == DT_FLAG_BASIC ) ptr[9]  = 'B';
    /* Which kind of datatype is that */
    switch( usflags & DT_FLAG_DATA_LANGUAGE ) {
    case DT_FLAG_DATA_C:
        ptr[12] = ' '; ptr[13] = 'C'; ptr[14] = ' '; break;
    case DT_FLAG_DATA_CPP:
        ptr[12] = 'C'; ptr[13] = 'P'; ptr[14] = 'P'; break;
    case DT_FLAG_DATA_FORTRAN:
        ptr[12] = 'F'; ptr[13] = '7'; ptr[14] = '7'; break;
    default:
        if( usflags & DT_FLAG_PREDEFINED ) {
            ptr[12] = 'E'; ptr[13] = 'R'; ptr[14] = 'R'; break;
        }
    }
    switch( usflags & DT_FLAG_DATA_TYPE ) {
    case DT_FLAG_DATA_INT:
        ptr[17] = 'I'; ptr[18] = 'N'; ptr[19] = 'T'; break;
    case DT_FLAG_DATA_FLOAT:
        ptr[17] = 'F'; ptr[18] = 'L'; ptr[19] = 'T'; break;
    case DT_FLAG_DATA_COMPLEX:
        ptr[17] = 'C'; ptr[18] = 'P'; ptr[19] = 'L'; break;
    default:
        if( usflags & DT_FLAG_PREDEFINED ) {
            ptr[17] = 'E'; ptr[18] = 'R'; ptr[19] = 'R'; break;
        }
    }
    return 21;
}

static int __dump_data_desc( dt_elem_desc_t* pDesc, int nbElems, char* ptr, size_t length )
{
    int i;
    size_t index = 0;

    for( i = 0; i < nbElems; i++ ) {
        index += _dump_data_flags( pDesc->elem.common.flags, ptr + index, length );
        if( length <= index ) break;
        index += snprintf( ptr + index, length - index, "%15s ", ompi_ddt_basicDatatypes[pDesc->elem.common.type]->name );
        if( length <= index ) break;
        if( DT_LOOP == pDesc->elem.common.type )
            index += snprintf( ptr + index, length - index, "%d times the next %d elements extent %d\n",
                               (int)pDesc->loop.loops, (int)pDesc->loop.items,
                               (int)pDesc->loop.extent );
	else if( DT_END_LOOP == pDesc->elem.common.type )
	    index += snprintf( ptr + index, length - index, "prev %d elements first elem displacement %ld size of data %d\n",
                           (int)pDesc->end_loop.items, (long)pDesc->end_loop.first_elem_disp,
                           (int)pDesc->end_loop.size );
        else
            index += snprintf( ptr + index, length - index, "count %d disp 0x%lx (%ld) extent %d (size %ld)\n",
                               (int)pDesc->elem.count, (long)pDesc->elem.disp, (long)pDesc->elem.disp,
                               (int)pDesc->elem.extent, (long)(pDesc->elem.count * ompi_ddt_basicDatatypes[pDesc->elem.common.type]->size) );
        pDesc++;

        if( length <= index ) break;
    }
    return index;
}

static inline int __dt_contain_basic_datatypes( const ompi_datatype_t* pData, char* ptr, size_t length )
{
    int i;
    size_t index = 0;
    uint64_t mask = 1;

    if( pData->flags & DT_FLAG_USER_LB ) index += snprintf( ptr, length - index, "lb " );
    if( pData->flags & DT_FLAG_USER_UB ) index += snprintf( ptr + index, length - index, "ub " );
    for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
        if( pData->bdt_used & mask )
            index += snprintf( ptr + index, length - index, "%s ", ompi_ddt_basicDatatypes[i]->name );
        mask <<= 1;
        if( length <= index ) break;
    }
    return index;
}

void ompi_ddt_dump( const ompi_datatype_t* pData )
{
    size_t length;
    int index = 0;
    char* buffer;

    length = pData->opt_desc.used + pData->desc.used;
    length = length * 100 + 500;
    buffer = (char*)malloc( length );
    index += snprintf( buffer, length - index, "Datatype %p[%s] size %ld align %d id %d length %d used %d\n"
                                               "true_lb %ld true_ub %ld (true_extent %ld) lb %ld ub %ld (extent %ld)\n"
                                               "nbElems %d loops %d flags %X (",
                     (void*)pData, pData->name, (long)pData->size, (int)pData->align, pData->id, (int)pData->desc.length, (int)pData->desc.used,
                     (long)pData->true_lb, (long)pData->true_ub, (long)(pData->true_ub - pData->true_lb),
                     (long)pData->lb, (long)pData->ub, (long)(pData->ub - pData->lb),
                     (int)pData->nbElems, (int)pData->btypes[DT_LOOP], (int)pData->flags );
    /* dump the flags */
    if( pData->flags == DT_FLAG_PREDEFINED )
        index += snprintf( buffer + index, length - index, "predefined " );
    else {
        if( pData->flags & DT_FLAG_DESTROYED ) index += snprintf( buffer + index, length - index, "destroyed " );
        if( pData->flags & DT_FLAG_COMMITED ) index += snprintf( buffer + index, length - index, "commited " );
        if( pData->flags & DT_FLAG_CONTIGUOUS) index += snprintf( buffer + index, length - index, "contiguous " );
    }
    index += snprintf( buffer + index, length - index, ")" );
    index += _dump_data_flags( pData->flags, buffer + index, length - index );
    {
        index += snprintf( buffer + index, length - index, "\n   contain " );
        index += __dt_contain_basic_datatypes( pData, buffer + index, length - index );
        index += snprintf( buffer + index, length - index, "\n" );
    }
    if( (pData->opt_desc.desc != pData->desc.desc) && (NULL != pData->opt_desc.desc) ) {
        /* If the data is already committed print everything including the last
         * fake DT_END_LOOP entry.
         */
        index += __dump_data_desc( pData->desc.desc, pData->desc.used + 1, buffer + index, length - index );
        index += snprintf( buffer + index, length - index, "Optimized description \n" );
        index += __dump_data_desc( pData->opt_desc.desc, pData->opt_desc.used + 1, buffer + index, length - index );
    } else {
        index += __dump_data_desc( pData->desc.desc, pData->desc.used, buffer + index, length - index );
        index += snprintf( buffer + index, length - index, "No optimized description\n" );
    }
    buffer[index] = '\0';  /* make sure we end the string with 0 */
    opal_output( 0, "%s\n", buffer );

    ompi_ddt_print_args( pData );

    free(buffer);
}
