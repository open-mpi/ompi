/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

/* other fields starting after bdt_used (index of DT_LOOP should be ONE) */
#define ZERO_DDT_ARRAY { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
                         0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define EMPTY_DATA(NAME) NULL, 0, "MPI_" # NAME, {0, 0, NULL}, {0, 0, NULL}, NULL, ZERO_DDT_ARRAY
#define BASEOBJ_DATA { OBJ_CLASS(ompi_datatype_t), 1 }
#define INIT_BASIC_DATA( TYPE, ALIGN, NAME )                             \
    { BASEOBJ_DATA, sizeof(TYPE), 0, sizeof(TYPE), ALIGN,               \
      0, sizeof(TYPE), DT_FLAG_BASIC | DT_FLAG_DATA, DT_##NAME, 1, \
      (((unsigned long long)1)<<(DT_##NAME)), EMPTY_DATA(NAME) }
/* Using this macro implies that at this point not all informations needed
 * to fill up the datatype are known. We fill them with zeros and then later
 * when the datatype engine will be initialized we complete with the
 * correct information.
 */
#define INIT_BASIC_TYPE( TYPE, NAME ) \
    { BASEOBJ_DATA, 0/*size*/, 0/*true_lb*/, 0/*true_ub*/, 0 /*align*/,    \
      0/*lb*/, 0/*ub*/, DT_FLAG_BASIC | DT_FLAG_DATA, TYPE, 1,  \
      (((unsigned long long)1)<<(TYPE)), EMPTY_DATA(NAME) }
/* The upeer bound and the true UB are set to the size of the datatype.
 * If it's not the case then they should be modified in the initialization
 * function.
 */
#if OMPI_WANT_F77_BINDINGS
#define INIT_BASIC_FORTRAN_TYPE( TYPE, NAME, SIZE, ALIGN ) \
    { BASEOBJ_DATA, SIZE, 0/*true_lb*/, SIZE/*true_ub*/, ALIGN, \
            0/*lb*/, SIZE/*ub*/, DT_FLAG_BASIC | DT_FLAG_DATA, (TYPE), 1, \
      (((unsigned long long)1)<<(TYPE)), EMPTY_DATA(NAME) }
#else
#define INIT_BASIC_FORTRAN_TYPE( TYPE, NAME, SIZE, ALIGN ) \
    INIT_BASIC_TYPE( TYPE, NAME )
#endif  /* OMPI_WANT_F77_BINDINGS */

ompi_datatype_t ompi_mpi_datatype_null = 
    { BASEOBJ_DATA, 0, 0, 0, 0,
      0, 0, DT_FLAG_BASIC, 0, 1,
      ((long long)0), EMPTY_DATA(DATATYPE_NULL) };

ompi_datatype_t ompi_mpi_loop = INIT_BASIC_TYPE( DT_LOOP, LOOP );
ompi_datatype_t ompi_mpi_end_loop = INIT_BASIC_TYPE( DT_END_LOOP, END_LOOP );
ompi_datatype_t ompi_mpi_ub = INIT_BASIC_TYPE( DT_LB, UB );
ompi_datatype_t ompi_mpi_lb = INIT_BASIC_TYPE( DT_LB, LB );
ompi_datatype_t ompi_mpi_char = INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, CHAR );
ompi_datatype_t ompi_mpi_character = INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, CHARACTER );
ompi_datatype_t ompi_mpi_unsigned_char = INIT_BASIC_DATA( unsigned char, OMPI_ALIGNMENT_CHAR, UNSIGNED_CHAR );
ompi_datatype_t ompi_mpi_byte = INIT_BASIC_DATA( unsigned char, OMPI_ALIGNMENT_CHAR, BYTE );
ompi_datatype_t ompi_mpi_short = INIT_BASIC_DATA( short, OMPI_ALIGNMENT_SHORT, SHORT );
ompi_datatype_t ompi_mpi_unsigned_short = INIT_BASIC_DATA( unsigned short, OMPI_ALIGNMENT_SHORT, UNSIGNED_SHORT );
ompi_datatype_t ompi_mpi_int = INIT_BASIC_DATA( int, OMPI_ALIGNMENT_INT, INT );
ompi_datatype_t ompi_mpi_unsigned = INIT_BASIC_DATA( unsigned int, OMPI_ALIGNMENT_INT, UNSIGNED_INT );
ompi_datatype_t ompi_mpi_long = INIT_BASIC_DATA( long, OMPI_ALIGNMENT_LONG, LONG );
ompi_datatype_t ompi_mpi_unsigned_long = INIT_BASIC_DATA( unsigned long, OMPI_ALIGNMENT_LONG, UNSIGNED_LONG );
#if HAVE_LONG_LONG
ompi_datatype_t ompi_mpi_long_long = INIT_BASIC_DATA( long long, OMPI_ALIGNMENT_LONG_LONG, LONG_LONG );
ompi_datatype_t ompi_mpi_long_long_int = INIT_BASIC_DATA( long long, OMPI_ALIGNMENT_LONG_LONG, LONG_LONG_INT );
ompi_datatype_t ompi_mpi_unsigned_long_long = INIT_BASIC_DATA( unsigned long long, OMPI_ALIGNMENT_LONG_LONG, UNSIGNED_LONG_LONG );
#else
ompi_datatype_t ompi_mpi_long_long = INIT_BASIC_DATA( void*, 0, UNAVAILABLE );
ompi_datatype_t ompi_mpi_long_long_int = INIT_BASIC_DATA( void*, 0, UNAVAILABLE );
ompi_datatype_t ompi_mpi_unsigned_long_long = INIT_BASIC_DATA( void*, 0, UNAVAILABLE );
#endif  /* HAVE_LONG_LONG */
ompi_datatype_t ompi_mpi_float = INIT_BASIC_DATA( float, OMPI_ALIGNMENT_FLOAT, FLOAT );
ompi_datatype_t ompi_mpi_double = INIT_BASIC_DATA( double, OMPI_ALIGNMENT_DOUBLE, DOUBLE );
ompi_datatype_t ompi_mpi_long_double = INIT_BASIC_DATA( long double, OMPI_ALIGNMENT_LONG_DOUBLE, LONG_DOUBLE );
ompi_datatype_t ompi_mpi_cplex = INIT_BASIC_DATA( ompi_complex_float_t, OMPI_ALIGNMENT_FLOAT, COMPLEX_FLOAT );
ompi_datatype_t ompi_mpi_dblcplex = INIT_BASIC_DATA( ompi_complex_double_t, OMPI_ALIGNMENT_DOUBLE, COMPLEX_DOUBLE );
#if HAVE_LONG_DOUBLE
ompi_datatype_t ompi_mpi_ldblcplex = INIT_BASIC_DATA( ompi_complex_long_double_t, OMPI_ALIGNMENT_LONG_DOUBLE, COMPLEX_LONG_DOUBLE );
#else
ompi_datatype_t ompi_mpi_ldblcplex = INIT_BASIC_DATA( void*, 0, UNAVAILABLE );
#endif  /* HAVE_LONG_DOUBLE */
ompi_datatype_t ompi_mpi_packed = INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, PACKED );
ompi_datatype_t ompi_mpi_logic = INIT_BASIC_FORTRAN_TYPE( DT_LOGIC, LOGIC, OMPI_SIZEOF_FORTRAN_LOGICAL, OMPI_ALIGNMENT_FORTRAN_LOGICAL );
ompi_datatype_t ompi_mpi_float_int = INIT_BASIC_TYPE( DT_FLOAT_INT, FLOAT_INT );
ompi_datatype_t ompi_mpi_double_int = INIT_BASIC_TYPE( DT_DOUBLE_INT, DOUBLE_INT );
#if HAVE_LONG_DOUBLE
ompi_datatype_t ompi_mpi_longdbl_int = INIT_BASIC_TYPE( DT_LONG_DOUBLE_INT, LONG_DOUBLE_INT );
#else
ompi_datatype_t ompi_mpi_longdbl_int = INIT_BASIC_DATA( void*, 0, UNAVAILABLE );
#endif  /* HAVE_LONG_DOUBLE */
ompi_datatype_t ompi_mpi_long_int = INIT_BASIC_TYPE( DT_LONG_INT, LONG_INT );
ompi_datatype_t ompi_mpi_2int = INIT_BASIC_TYPE( DT_2INT, 2INT );
ompi_datatype_t ompi_mpi_short_int = INIT_BASIC_TYPE( DT_SHORT_INT, SHORT_INT );
ompi_datatype_t ompi_mpi_integer = INIT_BASIC_FORTRAN_TYPE( DT_INTEGER, INTEGER, OMPI_SIZEOF_FORTRAN_INT, OMPI_ALIGNMENT_FORTRAN_INT );
ompi_datatype_t ompi_mpi_real = INIT_BASIC_FORTRAN_TYPE( DT_REAL, REAL, OMPI_SIZEOF_FORTRAN_REAL, OMPI_ALIGNMENT_FORTRAN_REAL );
ompi_datatype_t ompi_mpi_dblprec = INIT_BASIC_FORTRAN_TYPE( DT_DBLPREC, DBLPREC, OMPI_SIZEOF_FORTRAN_DBLPREC, OMPI_ALIGNMENT_FORTRAN_DBLPREC );
ompi_datatype_t ompi_mpi_2real = INIT_BASIC_TYPE( DT_2REAL, 2REAL );
ompi_datatype_t ompi_mpi_2dblprec = INIT_BASIC_TYPE( DT_2DBLPREC, 2DBLPREC );
ompi_datatype_t ompi_mpi_2integer = INIT_BASIC_TYPE( DT_2INTEGER, 2INTEGER );

ompi_datatype_t ompi_mpi_wchar = INIT_BASIC_TYPE( DT_WCHAR, WCHAR );
ompi_datatype_t ompi_mpi_cxx_cplex = INIT_BASIC_DATA( ompi_complex_float_t, OMPI_ALIGNMENT_FLOAT, COMPLEX_FLOAT );
ompi_datatype_t ompi_mpi_cxx_dblcplex = INIT_BASIC_DATA( ompi_complex_double_t, OMPI_ALIGNMENT_DOUBLE, COMPLEX_DOUBLE );
#if HAVE_LONG_DOUBLE
ompi_datatype_t ompi_mpi_cxx_ldblcplex = INIT_BASIC_DATA( ompi_complex_long_double_t, OMPI_ALIGNMENT_LONG_DOUBLE, COMPLEX_LONG_DOUBLE );
#else
ompi_datatype_t ompi_mpi_cxx_ldblcplex = INIT_BASIC_DATA( void*, 0, UNAVAILABLE );
#endif  /* HAVE_LONG_DOUBLE */

ompi_datatype_t ompi_mpi_cxx_bool = INIT_BASIC_DATA( SIZEOF_BOOL, OMPI_ALIGNMENT_CXX_BOOL, CXX_BOOL );
ompi_datatype_t ompi_mpi_2cplex = INIT_BASIC_TYPE( DT_2COMPLEX, 2COMPLEX );
ompi_datatype_t ompi_mpi_2dblcplex = INIT_BASIC_TYPE( DT_2DOUBLE_COMPLEX, 2DOUBLE_COMPLEX );
ompi_datatype_t ompi_mpi_unavailable = INIT_BASIC_TYPE( DT_UNAVAILABLE, UNAVAILABLE );

ompi_datatype_t ompi_mpi_real4 = INIT_BASIC_FORTRAN_TYPE( DT_FLOAT, REAL4, sizeof(float), 4 );
ompi_datatype_t ompi_mpi_real8 = INIT_BASIC_FORTRAN_TYPE( DT_DOUBLE, REAL8, sizeof(double), 8 );
#if HAVE_LONG_DOUBLE
ompi_datatype_t ompi_mpi_real16 = INIT_BASIC_FORTRAN_TYPE( DT_LONG_DOUBLE, REAL16, sizeof(long double), 16 );
#else
ompi_datatype_t ompi_mpi_real16 = INIT_BASIC_TYPE( DT_UNAVAILABLE, REAL16 );
#endif  /* HAVE_LONG_DOUBLE */
ompi_datatype_t ompi_mpi_integer1 = INIT_BASIC_FORTRAN_TYPE( DT_CHAR, INTEGER1, sizeof(char), 1 );
ompi_datatype_t ompi_mpi_integer2 = INIT_BASIC_FORTRAN_TYPE( DT_SHORT, INTEGER2, sizeof(short), 2 );
ompi_datatype_t ompi_mpi_integer4 = INIT_BASIC_FORTRAN_TYPE( DT_INT, INTEGER4, sizeof(int), sizeof(int) );
#if HAVE_LONG_LONG
ompi_datatype_t ompi_mpi_integer8 = INIT_BASIC_FORTRAN_TYPE( DT_LONG_LONG, INTEGER8, sizeof(long long), 8 );
#else
ompi_datatype_t ompi_mpi_integer8 = INIT_BASIC_TYPE( DT_UNAVAILABLE, INTEGER8 );
#endif  /* HAVE_LONG_LONG */
ompi_datatype_t ompi_mpi_integer16 = INIT_BASIC_TYPE( DT_UNAVAILABLE, INTEGER16 );
ompi_datatype_t ompi_mpi_complex8 = INIT_BASIC_FORTRAN_TYPE( DT_COMPLEX_FLOAT, COMPLEX8, OMPI_SIZEOF_FORTRAN_COMPLEX, OMPI_ALIGNMENT_FORTRAN_REAL );
ompi_datatype_t ompi_mpi_complex16 = INIT_BASIC_FORTRAN_TYPE( DT_COMPLEX_DOUBLE, COMPLEX16, OMPI_SIZEOF_FORTRAN_DBLCOMPLEX, OMPI_ALIGNMENT_FORTRAN_DBLPREC );
#if HAVE_LONG_DOUBLE
/* TODO: still have to add the discovery of long double complex in configure */
ompi_datatype_t ompi_mpi_complex32 = INIT_BASIC_FORTRAN_TYPE( DT_COMPLEX_LONG_DOUBLE, COMPLEX32, OMPI_SIZEOF_FORTRAN_DBLCOMPLEX * 2, OMPI_ALIGNMENT_FORTRAN_DBLPREC );
#else
ompi_datatype_t ompi_mpi_complex32 = INIT_BASIC_TYPE( DT_UNAVAILABLE, COMPLEX32 );
#endif  /* HAVE_LONG_DOUBLE */

/*
 * NOTE: The order of this array *MUST* match what is listed in
 * datatype_internal.h
 */
ompi_datatype_t* ompi_ddt_basicDatatypes[DT_MAX_PREDEFINED] = {
    &ompi_mpi_loop,
    &ompi_mpi_end_loop,
    &ompi_mpi_lb,
    &ompi_mpi_ub,
    &ompi_mpi_char,
    &ompi_mpi_character,
    &ompi_mpi_unsigned_char,
    &ompi_mpi_byte,
    &ompi_mpi_short,
    &ompi_mpi_unsigned_short,
    &ompi_mpi_int,
    &ompi_mpi_unsigned,
    &ompi_mpi_long,
    &ompi_mpi_unsigned_long,
    &ompi_mpi_long_long,
    &ompi_mpi_long_long_int,
    &ompi_mpi_unsigned_long_long,
    &ompi_mpi_float,
    &ompi_mpi_double,
    &ompi_mpi_long_double,
    &ompi_mpi_cplex,
    &ompi_mpi_dblcplex,
    &ompi_mpi_ldblcplex,
    &ompi_mpi_packed,
    &ompi_mpi_logic,
    &ompi_mpi_float_int,
    &ompi_mpi_double_int,
    &ompi_mpi_longdbl_int,
    &ompi_mpi_long_int,
    &ompi_mpi_2int,
    &ompi_mpi_short_int,
    &ompi_mpi_integer,
    &ompi_mpi_real,
    &ompi_mpi_dblprec,
    &ompi_mpi_2real,
    &ompi_mpi_2dblprec,
    &ompi_mpi_2integer,
    &ompi_mpi_wchar,
    &ompi_mpi_2cplex,
    &ompi_mpi_2dblcplex,
    &ompi_mpi_cxx_bool,
    &ompi_mpi_unavailable
};

ompi_pointer_array_t *ompi_datatype_f_to_c_table = NULL;

int ompi_ddt_local_sizes[DT_MAX_PREDEFINED];

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
        if( (PDST)->desc.desc != NULL )                                 \
            free( (PDST)->desc.desc );                                  \
        (PDST)->desc     = (PSRC)->desc;                                \
        if( (PDST)->opt_desc.desc != NULL )                             \
            free( (PDST)->opt_desc.desc );                              \
        (PDST)->opt_desc = (PSRC)->opt_desc;                            \
        memcpy( (PDST)->btypes, (PSRC)->btypes,                         \
                DT_MAX_PREDEFINED * sizeof(u_int32_t) );                \
    } while(0)

#define DECLARE_MPI2_COMPOSED_STRUCT_DDT( PDATA, MPIDDT, MPIDDTNAME, type1, type2, MPIType1, MPIType2 ) \
    do {                                                                \
        struct { type1 v1; type2 v2; } s[2];                            \
        ompi_datatype_t *types[2], *ptype;                               \
        int bLength[2] = {1, 1};                                        \
        long base, displ[2];                                            \
                                                                        \
        types[0] = ompi_ddt_basicDatatypes[MPIType1];                           \
        types[1] = ompi_ddt_basicDatatypes[MPIType2];                           \
        base = (long)(&(s[0]));                                         \
        displ[0] = (long)(&(s[0].v1));                                  \
        displ[0] -= base;                                               \
        displ[1] = (long)(&(s[0].v2));                                  \
        displ[1] -= base;                                               \
                                                                        \
        ompi_ddt_create_struct( 2, bLength, displ, types, &ptype );      \
        displ[0] = (long)(&(s[1]));                                     \
        displ[0] -= base;                                               \
        if( displ[0] != sizeof(s[0]) )                                  \
            ptype->ub = displ[0];  /* force a new extent for the datatype */ \
        ptype->flags |= DT_FLAG_FOREVER;                                \
        ptype->id = MPIDDT;                                             \
        ompi_ddt_commit( &ptype );                                       \
        COPY_DATA_DESC( PDATA, ptype );                                 \
        ptype->desc.desc = NULL;                                        \
        ptype->opt_desc.desc = NULL;                                    \
        OBJ_RELEASE( ptype );                                           \
        strncpy( (PDATA)->name, MPIDDTNAME, MPI_MAX_OBJECT_NAME );      \
    } while(0)

#define DECLARE_MPI2_COMPOSED_BLOCK_DDT( PDATA, MPIDDT, MPIDDTNAME, MPIType ) \
    do {                                                                \
        ompi_datatype_t *ptype;						\
        ompi_ddt_create_contiguous( 2, ompi_ddt_basicDatatypes[MPIType], &ptype ); \
        ptype->flags |= DT_FLAG_FOREVER;                                \
        ptype->id = (MPIDDT);						\
        ompi_ddt_commit( &ptype );					\
        COPY_DATA_DESC( (PDATA), ptype );				\
        ptype->desc.desc = NULL;                                        \
        ptype->opt_desc.desc = NULL;                                    \
        OBJ_RELEASE( ptype );                                           \
        strncpy( (PDATA)->name, (MPIDDTNAME), MPI_MAX_OBJECT_NAME );	\
    } while(0)

int ompi_ddt_init( void )
{
    int i;

    for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
        ompi_ddt_basicDatatypes[i]->desc.desc = (dt_elem_desc_t*)malloc(sizeof(dt_elem_desc_t));
        ompi_ddt_basicDatatypes[i]->desc.desc->flags  = DT_FLAG_BASIC | DT_FLAG_CONTIGUOUS;
        ompi_ddt_basicDatatypes[i]->desc.desc->type   = i;
        ompi_ddt_basicDatatypes[i]->desc.desc->count  = 1;
        ompi_ddt_basicDatatypes[i]->desc.desc->disp   = 0;
        ompi_ddt_basicDatatypes[i]->desc.desc->extent = ompi_ddt_basicDatatypes[i]->size;
        ompi_ddt_basicDatatypes[i]->desc.length       = 1;
        ompi_ddt_basicDatatypes[i]->desc.used         = 1;
        ompi_ddt_basicDatatypes[i]->btypes[i]         = 1;
    }

    /* Create the f2c translation table */
    ompi_datatype_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
    if (NULL == ompi_datatype_f_to_c_table) {
        return OMPI_ERROR;
    }
    
    /* the 2 complex datatypes (float and double) */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_cplex, DT_COMPLEX_FLOAT, "MPI_COMPLEX", float, float, DT_FLOAT, DT_FLOAT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_dblcplex, DT_COMPLEX_DOUBLE, "MPI_DOUBLE_COMPLEX", double, double, DT_DOUBLE, DT_DOUBLE );
    /* C++ complex types */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_cxx_cplex, DT_COMPLEX_FLOAT, "MPI_CXX_COMPLEX", float, float, DT_FLOAT, DT_FLOAT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_cxx_dblcplex, DT_COMPLEX_DOUBLE, "MPI_CXX_DOUBLE_COMPLEX", double, double, DT_DOUBLE, DT_DOUBLE );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_cxx_ldblcplex, DT_COMPLEX_LONG_DOUBLE, "MPI_CXX_LONG_DOUBLE_COMPLEX", long double, long double, DT_LONG_DOUBLE, DT_LONG_DOUBLE );

    /* Now the predefined MPI2 datatypes (they should last forever!) */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_float_int, DT_FLOAT_INT, "MPI_FLOAT_INT", float, int, DT_FLOAT, DT_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_double_int, DT_DOUBLE_INT, "MPI_DOUBLE_INT", double, int, DT_DOUBLE, DT_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_long_int, DT_LONG_INT, "MPI_LONG_INT", long, int, DT_LONG, DT_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_short_int, DT_SHORT_INT, "MPI_SHORT_INT", short, int, DT_SHORT, DT_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( &ompi_mpi_longdbl_int, DT_LONG_DOUBLE_INT, "MPI_LONG_DOUBLE_INT", long double, int, DT_LONG_DOUBLE, DT_INT );

    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2int, DT_2INT, "MPI_2INT", DT_INT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2integer, DT_2INTEGER, "MPI_2INTEGER", DT_INT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2real, DT_2REAL, "MPI_2REAL", DT_FLOAT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2dblprec, DT_2DBLPREC, "MPI_2DOUBLE_PRECISION", DT_DOUBLE );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2cplex, DT_2COMPLEX, "MPI_2COMPLEX", DT_COMPLEX_FLOAT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( &ompi_mpi_2dblcplex, DT_2DOUBLE_COMPLEX, "MPI_2DOUBLE_COMPLEX", DT_COMPLEX_DOUBLE );

    for( i = 0; i < DT_MAX_PREDEFINED; ++i ) {
        ompi_ddt_local_sizes[i] = ompi_ddt_basicDatatypes[i]->size;
    }

    /* Copy the desc pointer from the <DT_MAX_PREDEFINED datatypes to
       the synonym types */

    ompi_mpi_cxx_cplex.desc = ompi_mpi_cplex.desc;
    ompi_mpi_cxx_dblcplex.desc = ompi_mpi_dblcplex.desc;
    ompi_mpi_cxx_ldblcplex.desc = ompi_mpi_ldblcplex.desc;
    /* JMS this doesn't look right */
    ompi_mpi_cxx_bool.desc = ompi_mpi_int.desc;
    ompi_mpi_real4.desc = ompi_mpi_float.desc;
    ompi_mpi_real8.desc = ompi_mpi_double.desc;
    ompi_mpi_real16.desc = ompi_mpi_longdbl_int.desc;
    ompi_mpi_integer1.desc = ompi_mpi_char.desc;
    ompi_mpi_integer2.desc = ompi_mpi_short.desc;
    ompi_mpi_integer4.desc = ompi_mpi_int.desc;
    ompi_mpi_integer8.desc = ompi_mpi_long_long.desc;
    ompi_mpi_integer16.desc = ompi_mpi_unavailable.desc;
    /* JMS this doesn't looks right */
    ompi_mpi_complex8.desc = ompi_mpi_cplex.desc;
    /* JMS this doesn't looks right */
    ompi_mpi_complex16.desc = ompi_mpi_dblcplex.desc;
    /* JMS this doesn't looks right */
    ompi_mpi_complex32.desc = ompi_mpi_ldblcplex.desc;

    /* Start to populate the f2c index translation table */

    /* The order of the data registration should be the same as the
     * one in the mpif.h file. Any modification here should be
     * reflected there !!!  Do the Fortran types first so that mpif.h
     * can have consecutive, dense numbers. */ 

    /* This macro makes everything significantly easier to read below.
       All hail the moog!  :-) */

#define MOOG(name) ompi_mpi_##name.d_f_to_c_index = \
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, &ompi_mpi_##name);

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

    /* Now the C types */

    MOOG(wchar);
    MOOG(char);
    MOOG(unsigned_char);
    MOOG(short);
    MOOG(unsigned_short);
    MOOG(int);
    MOOG(unsigned);
    MOOG(long);
    MOOG(unsigned_long);
    MOOG(long_long);
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

    return OMPI_SUCCESS;
}

int ompi_ddt_finalize( void )
{
    int i;

    /* Remove all synonym desc copied pointers so that they don't get
       freed twice */

    ompi_mpi_cxx_cplex.desc.desc = NULL;
    ompi_mpi_cxx_dblcplex.desc.desc = NULL;
    ompi_mpi_cxx_ldblcplex.desc.desc = NULL;
    ompi_mpi_cxx_bool.desc.desc = NULL;
    ompi_mpi_real4.desc.desc = NULL;
    ompi_mpi_real8.desc.desc = NULL;
    ompi_mpi_real16.desc.desc = NULL;
    ompi_mpi_integer1.desc.desc = NULL;
    ompi_mpi_integer2.desc.desc = NULL;
    ompi_mpi_integer4.desc.desc = NULL;
    ompi_mpi_integer8.desc.desc = NULL;
    ompi_mpi_integer16.desc.desc = NULL;
    ompi_mpi_complex8.desc.desc = NULL;
    ompi_mpi_complex16.desc.desc = NULL;
    ompi_mpi_complex32.desc.desc = NULL;

   /* As they are statically allocated they cannot be released. But we 
    * can call OBJ_DESTRUCT, just to free all internally allocated ressources.
    */
   for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
       OBJ_DESTRUCT( ompi_ddt_basicDatatypes[i] );
   }

   /* Get rid of the Fortran2C translation table */
   OBJ_RELEASE(ompi_datatype_f_to_c_table);

   return OMPI_SUCCESS;
}

/********************************************************
 * Data dumping functions
 ********************************************************/

static void _dump_data_flags( unsigned short usflags )
{
    char flags[12] = "-----------";

    if( usflags & DT_FLAG_DESTROYED )                flags[0]  = 'd';
    if( usflags & DT_FLAG_COMMITED )                 flags[1]  = 'c';
    if( usflags & DT_FLAG_CONTIGUOUS )               flags[2]  = 'C';
    if( usflags & DT_FLAG_OVERLAP )                  flags[3]  = 'o';
    if( usflags & DT_FLAG_USER_LB )                  flags[4]  = 'l';
    if( usflags & DT_FLAG_USER_UB )                  flags[5]  = 'u';
    if( usflags & DT_FLAG_FOREVER )                  flags[6]  = 'F';
    if( usflags & DT_FLAG_IN_LOOP )                  flags[7]  = 'L';
    if( usflags & DT_FLAG_DATA )                     flags[8]  = 'D';
    if( usflags & DT_FLAG_INITIAL )                  flags[9]  = 'I';
    if( (usflags & DT_FLAG_BASIC) == DT_FLAG_BASIC ) flags[10] = 'B';
    flags[11] = 0;
    printf( "%s\t", flags );
}

static int __dump_data_desc( dt_elem_desc_t* pDesc, int nbElems )
{
    int i;

    for( i = 0; i < nbElems; i++ ) {
        _dump_data_flags( pDesc->flags );
        if( pDesc->type == DT_LOOP )
            printf( "%15s %d times the next %d elements extent %d\n",
                    ompi_ddt_basicDatatypes[pDesc->type]->name,
                    pDesc->count, (int)pDesc->disp, pDesc->extent );
	else if( pDesc->type == DT_END_LOOP )
	    printf( "%15s prev %d elements total true extent %d size of data %d\n",
                    ompi_ddt_basicDatatypes[pDesc->type]->name,
                    pDesc->count, (int)pDesc->disp, pDesc->extent );
        else
            printf( "%15s count %d disp 0x%lx (%ld) extent %d\n",
                    ompi_ddt_basicDatatypes[pDesc->type]->name,
                    pDesc->count, pDesc->disp, pDesc->disp, pDesc->extent );
        pDesc++;
    }
    return OMPI_SUCCESS;
}

static void __dt_contain_basic_datatypes( dt_desc_t* pData )
{
   int i;
   unsigned long long mask = 1;
   
   if( pData->flags & DT_FLAG_USER_LB ) printf( "lb " );
   if( pData->flags & DT_FLAG_USER_UB ) printf( "ub " );
   for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
      if( pData->bdt_used & mask )
         printf( "%s ", ompi_ddt_basicDatatypes[i]->name );
      mask <<= 1;
   }
}

void ompi_ddt_dump( dt_desc_t* data )
{
    dt_desc_t* pData = (dt_desc_t*)data;

    printf( "Datatype %p size %d align %d id %d length %d used %d\n\
   true_lb %ld true_ub %ld (true_extent %ld) lb %ld ub %ld (extent %ld)\n\
   nbElems %d loops %d flags %X (",
            (void*)pData, pData->size, pData->align, pData->id, pData->desc.length, pData->desc.used,
            pData->true_lb, pData->true_ub, pData->true_ub - pData->true_lb,
            pData->lb, pData->ub, pData->ub - pData->lb,
            pData->nbElems, pData->btypes[DT_LOOP], pData->flags );
    /* dump the flags */
    if( pData->flags == DT_FLAG_BASIC ) printf( "basic datatype " );
    else {
        if( pData->flags & DT_FLAG_DESTROYED ) printf( "destroyed " );
        if( pData->flags & DT_FLAG_COMMITED ) printf( "commited " );
        if( pData->flags & DT_FLAG_CONTIGUOUS) printf( "contiguous " );
    }
    printf( ")" ); _dump_data_flags( pData->flags );
    printf( "\n   contain " ); __dt_contain_basic_datatypes( pData ); printf( "\n" );
    __dump_data_desc( pData->desc.desc, pData->desc.used );
    if( pData->opt_desc.desc != NULL ) {
        printf( "Optimized description \n" );
        __dump_data_desc( pData->opt_desc.desc, pData->opt_desc.used );
    }
}
