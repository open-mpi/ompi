/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

/* other fields starting after bdt_used (index of DT_LOOP should be ONE) */
#define EMPTY_DATA(NAME) NULL, 0, "MPI_" # NAME, {0, 0, NULL}, {0, 0, NULL}, NULL, { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define BASEOBJ_DATA { OBJ_CLASS(ompi_datatype_t), 1 }
#define INIT_BASIC_DATA( TYPE, ALIGN, NAME )                             \
    { BASEOBJ_DATA, sizeof(TYPE), 0, sizeof(TYPE), ALIGN,               \
            0, sizeof(TYPE), DT_FLAG_BASIC | DT_FLAG_DATA, DT_##NAME, 1, \
            (((long long)1)<<(DT_##NAME)), EMPTY_DATA(NAME) }
#define INIT_BASIC_TYPE( TYPE, NAME ) \
    { BASEOBJ_DATA, 0, 0, 0, 0,          \
            0, 0, DT_FLAG_BASIC, DT_##NAME, 1,  \
            (((long long)1)<<(DT_##NAME)), EMPTY_DATA(NAME) }

ompi_datatype_t basicDatatypes[DT_MAX_PREDEFINED] = {
    INIT_BASIC_TYPE( DT_LOOP, LOOP ),
    INIT_BASIC_TYPE( DT_END_LOOP, END_LOOP ),
    INIT_BASIC_TYPE( DT_LB, LB ),
    INIT_BASIC_TYPE( DT_UB, UB ),
    INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, CHAR ),
    INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, CHARACTER ),
    INIT_BASIC_DATA( unsigned char, OMPI_ALIGNMENT_CHAR, UNSIGNED_CHAR ),
    INIT_BASIC_DATA( unsigned char, OMPI_ALIGNMENT_CHAR, BYTE ),
    INIT_BASIC_DATA( short, OMPI_ALIGNMENT_SHORT, SHORT ),
    INIT_BASIC_DATA( unsigned short, OMPI_ALIGNMENT_SHORT, UNSIGNED_SHORT ),
    INIT_BASIC_DATA( int, OMPI_ALIGNMENT_INT, INT ),
    INIT_BASIC_DATA( unsigned int, OMPI_ALIGNMENT_INT, UNSIGNED_INT ),
    INIT_BASIC_DATA( long, OMPI_ALIGNMENT_LONG, LONG ),
    INIT_BASIC_DATA( unsigned long, OMPI_ALIGNMENT_LONG, UNSIGNED_LONG ),
#if HAVE_LONG_LONG
    INIT_BASIC_DATA( long long, OMPI_ALIGNMENT_LONG_LONG, LONG_LONG ),
    INIT_BASIC_DATA( long long, OMPI_ALIGNMENT_LONG_LONG, LONG_LONG_INT ),
    INIT_BASIC_DATA( unsigned long long, OMPI_ALIGNMENT_LONG_LONG, UNSIGNED_LONG_LONG ),
#else
    INIT_BASIC_DATA( void*, 0, UNAVAILABLE ),
    INIT_BASIC_DATA( void*, 0, UNAVAILABLE ),
    INIT_BASIC_DATA( void*, 0, UNAVAILABLE ),
#endif  /* HAVE_LONG_LONG */
    INIT_BASIC_DATA( float, OMPI_ALIGNMENT_FLOAT, FLOAT ),
    INIT_BASIC_DATA( double, OMPI_ALIGNMENT_DOUBLE, DOUBLE ),
    INIT_BASIC_DATA( long double, OMPI_ALIGNMENT_LONG_DOUBLE, LONG_DOUBLE ),
    INIT_BASIC_DATA( ompi_complex_float_t, OMPI_ALIGNMENT_FLOAT, COMPLEX_FLOAT ),
    INIT_BASIC_DATA( ompi_complex_double_t, OMPI_ALIGNMENT_DOUBLE, COMPLEX_DOUBLE ),
#if HAVE_LONG_DOUBLE
    INIT_BASIC_DATA( ompi_complex_long_double_t, OMPI_ALIGNMENT_LONG_DOUBLE, COMPLEX_LONG_DOUBLE ),
#else
    INIT_BASIC_DATA( void*, 0, UNAVAILABLE ),
#endif  /* HAVE_LONG_DOUBLE */
    INIT_BASIC_DATA( char, OMPI_ALIGNMENT_CHAR, PACKED ),
    INIT_BASIC_DATA( int, OMPI_ALIGNMENT_INT, LOGIC ),
    INIT_BASIC_TYPE( DT_FLOAT_INT, FLOAT_INT ),
    INIT_BASIC_TYPE( DT_DOUBLE_INT, DOUBLE_INT ),
#if HAVE_LONG_DOUBLE
    INIT_BASIC_TYPE( DT_LONG_DOUBLE_INT, LONG_DOUBLE_INT ),
#else
    INIT_BASIC_DATA( void*, 0, UNAVAILABLE ),
#endif  /* HAVE_LONG_DOUBLE */
    INIT_BASIC_TYPE( DT_LONG_INT, LONG_INT ),
    INIT_BASIC_TYPE( DT_2INT, 2INT ),
    INIT_BASIC_TYPE( DT_SHORT_INT, SHORT_INT ),
    INIT_BASIC_TYPE( DT_INTEGER, INTEGER ),
    INIT_BASIC_TYPE( DT_REAL, REAL ),
    INIT_BASIC_TYPE( DT_DBLPREC, DBLPREC ),
    INIT_BASIC_TYPE( DT_2REAL, 2REAL ),
    INIT_BASIC_TYPE( DT_2DBLPREC, 2DBLPREC ),
    INIT_BASIC_TYPE( DT_2INTEGER, 2INTEGER ),
#if HAVE_LONG_DOUBLE
    INIT_BASIC_TYPE( DT_LONGDBL_INT, LONGDBL_INT ),
#else
    INIT_BASIC_DATA( void*, 0, UNAVAILABLE ),
#endif  /* HAVE_LONG_DOUBLE */
    INIT_BASIC_TYPE( DT_WCHAR, WCHAR ),
    INIT_BASIC_TYPE( DT_2COMPLEX, 2COMPLEX ),
    INIT_BASIC_TYPE( DT_2DOUBLE_COMPLEX, 2DOUBLE_COMPLEX ),
    INIT_BASIC_DATA( int, OMPI_ALIGNMENT_INT, CXX_BOOL ),
    INIT_BASIC_TYPE( DT_UNAVAILABLE, UNAVAILABLE )
};

static ompi_datatype_t hide_datatype_null = 
         { BASEOBJ_DATA, 0, 0, 0, 0,
           0, 0, DT_FLAG_BASIC, 0, 1,
           ((long long)0), EMPTY_DATA(DATATYPE_NULL) };

ompi_datatype_t* ompi_mpi_datatype_null = &hide_datatype_null;
ompi_datatype_t* ompi_mpi_char = basicDatatypes + DT_CHAR;
ompi_datatype_t* ompi_mpi_byte = basicDatatypes + DT_BYTE;
ompi_datatype_t* ompi_mpi_int = basicDatatypes + DT_INT;
ompi_datatype_t* ompi_mpi_logic = basicDatatypes + DT_LOGIC;
ompi_datatype_t* ompi_mpi_short = basicDatatypes + DT_SHORT;
ompi_datatype_t* ompi_mpi_long = basicDatatypes + DT_LONG;
ompi_datatype_t* ompi_mpi_float = basicDatatypes + DT_FLOAT;
ompi_datatype_t* ompi_mpi_double = basicDatatypes + DT_DOUBLE;
ompi_datatype_t* ompi_mpi_long_double = basicDatatypes + DT_LONG_DOUBLE;
ompi_datatype_t* ompi_mpi_cplex = basicDatatypes + DT_COMPLEX_FLOAT;
ompi_datatype_t* ompi_mpi_packed = basicDatatypes + DT_PACKED;
ompi_datatype_t* ompi_mpi_unsigned_char = basicDatatypes + DT_UNSIGNED_CHAR;
ompi_datatype_t* ompi_mpi_unsigned_short = basicDatatypes + DT_UNSIGNED_SHORT;
ompi_datatype_t* ompi_mpi_unsigned = basicDatatypes + DT_UNSIGNED_INT;
ompi_datatype_t* ompi_mpi_unsigned_long = basicDatatypes + DT_UNSIGNED_LONG;
ompi_datatype_t* ompi_mpi_ub = basicDatatypes + DT_UB;
ompi_datatype_t* ompi_mpi_lb = basicDatatypes + DT_LB;
ompi_datatype_t* ompi_mpi_float_int = basicDatatypes + DT_FLOAT_INT;
ompi_datatype_t* ompi_mpi_double_int = basicDatatypes + DT_DOUBLE_INT;
ompi_datatype_t* ompi_mpi_long_int = basicDatatypes + DT_LONG_INT;
ompi_datatype_t* ompi_mpi_2int = basicDatatypes + DT_2INT;
ompi_datatype_t* ompi_mpi_short_int = basicDatatypes + DT_SHORT_INT;
ompi_datatype_t* ompi_mpi_dblcplex = basicDatatypes + DT_COMPLEX_FLOAT;
ompi_datatype_t* ompi_mpi_integer = basicDatatypes + DT_INTEGER;
ompi_datatype_t* ompi_mpi_real = basicDatatypes + DT_REAL;
ompi_datatype_t* ompi_mpi_dblprec = basicDatatypes + DT_DBLPREC;
ompi_datatype_t* ompi_mpi_character = basicDatatypes + DT_CHARACTER;
ompi_datatype_t* ompi_mpi_2real = basicDatatypes + DT_2REAL;
ompi_datatype_t* ompi_mpi_2dblprec = basicDatatypes + DT_2DBLPREC;
ompi_datatype_t* ompi_mpi_2integer = basicDatatypes + DT_2INTEGER;
ompi_datatype_t* ompi_mpi_longdbl_int = basicDatatypes + DT_LONGDBL_INT;
ompi_datatype_t* ompi_mpi_wchar = basicDatatypes + DT_WCHAR;
ompi_datatype_t* ompi_mpi_long_long_int = basicDatatypes + DT_LONG_LONG_INT;
ompi_datatype_t* ompi_mpi_long_long = basicDatatypes + DT_LONG_LONG;
ompi_datatype_t* ompi_mpi_unsigned_long_long = basicDatatypes + DT_UNSIGNED_LONG_LONG;
ompi_datatype_t* ompi_mpi_cxx_cplex = basicDatatypes + DT_COMPLEX_FLOAT;
ompi_datatype_t* ompi_mpi_cxx_dblcplex = basicDatatypes + DT_COMPLEX_DOUBLE;
ompi_datatype_t* ompi_mpi_cxx_ldblcplex = basicDatatypes + DT_COMPLEX_LONG_DOUBLE;
ompi_datatype_t* ompi_mpi_cxx_bool = basicDatatypes + DT_CXX_BOOL;
ompi_datatype_t* ompi_mpi_2cplex = basicDatatypes + DT_2COMPLEX;
ompi_datatype_t* ompi_mpi_2dblcplex = basicDatatypes + DT_2DOUBLE_COMPLEX;

ompi_pointer_array_t *ompi_datatype_f_to_c_table = NULL;

int local_sizes[DT_MAX_PREDEFINED];

static ompi_convertor_t* pDumpConv = NULL;

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
        types[0] = basicDatatypes + MPIType1;                           \
        types[1] = basicDatatypes + MPIType2;                           \
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
        ompi_datatype_t *ptype;                                          \
        ompi_ddt_create_contiguous( 2, &(basicDatatypes[MPIType]), &ptype ); \
        ptype->flags |= DT_FLAG_FOREVER;                                \
        ptype->id = MPIDDT;                                             \
        ompi_ddt_commit( &ptype );                                       \
        COPY_DATA_DESC( PDATA, ptype );                                 \
        ptype->desc.desc = NULL;                                        \
        ptype->opt_desc.desc = NULL;                                    \
        OBJ_RELEASE( ptype );                                           \
        strncpy( PDATA->name, MPIDDTNAME, MPI_MAX_OBJECT_NAME );        \
    } while(0)

int ompi_ddt_init( void )
{
    int i;

    for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
        basicDatatypes[i].desc.desc = (dt_elem_desc_t*)malloc(sizeof(dt_elem_desc_t));
        basicDatatypes[i].desc.desc->flags  = DT_FLAG_BASIC | DT_FLAG_CONTIGUOUS;
        basicDatatypes[i].desc.desc->type   = i;
        basicDatatypes[i].desc.desc->count  = 1;
        basicDatatypes[i].desc.desc->disp   = 0;
        basicDatatypes[i].desc.desc->extent = basicDatatypes[i].size;
        basicDatatypes[i].desc.length       = 1;
        basicDatatypes[i].desc.used         = 1;
        basicDatatypes[i].btypes[i]         = 1;
    }

    /* Create the f2c translation table */
    ompi_datatype_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
    if (NULL == ompi_datatype_f_to_c_table) {
        return OMPI_ERROR;
    }
    
    /* the 2 complex datatypes (float and double) */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_cplex, DT_COMPLEX_FLOAT, "MPI_COMPLEX", float, float, DT_FLOAT, DT_FLOAT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_dblcplex, DT_COMPLEX_DOUBLE, "MPI_DOUBLE_COMPLEX", double, double, DT_DOUBLE, DT_DOUBLE );
    /* C++ complex types */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_cxx_cplex, DT_COMPLEX_FLOAT, "MPI_CXX_COMPLEX", float, float, DT_FLOAT, DT_FLOAT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_cxx_dblcplex, DT_COMPLEX_DOUBLE, "MPI_CXX_DOUBLE_COMPLEX", double, double, DT_DOUBLE, DT_DOUBLE );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_cxx_ldblcplex, DT_COMPLEX_LONG_DOUBLE, "MPI_CXX_LONG_DOUBLE_COMPLEX", long double, long double, DT_LONG_DOUBLE, DT_LONG_DOUBLE );

    /* Now the predefined MPI2 datatypes (they should last forever!) */
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_float_int, DT_FLOAT_INT, "MPI_FLOAT_INT", float, int, DT_FLOAT, DT_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_double_int, DT_DOUBLE_INT, "MPI_DOUBLE_INT", double, int, DT_DOUBLE, DT_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_long_int, DT_LONG_INT, "MPI_LONG_INT", long, int, DT_LONG, DT_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_short_int, DT_SHORT_INT, "MPI_SHORT_INT", short, int, DT_SHORT, DT_INT );
    DECLARE_MPI2_COMPOSED_STRUCT_DDT( ompi_mpi_longdbl_int, DT_LONG_DOUBLE_INT, "MPI_LONG_DOUBLE_INT", long double, int, DT_LONG_DOUBLE, DT_INT );

    DECLARE_MPI2_COMPOSED_BLOCK_DDT( ompi_mpi_2int, DT_2INT, "MPI_2INT", DT_INT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( ompi_mpi_2integer, DT_2INTEGER, "MPI_2INTEGER", DT_INT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( ompi_mpi_2real, DT_2REAL, "MPI_2REAL", DT_FLOAT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( ompi_mpi_2dblprec, DT_2DBLPREC, "MPI_2DOUBLE_PRECISION", DT_DOUBLE );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( ompi_mpi_2cplex, DT_2COMPLEX, "MPI_2COMPLEX", DT_COMPLEX_FLOAT );
    DECLARE_MPI2_COMPOSED_BLOCK_DDT( ompi_mpi_2dblcplex, DT_2DOUBLE_COMPLEX, "MPI_2DOUBLE_COMPLEX", DT_COMPLEX_DOUBLE );

    for( i = 0; i < DT_MAX_PREDEFINED; i++ )
        local_sizes[i] = basicDatatypes[i].size;

    /* Start to populate the f2c index translation table */
    ompi_pointer_array_add( ompi_datatype_f_to_c_table, NULL );  /* why not ? */
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_byte );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_packed );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_ub );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_lb );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_character );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_logic );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_integer );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_char );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_short );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_int );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_long_long );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_real );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_real );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_real );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_double );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_long_double );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_dblprec );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_cplex );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_dblcplex );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_2real );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_2dblcplex );
    ompi_pointer_array_add(ompi_datatype_f_to_c_table, ompi_mpi_2integer );

    return OMPI_SUCCESS;
}

int ompi_ddt_finalize( void )
{
   int i;

   /* As they are statically allocated they cannot be released. But we 
    * can call OBJ_DESTRUCT, just to free all internally allocated ressources.
    */
   for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
       OBJ_DESTRUCT( &(basicDatatypes[i]) );
   }

   if( pDumpConv != NULL ) {
       OBJ_RELEASE( pDumpConv );
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
                    basicDatatypes[pDesc->type].name,
                    pDesc->count, (int)pDesc->disp, pDesc->extent );
        else
            printf( "%15s count %d disp 0x%lx (%ld) extent %d\n",
                    basicDatatypes[pDesc->type].name,
                    pDesc->count, pDesc->disp, pDesc->disp, pDesc->extent );
        pDesc++;
    }
    return OMPI_SUCCESS;
}

static void __dt_contain_basic_datatypes( dt_desc_t* pData )
{
    int i, mask = 1;

    if( pData->flags & DT_FLAG_USER_LB ) printf( "lb " );
    if( pData->flags & DT_FLAG_USER_UB ) printf( "ub " );
    for( i = 0; i < DT_MAX_PREDEFINED; i++ ) {
        if( pData->bdt_used & mask )
            printf( "%s ", basicDatatypes[i].name );
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

#define DUMP_TYPE( TYPENAME, TYPE )                                     \
    static int dump_##TYPENAME( unsigned int count,                     \
                                char* from, unsigned int from_len, long from_extent, \
                                char* to, unsigned int to_len, long to_extent, \
                                int* used )                             \
    {                                                                   \
        int remote_type_size = sizeof(TYPE), res = 1;                   \
        if( (remote_type_size * count) > from_len ) {                   \
            count = from_len / remote_type_size;                        \
            if( (count * remote_type_size) != from_len ) {              \
                printf( "oops should I keep this data somewhere (excedent %d bytes)?\n", \
                        from_len - (count * remote_type_size) );        \
                res = -1;                                               \
            }                                                           \
            printf( "correct: %s count %d from %p with length %d to %p space %d\n", \
                    #TYPE, count, from, from_len, to, to_len );         \
        } else                                                          \
            printf( "         %s count %d from %p with length %d to %p space %d\n", \
                    #TYPE, count, from, from_len, to, to_len );         \
                                                                        \
        *used = count * to_extent;                                      \
        return res * count;                                             \
    }

DUMP_TYPE( char, char )
DUMP_TYPE( short, short )
DUMP_TYPE( int, int )
DUMP_TYPE( float, float )
DUMP_TYPE( long, long )
DUMP_TYPE( double, double )
DUMP_TYPE( long_long, long long )
DUMP_TYPE( long_double, long double )
DUMP_TYPE( complex_float, ompi_complex_float_t )
DUMP_TYPE( complex_double, ompi_complex_double_t )

static conversion_fct_t dump_functions[DT_MAX_PREDEFINED] = {
    (conversion_fct_t)NULL,                 /* DT_LOOP           */
    (conversion_fct_t)NULL,                 /* DT_LB             */
    (conversion_fct_t)NULL,                 /* DT_UB             */
    (conversion_fct_t)NULL,                 /* DT_SPACE          */
    (conversion_fct_t)dump_char,            /* DT_CHAR           */
    (conversion_fct_t)dump_char,            /* DT_BYTE           */
    (conversion_fct_t)dump_short,           /* DT_SHORT          */
    (conversion_fct_t)dump_int,             /* DT_INT            */
    (conversion_fct_t)dump_float,           /* DT_FLOAT          */
    (conversion_fct_t)dump_long,            /* DT_LONG           */
    (conversion_fct_t)dump_double,          /* DT_DOUBLE         */
    (conversion_fct_t)dump_long_long,       /* DT_LONG_LONG      */
    (conversion_fct_t)dump_long_double,     /* DT_LONG_DOUBLE    */
    (conversion_fct_t)dump_complex_float,   /* DT_COMPLEX_FLOAT  */
    (conversion_fct_t)dump_complex_double,  /* DT_COMPLEX_DOUBLE */
};

void ompi_ddt_dump_complete( dt_desc_t* data )
{
    dt_desc_t* pData = (dt_desc_t*)data;
    struct iovec fake = { (void*)0, 0 };

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
        if( pData->flags & DT_FLAG_OVERLAP ) printf( "overlap " );
    }
    printf( ")\n   contain " ); __dt_contain_basic_datatypes( pData );
    printf( "\n{\n" );
    if( pDumpConv == NULL ) {
        pDumpConv = ompi_convertor_create( 0, 0 );
    }
    ompi_convertor_init_for_recv( pDumpConv, 0, pData, 1, NULL, 0 );
    pDumpConv->pFunctions = dump_functions;

    fake.iov_len = pData->size;
    ompi_convertor_unpack( pDumpConv, &fake, 1 );
/* As this convertor wii be here forever (at least until the end of the
 * application, we should release the ddt pointer and free the stack.
 */
    OBJ_RELEASE( pData );
    pDumpConv->pDesc = NULL;
    free( pDumpConv->pStack );
    pDumpConv->pStack = NULL;
    printf( "}\n" );
}
