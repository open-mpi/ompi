/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

/* other fields starting after bdt_used (index of DT_LOOP should be ONE) */
#define EMPTY_DATA(NAME) NULL, "MPI_" # NAME, {0, 0, NULL}, {0, 0, NULL}, NULL, { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define BASEOBJ_DATA { OBJ_CLASS(lam_object_t), 1 }
#define INIT_BASIC_DATA( TYPE, ALIGN, NAME )                             \
    { BASEOBJ_DATA, sizeof(TYPE), 0, sizeof(TYPE), ALIGN,               \
            0, sizeof(TYPE), DT_FLAG_BASIC | DT_FLAG_DATA, DT_##NAME, 1, \
            (((long long)1)<<(DT_##NAME)), EMPTY_DATA(NAME) }
#define INIT_BASIC_TYPE( TYPE, NAME ) \
    { BASEOBJ_DATA, 0, 0, 0, 0,          \
            0, 0, DT_FLAG_BASIC, DT_##NAME, 1,  \
            (((long long)1)<<(DT_##NAME)), EMPTY_DATA(NAME) }

dt_desc_t basicDatatypes[DT_MAX_PREDEFINED] = {
    INIT_BASIC_TYPE( DT_LOOP, LOOP ),
    INIT_BASIC_TYPE( DT_END_LOOP, END_LOOP ),
    INIT_BASIC_TYPE( DT_LB, LB ),
    INIT_BASIC_TYPE( DT_UB, UB ),
    INIT_BASIC_DATA( char, LAM_ALIGNMENT_CHAR, CHAR ),
    INIT_BASIC_DATA( char, LAM_ALIGNMENT_CHAR, CHARACTER ),
    INIT_BASIC_DATA( unsigned char, LAM_ALIGNMENT_CHAR, UNSIGNED_CHAR ),
    INIT_BASIC_DATA( unsigned char, LAM_ALIGNMENT_CHAR, BYTE ),
    INIT_BASIC_DATA( short, LAM_ALIGNMENT_SHORT, SHORT ),
    INIT_BASIC_DATA( unsigned short, LAM_ALIGNMENT_SHORT, UNSIGNED_SHORT ),
    INIT_BASIC_DATA( int, LAM_ALIGNMENT_INT, INT ),
    INIT_BASIC_DATA( unsigned int, LAM_ALIGNMENT_INT, UNSIGNED_INT ),
    INIT_BASIC_DATA( long, LAM_ALIGNMENT_LONG, LONG ),
    INIT_BASIC_DATA( unsigned long, LAM_ALIGNMENT_LONG, UNSIGNED_LONG ),
    INIT_BASIC_DATA( long long, LAM_ALIGNMENT_LONG_LONG, LONG_LONG ),
    INIT_BASIC_DATA( long long, LAM_ALIGNMENT_LONG_LONG, LONG_LONG_INT ),
    INIT_BASIC_DATA( unsigned long long, LAM_ALIGNMENT_LONG_LONG, UNSIGNED_LONG_LONG ),
    INIT_BASIC_DATA( float, LAM_ALIGNMENT_FLOAT, FLOAT ),
    INIT_BASIC_DATA( double, LAM_ALIGNMENT_DOUBLE, DOUBLE ),
    INIT_BASIC_DATA( long double, LAM_ALIGNMENT_LONG_DOUBLE, LONG_DOUBLE ),
    INIT_BASIC_DATA( complex_float_t, LAM_ALIGNMENT_FLOAT, COMPLEX_FLOAT ),
    INIT_BASIC_DATA( complex_double_t, LAM_ALIGNMENT_DOUBLE, COMPLEX_DOUBLE ),
    INIT_BASIC_DATA( char, LAM_ALIGNMENT_CHAR, PACKED ),
    INIT_BASIC_DATA( int, LAM_ALIGNMENT_INT, LOGIC ),
    INIT_BASIC_TYPE( DT_FLOAT_INT, FLOAT_INT ),
    INIT_BASIC_TYPE( DT_DOUBLE_INT, DOUBLE_INT ),
    INIT_BASIC_TYPE( DT_LONG_INT, LONG_INT ),
    INIT_BASIC_TYPE( DT_2INT, 2INT ),
    INIT_BASIC_TYPE( DT_SHORT_INT, SHORT_INT ),
    INIT_BASIC_TYPE( DT_INTEGER, INTEGER ),
    INIT_BASIC_TYPE( DT_REAL, REAL ),
    INIT_BASIC_TYPE( DT_DBLPREC, DBLPREC ),
    INIT_BASIC_TYPE( DT_2REAL, 2REAL ),
    INIT_BASIC_TYPE( DT_2DBLPREC, 2DBLPREC ),
    INIT_BASIC_TYPE( DT_2INTEGER, 2INTEGER ),
    INIT_BASIC_TYPE( DT_LONGDBL_INT, LONGDBL_INT ),
    INIT_BASIC_TYPE( DT_WCHAR, WCHAR )
};

lam_datatype_t* lam_mpi_char = basicDatatypes + DT_CHAR;
lam_datatype_t* lam_mpi_byte = basicDatatypes + DT_BYTE;
lam_datatype_t* lam_mpi_int = basicDatatypes + DT_INT;
lam_datatype_t* lam_mpi_logic = basicDatatypes + DT_LOGIC;
lam_datatype_t* lam_mpi_short = basicDatatypes + DT_SHORT;
lam_datatype_t* lam_mpi_long = basicDatatypes + DT_LONG;
lam_datatype_t* lam_mpi_float = basicDatatypes + DT_FLOAT;
lam_datatype_t* lam_mpi_double = basicDatatypes + DT_DOUBLE;
lam_datatype_t* lam_mpi_long_double = basicDatatypes + DT_LONG_DOUBLE;
lam_datatype_t* lam_mpi_cplex = basicDatatypes + DT_COMPLEX_FLOAT;
lam_datatype_t* lam_mpi_packed = basicDatatypes + DT_PACKED;
lam_datatype_t* lam_mpi_unsigned_char = basicDatatypes + DT_UNSIGNED_CHAR;
lam_datatype_t* lam_mpi_unsigned_short = basicDatatypes + DT_UNSIGNED_SHORT;
lam_datatype_t* lam_mpi_unsigned = basicDatatypes + DT_UNSIGNED_INT;
lam_datatype_t* lam_mpi_unsigned_long = basicDatatypes + DT_UNSIGNED_LONG;
lam_datatype_t* lam_mpi_ub = basicDatatypes + DT_UB;
lam_datatype_t* lam_mpi_lb = basicDatatypes + DT_LB;
lam_datatype_t* lam_mpi_float_int = basicDatatypes + DT_FLOAT_INT;
lam_datatype_t* lam_mpi_double_int = basicDatatypes + DT_DOUBLE_INT;
lam_datatype_t* lam_mpi_long_int = basicDatatypes + DT_LONG_INT;
lam_datatype_t* lam_mpi_2int = basicDatatypes + DT_2INT;
lam_datatype_t* lam_mpi_short_int = basicDatatypes + DT_SHORT_INT;
lam_datatype_t* lam_mpi_dblcplex = basicDatatypes + DT_COMPLEX_FLOAT;
lam_datatype_t* lam_mpi_integer = basicDatatypes + DT_INTEGER;
lam_datatype_t* lam_mpi_real = basicDatatypes + DT_REAL;
lam_datatype_t* lam_mpi_dblprec = basicDatatypes + DT_DBLPREC;
lam_datatype_t* lam_mpi_character = basicDatatypes + DT_CHARACTER;
lam_datatype_t* lam_mpi_2real = basicDatatypes + DT_2REAL;
lam_datatype_t* lam_mpi_2dblprec = basicDatatypes + DT_2DBLPREC;
lam_datatype_t* lam_mpi_2integer = basicDatatypes + DT_2INTEGER;
lam_datatype_t* lam_mpi_longdbl_int = basicDatatypes + DT_LONGDBL_INT;
lam_datatype_t* lam_mpi_wchar = basicDatatypes + DT_WCHAR;
#if HAVE_LONG_LONG
lam_datatype_t* lam_mpi_long_long_int = basicDatatypes + DT_LONG_LONG_INT;
lam_datatype_t* lam_mpi_long_long = basicDatatypes + DT_LONG_LONG;
lam_datatype_t* lam_mpi_unsigned_long_long = basicDatatypes + DT_UNSIGNED_LONG_LONG;
#else
lam_datatype_t* lam_mpi_long_long_int = NULL;
lam_datatype_t* lam_mpi_long_long = NULL;
lam_datatype_t* lam_mpi_unsigned_long_long = NULL;
#endif  /* HAVE_LONG_LONG */
lam_datatype_t* lam_mpi_cxx_cplex = basicDatatypes + DT_COMPLEX_FLOAT;
lam_datatype_t* lam_mpi_cxx_dblcplex = basicDatatypes + DT_COMPLEX_DOUBLE;
lam_datatype_t* lam_mpi_cxx_ldblcplex;
lam_datatype_t* lam_mpi_cxx_bool;

int local_sizes[DT_MAX_PREDEFINED];

#define DECLARE_MPI2_COMPOSED_STRUCT_DDT( MPIDDT, type1, type2, MPIType1, MPIType2 ) \
   do { \
      struct { type1 v1; type2 v2; } s[2]; \
      dt_id_t types[2]; \
      dt_desc_t* pData; \
      int bLength[2] = {1, 1}; \
      MPI_Aint base, displ[2]; \
\
      types[0] =  _data_ids[MPIType1]; \
      types[1] = _data_ids[MPIType2]; \
      base = (MPI_Aint)(&(s[0])); \
      displ[0] = (MPI_Aint)(&(s[0].v1)); \
      displ[0] -= base; \
      displ[1] = (MPI_Aint)(&(s[0].v2)); \
      displ[1] -= base; \
\
      dt_create_struct( 2, bLength, displ, types, &pData ); \
      displ[0] = (MPI_Aint)(&(s[1])); \
      displ[0] -= base; \
      if( displ[0] != sizeof(s[0]) ) \
         pData->ub = displ[0];  /* force a new extent for the datatype */ \
      pData->flags |= DT_FLAG_FOREVER; \
      pData->name = #MPIDDT; \
          dt_commit( &pData ); \
      _data_ids[MPIDDT] = pData; \
      pData->id = MPIDDT; \
   } while(0);

#define DECLARE_MPI2_COMPOSED_BLOCK_DDT( MPIDDT, MPIType ) \
   do { \
      dt_desc_t* pData; \
      dt_create_contiguous( 2, _data_ids[MPIType], &pData ); \
      pData->flags |= DT_FLAG_FOREVER; \
      pData->name = #MPIDDT; \
      dt_commit( &pData ); \
      _data_ids[MPIDDT] = pData; \
      pData->id = MPIDDT; \
   } while(0)
                                                                                                 
int lam_ddt_init( void )
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

#ifdef TOTO
   /* the 2 complex datatypes (float and double) */
   DECLARE_MPI2_COMPOSED_STRUCT_DDT( MPI_COMPLEX, float, float, MPI_FLOAT, MPI_FLOAT );
   DECLARE_MPI2_COMPOSED_STRUCT_DDT( MPI_DOUBLE_COMPLEX, double, double, MPI_DOUBLE, MPI_DOUBLE );

   /* Now the predefined MPI2 datatypes (they should last forever!) */
   DECLARE_MPI2_COMPOSED_STRUCT_DDT( MPI_FLOAT_INT, float, int, MPI_FLOAT, MPI_INT );
   DECLARE_MPI2_COMPOSED_STRUCT_DDT( MPI_DOUBLE_INT, double, int, MPI_DOUBLE, MPI_INT );
   DECLARE_MPI2_COMPOSED_STRUCT_DDT( MPI_LONG_INT, long, int, MPI_LONG, MPI_INT );
   DECLARE_MPI2_COMPOSED_STRUCT_DDT( MPI_SHORT_INT, short, int, MPI_SHORT, MPI_INT );
   DECLARE_MPI2_COMPOSED_STRUCT_DDT( MPI_LONG_DOUBLE_INT, long double, int, MPI_LONG_DOUBLE, MPI_INT );

   DECLARE_MPI2_COMPOSED_BLOCK_DDT( MPI_2INT, MPI_INT );
   DECLARE_MPI2_COMPOSED_BLOCK_DDT( MPI_2REAL, MPI_FLOAT );
   DECLARE_MPI2_COMPOSED_BLOCK_DDT( MPI_2DOUBLE_PRECISION, MPI_DOUBLE );
   DECLARE_MPI2_COMPOSED_BLOCK_DDT( MPI_2INTEGER, MPI_INT );
   DECLARE_MPI2_COMPOSED_BLOCK_DDT( MPI_2COMPLEX, MPI_COMPLEX );
   DECLARE_MPI2_COMPOSED_BLOCK_DDT( MPI_2DOUBLE_COMPLEX, MPI_DOUBLE_COMPLEX );
#endif  /* TOTO */
   for( i = 0; i < DT_MAX_PREDEFINED; i++ )
       local_sizes[i] = basicDatatypes[i].size;

   return 0;
}

int lam_ddt_finalize( void )
{
   int i;

   for( i =0; i < DT_MAX_PREDEFINED; i++ ) {
      free( basicDatatypes[i].desc.desc );
      basicDatatypes[i].desc.desc   = NULL;
      basicDatatypes[i].desc.length = 0;
      basicDatatypes[i].desc.used   = 0;
   }

#ifdef TOTO
   OBJ_RELEASE( _data_ids[MPI_COMPLEX] );
   OBJ_RELEASE( _data_ids[MPI_DOUBLE_COMPLEX] );
   OBJ_RELEASE( _data_ids[MPI_FLOAT_INT] );
   OBJ_RELEASE( _data_ids[MPI_DOUBLE_INT] );
   OBJ_RELEASE( _data_ids[MPI_LONG_INT] );
   OBJ_RELEASE( _data_ids[MPI_SHORT_INT] );
   OBJ_RELEASE( _data_ids[MPI_LONG_DOUBLE_INT] );
   OBJ_RELEASE( _data_ids[MPI_2INT] );
   OBJ_RELEASE( _data_ids[MPI_2REAL] );
   OBJ_RELEASE( _data_ids[MPI_2DOUBLE_PRECISION] );
   OBJ_RELEASE( _data_ids[MPI_2INTEGER] );
   OBJ_RELEASE( _data_ids[MPI_2COMPLEX] );
   OBJ_RELEASE( _data_ids[MPI_2DOUBLE_COMPLEX] );
#endif  /* TOTO */

   return 0;
}
