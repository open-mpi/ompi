/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdbool.h>
#include "datatype.h"
#include "datatype/datatype_internal.h"
#include <time.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>

#define TIMER_DATA_TYPE struct timeval
#define GET_TIME(TV)   gettimeofday( &(TV), NULL )
#define ELAPSED_TIME(TSTART, TEND)  (((TEND).tv_sec - (TSTART).tv_sec) * 1000000 + ((TEND).tv_usec - (TSTART).tv_usec))

int mpich_typeub( void )
{
   int errs = 0;
   long extent, lb, extent1, extent2, extent3;
   long displ[2];
   int blens[2];
   ompi_datatype_t *type1, *type2, *type3, *types[2];

   ompi_ddt_create_vector( 2, 1, 4, ompi_ddt_basicDatatypes[DT_INT], &type1 );
   ompi_ddt_commit( &type1 );
   ompi_ddt_get_extent( type1, &lb, &extent );
   extent1 = 5 * sizeof(int);
   if (extent != extent1) {
      printf("EXTENT 1 %ld != %ld\n",extent,extent1);
      errs++;
      printf("extent(type1)=%ld\n",(long)extent);
   }

   blens[0] = 1;
   blens[1] = 1;
   displ[0] = 0;
   displ[1] = sizeof(int)*4;
   types[0] = type1;
   types[1] = &ompi_mpi_ub;  /*ompi_ddt_basicDatatypes[DT_UB];*/
   extent2  = displ[1];

   /*    using MPI_UB and Type_struct, monkey with the extent, making it 16
    */
   ompi_ddt_create_struct( 2, blens, displ, types, &type2 );
   ompi_ddt_commit( &type2 );
   ompi_ddt_get_extent( type2, &lb, &extent );
   if (extent != extent2) {
      printf("EXTENT 2 %ld != %ld\n",extent,extent2);
      errs++;
      printf("extent(type2)=%ld\n",(long)extent);
   }

   /*    monkey with the extent again, making it 4
    *     ===> MPICH gives 4
    *     ===> MPIF gives 16, the old extent
    */
   displ[1] = sizeof(int);
   types[0] = type2;
   types[1] = &ompi_mpi_ub;  /*ompi_ddt_basicDatatypes[DT_UB];*/
   extent3  = extent2;

   ompi_ddt_create_struct( 2, blens, displ, types, &type3 );
   ompi_ddt_commit( &type3 );

   ompi_ddt_get_extent( type3, &lb, &extent );
   if (extent != extent3) {
      printf("EXTENT 3 %ld != %ld\n",extent,extent3);
      errs++;
      printf("extent(type3)=%ld\n",(long)extent);
   }

   OBJ_RELEASE( type1 ); /*assert( type1 == NULL );*/
   OBJ_RELEASE( type2 ); /*assert( type2 == NULL );*/
   OBJ_RELEASE( type3 ); assert( type3 == NULL );
   return errs;
}

int mpich_typeub2( void )
{
   int blocklen[3], err = 0, sz1, sz2, sz3;
   long disp[3], lb, ub, ex1, ex2, ex3;
   ompi_datatype_t *types[3], *dt1, *dt2, *dt3;

   blocklen[0] = 1;
   blocklen[1] = 1;
   blocklen[2] = 1;
   disp[0] = -3;
   disp[1] = 0;
   disp[2] = 6;
   types[0] = &ompi_mpi_lb;  /* ompi_ddt_basicDatatypes[DT_LB]; */
   types[1] = &ompi_mpi_int;  /* ompi_ddt_basicDatatypes[DT_INT]; */
   types[2] = &ompi_mpi_ub;  /* ompi_ddt_basicDatatypes[DT_UB]; */

   ompi_ddt_create_struct(3,blocklen,disp, types,&dt1);
   ompi_ddt_commit(&dt1);

   ompi_ddt_type_lb(dt1, &lb);          ompi_ddt_type_ub(dt1, &ub);
   ompi_ddt_type_extent(dt1,&ex1);      ompi_ddt_type_size(dt1,&sz1);

   /* Values should be lb = -3, ub = 6 extent 9; size depends on implementation */
   if (lb != -3 || ub != 6 || ex1 != 9) {
      printf("Example 3.26 type1 lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex1, sz1);
      err++;
   }
   else
      printf("Example 3.26 type1 correct\n" );

   ompi_ddt_create_contiguous(2,dt1,&dt2);
   ompi_ddt_type_lb(dt2, &lb);          ompi_ddt_type_ub(dt2, &ub);
   ompi_ddt_type_extent(dt2,&ex2);      ompi_ddt_type_size(dt2,&sz2);
   /* Values should be lb = -3, ub = 15, extent = 18, size depends on implementation */
   if (lb != -3 || ub != 15 || ex2 != 18) {
      printf("Example 3.26 type2 lb %d ub %d extent %d size %d\n", (int)-3, (int)15, (int)18, 8);
      printf("Example 3.26 type2 lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex2, sz2);
      err++;
   }
   else
      printf("Example 3.26 type1 correct\n" );
   OBJ_RELEASE( dt2 ); assert( dt2 == NULL );
   ompi_ddt_create_contiguous(2,dt1,&dt2);
   ompi_ddt_type_lb(dt2, &lb);          ompi_ddt_type_ub(dt2, &ub);
   ompi_ddt_type_extent(dt2,&ex2);      ompi_ddt_type_size(dt2,&sz2);
   /* Values should be lb = -3, ub = 15, extent = 18, size depends on implementation */
   if (lb != -3 || ub != 15 || ex2 != 18) {
      printf("Example 3.26 type2 lb %d ub %d extent %d size %d\n", (int)-3, (int)15, (int)18, 8);
      printf("Example 3.26 type2 lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex2, sz2);
      err++;
   }
   else
      printf( "Example 3.26 type2 correct\n" );

   types[0]=dt1;               types[1]=dt1;
   blocklen[0]=1;              blocklen[1]=1;
   disp[0]=0;                  disp[1]=ex1;

   ompi_ddt_create_struct(2, blocklen, disp, types, &dt3);
   ompi_ddt_commit(&dt3);

   ompi_ddt_type_lb(dt3, &lb);          ompi_ddt_type_ub(dt3, &ub);
   ompi_ddt_type_extent(dt3,&ex3);      ompi_ddt_type_size(dt3,&sz3);
   /* Another way to express type2 */
   if (lb != -3 || ub != 15 || ex3 != 18) {
      printf("type3 lb %d ub %d extent %d size %d\n", (int)-3, (int)15, (int)18, 8);
      printf("type3 lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex3, sz2);
      err++;
   }
   else
      printf( "type3 correct\n" );

   OBJ_RELEASE( dt1 ); /*assert( dt1 == NULL );*/
   OBJ_RELEASE( dt2 ); /*assert( dt2 == NULL );*/
   OBJ_RELEASE( dt3 ); assert( dt3 == NULL );
   return err;
}

int mpich_typeub3( void )
{
   int blocklen[2], sz, err = 0, idisp[3];
   long disp[3], lb, ub, ex;
   ompi_datatype_t *types[3], *dt1, *dt2, *dt3, *dt4, *dt5;

   /* Create a datatype with explicit LB and UB */
   blocklen[0] = 1;
   blocklen[1] = 1;
   blocklen[2] = 1;
   disp[0] = -3;
   disp[1] = 0; 
   disp[2] = 6;
   types[0] = &ompi_mpi_lb;  /* ompi_ddt_basicDatatypes[DT_LB]; */
   types[1] = &ompi_mpi_int;  /* ompi_ddt_basicDatatypes[DT_INT]; */
   types[2] = &ompi_mpi_ub;  /* ompi_ddt_basicDatatypes[DT_UB]; */
   
   /* Generate samples for contiguous, hindexed, hvector, indexed, and vector (struct and contiguous tested in typeub2) */                                                                                                                         
   ompi_ddt_create_struct(3,blocklen,disp, types,&dt1);
   ompi_ddt_commit(&dt1);

   /* This type is the same as in typeub2, and is tested there */
   types[0]=dt1;               types[1]=dt1;
   blocklen[0]=1;              blocklen[1]=1;
   disp[0]=-4;                 disp[1]=7;
   idisp[0]=-4;                idisp[1]=7;

   ompi_ddt_create_hindexed( 2, blocklen, disp, dt1, &dt2 );
   ompi_ddt_commit( &dt2 );

   ompi_ddt_type_lb( dt2, &lb );       ompi_ddt_type_ub( dt2, &ub );
   ompi_ddt_type_extent( dt2, &ex );   ompi_ddt_type_size( dt2, &sz );

   if (lb != -7 || ub != 13 || ex != 20) {
      printf("hindexed lb %d ub %d extent %d size %d\n", (int)-7, (int)13, (int)20, sz);
      printf("hindexed lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex, sz);
      err++;
   }
   else
      printf( "hindexed ok\n" );

   ompi_ddt_create_indexed( 2, blocklen, idisp, dt1, &dt3 );
   ompi_ddt_commit( &dt3 );

   ompi_ddt_type_lb( dt3, &lb );       ompi_ddt_type_ub( dt3, &ub );
   ompi_ddt_type_extent( dt3, &ex );   ompi_ddt_type_size( dt3, &sz );

   if (lb != -39 || ub != 69 || ex != 108) {
      printf("indexed lb %d ub %d extent %d size %d\n", (int)-39, (int)69, (int)108, sz);
      printf("indexed lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex, sz);
      err++;
   }
   else
      printf( "indexed ok\n" );

   ompi_ddt_create_hvector( 2, 1, 14, dt1, &dt4 );
   ompi_ddt_commit( &dt4 );

   ompi_ddt_type_lb( dt4, &lb );       ompi_ddt_type_ub( dt4, &ub );
   ompi_ddt_type_extent( dt4, &ex );   ompi_ddt_type_size( dt4, &sz );

   if (lb != -3 || ub != 20 || ex != 23) {
      printf("hvector lb %d ub %d extent %d size %d\n", (int)-3, (int)20, (int)23, sz);
      printf("hvector lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex, sz);
      err++;
   }
   else
      printf( "hvector ok\n" );

   ompi_ddt_create_vector( 2, 1, 14, dt1, &dt5 );
   ompi_ddt_commit( &dt5 );

   ompi_ddt_type_lb( dt5, &lb );       ompi_ddt_type_ub( dt5, &ub );
   ompi_ddt_type_extent( dt5, &ex );   ompi_ddt_type_size( dt5, &sz );

   if (lb != -3 || ub != 132 || ex != 135) {
      printf("vector lb %d ub %d extent %d size %d\n", (int)-3, (int)132, (int)135, sz);
      printf("vector lb %d ub %d extent %d size %d\n", (int)lb, (int)ub, (int)ex, sz);
      err++;
   }
   else
      printf( "vector ok\n" );

   OBJ_RELEASE( dt1 ); /*assert( dt1 == NULL );*/
   OBJ_RELEASE( dt2 ); /*assert( dt2 == NULL );*/
   OBJ_RELEASE( dt3 ); /*assert( dt3 == NULL );*/
   OBJ_RELEASE( dt4 ); /*assert( dt4 == NULL );*/
   OBJ_RELEASE( dt5 ); assert( dt5 == NULL );
   return err;
}

void print_double_mat( unsigned int N, double* mat )
{
   int i, j;
   double* pMat;

   for( i = 0; i < N; i++ ) {
      printf( "(%4d) :", i * N * (int)sizeof(double) );
      pMat = mat + i * N;
      for( j = 0; j < N; j++ ) {
         printf( "%5.1f ", *pMat );
         pMat++;
      }
      printf( "\n" );
   }
}

int init_random_upper_matrix( unsigned int N, double* mat )
{
    int i, j;

    srand( time(NULL) );
    for( i = 0; i < N; i++ ) {
        mat += i;
        for( j = i; j < N; j++ ) {
            *mat = (double)random();
            mat++;
        }
    }
    return OMPI_SUCCESS;  
}

int check_diag_matrix( unsigned int N, double* mat1, double* mat2 )
{
   int i, j;

   for( i = 0; i < N; i++ ) {
      mat1 += i;
      mat2 += i;
      for( j = i; j < N; j++ ) {
         if( *mat1 != *mat2 ) {
            printf( "error in position (%d, %d) expect %f and find %f\n",
                    i, j, *mat1, *mat2 );
            return OMPI_ERROR;
         }
         mat1++; mat2++;
      }
   }
   return OMPI_SUCCESS;
}

ompi_datatype_t* upper_matrix( unsigned int mat_size )
{
    int *disp, i;
    unsigned int *blocklen;
    ompi_datatype_t* upper;

    disp = (int*)malloc( sizeof(int) * mat_size );
    blocklen = (unsigned int*)malloc( sizeof(unsigned int) * mat_size );

    for( i = 0; i < mat_size; i++ ) {
        disp[i] = i * mat_size + i;
        blocklen[i] = mat_size - i;
    }

    ompi_ddt_create_indexed( mat_size, blocklen, disp, ompi_ddt_basicDatatypes[DT_DOUBLE],
                             &upper );
    ompi_ddt_commit( &upper );
    free( disp );
    free( blocklen );
    return upper;
}

ompi_datatype_t* lower_matrix( unsigned int mat_size )
{
    int *disp, i;
    unsigned int *blocklen;
    ompi_datatype_t* upper;

    disp = (int*)malloc( sizeof(int) * mat_size );
    blocklen = (unsigned int*)malloc( sizeof(unsigned int) * mat_size );

    for( i = 0; i < mat_size; i++ ) {
        disp[i] = i * mat_size;
        blocklen[i] = i;
    }

    ompi_ddt_create_indexed( mat_size, blocklen, disp, ompi_ddt_basicDatatypes[DT_DOUBLE],
                             &upper );
    free( disp );
    free( blocklen );
    return upper;
}

extern long conversion_elapsed;

int test_upper( unsigned int length )
{
    double *mat1, *mat2, *inbuf;
    ompi_datatype_t *pdt, *pdt1;
    ompi_convertor_t * pConv;
    char *ptr;
    int i, j, split_chunk, total_length, rc, iov_count;
    unsigned int max_data, freeAfter;
    struct iovec a;
    TIMER_DATA_TYPE start, end;
    long total_time;

    printf( "test upper matrix\n" );
    pdt = upper_matrix( length );
    pdt1 = lower_matrix( length );
    /*dt_dump( pdt );*/

    mat1 = malloc( length * length * sizeof(double) );
    init_random_upper_matrix( length, mat1 );
    mat2 = calloc( length * length, sizeof(double) );

    total_length = length * (length + 1) * ( sizeof(double) / 2);
    inbuf = (double*)malloc( total_length );
    ptr = (char*)inbuf;
    /* copy upper matrix in the array simulating the input buffer */
    for( i = 0; i < length; i++ )
        for( j = i; j < length; j++ ) {
            *inbuf = mat1[i * length + j];
            inbuf++;
        }
    inbuf = (double*)ptr;
    pConv = ompi_convertor_create( 0, 0 );
    if( OMPI_SUCCESS != ompi_convertor_init_for_recv( pConv, 0, pdt, 1, mat2, 0, NULL ) ) {
        printf( "Cannot attach the datatype to a convertor\n" );
        return OMPI_ERROR;
    }

    /* test the automatic destruction pf the data */
    ompi_ddt_destroy( &pdt ); assert( pdt == NULL );
    ompi_ddt_destroy( &pdt1 ); assert( pdt1 == NULL );

    GET_TIME( start );
    split_chunk = (length + 1) * sizeof(double);
    /*    split_chunk = (total_length + 1) * sizeof(double); */
    for( i = total_length; i > 0; ) {
        if( i <= split_chunk ) {  /* equal test just to be able to set a breakpoint */
            split_chunk = i;
        }
        a.iov_base = ptr;
        a.iov_len = split_chunk;
        iov_count = 1;
        max_data = split_chunk;
        ompi_convertor_unpack( pConv, &a, &iov_count, &max_data, &freeAfter );
        ptr += max_data;
        i -= max_data;
        if( mat2[0] != inbuf[0] ) assert(0);
    }
    GET_TIME( end );
    total_time = ELAPSED_TIME( start, end );
    printf( "complete unpacking in %ld microsec\n", total_time );
    /*    printf( "conversion done in %ld microsec\n", conversion_elapsed ); */
    /*    printf( "stack management in %ld microsec\n", total_time - conversion_elapsed ); */
    free( inbuf );
    rc = check_diag_matrix( length, mat1, mat2 );
    free( mat1 );
    free( mat2 );
    OBJ_RELEASE( pConv );
    return rc;
}

ompi_datatype_t* test_matrix_borders( unsigned int size, unsigned int width )
{
   ompi_datatype_t *pdt, *pdt_line;
   int disp[2];
   unsigned int blocklen[2];
   
   disp[0] = 0;
   blocklen[0] = width;
   disp[1] = (size - width) * sizeof(double);
   blocklen[1] = width;

   ompi_ddt_create_indexed( 2, blocklen, disp, ompi_ddt_basicDatatypes[DT_DOUBLE],
                      &pdt_line );
   ompi_ddt_create_contiguous( size, pdt_line, &pdt );
   OBJ_RELEASE( pdt_line ); /*assert( pdt_line == NULL );*/
   return pdt;
}

ompi_datatype_t* test_contiguous( void )
{
    ompi_datatype_t *pdt, *pdt1, *pdt2;

    printf( "test contiguous (alignement)\n" );
    pdt1 = ompi_ddt_create( -1 );
    ompi_ddt_add( pdt1, ompi_ddt_basicDatatypes[DT_DOUBLE], 1, 0, -1 );
    ompi_ddt_dump( pdt1 );
    ompi_ddt_add( pdt1, ompi_ddt_basicDatatypes[DT_CHAR], 1, 8, -1 );
    ompi_ddt_dump( pdt1 );
    ompi_ddt_create_contiguous( 4, pdt1, &pdt2 );
    OBJ_RELEASE( pdt1 ); /*assert( pdt1 == NULL );*/
    ompi_ddt_dump( pdt2 );
    ompi_ddt_create_contiguous( 2, pdt2, &pdt );
    OBJ_RELEASE( pdt2 ); /*assert( pdt2 == NULL );*/
    ompi_ddt_dump( pdt );
    return pdt;
}

typedef struct __struct_char_double {
    char c;
    double d;
} char_double_t;

ompi_datatype_t* test_struct_char_double( void )
{
    char_double_t data;
    int lengths[] = {1, 1};
    long displ[] = {0, 0};
    ompi_datatype_t *pdt;
    ompi_datatype_t* types[] = { &ompi_mpi_char, &ompi_mpi_double };

    displ[0] = (char*)&(data.c) - (char*)&(data);
    displ[1] = (char*)&(data.d) - (char*)&(data);

    ompi_ddt_create_struct( 2, lengths, displ, types, &pdt );
    ompi_ddt_commit( &pdt );
    ompi_ddt_dump( pdt );
    return pdt;
}

ompi_datatype_t* test_create_twice_two_doubles( void )
{
    ompi_datatype_t* pdt;

    ompi_ddt_create_vector( 2, 2, 5, &ompi_mpi_double, &pdt );
    ompi_ddt_commit( &pdt );
    ompi_ddt_dump( pdt );
    return pdt;
}

/*
  Datatype 0x832cf28 size 0 align 1 id 0 length 4 used 0
  true_lb 0 true_ub 0 (true_extent 0) lb 0 ub 0 (extent 0)
  nbElems 0 loops 0 flags 6 (commited contiguous )-cC--------[---][---]
  contain 13 disp 0x420 (1056) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x478 (1144) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x4d0 (1232) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x528 (1320) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x580 (1408) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x5d8 (1496) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 13 disp 0x630 (1584) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 12 disp 0x68c (1676) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 11 disp 0x6e8 (1768) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 10 disp 0x744 (1860) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 9 disp 0x7a0 (1952) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 8 disp 0x7fc (2044) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 7 disp 0x858 (2136) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 6 disp 0x8b4 (2228) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 5 disp 0x910 (2320) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 4 disp 0x96c (2412) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 3 disp 0x9c8 (2504) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 2 disp 0xa24 (2596) extent 4
  --C-----D*-[ C ][INT]        MPI_INT count 1 disp 0xa80 (2688) extent 4
*/
static int blacs_length[] = { 13, 13, 13, 13, 13, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };
static int blacs_indices[] = { 1144/4, 1232/4, 1320/4, 1408/4, 1496/4, 1584/4, 1676/4, 1768/4,
                               1860/4, 1952/4, 2044/4, 2136/4, 2228/4, 2320/4, 2412/4, 2504/4,
                               2596/4, 2688/4 };
ompi_datatype_t* test_create_blacs_type( void )
{
    ompi_datatype_t *pdt;

    ompi_ddt_create_indexed( 18, blacs_length, blacs_indices, &ompi_mpi_int, &pdt );
    ompi_ddt_commit( &pdt );
    ompi_ddt_dump( pdt );
    return pdt;
}

ompi_datatype_t* test_create_blacs_type1( ompi_datatype_t* base_type )
{
    ompi_datatype_t *pdt;

    ompi_ddt_create_vector( 7, 1, 3, base_type, &pdt );
    ompi_ddt_commit( &pdt );
    ompi_ddt_dump( pdt );
    return pdt;
}

ompi_datatype_t* test_create_blacs_type2( ompi_datatype_t* base_type )
{
    ompi_datatype_t *pdt;

    ompi_ddt_create_vector( 7, 1, 2, base_type, &pdt );
    ompi_ddt_commit( &pdt );
    ompi_ddt_dump( pdt );
    return pdt;
}

ompi_datatype_t* test_struct( void )
{
    ompi_datatype_t* types[] = { &ompi_mpi_float  /* ompi_ddt_basicDatatypes[DT_FLOAT] */,
                                 NULL, 
                                 &ompi_mpi_char  /* ompi_ddt_basicDatatypes[DT_CHAR] */ };
    int lengths[] = { 2, 1, 3 };
    long disp[] = { 0, 16, 26 };
    ompi_datatype_t* pdt, *pdt1;
   
    printf( "test struct\n" );
    pdt1 = ompi_ddt_create( -1 );
    ompi_ddt_add( pdt1, ompi_ddt_basicDatatypes[DT_DOUBLE], 1, 0, -1 );
    ompi_ddt_add( pdt1, ompi_ddt_basicDatatypes[DT_CHAR], 1, 8, -1 );
    ompi_ddt_dump( pdt1 );

    types[1] = pdt1;

    ompi_ddt_create_struct( 3, lengths, disp, types, &pdt );
    OBJ_RELEASE( pdt1 ); /*assert( pdt1 == NULL );*/
    ompi_ddt_dump( pdt );
    return pdt;
}

typedef struct {
    int i1;
    int gap;
    int i2;
} sdata_intern;

typedef struct {
    int counter;
    sdata_intern v[10];
    int last;
} sstrange;

#define SSTRANGE_CNT 10
#define USE_RESIZED

ompi_datatype_t* create_strange_dt( void )
{
    sdata_intern v[2];
    long displ[3];
    ompi_datatype_t* types[3] = { &ompi_mpi_int };
    sstrange t[2];
    int pBlock[3] = {1, 10, 1}, dispi[3];
    ompi_datatype_t *pdt, *pdt1, *pdt2, *pdtTemp;

    dispi[0] = (int)((char*)&(v[0].i1) - (char*)&(v[0]));  /* 0 */
    dispi[1] = (int)(((char*)(&(v[0].i2)) - (char*)&(v[0])) / sizeof(int));  /* 2 */
    ompi_ddt_create_indexed_block( 2, 1, dispi, ompi_ddt_basicDatatypes[DT_INT], &pdtTemp );
#ifdef USE_RESIZED
    /* optional */
    displ[0] = 0;
    displ[1] = (char*)&(v[1]) - (char*)&(v[0]);
    ompi_ddt_create_resized( pdtTemp, displ[0], displ[1], &pdt1 );
    OBJ_RELEASE( pdtTemp ); assert( pdtTemp == NULL );
#else
    pdt1 = pdtTemp;
#endif  /* USE_RESIZED */

    types[1] = pdt1;
    types[2] = &ompi_mpi_int;
    displ[0] = 0;
    displ[1] = (long)((char*)&(t[0].v[0]) - (char*)&(t[0]));
    displ[2] = (long)((char*)&(t[0].last) - (char*)&(t[0]));
    ompi_ddt_create_struct( 3, pBlock, displ, types, &pdtTemp );
#ifdef USE_RESIZED
    /* optional */
    displ[1] = (char*)&(t[1]) - (char*)&(t[0]);
    ompi_ddt_create_resized( pdtTemp, displ[0], displ[1], &pdt2 );
    OBJ_RELEASE( pdtTemp ); assert( pdtTemp == NULL );
#else
    pdt2 = pdtTemp;
#endif  /* USE_RESIZED */

    ompi_ddt_create_contiguous( SSTRANGE_CNT, pdt2, &pdt );

    OBJ_RELEASE( pdt1 );
    OBJ_RELEASE( pdt2 );
    printf( "\nStrange datatype BEFORE COMMIT\n" );
    ompi_ddt_dump( pdt );
    ompi_ddt_commit( &pdt );
    printf( "\nStrange datatype AFTER COMMIT\n" );
    ompi_ddt_dump( pdt );
    return pdt;
}

int local_copy_ddt_count( ompi_datatype_t* pdt, int count )
{
    long extent;
    void *pdst, *psrc;
    ompi_ddt_type_extent( pdt, &extent );

    pdst = malloc( extent * count );
    psrc = malloc( extent * count );

    if( OMPI_SUCCESS != ompi_ddt_copy_content_same_ddt( pdt, count, pdst, psrc ) ) {
        printf( "Unable to copy the datatype in the function local_copy_ddt_count. Is the datatype committed ?\n" );
    }
    free( pdst );
    free( psrc );

    return OMPI_SUCCESS;
}

int local_copy_with_convertor_2datatypes( ompi_datatype_t* send_type, int send_count,
                                          ompi_datatype_t* recv_type, int recv_count,
                                          int chunk )
{
    long send_extent, recv_extent;
    void *pdst = NULL, *psrc = NULL, *ptemp = NULL;
    ompi_convertor_t *pSendConvertor = NULL, *pRecvConvertor = NULL;
    struct iovec iov;
    uint32_t iov_count, max_data;
    int32_t free_after = 0, length = 0, done1 = 0, done2 = 0;

    ompi_ddt_type_extent( send_type, &send_extent );
    ompi_ddt_type_extent( recv_type, &recv_extent );

    pdst  = malloc( recv_extent * recv_count );
    psrc  = malloc( send_extent * send_count );
    ptemp = malloc( chunk );

    /* fill up the receiver with ZEROS */
    memset( pdst, recv_count * recv_extent, 0 );
    {
        int i;
        for( i = 0; i < (send_count * send_extent); i++ )
            ((char*)psrc)[i] = i % 128 + 32;
    }
    memset( pdst, 0, recv_count * recv_extent );

    pSendConvertor = ompi_convertor_create( 0, 0 );
    if( OMPI_SUCCESS != ompi_convertor_init_for_send( pSendConvertor, 0, send_type, send_count, psrc, 0, NULL ) ) {
        printf( "Unable to create the send convertor. Is the datatype committed ?\n" );
        goto clean_and_return;
    }
    pRecvConvertor = ompi_convertor_create( 0, 0 );
    if( OMPI_SUCCESS != ompi_convertor_init_for_recv( pRecvConvertor, 0, recv_type, recv_count, pdst, 0, NULL ) ) {
        printf( "Unable to create the recv convertor. Is the datatype committed ?\n" );
        goto clean_and_return;
    }

    {  /* Initial destination */
        int i, j;
        for( j = 0; j < 7; j++ ) {
            for( i = 0; i < 2; i++ ) {
                printf( "%08x ", ((int*)pdst)[i*7+j] );
            }
            printf( "\n" );
        }
    }

    while( (done1 & done2) != 1 ) {
        /* They are supposed to finish in exactly the same time. */
        if( done1 | done2 ) {
            printf( "WRONG !!! the send is %d but the receive is %d\n", done1, done2 );
        }

        max_data = chunk;
        iov_count = 1;
        iov.iov_base = ptemp;
        iov.iov_len = chunk;

        if( done1 == 0 ) {
            done1 = ompi_convertor_pack( pSendConvertor, &iov, &iov_count, &max_data, &free_after );
            assert( free_after == 0 );
            if( 1 == done1 ) {
                printf( "pack finished\n" );
            }
            {
                int i;
                for( i = 0; i < 7; i++ )
                    printf( "%x\n", ((int*)ptemp)[i] );
            }
        }

        if( done2 == 0 ) {
            done2 = ompi_convertor_unpack( pRecvConvertor, &iov, &iov_count, &max_data, &free_after );
            assert( free_after == 0 );
            if( 1 == done2 ) {
                printf( "unpack finished\n" );
            }
        }

        length += max_data;
    }
    {  /* final destination */
        int i, j;
        for( j = 0; j < 7; j++ ) {
            for( i = 0; i < 2; i++ ) {
                printf( "%08x ", ((int*)pdst)[i*7+j] );
            }
            printf( "\n" );
        }
    }
 clean_and_return:
    if( pSendConvertor != NULL ) {
        OBJ_RELEASE( pSendConvertor ); assert( pSendConvertor == NULL );
    }
    if( pRecvConvertor != NULL ) {
        OBJ_RELEASE( pRecvConvertor ); assert( pRecvConvertor == NULL );
    }
    if( NULL != pdst ) free( pdst );
    if( NULL != psrc ) free( psrc );
    if( NULL != ptemp ) free( ptemp );
    return OMPI_SUCCESS;
}

int local_copy_with_convertor( ompi_datatype_t* pdt, int count, int chunk )
{
    long extent;
    void *pdst = NULL, *psrc = NULL, *ptemp = NULL;
    ompi_convertor_t *pSendConvertor = NULL, *pRecvConvertor = NULL;
    struct iovec iov;
    uint32_t iov_count, max_data;
    int32_t free_after = 0, length = 0, done1 = 0, done2 = 0;

    ompi_ddt_type_extent( pdt, &extent );

    pdst  = malloc( extent * count );
    psrc  = malloc( extent * count );
    ptemp = malloc( chunk );

    pSendConvertor = ompi_convertor_create( 0, 0 );
    if( OMPI_SUCCESS != ompi_convertor_init_for_send( pSendConvertor, 0, pdt, count, psrc, 0, NULL ) ) {
        printf( "Unable to create the send convertor. Is the datatype committed ?\n" );
        goto clean_and_return;
    }
    pRecvConvertor = ompi_convertor_create( 0, 0 );
    if( OMPI_SUCCESS != ompi_convertor_init_for_recv( pRecvConvertor, 0, pdt, count, pdst, 0, NULL ) ) {
        printf( "Unable to create the recv convertor. Is the datatype committed ?\n" );
        goto clean_and_return;
    }

    while( (done1 & done2) != 1 ) {
        /* They are supposed to finish in exactly the same time. */
        if( done1 | done2 ) {
            printf( "WRONG !!! the send is %s but the receive is %s\n",
                    (done1 ? "finish" : "not finish"),
                    (done2 ? "finish" : "not finish") );
        }

        max_data = chunk;
        iov_count = 1;
        iov.iov_base = ptemp;
        iov.iov_len = chunk;

        if( done1 == 0 ) {
            done1 = ompi_convertor_pack( pSendConvertor, &iov, &iov_count, &max_data, &free_after );
            assert( free_after == 0 );
            if( 1 == done1 ) {
                printf( "pack finished\n" );
            }
        }

        if( done2 == 0 ) {
            done2 = ompi_convertor_unpack( pRecvConvertor, &iov, &iov_count, &max_data, &free_after );
            assert( free_after == 0 );
            if( 1 == done2 ) {
                printf( "unpack finished\n" );
            }
        }

        length += max_data;
    }
 clean_and_return:
    if( pSendConvertor != NULL ) {
        OBJ_RELEASE( pSendConvertor ); assert( pSendConvertor == NULL );
    }
    if( pRecvConvertor != NULL ) {
        OBJ_RELEASE( pRecvConvertor ); assert( pRecvConvertor == NULL );
    }
    if( NULL != pdst ) free( pdst );
    if( NULL != psrc ) free( psrc );
    if( NULL != ptemp ) free( ptemp );
    return OMPI_SUCCESS;
}

int main( int argc, char* argv[] )
{
    ompi_datatype_t *pdt, *pdt1, *pdt2, *pdt3;
    int rc, length = 500;

    ompi_ddt_init();

    pdt = create_strange_dt();
    local_copy_ddt_count(pdt, 1);
    local_copy_with_convertor(pdt, 1, 4008);
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
   
    pdt = upper_matrix(100);
    local_copy_ddt_count(pdt, 1);
    local_copy_with_convertor(pdt, 1, 4008);
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    mpich_typeub();
    mpich_typeub2();
    mpich_typeub3();

    rc = test_upper( length );
    if( rc == 0 )
        printf( "decode [PASSED]\n" );
    else
        printf( "decode [NOT PASSED]\n" );

    pdt = test_matrix_borders( length, 100 );
    ompi_ddt_dump( pdt );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    printf( ">>--------------------------------------------<<\n" );
    pdt = test_contiguous();
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    printf( ">>--------------------------------------------<<\n" );
    pdt = test_struct();
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    printf( ">>--------------------------------------------<<\n" );

    pdt1 = ompi_ddt_create( -1 );
    pdt2 = ompi_ddt_create( -1 );
    pdt3 = ompi_ddt_create( -1 );
    ompi_ddt_add( pdt3, ompi_ddt_basicDatatypes[DT_INT], 10, 0, -1 );
    ompi_ddt_add( pdt3, ompi_ddt_basicDatatypes[DT_FLOAT], 5, 10 * sizeof(int), -1 );

    ompi_ddt_add( pdt2, ompi_ddt_basicDatatypes[DT_INT], 1, 0, -1 );
    ompi_ddt_add( pdt2, pdt3, 3, sizeof(int) * 1, -1 );

    ompi_ddt_add( pdt1, ompi_ddt_basicDatatypes[DT_LONG_LONG], 5, 0, -1 );
    ompi_ddt_add( pdt1, ompi_ddt_basicDatatypes[DT_LONG_DOUBLE], 2, sizeof(long long) * 5, -1 );

    printf( ">>--------------------------------------------<<\n" );
    ompi_ddt_dump( pdt1 );
    printf( ">>--------------------------------------------<<\n" );
    ompi_ddt_dump( pdt2 );
    printf( ">>--------------------------------------------<<\n" );
    ompi_ddt_dump( pdt3 );

    OBJ_RELEASE( pdt1 ); assert( pdt1 == NULL );
    OBJ_RELEASE( pdt2 ); assert( pdt2 == NULL );
    OBJ_RELEASE( pdt3 ); /*assert( pdt3 == NULL );*/

    pdt = test_struct_char_double();
    local_copy_with_convertor( pdt, 4500, 12 );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    pdt = test_create_twice_two_doubles();
    local_copy_with_convertor( pdt, 4500, 12 );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    pdt = test_create_blacs_type();
    local_copy_with_convertor( pdt, 4500, 1023 );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    pdt1 = test_create_blacs_type1( &ompi_mpi_int );
    pdt2 = test_create_blacs_type2( &ompi_mpi_int );
    local_copy_with_convertor_2datatypes( pdt1, 1, pdt2, 1, 100 );
    OBJ_RELEASE( pdt1 ); assert( pdt1 == NULL );
    OBJ_RELEASE( pdt2 ); assert( pdt2 == NULL );

    /* clean-ups all data allocations */
    ompi_ddt_finalize();

    return OMPI_SUCCESS;
}
