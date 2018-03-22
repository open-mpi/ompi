/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sun Microsystems Inc. All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ddt_lib.h"
#include "ompi/constants.h"
#include <time.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>
#include <string.h>

#define TIMER_DATA_TYPE struct timeval
#define GET_TIME(TV)   gettimeofday( &(TV), NULL )
#define ELAPSED_TIME(TSTART, TEND)  (((TEND).tv_sec - (TSTART).tv_sec) * 1000000 + ((TEND).tv_usec - (TSTART).tv_usec))

#define DUMP_DATA_AFTER_COMMIT 0x00000001
#define CHECK_PACK_UNPACK      0x00000002
#define VALIDATE_DATA          0x00000004
uint32_t outputFlags = CHECK_PACK_UNPACK | VALIDATE_DATA;

/**
 * Cache cleanup.
 */
#define CACHE_SIZE (4*1024*1024)
void cache_trash( void )
{
    char* buffer;

    buffer = (char*)malloc( sizeof(char) * CACHE_SIZE );
    memset( buffer, 1, CACHE_SIZE );
    memset( buffer, 0xff, CACHE_SIZE );
    free( buffer );
}

/**
 * Data-type functions.
 */
ompi_datatype_t* create_inversed_vector( const ompi_datatype_t* type, int length )
{
   ompi_datatype_t* type1;

   ompi_datatype_create_vector( length, 1, 2, type, &type1 );

   ompi_datatype_commit( &type1 );
   return type1;
}

void print_double_mat( unsigned int N, double* mat )
{
   unsigned int i, j;
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
    unsigned int i, j;

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
   unsigned int i, j;

   for( i = 0; i < N; i++ ) {
      mat1 += i;
      mat2 += i;
      for( j = i; j < N; j++ ) {
         if( *mat1 != *mat2 ) {
            printf( "error in position (%d, %d) expect %f and find %f\n",
                    i, j, *mat1, *mat2 );
            printf( "hex %lx != %lx\n", *(long*)mat1, *(long*)mat2 );
            return OMPI_ERROR;
         }
         mat1++; mat2++;
      }
   }
   return OMPI_SUCCESS;
}

ompi_datatype_t* upper_matrix( unsigned int mat_size )
{
    int *disp, *blocklen;
    unsigned int i;
    ompi_datatype_t* upper;

    disp = (int*)malloc( sizeof(int) * mat_size );
    blocklen = (int*)malloc( sizeof(int) * mat_size );

    for( i = 0; i < mat_size; i++ ) {
        disp[i] = i * mat_size + i;
        blocklen[i] = mat_size - i;
    }

    ompi_datatype_create_indexed( mat_size, blocklen, disp, &ompi_mpi_double.dt,
                             &upper );
    ompi_datatype_commit( &upper );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( upper );
    }
    free( disp );
    free( blocklen );
    return upper;
}

ompi_datatype_t* lower_matrix( unsigned int mat_size )
{
    int *disp, *blocklen;
    unsigned int i;
    ompi_datatype_t* upper;

    disp = (int*)malloc( sizeof(int) * mat_size );
    blocklen = (int*)malloc( sizeof(int) * mat_size );

    for( i = 0; i < mat_size; i++ ) {
        disp[i] = i * mat_size;
        blocklen[i] = i;
    }

    ompi_datatype_create_indexed( mat_size, blocklen, disp, &ompi_mpi_double.dt,
                             &upper );
    free( disp );
    free( blocklen );
    return upper;
}

ompi_datatype_t* test_matrix_borders( unsigned int size, unsigned int width )
{
   ompi_datatype_t *pdt, *pdt_line;
   int disp[2];
   int blocklen[2];

   disp[0] = 0;
   blocklen[0] = width;
   disp[1] = (size - width) * sizeof(double);
   blocklen[1] = width;

   ompi_datatype_create_indexed( 2, blocklen, disp, &ompi_mpi_double.dt,
                            &pdt_line );
   ompi_datatype_create_contiguous( size, pdt_line, &pdt );
   OBJ_RELEASE( pdt_line ); /*assert( pdt_line == NULL );*/
   return pdt;
}

ompi_datatype_t* test_contiguous( void )
{
    ompi_datatype_t *pdt, *pdt1, *pdt2;

    printf( "test contiguous (alignment)\n" );
    ompi_datatype_create_contiguous(0, &ompi_mpi_datatype_null.dt, &pdt1);
    ompi_datatype_add( pdt1, &ompi_mpi_double.dt, 1, 0, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt1 );
    }
    ompi_datatype_add( pdt1, &ompi_mpi_char.dt, 1, 8, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt1 );
    }
    ompi_datatype_create_contiguous( 4, pdt1, &pdt2 );
    OBJ_RELEASE( pdt1 ); /*assert( pdt1 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt2 );
    }
    ompi_datatype_create_contiguous( 2, pdt2, &pdt );
    OBJ_RELEASE( pdt2 ); /*assert( pdt2 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
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
    MPI_Aint displ[] = {0, 0};
    ompi_datatype_t *pdt;
    ompi_datatype_t* types[] = { &ompi_mpi_char.dt, &ompi_mpi_double.dt};

    displ[0] = (char*)&(data.c) - (char*)&(data);
    displ[1] = (char*)&(data.d) - (char*)&(data);

    ompi_datatype_create_struct( 2, lengths, displ, types, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* test_create_twice_two_doubles( void )
{
    ompi_datatype_t* pdt;

    ompi_datatype_create_vector( 2, 2, 5, &ompi_mpi_double.dt, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

/*
  Datatype 0x832cf28 size 0 align 1 id 0 length 4 used 0
  true_lb 0 true_ub 0 (true_extent 0) lb 0 ub 0 (extent 0)
  nbElems 0 loops 0 flags 6 (committed contiguous )-cC--------[---][---]
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

    ompi_datatype_create_indexed( 18, blacs_length, blacs_indices, &ompi_mpi_int.dt, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* test_create_blacs_type1( const ompi_datatype_t* base_type )
{
    ompi_datatype_t *pdt;

    ompi_datatype_create_vector( 7, 1, 3, base_type, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* test_create_blacs_type2( const ompi_datatype_t* base_type )
{
    ompi_datatype_t *pdt;

    ompi_datatype_create_vector( 7, 1, 2, base_type, &pdt );
    ompi_datatype_commit( &pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* test_struct( void )
{
    ompi_datatype_t* types[] = { &ompi_mpi_float.dt  /* ompi_datatype_basicDatatypes[DT_FLOAT] */,
                                 NULL,
                                 &ompi_mpi_char.dt  /* ompi_datatype_basicDatatypes[DT_CHAR] */ };
    int lengths[] = { 2, 1, 3 };
    MPI_Aint disp[] = { 0, 16, 26 };
    ompi_datatype_t* pdt, *pdt1;

    printf( "test struct\n" );
    ompi_datatype_create_contiguous(0, &ompi_mpi_datatype_null.dt, &pdt1);
    ompi_datatype_add( pdt1, &ompi_mpi_double.dt, 1, 0, -1 );
    ompi_datatype_add( pdt1, &ompi_mpi_char.dt, 1, 8, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt1 );
    }

    types[1] = pdt1;

    ompi_datatype_create_struct( 3, lengths, disp, types, &pdt );
    OBJ_RELEASE( pdt1 ); /*assert( pdt1 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

/* Create a non-contiguous resized datatype */
struct structure {
    double not_transfered;
    double transfered_1;
    double transfered_2;
};

ompi_datatype_t* create_struct_constant_gap_resized_ddt( ompi_datatype_t* type )
{
    struct structure data[1];
    ompi_datatype_t *struct_type, *temp_type;
    ompi_datatype_t *types[2] = {type, type};
    int blocklens[2] = {1, 1};
    MPI_Aint disps[3];

    MPI_Get_address(&data[0].transfered_1, &disps[0]);
    MPI_Get_address(&data[0].transfered_2, &disps[1]);
    MPI_Get_address(&data[0], &disps[2]);
    disps[1] -= disps[2]; /*  8 */
    disps[0] -= disps[2]; /* 16 */

    ompi_datatype_create_struct(2, blocklens, disps, types, &temp_type);
    ompi_datatype_create_resized(temp_type, 0, sizeof(data[0]), &struct_type);
    ompi_datatype_commit(&struct_type);
    OBJ_RELEASE(temp_type); assert( temp_type == NULL );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( struct_type );
    }

    return struct_type;
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
    MPI_Aint displ[3];
    ompi_datatype_t* types[3] = { &ompi_mpi_int.dt };
    sstrange t[2];
    int pBlock[3] = {1, 10, 1}, dispi[3];
    ompi_datatype_t *pdt, *pdt1, *pdt2, *pdtTemp;

    dispi[0] = (int)((char*)&(v[0].i1) - (char*)&(v[0]));  /* 0 */
    dispi[1] = (int)(((char*)(&(v[0].i2)) - (char*)&(v[0])) / sizeof(int));  /* 2 */
    ompi_datatype_create_indexed_block( 2, 1, dispi, &ompi_mpi_int.dt, &pdtTemp );
#ifdef USE_RESIZED
    /* optional */
    displ[0] = 0;
    displ[1] = (char*)&(v[1]) - (char*)&(v[0]);
    ompi_datatype_create_resized( pdtTemp, displ[0], displ[1], &pdt1 );
    OBJ_RELEASE( pdtTemp ); assert( pdtTemp == NULL );
#else
    pdt1 = pdtTemp;
#endif  /* USE_RESIZED */

    types[1] = pdt1;
    types[2] = &ompi_mpi_int.dt;
    displ[0] = 0;
    displ[1] = (long)((char*)&(t[0].v[0]) - (char*)&(t[0]));
    displ[2] = (long)((char*)&(t[0].last) - (char*)&(t[0]));
    ompi_datatype_create_struct( 3, pBlock, displ, types, &pdtTemp );
#ifdef USE_RESIZED
    /* optional */
    displ[1] = (char*)&(t[1]) - (char*)&(t[0]);
    ompi_datatype_create_resized( pdtTemp, displ[0], displ[1], &pdt2 );
    OBJ_RELEASE( pdtTemp ); assert( pdtTemp == NULL );
#else
    pdt2 = pdtTemp;
#endif  /* USE_RESIZED */

    ompi_datatype_create_contiguous( SSTRANGE_CNT, pdt2, &pdt );

    OBJ_RELEASE( pdt1 );
    OBJ_RELEASE( pdt2 );
    printf( "\nStrange datatype BEFORE COMMIT\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    ompi_datatype_commit( &pdt );
    printf( "\nStrange datatype AFTER COMMIT\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        ompi_datatype_dump( pdt );
    }
    return pdt;
}

ompi_datatype_t* create_contiguous_type( const ompi_datatype_t* data, int count )
{
    ompi_datatype_t* contiguous;

    ompi_datatype_create_contiguous( count, data, &contiguous );
    ompi_datatype_commit( &contiguous );
    return contiguous;
}

ompi_datatype_t* create_vector_type( const ompi_datatype_t* data, int count, int length, int stride )
{
    ompi_datatype_t* vector;

    ompi_datatype_create_vector( count, length, stride, data, &vector );
    ompi_datatype_commit( &vector );
    return vector;
}

