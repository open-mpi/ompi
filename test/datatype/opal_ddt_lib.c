/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2009-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "opal_ddt_lib.h"

#include "opal/constants.h"
#include "opal/datatype/opal_datatype.h"


uint32_t outputFlags = VALIDATE_DATA | CHECK_PACK_UNPACK | RESET_CONVERTORS | QUIT_ON_FIRST_ERROR;

static int32_t opal_datatype_create_indexed( int count, const int* pBlockLength, const int* pDisp,
                                             const opal_datatype_t* oldType, opal_datatype_t** newType );
static int32_t opal_datatype_create_hindexed( int count, const int* pBlockLength, const ptrdiff_t* pDisp,
                                              const opal_datatype_t* oldType, opal_datatype_t** newType );
static int32_t opal_datatype_create_struct( int count, const int* pBlockLength,
                                            const ptrdiff_t* pDisp,
                                            opal_datatype_t** pTypes, opal_datatype_t** newType );
static int32_t opal_datatype_create_vector( int count, int bLength, int stride,
                                            const opal_datatype_t* oldType, opal_datatype_t** newType );
static int32_t opal_datatype_create_hvector( int count, int bLength, ptrdiff_t stride,
                                             const opal_datatype_t* oldType, opal_datatype_t** newType );


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

opal_datatype_t* test_create_twice_two_doubles( void )
{
    opal_datatype_t* pdt;

    opal_datatype_create_vector( 2, 2, 5, &opal_datatype_float8, &pdt );
    opal_datatype_commit( pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
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
opal_datatype_t* test_create_blacs_type( void )
{
    opal_datatype_t *pdt;

    opal_datatype_create_indexed( 18, blacs_length, blacs_indices, &opal_datatype_int4, &pdt );
    opal_datatype_commit( pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }
    return pdt;
}

opal_datatype_t* test_create_blacs_type1( opal_datatype_t const * const base_type )
{
    opal_datatype_t *pdt;

    opal_datatype_create_vector( 7, 1, 3, base_type, &pdt );
    opal_datatype_commit( pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }
    return pdt;
}

opal_datatype_t* test_create_blacs_type2( opal_datatype_t const * const base_type )
{
    opal_datatype_t *pdt;

    opal_datatype_create_vector( 7, 1, 2, base_type, &pdt );
    opal_datatype_commit( pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }
    return pdt;
}


opal_datatype_t* test_struct( void )
{
    opal_datatype_t* types[] = { (opal_datatype_t*)&opal_datatype_float4,
                                 NULL,
                                 (opal_datatype_t*)&opal_datatype_int1 };
    int lengths[] = { 2, 1, 3 };
    ptrdiff_t disp[] = { 0, 16, 26 };
    opal_datatype_t* pdt, *pdt1;

    printf( "test struct\n" );
    opal_datatype_create_contiguous(0, &opal_datatype_empty, &pdt1);
    opal_datatype_add( pdt1, &opal_datatype_float8, 1, 0, -1 );
    opal_datatype_add( pdt1, &opal_datatype_int1, 1, 8, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt1 );
    }

    types[1] = pdt1;

    opal_datatype_create_struct( 3, lengths, disp, types, &pdt );
    OBJ_RELEASE( pdt1 ); /*assert( pdt1 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }
    return pdt;
}

typedef struct __struct_char_double {
    char c;
    double d;
} char_double_t;

opal_datatype_t* test_struct_char_double( void )
{
    char_double_t data;
    int lengths[] = {1, 1};
    ptrdiff_t displ[] = {0, 0};
    opal_datatype_t *pdt;
    opal_datatype_t* types[] = { (opal_datatype_t*)&opal_datatype_int1,
                                 (opal_datatype_t*)&opal_datatype_float8};

    displ[0] = (char*)&(data.c) - (char*)&(data);
    displ[1] = (char*)&(data.d) - (char*)&(data);

    opal_datatype_create_struct( 2, lengths, displ, types, &pdt );
    opal_datatype_commit( pdt );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }
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

opal_datatype_t* create_strange_dt( void )
{
    sdata_intern v[2];
    ptrdiff_t displ[3];
    opal_datatype_t *pdt, *pdt1;

    opal_datatype_create_contiguous(0, &opal_datatype_empty, &pdt1);
    opal_datatype_add( pdt1, &opal_datatype_float8, 1, 0, -1 );
    opal_datatype_add( pdt1, &opal_datatype_int1, 1, 8, -1 );

#ifdef USE_RESIZED
    /* optional */
    displ[0] = 0;
    displ[1] = (char*)&(v[1]) - (char*)&(v[0]);
    opal_datatype_resize( pdt1, displ[0], displ[1]);
#endif  /* USE_RESIZED */

    opal_datatype_create_contiguous( SSTRANGE_CNT, pdt1, &pdt );

    OBJ_RELEASE( pdt1 );
    printf( "\nStrange datatype BEFORE COMMIT\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }

    opal_datatype_commit( pdt );
    printf( "\nStrange datatype AFTER COMMIT\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }
    return pdt;
}

opal_datatype_t* create_vector_type( const opal_datatype_t* data, int count, int length, int stride )
{
    opal_datatype_t* vector;

    opal_datatype_create_vector( count, length, stride, data, &vector );
    opal_datatype_commit( vector );
    return vector;
}


opal_datatype_t* create_contiguous_type( const opal_datatype_t* type, int length )
{
   opal_datatype_t* newtype;

   opal_datatype_create_contiguous( length, type, &newtype );
   opal_datatype_commit( newtype );

   return newtype;
}

/* Create a non-contiguous resized datatype */
struct structure {
    double not_transfered;
    double transfered_1;
    double transfered_2;
};

opal_datatype_t* create_struct_constant_gap_resized_ddt( const opal_datatype_t* type )
{
    opal_datatype_t *struct_type;

    opal_datatype_create_contiguous(0, &opal_datatype_empty, &struct_type);
    opal_datatype_add( struct_type, &opal_datatype_float8, 1,  8, -1 );
    opal_datatype_add( struct_type, &opal_datatype_float8, 1, 16, -1 );

    opal_datatype_resize(struct_type, 0, sizeof(struct structure));
    opal_datatype_commit(struct_type);

    return struct_type;
}


/*****************************************************************************/
/* Copied Function to get test to work */
/*****************************************************************************/
static int32_t opal_datatype_create_indexed( int count, const int* pBlockLength, const int* pDisp,
                                             const opal_datatype_t* oldType, opal_datatype_t** newType )
{
    opal_datatype_t* pdt;
    int i, dLength, endat, disp;
    ptrdiff_t extent;

    if( 0 == count ) {
        *newType = opal_datatype_create( 0 );
        opal_datatype_add( *newType, &opal_datatype_empty, 0, 0, 0);
        return OPAL_SUCCESS;
    }

    disp = pDisp[0];
    dLength = pBlockLength[0];
    endat = disp + dLength;
    opal_datatype_type_extent( oldType, &extent );
    if( 1 >= count ) {
        pdt = opal_datatype_create( oldType->desc.used + 2 );
        /* multiply by count to make it zero if count is zero */
        opal_datatype_add( pdt, oldType, count * dLength, disp * extent, extent );
    } else {
        pdt = opal_datatype_create( count * (2 + oldType->desc.used) );
        for( i = 1; i < count; i++ ) {
            if( endat == pDisp[i] ) {
                /* contiguous with the previsious */
                dLength += pBlockLength[i];
                endat += pBlockLength[i];
            } else {
                opal_datatype_add( pdt, oldType, dLength, disp * extent, extent );
                disp = pDisp[i];
                dLength = pBlockLength[i];
                endat = disp + pBlockLength[i];
            }
        }
        opal_datatype_add( pdt, oldType, dLength, disp * extent, extent );
    }

    *newType = pdt;
    return OPAL_SUCCESS;
}

static int32_t opal_datatype_create_hindexed( int count, const int* pBlockLength, const ptrdiff_t* pDisp,
                                              const opal_datatype_t* oldType, opal_datatype_t** newType )
{
    opal_datatype_t* pdt;
    int i, dLength;
    ptrdiff_t extent, disp, endat;

    if( 0 == count ) {
        *newType = opal_datatype_create( 0 );
        opal_datatype_add( *newType, &opal_datatype_empty, 0, 0, 0);
        return OPAL_SUCCESS;
    }

    opal_datatype_type_extent( oldType, &extent );

    pdt = opal_datatype_create( count * (2 + oldType->desc.used) );
    disp = pDisp[0];
    dLength = pBlockLength[0];
    endat = disp + dLength * extent;
    if( 1 >= count ) {
        pdt = opal_datatype_create( oldType->desc.used + 2 );
        /* multiply by count to make it zero if count is zero */
        opal_datatype_add( pdt, oldType, count * dLength, disp, extent );
    } else {
        for( i = 1; i < count; i++ ) {
            if( endat == pDisp[i] ) {
                /* contiguous with the previsious */
                dLength += pBlockLength[i];
                endat += pBlockLength[i] * extent;
            } else {
                opal_datatype_add( pdt, oldType, dLength, disp, extent );
                disp = pDisp[i];
                dLength = pBlockLength[i];
                endat = disp + pBlockLength[i] * extent;
            }
        }
        opal_datatype_add( pdt, oldType, dLength, disp, extent );
    }
    *newType = pdt;
    return OPAL_SUCCESS;
}


static int32_t opal_datatype_create_struct( int count, const int* pBlockLength, const ptrdiff_t* pDisp,
                                            opal_datatype_t** pTypes, opal_datatype_t** newType )
{
    int i;
    ptrdiff_t disp = 0, endto, lastExtent, lastDisp;
    int lastBlock;
    opal_datatype_t *pdt, *lastType;

    if( 0 == count ) {
        *newType = opal_datatype_create( 0 );
        opal_datatype_add( *newType, &opal_datatype_empty, 0, 0, 0);
        return OPAL_SUCCESS;
    }

    /* if we compute the total number of elements before we can
     * avoid increasing the size of the desc array often.
     */
    lastType = (opal_datatype_t*)pTypes[0];
    lastBlock = pBlockLength[0];
    lastExtent = lastType->ub - lastType->lb;
    lastDisp = pDisp[0];
    endto = pDisp[0] + lastExtent * lastBlock;

    for( i = 1; i < count; i++ ) {
        if( (pTypes[i] == lastType) && (pDisp[i] == endto) ) {
            lastBlock += pBlockLength[i];
            endto = lastDisp + lastBlock * lastExtent;
        } else {
            disp += lastType->desc.used;
            if( lastBlock > 1 ) disp += 2;
            lastType = (opal_datatype_t*)pTypes[i];
            lastExtent = lastType->ub - lastType->lb;
            lastBlock = pBlockLength[i];
            lastDisp = pDisp[i];
            endto = lastDisp + lastExtent * lastBlock;
        }
    }
    disp += lastType->desc.used;
    if( lastBlock != 1 ) disp += 2;

    lastType = (opal_datatype_t*)pTypes[0];
    lastBlock = pBlockLength[0];
    lastExtent = lastType->ub - lastType->lb;
    lastDisp = pDisp[0];
    endto = pDisp[0] + lastExtent * lastBlock;

    pdt = opal_datatype_create( (int32_t)disp );

    /* Do again the same loop but now add the elements */
    for( i = 1; i < count; i++ ) {
        if( (pTypes[i] == lastType) && (pDisp[i] == endto) ) {
            lastBlock += pBlockLength[i];
            endto = lastDisp + lastBlock * lastExtent;
        } else {
            opal_datatype_add( pdt, lastType, lastBlock, lastDisp, lastExtent );
            lastType = (opal_datatype_t*)pTypes[i];
            lastExtent = lastType->ub - lastType->lb;
            lastBlock = pBlockLength[i];
            lastDisp = pDisp[i];
            endto = lastDisp + lastExtent * lastBlock;
        }
    }
    opal_datatype_add( pdt, lastType, lastBlock, lastDisp, lastExtent );

     *newType = pdt;
    return OPAL_SUCCESS;
}


static int32_t opal_datatype_create_vector( int count, int bLength, int stride,
                                            const opal_datatype_t* oldType, opal_datatype_t** newType )
{
    opal_datatype_t *pTempData, *pData;
    ptrdiff_t extent = oldType->ub - oldType->lb;


    if( 0 == count ) {
        *newType = opal_datatype_create( 0 );
        opal_datatype_add( *newType, &opal_datatype_empty, 0, 0, 0);
        return OPAL_SUCCESS;
    }

    pData = opal_datatype_create( oldType->desc.used + 2 );
    if( (bLength == stride) || (1 == count) ) {  /* the elements are contiguous */
        opal_datatype_add( pData, oldType, count * bLength, 0, extent );
    } else {
        if( 1 == bLength ) {
            opal_datatype_add( pData, oldType, count, 0, extent * stride );
        } else {
            opal_datatype_add( pData, oldType, bLength, 0, extent );
            pTempData = pData;
            pData = opal_datatype_create( oldType->desc.used + 2 + 2 );
            opal_datatype_add( pData, pTempData, count, 0, extent * stride );
            OBJ_RELEASE( pTempData );
        }
    }
    *newType = pData;
    return OPAL_SUCCESS;
}


static int32_t opal_datatype_create_hvector( int count, int bLength, ptrdiff_t stride,
                                             const opal_datatype_t* oldType, opal_datatype_t** newType )
{
    opal_datatype_t *pTempData, *pData;
    ptrdiff_t extent = oldType->ub - oldType->lb;

    if( 0 == count ) {
        *newType = opal_datatype_create( 0 );
        opal_datatype_add( *newType, &opal_datatype_empty, 0, 0, 0);
        return OPAL_SUCCESS;
    }

    pTempData = opal_datatype_create( oldType->desc.used + 2 );
    if( ((extent * bLength) == stride) || (1 == count) ) {  /* contiguous */
        pData = pTempData;
        opal_datatype_add( pData, oldType, count * bLength, 0, extent );
    } else {
        if( 1 == bLength ) {
            pData = pTempData;
            opal_datatype_add( pData, oldType, count, 0, stride );
        } else {
            opal_datatype_add( pTempData, oldType, bLength, 0, extent );
            pData = opal_datatype_create( oldType->desc.used + 2 + 2 );
            opal_datatype_add( pData, pTempData, count, 0, stride );
            OBJ_RELEASE( pTempData );
        }
    }
    *newType = pData;
    return OPAL_SUCCESS;
}

/*****************************************************************************/
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
    return OPAL_SUCCESS;
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
            return OPAL_ERROR;
         }
         mat1++; mat2++;
      }
   }
   return OPAL_SUCCESS;
}


opal_datatype_t* upper_matrix( unsigned int mat_size )
{
    int *disp, *blocklen;
    unsigned int i;
    opal_datatype_t* upper;

    disp = (int*)malloc( sizeof(int) * mat_size );
    blocklen = (int*)malloc( sizeof(int) * mat_size );

    for( i = 0; i < mat_size; i++ ) {
        disp[i] = i * mat_size + i;
        blocklen[i] = mat_size - i;
    }

#if SIZEOF_DOUBLE == 4
    opal_datatype_create_indexed( mat_size, blocklen, disp,
                                  &opal_datatype_float4,
                                  &upper );
#else
    opal_datatype_create_indexed( mat_size, blocklen, disp,
                                  &opal_datatype_float8,
                                  &upper );
#endif
    opal_datatype_commit( upper );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( upper );
    }
    free( disp );
    free( blocklen );
    return upper;
}

opal_datatype_t* lower_matrix( unsigned int mat_size )
{
    int *disp, *blocklen;
    unsigned int i;
    opal_datatype_t* upper;

    disp = (int*)malloc( sizeof(int) * mat_size );
    blocklen = (int*)malloc( sizeof(int) * mat_size );

    for( i = 0; i < mat_size; i++ ) {
        disp[i] = i * mat_size;
        blocklen[i] = i;
    }
#if SIZEOF_DOUBLE == 4
    opal_datatype_create_indexed( mat_size, blocklen, disp, &opal_datatype_float4,
                                  &upper );
#else
    opal_datatype_create_indexed( mat_size, blocklen, disp, &opal_datatype_float8,
                                  &upper );
#endif
    free( disp );
    free( blocklen );
    return upper;
}

opal_datatype_t* test_matrix_borders( unsigned int size, unsigned int width )
{
   opal_datatype_t *pdt, *pdt_line;
   int disp[2];
   int blocklen[2];

   disp[0] = 0;
   blocklen[0] = width;
   disp[1] = (size - width) * sizeof(double);
   blocklen[1] = width;

   opal_datatype_create_indexed( 2, blocklen, disp, &opal_datatype_float8,
                                 &pdt_line );
   opal_datatype_create_contiguous( size, pdt_line, &pdt );
   OBJ_RELEASE( pdt_line ); /*assert( pdt_line == NULL );*/
   return pdt;
}


opal_datatype_t* test_contiguous( void )
{
    opal_datatype_t *pdt, *pdt1, *pdt2;

    printf( "test contiguous (alignment)\n" );
    opal_datatype_create_contiguous(0, &opal_datatype_empty, &pdt1);
    opal_datatype_add( pdt1, &opal_datatype_float8, 1, 0, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt1 );
    }
    opal_datatype_add( pdt1, &opal_datatype_int1, 1, 8, -1 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt1 );
    }
    opal_datatype_create_contiguous( 4, pdt1, &pdt2 );
    OBJ_RELEASE( pdt1 ); /*assert( pdt1 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt2 );
    }
    opal_datatype_create_contiguous( 2, pdt2, &pdt );
    OBJ_RELEASE( pdt2 ); /*assert( pdt2 == NULL );*/
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }
    return pdt;
}


