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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal_ddt_lib.h"
#include "opal/runtime/opal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_convertor.h"
#include <time.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdio.h>
#include <string.h>

/* Compile with:
gcc -DHAVE_CONFIG_H -I. -I../../include -I../.. -I../../include -I../../../ompi-trunk/opal -I../../../ompi-trunk/orte -g opal_datatype_test.c -o opal_datatype_test
*/

#define TIMER_DATA_TYPE struct timeval
#define GET_TIME(TV)   gettimeofday( &(TV), NULL )
#define ELAPSED_TIME(TSTART, TEND)  (((TEND).tv_sec - (TSTART).tv_sec) * 1000000 + ((TEND).tv_usec - (TSTART).tv_usec))

uint32_t remote_arch;

/**
 * Generic function computing the amount of memory to be allocated to fit
 * a contiguous memory layout of count times pdt.
 */
static size_t compute_memory_size( opal_datatype_t const * const pdt, int count )
{
    OPAL_PTRDIFF_TYPE extent, true_lb, true_extent;

    opal_datatype_type_extent( pdt, &extent );
    opal_datatype_get_true_extent( pdt, &true_lb, &true_extent );

    return (size_t)(true_lb + true_extent + (count-1) * extent);
}

static int test_upper( unsigned int length )
{
    double *mat1, *mat2, *inbuf;
    opal_datatype_t *pdt;
    opal_convertor_t * pConv;
    char *ptr;
    int rc;
    unsigned int i, j, iov_count, split_chunk, total_length;
    size_t max_data;
    struct iovec a;
    TIMER_DATA_TYPE start, end;
    long total_time;

    /*printf( "test upper matrix\n" );*/
    pdt = upper_matrix( length );
    /*opal_datatype_dump( pdt );*/

    mat1 = malloc( length * length * sizeof(double) );
    init_random_upper_matrix( length, mat1 );
    mat2 = calloc( length * length, sizeof(double) );

    total_length = length * (length + 1) * ( sizeof(double) / 2);
    inbuf = (double*)malloc( total_length );
    ptr = (char*)inbuf;
    /* copy upper matrix in the array simulating the input buffer */
    for( i = 0; i < length; i++ ) {
        uint32_t pos = i * length + i;
        for( j = i; j < length; j++, pos++ ) {
            *inbuf = mat1[pos];
            inbuf++;
        }
    }
    inbuf = (double*)ptr;
    pConv = opal_convertor_create( remote_arch, 0 );
    if( OPAL_SUCCESS != opal_convertor_prepare_for_recv( pConv, pdt, 1, mat2 ) ) {
        printf( "Cannot attach the datatype to a convertor\n" );
        return OPAL_ERROR;
    }

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
        opal_convertor_unpack( pConv, &a, &iov_count, &max_data );
        ptr += max_data;
        i -= max_data;
        if( mat2[0] != inbuf[0] ) assert(0);
    }
    GET_TIME( end );
    total_time = ELAPSED_TIME( start, end );
    printf( "complete unpacking in %ld microsec\n", total_time );
    free( inbuf );
    rc = check_diag_matrix( length, mat1, mat2 );
    free( mat1 );
    free( mat2 );

    /* test the automatic destruction pf the data */
    opal_datatype_destroy( &pdt );
    assert( pdt == NULL );

    OBJ_RELEASE( pConv );
    return rc;
}


/**
 * Conversion function. They deal with data-types in 3 ways, always making local copies.
 * In order to allow performance testings, there are 3 functions:
 *  - one copying directly from one memory location to another one using the
 *    data-type copy function.
 *  - one which use a 2 convertors created with the same data-type
 *  - and one using 2 convertors created from different data-types.
 *
 */
static int local_copy_ddt_count( opal_datatype_t const * const pdt, int count )
{
    OPAL_PTRDIFF_TYPE lb, extent;
    size_t malloced_size;
    char *odst, *osrc;
    void *pdst, *psrc;
    TIMER_DATA_TYPE start, end;
    long total_time;
    int errors = 0;

    malloced_size = compute_memory_size(pdt, count);
    opal_datatype_get_extent( pdt, &lb, &extent );

    odst = (char*)malloc( malloced_size );
    osrc = (char*)malloc( malloced_size );

    {
        for( size_t i = 0; i < malloced_size; i++ )
            osrc[i] = i % 128 + 32;
        memcpy(odst, osrc, malloced_size);
    }
    pdst = odst - lb;
    psrc = osrc - lb;

    cache_trash();  /* make sure the cache is useless */

    GET_TIME( start );
    if( OPAL_SUCCESS != opal_datatype_copy_content_same_ddt( pdt, count, pdst, psrc ) ) {
        printf( "Unable to copy the datatype in the function local_copy_ddt_count."
                " Is the datatype committed ?\n" );
    }
    GET_TIME( end );
    total_time = ELAPSED_TIME( start, end );
    printf( "direct local copy in %ld microsec\n", total_time );
    if(outputFlags & VALIDATE_DATA) {
        for( size_t i = 0; i < malloced_size; i++ ) {
            if( odst[i] != osrc[i] ) {
                printf("error at position %lu (%d != %d)\n",
                       (unsigned long)i, (int)(odst[i]), (int)(osrc[i]));
                errors++;
                if(outputFlags & QUIT_ON_FIRST_ERROR) {
                    opal_datatype_dump(pdt);
                    assert(0); exit(-1);
                }
            }
        }
        if( 0 == errors ) {
            printf("Validation check succesfully passed\n");
        } else {
            printf("Found %d errors. Giving up!\n", errors);
            exit(-1);
        }
    }
    free( odst );
    free( osrc );

    return (0 == errors ? OPAL_SUCCESS : errors);
}

static int
local_copy_with_convertor_2datatypes( opal_datatype_t const * const send_type, int send_count,
                                      opal_datatype_t const * const recv_type, int recv_count,
                                      int chunk )
{
    OPAL_PTRDIFF_TYPE send_lb, send_extent, recv_lb, recv_extent;
    void *pdst = NULL, *psrc = NULL, *ptemp = NULL;
    char *odst, *osrc;
    opal_convertor_t *send_convertor = NULL, *recv_convertor = NULL;
    struct iovec iov;
    uint32_t iov_count;
    size_t max_data, length = 0, send_malloced_size, recv_malloced_size;;
    int32_t done1 = 0, done2 = 0;
    TIMER_DATA_TYPE start, end, unpack_start, unpack_end;
    long total_time, unpack_time = 0;

    send_malloced_size = compute_memory_size(send_type, send_count);
    recv_malloced_size = compute_memory_size(recv_type, recv_count);

    opal_datatype_get_extent( send_type, &send_lb, &send_extent );
    opal_datatype_get_extent( recv_type, &recv_lb, &recv_extent );

    odst = (char*)malloc( recv_malloced_size );
    osrc = (char*)malloc( send_malloced_size );
    ptemp = malloc( chunk );

    /* fill up the receiver with ZEROS */
    {
        for( size_t i = 0; i < send_malloced_size; i++ )
            osrc[i] = i % 128 + 32;
    }
    memset( odst, 0, recv_malloced_size );
    pdst  = odst - recv_lb;
    psrc  = osrc - send_lb;

    send_convertor = opal_convertor_create( remote_arch, 0 );
    if( OPAL_SUCCESS != opal_convertor_prepare_for_send( send_convertor, send_type, send_count, psrc ) ) {
        printf( "Unable to create the send convertor. Is the datatype committed ?\n" );
        goto clean_and_return;
    }
    recv_convertor = opal_convertor_create( remote_arch, 0 );
    if( OPAL_SUCCESS != opal_convertor_prepare_for_recv( recv_convertor, recv_type, recv_count, pdst ) ) {
        printf( "Unable to create the recv convertor. Is the datatype committed ?\n" );
        goto clean_and_return;
    }

    cache_trash();  /* make sure the cache is useless */

    GET_TIME( start );
    while( (done1 & done2) != 1 ) {
        /* They are supposed to finish in exactly the same time. */
        if( done1 | done2 ) {
            printf( "WRONG !!! the send is %s but the receive is %s in local_copy_with_convertor_2datatypes\n",
                    (done1 ? "finish" : "not finish"),
                    (done2 ? "finish" : "not finish") );
        }

        max_data = chunk;
        iov_count = 1;
        iov.iov_base = ptemp;
        iov.iov_len = chunk;

        if( done1 == 0 ) {
            done1 = opal_convertor_pack( send_convertor, &iov, &iov_count, &max_data );
        }

        if( done2 == 0 ) {
            GET_TIME( unpack_start );
            done2 = opal_convertor_unpack( recv_convertor, &iov, &iov_count, &max_data );
            GET_TIME( unpack_end );
            unpack_time += ELAPSED_TIME( unpack_start, unpack_end );
        }

        length += max_data;

        if( outputFlags & RESET_CONVERTORS ) {
            size_t pos = 0;
            opal_convertor_set_position(send_convertor, &pos);
            pos = length;
            opal_convertor_set_position(send_convertor, &pos);
            assert(pos == length);

            pos = 0;
            opal_convertor_set_position(recv_convertor, &pos);
            pos = length;
            opal_convertor_set_position(recv_convertor, &pos);
            assert(pos == length);
        }
    }
    GET_TIME( end );
    total_time = ELAPSED_TIME( start, end );
    printf( "copying different data-types using convertors in %ld microsec\n", total_time );
    printf( "\t unpack in %ld microsec [pack in %ld microsec]\n", unpack_time,
            total_time - unpack_time );
 clean_and_return:
    if( send_convertor != NULL ) {
        OBJ_RELEASE( send_convertor ); assert( send_convertor == NULL );
    }
    if( recv_convertor != NULL ) {
        OBJ_RELEASE( recv_convertor ); assert( recv_convertor == NULL );
    }
    if( NULL != odst ) free( odst );
    if( NULL != osrc ) free( osrc );
    if( NULL != ptemp ) free( ptemp );
    return OPAL_SUCCESS;
}

static int local_copy_with_convertor( opal_datatype_t const * const pdt, int count, int chunk )
{
    OPAL_PTRDIFF_TYPE lb, extent;
    void *pdst = NULL, *psrc = NULL, *ptemp = NULL;
    char *odst, *osrc;
    opal_convertor_t *send_convertor = NULL, *recv_convertor = NULL;
    struct iovec iov;
    uint32_t iov_count;
    size_t max_data, length = 0, malloced_size;
    int32_t done1 = 0, done2 = 0, errors = 0;
    TIMER_DATA_TYPE start, end, unpack_start, unpack_end;
    long total_time, unpack_time = 0;

    malloced_size = compute_memory_size(pdt, count);
    opal_datatype_get_extent( pdt, &lb, &extent );

    odst = (char*)malloc( malloced_size );
    osrc = (char*)malloc( malloced_size );
    ptemp = malloc( chunk );

    {
        for( size_t i = 0; i < malloced_size; osrc[i] = i % 128 + 32, i++ );
        memcpy(odst, osrc, malloced_size);
    }
    pdst  = odst - lb;
    psrc  = osrc - lb;

    send_convertor = opal_convertor_create( remote_arch, 0 );
    if( OPAL_SUCCESS != opal_convertor_prepare_for_send( send_convertor, pdt, count, psrc ) ) {
        printf( "Unable to create the send convertor. Is the datatype committed ?\n" );
        goto clean_and_return;
    }

    recv_convertor = opal_convertor_create( remote_arch, 0 );
    if( OPAL_SUCCESS != opal_convertor_prepare_for_recv( recv_convertor, pdt, count, pdst ) ) {
        printf( "Unable to create the recv convertor. Is the datatype committed ?\n" );
        goto clean_and_return;
    }

    cache_trash();  /* make sure the cache is useless */

    GET_TIME( start );
    while( (done1 & done2) != 1 ) {
        /* They are supposed to finish in exactly the same time. */
        if( done1 | done2 ) {
            printf( "WRONG !!! the send is %s but the receive is %s in local_copy_with_convertor\n",
                    (done1 ? "finish" : "not finish"),
                    (done2 ? "finish" : "not finish") );
        }

        max_data = chunk;
        iov_count = 1;
        iov.iov_base = ptemp;
        iov.iov_len = chunk;

        if( done1 == 0 ) {
            done1 = opal_convertor_pack( send_convertor, &iov, &iov_count, &max_data );
        }

        if( done2 == 0 ) {
            GET_TIME( unpack_start );
            done2 = opal_convertor_unpack( recv_convertor, &iov, &iov_count, &max_data );
            GET_TIME( unpack_end );
            unpack_time += ELAPSED_TIME( unpack_start, unpack_end );
        }

        length += max_data;
        if( outputFlags & RESET_CONVERTORS ) {
            struct dt_stack_t stack[1+send_convertor->stack_pos];
            int i, stack_pos = send_convertor->stack_pos;
            size_t pos;

            if( 0 == done1 ) {
                memcpy(stack, send_convertor->pStack, (1+send_convertor->stack_pos) * sizeof(struct dt_stack_t));
                pos = 0;
                opal_convertor_set_position(send_convertor, &pos);
                pos = length;
                opal_convertor_set_position(send_convertor, &pos);
                assert(pos == length);
                for(i = 0; i <= stack_pos; i++ ) {
                    if( stack[i].index != send_convertor->pStack[i].index )
                        {errors = 1; printf("send stack[%d].index differs (orig %d != new %d) (completed %lu/%lu)\n",
                                            i, stack[i].index, send_convertor->pStack[i].index,
                                            length, pdt->size * count);}
                    if( stack[i].count != send_convertor->pStack[i].count ) {
                        if( stack[i].type == send_convertor->pStack[i].type ) {
                            {errors = 1; printf("send stack[%d].count differs (orig %lu != new %lu) (completed %lu/%lu)\n",
                                                    i, stack[i].count, send_convertor->pStack[i].count,
                                                    length, pdt->size * count);}
                        } else {
                            if( (OPAL_DATATYPE_MAX_PREDEFINED <= stack[i].type) || (OPAL_DATATYPE_MAX_PREDEFINED <= send_convertor->pStack[i].type) )
                                {errors = 1; printf("send stack[%d].type wrong (orig %d != new %d) (completed %lu/%lu)\n",
                                                    i, (int)stack[i].type, (int)send_convertor->pStack[i].type,
                                                    length, pdt->size * count);}
                            else if( (stack[i].count * opal_datatype_basicDatatypes[stack[i].type]->size) !=
                                     (send_convertor->pStack[i].count * opal_datatype_basicDatatypes[send_convertor->pStack[i].type]->size) )
                                {errors = 1; printf("send stack[%d].type*count differs (orig (%d,%lu) != new (%d, %lu)) (completed %lu/%lu)\n",
                                                    i, (int)stack[i].type, stack[i].count,
                                                    (int)send_convertor->pStack[i].type, send_convertor->pStack[i].count,
                                                    length, pdt->size * count);}
                        }
                    }
                    if( stack[i].disp != send_convertor->pStack[i].disp )
                        {errors = 1; printf("send stack[%d].disp differs (orig %p != new %p) (completed %lu/%lu)\n",
                                            i, (void*)stack[i].disp, (void*)send_convertor->pStack[i].disp,
                                            length, pdt->size * count);}
                    if(0 != errors) {assert(0); exit(-1);}
                }
            }
            if( 0 == done2 ) {
                memcpy(stack, recv_convertor->pStack, (1+recv_convertor->stack_pos) * sizeof(struct dt_stack_t));
                pos = 0;
                opal_convertor_set_position(recv_convertor, &pos);
                pos = length;
                opal_convertor_set_position(recv_convertor, &pos);
                assert(pos == length);
                for(i = 0; i <= stack_pos; i++ ) {
                    if( stack[i].index != recv_convertor->pStack[i].index )
                        {errors = 1; printf("recv stack[%d].index differs (orig %d != new %d) (completed %lu/%lu)\n",
                                            i, stack[i].index, recv_convertor->pStack[i].index,
                                            length, pdt->size * count);}
                    if( stack[i].count != recv_convertor->pStack[i].count ) {
                        if( stack[i].type == recv_convertor->pStack[i].type ) {
                            {errors = 1; printf("recv stack[%d].count differs (orig %lu != new %lu) (completed %lu/%lu)\n",
                                                    i, stack[i].count, recv_convertor->pStack[i].count,
                                                    length, pdt->size * count);}
                        } else {
                            if( (OPAL_DATATYPE_MAX_PREDEFINED <= stack[i].type) || (OPAL_DATATYPE_MAX_PREDEFINED <= recv_convertor->pStack[i].type) )
                                {errors = 1; printf("recv stack[%d].type wrong (orig %d != new %d) (completed %lu/%lu)\n",
                                                    i, (int)stack[i].type, (int)recv_convertor->pStack[i].type,
                                                    length, pdt->size * count);}
                            else if( (stack[i].count * opal_datatype_basicDatatypes[stack[i].type]->size) !=
                                     (recv_convertor->pStack[i].count * opal_datatype_basicDatatypes[recv_convertor->pStack[i].type]->size) )
                                {errors = 1; printf("recv stack[%d].type*count differs (orig (%d,%lu) != new (%d, %lu)) (completed %lu/%lu)\n",
                                                    i, (int)stack[i].type, stack[i].count,
                                                    (int)recv_convertor->pStack[i].type, recv_convertor->pStack[i].count,
                                                    length, pdt->size * count);}
                        }
                    }
                    if( stack[i].disp != recv_convertor->pStack[i].disp )
                        {errors = 1; printf("recv stack[%d].disp differs (orig %p != new %p) (completed %lu/%lu)\n",
                                            i, (void*)stack[i].disp, (void*)recv_convertor->pStack[i].disp,
                                            length, pdt->size * count);}
                    if(0 != errors) {assert(0); exit(-1);}
                }
            }
        }
    }
    GET_TIME( end );
    total_time = ELAPSED_TIME( start, end );
    printf( "copying same data-type using convertors in %ld microsec\n", total_time );
    printf( "\t unpack in %ld microsec [pack in %ld microsec]\n", unpack_time,
            total_time - unpack_time );

    if(outputFlags & VALIDATE_DATA) {
        for( size_t i = errors = 0; i < malloced_size; i++ ) {
            if( odst[i] != osrc[i] ) {
                printf("error at position %lu (%d != %d)\n",
                       (unsigned long)i, (int)(odst[i]), (int)(osrc[i]));
                errors++;
                if(outputFlags & QUIT_ON_FIRST_ERROR) {
                    opal_datatype_dump(pdt);
                    assert(0); exit(-1);
                }
            }
        }
        if( 0 == errors ) {
            printf("Validation check succesfully passed\n");
        } else {
            printf("Found %d errors. Giving up!\n", errors);
            exit(-1);
        }
    }
 clean_and_return:
    if( NULL != send_convertor ) OBJ_RELEASE( send_convertor );
    if( NULL != recv_convertor ) OBJ_RELEASE( recv_convertor );

    if( NULL != odst ) free( odst );
    if( NULL != osrc ) free( osrc );
    if( NULL != ptemp ) free( ptemp );
    return (0 == errors ? OPAL_SUCCESS : errors);
}

/**
 * Main function. Call several tests and print-out the results. It try to stress the convertor
 * using difficult data-type constructions as well as strange segment sizes for the conversion.
 * Usually, it is able to detect most of the data-type and convertor problems. Any modifications
 * on the data-type engine should first pass all the tests from this file, before going into other
 * tests.
 */
int main( int argc, char* argv[] )
{
    opal_datatype_t *pdt, *pdt1, *pdt2, *pdt3;
    int rc, length = 500;

    opal_datatype_init();

    /**
     * By default simulate homogeneous architectures.
     */
    remote_arch = opal_local_arch;
    printf( "\n\n#\n * TEST CREATE CONTIGUOUS\n#\n\n" );
    pdt = create_contiguous_type( &opal_datatype_int1, 10 );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_count(pdt, 100);
        local_copy_with_convertor(pdt, 100, 956);
    }
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    printf( "\n\n#\n * TEST STRANGE DATATYPE\n#\n\n" );
    pdt = create_strange_dt();
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_count(pdt, 1);
        local_copy_with_convertor(pdt, 1, 956);
    }
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    printf( "\n\n#\n * TEST UPPER TRIANGULAR MATRIX (size 100)\n#\n\n" );
    pdt = upper_matrix(100);
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_count(pdt, 1);
        local_copy_with_convertor(pdt, 1, 48);
    }
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    mpich_typeub();
    mpich_typeub2();
    mpich_typeub3();

    printf( "\n\n#\n * TEST UPPER MATRIX\n#\n\n" );
    rc = test_upper( length );
    if( rc == 0 )
        printf( "decode [PASSED]\n" );
    else
        printf( "decode [NOT PASSED]\n" );

    printf( "\n\n#\n * TEST MATRIX BORDERS\n#\n\n" );
    pdt = test_matrix_borders( length, 100 );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt );
    }
    OBJ_RELEASE( pdt ); assert( pdt == NULL );


    printf( "\n\n#\n * TEST CONTIGUOUS\n#\n\n" );
    pdt = test_contiguous();
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    printf( "\n\n#\n * TEST STRUCT\n#\n\n" );
    pdt = test_struct();
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    opal_datatype_create_contiguous(0, &opal_datatype_empty, &pdt1);
    opal_datatype_create_contiguous(0, &opal_datatype_empty, &pdt2);
    opal_datatype_create_contiguous(0, &opal_datatype_empty, &pdt3);

    opal_datatype_add( pdt3, &opal_datatype_int4, 10, 0, -1 );
    opal_datatype_add( pdt3, &opal_datatype_float4, 5, 10 * sizeof(int), -1 );

    opal_datatype_add( pdt2, &opal_datatype_float4, 1, 0, -1 );
    opal_datatype_add( pdt2, pdt3, 3, sizeof(int) * 1, -1 );

    opal_datatype_add( pdt1, &opal_datatype_int8, 5, 0, -1 );
    opal_datatype_add( pdt1, &opal_datatype_float16, 2, sizeof(long long) * 5, -1 );

    printf( ">>--------------------------------------------<<\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt1 );
    }
    printf( ">>--------------------------------------------<<\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt2 );
    }
    printf( ">>--------------------------------------------<<\n" );
    if( outputFlags & DUMP_DATA_AFTER_COMMIT ) {
        opal_datatype_dump( pdt3 );
    }
    
    OBJ_RELEASE( pdt1 ); assert( pdt1 == NULL );
    OBJ_RELEASE( pdt2 ); assert( pdt2 == NULL );
    OBJ_RELEASE( pdt3 ); assert( pdt3 == NULL );

    printf( ">>--------------------------------------------<<\n" );
    printf( " Contiguous data-type (opal_datatype_float8)\n" );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        opal_datatype_t const * const ddt = &opal_datatype_float8;
        local_copy_ddt_count( ddt, 4500);
        local_copy_with_convertor( ddt, 4500, 12 );
        local_copy_with_convertor_2datatypes( ddt, 4500, ddt, 4500, 12 );
    }
    printf( ">>--------------------------------------------<<\n" );
    
    printf( ">>--------------------------------------------<<\n" );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        printf( "Contiguous multiple data-type (4500*1)\n" );
        pdt = create_contiguous_type( &opal_datatype_float8, 4500 );
        local_copy_ddt_count(pdt, 1);
        local_copy_with_convertor( pdt, 1, 12 );
        local_copy_with_convertor_2datatypes( pdt, 1, pdt, 1, 12 );
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (450*10)\n" );
        pdt = create_contiguous_type( &opal_datatype_float8, 450 );
        local_copy_ddt_count(pdt, 10);
        local_copy_with_convertor( pdt, 10, 12 );
        local_copy_with_convertor_2datatypes( pdt, 10, pdt, 10, 12 );
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (45*100)\n" );
        pdt = create_contiguous_type( &opal_datatype_float8, 45 );
        local_copy_ddt_count(pdt, 100);
        local_copy_with_convertor( pdt, 100, 12 );
        local_copy_with_convertor_2datatypes( pdt, 100, pdt, 100, 12 );
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (100*45)\n" );
        pdt = create_contiguous_type( &opal_datatype_float8, 100 );
        local_copy_ddt_count(pdt, 45);
        local_copy_with_convertor( pdt, 45, 12 );
        local_copy_with_convertor_2datatypes( pdt, 45, pdt, 45, 12 );
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (10*450)\n" );
        pdt = create_contiguous_type( &opal_datatype_float8, 10 );
        local_copy_ddt_count(pdt, 450);
        local_copy_with_convertor( pdt, 450, 12 );
        local_copy_with_convertor_2datatypes( pdt, 450, pdt, 450, 12 );
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
        printf( "Contiguous multiple data-type (1*4500)\n" );
        pdt = create_contiguous_type( &opal_datatype_float8, 1 );
        local_copy_ddt_count(pdt, 4500);
        local_copy_with_convertor( pdt, 4500, 12 );
        local_copy_with_convertor_2datatypes( pdt, 4500, pdt, 4500, 12 );
        OBJ_RELEASE( pdt ); assert( pdt == NULL );
    }
    printf( ">>--------------------------------------------<<\n" );
    printf( ">>--------------------------------------------<<\n" );
    printf( "Vector data-type (450 times 10 double stride 11)\n" );
    pdt = create_vector_type( &opal_datatype_float8, 450, 10, 11 );
    opal_datatype_dump( pdt );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_count(pdt, 1);
        local_copy_with_convertor( pdt, 1, 12 );
        local_copy_with_convertor_2datatypes( pdt, 1, pdt, 1, 12 );
        local_copy_with_convertor( pdt, 1, 82 );
        local_copy_with_convertor_2datatypes( pdt, 1, pdt, 1, 82 );
        local_copy_with_convertor( pdt, 1, 6000 );
        local_copy_with_convertor_2datatypes( pdt, 1, pdt, 1, 6000 );
        local_copy_with_convertor( pdt, 1, 36000 );
        local_copy_with_convertor_2datatypes( pdt, 1, pdt, 1, 36000 );
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    printf( ">>--------------------------------------------<<\n" );
    printf( "Struct data-type resized (double unused followed by 2 used doubles)\n" );
    pdt = create_struct_constant_gap_resized_ddt( &opal_datatype_float8 );
    opal_datatype_dump( pdt );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_count(pdt, 1);
        local_copy_with_convertor( pdt, 100, 11 );
        local_copy_with_convertor_2datatypes( pdt, 100, pdt, 100, 11 );
        local_copy_with_convertor( pdt, 100, 82 );
        local_copy_with_convertor_2datatypes( pdt, 100, pdt, 100, 81 );
        local_copy_with_convertor( pdt, 1500, 6000 );
        local_copy_with_convertor_2datatypes( pdt, 1500, pdt, 1500, 666 );
        local_copy_with_convertor( pdt, 10000, 36000 );
        local_copy_with_convertor_2datatypes( pdt, 10000, pdt, 10000, 1111 );
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
     
    printf( ">>--------------------------------------------<<\n" );
    pdt = test_struct_char_double();
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_count(pdt, 4500);
        local_copy_with_convertor( pdt, 4500, 12 );
        local_copy_with_convertor_2datatypes( pdt, 4500, pdt, 4500, 12 );
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );
    
    printf( ">>--------------------------------------------<<\n" );
    pdt = test_create_twice_two_doubles();
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_ddt_count(pdt, 4500);
        local_copy_with_convertor( pdt, 4500, 12 );
        local_copy_with_convertor_2datatypes( pdt, 4500, pdt, 4500, 12 );
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    printf( ">>--------------------------------------------<<\n" );
    pdt = test_create_blacs_type();
    if( outputFlags & CHECK_PACK_UNPACK ) {
        opal_datatype_dump( pdt );
        local_copy_ddt_count(pdt, 4500);
        local_copy_with_convertor( pdt, 4500, 956 );
        local_copy_with_convertor_2datatypes( pdt, 4500, pdt, 4500, 956 );
        local_copy_with_convertor( pdt, 4500, 16*1024 );
        local_copy_with_convertor_2datatypes( pdt, 4500, pdt, 4500, 16*1024 );
        local_copy_with_convertor( pdt, 4500, 64*1024 );
        local_copy_with_convertor_2datatypes( pdt, 4500, pdt, 4500, 64*1024 );
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt ); assert( pdt == NULL );

    printf( ">>--------------------------------------------<<\n" );
    pdt1 = test_create_blacs_type1( &opal_datatype_int4 );
    pdt2 = test_create_blacs_type2( &opal_datatype_int4 );
    if( outputFlags & CHECK_PACK_UNPACK ) {
        local_copy_with_convertor_2datatypes( pdt1, 1, pdt2, 1, 100 );
    }
    printf( ">>--------------------------------------------<<\n" );
    OBJ_RELEASE( pdt1 ); assert( pdt1 == NULL );
    OBJ_RELEASE( pdt2 ); assert( pdt2 == NULL );

    /* clean-ups all data allocations */
    opal_datatype_finalize();
    opal_finalize();
    return OPAL_SUCCESS;
}
