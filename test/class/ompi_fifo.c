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
#include <stdlib.h>

#include "support.h"
#include "mca/mpool/mpool.h"

static void *malloc_noalign(size_t size, size_t dummy) {
    return malloc(size);
}

size_t offset;
static void *malloc_base_addr(void){
    return (void *)offset;
}

#include "class/ompi_fifo.h"

/* simple allocator for some simple tests */
mca_mpool_base_module_t pool = {
    NULL, /* component structure */
    malloc_base_addr, /* mca_mpool_base_module_address_fn_t */
    malloc_noalign, /* mca_mpool_base_module_alloc_fn_t */
    realloc, /* ca_mpool_base_module_realloc_fn_t */
    free, /*mca_mpool_base_module_free_fn_t  */
    NULL, /* mca_mpool_base_module_register_fn_t */
    NULL, /* mca_mpool_base_module_deregister_fn_t */
    NULL  /* mca_mpool_base_module_finalize_fn_t */
};


int main(int argc, char **argv) {

    /* local variables */
    ompi_fifo_t fifo;
    int i,j,size_of_fifo,lazy_free,return_status,error_cnt,loop_cnt;
    cb_slot_t *slot_data;
    size_t cnt;
    union {
        int ivalue;
        void *vvalue;
    } value;

#if 0
    /* get queue size */
    size_of_fifo=atoi(argv[1]);
    lazy_free=atoi(argv[2]);
    loop_cnt=atoi(argv[3]);
    offset=atol(argv[4]);
#else
    size_of_fifo = 5;
    lazy_free = 1;
    loop_cnt = 2;
    offset = 2;
#endif

    /* init result tracking */
    test_init("ompi_circular_buffer_fifo");

    /* init fifo */
    return_status=ompi_fifo_init(size_of_fifo,lazy_free,0,0,0,&fifo,&pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_fifo_init \n");
    }

    /* populate fifo */
    error_cnt=0;
    for( i=0 ; i < loop_cnt*ompi_cb_fifo_size( (ompi_cb_fifo_t *)
                                               &(fifo.head->cb_fifo)); i++ ) {
        value.ivalue = i + 5;
        return_status=ompi_fifo_write_to_head(value.vvalue, 
                                              (ompi_fifo_t *)&(fifo), &pool, (size_t)pool.mpool_base());
        if( OMPI_CB_ERROR == return_status ) {
            test_failure(" ompi_cb_fifo_write_to_head\n");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }
    /* pop items off the queue */
    error_cnt=0;
    for( i=0 ; i < loop_cnt*ompi_cb_fifo_size( (ompi_cb_fifo_t *)
                                               &(fifo.head->cb_fifo)); i++ ) {
        value.vvalue = ompi_fifo_read_from_tail(&fifo, 
                                                (size_t)pool.mpool_base());
        if( (i+5) != value.ivalue ) {
            test_failure(" ompi_cb_fifo_read_from_tail\n");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* free fifo */
    return_status=ompi_fifo_free(&fifo,&pool);
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_fifo_free \n");
    }

    /* init fifo */
    return_status=ompi_fifo_init(size_of_fifo,lazy_free,0,0,0,&fifo,&pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_fifo_init \n");
    }

    /*
     * test slot reservation
     */
    cnt = sizeof(cb_slot_t) * loop_cnt * fifo.head->cb_fifo.size;
    slot_data=malloc(cnt);
    if( !slot_data ) {
        test_failure(" can't allocate memory for slot_data");
        goto ERRORS;
    }
    
    /* reserve slot fifo */
    error_cnt=0;
    for( i=0 ; i < loop_cnt*ompi_cb_fifo_size( (ompi_cb_fifo_t *)
                                               &(fifo.head->cb_fifo)); i++ ) {
        slot_data[i]=ompi_fifo_get_slot(&fifo,
                                        &pool, (size_t)pool.mpool_base());
        if( slot_data[i].index < 0 ) {
            test_failure(" ompi_fifo_get_slot \n");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* populate the reserved slots */
    error_cnt=0;
    for( i=0 ; i < loop_cnt*ompi_cb_fifo_size( (ompi_cb_fifo_t *)
                                               &(fifo.head->cb_fifo)); i++ ) {
        value.ivalue = i + 5;
        return_status=ompi_fifo_write_to_slot(&(slot_data[i]),
                                              value.vvalue,
                                              (size_t)pool.mpool_base());
        if( OMPI_CB_ERROR == return_status ) {
            test_failure(" ompi_fifo_write_to_slot \n");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* pop items off the queue */
    error_cnt=0;
    for( i=0 ; i < loop_cnt*ompi_cb_fifo_size( (ompi_cb_fifo_t *)
                                               &(fifo.head->cb_fifo)); i++ ) {
        value.vvalue = ompi_fifo_read_from_tail(&fifo, 
                                                (size_t)pool.mpool_base());
        if( (i+5) != value.ivalue ) {
            test_failure(" ompi_cb_fifo_read_from_tail II\n");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* free fifo */
    return_status=ompi_fifo_free(&fifo,&pool);
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_fifo_free  II\n");
    }

    /* 
     * re-use slots 
     */
    /* init fifo */
    return_status=ompi_fifo_init(size_of_fifo,lazy_free,0,0,0,&fifo,&pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_fifo_init \n");
    }

    /* populate fifo */
    for( j=0 ; j < loop_cnt ; j++ ) {
        error_cnt=0;
        for( i=0 ; i < ompi_cb_fifo_size( (ompi_cb_fifo_t *)
                                          &(fifo.head->cb_fifo)); i++ ) {
            value.ivalue = (i + 5) * (j + 1);
            return_status=ompi_fifo_write_to_head(value.vvalue,
                                                  (ompi_fifo_t *)&(fifo), &pool,
                                                  (size_t)pool.mpool_base());
            if( OMPI_CB_ERROR == return_status ) {
                test_failure(" ompi_cb_fifo_write_to_head\n");
                error_cnt++;
            }
        }
        if( 0 == error_cnt ) {
            test_success();
        }

        /* pop items off the queue */
        error_cnt=0;
        for( i=0 ; i < ompi_cb_fifo_size( (ompi_cb_fifo_t *)
                                          &(fifo.head->cb_fifo)); i++ ) {
            value.vvalue = ompi_fifo_read_from_tail(&fifo,
                                                    (size_t)pool.mpool_base());
            if( ((i+5)*(j+1)) != value.ivalue ) {
                test_failure(" ompi_cb_fifo_read_from_tail\n");
                error_cnt++;
            }
        }
        if( 0 == error_cnt ) {
            test_success();
        }
    }
    
    /* free fifo */
    return_status=ompi_fifo_free(&fifo,&pool);
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_fifo_free \n");
    }

ERRORS:

    /* finalize result tracking */
    return test_finalize();
}
