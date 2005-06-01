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

static void *malloc_noalign(mca_mpool_base_module_t* mpool, size_t size, size_t dummy, void* user_out) {
    return malloc(size);
}

size_t offset;
static void *malloc_base_addr(mca_mpool_base_module_t* mpool){
    return (void *)offset;
}
static void my_free(mca_mpool_base_module_t* mpool, void* addr)
{
    free(addr); 
}

static void* my_realloc(mca_mpool_base_module_t* mpool, void* addr, size_t size, void* user_out){
   return  realloc(addr, size);
}

#include "class/ompi_circular_buffer_fifo.h"

/* simple allocator for some simple tests */
mca_mpool_base_module_t pool = {
    NULL, /* component structure */
    malloc_base_addr, /* mca_mpool_base_module_address_fn_t */
    malloc_noalign, /* mca_mpool_base_module_alloc_fn_t */
    my_realloc, /* ca_mpool_base_module_realloc_fn_t */
    my_free, /*mca_mpool_base_module_free_fn_t  */
    NULL, /* mca_mpool_base_module_register_fn_t */
    NULL, /* mca_mpool_base_module_deregister_fn_t */
    NULL  /* mca_mpool_base_module_finalize_fn_t */
};


int main(int argc, char **argv) {

    /* local variables */
    ompi_cb_fifo_t fifo;
    int i,size_of_fifo,lazy_free,return_status,error_cnt,loop_cnt;
    bool queue_empty;
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
    return_status=ompi_cb_fifo_init(size_of_fifo,lazy_free,0,0,0,&fifo,
            &pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init");
    }

    /* populate fifo */
    error_cnt=0;
    for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
        value.ivalue = i + 5;
        return_status=ompi_cb_fifo_write_to_head(value.vvalue, &fifo,
                                                 (size_t)pool.mpool_base(&pool));
        if( OMPI_CB_ERROR == return_status ) {
            test_failure(" ompi_cb_fifo_write_to_head");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* try an over-fill the queue */
    error_cnt=0;
    for( i=0 ; i < 3 ; i++ ) {
        value.ivalue = i;
        return_status=ompi_cb_fifo_write_to_head(value.vvalue, &fifo,
                                                 (size_t)pool.mpool_base(&pool));
        if( OMPI_CB_ERROR != return_status ) {
            test_failure(" ompi_cb_fifo_write_to_head :: over-fill queue");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* pop items off the queue */
    error_cnt=0;
    for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
        value.vvalue =ompi_cb_fifo_read_from_tail(&fifo,0,&queue_empty,
                                                  (size_t)pool.mpool_base(&pool));
        if( (i+5) != value.ivalue ) {
            test_failure(" ompi_cb_fifo_read_from_tail (1)");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* free fifo */
    return_status=ompi_cb_fifo_free(&fifo,&pool);
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init");
    }

    /* init fifo - lazy_free greater than size ==> should return error*/
    return_status=ompi_cb_fifo_init(size_of_fifo,size_of_fifo*2,0,0,0,&fifo,
            &pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS != return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init with lazy_free too large");
    }

    /* split the writting of data to the slot to a reserve, and then a
     * write */

    /* init fifo */
    return_status=ompi_cb_fifo_init(size_of_fifo,lazy_free,0,0,0,&fifo,
            &pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init");
    }

    /* populate fifo */
    error_cnt=0;
    for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
        return_status=ompi_cb_fifo_get_slot(&fifo,(size_t)pool.mpool_base(&pool));
        if( OMPI_CB_ERROR == return_status ) {
            test_failure(" ompi_cb_fifo_get_slot");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* try an over-fill the queue */
    error_cnt=0;
    for( i=0 ; i < 3 ; i++ ) {
        return_status=ompi_cb_fifo_get_slot(&fifo,(size_t)pool.mpool_base(&pool));
        if( OMPI_CB_ERROR != return_status ) {
            test_failure(" ompi_cb_fifo_get_slot :: over-fill queue");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* write to slot - all slots previously reserved, so just use
     * them now */
    error_cnt=0;
    for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
        value.ivalue = i + 5;
        return_status=ompi_cb_fifo_write_to_slot(i, value.vvalue, &fifo,
                                                 (size_t)pool.mpool_base(&pool));
        if( OMPI_CB_ERROR == return_status ) {
            test_failure(" ompi_cb_fifo_write_to_slot");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* pop items off the queue */
    error_cnt=0;
    for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
        value.vvalue = ompi_cb_fifo_read_from_tail(&fifo,0,&queue_empty,
                                                   (size_t)pool.mpool_base(&pool));
        if( (i+5) != value.ivalue ) {
            test_failure(" ompi_cb_fifo_read_from_tail (2)");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* free fifo */
    return_status=ompi_cb_fifo_free(&fifo,&pool);
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init");
    }

    /* go through the fifo multiple times */

    /* init fifo */
    return_status=ompi_cb_fifo_init(size_of_fifo,lazy_free,0,0,0,&fifo,&pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init");
    }

    error_cnt=0;
    for( i=0 ; i < ompi_cb_fifo_size(&fifo)*loop_cnt ; i++ ) {

        /* populate fifo */
        return_status=ompi_cb_fifo_get_slot(&fifo,
                                            (size_t)pool.mpool_base(&pool));
        if( OMPI_CB_ERROR == return_status ) {
            test_failure(" ompi_cb_fifo_get_slot");
            error_cnt++;
        }
        
        /* write to the slot */
        value.ivalue = i + 5;
        return_status=ompi_cb_fifo_write_to_slot(i%(ompi_cb_fifo_size(&fifo)),
                                                 value.vvalue, &fifo,
                                                 (size_t)pool.mpool_base(&pool));
        if( OMPI_CB_ERROR == return_status ) {
            test_failure(" ompi_cb_fifo_write_to_slot");
            error_cnt++;
        }
	
        /* pop items off the queue */
        if( (i % ompi_cb_fifo_size(&fifo) ) ==
            ompi_cb_fifo_size(&fifo)/2  ) {
            /* force a flush */
            value.vvalue = ompi_cb_fifo_read_from_tail(&fifo,1,&queue_empty,
                                                       (size_t)pool.mpool_base(&pool));
        } else {
            value.vvalue = ompi_cb_fifo_read_from_tail(&fifo,0,&queue_empty,
                                                       (size_t)pool.mpool_base(&pool));
        }
        if( (i+5) != value.ivalue ) {
            test_failure(" ompi_cb_fifo_read_from_tail (3)");
            error_cnt++;
        }
    }
    if( 0 == error_cnt ) {
        test_success();
    }

    /* free fifo */
    return_status=ompi_cb_fifo_free(&fifo,&pool);
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init");
    }

    /* finalize result tracking */
    return test_finalize();
}
