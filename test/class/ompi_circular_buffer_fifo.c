
/*   
 * $HEADER$
 */

#include <stdlib.h>

#include "support.h"
#include "mca/mpool/mpool.h"

static void *malloc_noalign(size_t size, size_t dummy) {
    return malloc(size);
}

#include "class/ompi_circular_buffer_fifo.h"

/* simple allocator for some simple tests */
mca_mpool_base_module_t pool = {
    NULL, /* component structure */
    NULL, /* mca_mpool_base_module_address_fn_t */
    malloc_noalign, /* mca_mpool_base_module_alloc_fn_t */
    realloc, /* ca_mpool_base_module_realloc_fn_t */
    free, /*mca_mpool_base_module_free_fn_t  */
    NULL, /* mca_mpool_base_module_register_fn_t */
    NULL, /* mca_mpool_base_module_deregister_fn_t */
    NULL  /* mca_mpool_base_module_finalize_fn_t */
};


int main(int argc, char **argv) {

    /* local variables */
    ompi_cb_fifo_t fifo;
    int i,size_of_fifo,lazy_free,return_status,error_cnt;
	void *ptr;

    /* get queue size */
    size_of_fifo=atoi(argv[1]);
    lazy_free=atoi(argv[2]);

    /* init result tracking */
    test_init("ompi_circular_buffer_fifo");

    /* init fifo */
    return_status=ompi_cb_fifo_init(size_of_fifo,lazy_free,0,0,0,&fifo,
            &pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS == return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init \n");
    }

	/* populate fifo */
    error_cnt=0;
	for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
		return_status=ompi_cb_fifo_write_to_head((void *)(i+5),&fifo);
		if( OMPI_CB_ERROR == return_status ) {
	   		test_failure(" ompi_cb_fifo_write_to_head\n");
            error_cnt++;
		}
	}
    if( 0 == error_cnt ) {
        test_success();
    }

	/* try an over-fill the queue */
    error_cnt=0;
	for( i=0 ; i < 3 ; i++ ) {
		return_status=ompi_cb_fifo_write_to_head((void *)i,&fifo);
		if( OMPI_CB_ERROR != return_status ) {
	   		test_failure(" ompi_cb_fifo_write_to_head :: over-fill queue\n");
            error_cnt++;
		}
	}
    if( 0 == error_cnt ) {
        test_success();
    }

	/* pop items off the queue */
    error_cnt=0;
	for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
		ptr=ompi_cb_fifo_read_from_tail(&fifo);
		if( (void *)(i+5) != ptr ) {
	   		test_failure(" ompi_cb_fifo_read_from_tail\n");
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
        test_failure(" ompi_cv_fifo_init \n");
    }

    /* init fifo - lazy_free greater than size ==> should return error*/
    return_status=ompi_cb_fifo_init(size_of_fifo,size_of_fifo*2,0,0,0,&fifo,
            &pool);
    /* check to see that retrun status is success */
    if( OMPI_SUCCESS != return_status ) {
        test_success();
    } else {
        test_failure(" ompi_cv_fifo_init with lazy_free too large \n");
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
        test_failure(" ompi_cv_fifo_init \n");
    }

	/* populate fifo */
    error_cnt=0;
	for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
		return_status=ompi_cb_fifo_get_slot(&fifo);
		if( OMPI_CB_ERROR == return_status ) {
	   		test_failure(" ompi_cb_fifo_get_slot \n");
            error_cnt++;
		}
	}
    if( 0 == error_cnt ) {
        test_success();
    }

	/* try an over-fill the queue */
    error_cnt=0;
	for( i=0 ; i < 3 ; i++ ) {
		return_status=ompi_cb_fifo_get_slot (&fifo);
		if( OMPI_CB_ERROR != return_status ) {
	   		test_failure(" ompi_cb_fifo_get_slot :: over-fill queue\n");
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
		return_status=ompi_cb_fifo_write_to_slot(i,(void *)(i+5),&fifo);
		if( OMPI_CB_ERROR == return_status ) {
	   		test_failure(" ompi_cb_fifo_write_to_slot \n");
            error_cnt++;
		}
	}
    if( 0 == error_cnt ) {
        test_success();
    }

	/* pop items off the queue */
    error_cnt=0;
	for( i=0 ; i < ompi_cb_fifo_size(&fifo); i++ ) {
		ptr=ompi_cb_fifo_read_from_tail(&fifo);
		if( (void *)(i+5) != ptr ) {
	   		test_failure(" ompi_cb_fifo_read_from_tail\n");
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
        test_failure(" ompi_cv_fifo_init \n");
    }

    /* finalize result tracking */
    return test_finalize();
}
