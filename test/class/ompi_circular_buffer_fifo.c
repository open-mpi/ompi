
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
    int size_of_fifo,lazy_free,return_status;

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


    /* finalize result tracking */
    return test_finalize();
}
