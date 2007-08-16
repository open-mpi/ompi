/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "ompi/constants.h"
#include "orte/util/proc_info.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/bml/bml.h"
#include "orte/mca/ns/ns_types.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_ob1.h"
#include "pml_ob1_rdma.h"

/*
 * Check to see if memory is registered or can be registered. Build a 
 * set of registrations on the request.
 */

size_t mca_pml_ob1_rdma_btls(
    mca_bml_base_endpoint_t* bml_endpoint,
    unsigned char* base,
    size_t size,
    mca_pml_ob1_rdma_btl_t* rdma_btls)
{
    size_t num_btls = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_rdma);
    size_t num_btls_used = 0;
    size_t n;

    /* shortcut when there are no rdma capable btls */
    if(num_btls == 0) {
        return 0;
    }

    /* check to see if memory is registered */        
    for(n = 0; n < num_btls && num_btls_used < MCA_PML_OB1_MAX_RDMA_PER_REQUEST;
            n++) {
        mca_bml_base_btl_t* bml_btl =
            mca_bml_base_btl_array_get_index(&bml_endpoint->btl_rdma,
                    (bml_endpoint->btl_rdma_index + n) % num_btls); 
        mca_mpool_base_registration_t* reg = NULL;
        mca_mpool_base_module_t *btl_mpool = bml_btl->btl_mpool;

        /* btl is rdma capable and registration is not required */
        if(NULL == btl_mpool) {
            reg = NULL;
        } else {
            if(!mca_pml_ob1.leave_pinned) {
                /* look through existing registrations */
                btl_mpool->mpool_find(btl_mpool, base, size, &reg);
            } else {
                /* register the memory */
                btl_mpool->mpool_register(btl_mpool, base, size, 0, &reg);
            }

            if(NULL == reg)
                bml_btl = NULL; /* skip it */
        }

        if(bml_btl != NULL) {
            rdma_btls[num_btls_used].bml_btl = bml_btl;
            rdma_btls[num_btls_used].btl_reg = reg;
            num_btls_used++;
        }
    }
    bml_endpoint->btl_rdma_index = (bml_endpoint->btl_rdma_index + 1) % num_btls;
    return num_btls_used;
}
