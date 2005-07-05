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
#include "portals_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#include "include/constants.h"
#include "opal/util/output.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"

#include "btl_portals.h"
#include "btl_portals_compat.h"
#include "btl_portals_endpoint.h"
#include "btl_portals_recv.h"
#include "btl_portals_frag.h"

mca_btl_portals_module_t mca_btl_portals_module = {
    {
        &mca_btl_portals_component.super,

        /* NOTE: All these default values are set in
           component_open() */

        0,   /* max size of first frag */
        0,   /* min send size */
        0,   /* max send size */
        0,   /* min rdma size */
        0,   /* max rdma size */
        0,   /* exclusivity - higher than sm, lower than self */
        0,   /* latency */
        0,   /* bandwidth */
        0,   /* btl flags */

        mca_btl_portals_add_procs,
        mca_btl_portals_del_procs,
        mca_btl_portals_register,
        mca_btl_portals_finalize,

        mca_btl_portals_alloc,
        mca_btl_portals_free,
        mca_btl_portals_prepare_src,
        mca_btl_portals_prepare_dst,
        mca_btl_portals_send,
        mca_btl_portals_put,
        mca_btl_portals_get,
    },
};



int
mca_btl_portals_add_procs(struct mca_btl_base_module_t* btl,
                          size_t nprocs, struct ompi_proc_t **procs,
                          struct mca_btl_base_endpoint_t** peers,
                          ompi_bitmap_t* reachable)
{
    int ret;
    struct ompi_proc_t *curr_proc = NULL;
    ptl_process_id_t *portals_procs = NULL;
    size_t i;
    unsigned long distance;
    struct mca_btl_portals_module_t *ptl_btl = 
        (struct mca_btl_portals_module_t*) btl;
    bool need_recv_setup = false;


    /* make sure our environment is fully initialized.  At end of this
       call, we have a working network handle on our module and
       portals_procs will have the portals process identifier for each
       proc (ordered, in theory) */
    ret = mca_btl_portals_add_procs_compat(ptl_btl, nprocs, procs, 
                                           &portals_procs);
    if (OMPI_SUCCESS != ret) return ret;

    OPAL_THREAD_LOCK(&ptl_btl->portals_lock);

    if (0 == opal_list_get_size(&ptl_btl->portals_endpoint_list)) {
        need_recv_setup = true;
    }

    /* loop through all procs, setting our reachable flag */
    for (i= 0; i < nprocs ; ++i) {
        curr_proc = procs[i];

        peers[i] = OBJ_NEW(mca_btl_portals_endpoint_t);
        peers[i]->endpoint_btl = ptl_btl;
        peers[i]->endpoint_proc = curr_proc;
        peers[i]->endpoint_ptl_id = portals_procs[i];

        opal_list_append(&ptl_btl->portals_endpoint_list,
                         (opal_list_item_t*) peers[i]);

        /* make sure we can reach the process - this is supposed to be
           a cheap-ish operation */
        ret = PtlNIDist(ptl_btl->portals_ni_h,
                        portals_procs[i],
                        &distance);
        if (ret != PTL_OK) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "Could not find distance to process %d", i);
            continue;
        }

        /* and here we can reach */
        ompi_bitmap_set_bit(reachable, i);
    }

    if (NULL != portals_procs) free(portals_procs);

    OPAL_THREAD_UNLOCK(&ptl_btl->portals_lock);

    if (need_recv_setup) {
        ret = mca_btl_portals_recv_enable(ptl_btl);
    } else {
        ret = OMPI_SUCCESS;
    }

    return ret;
}


int
mca_btl_portals_del_procs(struct mca_btl_base_module_t *btl,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_btl_base_endpoint_t **peers)
{
    mca_btl_portals_module_t *ptl_btl =
        (mca_btl_portals_module_t*) btl;
    size_t i = 0;
    int ret = OMPI_SUCCESS;
    bool need_recv_shutdown = false;

    OPAL_THREAD_LOCK(&ptl_btl->portals_lock);

    for (i = 0 ; i < nprocs ; ++i) {
        opal_list_remove_item(&ptl_btl->portals_endpoint_list,
                              (opal_list_item_t*) peers[i]);
        OBJ_RELEASE(peers[i]);
    }

    if (0 == opal_list_get_size(&ptl_btl->portals_endpoint_list)) {
        need_recv_shutdown = true;
    }

    OPAL_THREAD_UNLOCK(&ptl_btl->portals_lock);

    if (need_recv_shutdown) {
        ret = mca_btl_portals_recv_disable(ptl_btl);
    } else {
        ret = OMPI_SUCCESS;
    }

    return ret;
}


int
mca_btl_portals_register(struct mca_btl_base_module_t* btl, 
                          mca_btl_base_tag_t tag, 
                          mca_btl_base_module_recv_cb_fn_t cbfunc, 
                          void* cbdata)
{
    mca_btl_portals_module_t* portals_btl = (mca_btl_portals_module_t*) btl; 
    portals_btl->portals_reg[tag].cbfunc = cbfunc; 
    portals_btl->portals_reg[tag].cbdata = cbdata; 

    return OMPI_SUCCESS;
}


mca_btl_base_descriptor_t*
mca_btl_portals_alloc(struct mca_btl_base_module_t* btl,
                       size_t size)
{
    mca_btl_portals_module_t* portals_btl = (mca_btl_portals_module_t*) btl; 
    mca_btl_portals_frag_t* frag;
    int rc;
    
    if (size <= btl->btl_eager_limit) { 
        MCA_BTL_PORTALS_FRAG_ALLOC_EAGER(portals_btl, frag, rc); 
        frag->segment.seg_len = 
            size <= btl->btl_eager_limit ? 
            size : btl->btl_eager_limit ; 
    } else { 
        MCA_BTL_PORTALS_FRAG_ALLOC_MAX(portals_btl, frag, rc); 
        frag->segment.seg_len = 
            size <= btl->btl_max_send_size ? 
            size : btl->btl_max_send_size ; 
    }
    
    frag->base.des_flags = 0; 

    return (mca_btl_base_descriptor_t*) frag;
}


int
mca_btl_portals_free(struct mca_btl_base_module_t* btl, 
                      mca_btl_base_descriptor_t* des) 
{
    mca_btl_portals_frag_t* frag = (mca_btl_portals_frag_t*) des; 

    if (frag->size == 0) {
        MCA_BTL_PORTALS_FRAG_RETURN_USER(btl, frag); 
    } else if (frag->size == btl->btl_eager_limit){ 
        MCA_BTL_PORTALS_FRAG_RETURN_EAGER(btl, frag); 
    } else if (frag->size == btl->btl_max_send_size) {
        MCA_BTL_PORTALS_FRAG_RETURN_MAX(btl, frag); 
    }  else {
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS; 
}


int
mca_btl_portals_finalize(struct mca_btl_base_module_t *btl_base)
{
    struct mca_btl_portals_module_t *btl =
        (struct mca_btl_portals_module_t *) btl_base;
    int ret;

    if (PTL_INVALID_HANDLE != btl->portals_ni_h) {
        ret = PtlNIFini(btl->portals_ni_h);
        if (PTL_OK != ret) {
            opal_output_verbose(20, mca_btl_portals_component.portals_output,
                                "PtlNIFini returned %d", ret);
            return OMPI_ERROR;
        }
    }
    opal_output_verbose(20, mca_btl_portals_component.portals_output,
                        "successfully finalized module");

    return OMPI_SUCCESS;
}
