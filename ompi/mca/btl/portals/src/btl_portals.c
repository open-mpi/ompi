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

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#include "include/constants.h"
#include "opal/util/output.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "ompi/datatype/convertor.h"

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
        mca_btl_portals_get
    },
};



int
mca_btl_portals_add_procs(struct mca_btl_base_module_t* btl_base,
                          size_t nprocs, struct ompi_proc_t **procs,
                          struct mca_btl_base_endpoint_t** peers,
                          ompi_bitmap_t* reachable)
{
    struct mca_btl_portals_module_t *btl = 
        (struct mca_btl_portals_module_t*) btl_base;
    int ret;
    struct ompi_proc_t *curr_proc = NULL;
    ptl_process_id_t *portals_procs = NULL;
    size_t i;
    unsigned long distance;
    bool need_recv_setup = false;

    /* make sure our environment is fully initialized.  At end of this
       call, we have a working network handle on our module and
       portals_procs will have the portals process identifier for each
       proc (ordered, in theory) */
    ret = mca_btl_portals_add_procs_compat(btl, nprocs, procs, 
                                           &portals_procs);
    if (OMPI_SUCCESS != ret) return ret;

    OPAL_THREAD_LOCK(&btl->portals_lock);

    if (0 == opal_list_get_size(&btl->portals_endpoint_list)) {
        need_recv_setup = true;
    }

    /* loop through all procs, setting our reachable flag */
    for (i= 0; i < nprocs ; ++i) {
        curr_proc = procs[i];

        peers[i] = OBJ_NEW(mca_btl_portals_endpoint_t);
        peers[i]->endpoint_btl = btl;
        peers[i]->endpoint_proc = curr_proc;
        peers[i]->endpoint_ptl_id = portals_procs[i];

        opal_list_append(&btl->portals_endpoint_list,
                         (opal_list_item_t*) peers[i]);

        /* make sure we can reach the process - this is supposed to be
           a cheap-ish operation */
        ret = PtlNIDist(btl->portals_ni_h,
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

    if (need_recv_setup) {
        /* create eqs */
        int i;
        for (i = 0 ; i < MCA_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQAlloc(btl->portals_ni_h,
                                     btl->portals_eq_sizes[i],
                                     PTL_EQ_HANDLER_NONE,
                                     &(btl->portals_eq_handles[i]));
            if (PTL_OK != ptl_ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error creating EQ %d: %d", i, ptl_ret);
                OPAL_THREAD_UNLOCK(&btl->portals_lock);
                /* BWB - better error code? */
                return OMPI_ERROR;
            }
        }

        ret = mca_btl_portals_recv_enable(btl);
    } else {
        ret = OMPI_SUCCESS;
    }

    OPAL_THREAD_UNLOCK(&btl->portals_lock);

    return ret;
}


int
mca_btl_portals_del_procs(struct mca_btl_base_module_t *btl_base,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_btl_base_endpoint_t **peers)
{
    mca_btl_portals_module_t *btl =
        (mca_btl_portals_module_t*) btl_base;
    size_t i = 0;
    int ret = OMPI_SUCCESS;
    bool need_recv_shutdown = false;

    opal_output_verbose(100, mca_btl_portals_component.portals_output,
                        "del_procs called for %ld procs", (long) nprocs);

    OPAL_THREAD_LOCK(&btl->portals_lock);

    for (i = 0 ; i < nprocs ; ++i) {
        opal_list_remove_item(&btl->portals_endpoint_list,
                              (opal_list_item_t*) peers[i]);
        OBJ_RELEASE(peers[i]);
    }

    if (0 == opal_list_get_size(&btl->portals_endpoint_list)) {
        need_recv_shutdown = true;
    }

    if (need_recv_shutdown) {
        int i;

        ret = mca_btl_portals_recv_disable(btl);

        /* destroy eqs */
        for (i = 0 ; i < MCA_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQFree(btl->portals_eq_handles[i]);
            if (PTL_OK != ptl_ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error freeing EQ %d: %d", i, ptl_ret);
            }
        }

    } else {
        ret = OMPI_SUCCESS;
    }

    OPAL_THREAD_UNLOCK(&btl->portals_lock);

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

    return &frag->base;
}


int
mca_btl_portals_free(struct mca_btl_base_module_t* btl_base, 
                      mca_btl_base_descriptor_t* des) 
{
    mca_btl_portals_module_t* btl = (mca_btl_portals_module_t*) btl_base; 
    mca_btl_portals_frag_t* frag = (mca_btl_portals_frag_t*) des; 

    if (frag->size == 0) {
        MCA_BTL_PORTALS_FRAG_RETURN_USER(&btl->super, frag); 
    } else if (frag->size == btl->super.btl_eager_limit){ 
        MCA_BTL_PORTALS_FRAG_RETURN_EAGER(&btl->super, frag); 
    } else if (frag->size == btl->super.btl_max_send_size) {
        MCA_BTL_PORTALS_FRAG_RETURN_MAX(&btl->super, frag); 
    }  else {
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS; 
}


/* BWB - fix me - this needs to do RDMA when we get there... */
mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_src(struct mca_btl_base_module_t* btl,
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size)
{
    mca_btl_portals_frag_t* frag;
    size_t max_data = *size;
    struct iovec iov;
    uint32_t iov_count = 1;
    int32_t free_after;
    int rc;

    if (max_data+reserve <= btl->btl_eager_limit) {
        /*
         * if we aren't pinning the data and the requested size is less
         * than the eager limit pack into a fragment from the eager pool
         */
        MCA_BTL_PORTALS_FRAG_ALLOC_EAGER(btl, frag, rc);
        if (NULL == frag) {
            return NULL;
        }

        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, 
                                 &max_data, &free_after);
        *size  = max_data;
        if (rc < 0) {
            MCA_BTL_PORTALS_FRAG_RETURN_EAGER(btl, frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    } else {
        /* 
         * otherwise pack as much data as we can into a fragment
         * that is the max send size.
         */
        MCA_BTL_PORTALS_FRAG_ALLOC_MAX(btl, frag, rc);
        if (NULL == frag) {
            return NULL;
        }
        if (max_data + reserve > btl->btl_max_send_size){
            max_data = btl->btl_max_send_size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, 
                                 &max_data, &free_after);
        *size  = max_data;
        if ( rc < 0 ) {
            MCA_BTL_PORTALS_FRAG_RETURN_MAX(btl, frag);
            return NULL;
        }
        frag->segment.seg_len = max_data + reserve;
    }

    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;

    return &frag->base;
}


int
mca_btl_portals_finalize(struct mca_btl_base_module_t *btl_base)
{
    struct mca_btl_portals_module_t *btl =
        (struct mca_btl_portals_module_t *) btl_base;
    int ret, i;
    opal_list_item_t *item;

    /* finalize all communication */
    while (btl->portals_outstanding_sends > 0) {
        mca_btl_portals_component_progress();
    }

    if (0 != opal_list_get_size(&(btl->portals_queued_sends))) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Warning: there were %d queued sends not sent",
                    opal_list_get_size(&(btl->portals_queued_sends)));
    }

    OPAL_THREAD_LOCK(&btl->portals_lock);

    if (0 != opal_list_get_size(&btl->portals_endpoint_list)) {
        OPAL_THREAD_LOCK(&btl->portals_lock);
        while (NULL !=
               (item = opal_list_remove_first(&btl->portals_endpoint_list))) {
            OBJ_RELEASE(item);
        }

        /* only do this if there was something in the endpoint list.
           otherwise, it has alredy been done. */

        /* shut down recv queues */
        ret = mca_btl_portals_recv_disable(btl);

        /* destroy eqs */
        for (i = 0 ; i < MCA_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQFree(btl->portals_eq_handles[i]);
            if (PTL_OK != ptl_ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error freeing EQ %d: %d", i, ptl_ret);
            }
        }
    }

    OBJ_DESTRUCT(&btl->portals_endpoint_list);
    OBJ_DESTRUCT(&btl->portals_recv_chunks);
    OBJ_DESTRUCT(&btl->portals_queued_sends);

    OPAL_THREAD_UNLOCK(&btl->portals_lock);

    if (PTL_INVALID_HANDLE != btl->portals_ni_h) {
        ret = PtlNIFini(btl->portals_ni_h);
        if (PTL_OK != ret) {
            opal_output_verbose(20, mca_btl_portals_component.portals_output,
                                "PtlNIFini returned %d", ret);
            return OMPI_ERROR;
        }
    }

    OBJ_DESTRUCT(&btl->portals_lock);

    opal_output_verbose(20, mca_btl_portals_component.portals_output,
                        "successfully finalized module");

    return OMPI_SUCCESS;
}
