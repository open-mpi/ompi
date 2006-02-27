/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "ompi_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#include "ompi/constants.h"
#include "opal/util/output.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/datatype/convertor.h"
#include "ompi/datatype/datatype.h"

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
    int ret;
    struct ompi_proc_t *curr_proc = NULL;
    ptl_process_id_t *portals_procs = NULL;
    size_t i;
    unsigned long distance;
    bool need_activate = false;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    opal_output_verbose(50, mca_btl_portals_component.portals_output,
                        "Adding %d procs (%d)", nprocs,
                        mca_btl_portals_module.portals_num_procs);

    /* make sure our environment is fully initialized.  At end of this
       call, we have a working network handle on our module and
       portals_procs will have the portals process identifier for each
       proc (ordered, in theory) */
    ret = mca_btl_portals_add_procs_compat(&mca_btl_portals_module,
                                           nprocs, procs, 
                                           &portals_procs);
    if (OMPI_SUCCESS != ret) return ret;

    if (0 == mca_btl_portals_module.portals_num_procs) {
        need_activate = true;
    }

    /* loop through all procs, setting our reachable flag */
    for (i= 0; i < nprocs ; ++i) {
        curr_proc = procs[i];

        peers[i] = malloc(sizeof(mca_btl_base_endpoint_t));
        if (NULL == peers[i]) return OMPI_ERROR;
        *((mca_btl_base_endpoint_t*) peers[i]) = portals_procs[i];

        /* make sure we can reach the process - this is supposed to be
           a cheap-ish operation */
        ret = PtlNIDist(mca_btl_portals_module.portals_ni_h,
                        portals_procs[i],
                        &distance);
        if (ret != PTL_OK) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "Could not find distance to process %d", i);
            continue;
        }

        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_num_procs, 1);
        /* and here we can reach */
        ompi_bitmap_set_bit(reachable, i);
    }

    if (NULL != portals_procs) free(portals_procs);

    if (need_activate && mca_btl_portals_module.portals_num_procs > 0) {
        /* create eqs */
        int i;

        opal_output_verbose(50, mca_btl_portals_component.portals_output,
                            "Enabling progress");

        for (i = 0 ; i < OMPI_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQAlloc(mca_btl_portals_module.portals_ni_h,
                                     mca_btl_portals_module.portals_eq_sizes[i],
                                     PTL_EQ_HANDLER_NONE,
                                     &(mca_btl_portals_module.portals_eq_handles[i]));
            if (PTL_OK != ptl_ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error creating EQ %d: %d", i, ptl_ret);
                /* BWB - better error code? */
                return OMPI_ERROR;
            }
        }

        ret = mca_btl_portals_recv_enable(&mca_btl_portals_module);

        /* fill in send memory descriptor */
        mca_btl_portals_module.md_send.start = NULL;
        mca_btl_portals_module.md_send.length = 0;
        mca_btl_portals_module.md_send.threshold = 2; /* send and ack */
        mca_btl_portals_module.md_send.max_size = 0;
        mca_btl_portals_module.md_send.options = PTL_MD_EVENT_START_DISABLE;
        mca_btl_portals_module.md_send.user_ptr = NULL;
        mca_btl_portals_module.md_send.eq_handle = 
            mca_btl_portals_module.portals_eq_handles[OMPI_BTL_PORTALS_EQ_SEND];
    } else {
        ret = OMPI_SUCCESS;
    }

    opal_output_verbose(50, mca_btl_portals_component.portals_output,
                        "count: %d", mca_btl_portals_module.portals_num_procs);

    return ret;
}


int
mca_btl_portals_del_procs(struct mca_btl_base_module_t *btl_base,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_btl_base_endpoint_t **peers)
{
    size_t i = 0;
    int ret = OMPI_SUCCESS;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    opal_output_verbose(50, mca_btl_portals_component.portals_output,
                        "Removing %d procs (%d)", nprocs,
                        mca_btl_portals_module.portals_num_procs);

    for (i = 0 ; i < nprocs ; ++i) {
        free(peers[i]);
        OPAL_THREAD_ADD32(&mca_btl_portals_module.portals_num_procs, -1);
    }

    if (0 == mca_btl_portals_module.portals_num_procs) {
        int i;

        opal_output_verbose(50, mca_btl_portals_component.portals_output,
                            "Disabling progress");

        ret = mca_btl_portals_recv_disable(&mca_btl_portals_module);

        /* destroy eqs */
        for (i = 0 ; i < OMPI_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQFree(mca_btl_portals_module.portals_eq_handles[i]);
            if (PTL_OK != ptl_ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error freeing EQ %d: %d", i, ptl_ret);
            }
        }

    } else {
        ret = OMPI_SUCCESS;
    }

    return ret;
}


int
mca_btl_portals_register(struct mca_btl_base_module_t* btl_base, 
                          mca_btl_base_tag_t tag, 
                          mca_btl_base_module_recv_cb_fn_t cbfunc, 
                          void* cbdata)
{
    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    mca_btl_portals_module.portals_reg[tag].cbfunc = cbfunc; 
    mca_btl_portals_module.portals_reg[tag].cbdata = cbdata; 

    return OMPI_SUCCESS;
}


mca_btl_base_descriptor_t*
mca_btl_portals_alloc(struct mca_btl_base_module_t* btl_base,
                       size_t size)
{
    mca_btl_portals_frag_t* frag;
    int rc;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);
    
    if (size <= mca_btl_portals_module.super.btl_eager_limit) { 
        OMPI_BTL_PORTALS_FRAG_ALLOC_EAGER(&mca_btl_portals_module, frag, rc); 
        frag->segments[0].seg_len = 
            size <= mca_btl_portals_module.super.btl_eager_limit ? 
            size : mca_btl_portals_module.super.btl_eager_limit ; 
    } else { 
        OMPI_BTL_PORTALS_FRAG_ALLOC_MAX(&mca_btl_portals_module, frag, rc); 
        frag->segments[0].seg_len = 
            size <= mca_btl_portals_module.super.btl_max_send_size ? 
            size : mca_btl_portals_module.super.btl_max_send_size ; 
    }
    
    frag->base.des_src_cnt = 1;
    frag->base.des_flags = 0; 

    /* can't setup off an alloc right now - we don't know how much the
       caller will actually use */
    frag->md_h = PTL_INVALID_HANDLE;

    return &frag->base;
}


int
mca_btl_portals_free(struct mca_btl_base_module_t* btl_base, 
                      mca_btl_base_descriptor_t* des) 
{
    mca_btl_portals_frag_t* frag = (mca_btl_portals_frag_t*) des; 

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    if (frag->md_h != PTL_INVALID_HANDLE) {
        OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                             "rdma frag free frag 0x%x, callback 0x%x, bits %lld",
                             frag, frag->base.des_cbfunc, frag->segments[0].seg_key.key64));
        PtlMDUnlink(frag->md_h);
    }

    if (frag->size == 0) {
        OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag); 
    } else if (frag->size == mca_btl_portals_module.super.btl_eager_limit){ 
        OMPI_BTL_PORTALS_FRAG_RETURN_EAGER(&mca_btl_portals_module.super, frag); 
    } else if (frag->size == mca_btl_portals_module.super.btl_max_send_size) {
        OMPI_BTL_PORTALS_FRAG_RETURN_MAX(&mca_btl_portals_module.super, frag); 
    }  else {
        return OMPI_ERR_BAD_PARAM;
    }

    return OMPI_SUCCESS; 
}


mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_src(struct mca_btl_base_module_t* btl_base,
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
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    if (0 != ompi_convertor_need_buffers(convertor)) {
        /* if we need to use buffers to pack the data, grab either an
           eager or (if we need more space) max buffer, pack the data
           into the first segment, and return */
        if (max_data+reserve <= mca_btl_portals_module.super.btl_eager_limit) {
            /*
             * if we can't send out of the buffer directly and the
             * requested size is less than the eager limit, pack into a
             * fragment from the eager pool
             */
            OMPI_BTL_PORTALS_FRAG_ALLOC_EAGER(&mca_btl_portals_module, frag, ret);
            if (NULL == frag) {
                return NULL;
            }

            iov.iov_len = max_data;
            iov.iov_base = (unsigned char*) frag->segments[0].seg_addr.pval + reserve;
            ret = ompi_convertor_pack(convertor, &iov, &iov_count, 
                                      &max_data, &free_after);
            *size  = max_data;
            if (ret < 0) {
                OMPI_BTL_PORTALS_FRAG_RETURN_EAGER(&mca_btl_portals_module, frag);
                return NULL;
            }
            frag->segments[0].seg_len = max_data + reserve;
            frag->base.des_src_cnt = 1;

        } else {
            /* 
             * otherwise pack as much data as we can into a fragment
             * that is the max send size.
             */
            OMPI_BTL_PORTALS_FRAG_ALLOC_MAX(&mca_btl_portals_module, frag, ret);
            if (NULL == frag) {
                return NULL;
            }
            if (max_data + reserve > mca_btl_portals_module.super.btl_max_send_size){
                max_data = mca_btl_portals_module.super.btl_max_send_size - reserve;
            }
            iov.iov_len = max_data;
            iov.iov_base = (unsigned char*) frag->segments[0].seg_addr.pval + reserve;
            ret = ompi_convertor_pack(convertor, &iov, &iov_count, 
                                      &max_data, &free_after);
            *size  = max_data;
            if ( ret < 0 ) {
                OMPI_BTL_PORTALS_FRAG_RETURN_MAX(&mca_btl_portals_module, frag);
                return NULL;
            }
            frag->segments[0].seg_len = max_data + reserve;
            frag->base.des_src_cnt = 1;
        }

        /* clearly a send - delay setup of memory descriptor until send */
        frag->md_h = PTL_INVALID_HANDLE;
        

    } else {
        /* no need to pack - we can send directly out of the user's
           buffer.  If we have reserve space, use an eager fragment
           and give the caller the eager space as reserve.  If we have
           no reserve space needs, use a user frag */
        if (0 == reserve) {
            ptl_md_t md;
            ptl_handle_me_t me_h;

            /* user frags are always setup to use only one fragment */
            OMPI_BTL_PORTALS_FRAG_ALLOC_USER(&mca_btl_portals_module.super, frag, ret);
            if(NULL == frag){
                return NULL;
            }
            iov.iov_len = max_data;
            iov.iov_base = NULL;

            ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, 
                                &free_after);

            frag->segments[0].seg_len = max_data;
            frag->segments[0].seg_addr.pval = iov.iov_base;
            frag->segments[0].seg_key.key64 = OPAL_THREAD_ADD64(&(mca_btl_portals_module.portals_rdma_key), 1);
            frag->base.des_src_cnt = 1;

            /* either a put or get.  figure out which later */

            OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                                 "rdma src posted for frag 0x%x, callback 0x%x, bits %lld",
                                 frag, frag->base.des_cbfunc, frag->segments[0].seg_key.key64));

            /* create a match entry */
            ret = PtlMEAttach(mca_btl_portals_module.portals_ni_h,
                              OMPI_BTL_PORTALS_RDMA_TABLE_ID,
                              *((mca_btl_base_endpoint_t*) peer),
                              frag->segments[0].seg_key.key64, /* match */
                              0, /* ignore */
                              PTL_UNLINK,
                              PTL_INS_AFTER,
                              &me_h);
            if (PTL_OK != ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error creating rdma src ME: %d", ret);
                OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag);
                return NULL;
            }

            /* setup the memory descriptor.  RDMA should never need to be
               retransmitted, so we set the threshold for the event it will
               receive (PUT/GET START and END).  No need to track the unlinks
               later :) */
            md.start = frag->segments[0].seg_addr.pval;
            md.length = frag->segments[0].seg_len;
            md.threshold = PTL_MD_THRESH_INF;
            md.max_size = 0;
            md.options = PTL_MD_OP_PUT | PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE;
            md.user_ptr = frag; /* keep a pointer to ourselves */
            md.eq_handle = mca_btl_portals_module.portals_eq_handles[OMPI_BTL_PORTALS_EQ];

            ret = PtlMDAttach(me_h, 
                              md,
                              PTL_UNLINK,
                              &(frag->md_h));
            if (PTL_OK != ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error creating rdma src MD: %d", ret);
                PtlMEUnlink(me_h);
                OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag);
                return NULL;
            }

        } else {
            OMPI_BTL_PORTALS_FRAG_ALLOC_EAGER(&mca_btl_portals_module, frag, ret);
            if (NULL == frag) {
                return NULL;
            }

            iov.iov_len = max_data;
            iov.iov_base = NULL;
            ret = ompi_convertor_pack(convertor, &iov, &iov_count, 
                                      &max_data, &free_after);

            *size  = max_data;
            if (ret < 0) {
                OMPI_BTL_PORTALS_FRAG_RETURN_EAGER(&mca_btl_portals_module, frag);
                return NULL;
            }

            frag->segments[0].seg_len = reserve;
            frag->segments[1].seg_addr.pval = iov.iov_base;
            frag->segments[1].seg_len = max_data;
            frag->base.des_src_cnt = 2;

            frag->iov[0].iov_base = frag->segments[0].seg_addr.pval;
            frag->iov[0].iov_len = frag->segments[0].seg_len;
            frag->iov[1].iov_base = frag->segments[1].seg_addr.pval;
            frag->iov[1].iov_len = frag->segments[1].seg_len;

            /* clearly a send - delay setup of memory descriptor until send */
            frag->md_h = PTL_INVALID_HANDLE;
        }
    }

    frag->base.des_src = frag->segments;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;

    return &frag->base;
}


mca_btl_base_descriptor_t* 
mca_btl_portals_prepare_dst(struct mca_btl_base_module_t* btl_base, 
                            struct mca_btl_base_endpoint_t* peer,
                            mca_mpool_base_registration_t* registration, 
                            struct ompi_convertor_t* convertor,
                            size_t reserve,
                            size_t* size)
{
    mca_btl_portals_frag_t* frag;
    ptl_md_t md;
    ptl_handle_me_t me_h;
    int ret;
    long lb;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    OMPI_BTL_PORTALS_FRAG_ALLOC_USER(&mca_btl_portals_module.super, frag, ret);
    if(NULL == frag) {
        return NULL;
    }

    ompi_ddt_type_lb(convertor->pDesc, &lb);
    frag->segments[0].seg_len = *size;
    frag->segments[0].seg_addr.pval = convertor->pBaseBuf + lb + convertor->bConverted;
    frag->segments[0].seg_key.key64 = OPAL_THREAD_ADD64(&(mca_btl_portals_module.portals_rdma_key), 1);

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = frag->segments;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;
    frag->type = mca_btl_portals_frag_type_rdma;

    OPAL_OUTPUT_VERBOSE((90, mca_btl_portals_component.portals_output,
                         "rdma dest posted for frag 0x%x, callback 0x%x, bits %lld",
                         frag, frag->base.des_cbfunc, frag->segments[0].seg_key.key64));

    /* create a match entry */
    ret = PtlMEAttach(mca_btl_portals_module.portals_ni_h,
                      OMPI_BTL_PORTALS_RDMA_TABLE_ID,
                      *((mca_btl_base_endpoint_t*) peer),
                      frag->segments[0].seg_key.key64, /* match */
                      0, /* ignore */
                      PTL_UNLINK,
                      PTL_INS_AFTER,
                      &me_h);
    if (PTL_OK != ret) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Error creating rdma dest ME: %d", ret);
        OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag);
        return NULL;
    }

    /* setup the memory descriptor.  RDMA should never need to be
       retransmitted, so we set the threshold for the event it will
       receive (PUT/GET START and END).  No need to track the unlinks
       later :) */
    md.start = frag->segments[0].seg_addr.pval;
    md.length = frag->segments[0].seg_len;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = 0;
    md.options = PTL_MD_OP_PUT | PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = frag; /* keep a pointer to ourselves */
    md.eq_handle = mca_btl_portals_module.portals_eq_handles[OMPI_BTL_PORTALS_EQ];

    ret = PtlMDAttach(me_h, 
                      md,
                      PTL_UNLINK,
                      &(frag->md_h));
    if (PTL_OK != ret) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Error creating rdma dest MD: %d", ret);
        PtlMEUnlink(me_h);
        OMPI_BTL_PORTALS_FRAG_RETURN_USER(&mca_btl_portals_module.super, frag);
        return NULL;
    }

    return &frag->base;
}


int
mca_btl_portals_finalize(struct mca_btl_base_module_t *btl_base)
{
    int ret;

    assert(&mca_btl_portals_module == (mca_btl_portals_module_t*) btl_base);

    /* finalize all communication */
    while (mca_btl_portals_module.portals_outstanding_sends > 0) {
        mca_btl_portals_component_progress();
    }

    if (0 != opal_list_get_size(&(mca_btl_portals_module.portals_queued_sends))) {
        opal_output(mca_btl_portals_component.portals_output,
                    "Warning: there were %d queued sends not sent",
                    opal_list_get_size(&(mca_btl_portals_module.portals_queued_sends)));
    }

    if (mca_btl_portals_module.portals_num_procs != 0) {
        int i;

        ret = mca_btl_portals_recv_disable(&mca_btl_portals_module);

        /* destroy eqs */
        for (i = 0 ; i < OMPI_BTL_PORTALS_EQ_SIZE ; ++i) {
            int ptl_ret = PtlEQFree(mca_btl_portals_module.portals_eq_handles[i]);
            if (PTL_OK != ptl_ret) {
                opal_output(mca_btl_portals_component.portals_output,
                            "Error freeing EQ %d: %d", i, ptl_ret);
            }
        }

    } 

    OBJ_DESTRUCT(&mca_btl_portals_module.portals_recv_blocks);
    OBJ_DESTRUCT(&mca_btl_portals_module.portals_queued_sends);

    if (PTL_INVALID_HANDLE != mca_btl_portals_module.portals_ni_h) {
        ret = PtlNIFini(mca_btl_portals_module.portals_ni_h);
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
