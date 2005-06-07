/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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
#include <string.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"

#include "bmi_ib.h"
#include "bmi_ib_frag.h" 
#include "bmi_ib_addr.h"
#include "bmi_ib_proc.h"
#include "bmi_ib_endpoint.h"
#include "bmi_ib_priv.h"


mca_bmi_ib_module_t mca_bmi_ib_module = {
    {
        &mca_bmi_ib_component.super,
        0, /* max size of first fragment */
        0, /* min fragment size */
        0, /* max fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0,  /* TODO this should be PUT bmi flags */
        mca_bmi_ib_add_procs,
        mca_bmi_ib_del_procs,
        mca_bmi_ib_register, 
        mca_bmi_ib_finalize,
        /* we need alloc free, pack */ 
        mca_bmi_ib_alloc, 
        mca_bmi_ib_free, 
        mca_bmi_ib_prepare_src,
        mca_bmi_ib_prepare_dst,
        mca_bmi_ib_send,
        mca_bmi_ib_put,
        NULL /* get */ 
    }
};

int mca_bmi_ib_add_procs(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_bmi_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_bmi_ib_module_t* ib_bmi = (mca_bmi_ib_module_t*)bmi;
    int i, rc;

    for(i = 0; i < nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_bmi_ib_proc_t* ib_proc;
        mca_bmi_base_endpoint_t* ib_peer;

        if(NULL == (ib_proc = mca_bmi_ib_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OMPI_THREAD_LOCK(&ib_proc->proc_lock);

        /* The bmi_proc datastructure is shared by all IB PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the bmi_proc.
         */
        ib_peer = OBJ_NEW(mca_bmi_ib_endpoint_t);
        if(NULL == ib_peer) {
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        ib_peer->endpoint_bmi = ib_bmi;
        rc = mca_bmi_ib_proc_insert(ib_proc, ib_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ib_peer);
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
        peers[i] = ib_peer;
    }

    return OMPI_SUCCESS;
}

int mca_bmi_ib_del_procs(struct mca_bmi_base_module_t* bmi, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_bmi_base_endpoint_t ** peers)
{
    /* Stub */
    D_PRINT("Stub\n");
    return OMPI_SUCCESS;
}

int mca_bmi_ib_register(
                        struct mca_bmi_base_module_t* bmi, 
                        mca_bmi_base_tag_t tag, 
                        mca_bmi_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    /* TODO add register stuff here... */ 
    mca_bmi_ib_module_t* ib_bmi = (mca_bmi_ib_module_t*) bmi; 
    ib_bmi->ib_reg[tag].cbfunc = cbfunc; 
    ib_bmi->ib_reg[tag].cbdata = cbdata; 
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param bmi (IN)      BMI module
 * @param size (IN)     Request segment size.
 */
mca_bmi_base_descriptor_t* mca_bmi_ib_alloc(
    struct mca_bmi_base_module_t* bmi,
    size_t size)
{
    mca_bmi_ib_frag_t* frag;
    int rc;
    /* if(size <= mca_bmi_ib_component.first_fragment_size) { */
/*         MCA_BMI_IB_FRAG_ALLOC1(frag,rc); */
/*     } else { */
       
/*     } */
    MCA_BMI_IB_FRAG_ALLOC1(bmi, frag,rc); 
    frag->segment.seg_len = size;  
    return (mca_bmi_base_descriptor_t*)frag;
}

int mca_bmi_ib_free(
                           struct mca_bmi_base_module_t* bmi, 
                           mca_bmi_base_descriptor_t* des) 
{
    mca_bmi_ib_frag_t* frag = (mca_bmi_ib_frag_t*)des; 
    MCA_BMI_IB_FRAG_RETURN1(bmi, frag);     
    return frag->rc; 
}

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
mca_bmi_base_descriptor_t* mca_bmi_ib_prepare_src(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* peer,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    

}

/**
 * Pack data
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
mca_bmi_base_descriptor_t* mca_bmi_ib_prepare_dst(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* peer,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    return NULL;
}

int mca_bmi_ib_finalize(struct mca_bmi_base_module_t* bmi)
{
    return OMPI_SUCCESS;
}

/*
 *  Initiate a send. If this is the first fragment, use the fragment
 *  descriptor allocated with the send requests, otherwise obtain
 *  one from the free list. Initialize the fragment and foward
 *  on to the peer.
 */

int mca_bmi_ib_send( 
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* descriptor, 
    mca_bmi_base_tag_t tag)
   
{
    
    mca_bmi_ib_frag_t* frag = (mca_bmi_ib_frag_t*)descriptor; 
    frag->endpoint = endpoint; 
        
    frag->hdr->tag = tag; 
    frag->type = MCA_BMI_IB_FRAG_SEND; 
    frag->rc = mca_bmi_ib_endpoint_send(endpoint, frag);
           
    return frag->rc;
}

/*
 * RDMA local buffer to remote buffer address.
 */

int mca_bmi_ib_put( mca_bmi_base_module_t* bmi,
                    mca_bmi_base_endpoint_t* bmi_peer,
                    mca_bmi_base_descriptor_t* descriptor)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


