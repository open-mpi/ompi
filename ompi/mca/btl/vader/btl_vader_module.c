/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_vader.h"
#include "btl_vader_endpoint.h"
#include "btl_vader_fifo.h"
#include "btl_vader_fbox.h"

static int vader_del_procs (struct mca_btl_base_module_t *btl,
                            size_t nprocs, struct ompi_proc_t **procs,
                            struct mca_btl_base_endpoint_t **peers);

static int vader_register_error_cb (struct mca_btl_base_module_t* btl,
                                    mca_btl_base_module_error_cb_fn_t cbfunc);

static int vader_finalize (struct mca_btl_base_module_t* btl);

static int vader_free (struct mca_btl_base_module_t* btl, mca_btl_base_descriptor_t* des);

static struct mca_btl_base_descriptor_t *vader_prepare_src (
                                                            struct mca_btl_base_module_t *btl,
                                                            struct mca_btl_base_endpoint_t *endpoint,
                                                            mca_mpool_base_registration_t *registration,
                                                            struct opal_convertor_t *convertor,
                                                            uint8_t order,
                                                            size_t reserve,
                                                            size_t *size,
                                                            uint32_t flags
                                                            );

static struct mca_btl_base_descriptor_t *vader_prepare_dst (
                                                            struct mca_btl_base_module_t *btl,
                                                            struct mca_btl_base_endpoint_t *endpoint,
                                                            struct mca_mpool_base_registration_t *registration,
                                                            struct opal_convertor_t *convertor,
                                                            uint8_t order,
                                                            size_t reserve,
                                                            size_t *size,
                                                            uint32_t flags);

static int vader_add_procs(struct mca_btl_base_module_t* btl,
                           size_t nprocs, struct ompi_proc_t **procs,
                           struct mca_btl_base_endpoint_t** peers,
                           struct opal_bitmap_t* reachability);

static int vader_ft_event (int state);

mca_btl_vader_t mca_btl_vader = {
    {
        &mca_btl_vader_component.super,
        .btl_eager_limit = 0,
        .btl_rndv_eager_limit = 0,
        .btl_max_send_size = 0,
        .btl_rdma_pipeline_send_length = 0,
        .btl_rdma_pipeline_frag_size = 0,
        .btl_min_rdma_pipeline_size = 0,
        .btl_exclusivity = 0,
        .btl_latency = 0,
        .btl_bandwidth = 0,
        .btl_flags = 0,
        .btl_seg_size = 0,
        .btl_add_procs = vader_add_procs,
        .btl_del_procs = vader_del_procs,
        .btl_register = NULL,
        .btl_finalize = vader_finalize,
        .btl_alloc = mca_btl_vader_alloc,
        .btl_free = vader_free,
        .btl_prepare_src = vader_prepare_src,
        .btl_prepare_dst = vader_prepare_dst,
        .btl_send = mca_btl_vader_send,
        .btl_sendi = mca_btl_vader_sendi,
        .btl_put = mca_btl_vader_put,
        .btl_get = mca_btl_vader_get,
        .btl_dump = mca_btl_base_dump,
        .btl_mpool = NULL,
        .btl_register_error = vader_register_error_cb,
        .btl_ft_event = vader_ft_event
    }
};

static int vader_btl_first_time_init(mca_btl_vader_t *vader_btl, int n)
{
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    int rc;

    /* generate the endpoints */
    component->endpoints = (struct mca_btl_base_endpoint_t *) calloc (n, sizeof (struct mca_btl_base_endpoint_t));

    component->segment_offset = (n + 1) * 4096;

    /* initialize fragment descriptor free lists */
    /* initialize free list for send fragments */
    rc = ompi_free_list_init_ex_new(&component->vader_frags_eager,
                                    sizeof (mca_btl_vader_frag_t),
                                    opal_cache_line_size, OBJ_CLASS(mca_btl_vader_frag_t),
                                    0, opal_cache_line_size,
                                    component->vader_free_list_num,
                                    component->vader_free_list_max,
                                    component->vader_free_list_inc,
                                    NULL, mca_btl_vader_frag_init,
                                    (void *) (sizeof (mca_btl_vader_hdr_t) +
                                              mca_btl_vader.super.btl_eager_limit));
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    /* initialize free list for put/get fragments */
    rc = ompi_free_list_init_ex_new(&component->vader_frags_user, 
                                    sizeof(mca_btl_vader_frag_t),
                                    opal_cache_line_size, OBJ_CLASS(mca_btl_vader_frag_t),
                                    0, opal_cache_line_size,
                                    component->vader_free_list_num,
                                    component->vader_free_list_max,
                                    component->vader_free_list_inc,
                                    NULL, mca_btl_vader_frag_init,
                                    (void *) (sizeof(mca_btl_vader_hdr_t) +
                                              mca_btl_vader_component.max_inline_send));
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    /* set flag indicating btl has been inited */
    vader_btl->btl_inited = true;

    return OMPI_SUCCESS;
}


static int init_vader_endpoint (struct mca_btl_base_endpoint_t *ep, struct ompi_proc_t *proc, int local_rank) {
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    struct vader_modex_t *modex;
    size_t msg_size;
    int rc;

    ep->peer_smp_rank = local_rank;

    if (OMPI_SUCCESS != (rc = ompi_modex_recv(&component->super.btl_version,
                                              proc, (void *)&modex, &msg_size))) {
        return rc;
    }

    ep->apid = xpmem_get (modex->seg_id, XPMEM_RDWR, XPMEM_PERMIT_MODE, (void *) 0666);
    ep->rcache = mca_rcache_base_module_create("vma");
    ep->next_fbox_out = 0;
    ep->next_fbox_in  = 0;

    /* Attatch to the remote process' segment */
    ep->segment_base  =
        vader_reg_to_ptr (vader_get_registation (ep, modex->segment_base, mca_btl_vader_component.segment_size,
                                                 MCA_MPOOL_FLAGS_PERSIST),
                          modex->segment_base);

    ep->fifo     = (struct vader_fifo_t *) ep->segment_base;
    ep->fbox_out = ep->segment_base + (1 + component->my_smp_rank) * 4096;
    ep->fbox_in  = component->my_segment + (1 + local_rank) * 4096;

    memset (ep->fbox_in, MCA_BTL_VADER_FBOX_FREE, 4096);

    return OMPI_SUCCESS;
}

/**
 * PML->BTL notification of change in the process list.
 * PML->BTL Notification that a receive fragment has been matched.
 * Called for message that is send from process with the virtual
 * address of the shared memory segment being different than that of
 * the receiver.
 *
 * @param btl (IN)
 * @param proc (IN)
 * @param peer (OUT)
 * @return     OMPI_SUCCESS or error status on failure.
 *
 */

static int vader_add_procs (struct mca_btl_base_module_t* btl,
                            size_t nprocs, struct ompi_proc_t **procs,
                            struct mca_btl_base_endpoint_t **peers,
                            opal_bitmap_t *reachability)
{
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    mca_btl_vader_t *vader_btl = (mca_btl_vader_t *) btl;
    int32_t n_local_procs = 0, proc, local_rank, my_smp_rank = -1;
    bool have_connected_peer = false;
    ompi_proc_t *my_proc;
    int rc = OMPI_SUCCESS;

    /* initializion */

    /* get pointer to my proc structure */
    if (NULL == (my_proc = ompi_proc_local())) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Get unique host identifier for each process in the list,
     * and idetify procs that are on this host.  Add procs on this
     * host to shared memory reachbility list.  Also, get number
     * of local procs in the procs list. */
    for (proc = 0; proc < (int32_t) nprocs; ++proc) {
        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job) */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid ||
            !OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            continue;
        }

        n_local_procs++;

        if (my_proc != procs[proc]) {
            /* we have someone to talk to */
            have_connected_peer = true;

            /* add this proc to shared memory accessibility list */
            rc = opal_bitmap_set_bit (reachability, proc);
            if(OMPI_SUCCESS != rc) {
                return rc;
            }
        } else {
            my_smp_rank = mca_btl_vader_component.my_smp_rank = n_local_procs - 1;
        }
    }

    /* jump out if there's not someone we can talk to */
    if (!have_connected_peer) {
        return OMPI_SUCCESS;
    }

    /* make sure that my_smp_rank has been defined */
    if(-1 == my_smp_rank) {
        return  OMPI_ERROR;
    }

    if (!vader_btl->btl_inited) {
        rc = vader_btl_first_time_init (vader_btl, n_local_procs);
        if (rc != OMPI_SUCCESS) {
            return rc;
        }
    }

    for (proc = 0, local_rank = 0; proc < (int32_t) nprocs; ++proc) {
        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job) */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid ||
            !OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            peers[proc] = NULL;
            continue;
        }

        /* setup endpoint */
        peers[proc] = component->endpoints + local_rank;
        init_vader_endpoint (peers[proc], procs[proc], local_rank++);

        /* check to see if this is me */
        if (my_proc == procs[proc]) {
            peers[proc] = NULL;
        }
    }

    /* update the local smp process count */
    component->num_smp_procs += n_local_procs;

    return OMPI_SUCCESS;
}

/**
 * PML->BTL notification of change in the process list.
 *
 * @param btl (IN)     BTL instance
 * @param proc (IN)    Peer process
 * @param peer (IN)    Peer addressing information.
 * @return             Status indicating if cleanup was successful
 *
 */

static int vader_del_procs(struct mca_btl_base_module_t *btl,
                           size_t nprocs, struct ompi_proc_t **procs,
                           struct mca_btl_base_endpoint_t **peers)
{
    return OMPI_SUCCESS;
}


/**
 * MCA->BTL Clean up any resources held by BTL module
 * before the module is unloaded.
 *
 * @param btl (IN)   BTL module.
 *
 * Prior to unloading a BTL module, the MCA framework will call
 * the BTL finalize method of the module. Any resources held by
 * the BTL should be released and if required the memory corresponding
 * to the BTL module freed.
 *
 */

static int vader_finalize(struct mca_btl_base_module_t *btl)
{
    return OMPI_SUCCESS;
}


/**
 * Register a callback function that is called on error..
 *
 * @param btl (IN)     BTL module
 * @param cbfunc (IN)  function to call on error
 * @return             Status indicating if cleanup was successful
 */
static int vader_register_error_cb(struct mca_btl_base_module_t* btl,
                                   mca_btl_base_module_error_cb_fn_t cbfunc)
{
    ((mca_btl_vader_t *)btl)->error_cb = cbfunc;
    return OMPI_SUCCESS;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
mca_btl_base_descriptor_t *mca_btl_vader_alloc(struct mca_btl_base_module_t *btl,
                                               struct mca_btl_base_endpoint_t *endpoint,
                                               uint8_t order, size_t size, uint32_t flags)
{
    mca_btl_vader_frag_t *frag = NULL;

    if (size <= (size_t) mca_btl_vader_component.max_inline_send) {
        (void) MCA_BTL_VADER_FRAG_ALLOC_USER(frag);
    } else if (size <= mca_btl_vader.super.btl_eager_limit) {
        (void) MCA_BTL_VADER_FRAG_ALLOC_EAGER(frag);
    }

    if (OPAL_LIKELY(frag != NULL)) {
        frag->segments[0].seg_len  = size;
        frag->endpoint         = endpoint;

        frag->base.des_flags   = flags;
        frag->base.order       = order;
    }

    return (mca_btl_base_descriptor_t *) frag;
}

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
static int vader_free (struct mca_btl_base_module_t *btl, mca_btl_base_descriptor_t *des)
{
    MCA_BTL_VADER_FRAG_RETURN((mca_btl_vader_frag_t *) des);

    return OMPI_SUCCESS;
}

struct mca_btl_base_descriptor_t *vader_prepare_dst(struct mca_btl_base_module_t *btl,
                                                    struct mca_btl_base_endpoint_t *endpoint,
                                                    struct mca_mpool_base_registration_t *registration,
                                                    struct opal_convertor_t *convertor,
                                                    uint8_t order, size_t reserve, size_t *size,
                                                    uint32_t flags)
{
    mca_btl_vader_frag_t *frag;
    void *data_ptr;

    (void) MCA_BTL_VADER_FRAG_ALLOC_USER(frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }
    
    opal_convertor_get_current_pointer (convertor, (void **) &data_ptr);

    frag->segments[0].seg_addr.lval = (uint64_t)(uintptr_t) data_ptr;
    frag->segments[0].seg_len       = *size;
    
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    frag->endpoint = endpoint;

    return &frag->base;
}

/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 */
static struct mca_btl_base_descriptor_t *vader_prepare_src (struct mca_btl_base_module_t *btl,
                                                            struct mca_btl_base_endpoint_t *endpoint,
                                                            mca_mpool_base_registration_t *registration,
                                                            struct opal_convertor_t *convertor,
                                                            uint8_t order, size_t reserve, size_t *size,
                                                            uint32_t flags)
{
    struct iovec iov;
    mca_btl_vader_frag_t *frag;
    uint32_t iov_count = 1;
    void *data_ptr, *fbox_ptr;
    int rc;

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    if (OPAL_LIKELY(reserve)) {
        /* in place send fragment */
        if (OPAL_UNLIKELY(opal_convertor_need_buffers(convertor))) {
            /* non-contiguous data requires the convertor */
            (void) MCA_BTL_VADER_FRAG_ALLOC_EAGER(frag);
            if (OPAL_UNLIKELY(NULL == frag)) {
                return NULL;
            }

            iov.iov_len = *size;
            iov.iov_base =
                (IOVBASE_TYPE *)(((uintptr_t)(frag->segments[0].seg_addr.pval)) +
                                 reserve);

            rc = opal_convertor_pack (convertor, &iov, &iov_count, size);
            if (OPAL_UNLIKELY(rc < 0)) {
                MCA_BTL_VADER_FRAG_RETURN(frag);
                return NULL;
            }

            frag->segments[0].seg_len = reserve + *size;
            frag->base.des_src_cnt = 1;
        } else {
            (void) MCA_BTL_VADER_FRAG_ALLOC_USER(frag);
            if (OPAL_UNLIKELY(NULL == frag)) {
                return NULL;
            }

            if ((*size + reserve) > (size_t) mca_btl_vader_component.max_inline_send) {
                /* single copy send */
                frag->hdr->flags = MCA_BTL_VADER_FLAG_SINGLE_COPY;

                /* set up single copy io vector */
                frag->hdr->sc_iov.iov_base = data_ptr;
                frag->hdr->sc_iov.iov_len  = *size;

                frag->segments[0].seg_len = reserve;
                frag->segments[1].seg_len = *size;
                frag->segments[1].seg_addr.pval = data_ptr;
                frag->base.des_src_cnt = 2;
            } else {
                /* inline send */

                /* try to reserve a fast box for this transfer */
                fbox_ptr = mca_btl_vader_reserve_fbox (endpoint, reserve + *size);

                if (fbox_ptr) {
                    frag->hdr->flags |= MCA_BTL_VADER_FLAG_FBOX;
                    frag->segments[0].seg_addr.pval = fbox_ptr;
                }

                /* NTH: the covertor adds some latency so we bypass it here */
                vader_memmove ((void *)((uintptr_t)frag->segments[0].seg_addr.pval + reserve),
                               data_ptr, *size);
                frag->segments[0].seg_len = reserve + *size;
                frag->base.des_src_cnt = 1;
            }
        }
    } else {
        /* put/get fragment */
        (void) MCA_BTL_VADER_FRAG_ALLOC_USER(frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        frag->segments[0].seg_addr.lval = (uint64_t)(uintptr_t) data_ptr;
        frag->segments[0].seg_len       = reserve + *size;
        frag->base.des_src_cnt = 1;
    }

    frag->base.des_src     = frag->segments;
    frag->base.order       = order;
    frag->base.des_flags   = flags;

    frag->endpoint = endpoint;

    return &frag->base;
}

/**
 * Fault Tolerance Event Notification Function
 * @param state Checkpoint Stae
 * @return OMPI_SUCCESS or failure status
 */
static int vader_ft_event (int state)
{
    return OMPI_SUCCESS;
}
