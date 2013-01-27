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
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.  
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
        0, /* btl_eager_limit */
        0, /* btl_rndv_eager_limit */
        0, /* btl_max_send_size */
        0, /* btl_rdma_pipeline_send_length */
        0, /* btl_rdma_pipeline_frag_size */
        0, /* btl_min_rdma_pipeline_size */
        0, /* btl_exclusivity */
        0, /* bTl_latency */
        0, /* btl_bandwidth */
        0, /* btl_flags */
        0, /* btl segment size */
        vader_add_procs,
        vader_del_procs,
        NULL, /* btl_register */
        vader_finalize,
        mca_btl_vader_alloc,
        vader_free,
        vader_prepare_src,
        vader_prepare_dst,
        mca_btl_vader_send,
        mca_btl_vader_sendi,
        mca_btl_vader_put,
        mca_btl_vader_get,
        mca_btl_base_dump,
        NULL, /* btl_mpool */
        vader_register_error_cb, /* register error */
        vader_ft_event
    }
};

static inline int vader_init_mpool (mca_btl_vader_t *vader_btl, int n)
{
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    mca_mpool_base_resources_t res;

    res.mem_node = -1;

    /* determine how much memory to create */
    /*
     * This heuristic formula mostly says that we request memory for:
     * - a vader fifo
     * - eager fragments (2 * n of them, allocated in vader_free_list_inc chunks)
     *
     * On top of all that, we sprinkle in some number of "opal_cache_line_size"
     * additions to account for some padding and edge effects that may lie
     * in the allocator.
     */
    res.size = sizeof (vader_fifo_t) + 4 * opal_cache_line_size +
        (2 * n + component->vader_free_list_inc) * (component->eager_limit + 2 * opal_cache_line_size);

    /* before we multiply by n, make sure the result won't overflow */
    /* Stick that little pad in, particularly since we'll eventually
     * need a little extra space.  E.g., in mca_mpool_vader_init() in
     * mpool_vader_component.c when sizeof(mca_common_sm_module_t) is
     * added.
     */
    if ( ((double) res.size) * n > LONG_MAX - 4096 )
        return OMPI_ERR_OUT_OF_RESOURCE;

    res.size *= n;

    /* now, create it */
    component->vader_mpool =
        mca_mpool_base_module_create(component->vader_mpool_name,
                                     vader_btl, &res);
    /* Sanity check to ensure that we found it */
    if(NULL == component->vader_mpool) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    component->vader_mpool_base =
        component->vader_mpool->mpool_base (component->vader_mpool);

    return OMPI_SUCCESS;
}

static int vader_btl_first_time_init(mca_btl_vader_t *vader_btl, int n)
{
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    size_t size;
    char *vader_ctl_file;
    vader_fifo_t *my_fifos;
    ompi_proc_t **procs;
    size_t num_procs;
    int i, rc;

    rc = vader_init_mpool (vader_btl, n);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    /* create a list of peers */
    component->vader_peers = (struct mca_btl_base_endpoint_t **)
        calloc(n, sizeof(struct mca_btl_base_endpoint_t *));
    if(NULL == component->vader_peers)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* Allocate Shared Memory BTL process coordination
     * data structure.  This will reside in shared memory */

    /* set file name */
    if(asprintf(&vader_ctl_file, "%s"OPAL_PATH_SEP"vader_btl_module.%s",
                ompi_process_info.job_session_dir,
                ompi_process_info.nodename) < 0)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* Pass in a data segment alignment of 0 to get no data
       segment (only the shared control structure) */
    size = sizeof (mca_common_sm_seg_header_t) +
        n * (sizeof (vader_fifo_t *) + sizeof (char *)
             + sizeof (xpmem_segid_t)) + opal_cache_line_size;
    procs = ompi_proc_world(&num_procs);
    if (!(mca_btl_vader_component.vader_seg =
          mca_common_sm_init(procs, num_procs, size, vader_ctl_file,
                             sizeof (mca_common_sm_seg_header_t),
                             opal_cache_line_size))) {
        opal_output(0, "vader_add_procs: unable to create shared memory "
                    "BTL coordinating strucure :: size %lu \n",
                    (unsigned long) size);
        free(procs);
        free(vader_ctl_file);
        return OMPI_ERROR;
    }
    free(procs);
    free(vader_ctl_file);

    component->shm_fifo    = (volatile vader_fifo_t **) component->vader_seg->module_data_addr;
    component->shm_bases   = (char **)(component->shm_fifo + n);
    component->shm_seg_ids = (xpmem_segid_t *)(component->shm_bases + n);

    /* set the base of the shared memory segment */
    component->shm_bases[component->my_smp_rank] = (char *)component->vader_mpool_base;
    component->shm_seg_ids[component->my_smp_rank] = component->my_seg_id;

    /* initialize the fifo and fast boxes "owned" by this process */
    posix_memalign ((void **)&my_fifos, getpagesize (), (n + 1) * getpagesize ());
    if(NULL == my_fifos)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* cache the pointer to the 2d fifo array.  These addresses
     * are valid in the current process space */
    component->fifo = (vader_fifo_t **) calloc (n, sizeof(vader_fifo_t *));
    if(NULL == component->fifo)
        return OMPI_ERR_OUT_OF_RESOURCE;

    component->shm_fifo[component->my_smp_rank] =
        component->fifo[component->my_smp_rank] = my_fifos;

    component->apids = (xpmem_apid_t *) calloc (n, sizeof (xpmem_apid_t));
    if (NULL == component->apids)
        return OMPI_ERR_OUT_OF_RESOURCE;

    component->xpmem_rcaches =
        (struct mca_rcache_base_module_t **) calloc (n, sizeof (struct mca_rcache_base_module_t *));
    if (NULL == component->xpmem_rcaches)
        return OMPI_ERR_OUT_OF_RESOURCE;

    component->vader_fboxes_in = (unsigned char **) calloc (n, sizeof (char *));
    if (NULL == component->vader_fboxes_in)
        return OMPI_ERR_OUT_OF_RESOURCE;

    component->vader_fboxes_out = (unsigned char **) calloc (n, sizeof (char *));
    if (NULL == component->vader_fboxes_out)
        return OMPI_ERR_OUT_OF_RESOURCE;

    component->vader_next_fbox_in = (unsigned char *) calloc (64, 1);
    if (NULL == component->vader_next_fbox_in)
        return OMPI_ERR_OUT_OF_RESOURCE;

    component->vader_next_fbox_out = (unsigned char *) calloc (64, 1);
    if (NULL == component->vader_next_fbox_out)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* initialize fragment descriptor free lists */
    /* initialize free list for send fragments */
    i = ompi_free_list_init_new(&component->vader_frags_eager,
                                sizeof (mca_btl_vader_frag_t),
                                opal_cache_line_size, OBJ_CLASS(mca_btl_vader_frag_t),
                                sizeof (mca_btl_vader_hdr_t) + component->eager_limit,
                                opal_cache_line_size,
                                component->vader_free_list_num,
                                component->vader_free_list_max,
                                component->vader_free_list_inc,
                                component->vader_mpool);
    if (OMPI_SUCCESS != i)
        return i;

    /* initialize free list for put/get fragments */
    i = ompi_free_list_init_new(&component->vader_frags_user, 
                                sizeof(mca_btl_vader_frag_t),
                                opal_cache_line_size, OBJ_CLASS(mca_btl_vader_frag_t),
                                sizeof(mca_btl_vader_hdr_t) + mca_btl_vader_max_inline_send,
                                opal_cache_line_size,
                                component->vader_free_list_num,
                                component->vader_free_list_max,
                                component->vader_free_list_inc,
                                component->vader_mpool);
    if (OMPI_SUCCESS != i)
        return i;

    /* set flag indicating btl has been inited */
    vader_btl->btl_inited = true;

    return OMPI_SUCCESS;
}

static struct
mca_btl_base_endpoint_t *create_vader_endpoint (int local_proc, struct ompi_proc_t *proc)
{
    struct mca_btl_base_endpoint_t *ep = (struct mca_btl_base_endpoint_t *)
        calloc(1, sizeof (struct mca_btl_base_endpoint_t));
    if(NULL != ep) {
        ep->peer_smp_rank = local_proc + mca_btl_vader_component.num_smp_procs;
    }

    return ep;
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
    int32_t n_local_procs = 0, proc, i, my_smp_rank = -1;
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
            peers[proc] = NULL;
            continue;
        }

        /* check to see if this is me */
        if (my_proc == procs[proc]) {
            my_smp_rank = component->my_smp_rank = n_local_procs++;
            continue;
        }

        /* we have someone to talk to */
        have_connected_peer = true;

        if (!(peers[proc] = create_vader_endpoint (n_local_procs, procs[proc]))) {
            rc = OMPI_ERROR;
            goto CLEANUP;
        }
        n_local_procs++;

        /* add this proc to shared memory accessibility list */
        rc = opal_bitmap_set_bit (reachability, proc);
        if(OMPI_SUCCESS != rc) {
            goto CLEANUP;
        }
    }

    /* jump out if there's not someone we can talk to */
    if (!have_connected_peer) {
        goto CLEANUP;
    }

    /* make sure that my_smp_rank has been defined */
    if(-1 == my_smp_rank) {
        rc = OMPI_ERROR;
        goto CLEANUP;
    }

    if (!vader_btl->btl_inited) {
        rc = vader_btl_first_time_init(vader_btl, n_local_procs);
        if(rc != OMPI_SUCCESS) {
            goto CLEANUP;
        }
    }

    /* set local proc's smp rank in the peers structure for
     * rapid access and calculate reachability */
    for (proc = 0; proc < (int32_t) nprocs; ++proc) {
        if(NULL == peers[proc])
            continue;
        component->vader_peers[peers[proc]->peer_smp_rank] = peers[proc];
        peers[proc]->my_smp_rank = my_smp_rank;
    }

    /* initialize own FIFOs */
    /*
     * The receiver initializes all its FIFOs.  All components will
     * be allocated near the receiver.  Nothing will be local to
     * "the sender" since there will be many senders.
     */
    rc = vader_fifo_init (component->fifo[my_smp_rank]);
    if (OMPI_SUCCESS != rc) {
        goto CLEANUP;
    }

    opal_atomic_wmb();

    /* Sync with other local procs. Force the FIFO initialization to always
     * happens before the readers access it.
     */
    opal_atomic_add_32( &component->vader_seg->module_seg->seg_inited, 1);
    while (n_local_procs >
           component->vader_seg->module_seg->seg_inited) {
        opal_progress();
        opal_atomic_rmb();
    }

    /* coordinate with other processes */
    for (i = 0 ; i < n_local_procs ; ++i) {
        int peer_smp_rank = i + component->num_smp_procs;

        /* spin until this element is allocated */
        /* doesn't really wait for that process... FIFO might be allocated, but not initialized */
        while (NULL == component->shm_fifo[peer_smp_rank]) {
            opal_progress();
            opal_atomic_rmb();
        }

        if (my_smp_rank != peer_smp_rank) {
            void *rem_ptr = (void *) component->shm_fifo[peer_smp_rank];

            component->apids[peer_smp_rank] =
                xpmem_get (component->shm_seg_ids[peer_smp_rank],
                           XPMEM_RDWR, XPMEM_PERMIT_MODE, (void *) 0666);
            component->xpmem_rcaches[peer_smp_rank] = mca_rcache_base_module_create("vma");

            /* get a persistent pointer to the peer's fifo */
            component->fifo[peer_smp_rank] =
                vader_reg_to_ptr (vader_get_registation (peer_smp_rank, rem_ptr,
                                                         (n_local_procs + 1) * getpagesize (),
                                                         MCA_MPOOL_FLAGS_PERSIST), rem_ptr);

            /* fast boxes are allocated at the same time as the fifos */
            component->vader_fboxes_in[peer_smp_rank] = (unsigned char *) component->fifo[my_smp_rank] +
                (peer_smp_rank + 1) * getpagesize ();
            component->vader_fboxes_out[peer_smp_rank] = (unsigned char *) component->fifo[peer_smp_rank] +
                (my_smp_rank + 1) * getpagesize ();

            component->vader_next_fbox_in[peer_smp_rank] = 0;
            component->vader_next_fbox_out[peer_smp_rank] = 0;

            memset (component->vader_fboxes_in[peer_smp_rank], MCA_BTL_VADER_FBOX_FREE, getpagesize());
        }
    }

    /* update the local smp process count */
    component->num_smp_procs += n_local_procs;

    /* make sure we have enough eager fragmnents for each process */
    rc = ompi_free_list_resize(&component->vader_frags_eager,
                               component->num_smp_procs * 2);

 CLEANUP:

    return rc;
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

    if (size <= (size_t) mca_btl_vader_max_inline_send) {
        (void) MCA_BTL_VADER_FRAG_ALLOC_USER(frag);
    } else if (size <= mca_btl_vader_component.eager_limit) {
        (void) MCA_BTL_VADER_FRAG_ALLOC_EAGER(frag);
    }

    if (OPAL_LIKELY(frag != NULL)) {
        frag->segments[0].seg_len  = size;
        frag->endpoint         = endpoint;

        frag->base.des_flags   = flags;
        frag->base.order       = order;
        frag->base.des_src     = frag->segments;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst     = frag->segments;
        frag->base.des_src_cnt = 1;
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
    
    frag->base.des_dst     = frag->segments;
    frag->base.des_dst_cnt = 1;
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
    struct iovec iov, *lcl_mem;
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

            if ((*size + reserve) > (size_t) mca_btl_vader_max_inline_send) {
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
                fbox_ptr = mca_btl_vader_reserve_fbox (endpoint->peer_smp_rank, reserve + *size);

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
