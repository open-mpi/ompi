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
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "btl_vader_xpmem.h"

#include <string.h>

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
        .btl_add_procs = vader_add_procs,
        .btl_del_procs = vader_del_procs,
        .btl_finalize = vader_finalize,
        .btl_alloc = mca_btl_vader_alloc,
        .btl_free = vader_free,
        .btl_prepare_src = vader_prepare_src,
        .btl_prepare_dst = vader_prepare_dst,
        .btl_send = mca_btl_vader_send,
        .btl_sendi = mca_btl_vader_sendi,
        .btl_dump = mca_btl_base_dump,
        .btl_register_error = vader_register_error_cb,
        .btl_ft_event = vader_ft_event
    }
};

static int vader_btl_first_time_init(mca_btl_vader_t *vader_btl, int n)
{
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    int rc;

    /* generate the endpoints */
    component->endpoints = (struct mca_btl_base_endpoint_t *) calloc (n + 1, sizeof (struct mca_btl_base_endpoint_t));
    component->endpoints[n].peer_smp_rank = -1;
    component->fbox_in_endpoints = calloc (n + 1, sizeof (void *));

    if (NULL == component->endpoints || NULL == component->fbox_in_endpoints) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    component->segment_offset = MCA_BTL_VADER_FIFO_SIZE;

    /* initialize fragment descriptor free lists */
    /* initialize free list for single copy (get, put) */
    if (MCA_BTL_VADER_NONE != mca_btl_vader_component.single_copy_mechanism) {
        rc = ompi_free_list_init_ex_new (&component->vader_frags_rdma,
                                         sizeof(mca_btl_vader_frag_t), 8,
                                         OBJ_CLASS(mca_btl_vader_frag_t),
                                         0, opal_cache_line_size,
                                         component->vader_free_list_num,
                                         component->vader_free_list_max,
                                         component->vader_free_list_inc,
                                         NULL, mca_btl_vader_frag_init, (void *) 0);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    /* initialize free list for small send and inline fragments */
    rc = ompi_free_list_init_ex_new(&component->vader_frags_user, 
                                    sizeof(mca_btl_vader_frag_t),
                                    opal_cache_line_size, OBJ_CLASS(mca_btl_vader_frag_t),
                                    0, opal_cache_line_size,
                                    component->vader_free_list_num,
                                    component->vader_free_list_max,
                                    component->vader_free_list_inc,
                                    NULL, mca_btl_vader_frag_init,
                                    (void *)(intptr_t) mca_btl_vader_component.max_inline_send);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    /* initialize free list for buffered send fragments */
    rc = ompi_free_list_init_ex_new(&component->vader_frags_eager,
                                    sizeof (mca_btl_vader_frag_t),
                                    opal_cache_line_size, OBJ_CLASS(mca_btl_vader_frag_t),
                                    0, opal_cache_line_size,
                                    component->vader_free_list_num,
                                    component->vader_free_list_max,
                                    component->vader_free_list_inc,
                                    NULL, mca_btl_vader_frag_init,
                                    (void *)(intptr_t) mca_btl_vader.super.btl_eager_limit);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    if (MCA_BTL_VADER_XPMEM != mca_btl_vader_component.single_copy_mechanism) {
        /* initialize free list for buffered send fragments */
        rc = ompi_free_list_init_ex_new(&component->vader_frags_max_send,
                                        sizeof (mca_btl_vader_frag_t),
                                        opal_cache_line_size, OBJ_CLASS(mca_btl_vader_frag_t),
                                        0, opal_cache_line_size,
                                        component->vader_free_list_num,
                                        component->vader_free_list_max,
                                        component->vader_free_list_inc,
                                        NULL, mca_btl_vader_frag_init,
                                        (void *)(intptr_t) mca_btl_vader.super.btl_max_send_size);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    /* set flag indicating btl has been inited */
    vader_btl->btl_inited = true;

    return OMPI_SUCCESS;
}


static int init_vader_endpoint (struct mca_btl_base_endpoint_t *ep, struct ompi_proc_t *proc, int remote_rank) {
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    union vader_modex_t *modex;
    size_t msg_size;
    int rc;

    OBJ_CONSTRUCT(ep, mca_btl_vader_endpoint_t);

    ep->peer_smp_rank = remote_rank;

    if (remote_rank != MCA_BTL_VADER_LOCAL_RANK) {
        if (OMPI_SUCCESS != (rc = ompi_modex_recv(&component->super.btl_version,
                                                  proc, (void *)&modex, &msg_size))) {
            return rc;
        }

        /* attatch to the remote segment */
#if OMPI_BTL_VADER_HAVE_XPMEM
        if (MCA_BTL_VADER_XPMEM == mca_btl_vader_component.single_copy_mechanism) {
            /* always use xpmem if it is available */
            ep->segment_data.xpmem.apid = xpmem_get (modex->xpmem.seg_id, XPMEM_RDWR, XPMEM_PERMIT_MODE, (void *) 0666);
            ep->segment_data.xpmem.rcache = mca_rcache_base_module_create("vma");
            (void) vader_get_registation (ep, modex->xpmem.segment_base, mca_btl_vader_component.segment_size,
                                          MCA_MPOOL_FLAGS_PERSIST, (void **) &ep->segment_base);
        } else {
#endif
            /* store a copy of the segment information for detach */
            ep->segment_data.other.seg_ds = malloc (sizeof (opal_shmem_ds_t));
            if (NULL == ep->segment_data.other.seg_ds) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            ep->segment_data.other.seg_ds->seg_base_addr = modex->shmem.segment_base;
            msg_size -= sizeof (modex->shmem.segment_base);

            memcpy (ep->segment_data.other.seg_ds, modex->shmem.buffer, msg_size);

            ep->segment_base = opal_shmem_segment_attach (ep->segment_data.other.seg_ds);
            if (NULL == ep->segment_base) {
                return OMPI_ERROR;
            }
#if OMPI_BTL_VADER_HAVE_XPMEM
        }
#endif
        OBJ_CONSTRUCT(&ep->lock, opal_mutex_t);

        free (modex);
    } else {
        /* set up the segment base so we can calculate a virtual to real for local pointers */
        ep->segment_base = component->my_segment;
    }

    ep->fifo = (struct vader_fifo_t *) ep->segment_base;

    return OMPI_SUCCESS;
}

static int fini_vader_endpoint (struct mca_btl_base_endpoint_t *ep)
{
    /* check if the endpoint is initialized. avoids a double-destruct */
    if (ep->fifo) {
        OBJ_DESTRUCT(ep);
    }

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
    const ompi_proc_t *my_proc;
    int rc = OMPI_SUCCESS;

    /* initializion */

    /* get pointer to my proc structure */
    if (NULL == (my_proc = ompi_proc_local())) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* jump out if there's not someone we can talk to */
    if (1 > MCA_BTL_VADER_NUM_LOCAL_PEERS) {
        return OMPI_SUCCESS;
    }

    /* make sure that my local rank has been defined */
    if (ORTE_LOCAL_RANK_INVALID == MCA_BTL_VADER_LOCAL_RANK) {
        return OMPI_ERROR;
    }

    if (!vader_btl->btl_inited) {
        rc = vader_btl_first_time_init (vader_btl, 1 + MCA_BTL_VADER_NUM_LOCAL_PEERS);
        if (rc != OMPI_SUCCESS) {
            return rc;
        }
    }

    for (int32_t proc = 0, local_rank = 0 ; proc < (int32_t) nprocs ; ++proc) {
        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job) */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid ||
            !OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            peers[proc] = NULL;
            continue;
        }

        if (my_proc != procs[proc]) {
            /* add this proc to shared memory accessibility list */
            rc = opal_bitmap_set_bit (reachability, proc);
            if(OPAL_SUCCESS != rc) {
                return rc;
            }
        }

        /* setup endpoint */
        peers[proc] = component->endpoints + local_rank;
        rc = init_vader_endpoint (peers[proc], procs[proc], local_rank++);
        if (OMPI_SUCCESS != rc) {
            break;
        }
    }

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
    for (size_t i = 0 ; i < nprocs ; ++i) {
        if (peers[i]) {
            fini_vader_endpoint (peers[i]);
            peers[i] = NULL;
        }
    }

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
    mca_btl_vader_component_t *component = &mca_btl_vader_component;
    mca_btl_vader_t *vader_btl = (mca_btl_vader_t *) btl;

    if (!vader_btl->btl_inited) {
        return OMPI_SUCCESS;
    }

    for (int i = 0 ; i < 1 + MCA_BTL_VADER_NUM_LOCAL_PEERS ; ++i) {
        fini_vader_endpoint (component->endpoints + i);
    }

    free (component->endpoints);
    component->endpoints = NULL;

    vader_btl->btl_inited = false;

    free (component->fbox_in_endpoints);
    component->fbox_in_endpoints = NULL;

    if (MCA_BTL_VADER_XPMEM != mca_btl_vader_component.single_copy_mechanism) {
        opal_shmem_unlink (&mca_btl_vader_component.seg_ds);
        opal_shmem_segment_detach (&mca_btl_vader_component.seg_ds);
    }

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
        (void) MCA_BTL_VADER_FRAG_ALLOC_USER(frag, endpoint);
    } else if (size <= mca_btl_vader.super.btl_eager_limit) {
        (void) MCA_BTL_VADER_FRAG_ALLOC_EAGER(frag, endpoint);
    } else if (MCA_BTL_VADER_XPMEM != mca_btl_vader_component.single_copy_mechanism &&
               size <= mca_btl_vader.super.btl_max_send_size) {
        (void) MCA_BTL_VADER_FRAG_ALLOC_MAX(frag, endpoint);
    }

    if (OPAL_LIKELY(frag != NULL)) {
        frag->segments[0].base.seg_len  = size;

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

    (void) MCA_BTL_VADER_FRAG_ALLOC_RDMA(frag, endpoint);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    frag->segments[0].base.seg_addr.lval = (uint64_t)(uintptr_t) data_ptr;
    frag->segments[0].base.seg_len       = *size;

#if OMPI_BTL_VADER_HAVE_KNEM
    if (MCA_BTL_VADER_KNEM == mca_btl_vader_component.single_copy_mechanism) {
        struct knem_cmd_create_region knem_cr;
        struct knem_cmd_param_iovec knem_iov;

        knem_iov.base = (uintptr_t) data_ptr;
        knem_iov.len = *size;

        knem_cr.iovec_array = (uintptr_t) &knem_iov;
        knem_cr.iovec_nr = 1;
        knem_cr.protection = PROT_WRITE;
        /* Vader will explicitly destroy this cookie */
        knem_cr.flags = 0;
        if (OPAL_UNLIKELY(ioctl(mca_btl_vader.knem_fd, KNEM_CMD_CREATE_REGION, &knem_cr) < 0)) {
            MCA_BTL_VADER_FRAG_RETURN(frag);
            return NULL;
        }

        frag->segments[0].cookie = knem_cr.cookie;
        frag->cookie = knem_cr.cookie;
    }
#endif /* OMPI_BTL_SM_HAVE_KNEM */
    
    frag->base.order       = order;
    frag->base.des_flags   = flags;

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
    const size_t total_size = reserve + *size;
    mca_btl_vader_frag_t *frag;
    unsigned char *fbox;
    void *data_ptr;
    int rc;

    opal_convertor_get_current_pointer (convertor, &data_ptr);

    if (OPAL_LIKELY(reserve)) {
        /* in place send fragment */
        if (OPAL_UNLIKELY(opal_convertor_need_buffers(convertor))) {
            uint32_t iov_count = 1;
            struct iovec iov;

            /* non-contiguous data requires the convertor */
            if (MCA_BTL_VADER_XPMEM != mca_btl_vader_component.single_copy_mechanism &&
                total_size > mca_btl_vader.super.btl_eager_limit) {
                (void) MCA_BTL_VADER_FRAG_ALLOC_MAX(frag, endpoint);
            } else
                (void) MCA_BTL_VADER_FRAG_ALLOC_EAGER(frag, endpoint);

            if (OPAL_UNLIKELY(NULL == frag)) {
                return NULL;
            }

            iov.iov_len = *size;
            iov.iov_base =
                (IOVBASE_TYPE *)(((uintptr_t)(frag->segments[0].base.seg_addr.pval)) +
                                 reserve);

            rc = opal_convertor_pack (convertor, &iov, &iov_count, size);
            if (OPAL_UNLIKELY(rc < 0)) {
                MCA_BTL_VADER_FRAG_RETURN(frag);
                return NULL;
            }

            frag->segments[0].base.seg_len = *size + reserve;
        } else {
            if (MCA_BTL_VADER_XPMEM != mca_btl_vader_component.single_copy_mechanism) {
                if (OPAL_LIKELY(total_size <= mca_btl_vader.super.btl_eager_limit)) {
                    (void) MCA_BTL_VADER_FRAG_ALLOC_EAGER(frag, endpoint);
                } else {
                    (void) MCA_BTL_VADER_FRAG_ALLOC_MAX(frag, endpoint);
                }
            } else
                (void) MCA_BTL_VADER_FRAG_ALLOC_USER(frag, endpoint);

            if (OPAL_UNLIKELY(NULL == frag)) {
                return NULL;
            }

#if OMPI_BTL_VADER_HAVE_XPMEM
            /* use xpmem to send this segment if it is above the max inline send size */
            if (OPAL_UNLIKELY(MCA_BTL_VADER_XPMEM == mca_btl_vader_component.single_copy_mechanism &&
                total_size > (size_t) mca_btl_vader_component.max_inline_send)) {
                /* single copy send */
                frag->hdr->flags = MCA_BTL_VADER_FLAG_SINGLE_COPY;

                /* set up single copy io vector */
                frag->hdr->sc_iov.iov_base = data_ptr;
                frag->hdr->sc_iov.iov_len  = *size;

                frag->segments[0].base.seg_len = reserve;
                frag->segments[1].base.seg_len = *size;
                frag->segments[1].base.seg_addr.pval = data_ptr;
                frag->base.des_src_cnt = 2;
            } else {
#endif

                /* inline send */
                if (OPAL_LIKELY(MCA_BTL_DES_FLAGS_BTL_OWNERSHIP & flags)) {
                    /* try to reserve a fast box for this transfer only if the
                     * fragment does not belong to the caller */
                    fbox = mca_btl_vader_reserve_fbox (endpoint, total_size);
                    if (OPAL_LIKELY(fbox)) {
                        frag->segments[0].base.seg_addr.pval = fbox;
                    }

                    frag->fbox = fbox;
                }

                /* NTH: the covertor adds some latency so we bypass it here */
                memcpy ((void *)((uintptr_t)frag->segments[0].base.seg_addr.pval + reserve), data_ptr, *size);
                frag->segments[0].base.seg_len = total_size;
#if OMPI_BTL_VADER_HAVE_XPMEM
            }
#endif
        }
    } else {
        /* put/get fragment */
        (void) MCA_BTL_VADER_FRAG_ALLOC_RDMA(frag, endpoint);
        if (OPAL_UNLIKELY(NULL == frag)) {
            return NULL;
        }

        frag->segments[0].base.seg_addr.lval = (uint64_t)(uintptr_t) data_ptr;
        frag->segments[0].base.seg_len       = total_size;
#if OMPI_BTL_VADER_HAVE_KNEM
        if (MCA_BTL_VADER_KNEM == mca_btl_vader_component.single_copy_mechanism) {
            struct knem_cmd_create_region knem_cr;
            struct knem_cmd_param_iovec knem_iov;

            knem_iov.base = (uintptr_t) data_ptr;
            knem_iov.len = total_size;

            knem_cr.iovec_array = (uintptr_t) &knem_iov;
            knem_cr.iovec_nr = 1;
            knem_cr.protection = PROT_READ;
            /* Vader will explicitly destroy this cookie */
            knem_cr.flags = 0;
            if (OPAL_UNLIKELY(ioctl(mca_btl_vader.knem_fd, KNEM_CMD_CREATE_REGION, &knem_cr) < 0)) {
                MCA_BTL_VADER_FRAG_RETURN(frag);
                return NULL;
            }

            frag->segments[0].cookie = knem_cr.cookie;
            frag->cookie = knem_cr.cookie;
        }
#endif /* OMPI_BTL_SM_HAVE_KNEM */
    }

    frag->base.order       = order;
    frag->base.des_flags   = flags;

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

static void mca_btl_vader_endpoint_constructor (mca_btl_vader_endpoint_t *ep)
{
    OBJ_CONSTRUCT(&ep->pending_frags, opal_list_t);
    ep->fifo = NULL;
}

static void mca_btl_vader_endpoint_destructor (mca_btl_vader_endpoint_t *ep)
{
    OBJ_DESTRUCT(&ep->pending_frags);

#if OMPI_BTL_VADER_HAVE_XPMEM
    if (MCA_BTL_VADER_XPMEM == mca_btl_vader_component.single_copy_mechanism) {
        if (ep->segment_data.xpmem.rcache) {
            /* clean out the registration cache */
            const int nregs = 100;
            mca_mpool_base_registration_t *regs[nregs];
            int reg_cnt;

            do {
                reg_cnt = ep->segment_data.xpmem.rcache->rcache_find_all(ep->segment_data.xpmem.rcache, 0, (size_t)-1,
                                                                          regs, nregs);

                for (int i = 0 ; i < reg_cnt ; ++i) {
                    /* otherwise dereg will fail on assert */
                    regs[i]->ref_count = 0;
                    OBJ_RELEASE(regs[i]);
                }
            } while (reg_cnt == nregs);

            ep->segment_data.xpmem.rcache = NULL;
        }

        if (ep->segment_base) {
            xpmem_release (ep->segment_data.xpmem.apid);
            ep->segment_data.xpmem.apid = 0;
        }
    } else
#endif
    if (ep->segment_data.other.seg_ds) {
        /* disconnect from the peer's segment. in 1.8 the opal_shmem_ds_t in the endpoint
         * is safe to use directly. */
        opal_shmem_segment_detach (ep->segment_data.other.seg_ds);
        free (ep->segment_data.other.seg_ds);
        ep->segment_data.other.seg_ds = NULL;
    }

    ep->fbox_in.buffer = ep->fbox_out.buffer = NULL;
    ep->segment_base = NULL;
    ep->fifo = NULL;
}

OBJ_CLASS_INSTANCE(mca_btl_vader_endpoint_t, opal_list_item_t, mca_btl_vader_endpoint_constructor, mca_btl_vader_endpoint_destructor);
