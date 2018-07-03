/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_UGNI_FRAG_H)
#define MCA_BTL_UGNI_FRAG_H

#include "btl_ugni.h"
#include "btl_ugni_endpoint.h"

#include <string.h>

typedef struct mca_btl_ugni_send_frag_hdr_t {
    uint32_t lag;
} mca_btl_ugni_send_frag_hdr_t;

typedef struct mca_btl_ugni_send_ex_frag_hdr_t {
    mca_btl_ugni_send_frag_hdr_t send;
    uint8_t                      pml_header[128];
} mca_btl_ugni_send_ex_frag_hdr_t;

typedef struct mca_btl_ugni_rdma_frag_hdr_t {
    void *ctx;
} mca_btl_ugni_rdma_frag_hdr_t;

typedef struct mca_btl_ugni_eager_frag_hdr_t {
    mca_btl_ugni_send_frag_hdr_t send;
    uint32_t size;
    uint64_t address;
    mca_btl_base_registration_handle_t memory_handle;
    void *ctx;
} mca_btl_ugni_eager_frag_hdr_t;

typedef struct mca_btl_ugni_eager_ex_frag_hdr_t {
    mca_btl_ugni_eager_frag_hdr_t eager;
    uint8_t                       pml_header[128];
} mca_btl_ugni_eager_ex_frag_hdr_t;

typedef union mca_btl_ugni_frag_hdr_t {
    mca_btl_ugni_send_frag_hdr_t     send;
    mca_btl_ugni_send_ex_frag_hdr_t  send_ex;
    mca_btl_ugni_rdma_frag_hdr_t     rdma;
    mca_btl_ugni_eager_frag_hdr_t    eager;
    mca_btl_ugni_eager_ex_frag_hdr_t eager_ex;
} mca_btl_ugni_frag_hdr_t;

enum {
    MCA_BTL_UGNI_FRAG_BUFFERED      = 1,  /* frag data is buffered */
    MCA_BTL_UGNI_FRAG_COMPLETE      = 2,  /* smsg complete for frag */
    MCA_BTL_UGNI_FRAG_EAGER         = 4,  /* eager get frag */
    MCA_BTL_UGNI_FRAG_IGNORE        = 8,  /* ignore local smsg completion */
    MCA_BTL_UGNI_FRAG_SMSG_COMPLETE = 16, /* SMSG has completed for this message */
    MCA_BTL_UGNI_FRAG_RESPONSE      = 32,
};

struct mca_btl_ugni_base_frag_t;

typedef struct mca_btl_ugni_base_frag_t {
    mca_btl_base_descriptor_t    base;
    volatile int32_t             ref_cnt;
    uint32_t                     msg_id;
    uint16_t                     hdr_size;
    uint16_t                     flags;
    mca_btl_ugni_frag_hdr_t      hdr;
    mca_btl_base_segment_t       segments[2];
    gni_post_descriptor_t        post_desc;
    mca_btl_base_endpoint_t     *endpoint;
    mca_btl_ugni_reg_t          *registration;
    opal_free_list_t            *my_list;
    mca_btl_base_registration_handle_t memory_handle;
} mca_btl_ugni_base_frag_t;

typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_smsg_frag_t;
typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_rdma_frag_t;
typedef struct mca_btl_ugni_base_frag_t mca_btl_ugni_eager_frag_t;

typedef struct mca_btl_ugni_post_descriptor_t {
    /** endpoint currently associated with this desctiptor */
    mca_btl_base_endpoint_t *endpoint;
    /** local memory handle (for callback) */
    mca_btl_base_registration_handle_t *local_handle;
    /** currently associated completion queue */
    mca_btl_ugni_cq_t *cq;
    /** user callback function */
    mca_btl_base_rdma_completion_fn_t cbfunc;
    /** user callback data */
    void *cbdata;
    /** user callback context */
    void *ctx;
    /** opal status of this descriptor. filled in by
     * mca_btl_ugni_cq_get_completed_desc_device() */
    int rc;
    /** true if posted with the BTE. false if FMA. this is used as part
     * of the BTE throttling code. */
    bool use_bte;
    /** uGNI library post descriptor. this is last in this structure
     * to try to keep it hot in the cache after copying this descriptor
     * into the allocated descritor. (post follows almost immediately
     * after allocate. */
    gni_post_descriptor_t gni_desc;
} mca_btl_ugni_post_descriptor_t;

OBJ_CLASS_DECLARATION(mca_btl_ugni_post_descriptor_t);

typedef struct mca_btl_ugni_rdma_desc_t {
    opal_free_list_item_t super;
    mca_btl_ugni_post_descriptor_t btl_ugni_desc;
    mca_btl_ugni_device_t *device;
    gni_ep_handle_t gni_handle;
    int tries;
} mca_btl_ugni_rdma_desc_t;

OBJ_CLASS_DECLARATION(mca_btl_ugni_rdma_desc_t);

#define MCA_BTL_UGNI_GNI_DESC_TO_RDMA_DESC(desc) \
    ((mca_btl_ugni_rdma_desc_t *) ((uintptr_t)(desc) - offsetof (mca_btl_ugni_rdma_desc_t, btl_ugni_desc) - offsetof (mca_btl_ugni_post_descriptor_t, gni_desc)))

/**
 * Initialize a RDMA descriptor
 *
 * @param[in]    item    free list item (must be of class mca_btl_ugni_rdma_desc_t)
 * @param[in]    ctx     pointer to ugni device context
 *
 * This function initializes a mca_btl_ugni_rdma_desc_t for use. It allocates
 * resources from the ugni library. This must be called before a RDMA
 * descriptor can be used. Usually this is passed as an argument to
 * opal_free_list_init().
 */
int mca_btl_ugni_rdma_desc_init (opal_free_list_item_t *item, void *ctx);

/**
 * @brief get an endpoint handle from a device's free list
 *
 * @param[in] ep      btl endpoint
 * @param[in] device  btl device to use
 * @param[in] use_bte whether this descriptor will be used with the BTE
 *
 * This function MUST be called with the device lock held. This was done over using
 * the atomic free list to avoid unnecessary atomics in the critical path.
 */
static inline mca_btl_ugni_rdma_desc_t *
mca_btl_ugni_alloc_rdma_desc (mca_btl_ugni_device_t *device, mca_btl_ugni_post_descriptor_t *ugni_desc, const bool use_bte)
{
    mca_btl_ugni_rdma_desc_t *desc = (mca_btl_ugni_rdma_desc_t *) opal_free_list_get_st (&device->rdma_descs);
    mca_btl_ugni_endpoint_t *ep = ugni_desc->endpoint;
    gni_return_t grc;

    if (OPAL_LIKELY(NULL != desc)) {
        grc = GNI_EpBind (desc->gni_handle, ep->ep_rem_addr, ep->ep_rem_id | device->dev_index);
        if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc)) {
            opal_free_list_return_st (&device->rdma_descs, &desc->super);
            return NULL;
        }

        desc->device = device;
        desc->tries = 0;
        desc->btl_ugni_desc = *ugni_desc;
        desc->btl_ugni_desc.use_bte = use_bte;
    }

    return desc;
}

static inline void mca_btl_ugni_return_rdma_desc (mca_btl_ugni_rdma_desc_t *desc)
{
    (void) GNI_EpUnbind (desc->gni_handle);
    opal_free_list_return_st (&desc->device->rdma_descs, &desc->super);
}

static inline void mca_btl_ugni_post_desc_complete (mca_btl_ugni_module_t *module, mca_btl_ugni_post_descriptor_t *desc, int rc)
{
    BTL_VERBOSE(("RDMA/FMA/ATOMIC operation complete for post descriptor %p. rc = %d", (void *) desc, rc));

    if (NULL != desc->cbfunc) {
        /* call the user's callback function */
        desc->cbfunc (&module->super, desc->endpoint, (void *)(intptr_t) desc->gni_desc.local_addr,
                      desc->local_handle, desc->ctx, desc->cbdata, rc);
    }
}

OBJ_CLASS_DECLARATION(mca_btl_ugni_smsg_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_ugni_rdma_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_ugni_eager_frag_t);

int mca_btl_ugni_frag_init (mca_btl_ugni_base_frag_t *frag, void *id);

static inline mca_btl_ugni_base_frag_t *mca_btl_ugni_frag_alloc (mca_btl_base_endpoint_t *ep,
                                                                 opal_free_list_t *list)
{
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) opal_free_list_get (list);
    if (OPAL_LIKELY(NULL != frag)) {
        frag->endpoint = ep;
        frag->ref_cnt = 1;
    }

    return frag;
}

static inline int mca_btl_ugni_frag_return (mca_btl_ugni_base_frag_t *frag)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (frag->endpoint);
    if (frag->registration) {
        ugni_module->rcache->rcache_deregister (ugni_module->rcache,
                                                (mca_rcache_base_registration_t *) frag->registration);
        frag->registration = NULL;
    }

    frag->flags = 0;

    opal_free_list_return (frag->my_list, (opal_free_list_item_t *) frag);

    return OPAL_SUCCESS;
}

static inline bool mca_btl_ugni_frag_del_ref (mca_btl_ugni_base_frag_t *frag, int rc) {
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (frag->endpoint);
    /* save the descriptor flags since the callback is allowed to free the frag */
    int des_flags = frag->base.des_flags;
    int32_t ref_cnt;

    opal_atomic_mb ();

    ref_cnt = OPAL_THREAD_ADD_FETCH32(&frag->ref_cnt, -1);
    if (ref_cnt) {
        assert (ref_cnt > 0);
        return false;
    }

    /* call callback if specified */
    if (des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
        frag->base.des_cbfunc(&ugni_module->super, frag->endpoint, &frag->base, rc);
    }

    if (des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
        mca_btl_ugni_frag_return (frag);
    }

    return true;
}

static inline void mca_btl_ugni_frag_complete (mca_btl_ugni_base_frag_t *frag, int rc) {
    BTL_VERBOSE(("frag complete. flags = %d", frag->base.des_flags));

    frag->flags |= MCA_BTL_UGNI_FRAG_COMPLETE;

    mca_btl_ugni_frag_del_ref (frag, rc);
}

static inline bool mca_btl_ugni_frag_check_complete (mca_btl_ugni_base_frag_t *frag) {
    return !!(MCA_BTL_UGNI_FRAG_COMPLETE & frag->flags);
}


void mca_btl_ugni_wait_list_append (mca_btl_ugni_module_t *ugni_module, mca_btl_base_endpoint_t *endpoint,
                                    mca_btl_ugni_base_frag_t *frag);

static inline mca_btl_ugni_base_frag_t *mca_btl_ugni_frag_alloc_smsg (mca_btl_base_endpoint_t *ep)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    return mca_btl_ugni_frag_alloc (ep, ugni_module->frags_lists + MCA_BTL_UGNI_LIST_SMSG);
}

static inline mca_btl_ugni_base_frag_t *mca_btl_ugni_frag_alloc_rdma (mca_btl_base_endpoint_t *ep)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    return mca_btl_ugni_frag_alloc (ep, ugni_module->frags_lists + MCA_BTL_UGNI_LIST_RDMA);
}

static inline mca_btl_ugni_base_frag_t *mca_btl_ugni_frag_alloc_rdma_int (mca_btl_base_endpoint_t *ep)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    return mca_btl_ugni_frag_alloc (ep, ugni_module->frags_lists + MCA_BTL_UGNI_LIST_RDMA_INT);
}

static inline mca_btl_ugni_base_frag_t *mca_btl_ugni_frag_alloc_eager_send (mca_btl_base_endpoint_t *ep)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    return mca_btl_ugni_frag_alloc (ep, ugni_module->frags_lists + MCA_BTL_UGNI_LIST_EAGER_SEND);
}

static inline mca_btl_ugni_base_frag_t *mca_btl_ugni_frag_alloc_eager_recv (mca_btl_base_endpoint_t *ep)
{
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_ep_btl (ep);
    return mca_btl_ugni_frag_alloc (ep, ugni_module->frags_lists + MCA_BTL_UGNI_LIST_EAGER_RECV);
}

#endif /* MCA_BTL_UGNI_FRAG_H */
