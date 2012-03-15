/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_rdma.h"
#include "btl_ugni_smsg.h"

#include "opal/include/opal/align.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/memoryhooks/memory.h"
#include "ompi/runtime/params.h"

static int btl_ugni_component_register(void);
static int btl_ugni_component_open(void);
static int btl_ugni_component_close(void);
static mca_btl_base_module_t **mca_btl_ugni_component_init(int *, bool, bool);
static int mca_btl_ugni_component_progress(void);

mca_btl_ugni_component_t mca_btl_ugni_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            MCA_BTL_BASE_VERSION_2_0_0,

            "ugni",                       /* MCA component name */
            OMPI_MAJOR_VERSION,           /* MCA component major version */
            OMPI_MINOR_VERSION,           /* MCA component minor version */
            OMPI_RELEASE_VERSION,         /* MCA component release version */
            btl_ugni_component_open,      /* component open */
            btl_ugni_component_close,     /* component close */
            NULL,                         /* component query */
            btl_ugni_component_register,  /* component register */
        },
        {
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        mca_btl_ugni_component_init,
        mca_btl_ugni_component_progress,
    }
};

static inline int
mca_btl_ugni_param_register_int (const char *param_name, const char *help,
                                 int value)
{
    mca_base_param_reg_int(&mca_btl_ugni_component.super.btl_version,
                           param_name, help, false, false, value, &value);
    return value;
}

static int
btl_ugni_component_register(void)
{
    mca_btl_ugni_component.ugni_free_list_num =
        mca_btl_ugni_param_register_int("free_list_num", NULL, 8);
    mca_btl_ugni_component.ugni_free_list_max =
        mca_btl_ugni_param_register_int("free_list_max", NULL, 16384);
    mca_btl_ugni_component.ugni_free_list_inc =
        mca_btl_ugni_param_register_int("free_list_inc", NULL, 64);

    mca_btl_ugni_component.ugni_eager_num =
        mca_btl_ugni_param_register_int("eager_num", NULL, 16);
    mca_btl_ugni_component.ugni_eager_max =
        mca_btl_ugni_param_register_int("eager_max", NULL, 64);
    mca_btl_ugni_component.ugni_eager_inc =
        mca_btl_ugni_param_register_int("eager_inc", NULL, 16);
    

    mca_btl_ugni_component.cq_size =
        mca_btl_ugni_param_register_int("cq_size", NULL, 25000);

    /* SMSG limit. 0 - autoselect */
    mca_btl_ugni_component.ugni_smsg_limit =
        mca_btl_ugni_param_register_int("smsg_limit", "Maximum size message that "
                                        "will be sent using the SMSG/MSGQ protocol "
                                        "(0 - autoselect(default))", 0);

    mca_btl_ugni_component.smsg_max_credits =
        mca_btl_ugni_param_register_int("smsg_max_credits", "Maximum number of "
                                        "outstanding SMSG/MSGQ message (default 32)",
                                        32);

    mca_btl_ugni_component.ugni_fma_limit =
        mca_btl_ugni_param_register_int("fma_limit", "Maximum size message that "
                                        "will be sent using the FMA (Fast Memory "
                                        "Access) protocol (default 4095)",
                                        4 * 1024 - 1);

    mca_btl_ugni_component.ugni_get_limit =
        mca_btl_ugni_param_register_int("get_limit", NULL, 512 * 1024);

    mca_btl_ugni_component.rdma_max_retries =
        mca_btl_ugni_param_register_int("rdma_max_retries", NULL, 8);

    mca_btl_ugni_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;

    /* smsg threshold */
    mca_btl_ugni_module.super.btl_eager_limit               = 8 * 1024;
    mca_btl_ugni_module.super.btl_rndv_eager_limit          = 8 * 1024;
    mca_btl_ugni_module.super.btl_rdma_pipeline_frag_size   = 2 * 1024 * 1024;
    mca_btl_ugni_module.super.btl_max_send_size             = 8 * 1024;
    mca_btl_ugni_module.super.btl_rdma_pipeline_send_length = 8 * 1024;

    /* threshold for put */
    mca_btl_ugni_module.super.btl_min_rdma_pipeline_size    = 8 * 1024;

    mca_btl_ugni_module.super.btl_flags = MCA_BTL_FLAGS_SEND |
        MCA_BTL_FLAGS_RDMA;

    mca_btl_ugni_module.super.btl_bandwidth = 40000; /* Mbs */
    mca_btl_ugni_module.super.btl_latency   = 2;     /* Microsecs */

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_ugni_component.super.btl_version,
                                &mca_btl_ugni_module.super);
    return OMPI_SUCCESS;
}

static int
btl_ugni_component_open(void)
{
    mca_btl_ugni_component.ugni_num_btls = 0;
    mca_btl_ugni_component.modules = NULL;

    OBJ_CONSTRUCT(&mca_btl_ugni_component.ugni_frags_smsg, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_ugni_component.ugni_frags_rdma, ompi_free_list_t);

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */
static int
btl_ugni_component_close(void)
{
    ompi_common_ugni_fini ();

    OBJ_DESTRUCT(&mca_btl_ugni_component.ugni_frags_smsg);
    OBJ_DESTRUCT(&mca_btl_ugni_component.ugni_frags_rdma);

    return OMPI_SUCCESS;
}

static void mca_btl_ugni_autoset_leave_pinned (void) {
    mca_base_param_source_t source;
    int index, rc, value;

    /* If we have a memory manager available, and
       mpi_leave_pinned==-1, then unless the user explicitly set
       mpi_leave_pinned_pipeline==0, then set mpi_leave_pinned to 1.
       We have a memory manager if we have both FREE and MUNMAP
       support */
    value = opal_mem_hooks_support_level();
    if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & value)) {
        rc = 0;
        index = mca_base_param_find("mpi", NULL, "leave_pinned");
        if (index >= 0) {
            if (OPAL_SUCCESS == mca_base_param_lookup_int(index, &value) &&
                -1 == value) {
                ++rc;
            }
        }
        index = mca_base_param_find("mpi", NULL, "leave_pinned_pipeline");
        if (index >= 0) {
            if (OPAL_SUCCESS == mca_base_param_lookup_int(index, &value) &&
                OPAL_SUCCESS == mca_base_param_lookup_source(index, &source,
                                                             NULL)) {
                if (0 == value && MCA_BASE_PARAM_SOURCE_DEFAULT == source) {
                    ++rc;
                }
            }
        }
        /* If we were good on both parameters, then set leave_pinned=1 */
        if (2 == rc) {
            ompi_mpi_leave_pinned = 1;
            ompi_mpi_leave_pinned_pipeline = 0;
        }
    }
}

static int mca_btl_ugni_smsg_setup (void) {
    gni_smsg_attr_t tmp_smsg_attrib;
    unsigned int mbox_size;
    int rc;

    /* calculate mailbox size */
    tmp_smsg_attrib.msg_type       = GNI_SMSG_TYPE_MBOX_AUTO_RETRANSMIT;
    tmp_smsg_attrib.msg_maxsize    = mca_btl_ugni_component.ugni_smsg_limit;
    tmp_smsg_attrib.mbox_maxcredit = mca_btl_ugni_component.smsg_max_credits;

    rc = GNI_SmsgBufferSizeNeeded (&tmp_smsg_attrib, &mbox_size);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        BTL_ERROR(("error in GNI_SmsgBufferSizeNeeded"));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    mca_btl_ugni_component.smsg_mbox_size = OPAL_ALIGN(mbox_size, 64, unsigned int);

    return OMPI_SUCCESS;
}

static mca_btl_base_module_t **
mca_btl_ugni_component_init (int *num_btl_modules,
                             bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    struct mca_btl_base_module_t **base_modules;
    mca_btl_ugni_module_t *ugni_modules;
    unsigned int i;
    size_t nprocs;
    int rc;

    /* Initialize ugni library and create communication domain */
    rc = ompi_common_ugni_init();
    if (OMPI_SUCCESS != rc) {
        return NULL;
    }

    /* Create and initialize modules
     * Create one module per device
     * One btl == One module
     */
    /* Manju: I should set this automatically, not hardcoded */
    mca_btl_ugni_component.ugni_num_btls = ompi_common_ugni_module.device_count;

    BTL_VERBOSE(("btl/ugni initializing"));

    ugni_modules = mca_btl_ugni_component.modules = (mca_btl_ugni_module_t *)
        calloc (mca_btl_ugni_component.ugni_num_btls,
                sizeof (mca_btl_ugni_module_t));

    if (OPAL_UNLIKELY(NULL == mca_btl_ugni_component.modules)) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        return NULL;
    }

    base_modules = (struct mca_btl_base_module_t **)
        calloc (mca_btl_ugni_component.ugni_num_btls,
                sizeof (struct mca_btl_base_module_t *));
    if (OPAL_UNLIKELY(NULL == base_modules)) {
        BTL_ERROR(("Malloc failed : %s:%d", __FILE__, __LINE__));
        return NULL;
    }

    mca_btl_ugni_autoset_leave_pinned ();

    (void) ompi_proc_world (&nprocs);

    if (0 == mca_btl_ugni_component.ugni_smsg_limit) {
        /* auto-set the smsg limit based on the number of ranks */
        if (nprocs <= 1024) {
            mca_btl_ugni_component.ugni_smsg_limit = 1024;
        } else if (nprocs <= 16384) {
            mca_btl_ugni_component.ugni_smsg_limit = 512;
        } else {
            mca_btl_ugni_component.ugni_smsg_limit = 256;
        }
    }

    mca_btl_ugni_component.smsg_max_data = mca_btl_ugni_component.ugni_smsg_limit -
        sizeof (mca_btl_ugni_send_frag_hdr_t);

    /* module settings */
    mca_btl_ugni_module.super.btl_rdma_pipeline_send_length = mca_btl_ugni_module.super.btl_eager_limit;

    rc = mca_btl_ugni_smsg_setup ();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return NULL;
    }

    for (i = 0 ; i < mca_btl_ugni_component.ugni_num_btls ; ++i) {
        mca_btl_ugni_module_t *ugni_module = ugni_modules + i;

        rc = mca_btl_ugni_module_init (ugni_module,
                                       ompi_common_ugni_module.devices + i);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            BTL_ERROR(("Failed to initialize uGNI module @ %s:%d", __FILE__,
                       __LINE__));
            return NULL;
        }

        base_modules[i] = (mca_btl_base_module_t *) ugni_module;
    }

    *num_btl_modules = mca_btl_ugni_component.ugni_num_btls;

    BTL_VERBOSE(("btl/ugni done initializing modules"));

    return base_modules;
}

static void mca_btl_ugni_callback_reverse_get (ompi_common_ugni_post_desc_t *desc, int rc)
{
    mca_btl_ugni_base_frag_t *frag = MCA_BTL_UGNI_DESC_TO_FRAG(desc);

    BTL_VERBOSE(("reverse get (put) for rem_ctx %p complete", frag->hdr.rdma.ctx));

    /* tell peer the put is complete */
    rc = ompi_mca_btl_ugni_smsg_send (frag, false, &frag->hdr.rdma, sizeof (frag->hdr.rdma),
                                      NULL, 0, MCA_BTL_UGNI_TAG_PUT_COMPLETE);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        opal_list_append (&frag->endpoint->btl->failed_frags, (opal_list_item_t *) frag);
    }
}

static void mca_btl_ugni_callback_eager_get (ompi_common_ugni_post_desc_t *desc, int rc)
{
    mca_btl_ugni_base_frag_t *frag = MCA_BTL_UGNI_DESC_TO_FRAG(desc);
    mca_btl_active_message_callback_t *reg;

    BTL_VERBOSE(("eager get for rem_ctx %p complete", frag->hdr.eager.ctx));

    /* the frag is already set up for the send callback */
    frag->segments[0].seg_len = frag->hdr.eager.len;
    reg = mca_btl_base_active_message_trigger + frag->hdr.eager.tag;
    reg->cbfunc(&frag->endpoint->btl->super, frag->hdr.eager.tag, &(frag->base), reg->cbdata);

    /* tell peer the get is complete */
    rc = ompi_mca_btl_ugni_smsg_send (frag, false, &frag->hdr.eager, sizeof (frag->hdr.eager),
                                      NULL, 0, MCA_BTL_UGNI_TAG_GET_COMPLETE);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        opal_list_append (&frag->endpoint->btl->failed_frags, (opal_list_item_t *) frag);
    }
}

static inline int mca_btl_ugni_start_reverse_get (mca_btl_base_endpoint_t *ep,
                                                  mca_btl_ugni_rdma_frag_hdr_t hdr)
{
    mca_btl_ugni_base_frag_t *frag;
    int rc;

    BTL_VERBOSE(("starting reverse get (put) for remote ctx: %p", hdr.ctx));

    rc = MCA_BTL_UGNI_FRAG_ALLOC_RDMA(ep, frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        BTL_ERROR(("error allocating rdma frag for reverse get"));
        return rc;
    }

    frag->hdr.rdma = hdr;

    frag->base.des_cbfunc = NULL;
    frag->base.des_flags  = MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

    frag->segments[0] = hdr.src_seg;
    frag->base.des_src = frag->segments;
    frag->base.des_src_cnt = 1;

    frag->segments[1] = hdr.dst_seg;
    frag->base.des_dst = frag->segments + 1;
    frag->base.des_dst_cnt = 1;

    rc = mca_btl_ugni_put (&ep->btl->super, ep, &frag->base);
    assert (OMPI_SUCCESS == rc);

    frag->post_desc.cbfunc = mca_btl_ugni_callback_reverse_get;

    return rc;
}

static inline int mca_btl_ugni_start_eager_get (mca_btl_base_endpoint_t *ep,
                                                mca_btl_ugni_eager_frag_hdr_t hdr);

static void mca_btl_ugni_callback_eager_get_retry (ompi_common_ugni_post_desc_t *desc, int rc)
{
    mca_btl_ugni_base_frag_t *frag = MCA_BTL_UGNI_DESC_TO_FRAG(desc);

    (void) mca_btl_ugni_start_eager_get(frag->endpoint, frag->hdr.eager);

    mca_btl_ugni_frag_return (frag);
}


static inline int mca_btl_ugni_start_eager_get (mca_btl_base_endpoint_t *ep,
                                                mca_btl_ugni_eager_frag_hdr_t hdr)
{
    mca_btl_ugni_base_frag_t *frag;
    int rc;

    BTL_VERBOSE(("starting eager get for remote ctx: %p", hdr.ctx));

    rc = MCA_BTL_UGNI_FRAG_ALLOC_EAGER_RECV(ep, frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        (void) MCA_BTL_UGNI_FRAG_ALLOC_RDMA(ep, frag);
        assert (NULL != frag);

        frag->hdr.eager = hdr;
        frag->post_desc.cbfunc = mca_btl_ugni_callback_eager_get_retry;

        opal_list_append (&ep->btl->failed_frags, (opal_list_item_t *) frag);
        return rc;
    }

    frag->hdr.eager = hdr;

    frag->base.des_cbfunc = NULL;
    frag->base.des_flags  = MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;

    frag->base.des_dst = frag->segments;
    frag->base.des_dst_cnt = 1;

    frag->segments[1] = hdr.src_seg;
    frag->base.des_src = frag->segments + 1;
    frag->base.des_src_cnt = 1;

    /* increase size to a multiple of 4 bytes (required for get) */
    frag->segments[0].seg_len = (hdr.len + 3) & ~3;
    frag->segments[1].seg_len = (hdr.len + 3) & ~3;

    if (frag->segments[0].seg_len <= mca_btl_ugni_component.ugni_fma_limit) {
        rc = mca_btl_ugni_post_fma (frag, GNI_POST_FMA_GET, frag->base.des_dst, frag->base.des_src);
    } else {
        rc = mca_btl_ugni_post_bte (frag, GNI_POST_RDMA_GET, frag->base.des_dst, frag->base.des_src);
    }
    assert (OMPI_SUCCESS == rc);

    frag->post_desc.cbfunc = mca_btl_ugni_callback_eager_get;

    return rc;
}

static inline int
mca_btl_ugni_smsg_process (mca_btl_base_endpoint_t *ep)
{
    mca_btl_active_message_callback_t *reg;
    mca_btl_ugni_base_frag_t frag;
    bool disconnect = false;
    uintptr_t data_ptr;
    gni_return_t rc;
    int count = 0;

    /* per uGNI documentation we loop until the mailbox is empty */
    do {
        uint8_t tag = GNI_SMSG_ANY_TAG;

        rc = GNI_SmsgGetNextWTag (ep->common->ep_handle, (void **) &data_ptr, &tag);
        if (GNI_RC_NOT_DONE == rc) {
            BTL_VERBOSE(("no smsg message waiting. rc = %d", rc));

            return count;
        }

        if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
            fprintf (stderr, "Unhandled Smsg error: %d\n", rc);
            assert (0);
            return OMPI_ERROR;
        }

        if (OPAL_UNLIKELY(0 == data_ptr)) {
            BTL_ERROR(("null data ptr!"));
            assert (0);
            return OMPI_ERROR;
        }

        count++;

        BTL_VERBOSE(("got smsg fragment. tag = %d\n", tag));

        switch (tag) {
        case MCA_BTL_UGNI_TAG_SEND:
            frag.hdr.send = ((mca_btl_ugni_send_frag_hdr_t *) data_ptr)[0];

            BTL_VERBOSE(("received smsg fragment. hdr = {len = %u, tag = %d}",
                         (unsigned int) frag.hdr.send.len, frag.hdr.send.tag));

            reg = mca_btl_base_active_message_trigger + frag.hdr.send.tag;
            frag.base.des_dst     = frag.segments;
            frag.base.des_dst_cnt = 1;

            frag.segments[0].seg_addr.pval = (void *)((uintptr_t)data_ptr + sizeof (mca_btl_ugni_send_frag_hdr_t));
            frag.segments[0].seg_len       = frag.hdr.send.len;

            reg->cbfunc(&ep->btl->super, frag.hdr.send.tag, &(frag.base), reg->cbdata);

            break;
        case MCA_BTL_UGNI_TAG_PUT_INIT:
            frag.hdr.rdma = ((mca_btl_ugni_rdma_frag_hdr_t *) data_ptr)[0];

            mca_btl_ugni_start_reverse_get (ep, frag.hdr.rdma);
            break;
        case MCA_BTL_UGNI_TAG_PUT_COMPLETE:
            frag.hdr.rdma = ((mca_btl_ugni_rdma_frag_hdr_t *) data_ptr)[0];

            mca_btl_ugni_post_frag_complete (frag.hdr.rdma.ctx, OMPI_SUCCESS);
            break;
        case MCA_BTL_UGNI_TAG_GET_INIT:
            frag.hdr.eager = ((mca_btl_ugni_eager_frag_hdr_t *) data_ptr)[0];

            mca_btl_ugni_start_eager_get (ep, frag.hdr.eager);
            break;
        case MCA_BTL_UGNI_TAG_GET_COMPLETE:
            frag.hdr.eager = ((mca_btl_ugni_eager_frag_hdr_t *) data_ptr)[0];

            mca_btl_ugni_post_frag_complete (frag.hdr.eager.ctx, OMPI_SUCCESS);
            break;
        case MCA_BTL_UGNI_TAG_DISCONNECT:
            /* remote endpoint has disconnected */
            disconnect = true;
            break;
        default:
            BTL_ERROR(("unknown tag %d\n", tag));
            break;
        }

        rc = GNI_SmsgRelease (ep->common->ep_handle);
        if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
            BTL_ERROR(("Smsg release failed!"));
            return OMPI_ERROR;
        }
    } while (!disconnect);

    /* disconnect if we get here */
    mca_btl_ugni_ep_disconnect (ep, false);

    return count;
}

static inline int
mca_btl_ugni_progress_datagram (mca_btl_ugni_module_t *btl)
{
    uint32_t remote_addr, remote_id;
    mca_btl_base_endpoint_t *ep;
    gni_post_state_t post_state;
    gni_ep_handle_t handle;
    uint64_t datagram_id;
    gni_return_t grc;
    int count = 0;

    /* check for datagram completion */
    grc = GNI_PostDataProbeById (btl->device->dev_handle, &datagram_id);
    if (OPAL_LIKELY(GNI_RC_SUCCESS != grc)) {
        return 0;
    }

    if ((datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK) ==
        MCA_BTL_UGNI_CONNECT_WILDCARD_ID) {
        handle = btl->wildcard_ep;
    } else {
        handle =
            btl->endpoints[(uint32_t)(datagram_id & 0xffffffffull)]->common->ep_handle;
    }

    /* wait for the incoming datagram to complete (in case it isn't) */
    grc = GNI_EpPostDataWaitById (handle, datagram_id, -1, &post_state,
                                 &remote_addr, &remote_id);
    if (GNI_RC_SUCCESS != grc) {
        BTL_ERROR(("GNI_EpPostDataWaitById failed with rc = %d", grc));
        return ompi_common_rc_ugni_to_ompi (grc);
    }

    BTL_VERBOSE(("got a datagram completion: id = %" PRIx64 ", state = %d, "
                 "peer = %d", datagram_id, post_state, remote_id));

    ep = btl->endpoints[remote_id];

    OPAL_THREAD_LOCK(&ep->common->lock);

    /* NTH: TODO -- error handling */
    (void) mca_btl_ugni_ep_connect_progress (ep);

    if (OMPI_COMMON_UGNI_CONNECTED == MCA_BTL_UGNI_EP_STATE(ep)) {
        /*  process messages waiting in the endpoint's smsg mailbox */
        count = mca_btl_ugni_smsg_process (ep);
    }

    OPAL_THREAD_UNLOCK(&ep->common->lock);

    if ((datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK) ==
        MCA_BTL_UGNI_CONNECT_WILDCARD_ID) {
        mca_btl_ugni_wildcard_ep_post (btl);
    }

    return count;
}

static inline int
mca_btl_ugni_handle_smsg_overrun (mca_btl_ugni_module_t *btl)
{
    gni_cq_entry_t event_data;
    unsigned int ep_index;
    int count, rc;

    BTL_VERBOSE(("btl/ugni_component detect SMSG CQ overrun. "
                 "processing message backlog..."));

    /* we don't know which endpoint lost an smsg completion. clear the
       smsg remote cq and check all mailboxes */

    /* clear out remote cq */
    do {
        rc = GNI_CqGetEvent (btl->smsg_remote_cq, &event_data);
    } while (GNI_RC_SUCCESS == rc);

    count = 0;

    for (ep_index = 0 ; ep_index < btl->endpoint_count ; ++ep_index) {
        mca_btl_base_endpoint_t *ep = btl->endpoints[ep_index];

        if (NULL == ep || OMPI_COMMON_UGNI_CONNECTED != MCA_BTL_UGNI_EP_STATE(ep)) {
            continue;
        }

        do {
            /* clear out smsg mailbox */
            rc = mca_btl_ugni_smsg_process (ep);
            if (rc > 0)
                count += rc;
        } while (rc > 0);
    }

    return count;
}

static inline int
mca_btl_ugni_progress_smsg (mca_btl_ugni_module_t *btl)
{
    mca_btl_base_endpoint_t *ep;
    gni_cq_entry_t event_data;
    int rc;

    rc = GNI_CqGetEvent (btl->smsg_remote_cq, &event_data);
    if (GNI_RC_NOT_DONE == rc) {
        return 0;
    }

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc || !GNI_CQ_STATUS_OK(event_data) ||
                      GNI_CQ_OVERRUN(event_data))) {
        if (GNI_RC_ERROR_RESOURCE == rc ||
            (GNI_RC_SUCCESS == rc && GNI_CQ_OVERRUN(event_data))) {
            /* recover from smsg cq overrun */
            return mca_btl_ugni_handle_smsg_overrun (btl);
        }

        BTL_ERROR(("unhandled error in GNI_CqGetEvent"));

        /* unhandled error: crash */
        assert (0);
        return OMPI_ERROR;
    }

    BTL_VERBOSE(("REMOTE CQ: Got event 0x%" PRIx64 ". msg id = %" PRIu64
                 ". ok = %d, type = %" PRIu64 "\n", (uint64_t) event_data,
                 GNI_CQ_GET_MSG_ID(event_data), GNI_CQ_STATUS_OK(event_data),
                 GNI_CQ_GET_TYPE(event_data)));

    /* we could check the message type here but it seems to always be a POST */

    ep = btl->endpoints[GNI_CQ_GET_MSG_ID(event_data)];
    if (OPAL_UNLIKELY(OMPI_COMMON_UGNI_CONNECTED != MCA_BTL_UGNI_EP_STATE(ep))) {
        /* due to the nature of datagrams we may get a smsg completion before
           we get mailbox info from the peer */
        BTL_VERBOSE(("event occurred on an unconnected endpoint! ep state = %d", MCA_BTL_UGNI_EP_STATE(ep)));
        return 0;
    }

    return mca_btl_ugni_smsg_process (ep);
}

static inline int
mca_btl_ugni_progress_bte (mca_btl_ugni_module_t *btl)
{
    return ompi_common_ugni_process_completed_post (btl->device, btl->bte_local_cq);
}

static int
mca_btl_ugni_retry_failed (mca_btl_ugni_module_t *btl)
{
    int count = opal_list_get_size (&btl->failed_frags);
    opal_list_item_t *item;

    while (count-- && NULL != (item = opal_list_remove_first (&btl->failed_frags))) {
        mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) item;

        frag->post_desc.cbfunc (&frag->post_desc, OMPI_SUCCESS);
    }

    return 0;
}

static int
mca_btl_ugni_component_progress (void)
{
    mca_btl_ugni_module_t *btl;
    unsigned int i;
    int count;

    count = ompi_common_ugni_progress ();

    for (i = 0 ; i < mca_btl_ugni_component.ugni_num_btls ; ++i) {
        btl = mca_btl_ugni_component.modules + i;

        mca_btl_ugni_retry_failed (btl);

        count += mca_btl_ugni_progress_datagram (btl);
        count += mca_btl_ugni_progress_smsg (btl);
        count += mca_btl_ugni_progress_bte (btl);
    }

    return count;
}
