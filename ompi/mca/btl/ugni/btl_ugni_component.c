/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni.h"
#include "btl_ugni_endpoint.h"
#include "btl_ugni_frag.h"
#include "btl_ugni_rdma.h"

#include "opal/include/opal/align.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/memoryhooks/memory.h"
#include "ompi/runtime/params.h"

int mca_btl_ugni_smsg_max_credits = 32;
int mca_btl_ugni_smsg_mbox_size;

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

static inline char *
mca_btl_ugni_param_register_string(const char *param_name,
                                   const char *default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl", "ugni", param_name, NULL,
                                            default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int
mca_btl_ugni_param_register_int (const char *param_name, int value)
{
    int id = mca_base_param_register_int("btl", "ugni", param_name, NULL, value);
    mca_base_param_lookup_int(id, &value);
    return value;
}

static int
btl_ugni_component_register(void)
{
    mca_btl_ugni_component.ugni_free_list_num =
        mca_btl_ugni_param_register_int("free_list_num", 8);
    mca_btl_ugni_component.ugni_free_list_max =
        mca_btl_ugni_param_register_int("free_list_max", -1);
    mca_btl_ugni_component.ugni_free_list_inc =
        mca_btl_ugni_param_register_int("free_list_inc", 64);

    mca_btl_ugni_component.cq_size =
        mca_btl_ugni_param_register_int("cq_size", 25000);

    mca_btl_ugni_component.btl_fma_limit =
        mca_btl_ugni_param_register_int("fma_limit", 4 * 1024);

    mca_btl_ugni_component.btl_get_limit =
        mca_btl_ugni_param_register_int("get_limit", 8 * 1024);

    mca_btl_ugni_component.rdma_max_retries =
        mca_btl_ugni_param_register_int("rdma_max_retries", 8);

    mca_btl_ugni_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;

    /* smsg threshold */
    mca_btl_ugni_module.super.btl_eager_limit               = 0; /* set dynamically in module_init */
    mca_btl_ugni_module.super.btl_rndv_eager_limit          = 8 * 1024;
    mca_btl_ugni_module.super.btl_rdma_pipeline_frag_size   = 2 * 1024 * 1024;
    mca_btl_ugni_module.super.btl_max_send_size             = 0; /* set this later */
    mca_btl_ugni_module.super.btl_rdma_pipeline_send_length = 0; /* set this later */

    /* threshold for put */
    mca_btl_ugni_module.super.btl_min_rdma_pipeline_size    = 0;

    mca_btl_ugni_module.super.btl_flags = MCA_BTL_FLAGS_SEND |
                                          MCA_BTL_FLAGS_RDMA |
                                          MCA_BTL_FLAGS_RDMA_MATCHED;

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

    OBJ_CONSTRUCT(&mca_btl_ugni_component.ugni_frags_eager, ompi_free_list_t);
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

    OBJ_DESTRUCT(&mca_btl_ugni_component.ugni_frags_eager);
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
    tmp_smsg_attrib.msg_maxsize    = mca_btl_ugni_component.eager_limit + sizeof (mca_btl_ugni_frag_hdr_t);
    tmp_smsg_attrib.mbox_maxcredit = mca_btl_ugni_smsg_max_credits;

    rc = GNI_SmsgBufferSizeNeeded (&tmp_smsg_attrib, &mbox_size);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        BTL_ERROR(("error in GNI_SmsgBufferSizeNeeded"));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    /* NTH: increase the mbox size to a multiple of 4096. ugni examples increase to a multiple of 
       64 bytes but we see hangs with anything less than 4096. */
    mca_btl_ugni_smsg_mbox_size = OPAL_ALIGN(mbox_size, 4096, unsigned int);

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

    if (0 == mca_btl_ugni_component.eager_limit) {
        /* auto-set the eager limit based on the number of ranks */
        if (nprocs <= 1024) {
            mca_btl_ugni_component.eager_limit = 1024;
        } else if (nprocs <= 16384) {
            mca_btl_ugni_component.eager_limit = 512;
        } else {
            mca_btl_ugni_component.eager_limit = 256;
        }
    }

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

static inline void mca_btl_ugni_callback_reverse_get (mca_btl_base_module_t *btl,
                                                      mca_btl_base_endpoint_t *ep,
                                                      mca_btl_base_descriptor_t *des,
                                                      int rc)
{
    mca_btl_ugni_module_t *ugni_module = (mca_btl_ugni_module_t *) btl;
    mca_btl_ugni_base_frag_t *frag = (mca_btl_ugni_base_frag_t *) des;
    uint32_t msg_id = ORTE_PROC_MY_NAME->vpid;

    BTL_VERBOSE(("reverse get (put) for rem_ctx %p complete", des->des_cbdata));

    /* tell peer the put is complete */
    rc = GNI_SmsgSendWTag (frag->endpoint->common->ep_handle, &des->des_cbdata, sizeof (void *),
                           NULL, 0, msg_id, MCA_BTL_UGNI_TAG_PUT_COMPLETE);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        /* turn off btl ownership for now */
        des->des_flags &= ~MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;
        opal_list_append (&ugni_module->failed_frags, (opal_list_item_t *) des);
    } else {
        des->des_flags |= MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;
    }
}

static inline int mca_btl_ugni_start_progress_reverse_get (mca_btl_base_endpoint_t *ep,
                                                           mca_btl_base_segment_t *segments,
                                                           void *rem_ctx)
{
    mca_btl_ugni_base_frag_t *frag;
    int rc;

    BTL_VERBOSE(("starting reverse get (put) for remote ctx: %p", rem_ctx));

    MCA_BTL_UGNI_FRAG_ALLOC_RDMA(ep->btl, frag, rc);
    if (OPAL_UNLIKELY(NULL == frag)) {
        BTL_ERROR(("error allocating rdma frag for reverse get"));
        return rc;
    }

    frag->base.des_cbfunc = mca_btl_ugni_callback_reverse_get;
    frag->base.des_cbdata = rem_ctx;
    frag->endpoint = ep;

    memmove (&frag->segments, segments, 2 * sizeof (segments[0]));

    frag->base.des_src = frag->segments;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = frag->segments + 1;
    frag->base.des_dst_cnt = 1;

    rc = mca_btl_ugni_put (&ep->btl->super, ep, &frag->base);
    assert (OMPI_SUCCESS == rc);

    return rc;
}

static inline int
mca_btl_ugni_smsg_process (mca_btl_base_endpoint_t *ep)
{
    mca_btl_active_message_callback_t *reg;
    mca_btl_ugni_base_frag_t frag;
    mca_btl_base_segment_t *segments;
    mca_btl_ugni_frag_hdr_t *hdr;
    uintptr_t data_ptr;
    int count = 0;
    int rc;

    /* loop until the mailbox is empty */
    do {
        uint8_t tag = GNI_SMSG_ANY_TAG;

        rc = GNI_SmsgGetNextWTag (ep->common->ep_handle, (void **) &data_ptr, &tag);
        if (GNI_RC_SUCCESS != rc) {
            BTL_VERBOSE(("no smsg message waiting. rc = %d", rc));
            break;
        }

        if (OPAL_UNLIKELY(0 == data_ptr)) {
            BTL_ERROR(("null data ptr!"));
            return OMPI_ERROR;
        }

        count++;

        BTL_VERBOSE(("got smsg fragment. tag = %d\n", tag));

        switch (tag) {
        case MCA_BTL_UGNI_TAG_SEND:
            hdr = (mca_btl_ugni_frag_hdr_t *) data_ptr;

            BTL_VERBOSE(("received smsg fragment. hdr = {len = %u, tag = %d}",
                         (unsigned int) hdr->len, hdr->tag));

            reg = mca_btl_base_active_message_trigger + hdr->tag;
            frag.base.des_dst     = frag.segments;
            frag.base.des_dst_cnt = 1;

            frag.segments[0].seg_addr.pval = (void *)(data_ptr + sizeof (*hdr));
            frag.segments[0].seg_len       = hdr->len;

            reg->cbfunc(&ep->btl->super, hdr->tag, &(frag.base), reg->cbdata);

            break;
        case MCA_BTL_UGNI_TAG_DISCONNECT:
            /* remote endpoint has disconnected */
            rc = GNI_SmsgRelease (ep->common->ep_handle);
            if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
                BTL_ERROR(("Smsg release failed!"));
                return OMPI_ERROR;
            }

            mca_btl_ugni_ep_disconnect (ep, false);

            return count;
        case MCA_BTL_UGNI_TAG_PUT_INIT:
            segments = (mca_btl_base_segment_t *) data_ptr;

            mca_btl_ugni_start_progress_reverse_get (ep, segments,
                                                     ((void **)(segments + 2))[0]);

            break;
        case MCA_BTL_UGNI_TAG_PUT_COMPLETE:
            mca_btl_ugni_post_frag_complete (((void **)data_ptr)[0], OMPI_SUCCESS);

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
    } while (1);

    /* finished processing events */
    return count;
}

static inline int
mca_btl_ugni_progress_datagram (mca_btl_ugni_module_t *btl)
{
    uint32_t remote_addr, remote_id;
    uint64_t datagram_id;
    mca_btl_base_endpoint_t *ep;
    gni_ep_handle_t handle;
    gni_post_state_t post_state;
    int rc, count;

    count = 0;

    post_state = GNI_POST_PENDING;
    rc = GNI_PostDataProbeById (btl->device->dev_handle, &datagram_id);
    if (OPAL_LIKELY(GNI_RC_SUCCESS != rc)) {
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
    rc = GNI_EpPostDataWaitById (handle, datagram_id, -1, &post_state,
                                 &remote_addr, &remote_id);
    if (GNI_RC_SUCCESS != rc) {
        BTL_ERROR(("GNI_EpPostDataWaitById failed with rc = %d", rc));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    BTL_VERBOSE(("got a datagram completion: id = %" PRIx64 ", state = %d, "
                 "peer = %d", datagram_id, post_state, remote_id));

    ep = btl->endpoints[remote_id];

    OPAL_THREAD_LOCK(&ep->common->lock);

    /* NTH: TODO -- error handling */
    (void) mca_btl_ugni_ep_connect_progress (ep);

    if (ep->smsgs_waiting && OMPI_COMMON_UGNI_CONNECTED == MCA_BTL_UGNI_EP_STATE(ep)) {
        /*  process messages waiting in the endpoint's smsg mailbox */
        while ((rc = mca_btl_ugni_smsg_process (ep) > 0)) count += rc;
        ep->smsgs_waiting = false;
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
       smsg cq and check all mailboxes */

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

        /* flag the endpoint as having messages waiting */
        ep->smsgs_waiting = true;
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
        mca_btl_ugni_post_frag_complete ((void *) item, OMPI_SUCCESS);
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
