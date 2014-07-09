/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
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

#include "opal/memoryhooks/memory.h"
#include "ompi/runtime/params.h"

#include "ompi/attribute/attribute.h"
#include "ompi/communicator/communicator.h"

static int btl_ugni_component_register(void);
static int btl_ugni_component_open(void);
static int btl_ugni_component_close(void);
static mca_btl_base_module_t **mca_btl_ugni_component_init(int *, bool, bool);
static int mca_btl_ugni_component_progress(void);

mca_btl_ugni_component_t mca_btl_ugni_component = {
    .super = {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        .btl_version = {
            MCA_BTL_BASE_VERSION_2_0_0,

            .mca_component_name = "ugni",
            .mca_component_major_version = OMPI_MAJOR_VERSION,
            .mca_component_minor_version = OMPI_MINOR_VERSION,
            .mca_component_release_version = OMPI_RELEASE_VERSION,
            .mca_open_component = btl_ugni_component_open,
            .mca_close_component = btl_ugni_component_close,
            .mca_register_component_params = btl_ugni_component_register,
        },
        .btl_data = {
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        .btl_init = mca_btl_ugni_component_init,
        .btl_progress = mca_btl_ugni_component_progress,
    }
};

mca_base_var_enum_value_t mpool_values[] = {
    {MCA_BTL_UGNI_MPOOL_UDREG, "udreg"},
    {MCA_BTL_UGNI_MPOOL_GRDMA, "grdma"},
    {-1, NULL} /* sentinal */
};

static int
btl_ugni_component_register(void)
{
    mca_base_var_enum_t *new_enum;
    int rc;

    (void) mca_base_var_group_component_register(&mca_btl_ugni_component.super.btl_version,
                                                 "Gemini byte transport layer");

    mca_btl_ugni_component.ugni_free_list_num = 8;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "free_list_num", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_free_list_num);
    mca_btl_ugni_component.ugni_free_list_max = 16384;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "free_list_max", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_free_list_max);
    mca_btl_ugni_component.ugni_free_list_inc = 64;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "free_list_inc", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_free_list_inc);

    mca_btl_ugni_component.ugni_eager_num = 16;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "eager_num", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_eager_num);
    mca_btl_ugni_component.ugni_eager_max = 128;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "eager_max", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_eager_max);
    mca_btl_ugni_component.ugni_eager_inc = 16;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "eager_inc", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_eager_inc);

    mca_btl_ugni_component.remote_cq_size = 40000;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "remote_cq_size", "Remote SMSG completion queue "
                                           "size (default 40000)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.remote_cq_size);
    mca_btl_ugni_component.local_cq_size = 8192;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "local_cq_size", "Local completion queue size "
                                           "(default 8192)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.local_cq_size);

    mca_btl_ugni_component.ugni_smsg_limit = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "smsg_limit", "Maximum size message that "
                                           "will be sent using the SMSG/MSGQ protocol "
                                           "(0 - autoselect(default), 16k max)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_smsg_limit);

    mca_btl_ugni_component.smsg_max_credits = 32;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "smsg_max_credits", "Maximum number of "
                                           "outstanding SMSG/MSGQ message (default 32)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.smsg_max_credits);

    mca_btl_ugni_component.ugni_fma_limit = 1024;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "fma_limit", "Maximum size message that "
                                           "will be sent using the FMA (Fast Memory "
                                           "Access) protocol (default 1024, 64k max)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_fma_limit);

    mca_btl_ugni_component.ugni_get_limit = 1 * 1024 * 1024;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "get_limit", "Maximum size message that "
                                           "will be sent using a get protocol "
                                           "(default 1M)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_get_limit);

    mca_btl_ugni_component.rdma_max_retries = 16;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "rdma_max_retries", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.rdma_max_retries);

    mca_btl_ugni_component.smsg_max_retries = 16;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "smsg_max_retries", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.smsg_max_retries);

    mca_btl_ugni_component.max_mem_reg = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "max_mem_reg", "Maximum number of "
                                           "memory registrations a process can "
                                           "hold (0 - autoselect, -1 - unlimited)"
                                           " (default 0)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.max_mem_reg);

    mca_btl_ugni_component.mbox_increment = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "mbox_inc", "Number of SMSG mailboxes to "
                                           "allocate in each block (0 - autoselect(default))",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_ugni_component.mbox_increment);

    mca_btl_ugni_component.smsg_page_size = 2 << 20;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "smsg_page_size", "Page size to use for SMSG "
                                           "mailbox allocation (default 2M)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.smsg_page_size);

    /* btl/ugni can only support only a fixed set of mpools (these mpools have compatible resource
     * structures) */
    rc = mca_base_var_enum_create ("btl_ugni_mpool", mpool_values, &new_enum);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    mca_btl_ugni_component.mpool_type = MCA_BTL_UGNI_MPOOL_UDREG;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "mpool", "mpool to use", MCA_BASE_VAR_TYPE_INT, new_enum,
                                           0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_ugni_component.mpool_type);
    OBJ_RELEASE(new_enum);

    mca_btl_ugni_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;

    /* smsg threshold */
    mca_btl_ugni_module.super.btl_eager_limit               = 8 * 1024;
    mca_btl_ugni_module.super.btl_rndv_eager_limit          = 8 * 1024;
    mca_btl_ugni_module.super.btl_rdma_pipeline_frag_size   = 4 * 1024 * 1024;
    mca_btl_ugni_module.super.btl_max_send_size             = 8 * 1024;
    mca_btl_ugni_module.super.btl_rdma_pipeline_send_length = 8 * 1024;

    /* threshold for put */
    mca_btl_ugni_module.super.btl_min_rdma_pipeline_size    = 8 * 1024;

    mca_btl_ugni_module.super.btl_flags = MCA_BTL_FLAGS_SEND |
        MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_SEND_INPLACE;

    mca_btl_ugni_module.super.btl_seg_size = sizeof (mca_btl_ugni_segment_t);

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

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */
static int
btl_ugni_component_close(void)
{
    ompi_common_ugni_fini ();

    return OMPI_SUCCESS;
}

static void mca_btl_ugni_autoset_leave_pinned (void) {
    if (MCA_BTL_UGNI_MPOOL_UDREG != mca_btl_ugni_component.mpool_type) {
        int value = opal_mem_hooks_support_level();
        if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
            ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & value)) {
            /* Set leave pinned to 1 if leave pinned pipeline is not set */
            if (-1 == ompi_mpi_leave_pinned) {
                ompi_mpi_leave_pinned = !ompi_mpi_leave_pinned_pipeline;
            }
        } else {
            ompi_mpi_leave_pinned = 0;
            ompi_mpi_leave_pinned_pipeline = 0;
        }
    } else if (-1 == ompi_mpi_leave_pinned) {
        /* if udreg is in use we can set leave pinned without checking for the
         * memory hooks. */
        ompi_mpi_leave_pinned = !ompi_mpi_leave_pinned_pipeline;
    }
}

static mca_btl_base_module_t **
mca_btl_ugni_component_init (int *num_btl_modules,
                             bool enable_progress_threads,
                             bool enable_mpi_threads)
{
    struct mca_btl_base_module_t **base_modules;
    mca_btl_ugni_module_t *ugni_modules;
    unsigned int i;
    int rc;

    /* Currently refuse to run if MPI_THREAD_MULTIPLE is enabled */
    if (ompi_mpi_thread_multiple && !mca_btl_base_thread_multiple_override) {
        opal_output_verbose(5, ompi_btl_base_framework.framework_output,
                            "btl:ugni: MPI_THREAD_MULTIPLE not supported; skipping this component");
        return NULL;
    }

    if (16384 < mca_btl_ugni_component.ugni_smsg_limit) {
        mca_btl_ugni_component.ugni_smsg_limit = 16384;
    }

    if (65536 < mca_btl_ugni_component.ugni_fma_limit) {
        mca_btl_ugni_component.ugni_fma_limit = 65536;
    }

    /* Initialize ugni library and create communication domain */
    rc = ompi_common_ugni_init();
    if (OMPI_SUCCESS != rc) {
        return NULL;
    }

    /* Create and initialize one module per uGNI device */
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

    mca_btl_ugni_module.super.btl_rdma_pipeline_send_length = mca_btl_ugni_module.super.btl_eager_limit;

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

    BTL_VERBOSE(("btl/ugni done initializing %d module(s)", *num_btl_modules));

    return base_modules;
}

static inline int
mca_btl_ugni_progress_datagram (mca_btl_ugni_module_t *ugni_module)
{
    uint32_t remote_addr, remote_id;
    uint64_t datagram_id, data;
    mca_btl_base_endpoint_t *ep;
    gni_post_state_t post_state;
    gni_ep_handle_t handle;
    gni_return_t grc;
    int count = 0, rc;

    /* check for datagram completion */
    grc = GNI_PostDataProbeById (ugni_module->device->dev_handle, &datagram_id);
    if (OPAL_LIKELY(GNI_RC_SUCCESS != grc)) {
        return 0;
    }

    data = datagram_id & ~(MCA_BTL_UGNI_DATAGRAM_MASK);

    BTL_VERBOSE(("datgram_id: %" PRIx64 ", mask: %" PRIx64, datagram_id, (uint64_t) (datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK)));

    if ((datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK) == MCA_BTL_UGNI_CONNECT_DIRECTED_ID) {
        ep = (mca_btl_base_endpoint_t *) opal_pointer_array_get_item (&ugni_module->endpoints, data);
        handle = ep->smsg_ep_handle;
    } else {
        handle = ugni_module->wildcard_ep;
    }

    /* wait for the incoming datagram to complete (in case it isn't) */
    grc = GNI_EpPostDataWaitById (handle, datagram_id, -1, &post_state,
                                  &remote_addr, &remote_id);
    if (GNI_RC_SUCCESS != grc) {
        BTL_ERROR(("GNI_EpPostDataWaitById failed with rc = %d", grc));
        return ompi_common_rc_ugni_to_ompi (grc);
    }

    /* if this is a wildcard endpoint lookup the remote peer by the proc id we received */
    if (handle == ugni_module->wildcard_ep) {
        BTL_VERBOSE(("received connection attempt on wildcard endpoint from proc id: %" PRIx64, ugni_module->wc_remote_attr.proc_id));
        rc = opal_hash_table_get_value_uint64 (&ugni_module->id_to_endpoint,
                                               ugni_module->wc_remote_attr.proc_id,
                                               (void *) &ep);
        /* check if the endpoint is known */
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc || NULL == ep)) {
            BTL_ERROR(("received connection attempt from an unknown peer. rc: %d, ep: %p, id: 0x%" PRIx64,
                       rc, ep, ugni_module->wc_remote_attr.proc_id));
            return OMPI_ERR_NOT_FOUND;
        }
    } else {
        BTL_VERBOSE(("directed datagram complete for endpoint %p", ep));
    }

    /* should not have gotten a NULL endpoint */
    assert (NULL != ep);

    BTL_VERBOSE(("got a datagram completion: id = %" PRIx64 ", state = %d, "
                 "data = 0x%" PRIx64 ", ep = %p, remote id: %d", datagram_id, post_state,
                 data, ep, remote_id));

    /* NTH: TODO -- error handling */
    (void) mca_btl_ugni_ep_connect_progress (ep);

    if (MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state) {
        /*  process messages waiting in the endpoint's smsg mailbox */
        count = mca_btl_ugni_smsg_process (ep);
    }

    /* repost the wildcard datagram */
    if (handle == ugni_module->wildcard_ep) {
        mca_btl_ugni_wildcard_ep_post (ugni_module);
    }

    return count;
}

static inline int
mca_btl_ugni_progress_rdma (mca_btl_ugni_module_t *ugni_module)
{
    ompi_common_ugni_post_desc_t *desc;
    mca_btl_ugni_base_frag_t *frag;
    gni_cq_entry_t event_data = 0;
    uint32_t recoverable = 1;
    gni_return_t rc;

    rc = GNI_CqGetEvent (ugni_module->rdma_local_cq, &event_data);
    if (GNI_RC_NOT_DONE == rc) {
        return 0;
    }

    if (OPAL_UNLIKELY((GNI_RC_SUCCESS != rc && !event_data) || GNI_CQ_OVERRUN(event_data))) {
        /* TODO -- need to handle overrun -- how do we do this without an event?
           will the event eventually come back? Ask Cray */
        BTL_ERROR(("unhandled post error! ugni rc = %d", rc));
        assert (0);
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    rc = GNI_GetCompleted (ugni_module->rdma_local_cq, event_data, (gni_post_descriptor_t **) &desc);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc && GNI_RC_TRANSACTION_ERROR != rc)) {
        BTL_ERROR(("Error in GNI_GetComplete %s", gni_err_str[rc]));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    frag = MCA_BTL_UGNI_DESC_TO_FRAG(desc);

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc || !GNI_CQ_STATUS_OK(event_data))) {
        (void) GNI_CqErrorRecoverable (event_data, &recoverable);

        if (OPAL_UNLIKELY(++frag->post_desc.tries >= mca_btl_ugni_component.rdma_max_retries ||
                          !recoverable)) {
            /* give up */
            BTL_ERROR(("giving up on frag %p", (void *) frag));
            frag->cbfunc (frag, OMPI_ERROR);

            return OMPI_ERROR;
        }

        /* repost transaction */
        mca_btl_ugni_repost (frag, OMPI_SUCCESS);

        return 0;
    }

    BTL_VERBOSE(("RDMA/FMA complete for frag %p", (void *) frag));

    frag->cbfunc (frag, ompi_common_rc_ugni_to_ompi (rc));

    return 1;
}

static inline int
mca_btl_ugni_retry_failed (mca_btl_ugni_module_t *ugni_module)
{
    int count = opal_list_get_size (&ugni_module->failed_frags);
    int i;

    for (i = 0 ; i < count ; ++i) {
        mca_btl_ugni_base_frag_t *frag =
            (mca_btl_ugni_base_frag_t *) opal_list_remove_first (&ugni_module->failed_frags);
        assert (NULL != frag);

        frag->cbfunc (frag, OMPI_SUCCESS);
    }

    return count;
}

static inline int
mca_btl_ugni_progress_wait_list (mca_btl_ugni_module_t *ugni_module)
{
    int count = opal_list_get_size (&ugni_module->ep_wait_list);
    int rc, i;

    for (i = 0 ; i < count ; ++i) {
        mca_btl_base_endpoint_t *endpoint =
            (mca_btl_base_endpoint_t *) opal_list_remove_first (&ugni_module->ep_wait_list);
        assert (NULL != endpoint);

        endpoint->wait_listed = false;

        rc = mca_btl_ugni_progress_send_wait_list (endpoint);
        if (OMPI_SUCCESS != rc && false == endpoint->wait_listed) {
            opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
            endpoint->wait_listed = true;
        }
    }

    return count;
}

static int mca_btl_ugni_component_progress (void)
{
    mca_btl_ugni_module_t *ugni_module;
    unsigned int i;
    int count = 0;

    for (i = 0 ; i < mca_btl_ugni_component.ugni_num_btls ; ++i) {
        ugni_module = mca_btl_ugni_component.modules + i;

        mca_btl_ugni_retry_failed (ugni_module);
        mca_btl_ugni_progress_wait_list (ugni_module);

        count += mca_btl_ugni_progress_datagram (ugni_module);
        count += mca_btl_ugni_progress_local_smsg (ugni_module);
        count += mca_btl_ugni_progress_remote_smsg (ugni_module);
        count += mca_btl_ugni_progress_rdma (ugni_module);
    }

    return count;
}
