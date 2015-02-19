/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
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
#include "opal/runtime/opal_params.h"

#include "opal/mca/base/mca_base_pvar.h"

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
            MCA_BTL_DEFAULT_VERSION("ugni"),
            .mca_open_component = btl_ugni_component_open,
            .mca_close_component = btl_ugni_component_close,
            .mca_register_component_params = btl_ugni_component_register,
        },
        .btl_data = {
            .param_field = MCA_BASE_METADATA_PARAM_CHECKPOINT
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
    gni_nic_device_t device_type;
    int rc;

    (void) mca_base_var_group_component_register(&mca_btl_ugni_component.super.btl_version,
                                                 "uGNI byte transport layer");

    mca_btl_ugni_component.ugni_free_list_num = 8;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "free_list_num", NULL, MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.ugni_free_list_num);
    mca_btl_ugni_component.ugni_free_list_max = 4096;
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

    mca_btl_ugni_component.progress_thread_requested = 0;
    (void) mca_base_component_var_register(&mca_btl_ugni_component.super.btl_version,
                                           "request_progress_thread",
                                           "Enable to request ugni btl progress thread - requires MPI_THREAD_MULTIPLE support",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_ugni_component.progress_thread_requested);

    /* performance variables */
    mca_btl_ugni_progress_thread_wakeups = 0;
    (void) mca_base_component_pvar_register(&mca_btl_ugni_component.super.btl_version,
                                            "progress_thread_wakeups", "Number of times the progress thread "
                                            "has been woken", OPAL_INFO_LVL_9, MCA_BASE_PVAR_CLASS_COUNTER,
                                            MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                            MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS, NULL,
                                            NULL, NULL, &mca_btl_ugni_progress_thread_wakeups);

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

    mca_btl_ugni_module.super.btl_get_limit = 1 * 1024 * 1024;

    /* determine if there are get alignment restrictions */
    GNI_GetDeviceType (&device_type);

    if (GNI_DEVICE_GEMINI == device_type) {
        mca_btl_ugni_module.super.btl_get_alignment = 4;
    } else {
        mca_btl_ugni_module.super.btl_get_alignment = 0;
    }

    /* threshold for put */
    mca_btl_ugni_module.super.btl_min_rdma_pipeline_size    = 8 * 1024;

    mca_btl_ugni_module.super.btl_flags = MCA_BTL_FLAGS_SEND |
        MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_SEND_INPLACE | MCA_BTL_FLAGS_ATOMIC_OPS |
        MCA_BTL_FLAGS_ATOMIC_FOPS;
    mca_btl_ugni_module.super.btl_atomic_flags = MCA_BTL_ATOMIC_SUPPORTS_ADD |
        MCA_BTL_ATOMIC_SUPPORTS_AND | MCA_BTL_ATOMIC_SUPPORTS_OR | MCA_BTL_ATOMIC_SUPPORTS_XOR |
        MCA_BTL_ATOMIC_SUPPORTS_CSWAP;

    mca_btl_ugni_module.super.btl_registration_handle_size = sizeof (mca_btl_base_registration_handle_t);

    mca_btl_ugni_module.super.btl_bandwidth = 40000; /* Mbs */
    mca_btl_ugni_module.super.btl_latency   = 2;     /* Microsecs */

    mca_btl_ugni_module.super.btl_get_local_registration_threshold = 0;
    mca_btl_ugni_module.super.btl_put_local_registration_threshold = mca_btl_ugni_component.ugni_fma_limit;

    /* Call the BTL based to register its MCA params */
    mca_btl_base_param_register(&mca_btl_ugni_component.super.btl_version,
                                &mca_btl_ugni_module.super);

    return OPAL_SUCCESS;
}

static int
btl_ugni_component_open(void)
{
    mca_btl_ugni_component.ugni_num_btls = 0;
    mca_btl_ugni_component.modules = NULL;

    return OPAL_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */
static int
btl_ugni_component_close(void)
{
    opal_common_ugni_fini ();

    if (mca_btl_ugni_component.modules) {
        free (mca_btl_ugni_component.modules);
        mca_btl_ugni_component.modules = NULL;
    }

    return OPAL_SUCCESS;
}

static void mca_btl_ugni_autoset_leave_pinned (void) {
    if (MCA_BTL_UGNI_MPOOL_UDREG != mca_btl_ugni_component.mpool_type) {
        int value = opal_mem_hooks_support_level();
        if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
            ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) & value)) {
            /* Set leave pinned to 1 if leave pinned pipeline is not set */
            if (-1 == opal_leave_pinned) {
                opal_leave_pinned = !opal_leave_pinned_pipeline;
            }
        } else {
            opal_leave_pinned = 0;
            opal_leave_pinned_pipeline = 0;
        }
    } else if (-1 == opal_leave_pinned) {
        /* if udreg is in use we can set leave pinned without checking for the
         * memory hooks. */
        opal_leave_pinned = !opal_leave_pinned_pipeline;
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

    if (16384 < mca_btl_ugni_component.ugni_smsg_limit) {
        mca_btl_ugni_component.ugni_smsg_limit = 16384;
    }

    if (65536 < mca_btl_ugni_component.ugni_fma_limit) {
        mca_btl_ugni_component.ugni_fma_limit = 65536;
    }

    mca_btl_ugni_module.super.btl_put_local_registration_threshold = mca_btl_ugni_component.ugni_fma_limit;

    if (enable_mpi_threads && mca_btl_ugni_component.progress_thread_requested) {
        mca_btl_ugni_component.progress_thread_enabled = 1;
    }

    /* Initialize ugni library and create communication domain */
    rc = opal_common_ugni_init();
    if (OPAL_SUCCESS != rc) {
        return NULL;
    }

    /* Create and initialize one module per uGNI device */
    mca_btl_ugni_component.ugni_num_btls = opal_common_ugni_module.device_count;

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
                                       opal_common_ugni_module.devices + i);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
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
    OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);  /* TODO: may not need lock for this function */
    grc = GNI_PostDataProbeById (ugni_module->device->dev_handle, &datagram_id);
    OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);  
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
    OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);  /* TODO: may not need lock for this function */
    grc = GNI_EpPostDataWaitById (handle, datagram_id, -1, &post_state,
                                  &remote_addr, &remote_id);
    OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);  
    if (GNI_RC_SUCCESS != grc) {
        BTL_ERROR(("GNI_EpPostDataWaitById failed with rc = %d", grc));
        return opal_common_rc_ugni_to_opal (grc);
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
                       rc, (void *) ep, ugni_module->wc_remote_attr.proc_id));
            return OPAL_ERR_NOT_FOUND;
        }
    } else {
        BTL_VERBOSE(("directed datagram complete for endpoint %p", (void *) ep));
    }

    /* should not have gotten a NULL endpoint */
    assert (NULL != ep);

    BTL_VERBOSE(("got a datagram completion: id = %" PRIx64 ", state = %d, "
                 "data = 0x%" PRIx64 ", ep = %p, remote id: %d", datagram_id, post_state,
                 data, (void *) ep, remote_id));

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

#if OPAL_ENABLE_DEBUG
static inline void btl_ugni_dump_post_desc (mca_btl_ugni_post_descriptor_t *desc)
{

    fprintf (stderr, "desc->desc.base.post_id          = %" PRIx64 "\n", desc->desc.base.post_id);
    fprintf (stderr, "desc->desc.base.status           = %" PRIx64 "\n", desc->desc.base.status);
    fprintf (stderr, "desc->desc.base.cq_mode_complete = %hu\n", desc->desc.base.cq_mode_complete);
    fprintf (stderr, "desc->desc.base.type             = %d\n", desc->desc.base.type);
    fprintf (stderr, "desc->desc.base.cq_mode          = %hu\n", desc->desc.base.cq_mode);
    fprintf (stderr, "desc->desc.base.dlvr_mode        = %hu\n", desc->desc.base.dlvr_mode);
    fprintf (stderr, "desc->desc.base.local_addr       = %" PRIx64 "\n", desc->desc.base.local_addr);
    fprintf (stderr, "desc->desc.base.local_mem_hndl   = {%" PRIx64 ", %" PRIx64 "}\n", desc->desc.base.local_mem_hndl.qword1,
             desc->desc.base.local_mem_hndl.qword2);
    fprintf (stderr, "desc->desc.base.remote_addr      = %" PRIx64 "\n", desc->desc.base.remote_addr);
    fprintf (stderr, "desc->desc.base.remote_mem_hndl  = {%" PRIx64 ", %" PRIx64 "}\n", desc->desc.base.remote_mem_hndl.qword1,
             desc->desc.base.remote_mem_hndl.qword2);
    fprintf (stderr, "desc->desc.base.length           = %" PRIu64 "\n", desc->desc.base.length);
    fprintf (stderr, "desc->desc.base.rdma_mode        = %hu\n", desc->desc.base.rdma_mode);
    fprintf (stderr, "desc->desc.base.amo_cmd          = %d\n", desc->desc.base.amo_cmd);
}
#endif

static inline int mca_btl_ugni_progress_rdma (mca_btl_ugni_module_t *ugni_module, int which_cq)
{
    mca_btl_ugni_post_descriptor_t *post_desc = NULL;
    gni_cq_entry_t event_data = 0;
    gni_post_descriptor_t *desc;
    uint32_t recoverable = 1;
    gni_return_t grc;
    gni_cq_handle_t the_cq;

    the_cq = (which_cq == 0) ? ugni_module->rdma_local_cq : ugni_module->rdma_local_irq_cq;

    OPAL_THREAD_LOCK(&ugni_module->device->dev_lock);
    grc = GNI_CqGetEvent (the_cq, &event_data);
    if (GNI_RC_NOT_DONE == grc) {
        OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
        return 0;
    }

    if (OPAL_UNLIKELY((GNI_RC_SUCCESS != grc && !event_data) || GNI_CQ_OVERRUN(event_data))) {
        /* TODO -- need to handle overrun -- how do we do this without an event?
           will the event eventually come back? Ask Cray */
        BTL_ERROR(("unhandled post error! ugni rc = %d %s", grc, gni_err_str[grc]));
        OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);

        return opal_common_rc_ugni_to_opal (grc);
    }

    grc = GNI_GetCompleted (the_cq, event_data, &desc);
    OPAL_THREAD_UNLOCK(&ugni_module->device->dev_lock);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc && GNI_RC_TRANSACTION_ERROR != grc)) {
        BTL_ERROR(("Error in GNI_GetComplete %s", gni_err_str[grc]));
        return opal_common_rc_ugni_to_opal (grc);
    }

    post_desc = MCA_BTL_UGNI_DESC_TO_PDESC(desc);

    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != grc || !GNI_CQ_STATUS_OK(event_data))) {
        (void) GNI_CqErrorRecoverable (event_data, &recoverable);

        if (OPAL_UNLIKELY(++post_desc->desc.tries >= mca_btl_ugni_component.rdma_max_retries ||
                          !recoverable)) {
            char char_buffer[1024];
            GNI_CqErrorStr (event_data, char_buffer, 1024);
            /* give up */
            BTL_ERROR(("giving up on desciptor %p, recoverable %d: %s", (void *) post_desc,
                       recoverable, char_buffer));
#if OPAL_ENABLE_DEBUG
            btl_ugni_dump_post_desc (post_desc);
#endif
            mca_btl_ugni_post_desc_complete (ugni_module, post_desc, OPAL_ERROR);

            return OPAL_ERROR;
        }

        mca_btl_ugni_repost (ugni_module, post_desc);

        return 0;
    }

    mca_btl_ugni_post_desc_complete (ugni_module, post_desc, opal_common_rc_ugni_to_opal (grc));

    return 1;
}

static inline int
mca_btl_ugni_post_pending (mca_btl_ugni_module_t *ugni_module)
{
    int count = opal_list_get_size (&ugni_module->pending_descriptors);
    int i;

    for (i = 0 ; i < count ; ++i) {
        OPAL_THREAD_LOCK(&ugni_module->pending_descriptors_lock);
        mca_btl_ugni_post_descriptor_t *post_desc =
            (mca_btl_ugni_post_descriptor_t *) opal_list_remove_first (&ugni_module->pending_descriptors);
        OPAL_THREAD_UNLOCK(&ugni_module->pending_descriptors_lock);

        if (OPAL_SUCCESS != mca_btl_ugni_repost (ugni_module, post_desc)) {
            break;
        }
    }

    return i;
}

static inline int
mca_btl_ugni_progress_wait_list (mca_btl_ugni_module_t *ugni_module)
{
    int rc = OPAL_SUCCESS;
    mca_btl_base_endpoint_t *endpoint = NULL;
    int count;

    OPAL_THREAD_LOCK(&ugni_module->ep_wait_list_lock);
    count  = opal_list_get_size(&ugni_module->ep_wait_list);
    OPAL_THREAD_UNLOCK(&ugni_module->ep_wait_list_lock);

    do {
        OPAL_THREAD_LOCK(&ugni_module->ep_wait_list_lock);
        endpoint = (mca_btl_base_endpoint_t *) opal_list_remove_first (&ugni_module->ep_wait_list);
        OPAL_THREAD_UNLOCK(&ugni_module->ep_wait_list_lock);
        if (endpoint != NULL) {

            endpoint->wait_listed = false;

            rc = mca_btl_ugni_progress_send_wait_list (endpoint);

            if (OPAL_SUCCESS != rc && false == endpoint->wait_listed) {

                endpoint->wait_listed = true;
                OPAL_THREAD_LOCK(&ugni_module->ep_wait_list_lock);
                opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
                OPAL_THREAD_UNLOCK(&ugni_module->ep_wait_list_lock);
            }
        }

        --count;
        if (count == 0) break;

    } while (endpoint != NULL) ;

    return rc;
}

static int mca_btl_ugni_component_progress (void)
{
    mca_btl_ugni_module_t *ugni_module;
    unsigned int i;
    int count = 0;

    for (i = 0 ; i < mca_btl_ugni_component.ugni_num_btls ; ++i) {
        ugni_module = mca_btl_ugni_component.modules + i;

        mca_btl_ugni_progress_wait_list (ugni_module);

        count += mca_btl_ugni_progress_datagram (ugni_module);
        count += mca_btl_ugni_progress_local_smsg (ugni_module);
        count += mca_btl_ugni_progress_remote_smsg (ugni_module);
        count += mca_btl_ugni_progress_rdma (ugni_module, 0);
        if (mca_btl_ugni_component.progress_thread_enabled) {
            count += mca_btl_ugni_progress_rdma (ugni_module, 1);
        }

        /* post pending after progressing rdma */
        mca_btl_ugni_post_pending (ugni_module);
    }

    return count;
}
