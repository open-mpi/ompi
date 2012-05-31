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
        mca_btl_ugni_param_register_int("eager_max", NULL, 128);
    mca_btl_ugni_component.ugni_eager_inc =
        mca_btl_ugni_param_register_int("eager_inc", NULL, 16);

    mca_btl_ugni_component.remote_cq_size =
        mca_btl_ugni_param_register_int("remote_cq_size", "Remote SMSG completion queue "
                                        "size (default 40000)", 40000);

    mca_btl_ugni_component.local_cq_size =
        mca_btl_ugni_param_register_int("local_cq_size", "Local completion queue size "
                                        "(default 8192)", 8192);

    mca_btl_ugni_component.ugni_smsg_limit =
        mca_btl_ugni_param_register_int("smsg_limit", "Maximum size message that "
                                        "will be sent using the SMSG/MSGQ protocol "
                                        "(0 - autoselect(default), 16k max)", 0);

    if (16384 < mca_btl_ugni_component.ugni_smsg_limit) {
        mca_btl_ugni_component.ugni_smsg_limit = 16384;
    }

    mca_btl_ugni_component.smsg_max_credits =
        mca_btl_ugni_param_register_int("smsg_max_credits", "Maximum number of "
                                        "outstanding SMSG/MSGQ message (default 32)",
                                        32);

    mca_btl_ugni_component.ugni_fma_limit =
        mca_btl_ugni_param_register_int("fma_limit", "Maximum size message that "
                                        "will be sent using the FMA (Fast Memory "
                                        "Access) protocol (default 1024, 64k max)",
                                        1024);

    if (65536 < mca_btl_ugni_component.ugni_fma_limit) {
        mca_btl_ugni_component.ugni_fma_limit = 65536;
    }

    mca_btl_ugni_component.ugni_get_limit =
        mca_btl_ugni_param_register_int("get_limit", "Maximum size message that "
                                        "will be sent using a get protocol "
                                        "(default 4M)", 4 * 1024 * 1024);

    mca_btl_ugni_component.rdma_max_retries =
        mca_btl_ugni_param_register_int("rdma_max_retries", NULL, 16);


    mca_btl_ugni_component.smsg_max_retries =
        mca_btl_ugni_param_register_int("smsg_max_retries", NULL, 16);

    mca_btl_ugni_component.max_mem_reg =
        mca_btl_ugni_param_register_int("max_mem_reg", "Maximum number of "
                                        "memory registrations a process can "
                                        "hold (0 - autoselect, -1 - unlimited)"
                                        " (default 0)", 0);

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
    size_t nprocs;
    gni_return_t rc;

    (void) ompi_proc_world (&nprocs);

    if (0 == mca_btl_ugni_component.ugni_smsg_limit) {
        /* auto-set the smsg limit based on the number of ranks */
        if (nprocs <= 512) {
            mca_btl_ugni_component.ugni_smsg_limit = 8192;
        } else if (nprocs <= 1024) {
            mca_btl_ugni_component.ugni_smsg_limit = 2048;
        } else if (nprocs <= 8192) {
            mca_btl_ugni_component.ugni_smsg_limit = 1024;
        } else if (nprocs <= 16384) {
            mca_btl_ugni_component.ugni_smsg_limit = 512;
        } else {
            mca_btl_ugni_component.ugni_smsg_limit = 256;
        }
    }

    mca_btl_ugni_component.smsg_max_data = mca_btl_ugni_component.ugni_smsg_limit -
        sizeof (mca_btl_ugni_send_frag_hdr_t);

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
    int rc;

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

    rc = mca_btl_ugni_smsg_setup ();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return NULL;
    }

    if (mca_btl_ugni_component.ugni_smsg_limit == mca_btl_ugni_module.super.btl_eager_limit) {
        mca_btl_ugni_module.super.btl_eager_limit = mca_btl_ugni_component.smsg_max_data;
    }

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

    BTL_VERBOSE(("btl/ugni done initializing modules"));

    return base_modules;
}

static inline int
mca_btl_ugni_progress_datagram (mca_btl_ugni_module_t *ugni_module)
{
    uint32_t remote_addr, remote_id;
    mca_btl_base_endpoint_t *ep;
    gni_post_state_t post_state;
    gni_ep_handle_t handle;
    uint64_t datagram_id;
    gni_return_t grc;
    int count = 0;

    /* check for datagram completion */
    grc = GNI_PostDataProbeById (ugni_module->device->dev_handle, &datagram_id);
    if (OPAL_LIKELY(GNI_RC_SUCCESS != grc)) {
        return 0;
    }

    if ((datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK) ==
        MCA_BTL_UGNI_CONNECT_WILDCARD_ID) {
        handle = ugni_module->wildcard_ep;
    } else {
        handle =
            ugni_module->endpoints[(uint32_t)(datagram_id & 0xffffffffull)]->smsg_ep_handle;
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

    ep = ugni_module->endpoints[remote_id];

    /* NTH: TODO -- error handling */
    (void) mca_btl_ugni_ep_connect_progress (ep);

    if (MCA_BTL_UGNI_EP_STATE_CONNECTED == ep->state) {
        /*  process messages waiting in the endpoint's smsg mailbox */
        count = mca_btl_ugni_smsg_process (ep);
    }

    /* repost the wildcard datagram */
    if ((datagram_id & MCA_BTL_UGNI_DATAGRAM_MASK) ==
        MCA_BTL_UGNI_CONNECT_WILDCARD_ID) {
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

    BTL_VERBOSE(("RDMA/FMA complete for frag %p", frag));

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

        rc = mca_btl_progress_send_wait_list (endpoint);
        if (OMPI_SUCCESS != rc) {
            opal_list_append (&ugni_module->ep_wait_list, &endpoint->super);
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
