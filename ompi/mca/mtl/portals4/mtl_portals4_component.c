/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "opal/mca/pmix/pmix.h"

#include "mtl_portals4.h"
#include "mtl_portals4_request.h"
#include "mtl_portals4_recv_short.h"
#include "mtl_portals4_message.h"
#include "ompi/runtime/mpiruntime.h"

static int param_priority;

static int ompi_mtl_portals4_component_register(void);
static int ompi_mtl_portals4_component_open(void);
static int ompi_mtl_portals4_component_close(void);
static int ompi_mtl_portals4_component_query(mca_base_module_t **module, int *priority);
static mca_mtl_base_module_t*
ompi_mtl_portals4_component_init(bool enable_progress_threads,
                                 bool enable_mpi_threads);

OMPI_MODULE_DECLSPEC extern mca_mtl_base_component_2_0_0_t mca_mtl_portals4_component;

mca_mtl_base_component_2_0_0_t mca_mtl_portals4_component = {

    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */

    .mtl_version = {
        MCA_MTL_BASE_VERSION_2_0_0,

        .mca_component_name = "portals4",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),
        .mca_open_component = ompi_mtl_portals4_component_open,
        .mca_close_component = ompi_mtl_portals4_component_close,
        .mca_query_component = ompi_mtl_portals4_component_query,
        .mca_register_component_params = ompi_mtl_portals4_component_register,
    },
    .mtl_data = {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },

    .mtl_init = ompi_mtl_portals4_component_init,
};

static mca_base_var_enum_value_t long_protocol_values[] = {
    {eager, "eager"},
    {rndv, "rndv"},
    {0, NULL}
};

OBJ_CLASS_INSTANCE(ompi_mtl_portals4_rndv_get_frag_t,
                   opal_free_list_item_t,
                   NULL, NULL);

static int
ompi_mtl_portals4_component_register(void)
{
    mca_base_var_enum_t *new_enum;
    int ret;

    ompi_mtl_portals4.use_logical = 0;
    (void) mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "use_logical",
                                           "Use the logical to physical table to accelerate portals4 adressing: 1 (true) : 0 (false)",
                                           MCA_BASE_VAR_TYPE_INT,
                                           NULL,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.use_logical);

    param_priority = 10;
    (void) mca_base_component_var_register (&mca_mtl_portals4_component.mtl_version,
                                            "priority", "Priority of the Portals4 MTL component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &param_priority);
    ompi_mtl_portals4.short_limit = 2 * 1024;
    (void) mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "short_limit",
                                           "Size limit for short messages",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG,
                                           NULL,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.short_limit);


    ompi_mtl_portals4.eager_limit = 2 * 1024;
    (void) mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "eager_limit",
                                           "Cross-over point from eager to rendezvous sends",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG,
                                           NULL,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.eager_limit);

    ompi_mtl_portals4.recv_short_num = 32;
    (void) mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "short_recv_num",
                                           "Number of short message receive blocks",
                                           MCA_BASE_VAR_TYPE_INT,
                                           NULL,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.recv_short_num);

    ompi_mtl_portals4.recv_short_size = 2 * 1024 * 1024;
    (void) mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "short_recv_size",
                                           "Size of short message receive blocks",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG,
                                           NULL,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.recv_short_size);

    ompi_mtl_portals4.send_queue_size = 1024;
    (void) mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "send_event_queue_size",
                                           "Size of the send event queue in entries",
                                           MCA_BASE_VAR_TYPE_INT,
                                           NULL,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.send_queue_size);

    ompi_mtl_portals4.recv_queue_size = 1024;
    (void) mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "recv_event_queue_size",
                                           "Size of the recv event queue in entries",
                                           MCA_BASE_VAR_TYPE_INT,
                                           NULL,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.recv_queue_size);

    ompi_mtl_portals4.protocol = eager;
    mca_base_var_enum_create("mtl_portals4_long_protocol", long_protocol_values, &new_enum);
    ret = mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "long_protocol",
                                           "Protocol to use for long messages.  Valid entries are eager and rndv",
                                           MCA_BASE_VAR_TYPE_INT,
                                           new_enum,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.protocol);

    ompi_mtl_portals4.max_msg_size_mtl = PTL_SIZE_MAX;
    (void) mca_base_component_var_register(&mca_mtl_portals4_component.mtl_version,
                                           "max_msg_size",
                                           "Max size supported by portals4 (above that, a message is cut into messages less than that size)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_LONG,
                                           NULL,
                                           0,
                                           0,
                                           OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_portals4.max_msg_size_mtl);

    OBJ_RELEASE(new_enum);
    if (0 > ret) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    return OMPI_SUCCESS;
}

static int
ompi_mtl_portals4_component_open(void)
{
    ompi_mtl_portals4.base.mtl_request_size =
        sizeof(ompi_mtl_portals4_request_t) -
        sizeof(struct mca_mtl_request_t);

    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Flow control: "
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
                        "yes"
#else
                        "no"
#endif
                        );
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Max message size: %lu", (unsigned long)
                        ompi_mtl_portals4.max_msg_size_mtl);
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Short limit: %d", (int)
                        ompi_mtl_portals4.short_limit);
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Eager limit: %d", (int)
                        ompi_mtl_portals4.eager_limit);
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Short receive blocks: %d",
                        ompi_mtl_portals4.recv_short_num);
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Send queue size: %d", ompi_mtl_portals4.send_queue_size);
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Recv queue size: %d", ompi_mtl_portals4.recv_queue_size);
    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Long protocol: %s",
                        (ompi_mtl_portals4.protocol == eager) ? "Eager" :
                        (ompi_mtl_portals4.protocol == rndv) ? "Rendezvous" :
                         "Other");

    OBJ_CONSTRUCT(&ompi_mtl_portals4.fl_message, opal_free_list_t);
    opal_free_list_init(&ompi_mtl_portals4.fl_message,
                        sizeof(ompi_mtl_portals4_message_t) +
                        ompi_mtl_portals4.eager_limit,
                        opal_cache_line_size,
                        OBJ_CLASS(ompi_mtl_portals4_message_t),
                        0, 0, 1, -1, 1, NULL, 0, NULL, NULL, NULL);

    OBJ_CONSTRUCT(&ompi_mtl_portals4.fl_rndv_get_frag, opal_free_list_t);
    opal_free_list_init(&ompi_mtl_portals4.fl_rndv_get_frag,
                        sizeof(ompi_mtl_portals4_rndv_get_frag_t),
                        opal_cache_line_size,
                        OBJ_CLASS(ompi_mtl_portals4_rndv_get_frag_t),
                        0, 0, 1, -1, 1, NULL, 0, NULL, NULL, NULL);

    ompi_mtl_portals4.ni_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.send_eq_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.recv_eq_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.zero_md_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.send_md_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.long_overflow_me_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.recv_idx = (ptl_pt_index_t) ~0UL;
    ompi_mtl_portals4.read_idx = (ptl_pt_index_t) ~0UL;

    ompi_mtl_portals4.need_init=1;

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    ompi_mtl_portals4.use_flowctl=1;
#else
    ompi_mtl_portals4.use_flowctl=0;
#endif

    return OMPI_SUCCESS;
}

#define NEED_ALL_PROCS (ompi_mtl_portals4.use_logical || ompi_mtl_portals4.use_flowctl)

static int
ompi_mtl_portals4_component_query(mca_base_module_t **module, int *priority)
{
    /*
     * assume if portals4 MTL was compiled, the user wants it
     */

    *priority = param_priority;
    *module = (mca_base_module_t *)&ompi_mtl_portals4.base;

    if (NEED_ALL_PROCS) {
        /* let the pml know we need add_procs to be calls with all the
         * procs in the job */
        ompi_mtl_portals4.base.mtl_flags |= MCA_MTL_BASE_FLAG_REQUIRE_WORLD;
    }

    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals4_component_close(void)
{
    OBJ_DESTRUCT(&ompi_mtl_portals4.fl_message);

    return OMPI_SUCCESS;
}


static mca_mtl_base_module_t*
ompi_mtl_portals4_component_init(bool enable_progress_threads,
                                 bool enable_mpi_threads)
{
    int ret;
    ptl_process_t id;
    ptl_ni_limits_t actual_limits;

    if (enable_mpi_threads && ompi_mpi_thread_multiple) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "mtl portals4 is initialized for threads");
    }


    /* Initialize Portals and create a physical, matching interface */
    ret = PtlInit();
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        return NULL;
    }

    if (ompi_mtl_portals4.use_logical)
        ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_LOGICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,
                    NULL,
                    &actual_limits,
                    &ompi_mtl_portals4.ni_h);
    else ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_PHYSICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,
                    NULL,
                    &actual_limits,
                    &ompi_mtl_portals4.ni_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlNIInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    if (actual_limits.max_msg_size < ompi_mtl_portals4.max_msg_size_mtl)
        ompi_mtl_portals4.max_msg_size_mtl = actual_limits.max_msg_size;
    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_framework.framework_output,
        "Due to portals4 and user configuration messages will not go over the size of %lu", ompi_mtl_portals4.max_msg_size_mtl));

    if (ompi_comm_rank(MPI_COMM_WORLD) == 0) {
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_entries=%d", actual_limits.max_entries);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_unexpected_headers=%d", actual_limits.max_unexpected_headers);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_mds=%d", actual_limits.max_mds);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_eqs=%d", actual_limits.max_eqs);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_cts=%d", actual_limits.max_cts);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_pt_index=%d", actual_limits.max_pt_index);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_iovecs=%d", actual_limits.max_iovecs);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_list_size=%d", actual_limits.max_list_size);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_triggered_ops=%d", actual_limits.max_triggered_ops);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_msg_size=%ld", actual_limits.max_msg_size);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_atomic_size=%ld", actual_limits.max_atomic_size);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_fetch_atomic_size=%ld", actual_limits.max_fetch_atomic_size);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_waw_ordered_size=%ld", actual_limits.max_waw_ordered_size);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_war_ordered_size=%ld", actual_limits.max_war_ordered_size);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "max_volatile_size=%ld", actual_limits.max_volatile_size);
        opal_output_verbose(10, ompi_mtl_base_framework.framework_output, "features=%u", actual_limits.features);
    }

    ret = PtlGetUid(ompi_mtl_portals4.ni_h, &ompi_mtl_portals4.uid);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlGetUid failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Publish our NID/PID in the modex */
    ret = PtlGetPhysId(ompi_mtl_portals4.ni_h, &id);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlGetPhysId failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ompi_mtl_portals4.ptl_process_id = id;
    OPAL_OUTPUT_VERBOSE((90, ompi_mtl_base_framework.framework_output,
        "PtlGetPhysId rank=%x nid=%x pid=%x\n", id.rank, id.phys.nid, id.phys.pid));

    OPAL_MODEX_SEND(ret, OPAL_PMIX_GLOBAL,
                    &mca_mtl_portals4_component.mtl_version,
                    &id, sizeof(id));
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: ompi_modex_send failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                         "My nid,pid = %x,%x",
                         id.phys.nid, id.phys.pid));

    ompi_mtl_portals4.base.mtl_max_tag = MTL_PORTALS4_MAX_TAG;
    return &ompi_mtl_portals4.base;

 error:
    return NULL;
}


int
ompi_mtl_portals4_get_error(int ptl_error)
{
    int ret;

    switch (ptl_error) {
    case PTL_OK:
        ret = OMPI_SUCCESS;
        break;
    case PTL_ARG_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_CT_NONE_REACHED:
        ret = OMPI_ERR_TIMEOUT;
        break;
    case PTL_EQ_DROPPED:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_EQ_EMPTY:
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        break;
    case PTL_FAIL:
        ret = OMPI_ERROR;
        break;
    case PTL_IN_USE:
        ret = OMPI_ERR_RESOURCE_BUSY;
        break;
    case PTL_INTERRUPTED:
        ret = OMPI_ERR_RESOURCE_BUSY;
        break;
    case PTL_LIST_TOO_LONG:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_NO_INIT:
        ret = OMPI_ERR_FATAL;
        break;
    case PTL_NO_SPACE:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_PID_IN_USE:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    case PTL_PT_FULL:
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_PT_EQ_NEEDED:
        ret = OMPI_ERR_FATAL;
        break;
    case PTL_PT_IN_USE:
        ret = OMPI_ERR_RESOURCE_BUSY;
        break;
    default:
        ret = OMPI_ERROR;
    }

    return ret;
}


int
ompi_mtl_portals4_progress(void)
{
    int count = 0, ret;
    unsigned int which;
    ptl_event_t ev;
    ompi_mtl_portals4_base_request_t *ptl_request;
    ompi_mtl_portals4_rndv_get_frag_t *rndv_get_frag;

    while (true) {
	ret = PtlEQPoll(ompi_mtl_portals4.eqs_h, 2, 0, &ev, &which);
        if (PTL_OK == ret) {
            OPAL_OUTPUT_VERBOSE((60, ompi_mtl_base_framework.framework_output,
                                 "Found event of type %d\n", ev.type));
            count++;
            switch (ev.type) {
            case PTL_EVENT_GET:
            case PTL_EVENT_PUT:
            case PTL_EVENT_PUT_OVERFLOW:
            case PTL_EVENT_SEND:
            case PTL_EVENT_ACK:
            case PTL_EVENT_AUTO_FREE:
            case PTL_EVENT_AUTO_UNLINK:
            case PTL_EVENT_SEARCH:
            case PTL_EVENT_LINK:
                if (NULL != ev.user_ptr) {
                    ptl_request = ev.user_ptr;
                    ret = ptl_request->event_callback(&ev, ptl_request);
                    if (OMPI_SUCCESS != ret) {
                        opal_output(ompi_mtl_base_framework.framework_output,
                                    "Error returned from target event callback: %d", ret);
                        abort();
                    }
                }
                break;

            case PTL_EVENT_REPLY:
                if (NULL != ev.user_ptr) {
                    rndv_get_frag = ev.user_ptr;
                    ret = rndv_get_frag->event_callback(&ev, rndv_get_frag);
                    if (OMPI_SUCCESS != ret) {
                        opal_output(ompi_mtl_base_framework.framework_output,
                                    "Error returned from target event callback: %d", ret);
                        abort();
                    }
                }
                break;

            case PTL_EVENT_PT_DISABLED:
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
                OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_framework.framework_output,
                                     "Received PT_DISABLED event on pt %d\n",
                                     (int) ev.pt_index));
                ret = ompi_mtl_portals4_flowctl_trigger();
                if (OMPI_SUCCESS != ret) {
                    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                        "%s:%d: flowctl_trigger() failed: %d\n",
                                        __FILE__, __LINE__, ret);
                    abort();
                }
#else
                opal_output(ompi_mtl_base_framework.framework_output,
                            "Flow control situation without recovery (PT_DISABLED)");
                abort();
#endif
                break;

            case PTL_EVENT_GET_OVERFLOW:
            case PTL_EVENT_FETCH_ATOMIC:
            case PTL_EVENT_FETCH_ATOMIC_OVERFLOW:
            case PTL_EVENT_ATOMIC:
            case PTL_EVENT_ATOMIC_OVERFLOW:
                opal_output(ompi_mtl_base_framework.framework_output,
                            "Unexpected event of type %d", ev.type);
            }
        } else if (PTL_EQ_EMPTY == ret) {
            break;
        } else if (PTL_EQ_DROPPED == ret) {
            opal_output(ompi_mtl_base_framework.framework_output,
                        "Flow control situation without recovery (EQ_DROPPED): %d",
                        which);
            abort();
        } else {
            opal_output(ompi_mtl_base_framework.framework_output,
                        "Error returned from PtlEQGet: %d", ret);
            break;
        }
    }

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    if (OPAL_UNLIKELY(0 == count &&
                      0 != opal_list_get_size(&ompi_mtl_portals4.flowctl.pending_sends))) {
        ompi_mtl_portals4_pending_list_progress();
    }
#endif

    return count;
}

