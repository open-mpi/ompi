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

    {
        MCA_MTL_BASE_VERSION_2_0_0,

        "portals4", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        ompi_mtl_portals4_component_open,  /* component open */
        ompi_mtl_portals4_component_close,  /* component close */
        ompi_mtl_portals4_component_query,  /* component close */
        ompi_mtl_portals4_component_register
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },

    ompi_mtl_portals4_component_init,  /* component init */
};

static mca_base_var_enum_value_t long_protocol_values[] = {
    {eager, "eager"},
    {rndv, "rndv"},
    {0, NULL}
};

static int
ompi_mtl_portals4_component_register(void)
{
    mca_base_var_enum_t *new_enum;
    int ret;

    param_priority = 10;
    (void) mca_base_component_var_register (&mca_mtl_portals4_component.mtl_version,
                                            "priority", "Priority of the Portals4 MTL component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &param_priority);

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

    ompi_mtl_portals4.ni_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.send_eq_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.recv_eq_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.zero_md_h = PTL_INVALID_HANDLE;

#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    ompi_mtl_portals4.send_md_hs = NULL;
#else
    ompi_mtl_portals4.send_md_h = PTL_INVALID_HANDLE;
#endif

    ompi_mtl_portals4.long_overflow_me_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.recv_idx = (ptl_pt_index_t) ~0UL;
    ompi_mtl_portals4.read_idx = (ptl_pt_index_t) ~0UL;

    return OMPI_SUCCESS;
}

static int
ompi_mtl_portals4_component_query(mca_base_module_t **module, int *priority)
{
    /*
     * assume if portals4 MTL was compiled, the user wants it
     */
 
    *priority = param_priority;
    *module = (mca_base_module_t *)&ompi_mtl_portals4.base;
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
    ptl_md_t md;
    ptl_me_t me;

    /* Initialize Portals and create a physical, matching interface */
    ret = PtlInit();
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        return NULL;
    }

    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_PHYSICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,
                    NULL,
                    NULL,
                    &ompi_mtl_portals4.ni_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlNIInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ret = PtlGetUid(ompi_mtl_portals4.ni_h, &ompi_mtl_portals4.uid);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlGetUid failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Publish our NID/PID in the modex */
    ret = PtlGetId(ompi_mtl_portals4.ni_h, &id);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlGetId failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    OPAL_MODEX_SEND(ret, PMIX_SYNC_REQD, PMIX_GLOBAL,
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

    /* create event queues */
    ret = PtlEQAlloc(ompi_mtl_portals4.ni_h,
                     ompi_mtl_portals4.send_queue_size,
                     &ompi_mtl_portals4.send_eq_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlEQAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    ret = PtlEQAlloc(ompi_mtl_portals4.ni_h,
                     ompi_mtl_portals4.recv_queue_size,
                     &ompi_mtl_portals4.recv_eq_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlEQAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Create send and long message (read) portal table entries */
    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_ONLY_USE_ONCE | 
                     PTL_PT_ONLY_TRUNCATE | 
                     PTL_PT_FLOWCTRL,
                     ompi_mtl_portals4.recv_eq_h,
                     REQ_RECV_TABLE_ID,
                     &ompi_mtl_portals4.recv_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_ONLY_USE_ONCE |
                     PTL_PT_ONLY_TRUNCATE,
                     ompi_mtl_portals4.send_eq_h,
                     REQ_READ_TABLE_ID,
                     &ompi_mtl_portals4.read_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* bind zero-length md for sending acks */
    md.start     = NULL;
    md.length    = 0;
    md.options   = 0;
    md.eq_handle = PTL_EQ_NONE;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                    &md,
                    &ompi_mtl_portals4.zero_md_h); 
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Bind MD/MDs across all memory.  We prefer (for obvious reasons)
       to have a single MD across all of memory */
#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    {
        int i;
        int num_mds = ompi_mtl_portals4_get_num_mds();
        ptl_size_t size = (1ULL << OPAL_PORTALS4_MAX_MD_SIZE) - 1;
        ptl_size_t offset_unit = (1ULL << OPAL_PORTALS4_MAX_MD_SIZE) / 2;

        ompi_mtl_portals4.send_md_hs = malloc(sizeof(ptl_handle_md_t) * num_mds);
        if (NULL == ompi_mtl_portals4.send_md_hs) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: Error allocating MD array",
                                __FILE__, __LINE__);
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto error;
        }

        for (i = 0 ; i < num_mds ; ++i) {
            ompi_mtl_portals4.send_md_hs[i] = PTL_INVALID_HANDLE;
        }

        for (i = 0 ; i < num_mds ; ++i) {
            md.start = (char*) (offset_unit * i);
            md.length = (i - 1 == num_mds) ? size / 2 : size;
            md.options = 0;
            md.eq_handle = ompi_mtl_portals4.send_eq_h;
            md.ct_handle = PTL_CT_NONE;

            opal_output_verbose(50, ompi_mtl_base_framework.framework_output,
                                "Binding md from %p of length %lx",
                                md.start, md.length);

            ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                            &md,
                            &ompi_mtl_portals4.send_md_hs[i]); 
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: PtlMDBind failed: %d\n",
                                    __FILE__, __LINE__, ret);
                goto error;
            }
        }
    }
#else
        md.start = 0;
        md.length = PTL_SIZE_MAX;
        md.options = 0;
        md.eq_handle = ompi_mtl_portals4.send_eq_h;
        md.ct_handle = PTL_CT_NONE;

        ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                        &md,
                        &ompi_mtl_portals4.send_md_h); 
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlMDBind failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto error;
        }
#endif

    /* Handle long overflows */
    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = ompi_mtl_portals4.uid;
    me.options = PTL_ME_OP_PUT | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_COMM_DISABLE | 
        PTL_ME_EVENT_UNLINK_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = MTL_PORTALS4_LONG_MSG;
    me.ignore_bits = MTL_PORTALS4_CONTEXT_MASK | 
        MTL_PORTALS4_SOURCE_MASK | 
        MTL_PORTALS4_TAG_MASK;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.recv_idx,
                      &me,
                      PTL_OVERFLOW_LIST,
                      NULL,
                      &ompi_mtl_portals4.long_overflow_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* attach short unex recv blocks */
    ret = ompi_mtl_portals4_recv_short_init();
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: short receive block initialization failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ompi_mtl_portals4.opcount = 0;
#if OPAL_ENABLE_DEBUG
    ompi_mtl_portals4.recv_opcount = 0;
#endif

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    ret = ompi_mtl_portals4_flowctl_init();
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: ompi_mtl_portals4_flowctl_init failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
#endif

    /* activate progress callback */
    ret = opal_progress_register(ompi_mtl_portals4_progress);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: opal_progress_register failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    return &ompi_mtl_portals4.base;

 error:
    if (!PtlHandleIsEqual(ompi_mtl_portals4.long_overflow_me_h, PTL_INVALID_HANDLE)) {
        PtlMEUnlink(ompi_mtl_portals4.long_overflow_me_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.zero_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(ompi_mtl_portals4.zero_md_h);
    }
#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    if (NULL != ompi_mtl_portals4.send_md_hs) {
        int i;
        int num_mds = ompi_mtl_portals4_get_num_mds();

        for (i = 0 ; i < num_mds ; ++i) {
            if (!PtlHandleIsEqual(ompi_mtl_portals4.send_md_hs[i], PTL_INVALID_HANDLE)) {
                PtlMDRelease(ompi_mtl_portals4.send_md_hs[i]);
            }
        }

        free(ompi_mtl_portals4.send_md_hs);
    }
#else
    if (!PtlHandleIsEqual(ompi_mtl_portals4.send_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(ompi_mtl_portals4.send_md_h);
    }
#endif
    if (ompi_mtl_portals4.read_idx != (ptl_pt_index_t) ~0UL) {
        PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.read_idx);
    }
    if (ompi_mtl_portals4.recv_idx != (ptl_pt_index_t) ~0UL) {
        PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.recv_idx);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.send_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(ompi_mtl_portals4.send_eq_h);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.recv_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(ompi_mtl_portals4.recv_eq_h);
    }
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
            case PTL_EVENT_REPLY:
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
