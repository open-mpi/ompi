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
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "mtl_portals4.h"
#include "mtl_portals4_request.h"
#include "mtl_portals4_recv_short.h"


static int ompi_mtl_portals4_component_open(void);
static int ompi_mtl_portals4_component_close(void);
static mca_mtl_base_module_t* ompi_mtl_portals4_component_init(
                                                               bool enable_progress_threads, bool enable_mpi_threads);

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
        ompi_mtl_portals4_component_close  /* component close */
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    },

    ompi_mtl_portals4_component_init,  /* component init */
};


static int
ompi_mtl_portals4_component_open(void)
{
    int tmp;
    char *tmp_proto;

    ompi_mtl_portals4.base.mtl_request_size = 
        sizeof(ompi_mtl_portals4_request_t) -
        sizeof(struct mca_mtl_request_t);

    mca_base_param_reg_int(&mca_mtl_portals4_component.mtl_version,
                           "eager_limit",
                           "Cross-over point from eager to rendezvous sends",
                           false,
                           false,
                           2 * 1024,
                           &tmp);
    ompi_mtl_portals4.eager_limit = tmp;

    mca_base_param_reg_int(&mca_mtl_portals4_component.mtl_version,
                           "short_recv_num",
                           "Number of short message receive blocks",
                           false,
                           false,
                           32,
                           &ompi_mtl_portals4.recv_short_num);

    mca_base_param_reg_int(&mca_mtl_portals4_component.mtl_version,
                           "short_recv_size",
                           "Size of short message receive blocks",
                           false,
                           false,
                           2 * 1024 * 1024,
                           &tmp);
    ompi_mtl_portals4.recv_short_size = tmp;

    mca_base_param_reg_int(&mca_mtl_portals4_component.mtl_version,
                           "queue_size",
                           "Size of the event queue in entries",
                           false,
                           false,
                           1024,
                           &ompi_mtl_portals4.queue_size);

    mca_base_param_reg_string(&mca_mtl_portals4_component.mtl_version,
                              "long_proto",
                              "Protocol to use for long messages.  Valid entries are eager, rndv, and triggered",
                              false,
                              false,
                              "eager",
                              &tmp_proto);
    if (0 == strcmp(tmp_proto, "eager")) {
        ompi_mtl_portals4.protocol = eager;        
    } else if (0 == strcmp(tmp_proto, "rndv")) {
        ompi_mtl_portals4.protocol = rndv;
    } else if (0 == strcmp(tmp_proto, "triggered")) {
        ompi_mtl_portals4.protocol = triggered;
    } else {
        opal_output(ompi_mtl_base_output,
                    "Unknown protocol type %s", tmp_proto);
        return OMPI_ERR_NOT_SUPPORTED;
    }

    opal_output_verbose(1, ompi_mtl_base_output,
                        "Eager limit: %d", (int) ompi_mtl_portals4.eager_limit);
    opal_output_verbose(1, ompi_mtl_base_output, 
                        "Short receive blocks: %d", ompi_mtl_portals4.recv_short_num);
    opal_output_verbose(1, ompi_mtl_base_output, 
                        "Queue size: %d", ompi_mtl_portals4.queue_size);
    opal_output_verbose(1, ompi_mtl_base_output, 
                        "Long protocol: %s", 
                        (ompi_mtl_portals4.protocol == eager) ? "Eager" :
                        (ompi_mtl_portals4.protocol == rndv) ? "Rendezvous" :
                        (ompi_mtl_portals4.protocol == triggered) ? "Triggered" : "Other");

    ompi_mtl_portals4.ni_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.eq_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.zero_md_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.long_overflow_me_h = PTL_INVALID_HANDLE;
    ompi_mtl_portals4.send_idx = (ptl_pt_index_t) ~0UL;
    ompi_mtl_portals4.read_idx = (ptl_pt_index_t) ~0UL;

    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals4_component_close(void)
{
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
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlInit failed: %d\n",
                    __FILE__, __LINE__, ret);
        return NULL;
    }

    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_PHYSICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,
                    NULL,
                    NULL,
                    0,
                    NULL,
                    NULL,
                    &ompi_mtl_portals4.ni_h);
    if (PTL_OK != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlNIInit failed: %d\n",
                    __FILE__, __LINE__, ret);
        goto error;
    }

    /* Publish our NID/PID in the modex */
    ret = PtlGetId(ompi_mtl_portals4.ni_h, &id);
    if (PTL_OK != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlGetId failed: %d\n",
                    __FILE__, __LINE__, ret);
        goto error;
    }

    ret = ompi_modex_send(&mca_mtl_portals4_component.mtl_version,
                          &id, sizeof(id));
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: ompi_modex_send failed: %d\n",
                    __FILE__, __LINE__, ret);
        goto error;
    }

    /* create event queue */
    ret = PtlEQAlloc(ompi_mtl_portals4.ni_h,
                     ompi_mtl_portals4.queue_size,
                     &ompi_mtl_portals4.eq_h);
    if (PTL_OK != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlEQAlloc failed: %d\n",
                    __FILE__, __LINE__, ret);
        goto error;
    }

    /* Create portal table entries */
    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_FLOWCTRL,
                     ompi_mtl_portals4.eq_h,
                     REQ_SEND_TABLE_ID,
                     &ompi_mtl_portals4.send_idx);
    if (PTL_OK != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlPTAlloc failed: %d\n",
                    __FILE__, __LINE__, ret);
        goto error;
    }
    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_FLOWCTRL,
                     ompi_mtl_portals4.eq_h,
                     REQ_READ_TABLE_ID,
                     &ompi_mtl_portals4.read_idx);
    if (PTL_OK != ret) {
        opal_output(ompi_mtl_base_output,
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
                    &ompi_mtl_portals4.zero_md_h ); 
    if (PTL_OK != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlMDBind failed: %d\n",
                    __FILE__, __LINE__, ret);
        goto error;
    }

    /* Handle long overflows */
    if (ompi_mtl_portals4.protocol != triggered) {
        me.start = NULL;
        me.length = 0;
        me.ct_handle = PTL_CT_NONE;
        me.min_free = 0;
        me.ac_id.uid = PTL_UID_ANY;
        me.options = PTL_ME_OP_PUT | PTL_ME_ACK_DISABLE | PTL_ME_EVENT_COMM_DISABLE;
        me.match_id.phys.nid = PTL_NID_ANY;
        me.match_id.phys.pid = PTL_PID_ANY;
        me.match_bits = PTL_LONG_MSG;
        me.ignore_bits = PTL_CONTEXT_MASK | PTL_SOURCE_MASK | PTL_TAG_MASK;
        ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                          ompi_mtl_portals4.send_idx,
                          &me,
                          PTL_OVERFLOW,
                          NULL,
                          &ompi_mtl_portals4.long_overflow_me_h);
        if (PTL_OK != ret) {
            opal_output(ompi_mtl_base_output,
                        "%s:%d: PtlMEAppend failed: %d\n",
                        __FILE__, __LINE__, ret);
            goto error;
        }
    }

    /* attach short unex recv blocks */
    ret = ompi_mtl_portals4_recv_short_init(&ompi_mtl_portals4);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: short receive block initialization failed: %d\n",
                    __FILE__, __LINE__, ret);
        goto error;
    }

    /* activate progress callback */
    ret = opal_progress_register(ompi_mtl_portals4_progress);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_mtl_base_output,
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
    if (ompi_mtl_portals4.read_idx != (ptl_pt_index_t) ~0UL) {
        PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.read_idx);
    }
    if (ompi_mtl_portals4.send_idx != (ptl_pt_index_t) ~0UL) {
        PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.send_idx);
    }
    if (!PtlHandleIsEqual(ompi_mtl_portals4.eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(ompi_mtl_portals4.eq_h);
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
    case PTL_SIZE_INVALID:
        ret = OMPI_ERR_BAD_PARAM;
        break;
    default:
        ret = OMPI_ERROR;
    }

    return ret;
}
