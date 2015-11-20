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
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/show_help.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/mpool/base/base.h"

#include "portals4.h"
#include "btl_portals4.h"
#include "btl_portals4_frag.h"
#include "btl_portals4_recv.h"

static int mca_btl_portals4_component_register(void);
static int mca_btl_portals4_component_open(void);
static int mca_btl_portals4_component_close(void);
static mca_btl_base_module_t** mca_btl_portals4_component_init(int *num_btls,
                                                       bool enable_progress_threads,
                                                       bool enable_mpi_threads);
int mca_btl_portals4_component_progress(void);

OPAL_MODULE_DECLSPEC extern mca_btl_portals4_component_t mca_btl_portals4_component;

mca_btl_portals4_component_t mca_btl_portals4_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      .btl_version = {
	MCA_BTL_DEFAULT_VERSION("portals4"),
        .mca_open_component = mca_btl_portals4_component_open,
        .mca_close_component = mca_btl_portals4_component_close,
        .mca_register_component_params = mca_btl_portals4_component_register,
      },
      .btl_data = {
          /* The component is not checkpoint ready */
          .param_field = MCA_BASE_METADATA_PARAM_NONE
      },

      .btl_init = mca_btl_portals4_component_init,
      .btl_progress = mca_btl_portals4_component_progress,
    }
};

static int
mca_btl_portals4_component_register(void)
{
    mca_btl_portals4_component.use_logical = 0;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "use_logical",
                           "Use the logical to physical table to accelerate portals4 adressing: 1 (true) : 0 (false)",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &mca_btl_portals4_component.use_logical);

    mca_btl_portals4_component.max_btls = 1;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "max_btls",
                           "Maximum number of accepted Portals4 cards",
                           MCA_BASE_VAR_TYPE_UNSIGNED_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &mca_btl_portals4_component.max_btls);

    mca_btl_portals4_component.portals_free_list_init_num = 16;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "free_list_init_num",
                           "Initial number of elements to initialize in free lists",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.portals_free_list_init_num));

    mca_btl_portals4_component.portals_free_list_max_num = 1024;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "free_list_max_num",
                           "Max number of elements to initialize in free lists",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.portals_free_list_max_num));

    mca_btl_portals4_component.portals_free_list_inc_num = 16;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "free_list_inc_num",
                           "Increment count for free lists",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.portals_free_list_inc_num));

    mca_btl_portals4_component.portals_free_list_eager_max_num = 32;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "eager_frag_limit",
                           "Maximum number of pre-pinned eager fragments",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.portals_free_list_eager_max_num));

    mca_btl_portals4_component.portals_need_ack =  1; /* default to true.. */
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "needs_ack",
                           "Require a portals level ACK",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.portals_need_ack));

    mca_btl_portals4_component.recv_queue_size = 4 * 1024;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "eq_recv_size",
                           "Size of the receive event queue",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.recv_queue_size));

    mca_btl_portals4_component.portals_max_outstanding_ops = 8 * 1024;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "max_pending_ops",
                           "Maximum number of pending send/rdma frags",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.portals_max_outstanding_ops));

    mca_btl_portals4_component.portals_recv_mds_num = 8;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "recv_md_num",
                           "Number of send frag receive descriptors",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.portals_recv_mds_num));

    mca_btl_portals4_component.portals_recv_mds_size = 256 * 1024;
    (void) mca_base_component_var_register(&mca_btl_portals4_component.super.btl_version,
                           "recv_md_size",
                           "Size of send frag receive descriptors",
                           MCA_BASE_VAR_TYPE_INT,
                           NULL,
                           0,
                           0,
                           OPAL_INFO_LVL_5,
                           MCA_BASE_VAR_SCOPE_READONLY,
                           &(mca_btl_portals4_component.portals_recv_mds_size));
    return OPAL_SUCCESS;
}

static int
mca_btl_portals4_component_open(void)
{
    OPAL_OUTPUT_VERBOSE((1, opal_btl_base_framework.framework_output, "mca_btl_portals4_component_open\n"));

    /*
     * fill default module state
     */
    mca_btl_portals4_module.super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_LOW + 100;
    mca_btl_portals4_module.super.btl_eager_limit = 32 * 1024;
    mca_btl_portals4_module.super.btl_rndv_eager_limit = 32 * 1024;
    mca_btl_portals4_module.super.btl_max_send_size = 64 * 1024;
    mca_btl_portals4_module.super.btl_rdma_pipeline_send_length = 64 * 1024;
    mca_btl_portals4_module.super.btl_rdma_pipeline_frag_size = INT_MAX;
    mca_btl_portals4_module.super.btl_min_rdma_pipeline_size = 0;
    mca_btl_portals4_module.super.btl_flags =
        MCA_BTL_FLAGS_RDMA |
        MCA_BTL_FLAGS_RDMA_MATCHED |
        MCA_BTL_FLAGS_SEND;

    mca_btl_portals4_module.super.btl_registration_handle_size = sizeof (mca_btl_base_registration_handle_t);

    mca_btl_portals4_module.super.btl_get_limit = SIZE_MAX;
    mca_btl_portals4_module.super.btl_put_limit = 0;        /* not implemented */
    mca_btl_portals4_module.super.btl_get_alignment = 0;
    mca_btl_portals4_module.super.btl_put_alignment = 0;

    mca_btl_portals4_module.super.btl_get_local_registration_threshold = 0;
    mca_btl_portals4_module.super.btl_put_local_registration_threshold = 0;

    mca_btl_portals4_module.super.btl_bandwidth = 1000;
    mca_btl_portals4_module.super.btl_latency = 0;

    mca_btl_base_param_register(&mca_btl_portals4_component.super.btl_version, &mca_btl_portals4_module.super);

    mca_btl_portals4_module.portals_num_procs = 0;

    mca_btl_portals4_module.recv_eq_h = PTL_EQ_NONE;

    mca_btl_portals4_module.send_md_h = PTL_INVALID_HANDLE;

    mca_btl_portals4_module.portals_ni_h = PTL_INVALID_HANDLE;
    mca_btl_portals4_module.zero_md_h = PTL_INVALID_HANDLE;

    mca_btl_portals4_module.long_overflow_me_h = PTL_INVALID_HANDLE;
    mca_btl_portals4_module.portals_outstanding_ops = 0;
    mca_btl_portals4_module.recv_idx = (ptl_pt_index_t) ~0UL;

    if (1 == mca_btl_portals4_component.use_logical) {
        /*
         * set the MCA_BTL_FLAGS_SINGLE_ADD_PROCS flag here in the default
         * module, so it gets copied into the module for each Portals4
         * interface during init().
         */
        mca_btl_portals4_module.super.btl_flags |= MCA_BTL_FLAGS_SINGLE_ADD_PROCS;
    }

    return OPAL_SUCCESS;
}


static int
mca_btl_portals4_component_close(void)
{
    opal_output_verbose(50, opal_btl_base_framework.framework_output, "mca_btl_portals4_component_close\n");

    /* release resources */
    /* close debugging stream */
    opal_output_close(opal_btl_base_framework.framework_output);
    opal_btl_base_framework.framework_output = -1;

    if (NULL != mca_btl_portals4_component.btls)  free(mca_btl_portals4_component.btls);
    if (NULL != mca_btl_portals4_component.eqs_h) free(mca_btl_portals4_component.eqs_h);
    mca_btl_portals4_component.btls = NULL;
    mca_btl_portals4_component.eqs_h = NULL;

    PtlFini();

    return OPAL_SUCCESS;
}

static mca_btl_base_module_t** mca_btl_portals4_component_init(int *num_btls,
                                                       bool enable_progress_threads,
                                                       bool enable_mpi_threads)
{
    mca_btl_portals4_module_t *portals4_btl = NULL;
    mca_btl_base_module_t **btls = NULL;
    unsigned int ret, interface;
    ptl_handle_ni_t *portals4_nis_h = NULL;
    ptl_process_t *ptl_process_ids = NULL;

    opal_output_verbose(50, opal_btl_base_framework.framework_output, "mca_btl_portals4_component_init\n");

    if (enable_mpi_threads && !mca_btl_base_thread_multiple_override) {
        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "btl portals4 disabled because threads enabled");
        return NULL;
    }

    /* Initialize Portals */
    ret = PtlInit();
    if (PTL_OK != ret) {
        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "PtlInit OK\n"));

    /*
     * Initialize the network interfaces (try to open the interfaces 0 to (max_btls-1) )
     */
    *num_btls = 0;
    portals4_nis_h = malloc(mca_btl_portals4_component.max_btls * sizeof(ptl_handle_ni_t));
    for (interface=0; interface<mca_btl_portals4_component.max_btls; interface++) {

        if (mca_btl_portals4_component.use_logical)
            ret = PtlNIInit((1 == mca_btl_portals4_component.max_btls) ? PTL_IFACE_DEFAULT : interface,
                    PTL_NI_LOGICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    NULL,              /* actual limits */
                    &portals4_nis_h[*num_btls] /* our interface handle */
                    );
        else ret = PtlNIInit((1 == mca_btl_portals4_component.max_btls) ? PTL_IFACE_DEFAULT : interface,
                    PTL_NI_PHYSICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    NULL,              /* actual limits */
                    &portals4_nis_h[*num_btls] /* our interface handle */
                    );
        if (PTL_OK != ret) {
            opal_output_verbose(90, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlNIInit failed for NI %d: %d\n", __FILE__, __LINE__, interface, ret);
        }
        else {
            OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "PtlNIInit OK for NI %d\n", *num_btls));
            (*num_btls)++;
        }
    }
    if (0 == *num_btls) goto error;

    /*
     * Configure the different network interfaces and the associated btl modules
     */
    mca_btl_portals4_component.num_btls = *num_btls;
    mca_btl_portals4_component.btls = malloc(mca_btl_portals4_component.num_btls * sizeof(mca_btl_portals4_module_t*) );
    mca_btl_portals4_component.eqs_h = malloc(mca_btl_portals4_component.num_btls * sizeof(ptl_handle_eq_t));
    ptl_process_ids = malloc(mca_btl_portals4_component.num_btls * sizeof(ptl_process_t) );

    for (interface=0; interface<mca_btl_portals4_component.num_btls; interface++) {
        mca_btl_portals4_component.btls[interface] = NULL;
        mca_btl_portals4_component.eqs_h[interface] = PTL_EQ_NONE;
    }
    for (interface=0; interface<mca_btl_portals4_component.num_btls; interface++) {
        portals4_btl = malloc(sizeof(mca_btl_portals4_module_t));
        mca_btl_portals4_component.btls[interface] = portals4_btl;

        /* Copy the default module */
        memcpy(portals4_btl, &mca_btl_portals4_module, sizeof(mca_btl_portals4_module_t));

        portals4_btl->interface_num = interface;
        portals4_btl->portals_ni_h = portals4_nis_h[interface];
        portals4_btl->portals_max_outstanding_ops = mca_btl_portals4_component.portals_max_outstanding_ops;

        OBJ_CONSTRUCT(&(portals4_btl->portals_frag_eager), opal_free_list_t);
        OBJ_CONSTRUCT(&(portals4_btl->portals_frag_max), opal_free_list_t);
        OBJ_CONSTRUCT(&(portals4_btl->portals_frag_user), opal_free_list_t);

        /* eager frags */
        opal_free_list_init (&(portals4_btl->portals_frag_eager),
                        sizeof(mca_btl_portals4_frag_eager_t) +
                        portals4_btl->super.btl_eager_limit,
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_portals4_frag_eager_t),
                        0,opal_cache_line_size,
                        mca_btl_portals4_component.portals_free_list_init_num,
                        mca_btl_portals4_component.portals_free_list_eager_max_num,
                        mca_btl_portals4_component.portals_free_list_inc_num,
                        NULL, 0, NULL, NULL, NULL);

        /* send frags */
        opal_free_list_init (&(portals4_btl->portals_frag_max),
                        sizeof(mca_btl_portals4_frag_max_t) +
                        portals4_btl->super.btl_max_send_size,
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_portals4_frag_max_t),
                        0,opal_cache_line_size,
                        mca_btl_portals4_component.portals_free_list_init_num,
                        mca_btl_portals4_component.portals_free_list_max_num,
                        mca_btl_portals4_component.portals_free_list_inc_num,
                        NULL, 0, NULL, NULL, NULL);

        /* user frags */
        opal_free_list_init (&(portals4_btl->portals_frag_user),
                        sizeof(mca_btl_portals4_frag_user_t),
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_portals4_frag_user_t),
                        0,opal_cache_line_size,
                        mca_btl_portals4_component.portals_free_list_init_num,
                        mca_btl_portals4_component.portals_free_list_max_num,
                        mca_btl_portals4_component.portals_free_list_inc_num,
                        NULL, 0, NULL, NULL, NULL);

        /* receive block list */
        OBJ_CONSTRUCT(&(portals4_btl->portals_recv_blocks), opal_list_t);
    }
    free(portals4_nis_h);
    portals4_nis_h = NULL;

    /* Publish our NID(s)/PID(s) in the modex */
    for (interface=0; interface<mca_btl_portals4_component.num_btls; interface++) {
        portals4_btl = mca_btl_portals4_component.btls[interface];

        ret = PtlGetPhysId(portals4_btl->portals_ni_h ,&ptl_process_ids[interface]);
        if (PTL_OK != ret) {
            opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "%s:%d: PtlGetPhysId for NI %d failed: %d\n",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }

        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                  "PtlGetPhysId NI number %d: ni_h=%d rank=%x nid=%x pid=%x\n",
                  interface, portals4_btl->portals_ni_h,
                  ptl_process_ids[interface].rank,
                  ptl_process_ids[interface].phys.nid, ptl_process_ids[interface].phys.pid));
    }
    OPAL_MODEX_SEND(ret, OPAL_PMIX_GLOBAL,
                    &mca_btl_portals4_component.super.btl_version,
                    ptl_process_ids, mca_btl_portals4_component.num_btls * sizeof(ptl_process_t));
    if (OPAL_SUCCESS != ret) {
        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                        "%s:%d: opal_modex_send failed: %d\n",
                        __FILE__, __LINE__, ret);
        goto error;
    }
    free(ptl_process_ids);
    ptl_process_ids = NULL;

    btls = malloc(mca_btl_portals4_component.num_btls * sizeof(mca_btl_portals4_module_t*) );
    memcpy(btls , mca_btl_portals4_component.btls,
            mca_btl_portals4_component.num_btls*sizeof(mca_btl_portals4_module_t*) );

    opal_output_verbose(1, opal_btl_base_framework.framework_output, "The btl portals4 component has been initialized and uses %d NI(s)",
        mca_btl_portals4_component.num_btls);

    mca_btl_portals4_component.need_init = 1;

    return btls;

 error:
    opal_output_verbose(1, opal_btl_base_framework.framework_output, "Error in mca_btl_portals4_component_init\n");

    if (*num_btls) {
        if (NULL != portals4_nis_h) free(portals4_nis_h);
        if (NULL != ptl_process_ids) free(ptl_process_ids);

        for (interface=0; interface<mca_btl_portals4_component.num_btls; interface++) {
            portals4_btl = mca_btl_portals4_component.btls[interface];
            if (NULL != portals4_btl) mca_btl_portals4_free_module(portals4_btl);
        }
        mca_btl_portals4_component.num_btls = 0;
        *num_btls = 0;
        if (NULL != mca_btl_portals4_component.btls)  free(mca_btl_portals4_component.btls);
        if (NULL != mca_btl_portals4_component.eqs_h) free(mca_btl_portals4_component.eqs_h);
        mca_btl_portals4_component.btls = NULL;
        mca_btl_portals4_component.eqs_h = NULL;

    }
 return NULL;
}

int
mca_btl_portals4_get_error(int ptl_error)
{
    int ret;

    switch (ptl_error) {
    case PTL_OK:
        ret = OPAL_SUCCESS;
        break;
    case PTL_ARG_INVALID:
        ret = OPAL_ERR_BAD_PARAM;
        break;
    case PTL_CT_NONE_REACHED:
        ret = OPAL_ERR_TIMEOUT;
        break;
    case PTL_EQ_DROPPED:
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_EQ_EMPTY:
        ret = OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        break;
    case PTL_FAIL:
        ret = OPAL_ERROR;
        break;
    case PTL_IN_USE:
        ret = OPAL_ERR_RESOURCE_BUSY;
        break;
    case PTL_INTERRUPTED:
        ret = OPAL_ERR_RESOURCE_BUSY;
        break;
    case PTL_LIST_TOO_LONG:
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_NO_INIT:
        ret = OPAL_ERR_FATAL;
        break;
    case PTL_NO_SPACE:
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_PID_IN_USE:
        ret = OPAL_ERR_BAD_PARAM;
        break;
    case PTL_PT_FULL:
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        break;
    case PTL_PT_EQ_NEEDED:
        ret = OPAL_ERR_FATAL;
        break;
    case PTL_PT_IN_USE:
        ret = OPAL_ERR_RESOURCE_BUSY;
        break;

    default:
        ret = OPAL_ERROR;
    }

    return ret;
}

int
mca_btl_portals4_component_progress(void)
{
    mca_btl_portals4_module_t *portals4_btl;
    int num_progressed = 0;
    int ret, btl_ownership;
    mca_btl_portals4_frag_t *frag = NULL;
    mca_btl_base_tag_t tag;
    static ptl_event_t ev;
    unsigned int which;
    mca_btl_active_message_callback_t* reg;
    mca_btl_base_segment_t seg[2];
    mca_btl_base_descriptor_t btl_base_descriptor;

    while (true) {
        ret = PtlEQPoll(mca_btl_portals4_component.eqs_h, mca_btl_portals4_component.num_btls, 0, &ev, &which);

        if (PTL_OK == ret) {
            OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output, "PtlEQPoll Event received: %d (fail=%d) on NI %d\n",
                ev.type, ev.ni_fail_type, which));
            num_progressed++;
            portals4_btl = mca_btl_portals4_component.btls[which];

            switch (ev.type) {

            case PTL_EVENT_SEND:   /* generated on source (origin) when put stops sending */

                frag = ev.user_ptr;
                if (NULL == frag) {
                    opal_output(opal_btl_base_framework.framework_output, "btl/portals4: PTL_EVENT_SEND event with NULL user_ptr");
                    break;
                }
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

                if (!mca_btl_portals4_component.portals_need_ack) {
                    /* my part's done, in portals we trust! */
                    if( MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ){
                        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                            "PTL_EVENT_SEND: Direct call to des_cbfunc: %lx\n", (uint64_t)frag->base.des_cbfunc));
                        frag->base.des_cbfunc(&portals4_btl->super,
                                              frag->endpoint,
                                              &frag->base,
                                              OPAL_SUCCESS);
                    }
                    if (btl_ownership) {
                        mca_btl_portals4_free(&portals4_btl->super, &frag->base);
                    }
                    if (0 != frag->size) {
                        OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
                        OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                            "PTL_EVENT_SEND: Decrementing portals_outstanding_ops=%d (1)\n",
                            portals4_btl->portals_outstanding_ops));
                    }
                }

                goto done;
                break;

            case PTL_EVENT_ACK:   /* Ack that a put as completed on other side. We just call the callback function */

                frag = ev.user_ptr;
                if (NULL == frag) {
                    opal_output(opal_btl_base_framework.framework_output, "btl/portals4: PTL_EVENT_ACK event with NULL user_ptr");
                    break;
                }
                OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                    "PTL_EVENT_ACK received rlength=%ld mlength=%ld des_flags=%d\n", ev.rlength, ev.mlength, frag->base.des_flags));
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

                /* other side received the message.  should have
                   received entire thing */
                /* let the PML know we're done */
                if (MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ) {
                    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                        "PTL_EVENT_ACK: Call to des_cbfunc %lx\n", (uint64_t)frag->base.des_cbfunc));
                    frag->base.des_cbfunc(&portals4_btl->super,
                                          frag->endpoint,
                                          &frag->base,
                                          OPAL_SUCCESS);
                }
                if (btl_ownership) {
                    mca_btl_portals4_free(&portals4_btl->super, &frag->base);
                }

                if (0 != frag->size) {
                    OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
                    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                        "PTL_EVENT_ACK: Decrementing portals_outstanding_ops=%d (2)\n", portals4_btl->portals_outstanding_ops));
                }

                goto done;
                break;

            case PTL_EVENT_PUT:   /* Generated on destination (target) when a put into memory ends */

                tag = (unsigned char) (ev.hdr_data);

                btl_base_descriptor.des_segments = seg;
                btl_base_descriptor.des_segment_count = 1;
                seg[0].seg_addr.pval = ev.start;
                seg[0].seg_len = ev.mlength;

                reg = mca_btl_base_active_message_trigger + tag;
                OPAL_OUTPUT_VERBOSE((50, opal_btl_base_framework.framework_output,
                    "PTL_EVENT_PUT: tag=%x base_descriptor=%p cbfunc: %lx\n", tag, (void*)&btl_base_descriptor, (uint64_t)reg->cbfunc));
                reg->cbfunc(&portals4_btl->super, tag, &btl_base_descriptor, reg->cbdata);

                goto done;
                break;

            case PTL_EVENT_PUT_OVERFLOW:
                /* */
                OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                    "PTL_EVENT_OVERFLOW received\n"));
                goto done;
                break;

            case PTL_EVENT_LINK:
                /* */
                frag = ev.user_ptr;
                if (NULL == frag) {
                    opal_output(opal_btl_base_framework.framework_output, "btl/portals4: PTL_EVENT_LINK event with NULL user_ptr");
                    break;
                }
                goto done;
                break;

            case PTL_EVENT_AUTO_UNLINK:
                /* */
                /* The Priority List is used, so PTL_EVENT_AUTO_FREE will never be received. So, we have to reactivate the block here */
                mca_btl_portals4_activate_block(ev.user_ptr);
                goto done;
                break;

            case PTL_EVENT_AUTO_FREE:
                /* */
                goto done;
                break;

            case PTL_EVENT_GET:   /* Generated on source (target) when a get from memory ends */
                /* */
                OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                    "PTL_EVENT_GET received at target rlength=%ld mlength=%ld\n", ev.rlength, ev.mlength));
                goto done;
                break;

            case PTL_EVENT_REPLY:
                /* */
                frag = ev.user_ptr;

                if (PTL_NI_PERM_VIOLATION == ev.ni_fail_type) {
                        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                            "Warning : PTL_EVENT_REPLY with PTL_NI_PERM_VIOLATION received, try to re-issue a PtlGet");

                    /* The distant PtlMEAppend is not finished (distant PTL_EVENT_LINK not received) */
                    /* Re-issue the PtlGet (see btl_portals4_rdma.c) */
                    ret = PtlGet(portals4_btl->send_md_h,
                                 (ptl_size_t) frag->addr,
                                 frag->length,
                                 frag->peer_proc,
                                 portals4_btl->recv_idx,
                                 frag->match_bits, /* match bits */
                                 0,
                                 frag);
                    if (OPAL_UNLIKELY(PTL_OK != ret)) {
                        opal_output_verbose(1, opal_btl_base_framework.framework_output,
                                            "%s:%d: Re-issued PtlGet failed: %d",
                                            __FILE__, __LINE__, ret);
                        return OPAL_ERROR;
                    }

                    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                        "Re-issued PtlGet length=%ld recv_idx=%d rank=%x pid=%x nid=%x match_bits=%lx\n",
                        frag->length, portals4_btl->recv_idx,
                        frag->peer_proc.rank, frag->peer_proc.phys.pid, frag->peer_proc.phys.nid, frag->match_bits));
                }
                else {
                    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                        "PTL_EVENT_REPLY: Call to rdma_cbfunc=%lx\n", (uint64_t)frag->rdma_cb.func));
                    frag->rdma_cb.func(&portals4_btl->super,
                                 frag->endpoint,
                                 ev.start,
                                 frag->rdma_cb.local_handle,
                                 frag->rdma_cb.context,
                                 frag->rdma_cb.data,
                                 OPAL_SUCCESS);

                    OPAL_BTL_PORTALS4_FRAG_RETURN_USER(&portals4_btl->super, frag);
                    OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
                    OPAL_OUTPUT_VERBOSE((90, opal_btl_base_framework.framework_output,
                        "PTL_EVENT_REPLY: Decrementing portals_outstanding_ops=%d\n", portals4_btl->portals_outstanding_ops));
                    goto done;
                }
                break;

            default:
                /* */
                goto done;
                break;
            }
        } else if (PTL_EQ_EMPTY == ret) {
            /* there's nothing in the queue.  This is actually the
               common case, so the easiest way to make the compiler
               emit something that doesn't completely blow here is to
               just go back to a good old goto */
            goto done;
            break;

        } else if (PTL_EQ_DROPPED == ret) {
            opal_output(opal_btl_base_framework.framework_output,
                        "Flow control situation without recovery (EQ_DROPPED)");
            break;
        } else {
            opal_output(opal_btl_base_framework.framework_output,
                        "Error returned from PtlEQPoll: %d", ret);
            break;
        }
    }
 done:
    return num_progressed;
}
