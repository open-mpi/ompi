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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "ompi/runtime/ompi_module_exchange.h"

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

OMPI_MODULE_DECLSPEC extern mca_btl_portals4_component_t mca_btl_portals4_component;

mca_btl_portals4_component_t mca_btl_portals4_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        MCA_BTL_BASE_VERSION_2_0_0,

        "portals4", /* MCA module name */
        OMPI_MAJOR_VERSION,  /* MCA module major version */
        OMPI_MINOR_VERSION,  /* MCA module minor version */
        OMPI_RELEASE_VERSION,  /* MCA module release version */
        mca_btl_portals4_component_open,  /* module open */
        mca_btl_portals4_component_close,  /* module close */
        NULL, /* component query */
        mca_btl_portals4_component_register, /* component register */
      },
      {
          /* The component is not checkpoint ready */
          MCA_BASE_METADATA_PARAM_NONE
      },
      
      mca_btl_portals4_component_init,  
      mca_btl_portals4_component_progress,
    }
};

static int
mca_btl_portals4_component_register(void)
{
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
    return OMPI_SUCCESS;
}

static int
mca_btl_portals4_component_open(void)
{
    OPAL_OUTPUT_VERBOSE((1, ompi_btl_base_framework.framework_output, "mca_btl_portals4_component_open\n"));

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
        MCA_BTL_FLAGS_RDMA_MATCHED;
    mca_btl_portals4_module.super.btl_seg_size = sizeof (mca_btl_portals4_segment_t);
    mca_btl_portals4_module.super.btl_bandwidth = 1000;
    mca_btl_portals4_module.super.btl_latency = 0;

    mca_btl_base_param_register(&mca_btl_portals4_component.super.btl_version, &mca_btl_portals4_module.super);

    mca_btl_portals4_module.portals_num_procs = 0;

    mca_btl_portals4_module.recv_eq_h = PTL_EQ_NONE;

#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
    mca_btl_portals4_module.send_md_hs = NULL;
#else
    mca_btl_portals4_module.send_md_h = PTL_INVALID_HANDLE;
#endif

    mca_btl_portals4_module.portals_ni_h = PTL_INVALID_HANDLE;
    mca_btl_portals4_module.zero_md_h = PTL_INVALID_HANDLE;

    mca_btl_portals4_module.long_overflow_me_h = PTL_INVALID_HANDLE;
    mca_btl_portals4_module.portals_outstanding_ops = 0;
    mca_btl_portals4_module.recv_idx = (ptl_pt_index_t) ~0UL;

    return OMPI_SUCCESS;
}


static int
mca_btl_portals4_component_close(void)
{
    opal_output_verbose(50, ompi_btl_base_framework.framework_output, "mca_btl_portals4_component_close\n");

    /* release resources */
    /* close debugging stream */
    opal_output_close(ompi_btl_base_framework.framework_output);
    ompi_btl_base_framework.framework_output = -1;

    if (NULL != mca_btl_portals4_component.btls)  free(mca_btl_portals4_component.btls);
    if (NULL != mca_btl_portals4_component.eqs_h) free(mca_btl_portals4_component.eqs_h);
    mca_btl_portals4_component.btls = NULL;
    mca_btl_portals4_component.eqs_h = NULL;

    PtlFini();

    return OMPI_SUCCESS;
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
    ptl_md_t md;
    ptl_me_t me;

    opal_output_verbose(50, ompi_btl_base_framework.framework_output, "mca_btl_portals4_component_init\n");

    if (enable_progress_threads || enable_mpi_threads) {
        opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "btl portals4 disabled because threads enabled");
        return NULL;
    }

    /* Initialize Portals */
    ret = PtlInit();
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "PtlInit OK\n"));

    /*
     * Initialize the network interfaces (try to open the interfaces 0 to (max_btls-1) )
     */
    *num_btls = 0;
    portals4_nis_h = malloc(mca_btl_portals4_component.max_btls * sizeof(ptl_handle_ni_t));
    for (interface=0; interface<mca_btl_portals4_component.max_btls; interface++) {

        ret = PtlNIInit((1 == mca_btl_portals4_component.max_btls) ? PTL_IFACE_DEFAULT : interface,
                    PTL_NI_PHYSICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,       /* let library assign our pid */
                    NULL,              /* no desired limits */
                    NULL,              /* actual limits */
                    &portals4_nis_h[*num_btls] /* our interface handle */
                    );
        if (PTL_OK != ret) {
            opal_output_verbose(90, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlNIInit failed for NI %d: %d\n", __FILE__, __LINE__, interface, ret);
        }
        else {
            OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "PtlNIInit OK for NI %d\n", *num_btls));
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

        OBJ_CONSTRUCT(&(portals4_btl->portals_frag_eager), ompi_free_list_t);
        OBJ_CONSTRUCT(&(portals4_btl->portals_frag_max), ompi_free_list_t);
        OBJ_CONSTRUCT(&(portals4_btl->portals_frag_user), ompi_free_list_t);

        /* eager frags */
        ompi_free_list_init_new(&(portals4_btl->portals_frag_eager),
                        sizeof(mca_btl_portals4_frag_eager_t) +
                        portals4_btl->super.btl_eager_limit,
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_portals4_frag_eager_t),
                        0,opal_cache_line_size,
                        mca_btl_portals4_component.portals_free_list_init_num,
                        mca_btl_portals4_component.portals_free_list_eager_max_num,
                        mca_btl_portals4_component.portals_free_list_inc_num,
                        NULL);

        /* send frags */
        ompi_free_list_init_new(&(portals4_btl->portals_frag_max),
                        sizeof(mca_btl_portals4_frag_max_t) +
                        portals4_btl->super.btl_max_send_size,
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_portals4_frag_max_t),
                        0,opal_cache_line_size,
                        mca_btl_portals4_component.portals_free_list_init_num,
                        mca_btl_portals4_component.portals_free_list_max_num,
                        mca_btl_portals4_component.portals_free_list_inc_num,
                        NULL);

        /* user frags */
        ompi_free_list_init_new(&(portals4_btl->portals_frag_user),
                        sizeof(mca_btl_portals4_frag_user_t),
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_portals4_frag_user_t),
                        0,opal_cache_line_size,
                        mca_btl_portals4_component.portals_free_list_init_num,
                        mca_btl_portals4_component.portals_free_list_max_num,
                        mca_btl_portals4_component.portals_free_list_inc_num,
                        NULL);

        /* receive block list */
        OBJ_CONSTRUCT(&(portals4_btl->portals_recv_blocks), opal_list_t);

        /* create event queue */
        ret = PtlEQAlloc(portals4_btl->portals_ni_h,
                     mca_btl_portals4_component.recv_queue_size,
                     &portals4_btl->recv_eq_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlEQAlloc failed for NI %d: %d\n",
                            __FILE__, __LINE__, interface, ret);
            goto error; 
        }
        mca_btl_portals4_component.eqs_h[interface] = portals4_btl->recv_eq_h;
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
            "PtlEQAlloc (recv_eq=%d) OK for NI %d\n", portals4_btl->recv_eq_h, interface));

        /* Create recv_idx portal table entrie */
        ret = PtlPTAlloc(portals4_btl->portals_ni_h,
                     PTL_PT_ONLY_USE_ONCE | 
                     PTL_PT_ONLY_TRUNCATE,
                     portals4_btl->recv_eq_h,
                     PTL_PT_ANY,
                     &portals4_btl->recv_idx);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed for NI %d: %d\n",
                            __FILE__, __LINE__, interface, ret);
            goto error;      
        }
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
            "PtlPTAlloc (recv_idx) OK for NI %d recv_idx=%d\n", interface, portals4_btl->recv_idx));

        /* bind zero-length md for sending acks */
        md.start     = NULL;
        md.length    = 0;
        md.options   = 0;
        md.eq_handle = PTL_EQ_NONE;
        md.ct_handle = PTL_CT_NONE;

        ret = PtlMDBind(portals4_btl->portals_ni_h,
                    &md,
                    &portals4_btl->zero_md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed for NI %d: %d\n",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
            "PtlMDBind (zero-length md=%d) OK for NI %d\n", portals4_btl->zero_md_h, interface));

        /* Bind MD/MDs across all memory.  We prefer (for obvious reasons)
           to have a single MD across all of memory */
#if OMPI_PORTALS4_MAX_MD_SIZE < OMPI_PORTALS4_MAX_VA_SIZE
        {
            int i;
            int num_mds = mca_btl_portals4_get_num_mds();
            ptl_size_t size = (1ULL << OMPI_PORTALS4_MAX_MD_SIZE) - 1;
            ptl_size_t offset_unit = (1ULL << OMPI_PORTALS4_MAX_MD_SIZE) / 2;

            portals4_btl->send_md_hs = malloc(sizeof(ptl_handle_md_t) * num_mds);
            if (NULL == portals4_btl->send_md_hs) {
                opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                                "%s:%d: Error allocating MD array",
                                __FILE__, __LINE__);
                ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
                goto error;
            }

            for (i = 0 ; i < num_mds ; ++i) {
                portals4_btl->send_md_hs[i] = PTL_INVALID_HANDLE;
            }

            for (i = 0 ; i < num_mds ; ++i) {
                md.start = (char*) (offset_unit * i);
                md.length = (i - 1 == num_mds) ? size / 2 : size;
                md.options = 0;
                md.eq_handle = portals4_btl->recv_eq_h;
                md.ct_handle = PTL_CT_NONE;

                opal_output_verbose(50, ompi_btl_base_framework.framework_output,
                                "Binding md from %p of length %lx",
                                md.start, md.length);

                ret = PtlMDBind(portals4_btl->portals_ni_h,
                            &md,
                            &portals4_btl->send_md_hs[i]); 
                if (PTL_OK != ret) {
                    opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                                    "%s:%d: PtlMDBind failed for NI %d: %d\n",
                                    __FILE__, __LINE__, interface, ret);
                    goto error;
                }
            }
            OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "PtlMDBind (all memory) OK for NI %d\n", interface));
        }
#else
        md.start = 0;
        md.length = PTL_SIZE_MAX;
        md.options = 0;
        md.eq_handle = portals4_btl->recv_eq_h;
        md.ct_handle = PTL_CT_NONE;

        ret = PtlMDBind(portals4_btl->portals_ni_h,
                    &md,
                    &portals4_btl->send_md_h); 
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed for NI %d: %d\n",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }
#endif

        /* Handle long overflows */
        me.start = NULL;
        me.length = 0;
        me.ct_handle = PTL_CT_NONE;
        me.min_free = 0;
        me.uid = PTL_UID_ANY;
        me.options = PTL_ME_OP_PUT |
            PTL_ME_EVENT_LINK_DISABLE |
            PTL_ME_EVENT_COMM_DISABLE |
            PTL_ME_EVENT_UNLINK_DISABLE;
        me.match_id.phys.nid = PTL_NID_ANY;
        me.match_id.phys.pid = PTL_PID_ANY;
        me.match_bits = BTL_PORTALS4_LONG_MSG;
        me.ignore_bits = BTL_PORTALS4_CONTEXT_MASK |
            BTL_PORTALS4_SOURCE_MASK |
            BTL_PORTALS4_TAG_MASK;
        ret = PtlMEAppend(portals4_btl->portals_ni_h,
                      portals4_btl->recv_idx,
                      &me,
                      PTL_OVERFLOW_LIST,
                      NULL,
                      &portals4_btl->long_overflow_me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed for NI %d: %d\n",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "PtlMEAppend (overflow list) OK for NI %d\n", interface));
    }
    free(portals4_nis_h);
    portals4_nis_h = NULL;

    /* Publish our NID(s)/PID(s) in the modex */
    for (interface=0; interface<mca_btl_portals4_component.num_btls; interface++) {
        portals4_btl = mca_btl_portals4_component.btls[interface];
        ret = PtlGetId(portals4_btl->portals_ni_h ,&ptl_process_ids[interface]);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "%s:%d: PtlGetId for NI %d failed: %d\n",
                            __FILE__, __LINE__, interface, ret);
            goto error;
        }
        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                  "PtlGetId NI number %d: ni_h=%d PtlGetId : nid=%x pid=%x\n",
                  interface, portals4_btl->portals_ni_h,
                  ptl_process_ids[interface].phys.nid, ptl_process_ids[interface].phys.pid));
    }
    ret = ompi_modex_send(&mca_btl_portals4_component.super.btl_version,
                          ptl_process_ids, mca_btl_portals4_component.num_btls * sizeof(ptl_process_t));
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                        "%s:%d: ompi_modex_send failed: %d\n",
                        __FILE__, __LINE__, ret);
        goto error;
    }
    free(ptl_process_ids);
    ptl_process_ids = NULL;

    btls = malloc(mca_btl_portals4_component.num_btls * sizeof(mca_btl_portals4_module_t*) );
    memcpy(btls , mca_btl_portals4_component.btls, 
            mca_btl_portals4_component.num_btls*sizeof(mca_btl_portals4_module_t*) );

    opal_output_verbose(1, ompi_btl_base_framework.framework_output, "The btl portals4 component has been initialized and uses %d NI(s)",
        mca_btl_portals4_component.num_btls);

    return btls;

 error:
    opal_output_verbose(1, ompi_btl_base_framework.framework_output, "Error in mca_btl_portals4_component_init\n");

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

    while (true) {
        ret = PtlEQPoll(mca_btl_portals4_component.eqs_h, mca_btl_portals4_component.num_btls, 0, &ev, &which);

        if (PTL_OK == ret) {
            OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output, "PtlEQPoll Event received: %d (%d) on NI %d\n",
                ev.type, ev.ni_fail_type, which));
            num_progressed++;
            portals4_btl = mca_btl_portals4_component.btls[which];

            switch (ev.type) {

            case PTL_EVENT_SEND:   /* generated on source (origin) when put stops sending */

                frag = ev.user_ptr;
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

                if (!mca_btl_portals4_component.portals_need_ack) {
                    /* my part's done, in portals we trust! */
                    if( MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ){
                        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                            "PTL_EVENT_SEND: Direct call to des_cbfunc: %lx\n", (uint64_t)frag->base.des_cbfunc));
                        frag->base.des_cbfunc(&portals4_btl->super,
                                              frag->endpoint,
                                              &frag->base,
                                              OMPI_SUCCESS);
                    }
                    if (btl_ownership) {
                        mca_btl_portals4_free(&portals4_btl->super, &frag->base);
                    }
                    if (0 != frag->size) {
                        OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
                        OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                            "PTL_EVENT_SEND: Decrementing portals_outstanding_ops=%d (1)\n",
                            portals4_btl->portals_outstanding_ops));
                    }
                }

                goto done;
                break;

            case PTL_EVENT_ACK:   /* Ack that a put as completed on other side. We just call the callback function */

                frag = ev.user_ptr;
                OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                    "PTL_EVENT_ACK received rlength=%ld mlength=%ld des_flags=%d\n", ev.rlength, ev.mlength, frag->base.des_flags));
                btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);

                /* other side received the message.  should have
                   received entire thing */
                /* let the PML know we're done */
                if (MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags ) {
                    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                        "PTL_EVENT_ACK: Call to des_cbfunc %lx\n", (uint64_t)frag->base.des_cbfunc));
                    frag->base.des_cbfunc(&portals4_btl->super,
                                          frag->endpoint,
                                          &frag->base,
                                          OMPI_SUCCESS);
                }
                if (btl_ownership) {
                    mca_btl_portals4_free(&portals4_btl->super, &frag->base);
                }

                if (0 != frag->size) {
                    OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1);
                    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                        "PTL_EVENT_ACK: Decrementing portals_outstanding_ops=%d (2)\n", portals4_btl->portals_outstanding_ops));
                }

                goto done;
                break;

            case PTL_EVENT_PUT:   /* Generated on destination (target) when a put into memory ends */

                frag = ev.user_ptr;
                tag = (unsigned char) (ev.hdr_data);

                frag->base.des_dst = seg;
                seg[0].seg_addr.pval = ev.start;
                seg[0].seg_len = ev.mlength;

                frag->base.des_dst_cnt = 1;

                reg = mca_btl_base_active_message_trigger + tag;
                OPAL_OUTPUT_VERBOSE((50, ompi_btl_base_framework.framework_output,
                                     "PTL_EVENT_PUT: tag=%x frag=%p cbfunc: %lx\n", tag, (void*)frag, (uint64_t)reg->cbfunc));
                reg->cbfunc(&portals4_btl->super, tag, &(frag->base), reg->cbdata);

                goto done;
                break;

            case PTL_EVENT_PUT_OVERFLOW:
                /* */
                goto done;
                break;

            case PTL_EVENT_LINK:
                /* */
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

            case PTL_EVENT_GET:
                /* */
                goto done;
                break;

            case PTL_EVENT_REPLY:
                /* */
                frag = ev.user_ptr;

                if (PTL_NI_PERM_VIOLATION == ev.ni_fail_type) {
                        opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                            "Warning : PTL_EVENT_REPLY with PTL_NI_PERM_VIOLATION received, try to re-issue a PtlGet");

                    /* The distant PtlMEAppend is not finished (distant PTL_EVENT_LINK not received) */
                    /* Re-issue the PtlGet (see btl_portals4_rdma.c) */
                    ret = PtlGet(frag->md_h,
                                 0,
                                 frag->length,
                                 frag->peer_proc,
                                 portals4_btl->recv_idx,
                                 frag->match_bits, /* match bits */
                                 0,
                                 frag);
                    if (OPAL_UNLIKELY(PTL_OK != ret)) {
                        opal_output_verbose(1, ompi_btl_base_framework.framework_output,
                                            "%s:%d: Re-issued PtlGet failed: %d",
                                            __FILE__, __LINE__, ret);
                        PtlMDRelease(frag->md_h);
                        frag->md_h = PTL_INVALID_HANDLE;
                        return OMPI_ERROR;
                    }

                    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                        "Re-issued PtlGet length=%ld recv_idx=%d pid=%x match_bits=%lx\n",
                        frag->length, portals4_btl->recv_idx, frag->peer_proc.phys.pid, frag->match_bits));
                }
                else {
                    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
                        "PTL_EVENT_REPLY: Call to des_cbfunc: %lx\n", (uint64_t)frag->base.des_cbfunc));
                    frag->base.des_cbfunc(&portals4_btl->super,
                                      frag->endpoint,
                                      &frag->base,
                                      OMPI_SUCCESS);
                    PtlMDRelease(frag->md_h);
                    frag->md_h = PTL_INVALID_HANDLE;

                    OMPI_BTL_PORTALS4_FRAG_RETURN_USER(&portals4_btl->super, frag);
                    OPAL_THREAD_ADD32(&portals4_btl->portals_outstanding_ops, -1); 
                    OPAL_OUTPUT_VERBOSE((90, ompi_btl_base_framework.framework_output,
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
            opal_output(ompi_btl_base_framework.framework_output,
                        "Flow control situation without recovery (EQ_DROPPED)");
            break;
        } else {
            opal_output(ompi_btl_base_framework.framework_output,
                        "Error returned from PtlEQPoll: %d", ret);
            break;
        }
    }
 done:
    return num_progressed;
}
