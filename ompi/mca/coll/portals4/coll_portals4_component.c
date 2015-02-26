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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "coll_portals4.h"
#include "coll_portals4_request.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

const char *mca_coll_portals4_component_version_string =
    "Open MPI Portals 4 collective MCA component version " OMPI_VERSION;

int mca_coll_portals4_priority = 10;

static int portals4_open(void);
static int portals4_close(void);
static int portals4_register(void);
static int portals4_init_query(bool enable_progress_threads,
                               bool enable_mpi_threads);
static mca_coll_base_module_t* portals4_comm_query(struct ompi_communicator_t *comm,
                                                   int *priority);
static int portals4_module_enable(mca_coll_base_module_t *module,
                                  struct ompi_communicator_t *comm);
static int portals4_progress(void);


mca_coll_portals4_component_t mca_coll_portals4_component = {
    {
        /* First, the mca_component_t struct containing meta information
         * about the component itself */

        {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            "portals4",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */
            portals4_open,
            portals4_close,
            NULL,
            portals4_register
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        portals4_init_query,
        portals4_comm_query
    }, 
};


static int
portals4_register(void)
{
    mca_coll_portals4_priority = 100;
    (void) mca_base_component_var_register(&mca_coll_portals4_component.super.collm_version, "priority",
                                           "Priority of the portals4 coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_portals4_priority);

    return OMPI_SUCCESS;
}


static int
portals4_open(void)
{
    int ret;

    mca_coll_portals4_component.ni_h = PTL_INVALID_HANDLE;
    mca_coll_portals4_component.uid = PTL_UID_ANY;
    mca_coll_portals4_component.pt_idx = -1;
    mca_coll_portals4_component.finish_pt_idx = -1;
    mca_coll_portals4_component.eq_h = PTL_INVALID_HANDLE;
    mca_coll_portals4_component.barrier_unex_me_h = PTL_INVALID_HANDLE;
    mca_coll_portals4_component.finish_me_h = PTL_INVALID_HANDLE;
#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    mca_coll_portals4_component.md_hs = NULL;
#else
    mca_coll_portals4_component.md_h = PTL_INVALID_HANDLE;
#endif
    
    OBJ_CONSTRUCT(&mca_coll_portals4_component.requests, opal_free_list_t);
    ret = opal_free_list_init (&mca_coll_portals4_component.requests,
                               sizeof(ompi_coll_portals4_request_t),
                               opal_cache_line_size,
                               OBJ_CLASS(ompi_coll_portals4_request_t),
                               0, 0, 8, 0, 8, NULL, 0, NULL, NULL, NULL);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: opal_free_list_init failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    return OMPI_SUCCESS;
}


static int
portals4_close(void)
{
    int ret;

    OBJ_DESTRUCT(&mca_coll_portals4_component.requests);

#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    if (NULL != mca_coll_portals4_component.md_hs) {
        int i;
        int num_mds = ompi_coll_portals4_get_num_mds();

        for (i = 0 ; i < num_mds ; ++i) {
            if (!PtlHandleIsEqual(mca_coll_portals4_component.md_hs[i], PTL_INVALID_HANDLE)) {
                ret = PtlMDRelease(mca_coll_portals4_component.md_hs[i]);
                if (PTL_OK != ret) {
                    opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                        "%s:%d: PtlMDRelease failed: %d\n",
                                        __FILE__, __LINE__, ret);
                }
            }
        }

        free(mca_coll_portals4_component.md_hs);
    }
#else
    if (!PtlHandleIsEqual(mca_coll_portals4_component.md_h, PTL_INVALID_HANDLE)) {
        ret = PtlMDRelease(mca_coll_portals4_component.md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                "%s:%d: PtlMDRelease failed: %d\n",
                                __FILE__, __LINE__, ret);
        }
    }
#endif
    if (!PtlHandleIsEqual(mca_coll_portals4_component.finish_me_h, PTL_INVALID_HANDLE)) {
        ret = PtlMEUnlink(mca_coll_portals4_component.finish_me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                "%s:%d: PtlMEUnlink failed: %d\n",
                                __FILE__, __LINE__, ret);
        }
    }
    if (!PtlHandleIsEqual(mca_coll_portals4_component.barrier_unex_me_h, PTL_INVALID_HANDLE)) {
        ret = PtlMEUnlink(mca_coll_portals4_component.barrier_unex_me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                "%s:%d: PtlMEUnlink failed: %d\n",
                                __FILE__, __LINE__, ret);
        }
    }
    if (mca_coll_portals4_component.finish_pt_idx >= 0) {
        ret = PtlPTFree(mca_coll_portals4_component.ni_h, mca_coll_portals4_component.finish_pt_idx);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                "%s:%d: PtlPTFree failed: %d\n",
                                __FILE__, __LINE__, ret);
        }
    }
    if (mca_coll_portals4_component.pt_idx >= 0) {
        ret = PtlPTFree(mca_coll_portals4_component.ni_h, mca_coll_portals4_component.pt_idx);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                "%s:%d: PtlPTFree failed: %d\n",
                                __FILE__, __LINE__, ret);
        }
    }
    if (!PtlHandleIsEqual(mca_coll_portals4_component.eq_h, PTL_INVALID_HANDLE)) {
        ret = PtlEQFree(mca_coll_portals4_component.eq_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                "%s:%d: PtlEQFree failed: %d\n",
                                __FILE__, __LINE__, ret);
        }
    }
    if (!PtlHandleIsEqual(mca_coll_portals4_component.ni_h, PTL_INVALID_HANDLE)) {
        ret = PtlNIFini(mca_coll_portals4_component.ni_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                "%s:%d: PtlNIFini failed: %d\n",
                                __FILE__, __LINE__, ret);
        }

        PtlFini();
    }

    opal_progress_unregister(portals4_progress);

    return OMPI_SUCCESS;
}



/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
static int
portals4_init_query(bool enable_progress_threads,
                    bool enable_mpi_threads)
{
    int ret;
    ptl_md_t md;
    ptl_me_t me;

    /* Make sure someone is populating the proc table, since we're not
       in a really good position to do so */
    if (NULL == ompi_proc_local()->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: Proc table not previously populated",
                            __FILE__, __LINE__);
        return OMPI_ERROR;
    }

    /* Initialize Portals and create a physical, matching interface */
    ret = PtlInit();
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_PHYSICAL | PTL_NI_MATCHING,
                    PTL_PID_ANY,
                    NULL,
                    NULL,
                    &mca_coll_portals4_component.ni_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlNIInit failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    /* FIX ME: Need to make sure our ID matches with the MTL... */

    ret = PtlGetUid(mca_coll_portals4_component.ni_h, &mca_coll_portals4_component.uid);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlGetUid failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    ret = PtlEQAlloc(mca_coll_portals4_component.ni_h,
                     4096,
                     &mca_coll_portals4_component.eq_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlEQAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    ret = PtlPTAlloc(mca_coll_portals4_component.ni_h,
                     0,
                     mca_coll_portals4_component.eq_h,
                     15,
                     &mca_coll_portals4_component.pt_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    ret = PtlPTAlloc(mca_coll_portals4_component.ni_h,
                     0,
                     mca_coll_portals4_component.eq_h,
                     16,
                     &mca_coll_portals4_component.finish_pt_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    /* Bind MD/MDs across all memory.  We prefer (for obvious reasons)
       to have a single MD across all of memory */
#if OPAL_PORTALS4_MAX_MD_SIZE < OPAL_PORTALS4_MAX_VA_SIZE
    {
        int i;
        int num_mds = ompi_coll_portals4_get_num_mds();
        ptl_size_t size = (1ULL << OPAL_PORTALS4_MAX_MD_SIZE) - 1;
        ptl_size_t offset_unit = (1ULL << OPAL_PORTALS4_MAX_MD_SIZE) / 2;

        mca_coll_portals4_component.md_hs = malloc(sizeof(ptl_handle_md_t) * num_mds);
        if (NULL == mca_coll_portals4_component.md_hs) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                "%s:%d: Error allocating MD array",
                                __FILE__, __LINE__);
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }

        for (i = 0 ; i < num_mds ; ++i) {
            mca_coll_portals4_component.md_hs[i] = PTL_INVALID_HANDLE;
        }

        for (i = 0 ; i < num_mds ; ++i) {
            md.start = (char*) (offset_unit * i);
            md.length = (i - 1 == num_mds) ? size / 2 : size;
            md.options = 0;
            md.eq_handle = PTL_EQ_NONE;
            md.ct_handle = PTL_CT_NONE;

            ret = PtlMDBind(mca_coll_portals4_component.ni_h,
                            &md,
                            &mca_coll_portals4_component.md_hs[i]); 
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                                    "%s:%d: PtlMDBind failed: %d\n",
                                    __FILE__, __LINE__, ret);
                return OMPI_ERROR;
            }
        }
    }
#else
    md.start = 0;
    md.length = PTL_SIZE_MAX;
    md.options = 0;
    md.eq_handle = PTL_EQ_NONE;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(mca_coll_portals4_component.ni_h,
                    &md,
                    &mca_coll_portals4_component.md_h); 
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlMDBind failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }
#endif

    /* setup finish ack ME */
    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | 
        PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = 0;
    me.ignore_bits = 0;

    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                      mca_coll_portals4_component.finish_pt_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &mca_coll_portals4_component.finish_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlMEAppend of barrier unexpected failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    /* Setup Barrier unexpected arena, which is not per-communicator specific. */
    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_EVENT_SUCCESS_DISABLE |
        PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    COLL_PORTALS4_SET_BITS(me.match_bits, 0, 0, COLL_PORTALS4_BARRIER, 0);
    me.ignore_bits = COLL_PORTALS4_CID_MASK | COLL_PORTALS4_OP_COUNT_MASK;

    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
                      mca_coll_portals4_component.pt_idx,
                      &me,
                      PTL_OVERFLOW_LIST,
                      NULL,
                      &mca_coll_portals4_component.barrier_unex_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: PtlMEAppend of barrier unexpected failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    /* activate progress callback */
    ret = opal_progress_register(portals4_progress);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                            "%s:%d: opal_progress_register failed: %d\n",
                            __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
portals4_comm_query(struct ompi_communicator_t *comm, 
                    int *priority)
{
    mca_coll_portals4_module_t *portals4_module;

    /* For now, we don't support intercommunicators and we probably
       never should handle the single proc case, since there's the
       self module... */
    if (OMPI_COMM_IS_INTER(comm) || ompi_comm_size(comm) < 2) {
        return NULL;
    }

    portals4_module = OBJ_NEW(mca_coll_portals4_module_t);
    if (NULL == portals4_module) return NULL;

    *priority = mca_coll_portals4_priority;
    portals4_module->super.coll_module_enable = portals4_module_enable;
    portals4_module->super.ft_event = NULL;
    portals4_module->super.coll_barrier = ompi_coll_portals4_barrier_intra;
    portals4_module->super.coll_ibarrier = ompi_coll_portals4_ibarrier_intra;

    portals4_module->barrier_count = 0;

    return &(portals4_module->super);
}


/*
 * Init module on the communicator
 */
static int
portals4_module_enable(mca_coll_base_module_t *module,
                       struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}


static int
portals4_progress(void)
{
    int count = 0, ret;
    ptl_event_t ev;
    ompi_coll_portals4_request_t *ptl_request;

    while (true) {
	ret = PtlEQGet(mca_coll_portals4_component.eq_h, &ev);
        if (PTL_OK == ret) {
            OPAL_OUTPUT_VERBOSE((60, ompi_coll_base_framework.framework_output,
                                 "Found event of type %d\n", ev.type));
            count++;
            if (PTL_OK == ev.ni_fail_type) {
                assert(0 != ev.hdr_data);
                ptl_request = (ompi_coll_portals4_request_t*) ev.hdr_data;
                assert(NULL != ptl_request);
                switch (ptl_request->type) {
                case OMPI_COLL_PORTALS4_TYPE_BARRIER:
                    ompi_coll_portals4_ibarrier_intra_fini(ptl_request);
                    break;
                }
            } else {
                opal_output(ompi_coll_base_framework.framework_output,
                            "Error reported in event: %d\n", ev.ni_fail_type);
                abort();
            }
        } else if (PTL_EQ_EMPTY == ret) {
            break;
        } else if (PTL_EQ_DROPPED == ret) {
            opal_output(ompi_coll_base_framework.framework_output,
                        "Flow control situation without recovery (EQ_DROPPED)\n");
            abort();
        } else {
            opal_output(ompi_coll_base_framework.framework_output,
                        "Error returned from PtlEQGet: %d", ret);
            break;
        }
    }

    return count;
}


OBJ_CLASS_INSTANCE(mca_coll_portals4_module_t,
                   mca_coll_base_module_t,
                   NULL, NULL);
