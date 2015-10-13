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
 * Copyright (c) 2013-2015 Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Bull SAS.  All rights reserved.
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
#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"

#define REQ_COLL_TABLE_ID           15
#define REQ_COLL_FINISH_TABLE_ID    16


ptl_op_t ompi_coll_portals4_atomic_op [OMPI_OP_NUM_OF_TYPES] =
{
        [OMPI_OP_NULL] = COLL_PORTALS4_NO_OP,
        [OMPI_OP_MAX] = PTL_MAX,
        [OMPI_OP_MIN] = PTL_MIN,
        [OMPI_OP_SUM] = PTL_SUM,
        [OMPI_OP_PROD] = PTL_PROD,
        [OMPI_OP_LAND] = PTL_LAND,
        [OMPI_OP_BAND] = PTL_BAND,
        [OMPI_OP_LOR] = PTL_LOR,
        [OMPI_OP_BOR] = PTL_BOR,
        [OMPI_OP_LXOR] = PTL_LXOR,
        [OMPI_OP_BXOR] = PTL_BXOR,
        [OMPI_OP_MAXLOC] = COLL_PORTALS4_NO_OP,
        [OMPI_OP_MINLOC] = COLL_PORTALS4_NO_OP,
        [OMPI_OP_REPLACE] = PTL_CSWAP,
};

ptl_datatype_t ompi_coll_portals4_atomic_datatype [OMPI_DATATYPE_MPI_MAX_PREDEFINED] =
{
        [OMPI_DATATYPE_MPI_EMPTY] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_UINT8_T] = PTL_UINT8_T,
        [OMPI_DATATYPE_MPI_INT16_T] = PTL_INT16_T,
        [OMPI_DATATYPE_MPI_UINT16_T] = PTL_UINT16_T,
        [OMPI_DATATYPE_MPI_INT32_T] = PTL_INT32_T,
        [OMPI_DATATYPE_MPI_UINT32_T] = PTL_UINT32_T,
        [OMPI_DATATYPE_MPI_INT64_T] = PTL_INT64_T,
        [OMPI_DATATYPE_MPI_UINT64_T] = PTL_UINT64_T,
        [OMPI_DATATYPE_MPI_FLOAT] = PTL_FLOAT,
        [OMPI_DATATYPE_MPI_DOUBLE] = PTL_DOUBLE,
        [OMPI_DATATYPE_MPI_LONG_DOUBLE] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_COMPLEX8] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_COMPLEX16] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_COMPLEX32] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_WCHAR] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_PACKED] = COLL_PORTALS4_NO_DTYPE,

        /* C++ / C99 datatypes */
        [OMPI_DATATYPE_MPI_BOOL] = COLL_PORTALS4_NO_DTYPE,

        /* Fortran datatypes */
        [OMPI_DATATYPE_MPI_LOGICAL] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_CHARACTER] = PTL_INT8_T,
        [OMPI_DATATYPE_MPI_INTEGER] = PTL_INT64_T,
        [OMPI_DATATYPE_MPI_REAL] = PTL_FLOAT,
        [OMPI_DATATYPE_MPI_DOUBLE_PRECISION] = PTL_DOUBLE,

        [OMPI_DATATYPE_MPI_COMPLEX] = PTL_FLOAT_COMPLEX,
        [OMPI_DATATYPE_MPI_DOUBLE_COMPLEX] = PTL_DOUBLE_COMPLEX,
        [OMPI_DATATYPE_MPI_LONG_DOUBLE_COMPLEX] = PTL_LONG_DOUBLE_COMPLEX,
        [OMPI_DATATYPE_MPI_2INT] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_2INTEGER] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_2REAL] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_2DBLPREC] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_2COMPLEX] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_2DOUBLE_COMPLEX] = COLL_PORTALS4_NO_DTYPE,

        [OMPI_DATATYPE_MPI_FLOAT_INT] = COLL_PORTALS4_NO_DTYPE,

        [OMPI_DATATYPE_MPI_DOUBLE_INT] = PTL_INT64_T,
        [OMPI_DATATYPE_MPI_LONG_DOUBLE_INT] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_LONG_INT] = PTL_INT32_T,
        [OMPI_DATATYPE_MPI_SHORT_INT] = PTL_INT16_T,

        /* MPI 2.2 types */
        [OMPI_DATATYPE_MPI_AINT] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_OFFSET] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_C_BOOL] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_C_COMPLEX] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_C_FLOAT_COMPLEX] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_C_DOUBLE_COMPLEX] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_C_LONG_DOUBLE_COMPLEX] = COLL_PORTALS4_NO_DTYPE,

        [OMPI_DATATYPE_MPI_LB] = COLL_PORTALS4_NO_DTYPE,
        [OMPI_DATATYPE_MPI_UB] = COLL_PORTALS4_NO_DTYPE,

        /* MPI 3.0 types */
        [OMPI_DATATYPE_MPI_COUNT] = COLL_PORTALS4_NO_DTYPE,

        [OMPI_DATATYPE_MPI_UNAVAILABLE] = COLL_PORTALS4_NO_DTYPE,

};


#define PORTALS4_SAVE_PREV_COLL_API(__module, __comm, __api)                                \
    do {                                                                                    \
        __module->previous_ ## __api            = __comm->c_coll.coll_ ## __api;            \
        __module->previous_ ## __api ## _module = __comm->c_coll.coll_ ## __api ## _module; \
        if (!comm->c_coll.coll_ ## __api || !comm->c_coll.coll_ ## __api ## _module) {      \
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,               \
                    "(%d/%s): no underlying " # __api"; disqualifying myself",              \
                    __comm->c_contextid, __comm->c_name);                                   \
                    return OMPI_ERROR;                                                      \
        }                                                                                   \
        OBJ_RETAIN(__module->previous_ ## __api ## _module);                                \
    } while(0)


const char *mca_coll_portals4_component_version_string =
        "Open MPI Portals 4 collective MCA component version " OMPI_VERSION;

int mca_coll_portals4_priority = 10;

#define MCA_COLL_PORTALS4_EQ_SIZE	4096

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

        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "portals4",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = portals4_open,
            .mca_close_component = portals4_close,
            .mca_register_component_params = portals4_register
        },
        .collm_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        .collm_init_query = portals4_init_query,
        .collm_comm_query = portals4_comm_query,
    },
};

int
opal_stderr(const char *msg, const char *file,
        const int line, const int ret)
{
    opal_output_verbose(1, ompi_coll_base_framework.framework_output,
            "%s:%d: %s: %d\n", file, line, msg, ret);
    return (OMPI_ERR_TEMP_OUT_OF_RESOURCE);
}

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

    mca_coll_portals4_component.use_binomial_gather_algorithm = 0;
    (void) mca_base_component_var_register(&mca_coll_portals4_component.super.collm_version, "use_binomial_gather_algorithm",
            "if 1 use a binomial tree algorithm for gather, otherwise use linear",
            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
            OPAL_INFO_LVL_9,
            MCA_BASE_VAR_SCOPE_READONLY,
            &mca_coll_portals4_component.use_binomial_gather_algorithm);

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
    mca_coll_portals4_component.unex_me_h = PTL_INVALID_HANDLE;
    mca_coll_portals4_component.finish_me_h = PTL_INVALID_HANDLE;
    mca_coll_portals4_component.zero_md_h = PTL_INVALID_HANDLE;
    mca_coll_portals4_component.data_md_h = PTL_INVALID_HANDLE;

    OBJ_CONSTRUCT(&mca_coll_portals4_component.requests, opal_free_list_t);
    ret = opal_free_list_init(&mca_coll_portals4_component.requests,
            sizeof(ompi_coll_portals4_request_t),
            opal_cache_line_size,
            OBJ_CLASS(ompi_coll_portals4_request_t),
            0, 0, 8, 0, 8, NULL, 0, NULL, NULL, NULL);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: ompi_free_list_init failed: %d\n",
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

    if (!PtlHandleIsEqual(mca_coll_portals4_component.zero_md_h, PTL_INVALID_HANDLE)) {
        ret = PtlMDRelease(mca_coll_portals4_component.zero_md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                    "%s:%d: PtlMDRelease failed: %d\n",
                    __FILE__, __LINE__, ret);
        }
    }
    mca_coll_portals4_component.zero_md_h = PTL_INVALID_HANDLE;

    if (!PtlHandleIsEqual(mca_coll_portals4_component.data_md_h, PTL_INVALID_HANDLE)) {
        ret = PtlMDRelease(mca_coll_portals4_component.data_md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                    "%s:%d: PtlMDRelease failed: %d\n",
                    __FILE__, __LINE__, ret);
        }
    }
    mca_coll_portals4_component.data_md_h = PTL_INVALID_HANDLE;

    if (!PtlHandleIsEqual(mca_coll_portals4_component.finish_me_h, PTL_INVALID_HANDLE)) {
        ret = PtlMEUnlink(mca_coll_portals4_component.finish_me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                    "%s:%d: PtlMEUnlink failed: %d\n",
                    __FILE__, __LINE__, ret);
        }
    }
    if (!PtlHandleIsEqual(mca_coll_portals4_component.unex_me_h, PTL_INVALID_HANDLE)) {
        ret = PtlMEUnlink(mca_coll_portals4_component.unex_me_h);
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
/*
    /!\ Called for each processes /!\
 */
static int
portals4_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    int ret;
    ptl_md_t md;
    ptl_me_t me;

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
            &mca_coll_portals4_component.ni_limits,
            &mca_coll_portals4_component.ni_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlNIInit failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }


    ret = PtlGetId(mca_coll_portals4_component.ni_h, &mca_coll_portals4_component.id);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlGetid failed: %d\n",
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
            MCA_COLL_PORTALS4_EQ_SIZE,
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
            REQ_COLL_TABLE_ID,
            &mca_coll_portals4_component.pt_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlPTAlloc failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    if (mca_coll_portals4_component.pt_idx != REQ_COLL_TABLE_ID) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlPTAlloc return wrong pt_idx: %d\n",
                __FILE__, __LINE__,
                mca_coll_portals4_component.finish_pt_idx);
        return OMPI_ERROR;
    }

    ret = PtlPTAlloc(mca_coll_portals4_component.ni_h,
            0,
            mca_coll_portals4_component.eq_h,
            REQ_COLL_FINISH_TABLE_ID,
            &mca_coll_portals4_component.finish_pt_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlPTAlloc failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    if (mca_coll_portals4_component.finish_pt_idx != REQ_COLL_FINISH_TABLE_ID) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlPTAlloc return wrong pt_idx: %d\n",
                __FILE__, __LINE__,
                mca_coll_portals4_component.finish_pt_idx);
        return OMPI_ERROR;
    }

    /* Bind MD/MDs across all memory.  We prefer (for obvious reasons)
       to have a single MD across all of memory */
    memset(&md, 0, sizeof(ptl_md_t));
    md.start = 0;
    md.length = 0;
    md.options = 0;
    md.eq_handle = PTL_EQ_NONE;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(mca_coll_portals4_component.ni_h,
            &md,
            &mca_coll_portals4_component.zero_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlMDBind failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }

    md.start = 0;
    md.length = PTL_SIZE_MAX;
    md.options = 0;
    md.eq_handle = PTL_EQ_NONE;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(mca_coll_portals4_component.ni_h,
            &md,
            &mca_coll_portals4_component.data_md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: PtlMDBind failed: %d\n",
                __FILE__, __LINE__, ret);
        return OMPI_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((90, ompi_coll_base_framework.framework_output, "PtlMDBind start=%p length=%lx\n", md.start, md.length));

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

    /* This ME is used for RTR exchange only */
    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = mca_coll_portals4_component.uid;
    me.options = PTL_ME_OP_PUT |
            PTL_ME_EVENT_SUCCESS_DISABLE | PTL_ME_EVENT_OVER_DISABLE |
            PTL_ME_EVENT_LINK_DISABLE | PTL_ME_EVENT_UNLINK_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;

    /* Note : the RTR bit must be set to match this ME,
     * this allows to discriminate the RTR from data flow
     * (especially for the Barrier operations)
     */
    COLL_PORTALS4_SET_BITS(me.match_bits, 0, 0, 1, 0, 0, 0);
    me.ignore_bits = ~COLL_PORTALS4_RTR_MASK;

    ret = PtlMEAppend(mca_coll_portals4_component.ni_h,
            mca_coll_portals4_component.pt_idx,
            &me,
            PTL_OVERFLOW_LIST,
            NULL,
            &mca_coll_portals4_component.unex_me_h);
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
    ptl_process_t              *proc;

    /* For now, we don't support intercommunicators and we probably
       never should handle the single proc case, since there's the
       self module... */
    if (OMPI_COMM_IS_INTER(comm) || ompi_comm_size(comm) < 2) {
        return NULL;
    }

    /* Make sure someone is populating the proc table, since we're not
       in a really good position to do so */
    proc = ompi_proc_local()->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4];
    if (NULL == proc) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: Proc table not previously populated",
                __FILE__, __LINE__);
        return NULL;
    }

    opal_output_verbose(50, ompi_coll_base_framework.framework_output,
                        "%s:%d: My nid,pid = (%x,%x)\n",
                        __FILE__, __LINE__, proc->phys.nid, proc->phys.pid);

    /* check for logical addressing mode in the MTL */
    if (0 == proc->phys.pid) {
        opal_output_verbose(1, ompi_coll_base_framework.framework_output,
                "%s:%d: proc->phys.pid==0, so mtl-portals4 is using logical addressing which coll-portals4 doesn't support.  Disqualifying myself.",
                __FILE__, __LINE__);
        return NULL;
    }

    portals4_module = OBJ_NEW(mca_coll_portals4_module_t);
    if (NULL == portals4_module) return NULL;

    *priority = mca_coll_portals4_priority;
    portals4_module->coll_count = 0;
    portals4_module->super.coll_module_enable = portals4_module_enable;
    portals4_module->super.ft_event = NULL;

    portals4_module->super.coll_barrier = ompi_coll_portals4_barrier_intra;
    portals4_module->super.coll_ibarrier = ompi_coll_portals4_ibarrier_intra;

    portals4_module->super.coll_gather   = ompi_coll_portals4_gather_intra;
    portals4_module->super.coll_igather  = ompi_coll_portals4_igather_intra;

    portals4_module->super.coll_scatter  = ompi_coll_portals4_scatter_intra;
    portals4_module->super.coll_iscatter = ompi_coll_portals4_iscatter_intra;

    portals4_module->cached_in_order_bmtree=NULL;
    portals4_module->cached_in_order_bmtree_root=-1;

    portals4_module->super.coll_bcast = ompi_coll_portals4_bcast_intra;
    portals4_module->super.coll_ibcast = ompi_coll_portals4_ibcast_intra;

    portals4_module->super.coll_allreduce = ompi_coll_portals4_allreduce_intra;
    portals4_module->super.coll_iallreduce = ompi_coll_portals4_iallreduce_intra;

    portals4_module->super.coll_reduce = ompi_coll_portals4_reduce_intra;
    portals4_module->super.coll_ireduce = ompi_coll_portals4_ireduce_intra;

    return &(portals4_module->super);
}


/*
 * Init module on the communicator
 */
static int
portals4_module_enable(mca_coll_base_module_t *module,
        struct ompi_communicator_t *comm)
{
    mca_coll_portals4_module_t *portals4_module = (mca_coll_portals4_module_t*) module;

    PORTALS4_SAVE_PREV_COLL_API(portals4_module, comm, allreduce);
    PORTALS4_SAVE_PREV_COLL_API(portals4_module, comm, iallreduce);
    PORTALS4_SAVE_PREV_COLL_API(portals4_module, comm, reduce);
    PORTALS4_SAVE_PREV_COLL_API(portals4_module, comm, ireduce);

    return OMPI_SUCCESS;
}


static char *failtype[] = {
        "PTL_NI_OK",
        "PTL_NI_PERM_VIOLATION",
        "PTL_NI_SEGV",
        "PTL_NI_PT_DISABLED",
        "PTL_NI_DROPPED",
        "PTL_NI_UNDELIVERABLE",
        "PTL_FAIL",
        "PTL_ARG_INVALID",
        "PTL_IN_USE",
        "PTL_ME_NO_MATCH",
        "PTL_NI_TARGET_INVALID",
        "PTL_NI_OP_VIOLATION"
};

static char *evname[] = {
        "PTL_EVENT_GET",
        "PTL_EVENT_GET_OVERFLOW",
        "PTL_EVENT_PUT",
        "PTL_EVENT_PUT_OVERFLOW",
        "PTL_EVENT_ATOMIC",
        "PTL_EVENT_ATOMIC_OVERFLOW",
        "PTL_EVENT_FETCH_ATOMIC",
        "PTL_EVENT_FETCH_ATOMIC_OVERFLOW",
        "PTL_EVENT_REPLY",
        "PTL_EVENT_SEND",
        "PTL_EVENT_ACK",
        "PTL_EVENT_PT_DISABLED",
        "PTL_EVENT_AUTO_UNLINK",
        "PTL_EVENT_AUTO_FREE",
        "PTL_EVENT_SEARCH",
        "PTL_EVENT_LINK"
};

/* Target EQ */
static int
portals4_progress(void)
{
    int count = 0, ret;
    ptl_event_t ev;
    ompi_coll_portals4_request_t *ptl_request;

    while (true) {
        ret = PtlEQGet(mca_coll_portals4_component.eq_h, &ev);
        if (PTL_OK == ret) {

            OPAL_OUTPUT_VERBOSE((10, ompi_coll_base_framework.framework_output, "event type=%s\n", evname[ev.type]));
            count++;

            switch (ev.type) {
            case PTL_EVENT_PUT:
                /* Non-Blocking / request */
                if (PTL_OK == ev.ni_fail_type) {
                    OPAL_OUTPUT_VERBOSE((50, ompi_coll_base_framework.framework_output,
                            "hdr_data %p, matchbits 0x%lx",
                            (void*) ev.hdr_data, ev.match_bits));
                    assert(0 != ev.hdr_data);
                    ptl_request = (ompi_coll_portals4_request_t*) ev.hdr_data;
                    assert(NULL != ptl_request);

                    switch (ptl_request->type) {
                    case OMPI_COLL_PORTALS4_TYPE_BARRIER:
                        ompi_coll_portals4_ibarrier_intra_fini(ptl_request);
                        break;
                    case OMPI_COLL_PORTALS4_TYPE_BCAST:
                        ompi_coll_portals4_ibcast_intra_fini(ptl_request);
                        break;
                    case OMPI_COLL_PORTALS4_TYPE_REDUCE:
                        ompi_coll_portals4_ireduce_intra_fini(ptl_request);
                        break;
                    case OMPI_COLL_PORTALS4_TYPE_ALLREDUCE:
                        ompi_coll_portals4_iallreduce_intra_fini(ptl_request);
                        break;
                    case OMPI_COLL_PORTALS4_TYPE_SCATTER:
                        ompi_coll_portals4_iscatter_intra_fini(ptl_request);
                        break;
                    case OMPI_COLL_PORTALS4_TYPE_GATHER:
                        ompi_coll_portals4_igather_intra_fini(ptl_request);
                        break;
                    }
                }

                if (PTL_OK != ev.ni_fail_type) {
                    OPAL_OUTPUT_VERBOSE((10, ompi_coll_base_framework.framework_output, "ni_fail_type=%s\n", failtype[ev.ni_fail_type]));
                }
                break;
            default:
                opal_output(ompi_coll_base_framework.framework_output,
                        "Unexpected event of type %d", ev.type);
                break;
            }

        }
        else if (PTL_EQ_EMPTY == ret) {
            break;
        }
        else if (PTL_EQ_DROPPED == ret) {
            opal_output(ompi_coll_base_framework.framework_output, "Flow control situation without recovery (EQ_DROPPED)\n");
            abort();
        }
        else {
            opal_output(ompi_coll_base_framework.framework_output, "Error returned from PtlEQGet: %d", ret);
            break;
        }
    }
    return count;
}

OBJ_CLASS_INSTANCE(mca_coll_portals4_module_t,
        mca_coll_base_module_t,
        NULL, NULL);
