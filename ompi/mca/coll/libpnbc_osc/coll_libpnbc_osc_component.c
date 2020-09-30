/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Ian Bradley Morgan and Anthony Skjellum. All
 *                         rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "coll_libpnbc_osc.h"
#include "pnbc_osc_internal.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/communicator/communicator.h"

/*
 * Public string showing the coll ompi_libpnbc_osc component version number
 */
const char *mca_coll_libpnbc_osc_component_version_string =
    "Open MPI libpnbc_osc collective MCA component version " OMPI_VERSION;


static int libpnbc_osc_priority = 10;
static bool libpnbc_osc_in_progress = false;     /* protect from recursive calls */

bool libpnbc_osc_ibcast_skip_dt_decision = true;

int libpnbc_osc_iallgather_algorithm = 0;             /* iallgather user forced algorithm */
static mca_base_var_enum_value_t iallgather_algorithms[] = {
    {0, "ignore"},
    {1, "linear"},
    {2, "recursive_doubling"},
    {0, NULL}
};

int libpnbc_osc_iallreduce_algorithm = 0;             /* iallreduce user forced algorithm */
static mca_base_var_enum_value_t iallreduce_algorithms[] = {
    {0, "ignore"},
    {1, "ring"},
    {2, "binomial"},
    {3, "rabenseifner"},
    {0, NULL}
};

int libpnbc_osc_ibcast_algorithm = 0;             /* ibcast user forced algorithm */
int libpnbc_osc_ibcast_knomial_radix = 4;
static mca_base_var_enum_value_t ibcast_algorithms[] = {
    {0, "ignore"},
    {1, "linear"},
    {2, "binomial"},
    {3, "chain"},
    {4, "knomial"},
    {0, NULL}
};

int libpnbc_osc_iexscan_algorithm = 0;             /* iexscan user forced algorithm */
static mca_base_var_enum_value_t iexscan_algorithms[] = {
    {0, "ignore"},
    {1, "linear"},
    {2, "recursive_doubling"},
    {0, NULL}
};

int libpnbc_osc_ireduce_algorithm = 0;            /* ireduce user forced algorithm */
static mca_base_var_enum_value_t ireduce_algorithms[] = {
    {0, "ignore"},
    {1, "chain"},
    {2, "binomial"},
    {3, "rabenseifner"},
    {0, NULL}
};

int libpnbc_osc_iscan_algorithm = 0;             /* iscan user forced algorithm */
static mca_base_var_enum_value_t iscan_algorithms[] = {
    {0, "ignore"},
    {1, "linear"},
    {2, "recursive_doubling"},
    {0, NULL}
};

static int libpnbc_osc_open(void);
static int libpnbc_osc_close(void);
static int libpnbc_osc_register(void);
static int libpnbc_osc_init_query(bool, bool);
static mca_coll_base_module_t *libpnbc_osc_comm_query(struct ompi_communicator_t *, int *);
static int libpnbc_osc_module_enable(mca_coll_base_module_t *, struct ompi_communicator_t *);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

ompi_coll_libpnbc_osc_component_t mca_coll_libpnbc_osc_component = {
    {
        /* First, the mca_component_t struct containing meta information
         * about the component itself */
        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "libpnbc_osc",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = libpnbc_osc_open,
            .mca_close_component = libpnbc_osc_close,
            .mca_register_component_params = libpnbc_osc_register,
        },
        .collm_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Initialization / querying functions */
        .collm_init_query = libpnbc_osc_init_query,
        .collm_comm_query = libpnbc_osc_comm_query,
    }
};


static int
libpnbc_osc_open(void)
{
    int ret;

    OBJ_CONSTRUCT(&mca_coll_libpnbc_osc_component.requests, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_coll_libpnbc_osc_component.active_requests, opal_list_t);
    OBJ_CONSTRUCT(&mca_coll_libpnbc_osc_component.lock, opal_mutex_t);
    ret = opal_free_list_init (&mca_coll_libpnbc_osc_component.requests,
                               sizeof(ompi_coll_libpnbc_osc_request_t), 8,
                               OBJ_CLASS(ompi_coll_libpnbc_osc_request_t),
                               0, 0, 0, -1, 8, NULL, 0, NULL, NULL, NULL);
    if (OMPI_SUCCESS != ret) return ret;

    /* note: active comms is the number of communicators who have had
       a non-blocking collective started */
    mca_coll_libpnbc_osc_component.active_comms = 0;

    return OMPI_SUCCESS;
}

static int
libpnbc_osc_close(void)
{
    if (0 != mca_coll_libpnbc_osc_component.active_comms) {
        opal_progress_unregister(ompi_coll_libpnbc_osc_progress);
    }

    OBJ_DESTRUCT(&mca_coll_libpnbc_osc_component.requests);
    OBJ_DESTRUCT(&mca_coll_libpnbc_osc_component.active_requests);
    OBJ_DESTRUCT(&mca_coll_libpnbc_osc_component.lock);

    return OMPI_SUCCESS;
}


static int
libpnbc_osc_register(void)
{
    mca_base_var_enum_t *new_enum = NULL;

    /* Use a low priority, but allow other components to be lower */
    libpnbc_osc_priority = 10;
    (void) mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                           "priority", "Priority of the libpnbc_osc coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &libpnbc_osc_priority);

    /* ibcast decision function can make the wrong decision if a legal
     * non-uniform data type signature is used. This has resulted in the
     * collective operation failing, and possibly producing wrong answers.
     * We are investigating a fix for this problem, but it is taking a while.
     *   https://github.com/open-mpi/ompi/issues/2256
     *   https://github.com/open-mpi/ompi/issues/1763
     * As a result we are adding an MCA parameter to make a conservative
     * decision to avoid this issue. If the user knows that their application
     * does not use data types in this way, then they can set this parameter
     * to get the old behavior. Once the issue is truely fixed, then this
     * parameter can be removed.
     */
    libpnbc_osc_ibcast_skip_dt_decision = true;
    (void) mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                           "ibcast_skip_dt_decision",
                                           "In ibcast only use size of communicator to choose algorithm, exclude data type signature. Set to 'false' to use data type signature in decision. WARNING: If you set this to 'false' then your application should not use non-uniform data type signatures in calls to ibcast.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &libpnbc_osc_ibcast_skip_dt_decision);

    libpnbc_osc_iallgather_algorithm = 0;
    (void) mca_base_var_enum_create("coll_libpnbc_osc_iallgather_algorithms", iallgather_algorithms, &new_enum);
    mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                    "iallgather_algorithm",
                                    "Which iallgather algorithm is used: 0 ignore, 1 linear, 2 recursive_doubling",
                                    MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_ALL,
                                    &libpnbc_osc_iallgather_algorithm);
    OBJ_RELEASE(new_enum);

    libpnbc_osc_iallreduce_algorithm = 0;
    (void) mca_base_var_enum_create("coll_libpnbc_osc_iallreduce_algorithms", iallreduce_algorithms, &new_enum);
    mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                    "iallreduce_algorithm",
                                    "Which iallreduce algorithm is used: 0 ignore, 1 ring, 2 binomial, 3 rabenseifner",
                                    MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_ALL,
                                    &libpnbc_osc_iallreduce_algorithm);
    OBJ_RELEASE(new_enum);

    libpnbc_osc_ibcast_algorithm = 0;
    (void) mca_base_var_enum_create("coll_libpnbc_osc_ibcast_algorithms", ibcast_algorithms, &new_enum);
    mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                    "ibcast_algorithm",
                                    "Which ibcast algorithm is used: 0 ignore, 1 linear, 2 binomial, 3 chain, 4 knomial",
                                    MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_ALL,
                                    &libpnbc_osc_ibcast_algorithm);
    OBJ_RELEASE(new_enum);

    libpnbc_osc_ibcast_knomial_radix = 4;
    (void) mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                           "ibcast_knomial_radix", "k-nomial tree radix for the ibcast algorithm (radix > 1)",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &libpnbc_osc_ibcast_knomial_radix);

    libpnbc_osc_iexscan_algorithm = 0;
    (void) mca_base_var_enum_create("coll_libpnbc_osc_iexscan_algorithms", iexscan_algorithms, &new_enum);
    mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                    "iexscan_algorithm",
                                    "Which iexscan algorithm is used: 0 ignore, 1 linear, 2 recursive_doubling",
                                    MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_ALL,
                                    &libpnbc_osc_iexscan_algorithm);
    OBJ_RELEASE(new_enum);

    libpnbc_osc_ireduce_algorithm = 0;
    (void) mca_base_var_enum_create("coll_libpnbc_osc_ireduce_algorithms", ireduce_algorithms, &new_enum);
    mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                    "ireduce_algorithm",
                                    "Which ireduce algorithm is used: 0 ignore, 1 chain, 2 binomial, 3 rabenseifner",
                                    MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_ALL,
                                    &libpnbc_osc_ireduce_algorithm);
    OBJ_RELEASE(new_enum);

    libpnbc_osc_iscan_algorithm = 0;
    (void) mca_base_var_enum_create("coll_libpnbc_osc_iscan_algorithms", iscan_algorithms, &new_enum);
    mca_base_component_var_register(&mca_coll_libpnbc_osc_component.super.collm_version,
                                    "iscan_algorithm",
                                    "Which iscan algorithm is used: 0 ignore, 1 linear, 2 recursive_doubling",
                                    MCA_BASE_VAR_TYPE_INT, new_enum, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                    OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_ALL,
                                    &libpnbc_osc_iscan_algorithm);
    OBJ_RELEASE(new_enum);

    return OMPI_SUCCESS;
}

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
static int
libpnbc_osc_init_query(bool enable_progress_threads,
                  bool enable_mpi_threads)
{
    /* Nothing to do */
    return OMPI_SUCCESS;
}

/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
libpnbc_osc_comm_query(struct ompi_communicator_t *comm,
                  int *priority)
{
    ompi_coll_libpnbc_osc_module_t *module;

    module = OBJ_NEW(ompi_coll_libpnbc_osc_module_t);
    if (NULL == module) return NULL;

    *priority = libpnbc_osc_priority;

    module->super.coll_module_enable = libpnbc_osc_module_enable;
    if (OMPI_COMM_IS_INTER(comm)) {
        module->super.coll_iallgather = NULL; 
        module->super.coll_iallgatherv = NULL;
        module->super.coll_iallreduce = NULL; 
        module->super.coll_ialltoall = NULL;
        module->super.coll_ialltoallv = NULL;
        module->super.coll_ialltoallw = NULL;
        module->super.coll_ibarrier = NULL;
        module->super.coll_ibcast = NULL;
        module->super.coll_iexscan = NULL;
        module->super.coll_igather = NULL;
        module->super.coll_igatherv = NULL;
        module->super.coll_ireduce = NULL;
        module->super.coll_ireduce_scatter = NULL;
        module->super.coll_ireduce_scatter_block = NULL;
        module->super.coll_iscan = NULL;
        module->super.coll_iscatter = NULL;
        module->super.coll_iscatterv = NULL;

        module->super.coll_allgather_init = NULL;
        module->super.coll_allgatherv_init = NULL;
        module->super.coll_allreduce_init = NULL;
        module->super.coll_alltoall_init = NULL;
        module->super.coll_alltoallv_init = NULL;
        module->super.coll_alltoallw_init = NULL;
        module->super.coll_barrier_init = NULL;
        module->super.coll_bcast_init = NULL;
        module->super.coll_exscan_init = NULL;
        module->super.coll_gather_init = NULL;
        module->super.coll_gatherv_init = NULL;
        module->super.coll_reduce_init = NULL;
        module->super.coll_reduce_scatter_init = NULL;
        module->super.coll_reduce_scatter_block_init = NULL;
        module->super.coll_scan_init = NULL;
        module->super.coll_scatter_init = NULL;
        module->super.coll_scatterv_init = NULL;
    } else {
        module->super.coll_iallgather = NULL; 
        module->super.coll_iallgatherv = NULL;
        module->super.coll_iallreduce = NULL; 
        module->super.coll_ialltoall = NULL;
        module->super.coll_ialltoallv = NULL;
        module->super.coll_ialltoallw = NULL;
        module->super.coll_ibarrier = NULL;
        module->super.coll_ibcast = NULL;
        module->super.coll_iexscan = NULL;
        module->super.coll_igather = NULL;
        module->super.coll_igatherv = NULL;
        module->super.coll_ireduce = NULL;
        module->super.coll_ireduce_scatter = NULL;
        module->super.coll_ireduce_scatter_block = NULL;
        module->super.coll_iscan = NULL;
        module->super.coll_iscatter = NULL;
        module->super.coll_iscatterv = NULL;

        
        module->super.coll_ineighbor_allgather = NULL;
        module->super.coll_ineighbor_allgatherv = NULL;
        module->super.coll_ineighbor_alltoall = NULL;
        module->super.coll_ineighbor_alltoallv = NULL;
        module->super.coll_ineighbor_alltoallw = NULL;

        module->super.coll_allgather_init = NULL;
        module->super.coll_allgatherv_init = NULL;
        module->super.coll_allreduce_init = NULL;
        module->super.coll_alltoall_init = NULL;
        module->super.coll_alltoallv_init = ompi_coll_libpnbc_osc_alltoallv_init;
        module->super.coll_alltoallw_init = NULL;
        module->super.coll_barrier_init = NULL;
        module->super.coll_bcast_init = NULL;
        module->super.coll_exscan_init = NULL;
        module->super.coll_gather_init = NULL;
        module->super.coll_gatherv_init = NULL;
        module->super.coll_reduce_init = NULL;
        module->super.coll_reduce_scatter_init = NULL;
        module->super.coll_reduce_scatter_block_init = NULL;
        module->super.coll_scan_init = NULL;
        module->super.coll_scatter_init = NULL;
        module->super.coll_scatterv_init = NULL;

        module->super.coll_neighbor_allgather_init = NULL;
        module->super.coll_neighbor_allgatherv_init = NULL;
        module->super.coll_neighbor_alltoall_init = NULL;
        module->super.coll_neighbor_alltoallv_init = NULL;
        module->super.coll_neighbor_alltoallw_init = NULL;
    }

    module->super.ft_event = NULL;

    return &(module->super);
}


/*
 * Init module on the communicator
 */
static int
libpnbc_osc_module_enable(mca_coll_base_module_t *module,
                     struct ompi_communicator_t *comm)
{
    /* All done */
    return OMPI_SUCCESS;
}


int
ompi_coll_libpnbc_osc_progress(void)
{
    ompi_coll_libpnbc_osc_request_t* request, *next;
    int res;

    if (0 == opal_list_get_size (&mca_coll_libpnbc_osc_component.active_requests)) {
        /* no requests -- nothing to do. do not grab a lock */
        return 0;
    }

    /* process active requests, and use mca_coll_libpnbc_osc_component.lock to access the
     * mca_coll_libpnbc_osc_component.active_requests list */
    OPAL_THREAD_LOCK(&mca_coll_libpnbc_osc_component.lock);
    /* return if invoked recursively */
    if (!libpnbc_osc_in_progress) {
        libpnbc_osc_in_progress = true;

        OPAL_LIST_FOREACH_SAFE(request, next, &mca_coll_libpnbc_osc_component.active_requests,
                               ompi_coll_libpnbc_osc_request_t) {
            OPAL_THREAD_UNLOCK(&mca_coll_libpnbc_osc_component.lock);
            res = PNBC_OSC_Progress(request);
            if( PNBC_OSC_CONTINUE != res ) {
                /* done, remove and complete */
                OPAL_THREAD_LOCK(&mca_coll_libpnbc_osc_component.lock);
                opal_list_remove_item(&mca_coll_libpnbc_osc_component.active_requests,
                                      &request->super.super.super);
                OPAL_THREAD_UNLOCK(&mca_coll_libpnbc_osc_component.lock);

                if( OMPI_SUCCESS == res || PNBC_OSC_OK == res || PNBC_OSC_SUCCESS == res ) {
                    request->super.req_status.MPI_ERROR = OMPI_SUCCESS;
                }
                else {
                    request->super.req_status.MPI_ERROR = res;
                }
                if(request->super.req_persistent) {
                    /* reset for the next communication */
                    request->row_offset = 0;
                }
                if(!request->super.req_persistent || !REQUEST_COMPLETE(&request->super)) {
            	    ompi_request_complete(&request->super, true);
                }
            }
            OPAL_THREAD_LOCK(&mca_coll_libpnbc_osc_component.lock);
        }
        libpnbc_osc_in_progress = false;
    }
    OPAL_THREAD_UNLOCK(&mca_coll_libpnbc_osc_component.lock);

    return 0;
}


static void
libpnbc_osc_module_construct(ompi_coll_libpnbc_osc_module_t *module)
{
    OBJ_CONSTRUCT(&module->mutex, opal_mutex_t);
    module->comm_registered = false;
}


static void
libpnbc_osc_module_destruct(ompi_coll_libpnbc_osc_module_t *module)
{
    OBJ_DESTRUCT(&module->mutex);

    /* if we ever were used for a collective op, do the progress cleanup. */
    if (true == module->comm_registered) {
        int32_t tmp =
            OPAL_THREAD_ADD_FETCH32(&mca_coll_libpnbc_osc_component.active_comms, -1);
        if (0 == tmp) {
            opal_progress_unregister(ompi_coll_libpnbc_osc_progress);
        }
    }
}


OBJ_CLASS_INSTANCE(ompi_coll_libpnbc_osc_module_t,
                   mca_coll_base_module_t,
                   libpnbc_osc_module_construct,
                   libpnbc_osc_module_destruct);


static int
request_start(size_t count, ompi_request_t ** requests)
{
    int res;
    size_t i;

    PNBC_OSC_DEBUG(5, " ** request_start **\n");

    for (i = 0; i < count; i++) {
        PNBC_OSC_Handle *handle = (PNBC_OSC_Handle *) requests[i];
        PNBC_OSC_Schedule *schedule = handle->schedule;

        PNBC_OSC_DEBUG(5, "--------------------------------\n");
        PNBC_OSC_DEBUG(5, "schedule %p size %u\n", &schedule, sizeof(schedule));
        PNBC_OSC_DEBUG(5, "handle %p size %u\n", &handle, sizeof(handle));
        PNBC_OSC_DEBUG(5, "data %p size %u\n", &schedule->data, sizeof(schedule->data));
        PNBC_OSC_DEBUG(5, "req_array %p size %u\n", &handle->req_array, sizeof(handle->req_array));
        PNBC_OSC_DEBUG(5, "row_offset=%u address=%p size=%u\n", handle->row_offset, &handle->row_offset, sizeof(handle->row_offset));
        PNBC_OSC_DEBUG(5, "req_count=%u address=%p size=%u\n", handle->req_count, &handle->req_count, sizeof(handle->req_count));
        PNBC_OSC_DEBUG(5, "tmpbuf address=%p size=%u\n", handle->tmpbuf, sizeof(handle->tmpbuf));
        PNBC_OSC_DEBUG(5, "--------------------------------\n");

        handle->super.req_complete = REQUEST_PENDING;
        handle->nbc_complete = false;

        res = PNBC_OSC_Start(handle);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            PNBC_OSC_DEBUG(5, " ** bad result from PNBC_OSC_Start **\n");
            return res;
        }
    }

    PNBC_OSC_DEBUG(5, " ** LEAVING request_start **\n");

    return OMPI_SUCCESS;

}


static int
request_cancel(struct ompi_request_t *request, int complete)
{
    return MPI_ERR_REQUEST;
}


static int
request_free(struct ompi_request_t **ompi_req)
{
    ompi_coll_libpnbc_osc_request_t *request =
        (ompi_coll_libpnbc_osc_request_t*) *ompi_req;

    if( !REQUEST_COMPLETE(&request->super) ) {
        return MPI_ERR_REQUEST;
    }

    OMPI_COLL_LIBPNBC_OSC_REQUEST_RETURN(request);
    *ompi_req = MPI_REQUEST_NULL;

    return OMPI_SUCCESS;
}


static void
request_construct(ompi_coll_libpnbc_osc_request_t *request)
{
    request->super.req_type = OMPI_REQUEST_COLL;
    request->super.req_status._cancelled = 0;
    request->super.req_start = request_start;
    request->super.req_free = request_free;
    request->super.req_cancel = request_cancel;
}


OBJ_CLASS_INSTANCE(ompi_coll_libpnbc_osc_request_t,
                   ompi_request_t,
                   request_construct,
                   NULL);
