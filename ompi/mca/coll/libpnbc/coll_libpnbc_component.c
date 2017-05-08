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
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "coll_libpnbc.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/communicator/communicator.h"

/*
 * Public string showing the coll ompi_libpnbc component version number
 */
const char *mca_coll_libpnbc_component_version_string =
    "Open MPI libpnbc collective MCA component version " OMPI_VERSION;


static int libpnbc_priority = 10;
bool libpnbc_ibcast_skip_dt_decision = true;


static int libpnbc_open(void);
static int libpnbc_close(void);
static int libpnbc_register(void);
static int libpnbc_init_query(bool, bool);
static mca_coll_base_module_t *libpnbc_comm_query(struct ompi_communicator_t *, int *);
static int libpnbc_module_enable(mca_coll_base_module_t *, struct ompi_communicator_t *);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

ompi_coll_libpnbc_component_t mca_coll_libpnbc_component = {
    {
        /* First, the mca_component_t struct containing meta information
         * about the component itself */
        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "libpnbc",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = libpnbc_open,
            .mca_close_component = libpnbc_close,
            .mca_register_component_params = libpnbc_register,
        },
        .collm_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Initialization / querying functions */
        .collm_init_query = libpnbc_init_query,
        .collm_comm_query = libpnbc_comm_query,
    }
};


static int
libpnbc_open(void)
{
    int ret;

    OBJ_CONSTRUCT(&mca_coll_libpnbc_component.requests, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_coll_libpnbc_component.active_requests, opal_list_t);
    OBJ_CONSTRUCT(&mca_coll_libpnbc_component.lock, opal_mutex_t);
    ret = opal_free_list_init (&mca_coll_libpnbc_component.requests,
                               sizeof(ompi_coll_libpnbc_request_t), 8,
                               OBJ_CLASS(ompi_coll_libpnbc_request_t),
                               0, 0, 0, -1, 8, NULL, 0, NULL, NULL, NULL);
    if (OMPI_SUCCESS != ret) return ret;

    /* note: active comms is the number of communicators who have had
       a non-blocking collective started */
    mca_coll_libpnbc_component.active_comms = 0;

    opal_atomic_init(&mca_coll_libpnbc_component.progress_lock, OPAL_ATOMIC_UNLOCKED);

    return OMPI_SUCCESS;
}

static int
libpnbc_close(void)
{
	printf("*** CLOSING LIBPNBC!! ***\n");

    if (0 != mca_coll_libpnbc_component.active_comms) {
        opal_progress_unregister(ompi_coll_libpnbc_progress);
    }

    OBJ_DESTRUCT(&mca_coll_libpnbc_component.requests);
    OBJ_DESTRUCT(&mca_coll_libpnbc_component.active_requests);
    OBJ_DESTRUCT(&mca_coll_libpnbc_component.lock);

    return OMPI_SUCCESS;
}


static int
libpnbc_register(void)
{
    /* Use a low priority, but allow other components to be lower */
    libpnbc_priority = 10;
    (void) mca_base_component_var_register(&mca_coll_libpnbc_component.super.collm_version,
                                           "priority", "Priority of the libpnbc coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &libpnbc_priority);

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
    libpnbc_ibcast_skip_dt_decision = true;
    (void) mca_base_component_var_register(&mca_coll_libpnbc_component.super.collm_version,
                                           "ibcast_skip_dt_decision",
                                           "In ibcast only use size of communicator to choose algorithm, exclude data type signature. Set to 'false' to use data type signature in decision. WARNING: If you set this to 'false' then your application should not use non-uniform data type signatures in calls to ibcast.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &libpnbc_ibcast_skip_dt_decision);

    return OMPI_SUCCESS;
}



/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
static int
libpnbc_init_query(bool enable_progress_threads,
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
libpnbc_comm_query(struct ompi_communicator_t *comm,
                  int *priority)
{
    ompi_coll_libpnbc_module_t *module;

    module = OBJ_NEW(ompi_coll_libpnbc_module_t);
    if (NULL == module) return NULL;

    *priority = libpnbc_priority;

    module->super.coll_module_enable = libpnbc_module_enable;
    if (OMPI_COMM_IS_INTER(comm)) {
        module->super.coll_iallgather_init = ompi_coll_libpnbc_iallgather_inter;
        module->super.coll_iallgatherv_init = ompi_coll_libpnbc_iallgatherv_inter;
        module->super.coll_iallreduce_init = ompi_coll_libpnbc_iallreduce_inter;
        module->super.coll_ialltoall_init = ompi_coll_libpnbc_ialltoall_inter;
        module->super.coll_ialltoallv_init = ompi_coll_libpnbc_ialltoallv_inter;
        module->super.coll_ialltoallw_init = ompi_coll_libpnbc_ialltoallw_inter;
        module->super.coll_ibarrier_init = ompi_coll_libpnbc_ibarrier_inter;
        module->super.coll_ibcast_init = ompi_coll_libpnbc_ibcast_inter;
        module->super.coll_iexscan_init = NULL;
        module->super.coll_igather_init = ompi_coll_libpnbc_igather_inter;
        module->super.coll_igatherv_init = ompi_coll_libpnbc_igatherv_inter;
        module->super.coll_ireduce_init = ompi_coll_libpnbc_ireduce_inter;
        module->super.coll_ireduce_scatter_init = ompi_coll_libpnbc_ireduce_scatter_inter;
        module->super.coll_ireduce_scatter_block_init = ompi_coll_libpnbc_ireduce_scatter_block_inter;
        module->super.coll_iscan_init = NULL;
        module->super.coll_iscatter_init = ompi_coll_libpnbc_iscatter_inter;
        module->super.coll_iscatterv_init = ompi_coll_libpnbc_iscatterv_inter;
    } else {
        module->super.coll_iallgather_init = ompi_coll_libpnbc_iallgather_init;
        module->super.coll_iallgatherv_init = ompi_coll_libpnbc_iallgatherv_init;
        module->super.coll_iallreduce_init = ompi_coll_libpnbc_iallreduce_init;
        module->super.coll_ialltoall_init = ompi_coll_libpnbc_ialltoall_init;
        module->super.coll_ialltoallv_init = ompi_coll_libpnbc_ialltoallv_init;
        module->super.coll_ialltoallw_init = ompi_coll_libpnbc_ialltoallw_init;
        module->super.coll_ibarrier_init = ompi_coll_libpnbc_ibarrier_init;
        module->super.coll_ibcast_init = ompi_coll_libpnbc_ibcast_init;
        module->super.coll_iexscan_init = ompi_coll_libpnbc_iexscan_init;
        module->super.coll_igather_init = ompi_coll_libpnbc_igather_init;
        module->super.coll_igatherv_init = ompi_coll_libpnbc_igatherv_init;
        module->super.coll_ireduce_init = ompi_coll_libpnbc_ireduce_init;
        module->super.coll_ireduce_scatter_init = ompi_coll_libpnbc_ireduce_scatter_init;
        module->super.coll_ireduce_scatter_block_init = ompi_coll_libpnbc_ireduce_scatter_block_init;
        module->super.coll_iscan_init = ompi_coll_libpnbc_iscan_init;
        module->super.coll_iscatter_init = ompi_coll_libpnbc_iscatter_init;
        module->super.coll_iscatterv_init = ompi_coll_libpnbc_iscatterv_init;

        module->super.coll_ineighbor_allgather_init = ompi_coll_libpnbc_ineighbor_allgather_init;
        module->super.coll_ineighbor_allgatherv_init = ompi_coll_libpnbc_ineighbor_allgatherv_init;
        module->super.coll_ineighbor_alltoall_init = ompi_coll_libpnbc_ineighbor_alltoall_init;
        module->super.coll_ineighbor_alltoallv_init = ompi_coll_libpnbc_ineighbor_alltoallv_init;
        module->super.coll_ineighbor_alltoallw_init = ompi_coll_libpnbc_ineighbor_alltoallw_init;

        module->super.coll_libpnbc_start = ompi_coll_libpnbc_start;

    }

    module->super.ft_event = NULL;

    if (OMPI_SUCCESS != PNBC_Init_comm(comm, module)) {
        OBJ_RELEASE(module);
        return NULL;
    }

    return &(module->super);
}


/*
 * Init module on the communicator
 */
static int
libpnbc_module_enable(mca_coll_base_module_t *module,
                     struct ompi_communicator_t *comm)
{
    /* All done */
    return OMPI_SUCCESS;
}


int
ompi_coll_libpnbc_progress(void)
{
	printf("[LIBPNBC - X] **** framework has called PNBC_progress. **** \n");

    ompi_coll_libpnbc_request_t* request, *next;
    int res;

    /* return if invoked recursively */
    if (opal_atomic_trylock(&mca_coll_libpnbc_component.progress_lock)) return 0;

    /* process active requests, and use mca_coll_libpnbc_component.lock to access the
     * mca_coll_libpnbc_component.active_requests list */
    OPAL_THREAD_LOCK(&mca_coll_libpnbc_component.lock);
    OPAL_LIST_FOREACH_SAFE(request, next, &mca_coll_libpnbc_component.active_requests,
                           ompi_coll_libpnbc_request_t) {
        OPAL_THREAD_UNLOCK(&mca_coll_libpnbc_component.lock);
        res = PNBC_Progress(request);
        printf("[LIBPNBC - X] **** framework returned from PNBC_Progress **** \n");
        if( PNBC_CONTINUE != res ) {
            /* done, remove and complete */
            OPAL_THREAD_LOCK(&mca_coll_libpnbc_component.lock);
            printf("[LIBPNBC - X] **** framework removing an active request **** \n");
            opal_list_remove_item(&mca_coll_libpnbc_component.active_requests,
                                  &request->super.super.super);
            OPAL_THREAD_UNLOCK(&mca_coll_libpnbc_component.lock);

            if( OMPI_SUCCESS == res || PNBC_OK == res || PNBC_SUCCESS == res ) {
                request->super.req_status.MPI_ERROR = OMPI_SUCCESS;
            }
            else {
                request->super.req_status.MPI_ERROR = res;
            }
            printf("[LIBPNBC - X] **** framework completing a request **** \n");

            if(!REQUEST_COMPLETE(&request->super)) {
            	ompi_request_complete(&request->super, true);
            }

        }
        OPAL_THREAD_LOCK(&mca_coll_libpnbc_component.lock);
    }
    OPAL_THREAD_UNLOCK(&mca_coll_libpnbc_component.lock);

    opal_atomic_unlock(&mca_coll_libpnbc_component.progress_lock);

    printf("[LIBPNBC - X] **** leaving ompi_coll_libpnbc_progress **** \n");

    return 0;
}


static void
libpnbc_module_construct(ompi_coll_libpnbc_module_t *module)
{
    OBJ_CONSTRUCT(&module->mutex, opal_mutex_t);
    module->comm_registered = false;
}


static void
libpnbc_module_destruct(ompi_coll_libpnbc_module_t *module)
{
    OBJ_DESTRUCT(&module->mutex);

    /* if we ever were used for a collective op, do the progress cleanup. */
    if (true == module->comm_registered) {
        int32_t tmp =
            OPAL_THREAD_ADD32(&mca_coll_libpnbc_component.active_comms, -1);
        if (0 == tmp) {
            opal_progress_unregister(ompi_coll_libpnbc_progress);
        }
    }
}


OBJ_CLASS_INSTANCE(ompi_coll_libpnbc_module_t,
                   mca_coll_base_module_t,
                   libpnbc_module_construct,
                   libpnbc_module_destruct);


static int
request_cancel(struct ompi_request_t *request, int complete)
{
    return MPI_ERR_REQUEST;
}


static int
request_free(struct ompi_request_t **ompi_req)
{

	printf("*** REQUEST FREE ***\n");
    //ompi_coll_libpnbc_request_t *request = (ompi_coll_libpnbc_request_t*) *ompi_req;

    //if( !REQUEST_COMPLETE(&request->super) ) {
    //    return MPI_ERR_REQUEST;
    //}

    //OMPI_COLL_LIBPNBC_REQUEST_RETURN(request);

    //*ompi_req = MPI_REQUEST_NULL;

    return OMPI_SUCCESS;
}


static void
request_construct(ompi_coll_libpnbc_request_t *request)
{
	printf("*** REQUEST CONSTRUCT ***\n");

    request->super.req_type = OMPI_REQUEST_COLL;
    request->super.req_status._cancelled = 0;
    request->super.req_free = request_free;
    request->super.req_cancel = request_cancel;
}


OBJ_CLASS_INSTANCE(ompi_coll_libpnbc_request_t,
                   ompi_request_t,
                   request_construct,
                   NULL);
