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
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Ian Bradley Morgan and Anthony Skjellum. All
 *                         rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2020      Bull SAS. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "coll_libnbc.h"
#include "nbc_internal.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_base_dynamic_file.h"
#include "opal/util/show_help.h"

/*
 * Public string showing the coll ompi_libnbc component version number
 */
const char *mca_coll_libnbc_component_version_string =
    "Open MPI libnbc collective MCA component version " OMPI_VERSION;


static int libnbc_priority = 10;
static bool libnbc_in_progress = false;     /* protect from recursive calls */

static int libnbc_open(void);
static int libnbc_close(void);
static int libnbc_register(void);
static int libnbc_init_query(bool, bool);
static mca_coll_base_module_t *libnbc_comm_query(struct ompi_communicator_t *, int *);
static int libnbc_module_enable(mca_coll_base_module_t *, struct ompi_communicator_t *);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

ompi_coll_libnbc_component_t mca_coll_libnbc_component = {
    {
        /* First, the mca_component_t struct containing meta information
         * about the component itself */
        .collm_version = {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            .mca_component_name = "libnbc",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),

            /* Component open and close functions */
            .mca_open_component = libnbc_open,
            .mca_close_component = libnbc_close,
            .mca_register_component_params = libnbc_register,
        },
        .collm_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Initialization / querying functions */
        .collm_init_query = libnbc_init_query,
        .collm_comm_query = libnbc_comm_query,
    }
};


static int
libnbc_open(void)
{
    int ret;
    if (mca_coll_libnbc_component.dynamic_rules_verbose > 0) {
        mca_coll_libnbc_component.stream = opal_output_open(NULL);
        opal_output_set_verbosity(mca_coll_libnbc_component.stream, mca_coll_libnbc_component.dynamic_rules_verbose);
    } else {
        mca_coll_libnbc_component.stream = -1;
    }
    if(mca_coll_libnbc_component.dynamic_rules_filename ) {
        int rc;
        opal_output_verbose(10, mca_coll_libnbc_component.stream ,
            "coll:libnbc:component_open Reading collective rules file [%s] which format is %d",
                     mca_coll_libnbc_component.dynamic_rules_filename,
                     mca_coll_libnbc_component.dynamic_rules_fileformat);
        rc = ompi_coll_base_read_rules_config_file( mca_coll_libnbc_component.dynamic_rules_filename,
                                                    mca_coll_libnbc_component.dynamic_rules_fileformat,
                                                    &(mca_coll_libnbc_component.all_base_rules), COLLCOUNT);
        if( rc >= 0 ) {
            opal_output_verbose(10, mca_coll_libnbc_component.stream ,"coll:libnbc:module_open Read %d valid rules\n", rc);
            if(ompi_coll_base_framework.framework_verbose >= 50) {
                ompi_coll_base_dump_all_rules (mca_coll_libnbc_component.all_base_rules, COLLCOUNT);
            }
        } else {
            opal_output_verbose(1, mca_coll_libnbc_component.stream ,"coll:libnbc:module_open Reading collective rules file failed\n");
            char error_name[12];
            sprintf(error_name,"file fail%1d", rc);
            error_name[11] = '\0';
            opal_show_help("help-mpi-coll-libnbc.txt", (const char*)error_name, true,
                           mca_coll_libnbc_component.dynamic_rules_filename, mca_coll_libnbc_component.dynamic_rules_fileformat);
            mca_coll_libnbc_component.all_base_rules = NULL;
        }
    } else {
        mca_coll_libnbc_component.all_base_rules = NULL;
    }

    OBJ_CONSTRUCT(&mca_coll_libnbc_component.requests, opal_free_list_t);
    OBJ_CONSTRUCT(&mca_coll_libnbc_component.active_requests, opal_list_t);
    OBJ_CONSTRUCT(&mca_coll_libnbc_component.lock, opal_mutex_t);
    ret = opal_free_list_init (&mca_coll_libnbc_component.requests,
                               sizeof(ompi_coll_libnbc_request_t), 8,
                               OBJ_CLASS(ompi_coll_libnbc_request_t),
                               0, 0, 0, -1, 8, NULL, 0, NULL, NULL, NULL);
    if (OMPI_SUCCESS != ret) return ret;

    /* note: active comms is the number of communicators who have had
       a non-blocking collective started */
    mca_coll_libnbc_component.active_comms = 0;

    return OMPI_SUCCESS;
}

static int
libnbc_close(void)
{
    if (0 != mca_coll_libnbc_component.active_comms) {
        opal_progress_unregister(ompi_coll_libnbc_progress);
    }

    OBJ_DESTRUCT(&mca_coll_libnbc_component.requests);
    OBJ_DESTRUCT(&mca_coll_libnbc_component.active_requests);
    OBJ_DESTRUCT(&mca_coll_libnbc_component.lock);

    if( NULL != mca_coll_libnbc_component.all_base_rules ) {
        ompi_coll_base_free_all_rules(mca_coll_libnbc_component.all_base_rules, COLLCOUNT);
        mca_coll_libnbc_component.all_base_rules = NULL;
    }
    /* close stream */
    if(mca_coll_libnbc_component.stream >= 0) {
        opal_output_close(mca_coll_libnbc_component.stream);
    }
    return OMPI_SUCCESS;
}


static int
libnbc_register(void)
{
    mca_base_var_enum_t *new_enum = NULL;

    /* Use a low priority, but allow other components to be lower */
    libnbc_priority = 10;
    (void) mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version,
                                           "priority", "Priority of the libnbc coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &libnbc_priority);

    mca_coll_libnbc_component.dynamic_rules_verbose = 0;
    (void) mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version, "dynamic_rules_verbose",
                                           "Verbose level of the libnbc coll component regarding on dynamic rules."
                                           " Examples: 0: no verbose, 1: selection errors, 10: selection output",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_libnbc_component.dynamic_rules_verbose);

    mca_coll_libnbc_component.dynamic_rules_filename = NULL;
    (void) mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version,
                                           "dynamic_rules_filename",
                                           "Filename of configuration file that contains the dynamic (@runtime) decision function rules",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_libnbc_component.dynamic_rules_filename);

    mca_coll_libnbc_component.dynamic_rules_fileformat = 0;
    (void) mca_base_component_var_register(&mca_coll_libnbc_component.super.collm_version,
                                           "dynamic_rules_fileformat",
                                           "Format of configuration file that contains the dynamic (@runtime) decision function rules. Accepted values are: 0 <comm_size, msg_size>, 1 <nodes_nb, comm_size, msg_size>",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_6,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_libnbc_component.dynamic_rules_fileformat);

    ompi_coll_libnbc_allgather_check_forced_init ();
    ompi_coll_libnbc_allreduce_check_forced_init ();
    ompi_coll_libnbc_alltoall_check_forced_init ();
    ompi_coll_libnbc_alltoallv_check_forced_init ();
    ompi_coll_libnbc_alltoallw_check_forced_init ();
    ompi_coll_libnbc_bcast_check_forced_init ();
    ompi_coll_libnbc_exscan_check_forced_init ();
    ompi_coll_libnbc_reduce_check_forced_init ();
    ompi_coll_libnbc_scan_check_forced_init ();

    return OMPI_SUCCESS;
}

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
static int
libnbc_init_query(bool enable_progress_threads,
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
libnbc_comm_query(struct ompi_communicator_t *comm,
                  int *priority)
{
    ompi_coll_libnbc_module_t *module;

    module = OBJ_NEW(ompi_coll_libnbc_module_t);
    if (NULL == module) return NULL;

    *priority = libnbc_priority;

    module->super.coll_module_enable = libnbc_module_enable;
    if (OMPI_COMM_IS_INTER(comm)) {
        module->super.coll_iallgather = ompi_coll_libnbc_iallgather_inter;
        module->super.coll_iallgatherv = ompi_coll_libnbc_iallgatherv_inter;
        module->super.coll_iallreduce = ompi_coll_libnbc_iallreduce_inter;
        module->super.coll_ialltoall = ompi_coll_libnbc_ialltoall_inter;
        module->super.coll_ialltoallv = ompi_coll_libnbc_ialltoallv_inter;
        module->super.coll_ialltoallw = ompi_coll_libnbc_ialltoallw_inter;
        module->super.coll_ibarrier = ompi_coll_libnbc_ibarrier_inter;
        module->super.coll_ibcast = ompi_coll_libnbc_ibcast_inter;
        module->super.coll_iexscan = NULL;
        module->super.coll_igather = ompi_coll_libnbc_igather_inter;
        module->super.coll_igatherv = ompi_coll_libnbc_igatherv_inter;
        module->super.coll_ireduce = ompi_coll_libnbc_ireduce_inter;
        module->super.coll_ireduce_scatter = ompi_coll_libnbc_ireduce_scatter_inter;
        module->super.coll_ireduce_scatter_block = ompi_coll_libnbc_ireduce_scatter_block_inter;
        module->super.coll_iscan = NULL;
        module->super.coll_iscatter = ompi_coll_libnbc_iscatter_inter;
        module->super.coll_iscatterv = ompi_coll_libnbc_iscatterv_inter;

        module->super.coll_allgather_init = ompi_coll_libnbc_allgather_inter_init;
        module->super.coll_allgatherv_init = ompi_coll_libnbc_allgatherv_inter_init;
        module->super.coll_allreduce_init = ompi_coll_libnbc_allreduce_inter_init;
        module->super.coll_alltoall_init = ompi_coll_libnbc_alltoall_inter_init;
        module->super.coll_alltoallv_init = ompi_coll_libnbc_alltoallv_inter_init;
        module->super.coll_alltoallw_init = ompi_coll_libnbc_alltoallw_inter_init;
        module->super.coll_barrier_init = ompi_coll_libnbc_barrier_inter_init;
        module->super.coll_bcast_init = ompi_coll_libnbc_bcast_inter_init;
        module->super.coll_exscan_init = NULL;
        module->super.coll_gather_init = ompi_coll_libnbc_gather_inter_init;
        module->super.coll_gatherv_init = ompi_coll_libnbc_gatherv_inter_init;
        module->super.coll_reduce_init = ompi_coll_libnbc_reduce_inter_init;
        module->super.coll_reduce_scatter_init = ompi_coll_libnbc_reduce_scatter_inter_init;
        module->super.coll_reduce_scatter_block_init = ompi_coll_libnbc_reduce_scatter_block_inter_init;
        module->super.coll_scan_init = NULL;
        module->super.coll_scatter_init = ompi_coll_libnbc_scatter_inter_init;
        module->super.coll_scatterv_init = ompi_coll_libnbc_scatterv_inter_init;
    } else {
        module->super.coll_iallgather = ompi_coll_libnbc_iallgather;
        module->super.coll_iallgatherv = ompi_coll_libnbc_iallgatherv;
        module->super.coll_iallreduce = ompi_coll_libnbc_iallreduce;
        module->super.coll_ialltoall = ompi_coll_libnbc_ialltoall;
        module->super.coll_ialltoallv = ompi_coll_libnbc_ialltoallv;
        module->super.coll_ialltoallw = ompi_coll_libnbc_ialltoallw;
        module->super.coll_ibarrier = ompi_coll_libnbc_ibarrier;
        module->super.coll_ibcast = ompi_coll_libnbc_ibcast;
        module->super.coll_iexscan = ompi_coll_libnbc_iexscan;
        module->super.coll_igather = ompi_coll_libnbc_igather;
        module->super.coll_igatherv = ompi_coll_libnbc_igatherv;
        module->super.coll_ireduce = ompi_coll_libnbc_ireduce;
        module->super.coll_ireduce_scatter = ompi_coll_libnbc_ireduce_scatter;
        module->super.coll_ireduce_scatter_block = ompi_coll_libnbc_ireduce_scatter_block;
        module->super.coll_iscan = ompi_coll_libnbc_iscan;
        module->super.coll_iscatter = ompi_coll_libnbc_iscatter;
        module->super.coll_iscatterv = ompi_coll_libnbc_iscatterv;

        module->super.coll_ineighbor_allgather = ompi_coll_libnbc_ineighbor_allgather;
        module->super.coll_ineighbor_allgatherv = ompi_coll_libnbc_ineighbor_allgatherv;
        module->super.coll_ineighbor_alltoall = ompi_coll_libnbc_ineighbor_alltoall;
        module->super.coll_ineighbor_alltoallv = ompi_coll_libnbc_ineighbor_alltoallv;
        module->super.coll_ineighbor_alltoallw = ompi_coll_libnbc_ineighbor_alltoallw;

        module->super.coll_allgather_init = ompi_coll_libnbc_allgather_init;
        module->super.coll_allgatherv_init = ompi_coll_libnbc_allgatherv_init;
        module->super.coll_allreduce_init = ompi_coll_libnbc_allreduce_init;
        module->super.coll_alltoall_init = ompi_coll_libnbc_alltoall_init;
        module->super.coll_alltoallv_init = ompi_coll_libnbc_alltoallv_init;
        module->super.coll_alltoallw_init = ompi_coll_libnbc_alltoallw_init;
        module->super.coll_barrier_init = ompi_coll_libnbc_barrier_init;
        module->super.coll_bcast_init = ompi_coll_libnbc_bcast_init;
        module->super.coll_exscan_init = ompi_coll_libnbc_exscan_init;
        module->super.coll_gather_init = ompi_coll_libnbc_gather_init;
        module->super.coll_gatherv_init = ompi_coll_libnbc_gatherv_init;
        module->super.coll_reduce_init = ompi_coll_libnbc_reduce_init;
        module->super.coll_reduce_scatter_init = ompi_coll_libnbc_reduce_scatter_init;
        module->super.coll_reduce_scatter_block_init = ompi_coll_libnbc_reduce_scatter_block_init;
        module->super.coll_scan_init = ompi_coll_libnbc_scan_init;
        module->super.coll_scatter_init = ompi_coll_libnbc_scatter_init;
        module->super.coll_scatterv_init = ompi_coll_libnbc_scatterv_init;

        module->super.coll_neighbor_allgather_init = ompi_coll_libnbc_neighbor_allgather_init;
        module->super.coll_neighbor_allgatherv_init = ompi_coll_libnbc_neighbor_allgatherv_init;
        module->super.coll_neighbor_alltoall_init = ompi_coll_libnbc_neighbor_alltoall_init;
        module->super.coll_neighbor_alltoallv_init = ompi_coll_libnbc_neighbor_alltoallv_init;
        module->super.coll_neighbor_alltoallw_init = ompi_coll_libnbc_neighbor_alltoallw_init;
    }

    module->super.ft_event = NULL;

    if (OMPI_SUCCESS != NBC_Init_comm(comm, module)) {
        OBJ_RELEASE(module);
        return NULL;
    }

    return &(module->super);
}


/*
 * Init module on the communicator
 */
static int
libnbc_module_enable(mca_coll_base_module_t *module,
                     struct ompi_communicator_t *comm)
{
    ompi_coll_libnbc_module_t* nbc_module = (ompi_coll_libnbc_module_t*) module;
    int i;
    if(mca_coll_libnbc_component.all_base_rules) {
        int size, nnodes;
        /* Allocate the data that hangs off the communicator */
        if (OMPI_COMM_IS_INTER(comm)) {
          size = ompi_comm_remote_size(comm);
        } else {
          size = ompi_comm_size(comm);
        }
        /* Get the number of nodes in communicator */
        nnodes = ompi_coll_base_get_nnodes(comm);
        for(i=0;i<COLLCOUNT;i++) {
            nbc_module->com_rules[i] = ompi_coll_base_get_com_rule_ptr(mca_coll_libnbc_component.all_base_rules,
                                                                       i, nnodes, size );
        }
    } else {
        for(i=0;i<COLLCOUNT;i++) {
            nbc_module->com_rules[i] = NULL;
        }
    }
    /* All done */
    return OMPI_SUCCESS;
}


int
ompi_coll_libnbc_progress(void)
{
    ompi_coll_libnbc_request_t* request, *next;
    int res;
    int completed = 0;

    if (0 == opal_list_get_size (&mca_coll_libnbc_component.active_requests)) {
        /* no requests -- nothing to do. do not grab a lock */
        return 0;
    }

    /* process active requests, and use mca_coll_libnbc_component.lock to access the
     * mca_coll_libnbc_component.active_requests list */
    OPAL_THREAD_LOCK(&mca_coll_libnbc_component.lock);
    /* return if invoked recursively */
    if (!libnbc_in_progress) {
        libnbc_in_progress = true;

        OPAL_LIST_FOREACH_SAFE(request, next, &mca_coll_libnbc_component.active_requests,
                               ompi_coll_libnbc_request_t) {
            OPAL_THREAD_UNLOCK(&mca_coll_libnbc_component.lock);
            res = NBC_Progress(request);
            if( NBC_CONTINUE != res ) {
                /* done, remove and complete */
                OPAL_THREAD_LOCK(&mca_coll_libnbc_component.lock);
                opal_list_remove_item(&mca_coll_libnbc_component.active_requests,
                                      &request->super.super.super.super);
                OPAL_THREAD_UNLOCK(&mca_coll_libnbc_component.lock);

                if( OMPI_SUCCESS == res || NBC_OK == res || NBC_SUCCESS == res ) {
                    request->super.super.req_status.MPI_ERROR = OMPI_SUCCESS;
                }
                else {
                    request->super.super.req_status.MPI_ERROR = res;
                }
                if(request->super.super.req_persistent) {
                    /* reset for the next communication */
                    request->row_offset = 0;
                }
                if(!request->super.super.req_persistent || !REQUEST_COMPLETE(&request->super.super)) {
            	    ompi_request_complete(&request->super.super, true);
                }
                completed++;
            }
            OPAL_THREAD_LOCK(&mca_coll_libnbc_component.lock);
        }
        libnbc_in_progress = false;
    }
    OPAL_THREAD_UNLOCK(&mca_coll_libnbc_component.lock);

    return completed;
}


static void
libnbc_module_construct(ompi_coll_libnbc_module_t *module)
{
    OBJ_CONSTRUCT(&module->mutex, opal_mutex_t);
    module->comm_registered = false;
}


static void
libnbc_module_destruct(ompi_coll_libnbc_module_t *module)
{
    OBJ_DESTRUCT(&module->mutex);

    /* if we ever were used for a collective op, do the progress cleanup. */
    if (true == module->comm_registered) {
        int32_t tmp =
            OPAL_THREAD_ADD_FETCH32(&mca_coll_libnbc_component.active_comms, -1);
        if (0 == tmp) {
            opal_progress_unregister(ompi_coll_libnbc_progress);
        }
    }
}


OBJ_CLASS_INSTANCE(ompi_coll_libnbc_module_t,
                   mca_coll_base_module_t,
                   libnbc_module_construct,
                   libnbc_module_destruct);


static int
request_start(size_t count, ompi_request_t ** requests)
{
    int res;
    size_t i;

    NBC_DEBUG(5, " ** request_start **\n");

    for (i = 0; i < count; i++) {
        NBC_Handle *handle = (NBC_Handle *) requests[i];
        NBC_Schedule *schedule = handle->schedule;

        NBC_DEBUG(5, "--------------------------------\n");
        NBC_DEBUG(5, "schedule %p size %u\n", &schedule, sizeof(schedule));
        NBC_DEBUG(5, "handle %p size %u\n", &handle, sizeof(handle));
        NBC_DEBUG(5, "data %p size %u\n", &schedule->data, sizeof(schedule->data));
        NBC_DEBUG(5, "req_array %p size %u\n", &handle->req_array, sizeof(handle->req_array));
        NBC_DEBUG(5, "row_offset=%u address=%p size=%u\n", handle->row_offset, &handle->row_offset, sizeof(handle->row_offset));
        NBC_DEBUG(5, "req_count=%u address=%p size=%u\n", handle->req_count, &handle->req_count, sizeof(handle->req_count));
        NBC_DEBUG(5, "tmpbuf address=%p size=%u\n", handle->tmpbuf, sizeof(handle->tmpbuf));
        NBC_DEBUG(5, "--------------------------------\n");

        handle->super.super.req_complete = REQUEST_PENDING;
        handle->nbc_complete = false;

        res = NBC_Start(handle);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
            NBC_DEBUG(5, " ** bad result from NBC_Start **\n");
            return res;
        }
    }

    NBC_DEBUG(5, " ** LEAVING request_start **\n");

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
    ompi_coll_libnbc_request_t *request =
        (ompi_coll_libnbc_request_t*) *ompi_req;

    if( !REQUEST_COMPLETE(&request->super.super) ) {
        return MPI_ERR_REQUEST;
    }

    OMPI_COLL_LIBNBC_REQUEST_RETURN(request);
    *ompi_req = MPI_REQUEST_NULL;

    return OMPI_SUCCESS;
}


static void
request_construct(ompi_coll_libnbc_request_t *request)
{
    request->super.super.req_type = OMPI_REQUEST_COLL;
    request->super.super.req_status._cancelled = 0;
    request->super.super.req_start = request_start;
    request->super.super.req_free = request_free;
    request->super.super.req_cancel = request_cancel;
}


OBJ_CLASS_INSTANCE(ompi_coll_libnbc_request_t,
                   ompi_coll_base_nbc_request_t,
                   request_construct,
                   NULL);
