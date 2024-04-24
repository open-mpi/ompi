/*
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2022 The University of Tennessee and The University
 *
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/runtime/params.h"
#include "ompi/group/group.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"


ompi_comm_rank_failure_callback_t *ompi_rank_failure_cbfunc = NULL;

/**
 * The handling of known failed processes is based on a two level process. On one
 * side the MPI library itself must know the failed processes (in order to be able
 * to correctly handle complex operations such as shrink). On the other side, the
 * failed processes acknowledged by the users should not be altered during any of
 * the internal calls, as they must only be updated upon user request.
 * Thus, the global list (ompi_group_all_failed_procs) is the list of all known
 * failed processes (by the MPI library internals), and it is allegedly updated
 * by the MPI library whenever new failure are noticed. However, the user interact
 * with this list via the MPI functions, and all failure notifications are reported
 * in the context of a communicator. Thus, using a single index to know the user-level
 * acknowledged failure is the simplest solution.
 */
/* deprecated ulfm v1 API */
int ompi_comm_failure_ack_internal(ompi_communicator_t* comm)
{
    opal_mutex_lock(&ompi_group_afp_mutex);
    /* Fix offset in the global failed list */
    comm->any_source_offset = ompi_group_size(ompi_group_all_failed_procs);
    /* Re-enable ANY_SOURCE */
    comm->any_source_enabled = true;
    /* use the AFP lock implicit memory barrier to propagate the update to
     * any_source_enabled at the same time.
     */
    comm->num_acked = -1; /* compat with v2 API: force recompute next time ack_failed is called */
    opal_mutex_unlock(&ompi_group_afp_mutex);

    return OMPI_SUCCESS;
}

/* deprecated ulfm v1 API; used internally in MPI_COMM_AGREE as well */
int ompi_comm_failure_get_acked_internal(ompi_communicator_t* comm, ompi_group_t **group )
{
    int ret, exit_status = OMPI_SUCCESS;
    int range[3];
    ompi_group_t *tmp_sub_group = NULL;

    /*
     * If no failure present, then return the empty group
     */
    if( 0 == comm->any_source_offset ) {
        *group = MPI_GROUP_EMPTY;
        OBJ_RETAIN(MPI_GROUP_EMPTY);
        exit_status = OMPI_SUCCESS;
        goto cleanup;
    }

    tmp_sub_group = OBJ_NEW(ompi_group_t);

    /*
     * Access just the offset number of failures
     */
    opal_mutex_lock(&ompi_group_afp_mutex);
    range[0] = 0;
    range[1] = comm->any_source_offset - 1;
    range[2] = 1;

    ret = ompi_group_range_incl(ompi_group_all_failed_procs, 1, &range, &tmp_sub_group);
    opal_mutex_unlock(&ompi_group_afp_mutex);
    if( OMPI_SUCCESS != ret ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Access the intersection between the failed subgroup and our group
     */
    if( OMPI_COMM_IS_INTER(comm) ) {
        ret = ompi_group_intersection(tmp_sub_group,
                                      comm->c_local_group,
                                      group);
    } else {
        ret = ompi_group_intersection(tmp_sub_group,
                                      comm->c_remote_group,
                                      group);
    }

    if( OMPI_SUCCESS != ret ) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != tmp_sub_group ) {
        OBJ_RELEASE(tmp_sub_group);
        tmp_sub_group = NULL;
    }

    return exit_status;
}

/* New v2 interface get_failed/ack_failed.
 * This interface uses a cached value comm->num_acked to track how many
 * processes in the group of this comm have been acknowledged in prior calls.
 * For compatibility with v1 interface (failure_get_acked), it still updates
 * the comm->any_source_offset, and the v1 interface failure_ack may erase the
 * cached value comm->num_acked with -1 to force recomputing this value in mixed
 * use cases (that is calling failure_ack will force full recomputation of
 * comm->num_acked during the next ack_failed call). */
int ompi_comm_ack_failed_internal(ompi_communicator_t* comm, int num_to_ack, int *num_acked) {
    int ret, exit_status = MPI_SUCCESS;
    int nf = -1, na = -1;
    ompi_group_t *c_group = OMPI_COMM_IS_INTER(comm)? comm->c_local_group: comm->c_remote_group;
    ompi_group_t *failed_group = NULL;

    opal_mutex_lock(&ompi_group_afp_mutex);

    /* shortcut when reading only */
    if(num_to_ack <= comm->num_acked)
        goto return_num_acked;

    /* shortcut when no new faults */
    if(comm->any_source_offset == ompi_group_size(ompi_group_all_failed_procs)
    && comm->num_acked >= 0 /* reset by a call to v1 API? */)
        goto return_num_acked;

    /* compute num_acked */
    ret = ompi_group_intersection(ompi_group_all_failed_procs,
                                  c_group, &failed_group);
    if(OMPI_SUCCESS != ret) {
        exit_status = ret;
        goto cleanup;
    }
    nf = ompi_group_size(failed_group);
    na = (num_to_ack < nf)? num_to_ack: nf; /* never ack more than requested */

    if(comm->num_acked < 0) { /* reset by a call to the v1 API: recompute it */
        if(0 == comm->any_source_offset) comm->num_acked = 0;
        else {
            int aso = comm->any_source_offset - 1;
            ret = ompi_group_translate_ranks(ompi_group_all_failed_procs, 1, &aso,
                                             failed_group, &comm->num_acked);
            comm->num_acked++; /* make it a group size again */
            if(OMPI_SUCCESS != ret) {
                exit_status = ret;
                goto cleanup;
            }
        }
    }

    if(comm->num_acked < na) { /* comm->num_acked needs to be updated during this call */
        comm->num_acked = na;
        if(nf == na) {
            /* all faults on comm acknowledged, resume any source then */
            comm->any_source_enabled = true;
            comm->any_source_offset = ompi_group_size(ompi_group_all_failed_procs); // compat with v1 interface
        }
        else {
            /* some faults not acknowledged, do not resume any source then, but
             * still update any_source_offset */
            assert(comm->num_acked > 0);
            int cna = comm->num_acked - 1;
            ret = ompi_group_translate_ranks(failed_group, 1, &cna,
                                             ompi_group_all_failed_procs, &comm->any_source_offset); // compat with v1 interface
            comm->any_source_offset++; /* make it a group size again */
            if(OMPI_SUCCESS != ret) {
                exit_status = ret;
                goto cleanup;
            }
        }
    }

return_num_acked:
    *num_acked = comm->num_acked;

cleanup:
    if(NULL != failed_group) OBJ_RELEASE(failed_group);
    /* use the AFP lock implicit memory barrier to propagate the update to
     * any_source_enabled, num_acked, etc. at the same time.
     */
    opal_mutex_unlock(&ompi_group_afp_mutex);

    return exit_status;
}

int ompi_comm_get_failed_internal(ompi_communicator_t* comm, ompi_group_t **group)
{
    int ret, exit_status = OMPI_SUCCESS;
    ompi_group_t *c_group = OMPI_COMM_IS_INTER(comm)? comm->c_local_group: comm->c_remote_group;
    opal_mutex_lock(&ompi_group_afp_mutex);
    ret = ompi_group_intersection(ompi_group_all_failed_procs,
                                  c_group,
                                  group);
    opal_mutex_unlock(&ompi_group_afp_mutex);
    if( OMPI_SUCCESS != ret ) {
        exit_status = ret;
    }
    return exit_status;
}

int ompi_comm_shrink_internal(ompi_communicator_t* comm, ompi_communicator_t** newcomm)
{
    int ret, exit_status = OMPI_SUCCESS;
    int flag = 1;
    ompi_group_t *failed_group = NULL, *comm_group = NULL, *alive_group = NULL, *alive_rgroup = NULL;
    ompi_communicator_t *newcomp = NULL;
    int mode;
#if OPAL_ENABLE_DEBUG
    double start, stop;
#endif

    *newcomm = MPI_COMM_NULL;

    /*
     * Step 1: Agreement on failed group in comm
     */
    /* --------------------------------------------------------- */
    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s ompi: comm_shrink: Agreement on failed processes",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME) ));
#if OPAL_ENABLE_DEBUG
    start = PMPI_Wtime();
#endif
    opal_mutex_lock(&ompi_group_afp_mutex);
    ompi_group_intersection(comm->c_remote_group, ompi_group_all_failed_procs, &failed_group);
    opal_mutex_unlock(&ompi_group_afp_mutex);
#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
#endif
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_shrink: group_inter: %g seconds",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-start));
#if OPAL_ENABLE_DEBUG
    start = PMPI_Wtime();
#endif
    do {
        /* We need to create the list of alive processes. Thus, we don't care about
         * the value of flag, instead we are only using the globally consistent
         * return value.
         */
        ret = comm->c_coll->coll_agree( &flag,
                                        1,
                                        &ompi_mpi_int.dt,
                                        &ompi_mpi_op_band.op,
                                        &failed_group, true,
                                        comm,
                                        comm->c_coll->coll_agree_module);
    } while( MPI_ERR_PROC_FAILED == ret );
#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
#endif
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_shrink: AGREE: %g seconds",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-start));
    if(OMPI_SUCCESS != ret) {
        opal_output(0, "%s:%d Agreement failure: %d\n", __FILE__, __LINE__, ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Step 2: Determine ranks for new communicator
     */
    /* --------------------------------------------------------- */
    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s ompi: comm_shrink: Determine ranking for new communicator intra %d",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), OMPI_COMM_IS_INTRA(comm)));
#if OPAL_ENABLE_DEBUG
    start = PMPI_Wtime();
#endif

    /* Create 'alive' groups */
    mode = OMPI_COMM_CID_INTRA_FT;
    comm_group = comm->c_local_group;
    ret = ompi_group_difference(comm_group, failed_group, &alive_group);
    if( OMPI_SUCCESS != ret ) {
        exit_status = ret;
        goto cleanup;
    }
    if( OMPI_COMM_IS_INTER(comm) ) {
        mode = OMPI_COMM_CID_INTER_FT;
        comm_group = comm->c_remote_group;
        ret = ompi_group_difference(comm_group, failed_group, &alive_rgroup);
        if( OMPI_SUCCESS != ret ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    ret = ompi_comm_set( &newcomp,                 /* new comm */
                         comm,                     /* old comm */
                         0,                        /* local_size */
                         NULL,                     /* local_ranks */
                         0,                        /* remote_size */
                         NULL,                     /* remote_ranks */
                         comm->c_keyhash,          /* attrs */
                         comm->error_handler,      /* error handler */
                         alive_group,              /* local group */
                         alive_rgroup,             /* remote group */
                         0                         /* flags */
                       );
    if( OMPI_SUCCESS != ret ) {
        exit_status = ret;
        goto cleanup;
    }
    if( NULL == newcomp ) {
        exit_status = MPI_ERR_INTERN;
        goto cleanup;
    }
#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
#endif
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_shrink: GRP COMPUTATION: %g seconds\n",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-start));
    /*
     * Step 3: Determine context id
     */
    /* --------------------------------------------------------- */
    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s ompi: comm_shrink: Determine context id",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME) ));
#if OPAL_ENABLE_DEBUG
    start = PMPI_Wtime();
#endif
    ret = ompi_comm_nextcid( newcomp,  /* new communicator */
                             comm,     /* old comm */
                             NULL,     /* bridge comm */
                             NULL,     /* local leader */
                             NULL,     /* remote_leader */
                             -1,       /* send_first */
                             mode);    /* mode */
    if( OMPI_SUCCESS != ret ) {
        opal_output_verbose(1, ompi_ftmpi_output_handle,
                            "%s ompi: comm_shrink: Determine context id failed with error %d",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), ret);
        exit_status = ret;
        goto cleanup;
    }
#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
#endif
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_shrink: NEXT CID: %g seconds\n",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-start));
    /*
     * Step 4: Activate the communicator
     */
    /* --------------------------------------------------------- */
    /* Set name for debugging purposes */
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d SHRUNK FROM %d",
             ompi_comm_get_local_cid(newcomp),
             ompi_comm_get_local_cid(comm));
#if OPAL_ENABLE_DEBUG
    start = PMPI_Wtime();
#endif
    /* activate communicator and init coll-module */
    ret = ompi_comm_activate( &newcomp, /* new communicator */
                              comm,
                              NULL,
                              NULL,
                              NULL,
                              -1,
                              mode);
    if( OMPI_SUCCESS != ret ) {
        exit_status = ret;
        goto cleanup;
    }
#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
#endif
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_shrink: COLL SELECT: %g seconds\n",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-start));

    /** Step 5: assign the output communicator */
    *newcomm = newcomp;

 cleanup:
    if( NULL != failed_group ) {
        OBJ_RELEASE(failed_group);
        failed_group = NULL;
    }
    if( NULL != alive_group ) {
        OBJ_RELEASE(alive_group);
        alive_group = NULL;
    }
    if( NULL != alive_rgroup ) {
        OBJ_RELEASE(alive_rgroup);
        alive_rgroup = NULL;
    }

    return exit_status;
}

struct ompi_comm_ishrink_context_t {
    opal_object_t super;
    ompi_communicator_t *comm;
    ompi_communicator_t **newcomm;
    ompi_group_t *failed_group;
    ompi_group_t *alive_group;
    ompi_group_t *alive_rgroup;
    int flag;
    double start;
};
typedef struct ompi_comm_ishrink_context_t ompi_comm_ishrink_context_t;
OBJ_CLASS_INSTANCE(ompi_comm_ishrink_context_t, opal_object_t, NULL, NULL);

static int ompi_comm_ishrink_check_agree(ompi_comm_request_t *request);
static int ompi_comm_ishrink_check_setrank(ompi_comm_request_t *request);
static int ompi_comm_ishrink_check_cid(ompi_comm_request_t *request);
static int ompi_comm_ishrink_check_activate(ompi_comm_request_t *request);

int ompi_comm_ishrink_internal(ompi_communicator_t* comm, ompi_communicator_t** newcomm, ompi_request_t** req)
{
    int rc;
#if OPAL_ENABLE_DEBUG
    double stop;
#endif
    ompi_comm_request_t *request;
    ompi_comm_ishrink_context_t *context;
    ompi_request_t *subreq[1];

    *newcomm = MPI_COMM_NULL;

    request = ompi_comm_request_get();
    if(OPAL_UNLIKELY( NULL == request )) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context = OBJ_NEW(ompi_comm_ishrink_context_t);
    if(OPAL_UNLIKELY( NULL == context )) {
        ompi_comm_request_return(request);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    context->comm = comm;
    context->newcomm = newcomm;
    context->failed_group = NULL;
    context->alive_group = NULL;
    context->alive_rgroup = NULL;
    request->context = &context->super;
    request->super.req_mpi_object.comm = comm;

    /*
     * Step 1: Agreement on failed group in comm
     */
    /* --------------------------------------------------------- */
    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s ompi: comm_ishrink: Agreement on failed processes",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME) ));
#if OPAL_ENABLE_DEBUG
    context->start = PMPI_Wtime();
#endif
    ompi_group_intersection(comm->c_remote_group, ompi_group_all_failed_procs, &context->failed_group);
#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_ishrink: group_inter: %g seconds",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-context->start));
    context->start = PMPI_Wtime();
#endif
    /* We need to create the list of alive processes. Thus, we don't care about
     * the value of flag, instead we are only using the globally consistent
     * return value.
     */
    context->flag = 1;
    rc = comm->c_coll->coll_iagree( &context->flag,
                                    1,
                                    &ompi_mpi_int.dt,
                                    &ompi_mpi_op_band.op,
                                    &context->failed_group, true,
                                    comm,
                                    subreq,
                                    comm->c_coll->coll_iagree_module );
    if( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(context->failed_group);
        ompi_comm_request_return(request);
        return rc;
    }

    ompi_comm_request_schedule_append(request, ompi_comm_ishrink_check_agree, subreq, 1);

    /* kick off the request */
    ompi_comm_request_start(request);
    *req = &request->super;

    return OMPI_SUCCESS;
}

static int ompi_comm_ishrink_check_agree(ompi_comm_request_t *request) {
    ompi_comm_ishrink_context_t *context =
        (ompi_comm_ishrink_context_t *)request->context;
    ompi_communicator_t *comm = context->comm;
    ompi_request_t *subreq[1];
    ompi_group_t *comm_group = NULL;
    int rc;
#if OPAL_ENABLE_DEBUG
    double stop;
#endif

#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_ishrink: AGREE: %g seconds",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-context->start));
#endif
    rc = request->super.req_status.MPI_ERROR;
    if( (OMPI_SUCCESS != rc) && (MPI_ERR_PROC_FAILED != rc) ) {
        opal_output(0, "%s:%d Agreement failure: %d\n", __FILE__, __LINE__, rc);
        OBJ_RELEASE(context->failed_group);
        ompi_comm_request_return(request);
        return rc;
    }

    if( MPI_ERR_PROC_FAILED == rc ) {
        /* previous round found more failures, redo */
        OBJ_RELEASE(context->failed_group);
        request->super.req_status.MPI_ERROR = MPI_SUCCESS;
        context->flag = 1;
        rc = comm->c_coll->coll_iagree( &context->flag,
                                        1,
                                        &ompi_mpi_int.dt,
                                        &ompi_mpi_op_band.op,
                                        &context->failed_group, true,
                                        comm,
                                        subreq,
                                        comm->c_coll->coll_iagree_module );
        if( OMPI_SUCCESS != rc ) {
            OBJ_RELEASE(context->failed_group);
            ompi_comm_request_return(request);
            return rc;
        }
        ompi_comm_request_schedule_append(request, ompi_comm_ishrink_check_agree, subreq, 1);
        return OMPI_SUCCESS;
    }

    /*
     * Step 2: Determine ranks for new communicator
     */
    /* --------------------------------------------------------- */
    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s ompi: comm_ishrink: Determine ranking for new communicator",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME) ));
#if OPAL_ENABLE_DEBUG
    context->start = PMPI_Wtime();
#endif

    /* Create 'alive' groups */
    comm_group = comm->c_local_group;
    rc = ompi_group_difference(comm_group, context->failed_group, &context->alive_group);
    if( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(context->failed_group);
        ompi_comm_request_return(request);
        return rc;
    }
    if( OMPI_COMM_IS_INTER(comm) ) {
        comm_group = comm->c_remote_group;
        rc = ompi_group_difference(comm_group, context->failed_group, &context->alive_rgroup);
        if( OMPI_SUCCESS != rc ) {
            OBJ_RELEASE(context->alive_group);
            OBJ_RELEASE(context->failed_group);
            ompi_comm_request_return(request);
            return rc;
        }
    }
    OBJ_RELEASE(context->failed_group);

    rc = ompi_comm_set_nb( context->newcomm,         /* new comm */
                           comm,                     /* old comm */
                           0,                        /* local_size */
                           NULL,                     /* local_ranks */
                           0,                        /* remote_size */
                           NULL,                     /* remote_ranks */
                           comm->c_keyhash,          /* attrs */
                           comm->error_handler,      /* error handler */
                           context->alive_group,     /* local group */
                           context->alive_rgroup,    /* remote group */
                           0,                        /* flags */
                           subreq
                         );
    if( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(context->alive_group);
        if( NULL != context->alive_rgroup ) {
            OBJ_RELEASE(context->alive_rgroup);
        }
        ompi_comm_request_return(request);
        return rc;
    }

    ompi_comm_request_schedule_append(request, ompi_comm_ishrink_check_setrank, subreq, subreq[0] ? 1 : 0);
    return OMPI_SUCCESS;
}

static int ompi_comm_ishrink_check_setrank(ompi_comm_request_t *request) {
    ompi_comm_ishrink_context_t *context =
        (ompi_comm_ishrink_context_t *)request->context;
    ompi_request_t *subreq[1];
    int rc, mode;
#if OPAL_ENABLE_DEBUG
    double stop;
#endif

    /* cleanup temporary groups */
    OBJ_RELEASE(context->alive_group);
    if( NULL != context->alive_rgroup ) {
        OBJ_RELEASE(context->alive_rgroup);
    }

    /* check errors in prior step */
    rc = request->super.req_status.MPI_ERROR;
    if( OMPI_SUCCESS != rc ) {
        opal_output_verbose(1, ompi_ftmpi_output_handle,
                            "%s ompi: comm_ishrink: Construction failed with error %d",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), rc);
        OBJ_RELEASE(*context->newcomm);
        ompi_comm_request_return(request);
        return rc;
    }

#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_ishrink: GRP COMPUTATION: %g seconds\n",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-context->start));
#endif

    /*
     * Step 3: Determine context id
     */
    mode = OMPI_COMM_CID_INTRA_FT;
    if( OMPI_COMM_IS_INTER(context->comm) ) {
        mode = OMPI_COMM_CID_INTER_FT;
    }
    /* --------------------------------------------------------- */
    OPAL_OUTPUT_VERBOSE((5, ompi_ftmpi_output_handle,
                         "%s ompi: comm_ishrink: Determine context id",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME) ));
#if OPAL_ENABLE_DEBUG
    context->start = PMPI_Wtime();
#endif
    rc = ompi_comm_nextcid_nb( *context->newcomm, /* new communicator */
                               context->comm,     /* old comm */
                               NULL,              /* bridge comm */
                               NULL,              /* local leader */
                               NULL,              /* remote_leader */
                               false,             /* send_first */
                               mode,              /* mode */
                               subreq );
    if( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(*context->newcomm);
        ompi_comm_request_return(request);
        return rc;
    }

    ompi_comm_request_schedule_append(request, ompi_comm_ishrink_check_cid, subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_ishrink_check_cid(ompi_comm_request_t *request) {
    ompi_comm_ishrink_context_t *context =
        (ompi_comm_ishrink_context_t *)request->context;
    ompi_request_t *subreq[1];
    int rc, mode;
#if OPAL_ENABLE_DEBUG
    double stop;
#endif

    rc = request->super.req_status.MPI_ERROR;
    if( OMPI_SUCCESS != rc ) {
        opal_output_verbose(1, ompi_ftmpi_output_handle,
                            "%s ompi: comm_ishrink: Determine context id failed with error %d",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), rc);
        OBJ_RELEASE(*context->newcomm);
        ompi_comm_request_return(request);
        return rc;
    }
#if OPAL_ENABLE_DEBUG
    stop = PMPI_Wtime();
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_ishrink: NEXT CID: %g seconds\n",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-context->start));
#endif

    /*
     * Step 4: Activate the communicator
     */
    mode = OMPI_COMM_CID_INTRA_FT;
    if( OMPI_COMM_IS_INTER(context->comm) ) {
        mode = OMPI_COMM_CID_INTER_FT;
    }
    /* --------------------------------------------------------- */
    /* Set name for debugging purposes */
    ompi_communicator_t *newcomp = *context->newcomm;
    snprintf(newcomp->c_name, MPI_MAX_OBJECT_NAME, "MPI COMMUNICATOR %d SHRUNK FROM %d",
             ompi_comm_get_local_cid(newcomp),
             ompi_comm_get_local_cid(context->comm));
#if OPAL_ENABLE_DEBUG
    context->start = PMPI_Wtime();
#endif
    /* activate communicator and init coll-module */
    rc = ompi_comm_activate_nb( context->newcomm, /* new communicator */
                                context->comm,
                                NULL,
                                NULL,
                                NULL,
                                -1,
                                mode,
                                subreq );
    if( OMPI_SUCCESS != rc ) {
        OBJ_RELEASE(*context->newcomm);
        ompi_comm_request_return(request);
        return rc;
    }

    ompi_comm_request_schedule_append(request, ompi_comm_ishrink_check_activate, subreq, 1);

    return OMPI_SUCCESS;
}

static int ompi_comm_ishrink_check_activate(ompi_comm_request_t *request) {
    ompi_comm_ishrink_context_t *context =
        (ompi_comm_ishrink_context_t *)request->context;
    int rc;
#if OPAL_ENABLE_DEBUG
    double stop;
#endif

    rc = request->super.req_status.MPI_ERROR;
    if( OMPI_SUCCESS != rc ) {
        opal_output_verbose(1, ompi_ftmpi_output_handle,
                            "%s ompi: comm_ishrink: Activation failed with error %d",
                            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), rc);
        OBJ_RELEASE(*context->newcomm);
        ompi_comm_request_return(request);
        return rc;
    }
#if OPAL_ENABLE_DEBUG
    stop = MPI_Wtime();
    OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                         "%s ompi: comm_ishrink: COLL SELECT: %g seconds\n",
                         OMPI_NAME_PRINT(OMPI_PROC_MY_NAME), stop-context->start));
#endif

    return OMPI_SUCCESS;
}


bool ompi_comm_is_proc_active(ompi_communicator_t *comm, int peer_id, bool remote)
{
    ompi_proc_t* ompi_proc;

    /* Check MPI_ANY_SOURCE differently */
    if( OPAL_UNLIKELY(peer_id == MPI_ANY_SOURCE) ) {
        return ompi_comm_is_any_source_enabled(comm);
    }
    /* PROC_NULL is always 'ok' */
    if( OPAL_UNLIKELY(peer_id == MPI_PROC_NULL) ) {
        return true;
    }
#if OPAL_ENABLE_DEBUG
    /* Sanity check. Only valid ranks are accepted.  */
    if( (peer_id < 0) ||
        (!OMPI_COMM_IS_INTRA(comm) && peer_id >= ompi_comm_remote_size(comm)) ||
        ( OMPI_COMM_IS_INTRA(comm) && peer_id >= ompi_comm_size(comm) ) ) {
        return false;
    }
#endif
    ompi_proc = ompi_group_get_proc_ptr((remote ? comm->c_remote_group : comm->c_local_group),
                                        peer_id, false);
    /* If the proc is not known yet (get_proc_ptr returns NULL for a valid
     * peer_id), then we assume that the proc is alive. When it is dead, the
     * proc will exist. */
    return (NULL == ompi_proc) ? true : ompi_proc_is_active(ompi_proc);
}

int ompi_comm_set_rank_failed(ompi_communicator_t *comm, int peer_id, bool remote)
{
#if OPAL_ENABLE_DEBUG
    /* populate the proc in the comm's group array so that it is not a
       sentinel and can be read as failed */
    ompi_proc_t *ompi_proc = ompi_group_get_proc_ptr((remote ? comm->c_remote_group : comm->c_local_group),
                                                     peer_id, true);
    assert(NULL != ompi_proc);
#endif

    /* Disable ANY_SOURCE */
    comm->any_source_enabled = false;
    opal_atomic_wmb(); /* non-locked update needs a memory barrier to propagate */

    /* Disable collectives */
    MCA_PML_CALL(revoke_comm(comm, true));

    if( NULL != ompi_rank_failure_cbfunc ) {
        (*ompi_rank_failure_cbfunc)(comm, peer_id, remote);
    }

    return OMPI_SUCCESS;
}
