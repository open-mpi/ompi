/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Voltaire All rights reserved.
 * Copyright (c) 2006-2010 University of Houston.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/dss/dss.h"
#include "opal/mca/pmix/pmix.h"

#include "ompi/proc/proc.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"
#include "ompi/constants.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_list.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/request/request.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/mca/cid/base/base.h"

#include "cid_basic.h"

static int cid_basic_nextcid_nb (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                 ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                                 bool send_first, int mode, ompi_request_t **req);

static int cid_basic_nextcid (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                              ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                              bool send_first, int mode);

static int cid_basic_release_nb (int cid, ompi_request_t **req);

static int cid_basic_release (int cid);

static int cid_basic_init (void);

ompi_cid_base_module_t ompi_cid_basic_module = {
    .nextcid_nb = cid_basic_nextcid_nb,
    .nextcid = cid_basic_nextcid,
    .activate_nb= ompi_cid_base_comm_activate_nb,
    .activate= ompi_cid_base_comm_activate,
    .release_nb = cid_basic_release_nb,
    .release = cid_basic_release,
    .init = cid_basic_init
};

static opal_mutex_t ompi_cid_lock = OPAL_MUTEX_STATIC_INIT;

static int cid_basic_init (void)
{
    return OMPI_SUCCESS;
}

static int cid_basic_release_nb(int cid, ompi_request_t **req) {
    *req = &ompi_request_empty;
    return OMPI_SUCCESS;
}

static int cid_basic_release(int cid) {
    return OMPI_SUCCESS;
}

/* find the next available local cid and start an allreduce */
static int ompi_comm_allreduce_getnextcid (ompi_comm_request_t *request);
/* verify that the maximum cid is locally available and start an allreduce */
static int ompi_comm_checkcid (ompi_comm_request_t *request);
/* verify that the cid was available globally */
static int ompi_comm_nextcid_check_flag (ompi_comm_request_t *request);

static volatile int64_t ompi_comm_cid_lowest_id = INT64_MAX;

static int cid_basic_nextcid_nb (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                 ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                                 bool send_first, int mode, ompi_request_t **req)
{
    ompi_cid_base_cid_context_t *context;
    ompi_comm_request_t *request;
    int ret;

    context = OBJ_NEW(ompi_cid_base_cid_context_t);
    if (OPAL_UNLIKELY(NULL == context)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    ret = ompi_cid_base_cid_context_init (context, newcomm, comm, bridgecomm, arg0, arg1,
                                          "nextcid", send_first, mode);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OBJ_RELEASE(context);
        return ret;
    }

    context->start = ompi_mpi_communicators.lowest_free;

    request = ompi_comm_request_get ();
    if (NULL == request) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->context = &context->super;

    ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);
    ompi_comm_request_start (request);

    *req = &request->super;


    return OMPI_SUCCESS;
}

int cid_basic_nextcid (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                       ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                       bool send_first, int mode)
{
    ompi_request_t *req;
    int rc;

    rc = cid_basic_nextcid_nb (newcomm, comm, bridgecomm, arg0, arg1, send_first, mode, &req);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    ompi_request_wait_completion (req);
    rc = req->req_status.MPI_ERROR;
    ompi_comm_request_return ((ompi_comm_request_t *) req);

    return rc;
}

static int ompi_comm_allreduce_getnextcid (ompi_comm_request_t *request)
{
    ompi_cid_base_cid_context_t *context = (ompi_cid_base_cid_context_t *) request->context;
    int64_t my_id = ((int64_t) ompi_comm_get_cid (context->comm) << 32 | context->pml_tag);
    ompi_request_t *subreq;
    bool flag;
    int ret;
    int participate = (context->newcomm->c_local_group->grp_my_rank != MPI_UNDEFINED);

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);
    }

    if (ompi_comm_cid_lowest_id < my_id) {
        OPAL_THREAD_UNLOCK(&ompi_cid_lock);
        return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);
    }

    ompi_comm_cid_lowest_id = my_id;

    /**
     * This is the real algorithm described in the doc
     */
    if( participate ){
        flag = false;
        context->nextlocal_cid = mca_pml.pml_max_contextid;
        for (unsigned int i = context->start ; i < mca_pml.pml_max_contextid ; ++i) {
            flag = opal_pointer_array_test_and_set_item (&ompi_mpi_communicators, i,
                                                         context->comm);
            if (true == flag) {
                context->nextlocal_cid = i;
                break;
            }
        }
    } else {
        context->nextlocal_cid = 0;
    }

    ret = context->allreduce_fn (&context->nextlocal_cid, &context->nextcid, 1, MPI_MAX,
                                 context, &subreq);
    /* there was a failure during non-blocking collective
     * all we can do is abort
     */
    if (OMPI_SUCCESS != ret) {
        goto err_exit;
    }

    if ( ((unsigned int) context->nextlocal_cid == mca_pml.pml_max_contextid) ) {
        /* Our local CID space is out, others already aware (allreduce above) */
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }
    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    /* next we want to verify that the resulting commid is ok */
    return ompi_comm_request_schedule_append (request, ompi_comm_checkcid, &subreq, 1);
err_exit:
    if (participate && flag) {
        opal_pointer_array_test_and_set_item(&ompi_mpi_communicators, context->nextlocal_cid, NULL);
    }
    ompi_comm_cid_lowest_id = INT64_MAX;
    OPAL_THREAD_UNLOCK(&ompi_cid_lock);
    return ret;

}

static int ompi_comm_checkcid (ompi_comm_request_t *request)
{
    ompi_cid_base_cid_context_t *context = (ompi_cid_base_cid_context_t *) request->context;
    ompi_request_t *subreq;
    int ret;
    int participate = (context->newcomm->c_local_group->grp_my_rank != MPI_UNDEFINED);

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_checkcid, NULL, 0);
    }

    if( !participate ){
        context->flag = 1;
    } else {
        context->flag = (context->nextcid == context->nextlocal_cid);
        if ( participate && !context->flag) {
            opal_pointer_array_set_item(&ompi_mpi_communicators, context->nextlocal_cid, NULL);

            context->flag = opal_pointer_array_test_and_set_item (&ompi_mpi_communicators,
                                                                  context->nextcid, context->comm);
        }
    }

    ++context->iter;

    ret = context->allreduce_fn (&context->flag, &context->rflag, 1, MPI_MIN, context, &subreq);
    if (OMPI_SUCCESS == ret) {
        ompi_comm_request_schedule_append (request, ompi_comm_nextcid_check_flag, &subreq, 1);
    } else {
        if (participate && context->flag ) {
            opal_pointer_array_test_and_set_item(&ompi_mpi_communicators, context->nextlocal_cid, NULL);
        }
        ompi_comm_cid_lowest_id = INT64_MAX;
    }

    OPAL_THREAD_UNLOCK(&ompi_cid_lock);
    return ret;
}

static int ompi_comm_nextcid_check_flag (ompi_comm_request_t *request)
{
    ompi_cid_base_cid_context_t *context = (ompi_cid_base_cid_context_t *) request->context;
    int participate = (context->newcomm->c_local_group->grp_my_rank != MPI_UNDEFINED);

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_nextcid_check_flag, NULL, 0);
    }

    if (1 == context->rflag) {
        if( !participate ) {
            /* we need to provide something sane here
             * but we cannot use `nextcid` as we may have it
             * in-use, go ahead with next locally-available CID
             */
            context->nextlocal_cid = mca_pml.pml_max_contextid;
            for (unsigned int i = context->start ; i < mca_pml.pml_max_contextid ; ++i) {
                bool flag;
                flag = opal_pointer_array_test_and_set_item (&ompi_mpi_communicators, i,
                                                                context->comm);
                if (true == flag) {
                    context->nextlocal_cid = i;
                    break;
                }
            }
            context->nextcid = context->nextlocal_cid;
        }

        /* set the according values to the newcomm */
        context->newcomm->c_contextid = context->nextcid;
        opal_pointer_array_set_item (&ompi_mpi_communicators, context->nextcid, context->newcomm);

        /* unlock the cid generator */
        ompi_comm_cid_lowest_id = INT64_MAX;
        OPAL_THREAD_UNLOCK(&ompi_cid_lock);

        /* done! */
        return OMPI_SUCCESS;
    }

    if (participate && (1 == context->flag)) {
        /* we could use this cid, but other don't agree */
        opal_pointer_array_set_item (&ompi_mpi_communicators, context->nextcid, NULL);
        context->start = context->nextcid + 1; /* that's where we can start the next round */
    }

    ++context->iter;

    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    /* try again */
    return ompi_comm_allreduce_getnextcid (request);
}

