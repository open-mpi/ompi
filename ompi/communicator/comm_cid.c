/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
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
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2023 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2020-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "opal/mca/pmix/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"

#include "ompi/proc/proc.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"
#include "ompi/constants.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_list.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/request/request.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/ompi_rte.h"

#include "pmix.h"

/* for use when we don't have a PMIx that supports CID generation */
opal_atomic_int64_t ompi_comm_next_base_cid = 1;

/* A macro comparing two CIDs */
#define OMPI_COMM_CID_IS_LOWER(comm1,comm2) ( ((comm1)->c_index < (comm2)->c_index)? 1:0)

struct ompi_comm_cid_context_t;

typedef int (*ompi_comm_allreduce_impl_fn_t) (int *inbuf, int *outbuf, int count, struct ompi_op_t *op,
                                              struct ompi_comm_cid_context_t *cid_context,
                                              ompi_request_t **req);


struct ompi_comm_cid_context_t {
    opal_object_t super;

    ompi_communicator_t *newcomm;
    ompi_communicator_t **newcommp;
    ompi_communicator_t *comm;
    ompi_communicator_t *bridgecomm;

    ompi_comm_allreduce_impl_fn_t allreduce_fn;

    int nextcid;
    int nextlocal_cid;
#if OPAL_ENABLE_FT_MPI
    /* Revoke messages are unexpected and can be received even after a
     * communicator has been freed locally. If a new communicator reuses the
     * cid, we need to avoid revoking that new communicator instead of the
     * previous (freed) one, when a stall revoke message is received. An
     * epoch is attached to any cid, so that we can recognize which
     * communicator (cid, epoch) we want to revoke. MPI matching is not
     * modified. */
    int nextcid_epoch;
#endif /* OPAL_ENABLE_FT_MPI */
    int start;
    int flag, rflag;
    int local_leader;
    int remote_leader;
    int iter;
    /** storage for activate barrier */
    int ok;
    char *port_string;
    bool send_first;
    int pml_tag;
    char *pmix_tag;
};

typedef struct ompi_comm_cid_context_t ompi_comm_cid_context_t;

static void mca_comm_cid_context_construct (ompi_comm_cid_context_t *context)
{
    memset ((void *) ((intptr_t) context + sizeof (context->super)), 0, sizeof (*context) - sizeof (context->super));
}

static void mca_comm_cid_context_destruct (ompi_comm_cid_context_t *context)
{
    free (context->port_string);
    free (context->pmix_tag);
}

OBJ_CLASS_INSTANCE (ompi_comm_cid_context_t, opal_object_t,
                    mca_comm_cid_context_construct,
                    mca_comm_cid_context_destruct);

struct ompi_comm_allreduce_context_t {
    opal_object_t super;

    int *inbuf;
    int *outbuf;
    int count;
    struct ompi_op_t *op;
    ompi_comm_cid_context_t *cid_context;
    int *tmpbuf;

    /* for group allreduce */
    int peers_comm[3];
};

typedef struct ompi_comm_allreduce_context_t ompi_comm_allreduce_context_t;

static void ompi_comm_allreduce_context_construct (ompi_comm_allreduce_context_t *context)
{
    memset ((void *) ((intptr_t) context + sizeof (context->super)), 0, sizeof (*context) - sizeof (context->super));
}

static void ompi_comm_allreduce_context_destruct (ompi_comm_allreduce_context_t *context)
{
    free (context->tmpbuf);
}

OBJ_CLASS_INSTANCE (ompi_comm_allreduce_context_t, opal_object_t,
                    ompi_comm_allreduce_context_construct,
                    ompi_comm_allreduce_context_destruct);

/**
 * These functions make sure, that we determine the global result over
 * an intra communicators (simple), an inter-communicator and a
 * pseudo inter-communicator described by two separate intra-comms
 * and a bridge-comm (intercomm-create scenario).
 */

/* non-blocking intracommunicator allreduce */
static int ompi_comm_allreduce_intra_nb (int *inbuf, int *outbuf, int count,
                                         struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                         ompi_request_t **req);

/* non-blocking intercommunicator allreduce */
static int ompi_comm_allreduce_inter_nb (int *inbuf, int *outbuf, int count,
                                         struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                         ompi_request_t **req);

static int ompi_comm_allreduce_group_nb (int *inbuf, int *outbuf, int count,
                                         struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                         ompi_request_t **req);

static int ompi_comm_allreduce_intra_pmix_nb (int *inbuf, int *outbuf, int count,
                                              struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                              ompi_request_t **req);

static int ompi_comm_allreduce_intra_bridge_nb (int *inbuf, int *outbuf, int count,
                                                struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                                ompi_request_t **req);

#if OPAL_ENABLE_FT_MPI
static int ompi_comm_ft_allreduce_intra_nb(int *inbuf, int *outbuf, int count,
                                           struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                           ompi_request_t **req);

static int ompi_comm_ft_allreduce_inter_nb(int *inbuf, int *outbuf, int count,
                                           struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                           ompi_request_t **req);

static int ompi_comm_ft_allreduce_intra_pmix_nb(int *inbuf, int *outbuf, int count,
                                                struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                                ompi_request_t **req);
#endif /* OPAL_ENABLE_FT_MPI */


static opal_mutex_t ompi_cid_lock = OPAL_MUTEX_STATIC_INIT;


int ompi_comm_cid_init (void)
{
    return OMPI_SUCCESS;
}

static ompi_comm_cid_context_t *mca_comm_cid_context_alloc (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                                            ompi_communicator_t *bridgecomm, const void *arg0,
                                                            const void *arg1, const char *pmix_tag, bool send_first,
                                                            int mode)
{
    ompi_comm_cid_context_t *context;

    context = OBJ_NEW(ompi_comm_cid_context_t);
    if (OPAL_UNLIKELY(NULL == context)) {
        return NULL;
    }

    context->newcomm       = newcomm;
    context->comm          = comm;
    context->bridgecomm    = bridgecomm;
    context->pml_tag       = 0;

    /* Determine which implementation of allreduce we have to use
     * for the current mode. */
    switch (mode) {
    case OMPI_COMM_CID_INTRA:
        context->allreduce_fn = ompi_comm_allreduce_intra_nb;
        break;
    case OMPI_COMM_CID_INTER:
        context->allreduce_fn = ompi_comm_allreduce_inter_nb;
        break;
    case OMPI_COMM_CID_GROUP:
    case OMPI_COMM_CID_GROUP_NEW:
        context->allreduce_fn = ompi_comm_allreduce_group_nb;
        context->pml_tag = ((int *) arg0)[0];
        break;
    case OMPI_COMM_CID_INTRA_PMIX:
        context->allreduce_fn = ompi_comm_allreduce_intra_pmix_nb;
        context->local_leader = ((int *) arg0)[0];
        if (arg1) {
            context->port_string = strdup ((char *) arg1);
        }
        context->pmix_tag = strdup ((char *) pmix_tag);
        break;
    case OMPI_COMM_CID_INTRA_BRIDGE:
        context->allreduce_fn = ompi_comm_allreduce_intra_bridge_nb;
        context->local_leader = ((int *) arg0)[0];
        context->remote_leader = ((int *) arg1)[0];
        break;
#if OPAL_ENABLE_FT_MPI
    case OMPI_COMM_CID_INTRA_FT:
        context->allreduce_fn = ompi_comm_ft_allreduce_intra_nb;
        break;
    case OMPI_COMM_CID_INTER_FT:
        context->allreduce_fn = ompi_comm_ft_allreduce_inter_nb;
        break;
    case OMPI_COMM_CID_INTRA_PMIX_FT:
        context->allreduce_fn = ompi_comm_ft_allreduce_intra_pmix_nb;
        break;
#endif /* OPAL_ENABLE_FT_MPI */
    default:
        OBJ_RELEASE(context);
        return NULL;
    }

    context->send_first = send_first;
    context->iter = 0;
    context->ok = 1;

    return context;
}

static ompi_comm_allreduce_context_t *ompi_comm_allreduce_context_alloc (int *inbuf, int *outbuf,
                                                                         int count, struct ompi_op_t *op,
                                                                         ompi_comm_cid_context_t *cid_context)
{
    ompi_comm_allreduce_context_t *context;

    context = OBJ_NEW(ompi_comm_allreduce_context_t);
    if (OPAL_UNLIKELY(NULL == context)) {
        return NULL;
    }

    context->inbuf = inbuf;
    context->outbuf = outbuf;
    context->count = count;
    context->op = op;
    context->cid_context = cid_context;

    return context;
}

/* find the next available local cid and start an allreduce */
static int ompi_comm_allreduce_getnextcid (ompi_comm_request_t *request);
/* verify that the maximum cid is locally available and start an allreduce */
static int ompi_comm_checkcid (ompi_comm_request_t *request);
/* verify that the cid was available globally */
static int ompi_comm_nextcid_check_flag (ompi_comm_request_t *request);

static volatile int64_t ompi_comm_cid_lowest_id = INT64_MAX;
#if OPAL_ENABLE_FT_MPI
static int ompi_comm_cid_epoch = INT_MAX;
#endif /* OPAL_ENABLE_FT_MPI */

static int ompi_comm_ext_cid_new_block (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                        ompi_comm_extended_cid_block_t *new_block,
                                        const void *arg0, const void *arg1, bool send_first, int mode,
                                        ompi_request_t **req)
{
    pmix_info_t pinfo, *results = NULL;
    size_t nresults;
    opal_process_name_t *name_array = NULL;
    char *tag = NULL;
    size_t proc_count;
    size_t cid_base;
    int rc, leader_rank;
    int ret = OMPI_SUCCESS;
    pmix_proc_t *procs = NULL;

    rc = ompi_group_to_proc_name_array (newcomm->c_local_group, &name_array, &proc_count);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    switch (mode) {
    case OMPI_COMM_CID_GROUP_NEW:
        tag = (char *) arg0;
        break;
    case OMPI_COMM_CID_GROUP:
        ompi_group_translate_ranks (newcomm->c_local_group, 1, &(int){0},
                                    comm->c_local_group, &leader_rank);

        tag = ompi_comm_extended_cid_get_unique_tag (&comm->c_contextidb, *((int *) arg0), leader_rank);
        break;
    case OMPI_COMM_CID_INTRA:
        tag = ompi_comm_extended_cid_get_unique_tag (&comm->c_contextidb, -1, 0);
        break;
    }

    PMIX_INFO_LOAD(&pinfo, PMIX_GROUP_ASSIGN_CONTEXT_ID, NULL, PMIX_BOOL);

    PMIX_PROC_CREATE(procs, proc_count);
    for (size_t i = 0 ; i < proc_count; ++i) {
        OPAL_PMIX_CONVERT_NAME(&procs[i],&name_array[i]);
    }

    rc = PMIx_Group_construct(tag, procs, proc_count, &pinfo, 1, &results, &nresults);
    PMIX_INFO_DESTRUCT(&pinfo);
    if(PMIX_SUCCESS != rc) {
       char msg_string[1024];
        switch (rc) {
        case PMIX_ERR_UNREACH:
            sprintf(msg_string,"PMIx server unreachable");
            opal_show_help("help-comm.txt",
                           "MPI function not supported",
                           true,
                           "MPI_Comm_create_from_group/MPI_Intercomm_create_from_groups",
                           msg_string);

            ret = MPI_ERR_UNSUPPORTED_OPERATION;
            break;
        case PMIX_ERR_NOT_SUPPORTED:
            sprintf(msg_string,"PMIx server does not support PMIx Group operations");
            opal_show_help("help-comm.txt",
                           "MPI function not supported",
                           true,
                           "MPI_Comm_from_group/MPI_Intercomm_from_groups",
                           msg_string);
            ret = MPI_ERR_UNSUPPORTED_OPERATION;
            break;
        default:
            ret = opal_pmix_convert_status(rc);
            break;
        } 
        goto fn_exit;
    }

    for (size_t i=0; i<nresults; i++) {
        if (PMIX_CHECK_KEY(&results[i], PMIX_GROUP_CONTEXT_ID)) {
            PMIX_VALUE_GET_NUMBER(rc, &results[i].value, cid_base, size_t);
            if(PMIX_SUCCESS != rc) {
                ret = opal_pmix_convert_status(rc);
                goto fn_exit;
            }
            break;
        }
    }

    rc = PMIx_Group_destruct (tag, NULL, 0);
    if(PMIX_SUCCESS != rc) {
        ret = opal_pmix_convert_status(rc);
        goto fn_exit;
    }

    ompi_comm_extended_cid_block_initialize (new_block, cid_base, 0, 0);

fn_exit:
    if (NULL != results) {
        PMIX_INFO_FREE(results, nresults);
        results = NULL;
    }

    if(NULL != procs) {
        PMIX_PROC_FREE(procs, proc_count);
        procs = NULL;
    }

    if(NULL != name_array) {
        free (name_array);
        name_array = NULL;
    }

    return ret;
}

static int ompi_comm_nextcid_ext_nb (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                     ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                                     bool send_first, int mode, ompi_request_t **req)
{
    ompi_comm_extended_cid_block_t *block;
    bool is_new_block = false;
    int rc;

    /*
     * sanity check and coverity pacifier
     */
    if (!(OMPI_COMM_CID_GROUP == mode || OMPI_COMM_CID_GROUP_NEW == mode) && (NULL == comm)) {
        return OMPI_ERROR;
    }

    if (OMPI_COMM_CID_GROUP == mode || OMPI_COMM_CID_GROUP_NEW == mode) {
        /* new block belongs to the new communicator */
        block = &newcomm->c_contextidb;
    } else {
        block = &comm->c_contextidb;
    }

    if (NULL == arg1) {
        if (OMPI_COMM_CID_GROUP == mode || OMPI_COMM_CID_GROUP_NEW == mode ||
            !ompi_comm_extended_cid_block_available (&comm->c_contextidb)) {
            /* need a new block. it will be either assigned the the new communicator (MPI_Comm_create*_group)
             * or the parent (which has no more CIDs in its block) */
            rc = ompi_comm_ext_cid_new_block (newcomm, comm, block, arg0, arg1, send_first, mode, req);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
                return rc;
            }

            is_new_block = true;
        }
    } else {
        /* got a block already */
        *block = *((ompi_comm_extended_cid_block_t *) arg1);
        is_new_block = true;
    }

    if (block != &newcomm->c_contextidb) {
        (void) ompi_comm_extended_cid_block_new (block, &newcomm->c_contextidb, is_new_block);
    }

    for (unsigned int i = ompi_mpi_communicators.lowest_free ; i < mca_pml.pml_max_contextid ; ++i) {
        bool flag = opal_pointer_array_test_and_set_item (&ompi_mpi_communicators, i, newcomm);
        if (true == flag) {
            newcomm->c_index = i;
            break;
        }
    }

    newcomm->c_contextid = newcomm->c_contextidb.block_cid;

    opal_hash_table_set_value_ptr (&ompi_comm_hash, &newcomm->c_contextid,
                                   sizeof (newcomm->c_contextid), (void *) newcomm);
    *req = &ompi_request_empty;
    /* nothing more to do here */
    return OMPI_SUCCESS;
}

int ompi_comm_nextcid_nb (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                          ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                          bool send_first, int mode, ompi_request_t **req)
{
    ompi_comm_cid_context_t *context;
    ompi_comm_request_t *request;

    if (mca_pml_base_supports_extended_cid() && NULL == comm) {
        return ompi_comm_nextcid_ext_nb (newcomm, comm, bridgecomm, arg0, arg1, send_first, mode, req);
    }

    /* old CID algorighm */

    /* if we got here and comm is NULL then that means the app is  invoking MPI-4 Sessions or later
       functions but the pml does not support these functions so return not supported */
    if (NULL == comm) {
       char msg_string[1024];
       sprintf(msg_string,"The PML being used - %s - does not support MPI sessions related features", 
               mca_pml_base_selected_component.pmlm_version.mca_component_name);
       opal_show_help("help-comm.txt",
                      "MPI function not supported",
                      true,
                      "MPI_Comm_from_group/MPI_Intercomm_from_groups",
                      msg_string);

        return MPI_ERR_UNSUPPORTED_OPERATION;
    }

    newcomm->c_flags |= OMPI_COMM_GLOBAL_INDEX;

    context = mca_comm_cid_context_alloc (newcomm, comm, bridgecomm, arg0, arg1,
                                          "nextcid", send_first, mode);
    if (NULL == context) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context->start = ompi_mpi_communicators.lowest_free;

    request = ompi_comm_request_get ();
    if (NULL == request) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->context = &context->super;
    request->super.req_mpi_object.comm = context->comm;

    ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);
    ompi_comm_request_start (request);

    *req = &request->super;


    return OMPI_SUCCESS;
}

int ompi_comm_nextcid (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                       ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                       bool send_first, int mode)
{
    ompi_request_t *req;
    int rc;

    rc = ompi_comm_nextcid_nb (newcomm, comm, bridgecomm, arg0, arg1, send_first, mode, &req);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    if (&ompi_request_empty != req) {
        ompi_request_wait_completion (req);
        rc = req->req_status.MPI_ERROR;
        ompi_comm_request_return ((ompi_comm_request_t *) req);
    }

    return rc;
}

static int ompi_comm_allreduce_getnextcid (ompi_comm_request_t *request)
{
    ompi_comm_cid_context_t *context = (ompi_comm_cid_context_t *) request->context;
    int64_t my_id = ((int64_t) ompi_comm_get_local_cid (context->comm) << 32 | context->pml_tag);
    ompi_request_t *subreq;
    bool flag = false;
    int ret = OMPI_SUCCESS;
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
#if OPAL_ENABLE_FT_MPI
        context->nextcid_epoch = ompi_comm_cid_epoch - 1;
        if (0 == context->nextcid_epoch) {
            /* out of epochs, force an error by setting nextlocalcid */
            context->nextlocal_cid = mca_pml.pml_max_contextid;
        }
#endif /* OPAL_ENABLE_FT_MPI */
    } else {
        context->nextlocal_cid = 0;
#if OPAL_ENABLE_FT_MPI
        context->nextcid_epoch = INT_MAX;
#endif /* OPAL_ENABLE_FT_MPI */
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
    ompi_comm_cid_context_t *context = (ompi_comm_cid_context_t *) request->context;
    ompi_request_t *subreq;
    int ret;
    int participate = (context->newcomm->c_local_group->grp_my_rank != MPI_UNDEFINED);

    if (OMPI_SUCCESS != request->super.req_status.MPI_ERROR) {
        if (participate) {
            opal_pointer_array_set_item(&ompi_mpi_communicators, context->nextlocal_cid, NULL);
        }
        return request->super.req_status.MPI_ERROR;
    }

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

#if OPAL_ENABLE_FT_MPI
    if (context->flag) {
        context->flag = context->nextcid_epoch;
    }
#endif /* OPAL_ENABLE_FT_MPI */

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
    ompi_comm_cid_context_t *context = (ompi_comm_cid_context_t *) request->context;
    int participate = (context->newcomm->c_local_group->grp_my_rank != MPI_UNDEFINED);

    if (OMPI_SUCCESS != request->super.req_status.MPI_ERROR) {
        if (participate) {
            opal_pointer_array_set_item(&ompi_mpi_communicators, context->nextcid, NULL);
        }
        return request->super.req_status.MPI_ERROR;
    }

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_nextcid_check_flag, NULL, 0);
    }

    if (0 != context->rflag) {
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
#if OPAL_ENABLE_FT_MPI
        context->newcomm->c_epoch = INT_MAX - context->rflag; /* reorder for simpler debugging */
        ompi_comm_cid_epoch -= 1; /* protected by the cid_lock */
#endif /* OPAL_ENABLE_FT_MPI */
        context->newcomm->c_index = context->nextcid;

        /* to simplify coding always set the global CID even if it isn't used by the
         * active PML */
        context->newcomm->c_contextid.cid_base = 0;
        context->newcomm->c_contextid.cid_sub.u64 = context->nextcid;
        opal_pointer_array_set_item (&ompi_mpi_communicators, context->nextcid, context->newcomm);

        /* unlock the cid generator */
        ompi_comm_cid_lowest_id = INT64_MAX;
        OPAL_THREAD_UNLOCK(&ompi_cid_lock);

        /* done! */
        return OMPI_SUCCESS;
    }

    if (participate && (0 != context->flag)) {
        /* we could use this cid, but other don't agree */
        opal_pointer_array_set_item (&ompi_mpi_communicators, context->nextcid, NULL);
        context->start = context->nextcid + 1; /* that's where we can start the next round */
    }

    ++context->iter;

    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    /* try again */
    return ompi_comm_allreduce_getnextcid (request);
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/* This routine serves two purposes:
 * - the allreduce acts as a kind of Barrier,
 *   which avoids, that we have incoming fragments
 *   on the new communicator before everybody has set
 *   up the comm structure.
 * - some components (e.g. the collective MagPIe component
 *   might want to generate new communicators and communicate
 *   using the new comm. Thus, it can just be called after
 *   the 'barrier'.
 *
 * The reason that this routine is in comm_cid and not in
 * comm.c is, that this file contains the allreduce implementations
 * which are required, and thus we avoid having duplicate code...
 */

/* Non-blocking version of ompi_comm_activate */
static int ompi_comm_activate_nb_complete (ompi_comm_request_t *request);

static int ompi_comm_activate_complete (ompi_communicator_t **newcomm, ompi_communicator_t *comm)
{
    int ret;

    /**
     * Check to see if this process is in the new communicator.
     *
     * Specifically, this function is invoked by all processes in the
     * old communicator, regardless of whether they are in the new
     * communicator or not.  This is because it is far simpler to use
     * MPI collective functions on the old communicator to determine
     * some data for the new communicator (e.g., remote_leader) than
     * to kludge up our own pseudo-collective routines over just the
     * processes in the new communicator.  Hence, *all* processes in
     * the old communicator need to invoke this function.
     *
     * That being said, only processes in the new communicator need to
     * select a coll module for the new communicator.  More
     * specifically, processes who are not in the new communicator
     * should *not* select a coll module -- for example,
     * ompi_comm_rank(newcomm) returns MPI_UNDEFINED for processes who
     * are not in the new communicator.  This can cause errors in the
     * selection / initialization of a coll module.  Plus, it's
     * wasteful -- processes in the new communicator will end up
     * freeing the new communicator anyway, so we might as well leave
     * the coll selection as NULL (the coll base comm unselect code
     * handles that case properly).
     */
    if (MPI_UNDEFINED == (*newcomm)->c_local_group->grp_my_rank) {
        return OMPI_SUCCESS;
    }

    /* Let the collectives components fight over who will do
       collective on this new comm.  */
    if (OMPI_SUCCESS != (ret = mca_coll_base_comm_select(*newcomm))) {
        OBJ_RELEASE(*newcomm);
        *newcomm = MPI_COMM_NULL;
        return ret;
    }

    /* For an inter communicator, we have to deal with the potential
     * problem of what is happening if the local_comm that we created
     * has a lower CID than the parent comm. This is not a problem
     * as long as the user calls MPI_Comm_free on the inter communicator.
     * However, if the communicators are not freed by the user but released
     * by Open MPI in MPI_Finalize, we walk through the list of still available
     * communicators and free them one by one. Thus, local_comm is freed before
     * the actual inter-communicator. However, the local_comm pointer in the
     * inter communicator will still contain the 'previous' address of the local_comm
     * and thus this will lead to a segmentation violation. In order to prevent
     * that from happening, we increase the reference counter local_comm
     * by one if its CID is lower than the parent. We cannot increase however
     *  its reference counter if the CID of local_comm is larger than
     * the CID of the inter communicators, since a regular MPI_Comm_free would
     * leave in that the case the local_comm hanging around and thus we would not
     * recycle CID's properly, which was the reason and the cause for this trouble.
     */
    if (OMPI_COMM_IS_INTER(*newcomm)) {
        if (OMPI_COMM_CID_IS_LOWER(*newcomm, comm)) {
            OMPI_COMM_SET_EXTRA_RETAIN (*newcomm);
            OBJ_RETAIN (*newcomm);
        }
    }

    /* done */
    return OMPI_SUCCESS;
}

int ompi_comm_activate_nb (ompi_communicator_t **newcomm, ompi_communicator_t *comm,
                           ompi_communicator_t *bridgecomm, const void *arg0,
                           const void *arg1, bool send_first, int mode, ompi_request_t **req)
{
    ompi_comm_cid_context_t *context;
    ompi_comm_request_t *request;
    ompi_request_t *subreq;
    int ret = 0;

    /* the caller should not pass NULL for comm (it may be the same as *newcomm) */
    assert (NULL != comm);
    context = mca_comm_cid_context_alloc (*newcomm, comm, bridgecomm, arg0, arg1, "activate",
                                          send_first, mode);
    if (NULL == context) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* keep track of the pointer so it can be set to MPI_COMM_NULL on failure */
    context->newcommp = newcomm;

    request = ompi_comm_request_get ();
    if (NULL == request) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->context = &context->super;

    if (MPI_UNDEFINED != (*newcomm)->c_local_group->grp_my_rank) {
        /* Initialize the PML stuff in the newcomm  */
        if ( OMPI_SUCCESS != (ret = MCA_PML_CALL(add_comm(*newcomm))) ) {
            OBJ_RELEASE(*newcomm);
            OBJ_RELEASE(context);
            *newcomm = MPI_COMM_NULL;
            return ret;
        }
        OMPI_COMM_SET_PML_ADDED(*newcomm);
    }

    /* Step 1: the barrier, after which it is allowed to
     * send messages over the new communicator
     */
    ret = context->allreduce_fn (&context->ok, &context->ok, 1, MPI_MIN, context,
                                 &subreq);
    if (OMPI_SUCCESS != ret) {
        ompi_comm_request_return (request);
        return ret;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_activate_nb_complete, &subreq, 1);
    ompi_comm_request_start (request);

    *req = &request->super;

    return ret;
}

int ompi_comm_activate (ompi_communicator_t **newcomm, ompi_communicator_t *comm,
                        ompi_communicator_t *bridgecomm, const void *arg0,
                        const void *arg1, bool send_first, int mode)
{
    ompi_request_t *req;
    int rc;

    rc = ompi_comm_activate_nb (newcomm, comm, bridgecomm, arg0, arg1, send_first, mode, &req);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    if (&ompi_request_empty != req) {
        ompi_request_wait_completion (req);
        rc = req->req_status.MPI_ERROR;
        ompi_comm_request_return ((ompi_comm_request_t *) req);
    }

    return rc;
}

static int ompi_comm_activate_nb_complete (ompi_comm_request_t *request)
{
    ompi_comm_cid_context_t *context = (ompi_comm_cid_context_t *) request->context;
    return ompi_comm_activate_complete (context->newcommp, context->comm);
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
static int ompi_comm_allreduce_intra_nb (int *inbuf, int *outbuf, int count, struct ompi_op_t *op,
                                         ompi_comm_cid_context_t *context, ompi_request_t **req)
{
    ompi_communicator_t *comm = context->comm;

    return comm->c_coll->coll_iallreduce (inbuf, outbuf, count, MPI_INT, op, comm,
                                         req, comm->c_coll->coll_iallreduce_module);
}

/* Non-blocking version of ompi_comm_allreduce_inter */
static int ompi_comm_allreduce_inter_leader_exchange (ompi_comm_request_t *request);
static int ompi_comm_allreduce_inter_leader_reduce (ompi_comm_request_t *request);
static int ompi_comm_allreduce_inter_bcast (ompi_comm_request_t *request);

static int ompi_comm_allreduce_inter_nb (int *inbuf, int *outbuf,
                                         int count, struct ompi_op_t *op,
                                         ompi_comm_cid_context_t *cid_context,
                                         ompi_request_t **req)
{
    ompi_communicator_t *intercomm = cid_context->comm;
    ompi_comm_allreduce_context_t *context;
    ompi_comm_request_t *request;
    ompi_request_t *subreq;
    int local_rank, rc;

    if (!OMPI_COMM_IS_INTER (cid_context->comm)) {
        return MPI_ERR_COMM;
    }

    request = ompi_comm_request_get ();
    if (OPAL_UNLIKELY(NULL == request)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    context = ompi_comm_allreduce_context_alloc (inbuf, outbuf, count, op, cid_context);
    if (OPAL_UNLIKELY(NULL == context)) {
        ompi_comm_request_return (request);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->context = &context->super;

    /* Allocate temporary arrays */
    local_rank = ompi_comm_rank (intercomm);

    if (0 == local_rank) {
        context->tmpbuf  = (int *) calloc (count, sizeof(int));
        if (OPAL_UNLIKELY (NULL == context->tmpbuf)) {
            ompi_comm_request_return (request);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    /* Execute the inter-allreduce: the result from the local will be in the buffer of the remote group
     * and vise-versa. */
    rc = intercomm->c_local_comm->c_coll->coll_ireduce (inbuf, context->tmpbuf, count, MPI_INT, op, 0,
                                                       intercomm->c_local_comm, &subreq,
                                                       intercomm->c_local_comm->c_coll->coll_ireduce_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        ompi_comm_request_return (request);
        return rc;
    }

    if (0 == local_rank) {
        ompi_comm_request_schedule_append (request, ompi_comm_allreduce_inter_leader_exchange, &subreq, 1);
    } else {
        ompi_comm_request_schedule_append (request, ompi_comm_allreduce_inter_bcast, &subreq, 1);
    }

    ompi_comm_request_start (request);
    *req = &request->super;

    return OMPI_SUCCESS;
}


static int ompi_comm_allreduce_inter_leader_exchange (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;
    ompi_communicator_t *intercomm = context->cid_context->comm;
    ompi_request_t *subreqs[2];
    int rc;

    /* local leader exchange their data and determine the overall result
       for both groups */
    rc = MCA_PML_CALL(irecv (context->outbuf, context->count, MPI_INT, 0, OMPI_COMM_ALLREDUCE_TAG,
                             intercomm, subreqs));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = MCA_PML_CALL(isend (context->tmpbuf, context->count, MPI_INT, 0, OMPI_COMM_ALLREDUCE_TAG,
                             MCA_PML_BASE_SEND_STANDARD, intercomm, subreqs + 1));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_inter_leader_reduce, subreqs, 2);
}

static int ompi_comm_allreduce_inter_leader_reduce (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;

    ompi_op_reduce (context->op, context->tmpbuf, context->outbuf, context->count, MPI_INT);

    return ompi_comm_allreduce_inter_bcast (request);
}


static int ompi_comm_allreduce_inter_bcast (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;
    ompi_communicator_t *comm = context->cid_context->comm->c_local_comm;
    ompi_request_t *subreq;
    int rc;

    /* both roots have the same result. broadcast to the local group */
    rc = comm->c_coll->coll_ibcast (context->outbuf, context->count, MPI_INT, 0, comm,
                                   &subreq, comm->c_coll->coll_ibcast_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    return ompi_comm_request_schedule_append (request, NULL, &subreq, 1);
}

static int ompi_comm_allreduce_bridged_schedule_bcast (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;
    ompi_communicator_t *comm = context->cid_context->comm;
    ompi_request_t *subreq;
    int rc;

    rc = comm->c_coll->coll_ibcast (context->outbuf, context->count, MPI_INT,
                                   context->cid_context->local_leader, comm,
                                   &subreq, comm->c_coll->coll_ibcast_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    return ompi_comm_request_schedule_append (request, NULL, &subreq, 1);
}

static int ompi_comm_allreduce_bridged_xchng_complete (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;

    /* step 3: reduce leader data */
    ompi_op_reduce (context->op, context->tmpbuf, context->outbuf, context->count, MPI_INT);

    /* schedule the broadcast to local peers */
    return ompi_comm_allreduce_bridged_schedule_bcast (request);
}

static int ompi_comm_allreduce_bridged_reduce_complete (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;
    ompi_communicator_t *bridgecomm = context->cid_context->bridgecomm;
    ompi_request_t *subreq[2];
    int rc;

    /* step 2: leader exchange */
    rc = MCA_PML_CALL(irecv (context->outbuf, context->count, MPI_INT, context->cid_context->remote_leader,
                             OMPI_COMM_ALLREDUCE_TAG, bridgecomm, subreq + 1));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = MCA_PML_CALL(isend (context->tmpbuf, context->count, MPI_INT, context->cid_context->remote_leader,
                             OMPI_COMM_ALLREDUCE_TAG, MCA_PML_BASE_SEND_STANDARD, bridgecomm,
                             subreq));
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_bridged_xchng_complete, subreq, 2);
}

static int ompi_comm_allreduce_intra_bridge_nb (int *inbuf, int *outbuf,
                                                int count, struct ompi_op_t *op,
                                                ompi_comm_cid_context_t *cid_context,
                                                ompi_request_t **req)
{
    ompi_communicator_t *comm = cid_context->comm;
    ompi_comm_allreduce_context_t *context;
    int local_rank = ompi_comm_rank (comm);
    ompi_comm_request_t *request;
    ompi_request_t *subreq;
    int rc;

    context = ompi_comm_allreduce_context_alloc (inbuf, outbuf, count, op, cid_context);
    if (OPAL_UNLIKELY(NULL == context)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (local_rank == cid_context->local_leader) {
        context->tmpbuf = (int *) calloc (count, sizeof (int));
        if (OPAL_UNLIKELY(NULL == context->tmpbuf)) {
            OBJ_RELEASE(context);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    request = ompi_comm_request_get ();
    if (OPAL_UNLIKELY(NULL == request)) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->context = &context->super;

    if (cid_context->local_leader == local_rank) {
        memcpy (context->tmpbuf, inbuf, count * sizeof (int));
    }

    /* step 1: reduce to the local leader */
    rc = comm->c_coll->coll_ireduce (inbuf, context->tmpbuf, count, MPI_INT, op,
                                    cid_context->local_leader, comm, &subreq,
                                    comm->c_coll->coll_ireduce_module);
    if ( OMPI_SUCCESS != rc ) {
        ompi_comm_request_return (request);
        return rc;
    }

    if (cid_context->local_leader == local_rank) {
        rc = ompi_comm_request_schedule_append (request, ompi_comm_allreduce_bridged_reduce_complete,
                                                &subreq, 1);
    } else {
        /* go ahead and schedule the broadcast */
        ompi_comm_request_schedule_append (request, NULL, &subreq, 1);

        rc = ompi_comm_allreduce_bridged_schedule_bcast (request);
    }

    if (OMPI_SUCCESS != rc) {
        ompi_comm_request_return (request);
        return rc;
    }

    ompi_comm_request_start (request);

    *req = &request->super;

    return OMPI_SUCCESS;
}

static int ompi_comm_allreduce_pmix_reduce_complete (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;
    ompi_comm_cid_context_t *cid_context = context->cid_context;
    int32_t size_count = context->count;
    pmix_info_t info;
    pmix_pdata_t pdat;
    pmix_data_buffer_t sbuf;
    int rc;
    int bytes_written;
    char *key;
    const int output_id = 0;
    const int verbosity_level = 1;

    PMIX_DATA_BUFFER_CONSTRUCT(&sbuf);

    rc = PMIx_Data_pack(NULL, &sbuf, context->tmpbuf, (int32_t)context->count, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_DATA_BUFFER_DESTRUCT(&sbuf);
        opal_output_verbose (verbosity_level, output_id, "pack failed: %s\n", PMIx_Error_string(rc));
        return opal_pmix_convert_status(rc);
    }

    PMIX_PDATA_CONSTRUCT(&pdat);
    PMIX_INFO_CONSTRUCT(&info);
    info.value.type = PMIX_BYTE_OBJECT;

    PMIX_DATA_BUFFER_UNLOAD(&sbuf, info.value.data.bo.bytes, info.value.data.bo.size);
    PMIX_DATA_BUFFER_DESTRUCT(&sbuf);

    bytes_written = opal_asprintf(&key,
                             cid_context->send_first ? "%s:%s:send:%d"
                                                     : "%s:%s:recv:%d",
                             cid_context->port_string,
                             cid_context->pmix_tag,
                             cid_context->iter);
    PMIX_LOAD_KEY(info.key, key);
    free(key);
    if (bytes_written == -1) {
        opal_output_verbose (verbosity_level, output_id, "writing info.key failed\n");
    } else {
        bytes_written = opal_asprintf(&key,
                                 cid_context->send_first ? "%s:%s:recv:%d"
                                                         : "%s:%s:send:%d",
                                 cid_context->port_string,
                                 cid_context->pmix_tag,
                                 cid_context->iter);
        PMIX_LOAD_KEY((char*)pdat.key, key);
        free(key);
        if (bytes_written == -1) {
            opal_output_verbose (verbosity_level, output_id, "writing pdat.value.key failed\n");
        }
    }

    if (bytes_written == -1) {
        // write with separate calls,
        // just in case the args are the cause of failure
        opal_output_verbose (verbosity_level, output_id, "send first: %d\n", cid_context->send_first);
        opal_output_verbose (verbosity_level, output_id, "port string: %s\n", cid_context->port_string);
        opal_output_verbose (verbosity_level, output_id, "pmix tag: %s\n", cid_context->pmix_tag);
        opal_output_verbose (verbosity_level, output_id, "iter: %d\n", cid_context->iter);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* this macro is not actually non-blocking. if a non-blocking version becomes available this function
     * needs to be reworked to take advantage of it. */
    rc = opal_pmix_base_exchange(&info, &pdat, 600);  // give them 10 minutes
    PMIX_INFO_DESTRUCT(&info);
    if (OPAL_SUCCESS != rc) {
        PMIX_PDATA_DESTRUCT(&pdat);
        return rc;
    }
    if (PMIX_BYTE_OBJECT != pdat.value.type) {
        PMIX_PDATA_DESTRUCT(&pdat);
        return OPAL_ERR_TYPE_MISMATCH;
    }

    PMIX_DATA_BUFFER_CONSTRUCT(&sbuf);
    PMIX_DATA_BUFFER_LOAD(&sbuf, pdat.value.data.bo.bytes, pdat.value.data.bo.size);

    rc = PMIx_Data_unpack(NULL, &sbuf, context->outbuf, &size_count, PMIX_INT);
    PMIX_DATA_BUFFER_DESTRUCT(&sbuf);
    if (OPAL_UNLIKELY(PMIX_SUCCESS != rc)) {
        return opal_pmix_convert_status(rc);
    }

    ompi_op_reduce (context->op, context->tmpbuf, context->outbuf, size_count, MPI_INT);

    return ompi_comm_allreduce_bridged_schedule_bcast (request);
}

static int ompi_comm_allreduce_intra_pmix_nb (int *inbuf, int *outbuf,
                                              int count, struct ompi_op_t *op,
                                              ompi_comm_cid_context_t *cid_context,
                                              ompi_request_t **req)
{
    ompi_communicator_t *comm = cid_context->comm;
    ompi_comm_allreduce_context_t *context;
    int local_rank = ompi_comm_rank (comm);
    ompi_comm_request_t *request;
    ompi_request_t *subreq;
    int rc;

    context = ompi_comm_allreduce_context_alloc (inbuf, outbuf, count, op, cid_context);
    if (OPAL_UNLIKELY(NULL == context)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (cid_context->local_leader == local_rank) {
        context->tmpbuf = (int *) calloc (count, sizeof(int));
        if (OPAL_UNLIKELY(NULL == context->tmpbuf)) {
            OBJ_RELEASE(context);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    request = ompi_comm_request_get ();
    if (NULL == request) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->context = &context->super;

    /* comm is an intra-communicator */
    rc = comm->c_coll->coll_ireduce (inbuf, context->tmpbuf, count, MPI_INT, op,
                                    cid_context->local_leader, comm,
                                    &subreq, comm->c_coll->coll_ireduce_module);
    if ( OMPI_SUCCESS != rc ) {
        ompi_comm_request_return (request);
        return rc;
    }

    if (cid_context->local_leader == local_rank) {
        rc = ompi_comm_request_schedule_append (request, ompi_comm_allreduce_pmix_reduce_complete,
                                                &subreq, 1);
    } else {
        /* go ahead and schedule the broadcast */
        rc = ompi_comm_request_schedule_append (request, NULL, &subreq, 1);

        rc = ompi_comm_allreduce_bridged_schedule_bcast (request);
    }

    if (OMPI_SUCCESS != rc) {
        ompi_comm_request_return (request);
        return rc;
    }

    ompi_comm_request_start (request);
    *req = (ompi_request_t *) request;

    /* use the same function as bridged to schedule the broadcast */
    return OMPI_SUCCESS;
}

static int ompi_comm_allreduce_group_broadcast (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;
    ompi_comm_cid_context_t *cid_context = context->cid_context;
    ompi_request_t *subreq[2];
    int subreq_count = 0;
    int rc;

    for (int i = 0 ; i < 2 ; ++i) {
        if (MPI_PROC_NULL != context->peers_comm[i + 1]) {
            rc = MCA_PML_CALL(isend(context->outbuf, context->count, MPI_INT, context->peers_comm[i+1],
                                    cid_context->pml_tag, MCA_PML_BASE_SEND_STANDARD,
                                    cid_context->comm, subreq + subreq_count++));
            if (OMPI_SUCCESS != rc) {
                return rc;
            }
        }
    }

    return ompi_comm_request_schedule_append (request, NULL, subreq, subreq_count);
}

static int ompi_comm_allreduce_group_recv_complete (ompi_comm_request_t *request)
{
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t *) request->context;
    ompi_comm_cid_context_t *cid_context = context->cid_context;
    int *tmp = context->tmpbuf;
    ompi_request_t *subreq[2];
    int rc;

    for (int i = 0 ; i < 2 ; ++i) {
        if (MPI_PROC_NULL != context->peers_comm[i + 1]) {
            ompi_op_reduce (context->op, tmp, context->outbuf, context->count, MPI_INT);
            tmp += context->count;
        }
    }

    if (MPI_PROC_NULL != context->peers_comm[0]) {
        /* interior node */
        rc = MCA_PML_CALL(isend(context->outbuf, context->count, MPI_INT, context->peers_comm[0],
                                cid_context->pml_tag, MCA_PML_BASE_SEND_STANDARD,
                                cid_context->comm, subreq));
        if (OMPI_SUCCESS != rc) {
            return rc;
        }

        rc = MCA_PML_CALL(irecv(context->outbuf, context->count, MPI_INT, context->peers_comm[0],
                                cid_context->pml_tag, cid_context->comm, subreq + 1));
        if (OMPI_SUCCESS != rc) {
            return rc;
        }

        return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_group_broadcast, subreq, 2);
    }

    /* root */
    return ompi_comm_allreduce_group_broadcast (request);
}

static int ompi_comm_allreduce_group_nb (int *inbuf, int *outbuf, int count,
                                         struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                         ompi_request_t **req)
{
    ompi_group_t *group = cid_context->newcomm->c_local_group;
    const int group_size = ompi_group_size (group);
    const int group_rank = ompi_group_rank (group);
    ompi_communicator_t *comm = cid_context->comm;
    int peers_group[3], *tmp, subreq_count = 0;
    ompi_comm_allreduce_context_t *context;
    ompi_comm_request_t *request;
    ompi_request_t *subreq[3];

    context = ompi_comm_allreduce_context_alloc (inbuf, outbuf, count, op, cid_context);
    if (NULL == context) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    tmp = context->tmpbuf = calloc (sizeof (int), count * 3);
    if (NULL == context->tmpbuf) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request = ompi_comm_request_get ();
    if (NULL == request) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->context = &context->super;

    /* basic recursive doubling allreduce on the group */
    peers_group[0] = group_rank ? ((group_rank - 1) >> 1) : MPI_PROC_NULL;
    peers_group[1] = (group_rank * 2 + 1) < group_size ? group_rank * 2 + 1: MPI_PROC_NULL;
    peers_group[2] = (group_rank * 2 + 2) < group_size ? group_rank * 2 + 2 : MPI_PROC_NULL;

    /* translate the ranks into the ranks of the parent communicator */
    ompi_group_translate_ranks (group, 3, peers_group, comm->c_local_group, context->peers_comm);

    /* reduce */
    memmove (outbuf, inbuf, sizeof (int) * count);

    for (int i = 0 ; i < 2 ; ++i) {
        if (MPI_PROC_NULL != context->peers_comm[i + 1]) {
            int rc = MCA_PML_CALL(irecv(tmp, count, MPI_INT, context->peers_comm[i + 1],
                                        cid_context->pml_tag, comm, subreq + subreq_count++));
            if (OMPI_SUCCESS != rc) {
                ompi_comm_request_return (request);
                return rc;
            }

            tmp += count;
        }
    }

    ompi_comm_request_schedule_append (request, ompi_comm_allreduce_group_recv_complete, subreq, subreq_count);

    ompi_comm_request_start (request);
    *req = &request->super;

    return OMPI_SUCCESS;
}

#if OPAL_ENABLE_FT_MPI

/**
 * Reduction operation using an agreement, to ensure that all processes
 *  agree on the same list.
 */
static int ompi_comm_ft_allreduce_agree_completion(ompi_comm_request_t* request) {
    int rc = request->super.req_status.MPI_ERROR;
    ompi_comm_allreduce_context_t *context = (ompi_comm_allreduce_context_t*) request->context;
    ompi_group_t **failed_group = (ompi_group_t**)&context->inbuf;

    /* Previous agreement found new failures, do another round */
    if(OPAL_UNLIKELY( MPI_ERR_PROC_FAILED == rc )) {
        ompi_communicator_t *comm = context->cid_context->comm;
        ompi_request_t *subreq;
        OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle, "ft_allreduce found a dead process during previous round; redo"));
        rc = comm->c_coll->coll_iagree(context->outbuf, context->count, &ompi_mpi_int.dt, context->op,
                                       failed_group, true,
                                       comm, &subreq, comm->c_coll->coll_iagree_module);
        if( OPAL_LIKELY(OMPI_SUCCESS == rc) ) {
            request->super.req_status.MPI_ERROR = MPI_SUCCESS;
            return ompi_comm_request_schedule_append(request, ompi_comm_ft_allreduce_agree_completion, &subreq, 1);
        }
    }
    OBJ_RELEASE(*failed_group);
    return rc;
}

static int ompi_comm_ft_allreduce_intra_nb(int *inbuf, int *outbuf, int count,
                                           struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                           ompi_request_t **req) {
    int rc;
    ompi_comm_allreduce_context_t *context;
    ompi_comm_request_t *request;
    ompi_request_t *subreq;
    ompi_communicator_t *comm = cid_context->comm;

    context = ompi_comm_allreduce_context_alloc(inbuf, outbuf, count, op, cid_context);
    if(OPAL_UNLIKELY( NULL == context )) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request = ompi_comm_request_get ();
    if(OPAL_UNLIKELY( NULL == request )) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    request->context = &context->super;
    request->super.req_mpi_object.comm = comm;

    /** Because the agreement operates "in place",
     *  one needs first to copy the inbuf into the outbuf
     */
    if( inbuf != outbuf ) {
        memcpy(outbuf, inbuf, count * sizeof(int));
    }

    /** Repurpose the inbuf to store the failed_group */
    ompi_group_t** failed_group = (ompi_group_t**) &context->inbuf;
    opal_mutex_lock(&ompi_group_afp_mutex);
    ompi_group_intersection(comm->c_remote_group, ompi_group_all_failed_procs, failed_group);
    opal_mutex_unlock(&ompi_group_afp_mutex);

    rc = comm->c_coll->coll_iagree(context->outbuf, context->count, &ompi_mpi_int.dt, context->op,
                                   failed_group, true,
                                   comm, &subreq, comm->c_coll->coll_iagree_module);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
        OBJ_RELEASE(*failed_group);
        ompi_comm_request_return(request);
        return rc;
    }

    ompi_comm_request_schedule_append (request, ompi_comm_ft_allreduce_agree_completion, &subreq, 1);
    ompi_comm_request_start (request);
    *req = &request->super;
    return OMPI_SUCCESS;
}

static int ompi_comm_ft_allreduce_inter_nb(int *inbuf, int *outbuf, int count,
                                           struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                           ompi_request_t **req) {
    return MPI_ERR_UNSUPPORTED_OPERATION;
}

static int ompi_comm_ft_allreduce_intra_pmix_nb(int *inbuf, int *outbuf, int count,
                                                struct ompi_op_t *op, ompi_comm_cid_context_t *cid_context,
                                                ompi_request_t **req) {
    //TODO: CID_INTRA_PMIX_FT needs an implementation, using the non-ft for now...
    return ompi_comm_allreduce_intra_pmix_nb(inbuf, outbuf, count, op, cid_context, req);
}

#endif /* OPAL_ENABLE_FT_MPI */

