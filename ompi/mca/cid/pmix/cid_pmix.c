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

#include "orte/runtime/orte_wait.h"

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

#include "cid_pmix.h"

static int cid_pmix_nextcid_nb (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                                bool send_first, int mode, ompi_request_t **req);

static int cid_pmix_nextcid (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                             ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                             bool send_first, int mode);

static int cid_pmix_release_nb (int cid, ompi_request_t **req);

static int cid_pmix_release (int cid);

static int cid_pmix_init (void);

ompi_cid_base_module_t ompi_cid_pmix_module = {
    .nextcid_nb = cid_pmix_nextcid_nb,
    .nextcid = cid_pmix_nextcid,
    .activate_nb= ompi_cid_base_comm_activate_nb,
    .activate= ompi_cid_base_comm_activate,
    .release_nb = cid_pmix_release_nb,
    .release = cid_pmix_release,
    .init = cid_pmix_init
};

static opal_mutex_t ompi_cid_lock = OPAL_MUTEX_STATIC_INIT;

struct participant_t {
    opal_namelist_t super;
    int rank;
    int participate;
};

typedef struct participant_t participant_t;

static void participant_construct(participant_t * participant) {
    participant->rank = MPI_UNDEFINED;
    participant->participate = false;
}

static OBJ_CLASS_INSTANCE(participant_t, opal_namelist_t, participant_construct, NULL);

struct pmix_context_t {
    ompi_cid_base_cid_context_t super;
    int participate;
    int master;
    opal_list_t localprocs;
    int children;
};

typedef struct pmix_context_t pmix_context_t;

static void pmix_context_construct (pmix_context_t *context)
{
    OBJ_CONSTRUCT(&context->localprocs, opal_list_t);
    context->participate = true;
    context->master = MPI_UNDEFINED;
    context->children = 0;
}

static void pmix_context_destruct (pmix_context_t *context)
{
    OPAL_LIST_DESTRUCT(&context->localprocs);
}

static OBJ_CLASS_INSTANCE (pmix_context_t, ompi_cid_base_cid_context_t,
                           pmix_context_construct,
                           pmix_context_destruct);

int cid_pmix_init (void)
{
    return OMPI_SUCCESS;
}

/* find the next available local cid and start an allreduce */
static int ompi_comm_allreduce_getnextcid (ompi_comm_request_t *request);
static int ompi_comm_allreduce_getlocalprocs (ompi_comm_request_t *request);
static int ompi_comm_allreduce_nextlocal_cid (ompi_comm_request_t *request);
/* verify that the maximum cid is locally available and start an allreduce */
static int ompi_comm_checkcid (ompi_comm_request_t *request);
static int ompi_comm_checkcid2 (ompi_comm_request_t *request);
/* verify that the cid was available globally */
static int ompi_comm_nextcid_check_flag (ompi_comm_request_t *request);
static int ompi_comm_nextcid_setcid (ompi_comm_request_t *request);

static volatile int64_t ompi_comm_cid_lowest_id = INT64_MAX;

static int cid_pmix_nextcid_nb (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                                bool send_first, int mode, ompi_request_t **req)
{
    pmix_context_t *context;
    ompi_comm_request_t *request;
    ompi_request_t **reqs = NULL;
    int ret;

    context = OBJ_NEW(pmix_context_t);
    if (NULL == context) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ret = ompi_cid_base_cid_context_init (&context->super, newcomm, comm, bridgecomm, arg0, arg1,
                                          "nextcid", send_first, mode);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        OBJ_RELEASE(context);
        return ret;
    }

    context->super.start = 3;
    context->super.nextlocal_cid = 0;
    context->participate = (context->super.newcomm->c_local_group->grp_my_rank != MPI_UNDEFINED)?1:0;

    request = ompi_comm_request_get ();
    if (NULL == request) {
        OBJ_RELEASE(context);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    request->context = &context->super.super;
    context->master = -1;


    if (OMPI_COMM_IS_INTER(context->super.comm)) {
        context->children = 0;
        participant_t *p = OBJ_NEW(participant_t);
        p->super.name = OPAL_PROC_MY_NAME;
        p->rank = ompi_comm_rank(context->super.comm);
        p->participate = context->participate;
        opal_list_append(&context->localprocs, &p->super.super);
        ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);
    } else {
        ompi_group_t *group = context->super.comm->c_local_group;
        /* am I the master (e.g.) task with the lowest rank on this node */
        for (int i=0; i<ompi_comm_rank(context->super.comm); i++) {
            if (!ompi_proc_is_sentinel(group->grp_proc_pointers[i]) &&
               group->grp_proc_pointers[i]->super.proc_flags & OPAL_PROC_ON_NODE) {
                context->master = i;
                break;
            }
        }
        if (0 > context->master) {
            context->children = 0;
            for (int i=ompi_comm_rank(context->super.comm)+1; i<group->grp_proc_count; i++) {
                if (!ompi_proc_is_sentinel(group->grp_proc_pointers[i]) &&
                   group->grp_proc_pointers[i]->super.proc_flags & OPAL_PROC_ON_NODE) {
                    context->children++;
                }
            }
            if (context->children > 0) {
                int j=0;
                reqs = (ompi_request_t **)alloca(context->children * sizeof(ompi_request_t *));
                for (int i=ompi_comm_rank(context->super.comm)+1; i<group->grp_proc_count; i++) {
                    if (!ompi_proc_is_sentinel(group->grp_proc_pointers[i]) &&
                       group->grp_proc_pointers[i]->super.proc_flags & OPAL_PROC_ON_NODE) {
                        participant_t *p = OBJ_NEW(participant_t);
                        p->super.name = group->grp_proc_pointers[i]->super.proc_name;
                        p->rank = i;
                        MCA_PML_CALL(irecv(&p->participate, 1, MPI_INT, i, OMPI_COMM_LOCAL_TAG, context->super.comm, reqs+j));
                        opal_list_append(&context->localprocs, &p->super.super);
                        j++;
                    }
                }
                assert (j == context->children);
            }
            ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getlocalprocs, reqs, context->children);
        } else {
            /* I am not the master, tell the master whether I participate or not */
            reqs = (ompi_request_t **)alloca(sizeof(ompi_request_t *));
            MCA_PML_CALL(isend(&context->participate, 1, MPI_INT, context->master, OMPI_COMM_LOCAL_TAG, MCA_PML_BASE_SEND_STANDARD, context->super.comm, reqs));
            ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, reqs, 1);
        }
    }
    ompi_comm_request_start (request);

    *req = &request->super;


    return OMPI_SUCCESS;
}

int cid_pmix_nextcid (ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                      ompi_communicator_t *bridgecomm, const void *arg0, const void *arg1,
                      bool send_first, int mode)
{
    ompi_request_t *req;
    int rc;

    rc = cid_pmix_nextcid_nb (newcomm, comm, bridgecomm, arg0, arg1, send_first, mode, &req);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    ompi_request_wait_completion (req);
    rc = req->req_status.MPI_ERROR;
    ompi_comm_request_return ((ompi_comm_request_t *) req);

    return rc;
}

struct ompi_request_cid_t {
    ompi_request_t super;
    pmix_context_t *context;
};

typedef struct ompi_request_cid_t ompi_request_cid_t;

static int reqcid_free(ompi_request_t **req) {
    OBJ_RELEASE(*req);
    return OMPI_SUCCESS;
}

static int reqcid_cancel(ompi_request_t *req, int complete) {
   return OMPI_ERROR;
}

static void reqcid_cons(ompi_request_cid_t *req) {
    req->super.req_type = OMPI_REQUEST_PMIX;
    req->super.req_free = reqcid_free;
    req->super.req_cancel = reqcid_cancel;
    req->super.req_state = OMPI_REQUEST_ACTIVE;
}

static OBJ_CLASS_INSTANCE(ompi_request_cid_t, ompi_request_t, reqcid_cons, NULL);

static void getnextcid_cbfunc(int status, int cid, void *cbdata) {
    ompi_request_cid_t *req = (ompi_request_cid_t *)cbdata;
    if (OMPI_SUCCESS == status) {
        req->context->super.nextlocal_cid = cid;
        OMPI_REQUEST_FINI(&req->super);
        ompi_request_complete(&req->super, true);
    }
}

static int ompi_comm_allreduce_getnextcid (ompi_comm_request_t *request)
{
    pmix_context_t *context = (pmix_context_t *) request->context;
    int64_t my_id = ((int64_t) ompi_comm_get_cid (context->super.comm) << 32 | context->super.pml_tag);
    ompi_request_t *subreq = NULL;
#if 0
    bool flag;
    int ret;
#endif

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);
    }

    if (ompi_comm_cid_lowest_id < my_id) {
        OPAL_THREAD_UNLOCK(&ompi_cid_lock);
        return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);
    }

    ompi_comm_cid_lowest_id = my_id;

    if (0 <= context->master) {
        context->super.nextlocal_cid = 0;
        return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_nextlocal_cid, NULL, 0);
    }
    /**
     * This is the real algorithm described in the doc
     */
    if (0 < opal_list_get_size(&context->localprocs)) {
        if (context->super.nextlocal_cid < context->super.start) {
            ompi_request_cid_t *req = OBJ_NEW(ompi_request_cid_t);
            subreq = &req->super;
            // context->nextlocal_cid = mca_pml.pml_max_contextid;
            req->context = context;
            opal_pmix.cid_nb(&context->localprocs, context->super.start, context->super.nextlocal_cid, getnextcid_cbfunc, req);
        }
    } else {
        context->super.nextlocal_cid = 0;
    }
    return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_nextlocal_cid, &subreq, (NULL==subreq)?0:1);
}

static int ompi_comm_allreduce_getlocalprocs (ompi_comm_request_t *request)
{
    pmix_context_t *context = (pmix_context_t *) request->context;
    participant_t *proc, *next;

    
    OPAL_LIST_FOREACH_SAFE(proc, next, &context->localprocs, participant_t ) {
        if (!proc->participate) {
            opal_list_remove_item(&context->localprocs, &proc->super.super);
            context->children --;
        }
    }

    if (context->participate) {
        participant_t *proc = OBJ_NEW(participant_t);
        proc->super.name = OPAL_PROC_MY_NAME;
        proc->rank = ompi_comm_rank(context->super.comm);
        proc->participate = 1;
        opal_list_append(&context->localprocs, &proc->super.super);
    }
    
    return ompi_comm_request_schedule_append (request, ompi_comm_allreduce_getnextcid, NULL, 0);

}

static int ompi_comm_allreduce_nextlocal_cid (ompi_comm_request_t *request)
{
    pmix_context_t *context = (pmix_context_t *) request->context;
    int ret;

    ompi_request_t *subreq;
    ret = context->super.allreduce_fn (&context->super.nextlocal_cid, &context->super.nextcid, 1, MPI_MAX,
                                 &context->super, &subreq);
    /* there was a failure during non-blocking collective
     * all we can do is abort
     */
    if (OMPI_SUCCESS != ret) {
        goto err_exit;
    }

    if ( ((unsigned int) context->super.nextlocal_cid == mca_pml.pml_max_contextid) ) {
        /* Our local CID space is out, others already aware (allreduce above) */
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto err_exit;
    }
    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    /* next we want to verify that the resulting commid is ok */
    return ompi_comm_request_schedule_append (request, ompi_comm_checkcid, &subreq, 1);
err_exit:
    if (context->participate && context->super.nextlocal_cid != mca_pml.pml_max_contextid) {
        opal_pointer_array_test_and_set_item(&ompi_mpi_communicators, context->super.nextlocal_cid, NULL);
    }
    ompi_comm_cid_lowest_id = INT64_MAX;
    OPAL_THREAD_UNLOCK(&ompi_cid_lock);
    return ret;

}

static void checkcid_cbfunc(int status, int cid, void *cbdata) {
    ompi_request_cid_t *req = (ompi_request_cid_t *)cbdata;
    if (OMPI_SUCCESS == status) {
        if (cid == req->context->super.nextcid) {
            req->context->super.flag = 1;
        }
        req->context->super.nextlocal_cid = cid;
        OMPI_REQUEST_FINI(&req->super);
        ompi_request_complete(&req->super, true);
    }
}

static int ompi_comm_checkcid (ompi_comm_request_t *request)
{
    pmix_context_t *context = (pmix_context_t *) request->context;
    ompi_request_t *subreq = NULL;

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_checkcid, NULL, 0);
    }

    if (0 <= context->master || 0 == opal_list_get_size(&context->localprocs)) {
        context->super.flag = 1;
    } else {
        context->super.flag = (context->super.nextcid == context->super.nextlocal_cid);
        if (!context->super.flag) {
            ompi_request_cid_t *req = OBJ_NEW(ompi_request_cid_t);
            subreq = &req->super;
            req->context = context;
            opal_pmix.cid_nb(&context->localprocs, context->super.nextcid, context->super.nextlocal_cid, checkcid_cbfunc, req);
        }
    }

    ++context->super.iter;

    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    return ompi_comm_request_schedule_append (request, ompi_comm_checkcid2, &subreq, (NULL==subreq)?0:1);
}

static int ompi_comm_checkcid2 (ompi_comm_request_t *request)
{
    pmix_context_t *context = (pmix_context_t *) request->context;
    ompi_request_t *subreq;
    int ret;

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_checkcid2, NULL, 0);
    }

    ret = context->super.allreduce_fn (&context->super.flag, &context->super.rflag, 1, MPI_MIN, &context->super, &subreq);
    if (OMPI_SUCCESS == ret) {
        ompi_comm_request_schedule_append (request, ompi_comm_nextcid_check_flag, &subreq, 1);
    } else {
        if (context->participate && context->super.flag ) {
            opal_pointer_array_test_and_set_item(&ompi_mpi_communicators, context->super.nextlocal_cid, NULL);
        }
        ompi_comm_cid_lowest_id = INT64_MAX;
    }

    OPAL_THREAD_UNLOCK(&ompi_cid_lock);
    return ret;
}

static int ompi_comm_nextcid_check_flag (ompi_comm_request_t *request)
{
    pmix_context_t *context = (pmix_context_t *) request->context;

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_nextcid_check_flag, NULL, 0);
    }

    if (1 == context->super.rflag) {
        ompi_request_t ** reqs = NULL;
        int j = 0;
        if (0 <= context->master) {
            reqs = (ompi_request_t **)alloca(sizeof(ompi_request_t *));
            if (context->participate) {
                MCA_PML_CALL(irecv(&context->super.nextlocal_cid, 1, MPI_INT, context->master, OMPI_COMM_LOCAL_TAG,
                                   context->super.comm, reqs));
                j++;
            }
        } else {
            if (context->children > 0) {
                participant_t *p;
                reqs = (ompi_request_t **)alloca(context->children * sizeof(ompi_request_t *));
                OPAL_LIST_FOREACH(p, &context->localprocs, participant_t) {
                    if (ompi_comm_rank(context->super.comm) != p->rank) {
                        MCA_PML_CALL(isend(&context->super.nextlocal_cid, 1, MPI_INT, p->rank, OMPI_COMM_LOCAL_TAG,
                                           MCA_PML_BASE_SEND_STANDARD, context->super.comm, reqs + j));
                        j++;
                    }
                }
                assert(j == context->children);
            }
        }
        return ompi_comm_request_schedule_append (request, ompi_comm_nextcid_setcid, reqs, j);
    }

    if (context->super.flag) {
        /* we could use this cid, but other don't agree */
        context->super.start = context->super.nextcid + 1; /* that's where we can start the next round */
    }

    ++context->super.iter;

    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    /* try again */
    return ompi_comm_allreduce_getnextcid (request);
}

static int ompi_comm_nextcid_setcid (ompi_comm_request_t *request)
{
    pmix_context_t *context = (pmix_context_t *) request->context;

    if (OPAL_THREAD_TRYLOCK(&ompi_cid_lock)) {
        return ompi_comm_request_schedule_append (request, ompi_comm_nextcid_setcid, NULL, 0);
    }

    if( context->participate ) {
        /* set the according values to the newcomm */
        context->super.newcomm->c_contextid = context->super.nextcid;
        opal_pointer_array_set_item (&ompi_mpi_communicators, context->super.nextcid, context->super.newcomm);
    }

    /* unlock the cid generator */
    ompi_comm_cid_lowest_id = INT64_MAX;
    OPAL_THREAD_UNLOCK(&ompi_cid_lock);

    /* done! */
    return OMPI_SUCCESS;
}

static void cid_cbfunc(int status, int cid, void *cbdata) {
    bool *active = (bool *)cbdata;
    *active = false;
}

static int cid_pmix_release(int cid)
{
    opal_list_t procs;
    opal_namelist_t proc;
    bool active = true;
    OBJ_CONSTRUCT(&procs, opal_list_t);
    OBJ_CONSTRUCT(&proc, opal_namelist_t);
    proc.name = OPAL_PROC_MY_NAME;
    opal_list_append(&procs, &proc.super);
    opal_pmix.cid_nb(&procs, 0, cid, cid_cbfunc, &active);
    ORTE_WAIT_FOR_COMPLETION(active);
    opal_list_remove_item(&procs, &proc.super);
    OBJ_DESTRUCT(&procs);
    OBJ_DESTRUCT(&proc);
    return OMPI_SUCCESS;
}

static int cid_pmix_release_nb (int cid, ompi_request_t **req)
{
    return OMPI_ERROR;
}
