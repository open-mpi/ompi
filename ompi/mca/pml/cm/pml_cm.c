/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018 IBM Corporation. All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/runtime/opal_progress.h"

#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errcode-internal.h"
#include "ompi/mca/pml/base/pml_base_request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/ompi_modex.h"

#include "pml_cm.h"
#include "pml_cm_sendreq.h"
#include "pml_cm_recvreq.h"
#include "pml_cm_component.h"

static int mca_pml_cm_staged_progress_cb(void);

ompi_pml_cm_t ompi_pml_cm = {
    {
        .pml_add_procs     = mca_pml_cm_add_procs,
        .pml_del_procs     = mca_pml_cm_del_procs,
        .pml_enable        = mca_pml_cm_enable,
        .pml_progress      = NULL, /* No progress function. The MTL register their own */
        .pml_add_comm      = mca_pml_cm_add_comm,
        .pml_del_comm      = mca_pml_cm_del_comm,
        .pml_irecv_init    = mca_pml_cm_irecv_init,
        .pml_irecv         = mca_pml_cm_irecv,
        .pml_recv          = mca_pml_cm_recv,
        .pml_isend_init    = mca_pml_cm_isend_init,
        .pml_isend         = mca_pml_cm_isend,
        .pml_send          = mca_pml_cm_send,
        .pml_iprobe        = mca_pml_cm_iprobe,
        .pml_probe         = mca_pml_cm_probe,
        .pml_start         = mca_pml_cm_start,
        .pml_improbe       = mca_pml_cm_improbe,
        .pml_mprobe        = mca_pml_cm_mprobe,
        .pml_imrecv        = mca_pml_cm_imrecv,
        .pml_mrecv         = mca_pml_cm_mrecv,
        .pml_dump          = mca_pml_cm_dump,
        .pml_max_contextid = 0,
        .pml_max_tag       = 0,
        .pml_flags         = 0 /* flags */
    }
};


int
mca_pml_cm_enable(bool enable)
{
    /* BWB - FIX ME - need to have this actually do something,
       maybe? */
    opal_free_list_init (&mca_pml_base_send_requests,
                         sizeof(mca_pml_cm_hvy_send_request_t) + ompi_mtl->mtl_request_size,
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_cm_hvy_send_request_t),
                         0,opal_cache_line_size,
                         ompi_pml_cm.free_list_num,
                         ompi_pml_cm.free_list_max,
                         ompi_pml_cm.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);

    opal_free_list_init (&mca_pml_base_recv_requests,
                         sizeof(mca_pml_cm_hvy_recv_request_t) + ompi_mtl->mtl_request_size,
                         opal_cache_line_size,
                         OBJ_CLASS(mca_pml_cm_hvy_recv_request_t),
                         0,opal_cache_line_size,
                         ompi_pml_cm.free_list_num,
                         ompi_pml_cm.free_list_max,
                         ompi_pml_cm.free_list_inc,
                         NULL, 0, NULL, NULL, NULL);

    return OMPI_SUCCESS;
}


int
mca_pml_cm_add_comm(ompi_communicator_t* comm)
{
    /* should never happen, but it was, so check */
    if (comm->c_index > ompi_pml_cm.super.pml_max_contextid) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* initialize per-communicator data. MTLs may override this. */
    comm->c_pml_comm = NULL;

    /* notify the MTL about the added communicator */
    return OMPI_MTL_CALL(add_comm(ompi_mtl, comm));
}


int
mca_pml_cm_del_comm(ompi_communicator_t* comm)
{
    /* notify the MTL about the deleted communicator */
    return OMPI_MTL_CALL(del_comm(ompi_mtl, comm));
}


int
mca_pml_cm_add_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    int ret;

    /* A proc built on demand carries the local architecture until its
     * modex data has been read, so seed it before the check below looks
     * at it -- otherwise a peer of unknown architecture compares equal
     * to us and slips through. */
    for (size_t i = 0 ; i < nprocs ; ++i) {
        ret = ompi_proc_ensure_arch(procs[i]);
        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    for (size_t i = 0 ; i < nprocs ; ++i) {
        if (procs[i]->super.proc_arch != ompi_proc_local()->super.proc_arch) {
            return OMPI_ERR_NOT_SUPPORTED;
        }
    }
#endif

    /* make sure remote procs are using the same PML as us */
    if (OMPI_SUCCESS != (ret = mca_pml_base_pml_check_selected("cm",
                                                              procs,
                                                              nprocs))) {
        return ret;
    }

    ret = OMPI_MTL_CALL(add_procs(ompi_mtl, nprocs, procs));
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    for (size_t i = 0 ; i < nprocs ; ++i) {
        opal_proc_learned(&procs[i]->super, OPAL_PROC_FLAG_WIRED);
    }

    return OMPI_SUCCESS;
}


int
mca_pml_cm_del_procs(struct ompi_proc_t** procs, size_t nprocs)
{
    int ret;

    /* Unwired before the MTL is told, not after: the peer stops being
     * reachable the moment this starts. */
    for (size_t i = 0 ; i < nprocs ; ++i) {
        opal_proc_forget(&procs[i]->super, OPAL_PROC_FLAG_WIRED);
    }

    ret = OMPI_MTL_CALL(del_procs(ompi_mtl, nprocs, procs));
    return ret;
}


int mca_pml_cm_ensure_proc(ompi_proc_t *proc)
{
    if (OPAL_LIKELY(opal_proc_known(&proc->super, OPAL_PROC_FLAG_WIRED))) {
        return OMPI_SUCCESS;
    }

    /* No lock here on purpose: reading the connection info can progress
     * the runtime, which can call back into this PML, so holding one
     * across it would deadlock. Two threads reaching the same unwired
     * peer both wire it, which the MTL has to make harmless.
     *
     * Wiring a peer needs its connection info, which under an
     * asynchronous exchange may not have arrived yet. NOT_READY comes
     * back out to the caller, which either stages the operation or, if
     * it is allowed to block, waits for the exchange. */
    return mca_pml_cm_add_procs(&proc, 1);
}


int mca_pml_cm_wait_proc(ompi_proc_t *proc)
{
    int rc = mca_pml_cm_ensure_proc(proc);

    if (OPAL_LIKELY(OMPI_ERR_NOT_READY != rc)) {
        return rc;
    }

    /* The caller is a blocking operation, so waiting for the exchange
     * is the expected behaviour rather than a stall. */
    (void) ompi_modex_wait_if_needed();

    /* In direct-modex mode there is no fence to wait on and a blob may
     * still be in flight; progress until it lands. */
    while (OMPI_ERR_NOT_READY == (rc = mca_pml_cm_ensure_proc(proc))) {
        opal_progress();
    }

    return rc;
}


/* Registered exactly while requests are parked. This PML has no progress
 * function of its own -- the MTLs register theirs -- and a staged request
 * has nothing outstanding that would come back for it, so the queue being
 * non-empty is what has to keep a tick coming. */
static bool mca_pml_cm_staged_progress = false;

int mca_pml_cm_stage_request(mca_pml_cm_request_t *req)
{
    int rc = OMPI_SUCCESS;

    req->req_staged = true;

    OPAL_THREAD_LOCK(&ompi_pml_cm.lock);
    opal_list_append(&ompi_pml_cm.modex_pending, (opal_list_item_t *) req);
    if (!mca_pml_cm_staged_progress) {
        rc = opal_progress_register(mca_pml_cm_staged_progress_cb);
        if (OPAL_SUCCESS == rc) {
            mca_pml_cm_staged_progress = true;
        } else {
            opal_list_remove_item(&ompi_pml_cm.modex_pending, (opal_list_item_t *) req);
            req->req_staged = false;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_pml_cm.lock);

    return rc;
}


bool mca_pml_cm_unstage_request(mca_pml_cm_request_t *req)
{
    bool found = false;

    OPAL_THREAD_LOCK(&ompi_pml_cm.lock);
    if (req->req_staged) {
        opal_list_remove_item(&ompi_pml_cm.modex_pending, (opal_list_item_t *) req);
        req->req_staged = false;
        found = true;
    }
    OPAL_THREAD_UNLOCK(&ompi_pml_cm.lock);

    return found;
}


static int32_t mca_pml_cm_staged_peer(mca_pml_cm_request_t *req)
{
    if (MCA_PML_CM_REQUEST_SEND_HEAVY == req->req_pml_type) {
        return ((mca_pml_cm_hvy_send_request_t *) req)->req_peer;
    }
    return ((mca_pml_cm_hvy_recv_request_t *) req)->req_peer;
}


/*
 * A staged request was prepared against a peer whose architecture was
 * not known yet, so against the local convertor. That needs no fixing
 * up here: this PML refuses a peer whose architecture differs from ours
 * (see mca_pml_cm_add_procs()), so any peer that gets wired keeps the
 * local convertor, and any peer that does not fails the request below.
 */
/* True once this request is off our hands, either started or completed
 * with an error; false while it is still waiting on its peer. */
static bool mca_pml_cm_start_staged(mca_pml_cm_request_t *req)
{
    ompi_proc_t *proc = ompi_comm_peer_lookup(req->req_comm,
                                              mca_pml_cm_staged_peer(req));
    int rc = (NULL == proc) ? OMPI_ERR_UNREACH : mca_pml_cm_ensure_proc(proc);

    if (OMPI_ERR_NOT_READY == rc) {
        /* A direct-modex read can miss a blob that is on its way: come
         * back on a later tick. */
        (void) mca_pml_cm_stage_request(req);
        return false;
    }

    if (MCA_PML_CM_REQUEST_SEND_HEAVY == req->req_pml_type) {
        mca_pml_cm_hvy_send_request_t *sendreq = (mca_pml_cm_hvy_send_request_t *) req;

        if (OMPI_SUCCESS == rc) {
            MCA_PML_CM_HVY_SEND_REQUEST_POST(sendreq, rc);
        }
        if (OMPI_SUCCESS != rc) {
            /* The user already holds this request, so the only way to
             * report the failure is through its status, which carries
             * MPI codes. */
            sendreq->req_send.req_base.req_ompi.req_status.MPI_ERROR =
                ompi_errcode_get_mpi_code(rc);
            MCA_PML_CM_HVY_SEND_REQUEST_PML_COMPLETE(sendreq);
        }
    } else {
        mca_pml_cm_hvy_recv_request_t *recvreq = (mca_pml_cm_hvy_recv_request_t *) req;

        if (OMPI_SUCCESS == rc) {
            MCA_PML_CM_HVY_RECV_REQUEST_POST(recvreq, rc);
        }
        if (OMPI_SUCCESS != rc) {
            recvreq->req_base.req_ompi.req_status.MPI_ERROR =
                ompi_errcode_get_mpi_code(rc);
            MCA_PML_CM_HVY_RECV_REQUEST_PML_COMPLETE(recvreq);
        }
    }

    return true;
}


int mca_pml_cm_start_peer(mca_pml_cm_request_t *req, int peer, bool must_wait)
{
    ompi_proc_t *proc;
    int rc;

    if (MPI_ANY_SOURCE == peer) {
        return OMPI_SUCCESS;
    }

    proc = ompi_comm_peer_lookup(req->req_comm, peer);
    if (OPAL_UNLIKELY(NULL == proc)) {
        return OMPI_ERR_UNREACH;
    }
    if (OPAL_LIKELY(opal_proc_known(&proc->super, OPAL_PROC_FLAG_WIRED))) {
        return OMPI_SUCCESS;
    }

    rc = must_wait ? mca_pml_cm_wait_proc(proc) : mca_pml_cm_ensure_proc(proc);
    if (OMPI_ERR_NOT_READY != rc) {
        return rc;
    }

    /* Park it: the request is live from the caller's point of view, so
     * it has to look started even though the MTL has not seen it. */
    if (MCA_PML_CM_REQUEST_SEND_HEAVY == req->req_pml_type) {
        mca_pml_cm_hvy_send_request_t *sendreq = (mca_pml_cm_hvy_send_request_t *) req;

        MCA_PML_CM_SEND_REQUEST_START_SETUP(&sendreq->req_send);
    } else {
        mca_pml_cm_hvy_recv_request_t *recvreq = (mca_pml_cm_hvy_recv_request_t *) req;

        MCA_PML_CM_RECV_REQUEST_START_SETUP(recvreq);
    }

    rc = mca_pml_cm_stage_request(req);

    return (OMPI_SUCCESS == rc) ? OMPI_ERR_NOT_READY : rc;
}


/*
 * Slow paths, taken the first time an operation names a peer that is not
 * wired yet. They build a heavy request even where the fast path would
 * have used a thin one: a thin request does not remember its peer, tag
 * or mode, so it cannot be replayed once the connection info arrives.
 */
int mca_pml_cm_isend_slow(const void *buf, size_t count,
                          ompi_datatype_t *datatype, int dst, int tag,
                          mca_pml_base_send_mode_t sendmode,
                          ompi_communicator_t *comm,
                          ompi_request_t **request)
{
    mca_pml_cm_hvy_send_request_t *sendreq;
    ompi_proc_t *proc;
    uint32_t flags = 0;
    int ret;

    proc = ompi_comm_peer_lookup(comm, dst);
    if (OPAL_UNLIKELY(NULL == proc)) {
        return OMPI_ERR_UNREACH;
    }

    /* A buffered send has to copy the message out before it returns, so
     * it cannot be parked untouched. Rather than split the packing from
     * the posting, wait here: this path copies the whole message anyway,
     * and the wait only happens before the exchange has completed. */
    ret = (MCA_PML_BASE_SEND_BUFFERED == sendmode) ? mca_pml_cm_wait_proc(proc)
                                                   : mca_pml_cm_ensure_proc(proc);
    if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_READY != ret) {
        return ret;
    }

    MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, proc);
    if (OPAL_UNLIKELY(NULL == sendreq)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    MCA_PML_CM_HVY_SEND_REQUEST_INIT(sendreq, proc, comm, tag, dst, datatype,
                                     sendmode, false, false, buf, count, flags);

    if (OMPI_ERR_NOT_READY == ret) {
        MCA_PML_CM_SEND_REQUEST_START_SETUP(&sendreq->req_send);
        ret = mca_pml_cm_stage_request(&sendreq->req_send.req_base);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            (void) mca_pml_cm_unstage_request(&sendreq->req_send.req_base);
            MCA_PML_CM_HVY_SEND_REQUEST_RETURN(sendreq);
            return ret;
        }
        *request = (ompi_request_t *) sendreq;
        return OMPI_SUCCESS;
    }

    MCA_PML_CM_HVY_SEND_REQUEST_START(sendreq, ret);
    if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
        *request = (ompi_request_t *) sendreq;
    } else {
        MCA_PML_CM_HVY_SEND_REQUEST_RETURN(sendreq);
    }

    return ret;
}


int mca_pml_cm_irecv_slow(void *addr, size_t count, ompi_datatype_t *datatype,
                          int src, int tag, ompi_communicator_t *comm,
                          ompi_request_t **request)
{
    mca_pml_cm_hvy_recv_request_t *recvreq;
    ompi_proc_t *proc;
    uint32_t flags = 0;
    int ret;

    proc = ompi_comm_peer_lookup(comm, src);
    if (OPAL_UNLIKELY(NULL == proc)) {
        return OMPI_ERR_UNREACH;
    }

    ret = mca_pml_cm_ensure_proc(proc);
    if (OMPI_SUCCESS != ret && OMPI_ERR_NOT_READY != ret) {
        return ret;
    }

    MCA_PML_CM_HVY_RECV_REQUEST_ALLOC(recvreq);
    if (OPAL_UNLIKELY(NULL == recvreq)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    MCA_PML_CM_HVY_RECV_REQUEST_INIT(recvreq, proc, comm, tag, src, datatype,
                                     addr, count, flags, false);

    if (OMPI_ERR_NOT_READY == ret) {
        MCA_PML_CM_RECV_REQUEST_START_SETUP(recvreq);
        ret = mca_pml_cm_stage_request(&recvreq->req_base);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            (void) mca_pml_cm_unstage_request(&recvreq->req_base);
            MCA_PML_CM_HVY_RECV_REQUEST_RETURN(recvreq);
            return ret;
        }
        *request = (ompi_request_t *) recvreq;
        return OMPI_SUCCESS;
    }

    MCA_PML_CM_HVY_RECV_REQUEST_START(recvreq, ret);
    if (OPAL_LIKELY(OMPI_SUCCESS == ret)) {
        *request = (ompi_request_t *) recvreq;
    } else {
        MCA_PML_CM_HVY_RECV_REQUEST_RETURN(recvreq);
    }

    return ret;
}


static int mca_pml_cm_staged_progress_cb(void)
{
    mca_pml_cm_request_t *req, *next;
    opal_list_t ready;
    int started = 0;

    OPAL_THREAD_LOCK(&ompi_pml_cm.lock);
    if (opal_list_is_empty(&ompi_pml_cm.modex_pending)) {
        /* Nothing left to come back for. opal_progress() allows a callback
         * to remove itself, and a request staged after this point registers
         * it again. */
        if (mca_pml_cm_staged_progress) {
            opal_progress_unregister(mca_pml_cm_staged_progress_cb);
            mca_pml_cm_staged_progress = false;
        }
        OPAL_THREAD_UNLOCK(&ompi_pml_cm.lock);
        return 0;
    }
    /* Everything comes off, and what still cannot reach its peer is put
     * back by mca_pml_cm_start_staged(). Asking about a peer is also what
     * fetches its data where peers are fetched one at a time, so taking
     * requests whose peer nobody has asked about is the point. */
    OBJ_CONSTRUCT(&ready, opal_list_t);
    OPAL_LIST_FOREACH_SAFE(req, next, &ompi_pml_cm.modex_pending,
                           mca_pml_cm_request_t) {
        opal_list_remove_item(&ompi_pml_cm.modex_pending, (opal_list_item_t *) req);
        req->req_staged = false;
        opal_list_append(&ready, (opal_list_item_t *) req);
    }
    OPAL_THREAD_UNLOCK(&ompi_pml_cm.lock);

    OPAL_LIST_FOREACH_SAFE(req, next, &ready, mca_pml_cm_request_t) {
        opal_list_remove_item(&ready, (opal_list_item_t *) req);
        started += mca_pml_cm_start_staged(req) ? 1 : 0;
    }
    OBJ_DESTRUCT(&ready);

    return started;
}


/* print any available useful information from this communicator */
int
mca_pml_cm_dump(struct ompi_communicator_t* comm, int verbose)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
