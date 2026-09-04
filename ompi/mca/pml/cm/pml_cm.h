/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef PML_CM_H
#define PML_CM_H

#include "ompi_config.h"
#include "opal/class/opal_list.h"
#include "opal/mca/threads/mutex.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "ompi/request/request.h"
#include "ompi/mca/mtl/mtl.h"


#include "pml_cm_request.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"
#include "ompi/mca/mtl/mtl.h"
#include "pml_cm_recvreq.h"
#include "pml_cm_sendreq.h"
#include "ompi/message/message.h"


BEGIN_C_DECLS

struct mca_mtl_request_t;

struct ompi_pml_cm_t {
    mca_pml_base_module_t super;
    int                   free_list_num;
    int                   free_list_max;
    int                   free_list_inc;
    /** requests waiting for a peer's connection info to arrive */
    opal_list_t           modex_pending;
    opal_mutex_t          lock;
};
typedef struct ompi_pml_cm_t ompi_pml_cm_t;
extern ompi_pml_cm_t ompi_pml_cm;

/* PML interface functions */
OMPI_DECLSPEC extern int mca_pml_cm_add_procs(struct ompi_proc_t **procs, size_t nprocs);
OMPI_DECLSPEC extern int mca_pml_cm_del_procs(struct ompi_proc_t **procs, size_t nprocs);

/**
 * Wire a peer if it is not wired yet.
 *
 * Returns OMPI_ERR_NOT_READY when the peer's connection info has not
 * arrived: the caller either stages the operation or, when it is
 * allowed to block, calls mca_pml_cm_wait_proc() instead.
 */
OMPI_DECLSPEC extern int mca_pml_cm_ensure_proc(struct ompi_proc_t *proc);

/** Wire a peer, waiting for the exchange if need be. */
OMPI_DECLSPEC extern int mca_pml_cm_wait_proc(struct ompi_proc_t *proc);

/**
 * Park a request until its peer is wired, and replay it then. The peer is
 * the request's own, read back from it on each attempt, so this takes no
 * proc: nothing is recorded against one.
 */
OMPI_DECLSPEC extern int mca_pml_cm_stage_request(mca_pml_cm_request_t *req);

/** Take a parked request back off the list; false if it was not there. */
OMPI_DECLSPEC extern bool mca_pml_cm_unstage_request(mca_pml_cm_request_t *req);

/**
 * Wire the peer of a persistent request being started. Returns
 * OMPI_ERR_NOT_READY once the request has been parked instead, in which
 * case the caller must not start it.
 */
OMPI_DECLSPEC extern int mca_pml_cm_start_peer(mca_pml_cm_request_t *req, int peer,
                                               bool must_wait);

/**
 * Can an operation on this peer go straight to the MTL? A peer is wired
 * on first use, and MPI_ANY_SOURCE names no peer at all.
 */
__opal_attribute_always_inline__ static inline bool
mca_pml_cm_peer_wired(ompi_communicator_t *comm, int rank)
{
    ompi_proc_t *proc;

    if (MPI_ANY_SOURCE == rank) {
        return true;
    }
    proc = ompi_comm_peer_lookup(comm, rank);

    return (NULL != proc && opal_proc_known(&proc->super, OPAL_PROC_FLAG_WIRED));
}

/**
 * Slow paths for an operation whose peer is not wired yet. They build a
 * heavy request, which unlike a thin one remembers enough to be
 * replayed, and park it if the connection info has still not arrived.
 */
OMPI_DECLSPEC extern int mca_pml_cm_isend_slow(const void *buf, size_t count,
                                               ompi_datatype_t *datatype, int dst,
                                               int tag,
                                               mca_pml_base_send_mode_t sendmode,
                                               ompi_communicator_t *comm,
                                               ompi_request_t **request);
OMPI_DECLSPEC extern int mca_pml_cm_irecv_slow(void *addr, size_t count,
                                               ompi_datatype_t *datatype, int src,
                                               int tag, ompi_communicator_t *comm,
                                               ompi_request_t **request);

/**
 * Wire a peer for an operation that is allowed to block, so that the
 * call below it can assume the peer is usable.
 */
__opal_attribute_always_inline__ static inline int
mca_pml_cm_peer_wire_wait(ompi_communicator_t *comm, int rank)
{
    ompi_proc_t *proc;

    if (OPAL_LIKELY(mca_pml_cm_peer_wired(comm, rank))) {
        return OMPI_SUCCESS;
    }
    proc = ompi_comm_peer_lookup(comm, rank);

    return (NULL == proc) ? OMPI_ERR_UNREACH : mca_pml_cm_wait_proc(proc);
}

OMPI_DECLSPEC extern int mca_pml_cm_enable(bool enable);
OMPI_DECLSPEC extern int mca_pml_cm_progress(void);

OMPI_DECLSPEC extern int mca_pml_cm_add_comm(struct ompi_communicator_t* comm);
OMPI_DECLSPEC extern int mca_pml_cm_del_comm(struct ompi_communicator_t* comm);


__opal_attribute_always_inline__ static inline int
mca_pml_cm_irecv_init(void *addr,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      struct ompi_request_t **request)
{
    mca_pml_cm_hvy_recv_request_t *recvreq;
    uint32_t flags = 0;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    ompi_proc_t* ompi_proc;
#endif

    MCA_PML_CM_HVY_RECV_REQUEST_ALLOC(recvreq);
    if( OPAL_UNLIKELY(NULL == recvreq) ) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PML_CM_HVY_RECV_REQUEST_INIT(recvreq, ompi_proc, comm, tag, src,
                                     datatype, addr, count, flags, true);

    *request = (ompi_request_t*) recvreq;

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_irecv(void *addr,
                 size_t count,
                 ompi_datatype_t * datatype,
                 int src,
                 int tag,
                 struct ompi_communicator_t *comm,
                 struct ompi_request_t **request)
{
    int ret;
    uint32_t flags = 0;
    mca_pml_cm_thin_recv_request_t *recvreq;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    ompi_proc_t* ompi_proc = NULL;
#endif

    if (OPAL_UNLIKELY(!mca_pml_cm_peer_wired(comm, src))) {
        return mca_pml_cm_irecv_slow(addr, count, datatype, src, tag, comm, request);
    }

    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq);
    if( OPAL_UNLIKELY(NULL == recvreq) ) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm,
                                      src,
                                      datatype,
                                      addr,
                                      count,
                                      flags);

    MCA_PML_CM_THIN_RECV_REQUEST_START(recvreq, comm, tag, src, ret);

    if( OPAL_LIKELY(OMPI_SUCCESS == ret) ) *request = (ompi_request_t*) recvreq;

    return ret;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_recv(void *addr,
                size_t count,
                ompi_datatype_t * datatype,
                int src,
                int tag,
                struct ompi_communicator_t *comm,
                ompi_status_public_t * status)
{
    int ret;
    uint32_t flags = 0;
    mca_pml_cm_thin_recv_request_t *recvreq;

    if (OPAL_UNLIKELY(!mca_pml_cm_peer_wired(comm, src))) {
        ret = mca_pml_cm_peer_wire_wait(comm, src);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
    }

    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq);
    if (OPAL_UNLIKELY(NULL == recvreq))
        return OMPI_ERR_OUT_OF_RESOURCE;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    ompi_proc_t *ompi_proc = NULL;
#endif

    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm,
                                      src,
                                      datatype,
                                      addr,
                                      count,
                                      flags);

    assert(NULL == recvreq->req_base.req_ompi.req_complete_cb);

    MCA_PML_CM_THIN_RECV_REQUEST_START(recvreq, comm, tag, src, ret);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        MCA_PML_CM_THIN_RECV_REQUEST_RETURN(recvreq);
        return ret;
    }

    ompi_request_wait_completion(&recvreq->req_base.req_ompi);

    if (MPI_STATUS_IGNORE != status) {
        OMPI_COPY_STATUS(status, recvreq->req_base.req_ompi.req_status, false);
    }

    ret = recvreq->req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free((ompi_request_t **) &recvreq);
    return ret;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_isend_init(const void* buf,
                        size_t count,
                        ompi_datatype_t* datatype,
                        int dst,
                        int tag,
                        mca_pml_base_send_mode_t sendmode,
                        ompi_communicator_t* comm,
                        ompi_request_t** request)
{
    mca_pml_cm_hvy_send_request_t *sendreq;
    uint32_t flags = 0;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    ompi_proc_t* ompi_proc;
#endif

    MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
    if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PML_CM_HVY_SEND_REQUEST_INIT(sendreq, ompi_proc, comm, tag, dst,
                                     datatype, sendmode, true, false, buf, count, flags);

    /* Work around a leak in start by marking this request as complete. The
     * problem occurred because we do not have a way to differentiate an
     * initial request and an incomplete pml request in start. This line
     * allows us to detect this state. */
    sendreq->req_send.req_base.req_pml_complete = true;

    *request = (ompi_request_t*) sendreq;

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_isend(const void* buf,
                   size_t count,
                   ompi_datatype_t* datatype,
                   int dst,
                   int tag,
                   mca_pml_base_send_mode_t sendmode,
                   ompi_communicator_t* comm,
                   ompi_request_t** request)
{
    int ret;
    uint32_t flags = 0;

    if (OPAL_UNLIKELY(!mca_pml_cm_peer_wired(comm, dst))) {
        return mca_pml_cm_isend_slow(buf, count, datatype, dst, tag, sendmode,
                                     comm, request);
    }

    if(sendmode == MCA_PML_BASE_SEND_BUFFERED ) {
        mca_pml_cm_hvy_send_request_t* sendreq;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        ompi_proc_t* ompi_proc = NULL;
#endif

        MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
        if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;

        MCA_PML_CM_HVY_SEND_REQUEST_INIT(sendreq,
                                         ompi_proc,
                                         comm,
                                         tag,
                                         dst,
                                         datatype,
                                         sendmode,
                                         false,
                                         false,
                                         buf,
                                         count,
                                         flags);

        MCA_PML_CM_HVY_SEND_REQUEST_START( sendreq, ret);

        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) *request = (ompi_request_t*) sendreq;

    } else {
        mca_pml_cm_thin_send_request_t* sendreq;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        ompi_proc_t* ompi_proc = NULL;
#endif
        MCA_PML_CM_THIN_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
        if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;

        MCA_PML_CM_THIN_SEND_REQUEST_INIT(sendreq,
                                          ompi_proc,
                                          comm,
                                          tag,
                                          dst,
                                          datatype,
                                          sendmode,
                                          buf,
                                          count,
                                          flags);

        MCA_PML_CM_THIN_SEND_REQUEST_START(
                                           sendreq,
                                           comm,
                                           tag,
                                           dst,
                                           sendmode,
                                           false,
                                           ret);

        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) *request = (ompi_request_t*) sendreq;

    }

    return ret;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_send(const void *buf,
                size_t count,
                ompi_datatype_t* datatype,
                int dst,
                int tag,
                mca_pml_base_send_mode_t sendmode,
                ompi_communicator_t* comm)
{
    int ret = OMPI_ERROR;
    uint32_t flags = 0;
    ompi_proc_t * ompi_proc;

    if (OPAL_UNLIKELY(!mca_pml_cm_peer_wired(comm, dst))) {
        ret = mca_pml_cm_peer_wire_wait(comm, dst);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        }
    }

    if(sendmode == MCA_PML_BASE_SEND_BUFFERED) {
        mca_pml_cm_hvy_send_request_t *sendreq;

        MCA_PML_CM_HVY_SEND_REQUEST_ALLOC(sendreq, comm, dst, ompi_proc);
        if (OPAL_UNLIKELY(NULL == sendreq)) return OMPI_ERR_OUT_OF_RESOURCE;

        MCA_PML_CM_HVY_SEND_REQUEST_INIT(sendreq,
                                         ompi_proc,
                                         comm,
                                         tag,
                                         dst,
                                         datatype,
                                         sendmode,
                                         false,
                                         false,
                                         buf,
                                         count,
                                         flags);
        MCA_PML_CM_HVY_SEND_REQUEST_START(sendreq, ret);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            MCA_PML_CM_HVY_SEND_REQUEST_RETURN(sendreq);
            return ret;
        }

        ompi_request_free( (ompi_request_t**)&sendreq );
    } else {
        opal_convertor_t convertor;
	OBJ_CONSTRUCT(&convertor, opal_convertor_t);
#if !(OPAL_ENABLE_HETEROGENEOUS_SUPPORT)
	if (opal_datatype_is_contiguous_memory_layout(&datatype->super, count)) {

		convertor.remoteArch = ompi_mpi_local_convertor->remoteArch;
		convertor.flags      = ompi_mpi_local_convertor->flags;
		convertor.master     = ompi_mpi_local_convertor->master;

                /* Switches off device detection if
                   MTL set MCA_MTL_BASE_FLAG_ACCELERATOR_INIT_DISABLE during init */
                MCA_PML_CM_SWITCH_ACCELERATOR_CONVERTOR_OFF(flags, datatype, count);
                convertor.flags      |= flags;
                /* Sets CONVERTOR_ACCELERATOR flag if device buffer */
                opal_convertor_prepare_for_send(&convertor, &datatype->super, count, (unsigned char *)buf);
    } else
#endif
	{
		ompi_proc = ompi_comm_peer_lookup(comm, dst);

                MCA_PML_CM_SWITCH_ACCELERATOR_CONVERTOR_OFF(flags, datatype, count);

		opal_convertor_copy_and_prepare_for_send(
		ompi_proc->super.proc_convertor,
			&datatype->super, count, buf, flags,
			&convertor);
	}

        ret = OMPI_MTL_CALL(send(ompi_mtl,
                                 comm,
                                 dst,
                                 tag,
                                 &convertor,
                                 sendmode));
	OBJ_DESTRUCT(&convertor);
    }

    return ret;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_iprobe(int src, int tag,
                   struct ompi_communicator_t *comm,
                   int *matched, ompi_status_public_t * status)
{
    if (OPAL_UNLIKELY(!mca_pml_cm_peer_wired(comm, src))) {
        /* Nothing can have been received from a peer we have not wired
         * yet, and "no match yet" is an answer a probe is allowed to
         * give. Wiring it here would make the next probe conclusive. */
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, src);
        int ret = (NULL == proc) ? OMPI_ERR_UNREACH : mca_pml_cm_ensure_proc(proc);

        if (OMPI_SUCCESS != ret) {
            *matched = 0;
            return (OMPI_ERR_NOT_READY == ret) ? OMPI_SUCCESS : ret;
        }
    }

    return OMPI_MTL_CALL(iprobe(ompi_mtl,
                                comm, src, tag,
                                matched, status));
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_probe(int src, int tag,
                  struct ompi_communicator_t *comm,
                  ompi_status_public_t * status)
{
    int ret, matched = 0;

    ret = mca_pml_cm_peer_wire_wait(comm, src);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    while (true) {
        ret = OMPI_MTL_CALL(iprobe(ompi_mtl,
                                   comm, src, tag,
                                   &matched, status));
        if (OMPI_SUCCESS != ret) break;
        if (matched) break;
        opal_progress();
    }

    return ret;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_improbe(int src,
                   int tag,
                   struct ompi_communicator_t* comm,
                   int *matched,
                   struct ompi_message_t **message,
                   ompi_status_public_t* status)
{
    if (OPAL_UNLIKELY(!mca_pml_cm_peer_wired(comm, src))) {
        ompi_proc_t *proc = ompi_comm_peer_lookup(comm, src);
        int ret = (NULL == proc) ? OMPI_ERR_UNREACH : mca_pml_cm_ensure_proc(proc);

        if (OMPI_SUCCESS != ret) {
            *matched = 0;
            *message = MPI_MESSAGE_NULL;
            return (OMPI_ERR_NOT_READY == ret) ? OMPI_SUCCESS : ret;
        }
    }

    return OMPI_MTL_CALL(improbe(ompi_mtl,
                                 comm, src, tag,
                                 matched, message,
                                 status));
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_mprobe(int src,
                  int tag,
                  struct ompi_communicator_t* comm,
                  struct ompi_message_t **message,
                  ompi_status_public_t* status)
{
    int ret, matched = 0;

    ret = mca_pml_cm_peer_wire_wait(comm, src);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    while (true) {
        ret = OMPI_MTL_CALL(improbe(ompi_mtl,
                                    comm, src, tag,
                                    &matched, message,
                                    status));
        if (OMPI_SUCCESS != ret) break;
        if (matched) break;
        opal_progress();
    }

    return ret;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_imrecv(void *buf,
                  size_t count,
                  ompi_datatype_t *datatype,
                  struct ompi_message_t **message,
                  struct ompi_request_t **request)
{
    int ret;
    uint32_t flags = 0;
    mca_pml_cm_thin_recv_request_t *recvreq;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    ompi_proc_t* ompi_proc;
#endif
    ompi_communicator_t *comm = (*message)->comm;

    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq);
    if( OPAL_UNLIKELY(NULL == recvreq) ) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm,
                                      (*message)->peer,
                                      datatype,
                                      buf,
                                      count,
                                      flags);

    MCA_PML_CM_THIN_RECV_REQUEST_MATCHED_START(recvreq, message, ret);

    if( OPAL_LIKELY(OMPI_SUCCESS == ret) ) *request = (ompi_request_t*) recvreq;

    return ret;
}

__opal_attribute_always_inline__ static inline int
mca_pml_cm_mrecv(void *buf,
                 size_t count,
                 ompi_datatype_t *datatype,
                 struct ompi_message_t **message,
                 ompi_status_public_t* status)
{
    int ret;
    uint32_t flags = 0;
    mca_pml_cm_thin_recv_request_t *recvreq;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    ompi_proc_t* ompi_proc;
#endif
    ompi_communicator_t *comm = (*message)->comm;

    MCA_PML_CM_THIN_RECV_REQUEST_ALLOC(recvreq);
    if( OPAL_UNLIKELY(NULL == recvreq) ) return OMPI_ERR_OUT_OF_RESOURCE;

    MCA_PML_CM_THIN_RECV_REQUEST_INIT(recvreq,
                                      ompi_proc,
                                      comm,
                                      (*message)->peer,
                                      datatype,
                                      buf,
                                      count,
                                      flags);

    MCA_PML_CM_THIN_RECV_REQUEST_MATCHED_START(recvreq,
                                               message, ret);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != ret) ) {
        MCA_PML_CM_THIN_RECV_REQUEST_RETURN(recvreq);
        return ret;
    }

    ompi_request_wait_completion(&recvreq->req_base.req_ompi);

    if (MPI_STATUS_IGNORE != status) {
        OMPI_COPY_STATUS(status, recvreq->req_base.req_ompi.req_status, false);
    }
    ret = recvreq->req_base.req_ompi.req_status.MPI_ERROR;
    ompi_request_free( (ompi_request_t**)&recvreq );

    return ret;
}

OMPI_DECLSPEC extern int mca_pml_cm_start(size_t count, ompi_request_t** requests);


OMPI_DECLSPEC extern int mca_pml_cm_dump(struct ompi_communicator_t* comm,
                                         int verbose);

OMPI_DECLSPEC extern int mca_pml_cm_cancel(struct ompi_request_t *request, int flag);

END_C_DECLS

#endif  /* PML_CM_H_HAS_BEEN_INCLUDED */
