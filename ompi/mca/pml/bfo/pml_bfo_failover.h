/*
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * Functions that implement failover capabilities.
 */
                                                                                                                                                 
#ifndef MCA_PML_BFO_FAILOVER_H
#define MCA_PML_BFO_FAILOVER_H

#include "ompi/mca/btl/btl.h"
#include "pml_bfo_hdr.h"

BEGIN_C_DECLS

bool mca_pml_bfo_is_duplicate_msg(mca_pml_bfo_comm_proc_t* proc,
                                  mca_pml_bfo_match_hdr_t *hdr);
bool mca_pml_bfo_is_duplicate_fin(mca_pml_bfo_hdr_t* hdr, mca_btl_base_descriptor_t* rdma,
                                  mca_btl_base_module_t* btl);

mca_pml_bfo_recv_request_t* mca_pml_bfo_get_request(mca_pml_bfo_match_hdr_t *hdr);

void mca_pml_bfo_send_request_restart(mca_pml_bfo_send_request_t* sendreq,
                                      bool repost, mca_btl_base_tag_t tag);
void mca_pml_bfo_send_request_rndvrestartnotify(mca_pml_bfo_send_request_t* sendreq,
                                      bool repost, mca_btl_base_tag_t tag, int status,
                                      mca_btl_base_module_t* btl);
void mca_pml_bfo_send_request_rndvrestartnack(mca_pml_bfo_send_request_t* sendreq);

void
mca_pml_bfo_rndvrestartnotify_completion(mca_btl_base_module_t* btl,
                                         struct mca_btl_base_endpoint_t* ep,
                                         struct mca_btl_base_descriptor_t* des,
                                         int status);
void
mca_pml_bfo_check_recv_ctl_completion_status(mca_btl_base_module_t* btl,
                                             struct mca_btl_base_descriptor_t* des,
                                             int status);

/* Reset a receive request to the beginning */
void mca_pml_bfo_recv_request_reset(mca_pml_bfo_recv_request_t* recvreq);
/* Notify sender that receiver detected an error */
void mca_pml_bfo_recv_request_recverrnotify(mca_pml_bfo_recv_request_t* recvreq,
                                            mca_btl_base_tag_t tag, int status);
/* Ack the RNDVRESTARTNOTIFY message */
void mca_pml_bfo_recv_request_rndvrestartack(mca_pml_bfo_recv_request_t* recvreq,
                                             mca_btl_base_tag_t tag, int status,
                                             mca_btl_base_module_t* btl);
/* Nack the RNDVRESTARTNOTIFY message */
void mca_pml_bfo_recv_request_rndvrestartnack(mca_btl_base_descriptor_t* olddes,
                                              ompi_proc_t* ompi_proc, bool repost);

void mca_pml_bfo_recv_restart_completion(mca_btl_base_module_t* btl,
                                         struct mca_btl_base_endpoint_t* ep,
                                         struct mca_btl_base_descriptor_t* des,
                                         int status);
void mca_pml_bfo_failover_error_handler(struct mca_btl_base_module_t* btl,
                                        int32_t flags, ompi_proc_t *errproc, char *btlname);
void mca_pml_bfo_repost_match_fragment(struct mca_btl_base_descriptor_t* des);
void mca_pml_bfo_repost_fin(struct mca_btl_base_descriptor_t* des);

void mca_pml_bfo_map_out_btl(struct mca_btl_base_module_t* btl,
                             ompi_proc_t *errproc, char *btlname);

extern void mca_pml_bfo_map_out( mca_btl_base_module_t *btl,
                                 mca_btl_base_tag_t tag,
                                 mca_btl_base_descriptor_t* descriptor,
                                 void* cbdata );

int mca_pml_bfo_register_callbacks(void);

void mca_pml_bfo_update_rndv_fields(mca_pml_bfo_hdr_t* hdr,
                                    mca_pml_bfo_send_request_t*, char *type);
 


/**
 * Four new callbacks for the four new message types.
 */
extern void mca_pml_bfo_recv_frag_callback_rndvrestartnotify( mca_btl_base_module_t *btl,
                                                              mca_btl_base_tag_t tag,
                                                              mca_btl_base_descriptor_t* descriptor,
                                                              void* cbdata );

extern void mca_pml_bfo_recv_frag_callback_rndvrestartack( mca_btl_base_module_t *btl,
                                                           mca_btl_base_tag_t tag,
                                                           mca_btl_base_descriptor_t* descriptor,
                                                           void* cbdata );

extern void mca_pml_bfo_recv_frag_callback_rndvrestartnack( mca_btl_base_module_t *btl,
                                                            mca_btl_base_tag_t tag,
                                                            mca_btl_base_descriptor_t* descriptor,
                                                            void* cbdata );

extern void mca_pml_bfo_recv_frag_callback_recverrnotify( mca_btl_base_module_t *btl,
                                                          mca_btl_base_tag_t tag,
                                                          mca_btl_base_descriptor_t* descriptor,
                                                          void* cbdata );

/**
 * A bunch of macros to help isolate failover code from regular ob1 code.
 */

/* Drop any ACK fragments if request is in error state.  Do not want
 * to initiate any more activity. */
#define MCA_PML_BFO_ERROR_CHECK_ON_ACK_CALLBACK(sendreq)                          \
    if( OPAL_UNLIKELY((sendreq)->req_error)) {                                    \
         opal_output_verbose(20, mca_pml_bfo_output,                              \
                             "ACK: received: dropping because request in error, " \
                             "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",   \
                             (uint16_t)(sendreq)->req_send.req_base.req_sequence, \
                             (sendreq)->req_restartseq,                           \
                             (void *)(sendreq), (sendreq)->req_recv.pval,         \
                             (sendreq)->req_send.req_base.req_peer);              \
        return;                                                                   \
    }

/* Drop any FRAG fragments if request is in error state.  Do not want
 * to initiate any more activity. */
#define MCA_PML_BFO_ERROR_CHECK_ON_FRAG_CALLBACK(recvreq)                                \
    if( OPAL_UNLIKELY((recvreq)->req_errstate)) {                                        \
        opal_output_verbose(20, mca_pml_bfo_output,                                      \
                            "FRAG: received: dropping because request in error, "        \
                            "PML=%d, src_req=%p, dst_req=%p, peer=%d, offset=%d",        \
                            (uint16_t)(recvreq)->req_msgseq,                             \
                            (recvreq)->remote_req_send.pval,                             \
                            (void *)(recvreq),                                           \
                            (recvreq)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE, \
                            (int)hdr->hdr_frag.hdr_frag_offset);                         \
        return;                                                                          \
    }

/* Drop any PUT fragments if request is in error state.  Do not want
 * to initiate any more activity. */
#define MCA_PML_BFO_ERROR_CHECK_ON_PUT_CALLBACK(sendreq)                          \
    if( OPAL_UNLIKELY((sendreq)->req_error)) {                                    \
         opal_output_verbose(20, mca_pml_bfo_output,                              \
                             "PUT: received: dropping because request in error, " \
                             "PML=%d, src_req=%p, dst_req=%p, peer=%d",           \
                             (uint16_t)(sendreq)->req_send.req_base.req_sequence, \
                             (void *)(sendreq), (sendreq)->req_recv.pval,         \
                             (sendreq)->req_send.req_base.req_peer);              \
        return;                                                                   \
    }

/**
 * Macros for pml_bfo_recvreq.c file.
 */

/* This can happen if a FIN message arrives after the request was
 * marked in error.  So, just drop the message.  Note that the status
 * field is not being checked.  That is because the status field is the
 * value returned in the FIN hdr.hdr_fail field and may be used for
 * other things.  Note that we allow the various fields to be updated
 * in case this actually completes the request and the sending side
 * thinks it is done. */
#define MCA_PML_BFO_ERROR_CHECK_ON_FIN_FOR_PUT(recvreq)                                   \
    if( OPAL_UNLIKELY((recvreq)->req_errstate)) {                                         \
        opal_output_verbose(20, mca_pml_bfo_output,                                       \
                            "FIN: received on broken request, skipping, "                 \
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",            \
                            (recvreq)->req_msgseq, (recvreq)->req_restartseq,             \
                            (recvreq)->remote_req_send.pval, (void *)(recvreq),           \
                            (recvreq)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE); \
        /* Even though in error, it still might complete.  */                             \
        recv_request_pml_complete_check(recvreq);                                         \
        return;                                                                           \
    }

#define MCA_PML_BFO_ERROR_CHECK_ON_RDMA_READ_COMPLETION(recvreq)                            \
    if ((recvreq)->req_errstate) {                                                          \
        opal_output_verbose(30, mca_pml_bfo_output,                                         \
			    "RDMA read: completion failed, error already seen, "            \
			    "PML=%d, RQS=%d, src_req=%lx, dst_req=%lx, peer=%d",            \
			    (recvreq)->req_msgseq, (recvreq)->req_restartseq,               \
			    (unsigned long)(recvreq)->remote_req_send.pval,                 \
			    (unsigned long)(recvreq),                                       \
			    (recvreq)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);   \
	return;                                                                             \
    } else {                                                                                \
	opal_output_verbose(30, mca_pml_bfo_output,                                         \
			    "RDMA read: completion failed, sending RECVERRNOTIFY to "       \
			    "sender, PML=%d, RQS=%d, src_req=%lx, dst_req=%lx, peer=%d",    \
			    (recvreq)->req_msgseq, (recvreq)->req_restartseq,               \
			    (unsigned long)(recvreq)->remote_req_send.pval,                 \
			    (unsigned long)(recvreq),                                       \
			    (recvreq)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);   \
	mca_pml_bfo_recv_request_recverrnotify(recvreq, MCA_PML_BFO_HDR_TYPE_RGET, status); \
    }

#define MCA_PML_BFO_SECOND_ERROR_CHECK_ON_RDMA_READ_COMPLETION(recvreq, status, btl)        \
    /* See if the request has received a RNDVRESTARTNOTIFY */                               \
    if( OPAL_UNLIKELY(recvreq->req_errstate)) {                                             \
        if (recvreq->req_errstate & RECVREQ_RNDVRESTART_RECVED) {                           \
            opal_output_verbose(30, mca_pml_bfo_output,                                     \
                                "RDMA read: completion: recvreq has error, outstanding events=%d " \
                                "PML=%d, RQS=%d, src_req=%lx, dst_req=%lx, status=%d, peer=%d",    \
                                recvreq->req_events, recvreq->req_msgseq, recvreq->req_restartseq, \
                                (unsigned long)recvreq->remote_req_send.pval,               \
                                (unsigned long)recvreq, status,                             \
                                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE); \
            if (0 == recvreq->req_events) {                                                 \
                mca_pml_bfo_recv_request_rndvrestartack(recvreq, MCA_PML_BFO_HDR_TYPE_RGET, \
                                                        status, btl);                       \
            }                                                                               \
        }                                                                                   \
        MCA_PML_BFO_RDMA_FRAG_RETURN(frag);                                                 \
        return;                                                                             \
    }


/**
 * Macros for pml_bfo_sendreq.c file.
 */

/* The completion event for the RNDV message has returned with an
 * error. We know that the send request we are looking at is valid
 * because it cannot be completed until the sendreq->req_state value
 * reaches 0.  And for the sendreq->req_state to reach 0, the
 * completion event on the RNDV message must occur.  So, we do not
 * bother checking whether the send request is valid, because we know
 * it is, but we put a few asserts in for good measure.  We then check
 * a few fields in the request to decide what to do.  If the
 * sendreq->req_error is set, that means that something has happend
 * already to the request and we do not want to restart it.
 * Presumably, we may have received a RECVERRNOTIFY message from the
 * receiver.  We also check the sendreq->req_acked field to see if it
 * has been acked.  If it has, then again we do not restart everything
 * because obviously the RNDV message has made it to the other side. */
#define MCA_PML_BFO_ERROR_ON_RNDV_COMPLETION(sendreq, des)                             \
do {                                                                                   \
    assert(((mca_pml_bfo_hdr_t*)((des)->des_src->seg_addr.pval))->hdr_match.hdr_ctx == \
           (sendreq)->req_send.req_base.req_comm->c_contextid);                        \
    assert(((mca_pml_bfo_hdr_t*)((des)->des_src->seg_addr.pval))->hdr_match.hdr_src == \
           (sendreq)->req_send.req_base.req_comm->c_my_rank);                          \
    assert(((mca_pml_bfo_hdr_t*)((des)->des_src->seg_addr.pval))->hdr_match.hdr_seq == \
           (uint16_t)(sendreq)->req_send.req_base.req_sequence);                       \
    if ((!(sendreq)->req_error) && (!(sendreq)->req_acked)) {                          \
        (sendreq)->req_events--;                                                       \
        /* Assume RNDV did not make it, so restart from the beginning. */              \
        mca_pml_bfo_send_request_restart(sendreq, true, MCA_PML_BFO_HDR_TYPE_RNDV);    \
        return;                                                                        \
    }                                                                                  \
} while (0)

/* Now check the error state.  This request can be in error if the
 * RNDV message made it over, but the receiver got an error trying to
 * send the ACK back and therefore sent a RECVERRNOTIFY message.  In
 * that case, we want to start the restart dance as the receiver has
 * matched this message already.  Only restart if there are no
 * outstanding events on send request. */
#define MCA_PML_BFO_CHECK_SENDREQ_ERROR_ON_RNDV_COMPLETION(sendreq, status, btl)            \
    if ((sendreq)->req_error) {                                                             \
        opal_output_verbose(30, mca_pml_bfo_output,                                         \
                            "RNDV: completion: sendreq has error, outstanding events=%d, "  \
                            "PML=%d, RQS=%d, src_req=%lx, dst_req=%lx, status=%d, peer=%d", \
                            (sendreq)->req_events,                                          \
                            (uint16_t)(sendreq)->req_send.req_base.req_sequence,            \
                            (sendreq)->req_restartseq, (unsigned long)(sendreq),            \
                            (unsigned long)(sendreq)->req_recv.pval,                        \
                            status, (sendreq)->req_send.req_base.req_peer);                 \
        if (0 == (sendreq)->req_events) {                                                   \
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,                      \
                                                       MCA_PML_BFO_HDR_TYPE_RNDV,           \
                                                       status, btl);                        \
        }                                                                                   \
        return;                                                                             \
    }

/* This can happen if a FIN message arrives after the request was
 * marked in error.  So, just drop the message.  Note that the status
 * field is not checked here.  That is because that is the value
 * returned in the FIN hdr.hdr_fail field and may be used for other
 * things. */
#define MCA_PML_BFO_CHECK_SENDREQ_ERROR_ON_RGET_COMPLETION(sendreq, btl, des)              \
    if( OPAL_UNLIKELY(sendreq->req_error)) {                                               \
        opal_output_verbose(30, mca_pml_bfo_output,                                        \
                            "FIN: received on broken request, skipping, "                  \
                            "PML=%d, src_req=%lx, dst_req=%lx, peer=%d",                   \
                            (uint16_t)sendreq->req_send.req_base.req_sequence,             \
                            (unsigned long)sendreq, (unsigned long)sendreq->req_recv.pval, \
                            sendreq->req_send.req_base.req_peer);                          \
        btl->btl_free(btl, des);                                                           \
        return;                                                                            \
    }

/* If we get an error on the RGET message, then first make sure that
 * header matches the send request that we are pointing to.  This is
 * necessary, because even though the sending side got an error, the
 * RGET may have made it to the receiving side and the message transfer
 * may have completed.  This would then mean the send request has been
 * completed and perhaps in use by another communication.  So there is
 * no need to restart this request.  Therefore, ensure that we are
 * looking at the same request that the header thinks we are looking
 * at.  If not, then there is nothing else to be done. */
#define MCA_PML_BFO_ERROR_ON_SEND_CTL_COMPLETION(sendreq, des)                               \
do {                                                                                         \
    mca_pml_bfo_hdr_t* hdr = des->des_src->seg_addr.pval;                                    \
    mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)des->des_cbdata;      \
    switch (hdr->hdr_common.hdr_type) {                                                      \
    case MCA_PML_BFO_HDR_TYPE_RGET:                                                          \
        if ((hdr->hdr_match.hdr_ctx != sendreq->req_send.req_base.req_comm->c_contextid) ||  \
            (hdr->hdr_match.hdr_src != sendreq->req_send.req_base.req_comm->c_my_rank) ||    \
            (hdr->hdr_match.hdr_seq != (uint16_t)sendreq->req_send.req_base.req_sequence)) { \
            opal_output_verbose(30, mca_pml_bfo_output,                                      \
                                "RGET: completion event: dropping because no valid request " \
                                "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d "     \
                                "RQS:exp=%d,act=%d, dst_req=%p",                             \
                                (uint16_t)sendreq->req_send.req_base.req_sequence,           \
                                hdr->hdr_match.hdr_seq,                                      \
                                sendreq->req_send.req_base.req_comm->c_contextid,            \
                                hdr->hdr_match.hdr_ctx,                                      \
                                sendreq->req_send.req_base.req_comm->c_my_rank,              \
                                hdr->hdr_match.hdr_src,                                      \
                                sendreq->req_restartseq, hdr->hdr_rndv.hdr_restartseq,       \
                                (void *)sendreq);                                            \
            return;                                                                          \
        }                                                                                    \
        mca_pml_bfo_send_request_restart(sendreq, true, MCA_PML_BFO_HDR_TYPE_RGET);          \
        return;                                                                              \
    default:                                                                                 \
        opal_output(0, "%s:%d FATAL ERROR, unknown header (hdr=%d)",                         \
                    __FILE__, __LINE__, hdr->hdr_common.hdr_type);                           \
        orte_errmgr.abort(-1, NULL);                                                         \
    }                                                                                        \
} while (0)

/* Check if there has been an error on the send request when we get
 * a completion event on the RDMA write. */
#define MCA_PML_BFO_CHECK_SENDREQ_ERROR_ON_PUT_COMPLETION(sendreq, status, btl)                 \
    if ( OPAL_UNLIKELY(sendreq->req_error)) {                                                   \
        opal_output_verbose(30, mca_pml_bfo_output,                                             \
                            "RDMA write: completion: sendreq has error, outstanding events=%d," \
                            " PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, peer=%d",      \
                            sendreq->req_events,                                                \
                            (uint16_t)sendreq->req_send.req_base.req_sequence,                  \
                            sendreq->req_restartseq, (void *)sendreq,                           \
                            sendreq->req_recv.pval,                                             \
                            status, sendreq->req_send.req_base.req_peer);                       \
        if (0 == sendreq->req_events) {                                                         \
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,                          \
                                                       MCA_PML_BFO_HDR_TYPE_PUT,                \
                                                       status, btl);                            \
        }                                                                                       \
        MCA_PML_BFO_RDMA_FRAG_RETURN(frag);                                                     \
        return;                                                                                 \
    }

#define MCA_PML_BFO_CHECK_FOR_RNDV_RESTART(hdr, sendreq, type)  \
    if (0 < sendreq->req_restartseq) {                          \
        mca_pml_bfo_update_rndv_fields(hdr, sendreq, type);     \
    }


END_C_DECLS

#endif
