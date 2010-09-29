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


END_C_DECLS

#endif
