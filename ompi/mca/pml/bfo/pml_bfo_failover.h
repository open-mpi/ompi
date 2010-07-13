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
                                              

END_C_DECLS

#endif
