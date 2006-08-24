#ifndef __IB_H__
#define __IB_H__

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <evapi.h>
#include <vapi.h>
#include <vapi_common.h>
#include <mpi.h>

#include "dict.h"

#define IB_OK 0
#define IB_OOR 1
#define IB_CONTINUE 2

#define IB_RTR_SIZE 320 /* number of RTR buffers per peer */
#define IB_EAGER_SIZE 160 /* number of eager buffers per peer */
#define IB_EAGER_LIMIT 8247 /* equals 8192 + 64 - 9 (tag, index, size, flag) to ensure 64 byte alignment */

typedef struct {
  u_int64_t r_key; /* r_key of peer - TODO: should be u_int32_t ...*/
  u_int64_t addr; /* addr of peer - TODO: could be u_int32_t ... */
} IB_Peer_info;

typedef struct {
  u_int64_t r_key; /* r_key of peer - TODO: should be u_int32_t ... */
  u_int64_t addr; /* addr of peer - TODO: could be u_int32_t ... */
  int32_t tag; /* tag has to be at the end (we do not send with immediate, so tag indicates receive -> could be dangerous) */
} IB_Peer_info_tag;

typedef struct {
  VAPI_mr_hndl_t mr_hndl_p; /* memroy region handle - should really be a pointer to a memlist-element  ... */
  VAPI_sg_lst_entry_t sr_sg_lst; /* the IB SG list */
  VAPI_sr_desc_t sr_desc; /* the IB SR descr. */
  int p; /* how many procs (RCQ, SCQ, QP) are in this comm */
  struct struct_IB_Comminfo *comminfo; /* the communicator info struct */
  int tag; /* our tag */
  int rank; /* our rank */
  int peer; /* the peer (dst for send, src for recv */
  void *buf; /* we need the buf for eager messages on the receiver side */
  int sendel; /* the element in comminfo.send[element] which is use by this request to send RTR or eager messages - we want to free it after sending RTR/eager */
  enum {FREE, SEND_WAITING_RTR, SEND_POSTED_SR, SEND_SENT_EAGER, RECV_SENDING_RTR, RECV_SENT_RTR, RECV_WAITING_EAGER, SEND_DONE, RECV_DONE, RECV_EAGER_DONE} status; /* this indicates the operation (send,recv) and the status of this op */
} IB_Req;

typedef IB_Req* IB_Request;

struct struct_IB_Taglstel {
  int tag; /* the tag -> key element */
  IB_Req *req; /* the request having this tag */
};
typedef struct struct_IB_Taglstel IB_Taglstel;

struct struct_IB_Memlistel {
  void *buf;
  int size;
  VAPI_mr_hndl_t *mr;
  VAPI_rkey_t r_key;
  VAPI_lkey_t l_key;
};
typedef struct struct_IB_Memlistel IB_Memlistel;

typedef struct {
  int16_t index; /* the index in my free-array on the sender side - I should RDMA a -1 into this array to indicate the receiption */
  int16_t size; /* the actual size of the message, after this size follows a flag (single byte) to poll on receiption */
  int32_t tag; /* the message tag */
  int8_t buf[IB_EAGER_LIMIT]; /* the data buffer - should be chosen that the whole structure size is 64 byte aligned */
  int8_t flag; /* the flag to poll for completion - only if buffer is full */
} IB_Eager_data;

struct struct_IB_Comminfo {
  VAPI_qp_hndl_t *qp_hndl_arr; /* QPs to all ranks in this comm */
  VAPI_cq_hndl_t *sr_cq_hndl_arr; /* SR CQs for all ranks in this comm */
  VAPI_cq_hndl_t *rr_cq_hndl_arr; /* RR CQs for all ranks in this comm */
  
  /* the old crappy linear taglist */
  IB_Taglstel *taglisthead; /* the comm specific taglist */
  IB_Taglstel *taglistend; /* the end pointer of the taglist */

  hb_tree **taglist; /* the new fancy AVL tree taglists - one for each peer (2-d matching is not possible :-(, so the peer-dimension is like a hash-table O(1) :) ) */
  
  IB_Peer_info_tag *rtr_send; /* send queue for me (only for RTR) - IB_RTR_SIZE elements */
  VAPI_mr_hndl_t rtr_send_mr_hndl_p; /* memory region handle - needed to free MR ... */
  VAPI_lkey_t rtr_send_l_key; /* l_key for send */
  
  VAPI_mr_hndl_t *rtr_mr_hndl_p; /* memory region handles per proc - needed to free MR ... */
  IB_Peer_info *rtr_info; /* rtr r_key, addr for each host */
  VAPI_lkey_t *rtr_l_key; /* l_key for rtr region */
  volatile IB_Peer_info_tag **rtr; /* rtr queue for each host - IB_RTR_SIZE elements  */
  int **rtr_peer_free; /* indicates which elements are free at the specific peer (-1 means free, > -1 means filled with tag x */
  
  IB_Eager_data *eager_send; /* eager send queue for me (only for eager) - IB_EAGER_SIZE elements */
  VAPI_mr_hndl_t eager_send_mr_hndl_p; /* memory region handle - needed to free MR ... */
  VAPI_lkey_t eager_send_l_key; /* l_key for eager send */
  
  VAPI_mr_hndl_t *eager_mr_hndl_p; /* memory region handles per peer - needed to free MR ... */
  IB_Peer_info *eager_info; /* rtr r_key, addr for each host */
  VAPI_lkey_t *eager_l_key; /* l_key for rtr region per peer */
  volatile IB_Eager_data **eager; /* rtr queue for each host - IB_RTR_SIZE elements  */
  int **eager_peer_free; /* indicates which elements are free at the specific peer (-1 means free, > -1 means filled with tag x */
  
  int empty; /* this is just a buffer to RDMA into eager_peer_free on the sender */
  VAPI_mr_hndl_t *eager_free_mr_hndl_p; /* memory region handles per peer - needed to free MR ... */
  IB_Peer_info *eager_free_info; /* rtr r_key, addr for each host to RDMA EAGER_DONE */
  VAPI_mr_hndl_t empty_mr_hndl_p; /* memory region handle - needed to free MR ... */
  VAPI_lkey_t empty_l_key; /* l_key for empty int */
};
typedef struct struct_IB_Comminfo IB_Comminfo;

/* function prototypes ... */
int IB_Testall(int count, IB_Request *requests, int *flag);
int IB_Waitall(int count, IB_Request *requests);
int IB_Wait(IB_Request *req);
int IB_Test(IB_Request *req);
int IB_Isend(void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm mycomm, IB_Request *request);
int IB_Irecv(void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm mycomm, IB_Request *request);

#endif
