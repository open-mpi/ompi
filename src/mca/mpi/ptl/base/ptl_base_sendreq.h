/*
 * $HEADER$
 */

#ifndef MCA_PML_BASE_SEND_REQUEST_H
#define MCA_PML_BASE_SEND_REQUEST_H

#include "lam_config.h"

#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/pml/base/pml_base_request.h"


extern lam_class_info_t mca_ptl_base_send_request_cls;


struct mca_ptl_base_send_request_t {
    /* request object - common data structure for use by wait/test */
    mca_pml_base_request_t super;
    /* pointer to user data */
    unsigned char *req_data;
    /* size of send/recv in bytes */
    size_t req_length;
    /* number of bytes that have already been assigned to a fragment */
    size_t req_offset;
    /* number of fragments that have been allocated */
    size_t req_frags;
    /* number of bytes that have been sent */
    size_t req_bytes_sent;
    /* number of bytes that have been acked */
    size_t req_bytes_acked;
    /* clear to send flag */
    bool req_clear_to_send;
    /* type of send */
    mca_pml_base_send_mode_t req_send_mode;
    /* sequence number for MPI pt-2-pt ordering */
    mca_ptl_base_sequence_t req_msg_sequence_number;
    /* queue of fragments that are waiting to be acknowledged */
    mca_ptl_base_queue_t req_unacked_frags;
    /* PTL that allocated this descriptor */
    struct mca_ptl_proc_t* req_owner;
};
typedef struct mca_ptl_base_send_request_t mca_ptl_base_send_request_t;


void mca_ptl_base_send_request_init(mca_ptl_base_send_request_t*);
void mca_ptl_base_send_request_destroy(mca_ptl_base_send_request_t*);


static inline void mca_ptl_base_send_request_reinit(
    mca_ptl_base_send_request_t *request, 
    void *addr, 
    size_t length, 
    lam_datatype_t* datatype, 
    int peer, 
    int tag, 
    lam_communicator_t* comm, 
    mca_pml_base_send_mode_t sendmode,
    bool persistent) 
{ 
    request->req_clear_to_send = false; 
    request->req_offset = 0; 
    request->req_frags = 0; 
    request->req_bytes_sent = 0; 
    request->req_bytes_acked = 0; 
    request->req_send_mode = sendmode;
    request->super.req_addr = addr; 
    request->super.req_length = length; 
    request->super.req_datatype = datatype; 
    request->super.req_peer = peer; 
    request->super.req_tag = tag; 
    request->super.req_communicator = comm; 
    request->super.req_type = MCA_PML_REQUEST_SEND; 
    request->super.req_persistent = persistent; 
    request->super.req_mpi_done = false; 
    request->super.req_pml_layer_done = false; 
}


#endif

