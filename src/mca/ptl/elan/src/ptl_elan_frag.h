/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef _MCA_PTL_ELAN_FRAG_H
#define _MCA_PTL_ELAN_FRAG_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_elan.h"

struct mca_ptl_elan_peer_t;
struct ompi_ptl_elan_base_desc_t;

struct mca_ptl_elan_send_frag_t {
    mca_ptl_base_frag_t frag_base;  
    volatile int    frag_progressed;
    bool            frag_ack_pending;       /* Is there an ack to recv */
    struct ompi_ptl_elan_base_desc_t *desc;
};
typedef struct mca_ptl_elan_send_frag_t mca_ptl_elan_send_frag_t;

/**
 *  ELAN received fragment derived type.
 */
struct mca_ptl_elan_recv_frag_t {
    mca_ptl_base_recv_frag_t frag_recv; 
    size_t          frag_hdr_cnt;
    size_t          frag_msg_cnt; 
    volatile int    frag_progressed;        /* Is it record to request */
    bool            frag_ack_pending;       /* Is there an ack to send */
#if 0
    union {
       struct ompi_ptl_elan_qdma_desc_t   *qdma;
       struct ompi_ptl_elan_putget_desc_t *putget;
    } frag;
#endif
    char           *alloc_buff;
    char           *unex_buff;
};
typedef struct mca_ptl_elan_recv_frag_t mca_ptl_elan_recv_frag_t;

extern ompi_class_t mca_ptl_elan_send_frag_t_class;
extern ompi_class_t mca_ptl_elan_recv_frag_t_class;

mca_ptl_elan_send_frag_t *
mca_ptl_elan_alloc_desc(struct mca_ptl_base_module_t *ptl,
			struct mca_pml_base_request_t *req,
		       	int desc_type);

mca_ptl_elan_recv_frag_t *
mca_ptl_elan_alloc_recv_desc(struct mca_pml_base_recv_request_t *req);

/**
 * ELAN send request derived type. The send request contains 
 * the base send request and a point to the elan fragment descriptor
 */
struct mca_ptl_elan_send_request_t {
    mca_pml_base_send_request_t super;
    mca_ptl_elan_send_frag_t *req_frag; 
};
typedef struct mca_ptl_elan_send_request_t mca_ptl_elan_send_request_t;

void 
mca_ptl_elan_send_desc_done (
       	mca_ptl_elan_send_frag_t *desc,
       	mca_pml_base_send_request_t *req);
 
void 
mca_ptl_elan_recv_frag_done (
       	mca_ptl_base_header_t *header,
       	mca_ptl_elan_recv_frag_t* frag,
       	mca_pml_base_recv_request_t *request);
#endif
