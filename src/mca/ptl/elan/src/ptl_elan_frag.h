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

extern ompi_class_t mca_ptl_elan_recv_frag_t_class;

struct mca_ptl_elan_peer_t;
struct ompi_ptl_elan_base_desc_t;

struct mca_ptl_elan_desc_item_t {
    ompi_list_item_t   super;
    struct ompi_ptl_elan_base_desc_t *desc;
};
typedef struct mca_ptl_elan_desc_item_t mca_ptl_elan_desc_item_t;

/**
 *  ELAN received fragment derived type.
 */
struct mca_ptl_elan_recv_frag_t {
    mca_ptl_base_recv_frag_t super; 
    size_t          frag_hdr_cnt;  
    size_t          frag_msg_cnt; 
    int             frag_progressed;
    union {
       struct ompi_ptl_elan_qdma_frag_t   *qdma;
       struct ompi_ptl_elan_putget_frag_t *putget;
    } frag;
    char           *alloc_buff;
    char           *unex_buff;
};
typedef struct mca_ptl_elan_recv_frag_t mca_ptl_elan_recv_frag_t;

mca_ptl_elan_desc_item_t *
mca_ptl_elan_alloc_send_desc( struct mca_ptl_t *ptl,
                  struct mca_pml_base_send_request_t *sendreq);

mca_ptl_elan_recv_frag_t *
mca_ptl_elan_alloc_recv_desc(struct mca_pml_base_recv_request_t *req);

static inline void 
mca_ptl_elan_recv_frag_progress(mca_ptl_elan_recv_frag_t* frag) 
{ 
    /* Upto this point, this only means the fragment has been 
       matched with a posted receive descriptor */
    if (fetchNset (&frag->frag_progressed, 1) == 0) {
#if 1
	/* make sure this only happens once for threaded case */ 
	mca_pml_base_recv_request_t* request;
	mca_ptl_base_recv_progress_fn_t  progress;

	progress = (frag)->super.super.frag_owner->ptl_recv_progress;
	request = (frag)->super.frag_request; 
	
	/* progress the request */ 
	progress((frag)->super.super.frag_owner, request, &(frag)->super); 
	mca_ptl_elan_recv_frag_return((frag)->super.super.frag_owner, (frag)); 
#endif
    }

}


#endif
