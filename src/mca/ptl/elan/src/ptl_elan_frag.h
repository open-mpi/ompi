/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_ELAN_FRAG_H
#define MCA_PTL_ELAN_FRAG_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "ompi_config.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_elan.h"

extern ompi_class_t mca_ptl_elan_send_frag_t_class;
struct mca_ptl_base_peer_t;

/**
 * ELAN send fragment derived type.
 */
struct mca_ptl_elan_send_frag_t {
   mca_ptl_base_send_frag_t super;  /**< base send fragment descriptor */
   size_t frag_vec_cnt;             
   volatile int frag_progressed;   
};
typedef struct mca_ptl_elan_send_frag_t mca_ptl_elan_send_frag_t;


#define MCA_PTL_ELAN_SEND_FRAG_ALLOC(item, rc)  \
    OMPI_FREE_LIST_GET(&mca_ptl_elan_module.elan_send_frags, item, rc);


#define MCA_PTL_ELAN_RECV_FRAG_ALLOC(frag, rc) \
{ \
	ompi_list_item_t* item; \
	OMPI_FREE_LIST_GET(&mca_ptl_elan_module.elan_recv_frags, item, rc); \
	frag = (mca_ptl_elan_recv_frag_t*)item; \
}

bool 
mca_ptl_elan_send_frag_handler (mca_ptl_elan_send_frag_t *, int sd);


int 
mca_ptl_elan_send_frag_init (mca_ptl_elan_send_frag_t *,
                             struct mca_ptl_base_peer_t *,
                             struct mca_ptl_base_send_request_t *,
                             size_t offset, size_t * size, int flags);

extern ompi_class_t mca_ptl_elan_recv_frag_t_class;

/**
 *  ELAN received fragment derived type.
 */
struct mca_ptl_elan_recv_frag_t {
    mca_ptl_base_recv_frag_t super; /**< base receive fragment descriptor */
    size_t          frag_hdr_cnt;   /**< number of header bytes received */
    size_t          frag_msg_cnt;   /**< number of message bytes received */
    bool            frag_ack_pending; /**< an ack pending for this fragment */
    volatile int    frag_progressed; /**< flag to atomically progress */
};
typedef struct mca_ptl_elan_recv_frag_t mca_ptl_elan_recv_frag_t;

bool 
mca_ptl_elan_recv_frag_handler (mca_ptl_elan_recv_frag_t *, int sd);

void 
mca_ptl_elan_recv_frag_init (mca_ptl_elan_recv_frag_t * frag,
                              struct mca_ptl_base_peer_t *peer);

bool 
mca_ptl_elan_recv_frag_send_ack (mca_ptl_elan_recv_frag_t * frag); 

/*
 * For fragments that require an acknowledgment, this routine will be called
 * twice, once when the send completes, and again when the acknowledgment is 
 * returned. Only the last caller should update the request status, so we
 * add a lock w/ the frag_progressed flag.
 */
static inline void
mca_ptl_elan_send_frag_progress (mca_ptl_elan_send_frag_t * frag)
{
    mca_ptl_base_send_request_t *request = frag->super.frag_request;

    /* if this is an ack - simply return to pool */
    if (request == NULL) {
        mca_ptl_elan_send_frag_return (frag->super.super.frag_owner, frag);

    } else if (frag->frag_vec_cnt == 0 &&
               ((frag->super.super.frag_header.hdr_common.hdr_flags
                 & MCA_PTL_FLAGS_ACK_MATCHED) == 0 ||
                mca_ptl_base_send_request_matched (request))) {

        /* otherwise, if the message has been sent, and an ack has 
         * already been received, go ahead and update the request status 
         */

        /* make sure this only happens once in threaded case */
        if (ompi_using_threads ()
            && fetchNset (&frag->frag_progressed, 1) == 1) {
            return;
        } else {
            frag->frag_progressed = 1;
        }

        /* update request status */
        frag->super.super.frag_owner->ptl_send_progress (request,
                                                         &frag->super);

        /* the first fragment is allocated with the request, 
         * all others need to be returned to free list  
         */
        if (frag->super.super.frag_header.hdr_frag.hdr_frag_offset != 0)
            mca_ptl_elan_send_frag_return (frag->super.super.frag_owner,
                                           frag);
    }
}

static inline void
mca_ptl_elan_send_frag_init_ack (mca_ptl_elan_send_frag_t * ack,
                                 struct mca_ptl_t *ptl,
                                 struct mca_ptl_base_peer_t *ptl_peer,
                                 mca_ptl_elan_recv_frag_t * frag)
{
    return;
}

static inline void 
mca_ptl_elan_recv_frag_matched (mca_ptl_elan_recv_frag_t * frag)
{
    return;
}

static inline void
mca_ptl_elan_recv_frag_progress (mca_ptl_elan_recv_frag_t * frag)
{
    return;
}

#endif
