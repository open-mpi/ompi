/*
 *HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_SEND_FRAG_H
#define MCA_PTL_GM_SEND_FRAG_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "os/atomic.h"
#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "ptl_gm.h"
/*#include "ptl_gm_priv.h"*/
#include "ptl_gm_peer.h"

OBJ_CLASS_DECLARATION (mca_ptl_gm_send_frag_t);
OBJ_CLASS_DECLARATION (mca_ptl_gm_recv_frag_t);

/*struct mca_ptl_base_peer_t;*/

/**
 * GM send fragment derived type.
 */
struct mca_ptl_gm_send_frag_t {
    mca_ptl_base_send_frag_t send_frag; /**< base send fragment descriptor */
    void * send_buf;
    mca_pml_base_send_request_t *req;
    mca_ptl_gm_module_t *ptl;
    /*mca_ptl_gm_peer_t *peer;*/
    int status;
    int type;
};
typedef struct mca_ptl_gm_send_frag_t mca_ptl_gm_send_frag_t;


/*#define MCA_PTL_GM_SEND_FRAG_ALLOC(item, rc)  \*/
    /*OMPI_FREE_LIST_GET(&mca_ptl_gm_module.gm_send_frags, item, rc);*/


struct mca_ptl_gm_recv_frag_t {
    mca_ptl_base_recv_frag_t frag_recv;
    size_t frag_hdr_cnt;
    size_t frag_msg_cnt;
    volatile int frag_progressed;
    bool frag_ack_pending;
    void *alloc_recv_buffer;
    void *unex_recv_buffer;
    bool matched;
    bool have_allocated_buffer;
};

typedef struct mca_ptl_gm_recv_frag_t mca_ptl_gm_recv_frag_t;



mca_ptl_gm_send_frag_t *
mca_ptl_gm_alloc_send_frag ( struct mca_ptl_base_module_t *ptl,
                        struct mca_pml_base_send_request_t *sendreq);


int
 mca_ptl_gm_send_frag_init( mca_ptl_gm_send_frag_t* sendfrag,
                                mca_ptl_gm_peer_t * ptl_peer,
                                mca_pml_base_send_request_t * sendreq,
                                size_t offset,
                                size_t* size,
                                int flags);



mca_ptl_gm_recv_frag_t *
mca_ptl_gm_alloc_recv_frag(struct mca_ptl_base_module_t *ptl);

#endif
