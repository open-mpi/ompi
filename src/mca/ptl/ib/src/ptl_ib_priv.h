#ifndef MCA_PTL_IB_PRIV_H
#define MCA_PTL_IB_PRIV_H

#include <stdint.h>
#include "ptl_ib_vapi.h"

#define NUM_IB_SEND_BUF             (10)
#define NUM_IB_RECV_BUF             (10)

struct mca_ptl_ib_state_t {
    VAPI_hca_id_t                   hca_id;     
    /* ID of HCA */

    VAPI_hca_port_t                 port;       
    /* IB port of this PTL */

    VAPI_hca_hndl_t                 nic;        
    /* NIC handle */  

    VAPI_pd_hndl_t                  ptag;       
    /* Protection Domain tag */

    VAPI_cq_hndl_t                  cq_hndl;    
    /* Completion Queue handle */
    /* At present Send & Recv are tied to the same completion queue */

    EVAPI_async_handler_hndl_t      async_handler; 
    /* Async event handler used to detect weird/unknown events */
};

typedef struct mca_ptl_ib_state_t mca_ptl_ib_state_t;

typedef enum {
    IB_RECV,
    IB_SEND
} IB_wr_t;

struct vapi_memhandle_t {
    VAPI_mr_hndl_t                  hndl;
    /* Memory region handle */

    VAPI_lkey_t                     lkey;
    /* Local key to registered memory, needed for
     * posting send/recv requests */

    VAPI_rkey_t                     rkey;
    /* Remote key to registered memory, need to send this
     * to remote processes for incoming RDMA ops */
};

typedef struct vapi_memhandle_t vapi_memhandle_t;

struct vapi_descriptor_t {
    union {
        VAPI_rr_desc_t              rr;
        /* Receive descriptor */

        VAPI_sr_desc_t              sr;
        /* Send descriptor */
    };

    VAPI_sg_lst_entry_t sg_entry;
    /* Scatter/Gather entry */
};

typedef struct vapi_descriptor_t vapi_descriptor_t;

struct ib_buffer_t {
    vapi_descriptor_t               desc;
    /* Descriptor of the buffer */

    vapi_memhandle_t                hndl;
    /* Buffer handle */

    char                            buf[4096];
    /* Buffer space */
};

typedef struct ib_buffer_t ib_buffer_t;

/* mca_ptl_ib_peer_local_res_t contains information
 * regarding local resources dedicated to this
 * connection */
struct mca_ptl_ib_peer_local_res_t {

    VAPI_qp_hndl_t                  qp_hndl;
    /* Local QP handle */

    VAPI_qp_prop_t                  qp_prop;
    /* Local QP properties */

    ib_buffer_t                     *send;
    /* Pointer to send buffers */

    ib_buffer_t                     *recv;
    /* Pointer to recv buffers */
};

typedef struct mca_ptl_ib_peer_local_res_t mca_ptl_ib_peer_local_res_t;

/* mca_ptl_ib_peer_remote_res_t contains information 
 * regarding remote resources dedicated to this
 * connection */
struct mca_ptl_ib_peer_remote_res_t {

    VAPI_qp_num_t                   qp_num;
    /* Remote side QP number */

    IB_lid_t                        lid;
    /* Local identifier of the remote process */
};

typedef struct mca_ptl_ib_peer_remote_res_t mca_ptl_ib_peer_remote_res_t;

/* mca_ptl_ib_peer_conn_t contains private information
 * about the peer. This information is used to describe
 * the connection oriented information about this peer
 * and local resources associated with it. */
struct mca_ptl_ib_peer_conn_t {

    mca_ptl_ib_peer_local_res_t*        lres;
    /* Local resources associated with this connection */

    mca_ptl_ib_peer_remote_res_t*       rres;
    /* Remote resources associated with this connection */
};

typedef struct mca_ptl_ib_peer_conn_t mca_ptl_ib_peer_conn_t;

#define DUMP_IB_STATE(ib_state_ptr) {                               \
    ompi_output(0, "[%s:%d] ", __FILE__, __LINE__);                 \
    ompi_output(0, "Dumping IB state");                             \
    ompi_output(0, "HCA ID : %s", ib_state_ptr->hca_id);            \
    ompi_output(0, "LID : %d", ib_state_ptr->port.lid);             \
    ompi_output(0, "HCA handle : %d", ib_state_ptr->nic);           \
    ompi_output(0, "Protection Domain: %d", ib_state_ptr->ptag);    \
    ompi_output(0, "Comp Q handle : %d", ib_state_ptr->cq_hndl);    \
    ompi_output(0, "Async hndl : %d", ib_state_ptr->async_handler); \
}

#define IB_PREPARE_RECV_DESC(ib_buf_ptr) {                          \
    ib_buf_ptr->desc.rr.comp_type = VAPI_SIGNALED;                  \
    ib_buf_ptr->desc.rr.opcode = VAPI_RECEIVE;                      \
    ib_buf_ptr->desc.rr.id = (VAPI_virt_addr_t)                     \
        (MT_virt_addr_t) ib_buf_ptr;                                \
    ib_buf_ptr->desc.rr.sg_lst_len = 1;                             \
    ib_buf_ptr->desc.rr.sg_lst_p = &ib_buf_ptr->desc.sg_entry;      \
    ib_buf_ptr->desc.sg_entry.len = 4096;                           \
    ib_buf_ptr->desc.sg_entry.lkey = ib_buf_ptr->hndl.lkey;         \
    ib_buf_ptr->desc.sg_entry.addr = (VAPI_virt_addr_t)             \
        (MT_virt_addr_t) ib_buf_ptr->buf;                           \
}

int mca_ptl_ib_init_module(mca_ptl_ib_state_t*, int);
int mca_ptl_ib_get_num_hcas(uint32_t*);
int mca_ptl_ib_init_peer(mca_ptl_ib_state_t*, mca_ptl_ib_peer_conn_t*);
int mca_ptl_ib_peer_connect(mca_ptl_ib_state_t*, 
        mca_ptl_ib_peer_conn_t*);

#endif  /* MCA_PTL_IB_PRIV_H */
