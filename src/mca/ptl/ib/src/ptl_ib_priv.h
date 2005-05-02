/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PTL_IB_PRIV_H
#define MCA_PTL_IB_PRIV_H

#include <stdint.h>
#include "class/ompi_free_list.h"
#include "ptl_ib_vapi.h"
#include "ptl_ib_memory.h"

#define NUM_IB_SEND_BUF             (1)
#define NUM_IB_RECV_BUF             (4)

#define MCA_PTL_IB_FIRST_FRAG_SIZE  (65536)

typedef enum {
    IB_RECV,
    IB_SEND
} IB_wr_t;

typedef enum {
    IB_COMP_ERROR,
    IB_COMP_RECV,
    IB_COMP_SEND,
    IB_COMP_RDMA_W,
    IB_COMP_NOTHING
} IB_comp_t;

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
    ompi_list_item_t                super;
    vapi_descriptor_t               desc;
    /* Descriptor of the buffer */

    vapi_memhandle_t                hndl;
    /* Buffer handle */

    char                            buf[MCA_PTL_IB_FIRST_FRAG_SIZE];
    /* Buffer space */

    VAPI_qp_hndl_t                  qp_hndl;
    /* Queue pair used for this IB buffer */
};

typedef struct ib_buffer_t ib_buffer_t;


#define DUMP_IB_STATE(ib_ptl) {                               \
    ompi_output(0, "[%s:%d] ", __FILE__, __LINE__);           \
    ompi_output(0, "Dumping IB state");                       \
    ompi_output(0, "HCA ID : %s", ib_ptl->hca_id);            \
    ompi_output(0, "LID : %d", ib_ptl->port.lid);             \
    ompi_output(0, "HCA handle : %d", ib_ptl->nic);           \
    ompi_output(0, "Protection Domain: %d", ib_ptl->ptag);    \
    ompi_output(0, "Comp Q handle : %d", ib_ptl->cq_hndl);    \
    ompi_output(0, "Async hndl : %d", ib_ptl->async_handler); \
}

#define IB_PREPARE_RECV_DESC(ib_buf_ptr) {                          \
    ib_buf_ptr->desc.rr.comp_type = VAPI_SIGNALED;                  \
    ib_buf_ptr->desc.rr.opcode = VAPI_RECEIVE;                      \
    ib_buf_ptr->desc.rr.id = (VAPI_virt_addr_t)                     \
        (MT_virt_addr_t) ib_buf_ptr;                                \
    ib_buf_ptr->desc.rr.sg_lst_len = 1;                             \
    ib_buf_ptr->desc.rr.sg_lst_p = &ib_buf_ptr->desc.sg_entry;      \
    ib_buf_ptr->desc.sg_entry.len = MCA_PTL_IB_FIRST_FRAG_SIZE;     \
    ib_buf_ptr->desc.sg_entry.lkey = ib_buf_ptr->hndl.lkey;         \
    ib_buf_ptr->desc.sg_entry.addr = (VAPI_virt_addr_t)             \
        (MT_virt_addr_t) ib_buf_ptr->buf;                           \
}

#define IB_PREPARE_SEND_DESC(ib_buf_ptr, qp, msg_len,               \
        id_buf) {                                                   \
    ib_buf_ptr->desc.sr.comp_type = VAPI_SIGNALED;                  \
    ib_buf_ptr->desc.sr.opcode = VAPI_SEND;                         \
    ib_buf_ptr->desc.sr.remote_qkey = 0;                            \
    ib_buf_ptr->desc.sr.remote_qp = qp;                             \
    ib_buf_ptr->desc.sr.id = (VAPI_virt_addr_t)                     \
        (MT_virt_addr_t) id_buf;                                    \
    ib_buf_ptr->desc.sr.sg_lst_len = 1;                             \
    ib_buf_ptr->desc.sr.sg_lst_p = &ib_buf_ptr->desc.sg_entry;      \
    ib_buf_ptr->desc.sg_entry.len = msg_len;                        \
    ib_buf_ptr->desc.sg_entry.lkey = ib_buf_ptr->hndl.lkey;         \
    ib_buf_ptr->desc.sg_entry.addr = (VAPI_virt_addr_t)             \
        (MT_virt_addr_t) ib_buf_ptr->buf;                           \
}

#define IB_SET_REMOTE_QP_NUM(ib_buf_ptr, qp) {                      \
    ib_buf_ptr->desc.sr.remote_qp = qp;                             \
}

#define IB_SET_SEND_DESC_ID(ib_buf_ptr, addr) {                     \
    ib_buf_ptr->desc.sr.id = (VAPI_virt_addr_t)                     \
        (MT_virt_addr_t) addr;                                      \
}

#define IB_SET_SEND_DESC_LEN(ib_buf_ptr, msg_len) {                 \
    ib_buf_ptr->desc.sg_entry.len = msg_len;                        \
}

#define IB_PREPARE_RDMA_W_DESC(ib_buf_ptr, qp,                      \
        msg_len, user_buf, local_key, remote_key,                   \
        id_buf, remote_buf) {                                       \
    ib_buf_ptr->desc.sr.comp_type = VAPI_SIGNALED;                  \
    ib_buf_ptr->desc.sr.opcode = VAPI_RDMA_WRITE;                   \
    ib_buf_ptr->desc.sr.remote_qkey = 0;                            \
    ib_buf_ptr->desc.sr.remote_qp = qp;                             \
    ib_buf_ptr->desc.sr.id = (VAPI_virt_addr_t)                     \
        (MT_virt_addr_t) id_buf;                                    \
    ib_buf_ptr->desc.sr.sg_lst_len = 1;                             \
    ib_buf_ptr->desc.sr.sg_lst_p = &ib_buf_ptr->desc.sg_entry;      \
    ib_buf_ptr->desc.sg_entry.len = msg_len;                        \
    ib_buf_ptr->desc.sg_entry.lkey = local_key;                     \
    ib_buf_ptr->desc.sg_entry.addr = (VAPI_virt_addr_t)             \
        (MT_virt_addr_t) user_buf;                                  \
    ib_buf_ptr->desc.sr.remote_addr = (VAPI_virt_addr_t)            \
        (MT_virt_addr_t) remote_buf;                                \
    ib_buf_ptr->desc.sr.r_key = remote_key;                         \
}


struct mca_ptl_ib_module_t;
struct mca_ptl_base_peer_t;


int mca_ptl_ib_module_init(struct mca_ptl_ib_module_t*);

int mca_ptl_ib_register_mem(
    VAPI_hca_hndl_t nic, 
    VAPI_pd_hndl_t ptag,
    void* buf, 
    int len, 
    vapi_memhandle_t* memhandle);

int mca_ptl_ib_post_send(
    struct mca_ptl_ib_module_t *ib_module,
    struct mca_ptl_base_peer_t *peer, 
    ib_buffer_t *ib_buf, void*);

void mca_ptl_ib_buffer_repost(
    VAPI_hca_hndl_t nic,
    void* addr);

void mca_ptl_ib_prepare_ack(
    struct mca_ptl_ib_module_t *ib_module,
    void* addr_to_reg, int len_to_reg,
    void* ack_buf, int* len_added);

int mca_ptl_ib_rdma_write(
    struct mca_ptl_ib_module_t *ib_module,
    struct mca_ptl_base_peer_t *peer, 
    ib_buffer_t *ib_buf,
    void* send_buf, 
    size_t send_len, 
    void* remote_buf,
    VAPI_rkey_t remote_key, void*);

int mca_ptl_ib_create_qp(VAPI_hca_hndl_t nic,
    VAPI_pd_hndl_t ptag,
    VAPI_cq_hndl_t recv_cq,
    VAPI_cq_hndl_t send_cq,
    VAPI_qp_hndl_t* qp_hndl,
    VAPI_qp_prop_t* qp_prop,
    int transport_type);

int mca_ptl_ib_qp_init(
    VAPI_hca_hndl_t nic,
    VAPI_qp_hndl_t qp_hndl,
    VAPI_qp_num_t remote_qp,
    IB_lid_t remote_lid);

#endif  /* MCA_PTL_IB_PRIV_H */
