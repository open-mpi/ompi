#ifndef MCA_PTL_IB_PRIV_H
#define MCA_PTL_IB_PRIV_H

#include "ptl_ib_vapi.h"

/* Posting MAX_UD_PREPOST_DEPTH number of recv
 * buffers at PTL initialization. What happens when
 * more than these number of procs try to send their
 * queue pair handles?
 */
#define MAX_UD_PREPOST_DEPTH    (1)
#define BUFSIZE                 (4096)
#define NUM_BUFS                (5000)

typedef enum {
    IB_RECV,
    IB_SEND
} IB_wr_t;

struct vapi_memhandle_t {
    VAPI_mr_hndl_t hndl;
    VAPI_lkey_t lkey;
    VAPI_rkey_t rkey;
};

typedef struct vapi_memhandle_t vapi_memhandle_t;

struct vapi_descriptor_t {
    union {
        VAPI_rr_desc_t rr;
        VAPI_sr_desc_t sr;
    };

    VAPI_sg_lst_entry_t sg_entry;
};

typedef struct vapi_descriptor_t vapi_descriptor_t;

struct mca_ptl_ib_send_buf_t {
    mca_pml_base_request_t      *req;
    vapi_descriptor_t           desc;
    char                        buf[4096];
};

typedef struct mca_ptl_ib_send_buf_t mca_ptl_ib_send_buf_t;

struct mca_ptl_ib_recv_buf_t {
    mca_pml_base_request_t      *req;
    vapi_descriptor_t           desc;
    char                        buf[4096];
};

typedef struct mca_ptl_ib_recv_buf_t mca_ptl_ib_recv_buf_t;

#define MCA_PTL_IB_UD_RECV_DESC(ud_buf, len) {                      \
        desc->rr.comp_type = VAPI_SIGNALED;                         \
        desc->rr.opcode = VAPI_RECEIVE;                             \
        desc->rr.id = (VAPI_virt_addr_t)(MT_virt_addr_t) &(ud_buf); \
        desc->rr.sg_lst_len = 1;                                    \
        desc->rr.sg_lst_p = &(desc->sg_entry);                      \
        desc->sg_entry.len = len;                                   \
        desc->sg_entry.addr =                                       \
        (VAPI_virt_addr_t) (MT_virt_addr_t) ud_buf.buf_data;        \
        desc->sg_entry.lkey = ud_buf.memhandle.lkey;                \
}

#define MCA_PTL_IB_UD_SEND_DESC(ud_buf, len) {                      \
        desc->sr.comp_type = VAPI_SIGNALED;                         \
        desc->sr.opcode = VAPI_SEND;                                \
        desc->sr.id = (VAPI_virt_addr_t)(MT_virt_addr_t) ud_buf;    \
        desc->sr.sg_lst_len = 1;                                    \
        desc->sr.sg_lst_p = &(desc->sg_entry);                      \
        desc->sg_entry.len = len;                                   \
        desc->sg_entry.addr =                                       \
        (VAPI_virt_addr_t) (MT_virt_addr_t) ud_buf->buf_data;       \
        desc->sg_entry.lkey = ud_buf->memhandle.lkey;               \
}

int mca_ptl_ib_ud_cq_init(VAPI_hca_hndl_t, VAPI_cq_hndl_t*,
        VAPI_cq_hndl_t*);
int mca_ptl_ib_ud_qp_init(VAPI_hca_hndl_t, VAPI_qp_hndl_t);
int mca_ptl_ib_get_num_hcas(uint32_t*);
int mca_ptl_ib_get_hca_id(int, VAPI_hca_id_t*);
int mca_ptl_ib_get_hca_hndl(VAPI_hca_id_t, VAPI_hca_hndl_t*);
int mca_ptl_ib_query_hca_prop(VAPI_hca_hndl_t, VAPI_hca_port_t*);
int mca_ptl_ib_alloc_pd(VAPI_hca_hndl_t, VAPI_pd_hndl_t*);
int mca_ptl_ib_create_cq(VAPI_hca_hndl_t, VAPI_cq_hndl_t*);
int mca_ptl_ib_set_async_handler(VAPI_hca_hndl_t, 
        EVAPI_async_handler_hndl_t*);
int mca_ptl_ib_register_mem(VAPI_hca_hndl_t, VAPI_pd_hndl_t, void*, int, 
        vapi_memhandle_t*);
int mca_ptl_ib_set_comp_ev_hndl(VAPI_hca_hndl_t, VAPI_cq_hndl_t,
        VAPI_completion_event_handler_t, void*, 
        EVAPI_compl_handler_hndl_t*);
int mca_ptl_ib_req_comp_notif(VAPI_hca_hndl_t,VAPI_cq_hndl_t);
int mca_ptl_ib_get_comp_ev_hndl(VAPI_completion_event_handler_t*);
int mca_ptl_ib_init_send(void*, VAPI_qp_hndl_t, int);
int mca_ptl_ib_create_qp(VAPI_hca_hndl_t, VAPI_pd_hndl_t,
        VAPI_cq_hndl_t, VAPI_cq_hndl_t, VAPI_qp_hndl_t*,
        VAPI_qp_prop_t*, int);
int mca_ptl_ib_rc_qp_init(VAPI_hca_hndl_t, VAPI_qp_hndl_t,
        VAPI_qp_num_t, IB_lid_t);
void mca_ptl_ib_frag(struct mca_ptl_ib_module_t* module,
        mca_ptl_base_header_t * header);

#endif  /* MCA_PTL_IB_PRIV_H */
