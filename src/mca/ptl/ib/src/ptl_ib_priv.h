#ifndef MCA_PTL_IB_PRIV_H
#define MCA_PTL_IB_PRIV_H

#include "ptl_ib_vapi.h"

/* Posting MAX_UD_PREPOST_DEPTH number of recv
 * buffers at PTL initialization. What happens when
 * more than these number of procs try to send their
 * queue pair handles?
 */
#define MAX_UD_PREPOST_DEPTH    (1)

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

struct mca_ptl_ib_ud_buf_data_t {
    VAPI_qp_hndl_t              qp_hndl; /* Remote QP handle */
};

typedef struct mca_ptl_ib_ud_buf_data_t mca_ptl_ib_ud_buf_data_t;

struct mca_ptl_ib_ud_buf_t {
    vapi_descriptor_t           desc;
    vapi_memhandle_t            memhandle;
    mca_ptl_ib_ud_buf_data_t*   buf_data;
};

typedef struct mca_ptl_ib_ud_buf_t mca_ptl_ib_ud_buf_t;

int mca_ptl_ib_ud_cq_init(VAPI_hca_hndl_t, VAPI_cq_hndl_t*,
        VAPI_cq_hndl_t*);
int mca_ptl_ib_ud_qp_init(VAPI_hca_hndl_t, VAPI_cq_hndl_t,
        VAPI_cq_hndl_t, VAPI_pd_hndl_t, VAPI_qp_hndl_t*,
        VAPI_qp_prop_t*);
int mca_ptl_ib_get_num_hcas(uint32_t*);
int mca_ptl_ib_get_hca_id(int, VAPI_hca_id_t*);
int mca_ptl_ib_get_hca_hndl(VAPI_hca_id_t, VAPI_hca_hndl_t*);
int mca_ptl_ib_query_hca_prop(VAPI_hca_hndl_t, VAPI_hca_port_t*);
int mca_ptl_ib_alloc_pd(VAPI_hca_hndl_t, VAPI_pd_hndl_t*);
int mca_ptl_ib_create_cq(VAPI_hca_hndl_t, VAPI_cq_hndl_t*);
int mca_ptl_ib_set_async_handler(VAPI_hca_hndl_t, 
        EVAPI_async_handler_hndl_t*);
int mca_ptl_ib_post_ud_recv(VAPI_hca_hndl_t, VAPI_qp_hndl_t, 
        mca_ptl_ib_ud_buf_t*);
int mca_ptl_ib_prep_ud_bufs(VAPI_hca_hndl_t, mca_ptl_ib_ud_buf_t**);
int mca_ptl_ib_register_mem(VAPI_hca_hndl_t, void*, int, 
        vapi_memhandle_t*);
int mca_ptl_ib_set_comp_ev_hndl(VAPI_hca_hndl_t, VAPI_cq_hndl_t,
        VAPI_completion_event_handler_t, void*, 
        EVAPI_compl_handler_hndl_t*);
int mca_ptl_ib_req_comp_notif(VAPI_hca_hndl_t,VAPI_cq_hndl_t);
int mca_ptl_ib_get_comp_ev_hndl(VAPI_completion_event_handler_t*);



#endif  /* MCA_PTL_IB_PRIV_H */
