#include "ptl_ib_priv.h"


struct mca_ptl_ib_ud_buf_data_t {
    VAPI_qp_num_t              qp_num; /* Remote QP num */
};

typedef struct mca_ptl_ib_ud_buf_data_t mca_ptl_ib_ud_buf_data_t;

struct mca_ptl_ib_ud_buf_t {
    uint32_t                    in_use;  /* Set to 1 when using; after comp. set to 0 */
    vapi_descriptor_t           desc;
    vapi_memhandle_t            memhandle;
    void*                       buf_data;
};

typedef struct mca_ptl_ib_ud_buf_t mca_ptl_ib_ud_buf_t;

struct mca_ptl_ib_ud_buf_ctrl_t {
    uint32_t                    index;  /* The buffer to use, (circular fashion) */
    mca_ptl_ib_ud_buf_t*        bufs;   /* Array of structures of ud buffers */
};

typedef struct mca_ptl_ib_ud_buf_ctrl_t mca_ptl_ib_ud_buf_ctrl_t;

int mca_ptl_ib_post_ud_recv(VAPI_hca_hndl_t, VAPI_qp_hndl_t, 
        mca_ptl_ib_ud_buf_t*, int);
int mca_ptl_ib_prep_ud_bufs(VAPI_hca_hndl_t, VAPI_pd_hndl_t,
        mca_ptl_ib_ud_buf_t*, IB_wr_t, int);
