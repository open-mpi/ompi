#include "ptl_ib_vapi.h"
#include "ptl_ib.h"
#include "ptl_ib_priv.h"

VAPI_ret_t mca_ptl_ib_ud_cq_init(mca_ptl_ib_t* ptl_ib)
{
    VAPI_ret_t      ret;
    VAPI_cqe_num_t  act_num_cqe = 0;

    ret = VAPI_create_cq(ptl_ib->nic, DEFAULT_CQ_SIZE, 
            &(ptl_ib->ud_scq_hndl), &act_num_cqe);
    MCA_PTL_IB_VAPI_RET(ret, ret, "VAPI_create_cq");

    if(act_num_cqe == 0) {
        /* Couldn't give any CQ entries, not
         * enough resources */
        return VAPI_EAGAIN;
    }

    /* Send completion queue was allocated successfully,
     * proceed to allocate receive completion queue */

    act_num_cqe = 0;

    ret = VAPI_create_cq(ptl_ib->nic, DEFAULT_CQ_SIZE, 
            &(ptl_ib->ud_rcq_hndl), &act_num_cqe);
    MCA_PTL_IB_VAPI_RET(ret, ret, "VAPI_create_cq");

    if(act_num_cqe == 0) {
        /* Couldn't give any CQ entries, not
         * enough resources */
        return VAPI_EAGAIN;
    }

    return VAPI_OK;
}

/* Set up UD Completion Queue and Queue pair */

VAPI_ret_t mca_ptl_ib_ud_qp_init(mca_ptl_ib_t* ptl_ib)
{
    VAPI_qp_init_attr_t     qp_init_attr;
    VAPI_qp_attr_t          qp_attr;
    VAPI_qp_cap_t           qp_cap;
    VAPI_qp_attr_mask_t     qp_attr_mask;
    VAPI_ret_t              ret;

    qp_init_attr.cap.max_oust_wr_rq = DEFAULT_UD_WQ_SIZE;
    qp_init_attr.cap.max_oust_wr_sq = DEFAULT_UD_WQ_SIZE;
    qp_init_attr.cap.max_sg_size_rq = DEFAULT_UD_SG_LIST;
    qp_init_attr.cap.max_sg_size_sq = DEFAULT_UD_SG_LIST;
    qp_init_attr.pd_hndl            = ptl_ib->ptag;

    /* We don't have Reliable Datagram Handle right now */
    qp_init_attr.rdd_hndl           = 0;

    /* Set Send and Recv completion queues */
    qp_init_attr.rq_cq_hndl         = ptl_ib->ud_rcq_hndl;
    qp_init_attr.sq_cq_hndl         = ptl_ib->ud_scq_hndl;
    
    /* Signal all work requests on this queue pair */
    qp_init_attr.rq_sig_type        = VAPI_SIGNAL_ALL_WR;
    qp_init_attr.sq_sig_type        = VAPI_SIGNAL_ALL_WR;

    /* Use Unreliable Datagram transport service */
    qp_init_attr.ts_type            = VAPI_TS_UD;

    ret = VAPI_create_qp(ptl_ib->nic, &qp_init_attr, 
            &(ptl_ib->ud_qp_hndl), &(ptl_ib->ud_qp_prop));

    MCA_PTL_IB_VAPI_RET(ret, ret, "VAPI_create_qp");

    D_PRINT("UD QP[%d] created ..hndl=%d\n",
            ptl_ib->ud_qp_prop.qp_num,
            (int)ptl_ib->ud_qp_hndl);

    /* Modifying  QP to INIT */
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_INIT;
    QP_ATTR_MASK_SET(qp_attr_mask,QP_ATTR_QP_STATE);
    qp_attr.pkey_ix  = DEFAULT_PKEY_IX;
    QP_ATTR_MASK_SET(qp_attr_mask,QP_ATTR_PKEY_IX);
    qp_attr.port     = DEFAULT_PORT;
    QP_ATTR_MASK_SET(qp_attr_mask,QP_ATTR_PORT);
    qp_attr.qkey = 0;
    QP_ATTR_MASK_SET(qp_attr_mask,QP_ATTR_QKEY);

    ret = VAPI_modify_qp(ptl_ib->nic, 
            ptl_ib->ud_qp_hndl, &qp_attr, 
            &qp_attr_mask, &qp_cap);

    MCA_PTL_IB_VAPI_RET(ret, ret, "VAPI_modify_qp");

    D_PRINT("Modified UD to init..Qp\n");

    /*****************
     * INIT --> RTR
     *****************/
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state         = VAPI_RTR;
    QP_ATTR_MASK_SET(qp_attr_mask,QP_ATTR_QP_STATE);

    ret = VAPI_modify_qp(ptl_ib->nic, 
            ptl_ib->ud_qp_hndl, &qp_attr, 
            &qp_attr_mask, &qp_cap);
    MCA_PTL_IB_VAPI_RET(ret, ret, "VAPI_modify_qp");

    D_PRINT("Modified UD to RTR..Qp\n");

    /*********************
     * RTR -->  RTS
     *********************/
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state         = VAPI_RTS;
    QP_ATTR_MASK_SET(qp_attr_mask,QP_ATTR_QP_STATE);
    qp_attr.sq_psn           = DEFAULT_PSN;
    QP_ATTR_MASK_SET(qp_attr_mask,QP_ATTR_SQ_PSN);

    ret = VAPI_modify_qp(ptl_ib->nic, 
            ptl_ib->ud_qp_hndl, &qp_attr, 
            &qp_attr_mask, &qp_cap);

    MCA_PTL_IB_VAPI_RET(ret, ret, "VAPI_modify_qp");

    D_PRINT("Modified UD to RTS..Qp\n");

    /* Everything was fine ... return success! */
    return VAPI_OK;
}
