#include "ptl_ib_vapi.h"
#include "ptl_ib.h"
#include "ptl_ib_priv.h"

int mca_ptl_ib_ud_cq_init(mca_ptl_ib_t* ptl_ib)
{
    VAPI_ret_t      ret;
    VAPI_cqe_num_t  act_num_cqe = 0;

    ret = VAPI_create_cq(ptl_ib->nic, DEFAULT_CQ_SIZE, 
            &(ptl_ib->ud_scq_hndl), &act_num_cqe);

    D_PRINT("UD Send CQ handle :%d\n", ptl_ib->ud_scq_hndl);

    if((VAPI_OK != ret) || (0 == act_num_cqe)) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_create_cq");
        return OMPI_ERROR;
    }

    /* Send completion queue was allocated successfully,
     * proceed to allocate receive completion queue */

    act_num_cqe = 0;

    ret = VAPI_create_cq(ptl_ib->nic, DEFAULT_CQ_SIZE, 
            &(ptl_ib->ud_rcq_hndl), &act_num_cqe);

    D_PRINT("UD Recv CQ handle :%d\n", ptl_ib->ud_rcq_hndl);

    if((VAPI_OK != ret) || (act_num_cqe == 0)) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_create_cq");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/* Set up UD Completion Queue and Queue pair */

int mca_ptl_ib_ud_qp_init(mca_ptl_ib_t* ptl_ib)
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

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_create_qp");
        return OMPI_ERROR;
    }

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

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }

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

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }

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

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }
    D_PRINT("Modified UD to RTS..Qp\n");

    /* Everything was fine ... return success! */
    return OMPI_SUCCESS;
}

int mca_ptl_ib_get_num_hcas(uint32_t* num_hcas)
{
    VAPI_ret_t ret;

    /* List all HCAs */
    ret = EVAPI_list_hcas(0, num_hcas, NULL);

    if( (VAPI_OK != ret) && (VAPI_EAGAIN != ret)) {
        MCA_PTL_IB_VAPI_RET(ret, "EVAPI_list_hcas");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/* This function returns the hca_id for each PTL
 * in a round robin manner. Each PTL gets a different
 * HCA id ...
 *
 * If num PTLs > num HCAs, then those ptls will be
 * assigned HCA ids beginning from 0 again.
 */

int mca_ptl_ib_get_hca_id(int num, VAPI_hca_id_t* hca_id)
{
    int num_hcas;
    VAPI_ret_t ret;
    VAPI_hca_id_t* hca_ids = NULL;

    hca_ids = (VAPI_hca_id_t*) malloc(mca_ptl_ib_module.ib_num_hcas *
            sizeof(VAPI_hca_id_t));

    /* Now get the hca_id from underlying VAPI layer */
    ret = EVAPI_list_hcas(mca_ptl_ib_module.ib_num_hcas, 
            &num_hcas, hca_ids);

    /* HACK: right now, I have put VAPI_EAGAIN as
     * acceptable condition since we are trying to have
     * only 1 ptl support */
    if((VAPI_OK != ret) && (VAPI_EAGAIN != ret)) {
        MCA_PTL_IB_VAPI_RET(ret, "EVAPI_list_hcas");
        return OMPI_ERROR;
    } else {

        num = num % num_hcas;

        memcpy(hca_id, hca_ids[num], sizeof(VAPI_hca_id_t));
    }

    free(hca_ids);

    return OMPI_SUCCESS;
}

int mca_ptl_ib_get_hca_hndl(VAPI_hca_id_t hca_id,
        VAPI_hca_hndl_t* hca_hndl) 
{
    VAPI_ret_t ret; 

    /* Open the HCA */
    ret = EVAPI_get_hca_hndl(hca_id, hca_hndl);

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "EVAPI_get_hca_hndl");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_ptl_ib_query_hca_prop(VAPI_hca_hndl_t nic,
        VAPI_hca_port_t* port)
{
    VAPI_ret_t ret;

    /* Querying for port properties */
    ret = VAPI_query_hca_port_prop(nic,
            (IB_port_t)DEFAULT_PORT, 
            port);

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_query_hca_port_prop");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_ptl_ib_alloc_pd(VAPI_hca_hndl_t nic,
        VAPI_pd_hndl_t* ptag)
{
    VAPI_ret_t ret;

    ret = VAPI_alloc_pd(nic, ptag);

    if(ret != VAPI_OK) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_alloc_pd");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_ptl_ib_create_cq(VAPI_hca_hndl_t nic,
                VAPI_cq_hndl_t* cq_hndl)
{
    int act_num_cqe = 0;
    VAPI_ret_t ret;

    ret = VAPI_create_cq(nic, DEFAULT_CQ_SIZE,
            cq_hndl, &act_num_cqe);

    if( (VAPI_OK != ret) || (0 == act_num_cqe)) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_create_cq");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
