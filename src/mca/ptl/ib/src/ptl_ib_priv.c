#include "ptl_ib_vapi.h"
#include "ptl_ib.h"
#include "ptl_ib_priv.h"

static void async_event_handler(VAPI_hca_hndl_t hca_hndl,
        VAPI_event_record_t * event_p,
        void *priv_data)
{
    switch (event_p->type) {
        case VAPI_QP_PATH_MIGRATED:
        case VAPI_EEC_PATH_MIGRATED:
        case VAPI_QP_COMM_ESTABLISHED:
        case VAPI_EEC_COMM_ESTABLISHED:
        case VAPI_SEND_QUEUE_DRAINED:
        case VAPI_PORT_ACTIVE:
            {
                /*
                D_PRINT("Got an asynchronous event: %s\n",
                        VAPI_event_record_sym(event_p->type));
                */
                break;
            }
        case VAPI_CQ_ERROR:
        case VAPI_LOCAL_WQ_INV_REQUEST_ERROR:
        case VAPI_LOCAL_WQ_ACCESS_VIOL_ERROR:
        case VAPI_LOCAL_WQ_CATASTROPHIC_ERROR:
        case VAPI_PATH_MIG_REQ_ERROR:
        case VAPI_LOCAL_EEC_CATASTROPHIC_ERROR:
        case VAPI_LOCAL_CATASTROPHIC_ERROR:
        case VAPI_PORT_ERROR:
            {
                ompi_output(0, "Got an asynchronous event: %s (%s)",
                        VAPI_event_record_sym(event_p->type),
                        VAPI_event_syndrome_sym(event_p->
                            syndrome));
                break;
            }
        default:
            ompi_output(0, "Warning!! Got an undefined "
                    "asynchronous event\n");
    }

}

static void ud_completion_handler(VAPI_hca_hndl_t nic,
        VAPI_cq_hndl_t cq_hndl, void* priv_data)
{
    VAPI_ret_t ret;

    D_PRINT("Got interrupt!!\n");

    ret = VAPI_req_comp_notif(nic, cq_hndl, VAPI_NEXT_COMP);

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_req_comp_notif");
    }
}

int mca_ptl_ib_ud_cq_init(VAPI_hca_hndl_t nic,
        VAPI_cq_hndl_t* ud_scq_hndl,
        VAPI_cq_hndl_t* ud_rcq_hndl)
{
    VAPI_ret_t      ret;
    VAPI_cqe_num_t  act_num_cqe = 0;

    ret = VAPI_create_cq(nic, DEFAULT_CQ_SIZE, 
            ud_scq_hndl, &act_num_cqe);

    D_PRINT("UD Send CQ handle :%d\n", (int)*ud_scq_hndl);

    if((VAPI_OK != ret) || (0 == act_num_cqe)) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_create_cq");
        return OMPI_ERROR;
    }

    /* Send completion queue was allocated successfully,
     * proceed to allocate receive completion queue */

    act_num_cqe = 0;

    ret = VAPI_create_cq(nic, DEFAULT_CQ_SIZE, 
           ud_rcq_hndl, &act_num_cqe);

    D_PRINT("UD Recv CQ handle :%d\n", (int)*ud_rcq_hndl);

    if((VAPI_OK != ret) || (act_num_cqe == 0)) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_create_cq");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

/* Set up UD Completion Queue and Queue pair */

int mca_ptl_ib_ud_qp_init(VAPI_hca_hndl_t nic,
        VAPI_cq_hndl_t ud_rcq_hndl,
        VAPI_cq_hndl_t ud_scq_hndl,
        VAPI_pd_hndl_t ptag,
        VAPI_qp_hndl_t* ud_qp_hndl,
        VAPI_qp_prop_t* ud_qp_prop)
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
    qp_init_attr.pd_hndl            = ptag;

    /* We don't have Reliable Datagram Handle right now */
    qp_init_attr.rdd_hndl           = 0;

    /* Set Send and Recv completion queues */
    qp_init_attr.rq_cq_hndl         = ud_rcq_hndl;
    qp_init_attr.sq_cq_hndl         = ud_scq_hndl;
    
    /* Signal all work requests on this queue pair */
    qp_init_attr.rq_sig_type        = VAPI_SIGNAL_ALL_WR;
    qp_init_attr.sq_sig_type        = VAPI_SIGNAL_ALL_WR;

    /* Use Unreliable Datagram transport service */
    qp_init_attr.ts_type            = VAPI_TS_UD;

    ret = VAPI_create_qp(nic, &qp_init_attr, 
            ud_qp_hndl, ud_qp_prop);

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_create_qp");
        return OMPI_ERROR;
    }

    D_PRINT("UD QP[%d] created ..hndl=%d\n",
            ud_qp_prop->qp_num,
            (int)*ud_qp_hndl);

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

    ret = VAPI_modify_qp(nic, 
            (VAPI_qp_hndl_t)*ud_qp_hndl, &qp_attr, 
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

    ret = VAPI_modify_qp(nic, 
            (VAPI_qp_hndl_t)*ud_qp_hndl, &qp_attr, 
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

    ret = VAPI_modify_qp(nic, 
            (VAPI_qp_hndl_t)*ud_qp_hndl, &qp_attr, 
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
    uint32_t num_hcas;
    VAPI_ret_t ret;
    VAPI_hca_id_t* hca_ids = NULL;

    hca_ids = (VAPI_hca_id_t*) malloc(mca_ptl_ib_component.ib_num_hcas *
            sizeof(VAPI_hca_id_t));

    if(NULL == hca_ids) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Now get the hca_id from underlying VAPI layer */
    ret = EVAPI_list_hcas(mca_ptl_ib_component.ib_num_hcas, 
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
    uint32_t act_num_cqe = 0;
    VAPI_ret_t ret;

    ret = VAPI_create_cq(nic, DEFAULT_CQ_SIZE,
            cq_hndl, &act_num_cqe);

    if( (VAPI_OK != ret) || (0 == act_num_cqe)) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_create_cq");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_ptl_ib_set_async_handler(VAPI_hca_hndl_t nic,
        EVAPI_async_handler_hndl_t *async_handler)
{
    VAPI_ret_t ret;

    ret = EVAPI_set_async_event_handler(nic,
            async_event_handler, 0, async_handler);

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "EVAPI_set_async_event_handler");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_ptl_ib_prep_ud_bufs(VAPI_hca_hndl_t nic,
        mca_ptl_ib_ud_buf_t** ud_buf_ptr)
{
    int size, len;
    int i, num_ud_bufs;
    vapi_descriptor_t* desc;
    mca_ptl_ib_ud_buf_data_t* buf_data;
    vapi_memhandle_t* memhandle;
    mca_ptl_ib_ud_buf_t* ud_buf;

    num_ud_bufs = MAX_UD_PREPOST_DEPTH;

    size = num_ud_bufs * sizeof(mca_ptl_ib_ud_buf_t);

    len = sizeof(mca_ptl_ib_ud_buf_data_t);

    (mca_ptl_ib_ud_buf_t*)*ud_buf_ptr = (mca_ptl_ib_ud_buf_t*) malloc(size);

    if(NULL == *ud_buf_ptr) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ud_buf = (mca_ptl_ib_ud_buf_t*)*ud_buf_ptr;

    /* Walk through the list of UD bufs */
    for(i = 0; i < num_ud_bufs; i++) {
        desc = &ud_buf[i].desc;
        buf_data = ud_buf[i].buf_data;
        memhandle = &ud_buf[i].memhandle;

        buf_data = (mca_ptl_ib_ud_buf_data_t*) malloc(len);
        if(NULL == buf_data) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        if(mca_ptl_ib_register_mem(nic, buf_data, len, memhandle) 
                != OMPI_SUCCESS) {
            return OMPI_ERROR;
        }

        desc->rr.comp_type = VAPI_SIGNALED;
        desc->rr.opcode = VAPI_RECEIVE;
        desc->rr.id = (VAPI_virt_addr_t) (unsigned int) &ud_buf[i];
        desc->rr.sg_lst_len = 1;
        desc->rr.sg_lst_p = &(desc->sg_entry);
        desc->sg_entry.len = len;

        D_PRINT("length = %d\n", len);
        desc->sg_entry.lkey = memhandle->lkey;

        D_PRINT("local key = %d\n", (int) memhandle->lkey);

        desc->sg_entry.addr = (VAPI_virt_addr_t) (MT_virt_addr_t) (buf_data);
        D_PRINT("sg_entry.addr = %d\n", (int)desc->sg_entry.addr);
    }

    return OMPI_SUCCESS;
}

int mca_ptl_ib_register_mem(VAPI_hca_hndl_t nic,
        void* buf, int len, vapi_memhandle_t* memhandle)
{
    VAPI_ret_t ret;
    VAPI_mrw_t mr_in, mr_out;
    vapi_memhandle_t mem_handle;

    mr_in.acl = VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE;
    mr_in.l_key = 0;
    mr_in.r_key = 0;
    mr_in.pd_hndl = nic;
    mr_in.size = len;
    mr_in.start = (VAPI_virt_addr_t) (MT_virt_addr_t) buf;
    mr_in.type = VAPI_MR;

    ret = VAPI_register_mr(nic, &mr_in, &mem_handle.hndl, &mr_out);
    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_register_mr");
        return OMPI_ERROR;
    }

    mem_handle.lkey = mr_out.l_key;
    mem_handle.rkey = mr_out.r_key;

    memhandle->lkey = mem_handle.lkey;
    memhandle->rkey = mem_handle.rkey;

    D_PRINT("local key = %d, remote key = %d\n",
            mem_handle.lkey, mem_handle.rkey);

    memhandle->hndl = mem_handle.hndl;

    return OMPI_SUCCESS;
}

int mca_ptl_ib_post_ud_recv(VAPI_hca_hndl_t nic,
        VAPI_qp_hndl_t ud_qp_hndl, 
        mca_ptl_ib_ud_buf_t* ud_buf)
{
    int num_ud_bufs, i;
    VAPI_ret_t ret;
    vapi_descriptor_t* desc;

    num_ud_bufs = MAX_UD_PREPOST_DEPTH;

    for(i = 0; i< num_ud_bufs; i++) {
        desc = &ud_buf[i].desc;

        ret = VAPI_post_rr(nic, ud_qp_hndl, &desc->rr);

        if(VAPI_OK != ret) {
            MCA_PTL_IB_VAPI_RET(ret, "VAPI_post_rr");
            return OMPI_ERROR;
        }
    }
    return OMPI_SUCCESS;
}

int mca_ptl_ib_set_comp_ev_hndl(VAPI_hca_hndl_t nic, 
        VAPI_cq_hndl_t cq_hndl, VAPI_completion_event_handler_t handler, 
        void* priv_data, EVAPI_compl_handler_hndl_t *handler_hndl)
{
    VAPI_ret_t ret;

    ret = EVAPI_set_comp_eventh(nic, cq_hndl, handler, 
            priv_data, handler_hndl);

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "EVAPI_set_comp_eventh");
        return OMPI_ERROR;
    }

    D_PRINT("Completion hander: %p, Handle = %d\n",
            handler, (int)*handler_hndl);
    
    return OMPI_SUCCESS;
}

int mca_ptl_ib_req_comp_notif(VAPI_hca_hndl_t nic, VAPI_cq_hndl_t cq_hndl)
{
    VAPI_ret_t ret;

    ret = VAPI_req_comp_notif(nic, cq_hndl, VAPI_NEXT_COMP);

    if(VAPI_OK != ret) {
        MCA_PTL_IB_VAPI_RET(ret, "VAPI_req_comp_notif");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_ptl_ib_get_comp_ev_hndl(VAPI_completion_event_handler_t* handler_ptr)
{
    *handler_ptr = ud_completion_handler;

    D_PRINT("UD Completion Event Handler = %p\n", ud_completion_handler);

    return OMPI_SUCCESS;
}
