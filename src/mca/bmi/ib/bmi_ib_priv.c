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

#include "ompi_config.h"
#include "bmi_ib_vapi.h"
#include "bmi_ib_priv.h"
#include "bmi_ib.h"
#include "bmi_ib_frag.h" 
#include "bmi_ib_endpoint.h" 


/* 
 * This function returns the hca_id for each BMI
 * in a round robin manner. Each BMI gets a different
 * HCA id ...
 *
 * If num BMIs > num HCAs, then those bmis will be
 * assigned HCA ids beginning from 0 again.
 *
 */


/* static int mca_bmi_ib_get_hca_hndl(VAPI_hca_id_t hca_id, */
/*         VAPI_hca_hndl_t* hca_hndl)  */
/* { */
/*     VAPI_ret_t ret;  */

/*     /\* Open the HCA *\/ */
/*     ret = EVAPI_get_hca_hndl(hca_id, hca_hndl); */

/*     if(VAPI_OK != ret) { */
/*         MCA_BMI_IB_VAPI_RET(ret, "EVAPI_get_hca_hndl"); */
/*         return OMPI_ERROR; */
/*     } */

/*     return OMPI_SUCCESS; */
/* } */


/* static int mca_bmi_ib_query_hca_prop(VAPI_hca_hndl_t nic)  */
/* { */
/*     VAPI_hca_vendor_t hca_vendor;  */
/*     VAPI_hca_cap_t hca_cap;  */
/*     VAPI_ret_t ret;  */
/*     ret = VAPI_query_hca_prop(nic, &hca_vendor, &hca_cap);  */
/*     hca_cap.phys_port_num; */
    
/* } */
/* static int mca_bmi_ib_query_hca_port_prop(VAPI_hca_hndl_t nic, */
/*         VAPI_hca_port_t* port) */
/* { */
/*     VAPI_ret_t ret; */

/*     /\* Querying for port properties *\/ */
/*     ret = VAPI_query_hca_port_prop(nic, */
/*             (IB_port_t)DEFAULT_PORT,  */
/*             port); */

/*     if(VAPI_OK != ret) { */
/*         MCA_BMI_IB_VAPI_RET(ret, "VAPI_query_hca_port_prop"); */
/*         return OMPI_ERROR; */
/*     } */

/*     return OMPI_SUCCESS; */
/* } */


int mca_bmi_ib_create_qp(VAPI_hca_hndl_t nic,
        VAPI_pd_hndl_t ptag,
        VAPI_cq_hndl_t recv_cq,
        VAPI_cq_hndl_t send_cq,
        VAPI_qp_hndl_t* qp_hndl,
        VAPI_qp_prop_t* qp_prop,
        int transport_type)
{
    VAPI_ret_t ret;
    VAPI_qp_init_attr_t qp_init_attr;

    switch(transport_type) {

        case VAPI_TS_RC: /* Set up RC qp parameters */
            qp_init_attr.cap.max_oust_wr_rq = DEFAULT_WQ_SIZE;
            qp_init_attr.cap.max_oust_wr_sq = DEFAULT_WQ_SIZE;
            qp_init_attr.cap.max_sg_size_rq = DEFAULT_SG_LIST;
            qp_init_attr.cap.max_sg_size_sq = DEFAULT_SG_LIST;
            qp_init_attr.pd_hndl            = ptag;
            /* We don't have Reliable Datagram Handle right now */
            qp_init_attr.rdd_hndl           = 0;

            /* Set Send and Recv completion queues */
            qp_init_attr.rq_cq_hndl         = recv_cq;
            qp_init_attr.sq_cq_hndl         = send_cq;

            /* Signal all work requests on this queue pair */
            qp_init_attr.rq_sig_type        = VAPI_SIGNAL_REQ_WR;
            qp_init_attr.sq_sig_type        = VAPI_SIGNAL_REQ_WR;

            /* Use Unreliable Datagram transport service */
            qp_init_attr.ts_type            = VAPI_TS_RC;
            break;
        case VAPI_TS_UD: /* Set up UD qp parameters */
        default:
            return OMPI_ERR_NOT_IMPLEMENTED;
    }

    ret = VAPI_create_qp(nic, &qp_init_attr, 
            qp_hndl, qp_prop);
    
    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_RET(ret, "VAPI_create_qp");
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}


int mca_bmi_ib_qp_query(mca_bmi_ib_module_t* ib_bmi, VAPI_qp_hndl_t qp_hndl, VAPI_qp_num_t  qp_num)                                                                                                
{                                                          
    VAPI_hca_hndl_t nic = ib_bmi->nic; 
    
    VAPI_qp_attr_t       qp_attr;                                                             
    VAPI_qp_attr_mask_t  qp_attr_mask;                                                        
    VAPI_qp_init_attr_t  qp_init_attr;                                                        
    VAPI_ret_t             ret;             
                                                                                                  
    ret = VAPI_query_qp(nic, qp_hndl, &qp_attr, &qp_attr_mask, &qp_init_attr );          
    if (ret != VAPI_OK) {                                                                     
        ompi_output(0, "error querying the queue pair"); 
        return OMPI_ERROR; 
    }                      

    ib_bmi->ib_inline_max = qp_init_attr.cap.max_inline_data_sq;  
    return OMPI_SUCCESS; 
}       

int mca_bmi_ib_qp_init(VAPI_hca_hndl_t nic,
                       VAPI_qp_hndl_t qp_hndl,
                       VAPI_qp_num_t remote_qp,
                       IB_lid_t      remote_lid, 
                       IB_port_t port_id) 
{
    VAPI_ret_t              ret;
    VAPI_qp_attr_t          qp_attr;
    VAPI_qp_attr_mask_t     qp_attr_mask;
    VAPI_qp_cap_t           qp_cap;

    /* Modifying  QP to INIT */
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_INIT;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_STATE);
    qp_attr.pkey_ix = DEFAULT_PKEY_IX;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PKEY_IX);
    qp_attr.port = port_id; 
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PORT);
    qp_attr.remote_atomic_flags = VAPI_EN_REM_WRITE | VAPI_EN_REM_READ;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_REMOTE_ATOMIC_FLAGS);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_RET(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }

    D_PRINT("Modified to init..Qp %d", qp_hndl);

    /**********************  INIT --> RTR  ************************/
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_RTR;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_STATE);
    qp_attr.qp_ous_rd_atom = DEFAULT_QP_OUS_RD_ATOM;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_OUS_RD_ATOM);
    qp_attr.path_mtu = DEFAULT_MTU;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PATH_MTU);
    qp_attr.rq_psn = DEFAULT_PSN;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RQ_PSN);
    qp_attr.pkey_ix = DEFAULT_PKEY_IX;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_PKEY_IX);
    qp_attr.min_rnr_timer = DEFAULT_MIN_RNR_TIMER;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_MIN_RNR_TIMER);

    qp_attr.av.sl = DEFAULT_SERVICE_LEVEL;
    qp_attr.av.grh_flag = FALSE;
    qp_attr.av.static_rate = DEFAULT_STATIC_RATE;
    qp_attr.av.src_path_bits = DEFAULT_SRC_PATH_BITS;

    qp_attr.dest_qp_num = remote_qp;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_DEST_QP_NUM);
    qp_attr.av.dlid = remote_lid;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_AV);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_RET(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }

    D_PRINT("Modified to RTR..Qp %d", qp_hndl);

    /************** RTS *******************/
    QP_ATTR_MASK_CLR_ALL(qp_attr_mask);
    qp_attr.qp_state = VAPI_RTS;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_QP_STATE);
    qp_attr.sq_psn = DEFAULT_PSN;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_SQ_PSN);
    qp_attr.timeout = DEFAULT_TIME_OUT;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_TIMEOUT);
    qp_attr.retry_count = DEFAULT_RETRY_COUNT;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RETRY_COUNT);
    qp_attr.rnr_retry = DEFAULT_RNR_RETRY;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_RNR_RETRY);
    qp_attr.ous_dst_rd_atom = DEFAULT_MAX_RDMA_DST_OPS;
    QP_ATTR_MASK_SET(qp_attr_mask, QP_ATTR_OUS_DST_RD_ATOM);

    ret = VAPI_modify_qp(nic, qp_hndl,
            &qp_attr, &qp_attr_mask, &qp_cap);

    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_RET(ret, "VAPI_modify_qp");
        return OMPI_ERROR;
    }
    D_PRINT("Modified to RTS..Qp %d", qp_hndl);

    


    return OMPI_SUCCESS;
}


/* int mca_bmi_ib_register_mem(VAPI_hca_hndl_t nic, VAPI_pd_hndl_t ptag, */
/*         void* buf, int len, vapi_memhandle_t* memhandle) */
/* { */
/*     VAPI_ret_t ret; */
/*     VAPI_mrw_t mr_in, mr_out; */
/*     vapi_memhandle_t mem_handle; */

/*     mr_in.acl = VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE; */
/*     mr_in.l_key = 0; */
/*     mr_in.r_key = 0; */
/*     mr_in.pd_hndl = ptag; */
/*     mr_in.size = len; */
/*     mr_in.start = (VAPI_virt_addr_t) (MT_virt_addr_t) buf; */
/*     mr_in.type = VAPI_MR; */

/*     ret = VAPI_register_mr(nic, &mr_in, &mem_handle.hndl, &mr_out); */
/*     if(VAPI_OK != ret) { */
/*         MCA_BMI_IB_VAPI_RET(ret, "VAPI_register_mr"); */
/*         return OMPI_ERROR; */
/*     } */

/*     mem_handle.lkey = mr_out.l_key; */
/*     mem_handle.rkey = mr_out.r_key; */

/*     memhandle->lkey = mem_handle.lkey; */
/*     memhandle->rkey = mem_handle.rkey; */

/*     /\* D_PRINT("addr = %p, lkey = %d\n", buf, memhandle->lkey); *\/ */

/*     memhandle->hndl = mem_handle.hndl; */

/*     return OMPI_SUCCESS; */
/* } */


/* int mca_bmi_ib_post_send(mca_bmi_ib_module_t *ib_bmi, */
/*         mca_bmi_ib_endpoint_t *peer,  */
/*         ib_buffer_t *ib_buf, void* addr) */
/* { */
/*     VAPI_ret_t ret; */
/*     int msg_len = ib_buf->desc.sg_entry.len; */

/*     IB_PREPARE_SEND_DESC(ib_buf, (peer->rem_qp_num), */
/*                 msg_len, addr); */

/*     /\* TODO - get this from NIC properties *\/ */
/*     if(msg_len < 128) {  /\* query this information from VAPI_query_qp(property max_inline_data_sq) *\/  */
/*     ret = EVAPI_post_inline_sr(ib_bmi->nic, */
/*             peer->lcl_qp_hndl, */
/*             &ib_buf->desc.sr); */
/*     } else { */
/*     ret = VAPI_post_sr(ib_bmi->nic, */
/*             peer->lcl_qp_hndl, */
/*             &ib_buf->desc.sr); */
/*     } */

/*     if(VAPI_OK != ret) { */
/*         MCA_BMI_IB_VAPI_RET(ret, "VAPI_post_sr"); */
/*         return OMPI_ERROR; */
/*     } */
/*     return OMPI_SUCCESS; */
/* } */


/* void mca_bmi_ib_buffer_repost(VAPI_hca_hndl_t nic, void* addr) */
/* { */
   
/*     mca_bmi_ib_recv_frag_t * frag = (mca_bmi_ib_recv_frag_t*)addr; */

/*     frag->sg_entry.len = frag->size;  */
   

/*     frag->ret = VAPI_post_rr(nic, frag->endpoint->lcl_qp_hndl, &(frag->rr_desc)); */
    
/*     if(VAPI_OK != frag->ret) { */
/*         MCA_BMI_IB_VAPI_RET(frag->ret, "VAPI_post_rr"); */
/*         ompi_output(0, "Error in buffer reposting"); */
/*     } */
/* } */

/* void mca_bmi_ib_prepare_ack(mca_bmi_ib_module_t *ib_bmi, */
/*         void* addr_to_reg, int len_to_reg, */
/*         void* ack_buf, int* len_added) */
/* { */
/*     mca_bmi_ib_mem_registry_info_t *info =  */
/*         mca_bmi_ib_register_mem_with_registry(ib_bmi,  */
/*             addr_to_reg, (size_t)len_to_reg); */

/*     if(NULL == info) { */
/*         ompi_output(0, "Error in registering"); */
/*     } */

/*     A_PRINT("Sending Remote key : %d", info->reply.r_key); */

/*     memcpy(ack_buf,(void*) &(info->reply.r_key), sizeof(VAPI_rkey_t)); */

/*     *len_added = sizeof(VAPI_rkey_t); */
/* } */

/* int mca_bmi_ib_rdma_write(mca_bmi_ib_module_t *ib_bmi, */
/*         mca_bmi_ib_endpoint_t *peer, ib_buffer_t *ib_buf, */
/*         void* send_buf, size_t send_len, void* remote_buf, */
/*         VAPI_rkey_t remote_key, void* id_buf) */
/* { */
/*     VAPI_ret_t ret; */

/*     mca_bmi_ib_mem_registry_info_t *info =  */
/*         mca_bmi_ib_register_mem_with_registry(ib_bmi,  */
/*             send_buf, send_len); */

/*     if (NULL == info) { */
/*         return OMPI_ERROR; */
/*     } */

/*     /\* Prepare descriptor *\/ */
/*     IB_PREPARE_RDMA_W_DESC(ib_buf, (peer->rem_qp_num), */
/*             send_len, send_buf, (info->reply.l_key), remote_key,  */
/*             id_buf, remote_buf); */

/*     ret = VAPI_post_sr(ib_bmi->nic, */
/*             peer->lcl_qp_hndl, */
/*             &ib_buf->desc.sr); */
/*     if(ret != VAPI_OK) { */
/*         MCA_BMI_IB_VAPI_RET(ret, "VAPI_post_sr"); */
/*         return OMPI_ERROR; */
/*     } */

/*     return OMPI_SUCCESS; */
/* } */
