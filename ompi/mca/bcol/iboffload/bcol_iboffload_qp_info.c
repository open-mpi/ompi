/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <infiniband/mqe.h>
#include <infiniband/verbs.h>
#include <infiniband/mverbs.h>

#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_device.h"
#include "bcol_iboffload_qp_info.h"
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_endpoint.h"

static int mca_bcol_iboffload_dummy_frag_qp_prepost(
                mca_bcol_iboffload_endpoint_t *endpoint,
                int qp_index, int num_to_prepost)
{
    struct ibv_recv_wr *recv_wr, *recv_bad;
    int ret, num_preposted = 0, start_wr_index;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    mca_bcol_iboffload_recv_wr_manager *recv_wrs = &cm->recv_wrs;

    IBOFFLOAD_VERBOSE(10, ("Recv prepost call: endpoint %p, to prepost %d",
                          (void *) endpoint, num_to_prepost));

    if (OPAL_UNLIKELY(0 == num_to_prepost)) {
        IBOFFLOAD_VERBOSE(10, ("num_to_prepost = 0, return immediate"));
        return OMPI_SUCCESS;
    }

    /* make sure that we do not overrun number of rd_wqe */
    if (num_to_prepost > endpoint->qps[qp_index].rd_wqe) {
        IBOFFLOAD_VERBOSE(10, ("Reset num_to_prepost = %d, to rd_wqe = %d",
                                num_to_prepost, endpoint->qps[qp_index].rd_wqe));

        num_to_prepost = endpoint->qps[qp_index].rd_wqe;
    }

    OPAL_THREAD_LOCK(&recv_wrs->lock);

    /* calculate start index in array
     * of pre-allocated work requests */
    start_wr_index = cm->qp_infos[qp_index].rd_num - num_to_prepost;
    recv_wr = &recv_wrs->recv_work_requests[qp_index][start_wr_index];

    IBOFFLOAD_VERBOSE(10, ("Endpoint %p, qp_index - %d, to_porepost %d, "
                           "start index of WRs - %d, rd_wqe - %d",
                           (void *) endpoint, qp_index, num_to_prepost,
                            start_wr_index, endpoint->qps[qp_index].rd_wqe));

    while (num_preposted < num_to_prepost) {
        /* prepost the special barrier frag to recv queue */
        struct ibv_sge *dummy_sg_entry =
                    &endpoint->iboffload_module->device->dummy_frags[qp_index].sg_entry;

        recv_wr[num_preposted].sg_list = dummy_sg_entry;
        ++num_preposted;
    }

    if (OPAL_LIKELY(num_preposted > 0)) {
        /* Set the tail */
        recv_wr[num_preposted - 1].next = NULL;

        /* post the list of recvs */
        ret = ibv_post_recv(endpoint->qps[qp_index].qp->lcl_qp, recv_wr, &recv_bad);
        if (OPAL_UNLIKELY(0 != ret)) {
            IBOFFLOAD_ERROR(("ibv_post_recv failed, error: %s [%d], "
                             "qp_index - %d.\n", strerror(errno), ret, qp_index));

            return OMPI_ERROR;
        }

        /* recover last recv_wr if needed */
        if (OPAL_UNLIKELY(num_to_prepost != num_preposted)) {
            recv_wr[num_preposted - 1].next = &recv_wr[num_preposted];
        }

        /* decresing numbers of free recv wqe */
        endpoint->qps[qp_index].rd_wqe -= num_preposted;
    }

    OPAL_THREAD_UNLOCK(&recv_wrs->lock);

    IBOFFLOAD_VERBOSE(10, ("Endpoint %p, to_porepost %d, num preposted - %d, qp_index - %d",
                          (void *) endpoint, num_to_prepost, num_preposted, qp_index));

    return OMPI_SUCCESS;
}

/*
 * Receive prepost:
 * return values:
 * 0 - no prepost was done
 * -1 - fatal error during prepost
 * other value - number preposted elements
 */
static int mca_bcol_iboffload_frag_reg_qp_prepost(
                mca_bcol_iboffload_endpoint_t *endpoint,
                int qp_index, int num_to_prepost)
{
    ompi_free_list_item_t *item;
    mca_bcol_iboffload_frag_t *frag;

    struct ibv_recv_wr *recv_wr, *recv_bad;
    int i, ret, num_preposted = 0, start_wr_index;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    mca_bcol_iboffload_device_t *device = endpoint->iboffload_module->device;

    opal_list_t *preposted = &(endpoint->qps[qp_index].preposted_frags);
    mca_bcol_iboffload_recv_wr_manager *recv_wrs = &cm->recv_wrs;

    IBOFFLOAD_VERBOSE(10, ("Recv prepost call: endpoint %p, to prepost %d",
                          (void *) endpoint, num_to_prepost));

    if (OPAL_UNLIKELY(0 == num_to_prepost)) {
        IBOFFLOAD_VERBOSE(10, ("num_to_prepost = 0, return immediate"));
        return OMPI_SUCCESS;
    }

    /* make sure that we do not overrun number of rd_wqe */
    if (num_to_prepost > endpoint->qps[qp_index].rd_wqe) {
        IBOFFLOAD_VERBOSE(10, ("Reset num_to_prepost = %d, to rd_wqe = %d",
                                num_to_prepost, endpoint->qps[qp_index].rd_wqe));

        num_to_prepost = endpoint->qps[qp_index].rd_wqe;
    }

    OPAL_THREAD_LOCK(&recv_wrs->lock);

    /* calculate start index in array
     * of pre-allocated work requests */
    start_wr_index = cm->qp_infos[qp_index].rd_num - num_to_prepost;
    recv_wr = &recv_wrs->recv_work_requests[qp_index][start_wr_index];

    IBOFFLOAD_VERBOSE(10, ("Endpoint %p, qp_index - %d, to_porepost %d, "
                           "start index of WRs - %d, rd_wqe - %d",
                           (void *) endpoint, qp_index, num_to_prepost,
                            start_wr_index, endpoint->qps[qp_index].rd_wqe));

    while (num_preposted < num_to_prepost) {
        /* put the item on list of preposted */
        OMPI_FREE_LIST_GET(&device->frags_free[qp_index], item, ret);
        if (OPAL_UNLIKELY(NULL == item)) {
            break;
        }

        frag = (mca_bcol_iboffload_frag_t *) item;
        opal_list_append(preposted, (opal_list_item_t *) item);

        recv_wr[num_preposted].sg_list = &frag->sg_entry;
        /* TODO Pasha - fix it later */ /* Vasily: Is it right place to take a size value ???? */
        frag->sg_entry.length = cm->qp_infos[qp_index].size;
        ++num_preposted;
    }

    if (OPAL_LIKELY(num_preposted > 0)) {
        /* Set the tail */
        recv_wr[num_preposted - 1].next = NULL;

        /* post the list of recvs */
        ret = ibv_post_recv(endpoint->qps[qp_index].qp->lcl_qp, recv_wr, &recv_bad);
        if (OPAL_UNLIKELY(0 != ret)) {
            IBOFFLOAD_ERROR(("ibv_post_recv failed (%s), error: %s [%d], "
                             "qp_index - %d.\n",
                              ibv_get_device_name(device->dev.ib_dev),
                              strerror(errno), ret, qp_index));

            /* Return allocated frags */
            for (i = 0; i < num_preposted; i++) {
                OMPI_FREE_LIST_RETURN(&device->frags_free[qp_index],
                        (ompi_free_list_item_t *)
                            opal_list_remove_last(preposted));
            }

            return OMPI_ERROR;
        }

        /* recover last recv_wr if needed */
        if (OPAL_UNLIKELY(num_to_prepost != num_preposted)) {
            recv_wr[num_preposted - 1].next = &recv_wr[num_preposted];
        }

        /* decresing numbers of free recv wqe */
        endpoint->qps[qp_index].rd_wqe -= num_preposted;
    }

    OPAL_THREAD_UNLOCK(&recv_wrs->lock);

    IBOFFLOAD_VERBOSE(10, ("Endpoint %p, to_porepost %d, num preposted - %d",
                          (void *) endpoint, num_to_prepost, num_preposted));

    return OMPI_SUCCESS;
}


static void mca_bcol_iboffload_fillin_qp_attr(int qp_index,
                                   mca_bcol_iboffload_endpoint_t *ep,
                                   ompi_common_ofacm_base_qp_config_t *qp_config)
{
        uint32_t max_sge, *init_attr_mask = 
                                  &qp_config->init_attr_mask[qp_index];

        struct ibv_qp_attr *attr = &qp_config->attr[qp_index];
        struct ibv_qp_init_attr *init_attr = &qp_config->init_attr[qp_index];

        mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

        /* Set special init attributes mask */
        *init_attr_mask = IBV_M_QP_EXT_CLASS_1 |
                          IBV_M_QP_EXT_CLASS_2 |
                          IBV_M_QP_EXT_IGNORE_RQ_OVERFLOW;

        /* Set init attributes */
        init_attr->qp_type = IBV_QPT_RC;

/* Vasily: ??????
        init_attr->cap.max_inline_data =
            max_inline_size(qp, iboffload_module->device);
*/
        /* Pasha: we can not leave max_inline empty !
           Todo: copy max_inline_size() from ofacm to
           common area.
         */
        init_attr->cap.max_inline_data = (int32_t) cm->max_inline_data;

        /* We allocate SG list for some algorithms (Bruck's alltoall) */
        max_sge = ep->iboffload_module->group_size / 2 +
                       ep->iboffload_module->group_size % 2;

        /* max send sge should be less than device maximums */
        if (max_sge > (uint32_t)
                             ep->iboffload_module->device->ib_dev_attr.max_sge) {
            max_sge = (uint32_t) ep->iboffload_module->device->ib_dev_attr.max_sge;
        }

        init_attr->cap.max_send_sge = max_sge;
        init_attr->cap.max_recv_sge = max_sge; 
/* Vasily: the value will be changed later */
/* TODO Pasha: this is real crap */
        init_attr->cap.max_recv_wr  = (uint32_t) cm->cq_size;
        init_attr->cap.max_send_wr  = (uint32_t) cm->cq_size;

        /* Set attributes */

        /* attr->pkey_index = 0; */ /* Vasily: ????? */

        attr->port_num = ep->iboffload_module->port;
/* Vasily: the value will be changed later */
        attr->path_mtu = (uint32_t)cm->mtu;

        attr->max_dest_rd_atomic = cm->max_rdma_dst_ops;
        attr->min_rnr_timer = (uint32_t)cm->min_rnr_timer;

        attr->ah_attr.is_global = 0;
        attr->ah_attr.sl = (uint32_t)cm->service_level;
/* Vasily: from struct mca_bcol_iboffload_port_t ????? */
/*
        attr->ah_attr.src_path_bits = iboffload_module->src_path_bits;
*/
        attr->ah_attr.port_num = ep->iboffload_module->port;
        /* JMS to be filled in later dynamically */
        attr->ah_attr.static_rate = 0;
        /* RTS params */
        attr->timeout        = (uint32_t)cm->timeout;
        attr->retry_cnt      = (uint32_t)cm->retry_count;
        attr->rnr_retry      = (uint32_t)cm->rnr_retry;
        attr->max_rd_atomic  = (uint32_t)cm->max_rdma_dst_ops;

        /* Init for local mca_bcol_iboffload_endpoint_qp_t qps structure
         * that caches the qp information on endpoint */
        OBJ_CONSTRUCT(&ep->qps[qp_index].preposted_frags, opal_list_t);

        /* Pasha: Need to add function that will */
        ep->qps[qp_index].ib_inline_max = cm->max_inline_data;
        /* TODO Pasha - this is crap too... we do not have info for sevice qps. Fix it later */

        ep->qps[qp_index].sd_wqe = cm->qp_infos[qp_index].rd_num;
        ep->qps[qp_index].rd_wqe = cm->qp_infos[qp_index].rd_num;

        IBOFFLOAD_VERBOSE(10, ("ep - %p, qp index - %d, num of rd_wqe - %d.",
                               ep, qp_index, ep->qps[qp_index].rd_wqe));
}

static int mca_bcol_iboffload_alloc_reg_qp_resource(int qp_index, mca_bcol_iboffload_device_t *device)
{
    int length;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    ompi_free_list_t *frags_free = &device->frags_free[qp_index];

    OBJ_CONSTRUCT(frags_free, ompi_free_list_t);
    length = cm->qp_infos[qp_index].size;

    IBOFFLOAD_VERBOSE(10, ("free list len %d\n", length));
    if (OMPI_SUCCESS != ompi_free_list_init_ex_new(frags_free,
                sizeof(mca_bcol_iboffload_frag_t), MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                OBJ_CLASS(mca_bcol_iboffload_frag_t),
                length, cm->buffer_alignment,
                cm->free_list_num,
                cm->free_list_max,
                cm->free_list_inc,
                device->mpool,
                mca_bcol_iboffload_frag_init,
                (void *) &cm->qp_infos[qp_index].qp_index)) {
        IBOFFLOAD_ERROR(("Failed to allocate frags_free"));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
static int mca_bcol_iboffload_dealloc_reg_qp_resource(int qp_index, mca_bcol_iboffload_device_t *device)
{
    OBJ_DESTRUCT(&device->frags_free[qp_index]);

    return OMPI_SUCCESS;
}

static mca_bcol_iboffload_frag_t *mca_bcol_iboffload_get_dummy_frag(
                         mca_bcol_iboffload_endpoint_t *ep, int qp_index)
{
    return &ep->iboffload_module->device->dummy_frags[qp_index];
}

static mca_bcol_iboffload_frag_t *mca_bcol_iboffload_endpoint_get_preposted_frag(
                                   mca_bcol_iboffload_endpoint_t *ep, int qp_index)
{
    return (mca_bcol_iboffload_frag_t *)
              opal_list_remove_first(&ep->qps[qp_index].preposted_frags);
}

static void mca_bcol_iboffload_regular_qp_attr(int qp_index,
                                    mca_bcol_iboffload_endpoint_t *ep,
                                    ompi_common_ofacm_base_qp_config_t *qp_config)
{
    struct ibv_qp_init_attr *init_attr = &qp_config->init_attr[qp_index];

    mca_bcol_iboffload_fillin_qp_attr(qp_index, ep, qp_config);

    init_attr->send_cq = ep->iboffload_module->device->ib_cq;
    init_attr->recv_cq = ep->recv_cq[IBOFFLOAD_CQ_SMALL_MESSAGES];
}

static void mca_bcol_iboffload_large_buff_qp_attr(int qp_index,
                                    mca_bcol_iboffload_endpoint_t *ep,
                                    ompi_common_ofacm_base_qp_config_t *qp_config)
{
    struct ibv_qp_init_attr *init_attr = &qp_config->init_attr[qp_index];

    mca_bcol_iboffload_fillin_qp_attr(qp_index, ep, qp_config);

    init_attr->send_cq = ep->iboffload_module->device->ib_cq;
    init_attr->recv_cq = ep->recv_cq[IBOFFLOAD_CQ_LARGE_MESSAGES];
}

static void mca_bcol_iboffload_sync_qp_attr(int qp_index,
                                    mca_bcol_iboffload_endpoint_t *ep,
                                    ompi_common_ofacm_base_qp_config_t *qp_config)
{
    struct ibv_qp_init_attr *init_attr = &qp_config->init_attr[qp_index];

    mca_bcol_iboffload_fillin_qp_attr(qp_index, ep, qp_config);

    init_attr->send_cq = ep->iboffload_module->device->ib_cq;
    init_attr->recv_cq = ep->recv_cq[IBOFFLOAD_CQ_SYNC];
}

static int mca_bcol_iboffload_setup_barrier_qp(mca_bcol_iboffload_qp_info_t* qp_info)
{
    qp_info->config_qp = mca_bcol_iboffload_regular_qp_attr;
    qp_info->prepost_recv = mca_bcol_iboffload_dummy_frag_qp_prepost;

    qp_info->alloc_resource = NULL;
    qp_info->dealloc_resource = NULL;

    qp_info->get_preposted_recv = mca_bcol_iboffload_get_dummy_frag;

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_setup_regular_qp(mca_bcol_iboffload_qp_info_t* qp_info)
{
    qp_info->config_qp = mca_bcol_iboffload_regular_qp_attr;
    qp_info->prepost_recv = mca_bcol_iboffload_frag_reg_qp_prepost;

    qp_info->alloc_resource = mca_bcol_iboffload_alloc_reg_qp_resource;
    qp_info->dealloc_resource = mca_bcol_iboffload_dealloc_reg_qp_resource;

    qp_info->get_preposted_recv = mca_bcol_iboffload_endpoint_get_preposted_frag;

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_setup_large_buff_qp(mca_bcol_iboffload_qp_info_t* qp_info)
{
    qp_info->config_qp = mca_bcol_iboffload_large_buff_qp_attr;

    qp_info->prepost_recv = NULL; /* We use "manual" ML frag preposting for this QP */
    qp_info->alloc_resource = NULL;
    qp_info->dealloc_resource = NULL;
    qp_info->get_preposted_recv = NULL;

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_setup_credit_qp(mca_bcol_iboffload_qp_info_t* qp_info)
{
    qp_info->config_qp = mca_bcol_iboffload_large_buff_qp_attr;
    qp_info->prepost_recv = mca_bcol_iboffload_dummy_frag_qp_prepost;

    qp_info->alloc_resource = NULL;
    qp_info->dealloc_resource = NULL;

    qp_info->get_preposted_recv = mca_bcol_iboffload_get_dummy_frag;

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_setup_sync_qp(mca_bcol_iboffload_qp_info_t* qp_info)
{
    qp_info->config_qp = mca_bcol_iboffload_sync_qp_attr;
    qp_info->prepost_recv = mca_bcol_iboffload_dummy_frag_qp_prepost;

    qp_info->alloc_resource = NULL;
    qp_info->dealloc_resource = NULL;

    qp_info->get_preposted_recv = mca_bcol_iboffload_get_dummy_frag;

    return OMPI_SUCCESS;
}

mca_bcol_iboffload_setup_qps_fn_t setup_qps_fn[MCA_BCOL_IBOFFLOAD_QP_LAST] = {
    mca_bcol_iboffload_setup_barrier_qp,    /* MCA_BCOL_IBOFFLOAD_QP_BARRIER */
    mca_bcol_iboffload_setup_regular_qp,    /* MCA_BCOL_IBOFFLOAD_QP_REGULAR */
    mca_bcol_iboffload_setup_sync_qp,       /* MCA_BCOL_IBOFFLOAD_QP_SYNC */
    mca_bcol_iboffload_setup_credit_qp,     /* MCA_BCOL_IBOFFLOAD_QP_CREDIT */
    mca_bcol_iboffload_setup_large_buff_qp, /* MCA_BCOL_IBOFFLOAD_QP_LARGE_BUFF */
    /* MCA_BCOL_IBOFFLOAD_QP_LAST */
};
