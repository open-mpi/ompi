/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_IBOFFLOAD_ENDPOINT_H
#define MCA_BCOL_IBOFFLOAD_ENDPOINT_H

#include "ompi_config.h"
#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"

#include "ompi/mca/sbgp/ibnet/sbgp_ibnet.h"

#define BCOL_IBOFFLOAD_ENDPOINT_PORT(cgroup, ep) (ep)->ibnet_proc->use_port[(cgroup)->index]
#define BCOL_IBOFFLOAD_ENDPOINT_PORT_IDX(cgroup, ep) (BCOL_IBOFFLOAD_ENDPOINT_PORT(cgroup, ep) - 1)

BEGIN_C_DECLS

struct mca_bcol_iboffload_endpoint_qp_t {
    struct ompi_common_ofacm_base_qp_t *qp;
    size_t ib_inline_max;
    int32_t  sd_wqe;             /* Number of available send wqe entries */
    int32_t  rd_wqe;             /* Number of available recv wqe entries */
    opal_list_t preposted_frags; /* List of preposted frags */
    /* opal_mutex_t lock; */     /* Do I need lock here ? */
};

typedef struct mca_bcol_iboffload_endpoint_qp_t mca_bcol_iboffload_endpoint_qp_t;

enum {
    IBOFFLOAD_CQ_SMALL_MESSAGES = 0,
    IBOFFLOAD_CQ_SYNC,
    IBOFFLOAD_CQ_LARGE_MESSAGES,
    IBOFFLOAD_CQ_LAST
};

/* Endpoint object */
struct mca_bcol_iboffload_endpoint_t {
    opal_list_item_t super;

    /** BTL module that created this connection */
    mca_bcol_iboffload_module_t *iboffload_module;

    /** proc structure corresponding to endpoint */
    mca_sbgp_ibnet_proc_t *ibnet_proc;

    /** lock for concurrent access to endpoint state */
    opal_mutex_t                endpoint_lock;

    /** Penging frag list */
    opal_list_t                 pending_frags;

    /** QPs information */
    mca_bcol_iboffload_endpoint_qp_t *qps;

    /** endpoint index on array */
    int32_t index;

    /** CQ for receive queues on this endpoint */
    struct ibv_cq *recv_cq[IBOFFLOAD_CQ_LAST];

    /** QP configuration information */
    ompi_common_ofacm_base_qp_config_t qp_config;

    /** cpc context */
    ompi_common_ofacm_base_local_connection_context_t *cpc_context;

    /** caching pointer to remote info */
    ompi_common_ofacm_base_remote_connection_context_t *remote_info;

    /** caching pointer to cpc */
    ompi_common_ofacm_base_module_t *endpoint_cpc;

    /** The struct is used for zero RDMA with immediate
        in some collectives, in barrier for example. */
    mca_bcol_iboffload_rdma_info_t remote_zero_rdma_addr;
    mca_bcol_iboffload_rem_rdma_block_t remote_rdma_block;

    /** The pointer to device - In the destruction function
        the iboffload module may not exist any more - caching the device */
    struct mca_bcol_iboffload_device_t *device;

    bool need_toset_remote_rdma_info;

    mca_bcol_iboffload_rdma_info_t remote_rdma_info[MAX_REMOTE_RDMA_INFO];
};
typedef struct mca_bcol_iboffload_endpoint_t mca_bcol_iboffload_endpoint_t;
OBJ_CLASS_DECLARATION(mca_bcol_iboffload_endpoint_t);

/* Function declaration */
int mca_bcol_iboffload_endpoint_init(mca_bcol_iboffload_endpoint_t *ep);

static inline __opal_attribute_always_inline__
    int check_endpoint_state(mca_bcol_iboffload_endpoint_t *ep,
                             mca_bcol_base_descriptor_t *des,
                             opal_list_t *pending_list)
{
    int rc = OMPI_ERR_RESOURCE_BUSY;

    OPAL_THREAD_LOCK(&ep->cpc_context->context_lock);
    /* Adding here one more redirection in critical path. Need to think
     * what is the best way to prevent it */
    switch(ep->cpc_context->state) {
        case MCA_COMMON_OFACM_CLOSED:
            rc = ep->endpoint_cpc->cbm_start_connect(ep->cpc_context);
            if (OMPI_SUCCESS == rc) {
                rc = OMPI_ERR_RESOURCE_BUSY;
            }
            /*
             * As long as we expect a message from the peer (in order
             * to setup the connection) let the event engine pool the
             * OOB events. Note: we increment it once peer active
             * connection.
             */
            opal_progress_event_users_increment();
            /* fall through */
        default:
            /* opal_list_append(pending_list, (opal_list_item_t *)des); */ /* Vasily: will be uncomment later */
            break;
        case MCA_COMMON_OFACM_FAILED:
            rc = OMPI_ERR_UNREACH;
            break;
        case MCA_COMMON_OFACM_CONNECTED:
            rc = OMPI_SUCCESS;
            break;
    }

    OPAL_THREAD_UNLOCK(&ep->cpc_context->context_lock);
    return rc;
}

int mca_bcol_iboffloads_create_endpoints(mca_sbgp_ibnet_connection_group_info_t *cgroup,
        mca_bcol_iboffload_module_t *module);

int mca_bcol_iboffload_endpoint_post_recvs(void *context);

static inline __opal_attribute_always_inline__ int
                            mca_bcol_iboffload_prepost_recv(
                        mca_bcol_iboffload_endpoint_t *endpoint,
                        int qp_index, int num_to_prepost)
{
    mca_bcol_iboffload_prepost_qps_fn_t prepost_recv =
                       mca_bcol_iboffload_component.qp_infos[qp_index].prepost_recv;
    if (NULL != prepost_recv) {
        return prepost_recv(endpoint, qp_index, num_to_prepost);
    }

    return OMPI_SUCCESS;
}

static inline __opal_attribute_always_inline__ int
                    mca_bcol_iboffload_post_ml_scatter_recv_frag(
                        int qp_index, uint32_t dest_rank,
                        int nitems, struct iovec *buff_iovec,
                        uint32_t lkey,
                        struct ibv_sge *sg_entries,
                        mca_bcol_iboffload_frag_t *frag,
                        mca_bcol_iboffload_module_t *iboffload)
{
    int ret, start_wr_index;
    struct ibv_recv_wr *recv_wr, *recv_bad;
    int i;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    mca_bcol_iboffload_endpoint_t *endpoint = iboffload->endpoints[dest_rank];

    mca_bcol_iboffload_recv_wr_manager *recv_wrs = &cm->recv_wrs;
    mca_bcol_iboffload_device_t *device = endpoint->iboffload_module->device;

    IBOFFLOAD_VERBOSE(10, ("Recv prepost call: endpoint %p, qp_index %d",
                          (void *) endpoint, qp_index));

    /* make sure that we do not overrun number of rd_wqe */
    if (0 >= endpoint->qps[qp_index].rd_wqe) {
        IBOFFLOAD_VERBOSE(10, ("There are no rd_wqe - %d",
                                endpoint->qps[qp_index].rd_wqe));

        return 0;
    }

    OPAL_THREAD_LOCK(&recv_wrs->lock);

    /* Calculate start index in array
     * of pre-allocated work requests */
    start_wr_index = cm->qp_infos[qp_index].rd_num - 1;
    recv_wr = &recv_wrs->recv_work_requests[qp_index][start_wr_index];

    IBOFFLOAD_VERBOSE(10, ("Endpoint %p, qp_index - %d, "
                           "start index of WRs - %d", (void *) endpoint,
                            qp_index, start_wr_index));

    for (i = 0; i < nitems; i++) {
        sg_entries[i].length = buff_iovec[i].iov_len;
        sg_entries[i].addr = (uint64_t)buff_iovec[i].iov_base;
        sg_entries[i].lkey = lkey;

       IBOFFLOAD_VERBOSE(10, ("Recv SGE List item %d , length %d , address %p",
                               i, sg_entries[i].length, sg_entries[i].addr));

       IBOFFLOAD_VERBOSE(10, ("Recv SGE List item %d , iovec length %d",
                               i, buff_iovec[i].iov_len));
    }

    recv_wr->num_sge = nitems;
    recv_wr->sg_list = sg_entries;

    /* Set the tail */
    recv_wr->next = NULL;

    /* post the list of recvs */
    ret = ibv_post_recv(endpoint->qps[qp_index].qp->lcl_qp, recv_wr, &recv_bad);
    if (OPAL_UNLIKELY(0 != ret)) {
        IBOFFLOAD_ERROR(("ibv_post_recv failed (%s), error: %s [%d], "
                         "qp_index - %d.\n",
                          ibv_get_device_name(device->dev.ib_dev),
                          strerror(errno), ret, qp_index));

        return -1;
    }

    /* decresing numbers of free recv wqe */
    --endpoint->qps[qp_index].rd_wqe;

    OPAL_THREAD_UNLOCK(&recv_wrs->lock);

    IBOFFLOAD_VERBOSE(10, ("Return success: "
                          "endpoint %p, qp_index %d, dest_rank %d",
                           endpoint, qp_index, dest_rank));

    return 1;
}

static inline __opal_attribute_always_inline__ int
                    mca_bcol_iboffload_prepost_ml_recv_frag(
                        int qp_index, uint32_t dest_rank,
                        mca_bcol_iboffload_frag_t *frag,
                        mca_bcol_iboffload_module_t *iboffload)
{
    int ret, start_wr_index;
    struct ibv_recv_wr *recv_wr, *recv_bad;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    mca_bcol_iboffload_endpoint_t *endpoint = iboffload->endpoints[dest_rank];

    mca_bcol_iboffload_recv_wr_manager *recv_wrs = &cm->recv_wrs;
    mca_bcol_iboffload_device_t *device = endpoint->iboffload_module->device;

    IBOFFLOAD_VERBOSE(10, ("Recv prepost call: endpoint %p, qp_index %d",
                          (void *) endpoint, qp_index));

    /* make sure that we do not overrun number of rd_wqe */
    if (0 >= endpoint->qps[qp_index].rd_wqe) {
        IBOFFLOAD_VERBOSE(10, ("There are no rd_wqe - %d",
                                endpoint->qps[qp_index].rd_wqe));

        return 0;
    }

    OPAL_THREAD_LOCK(&recv_wrs->lock);

    /* Calculate start index in array
     * of pre-allocated work requests */
    start_wr_index = cm->qp_infos[qp_index].rd_num - 1;
    recv_wr = &recv_wrs->recv_work_requests[qp_index][start_wr_index];

    IBOFFLOAD_VERBOSE(10, ("Endpoint %p, qp_index - %d, "
                           "start index of WRs - %d", (void *) endpoint,
                            qp_index, start_wr_index));

    recv_wr->sg_list = &frag->sg_entry;

    /* Set the tail */
    recv_wr->next = NULL;

    /* post the list of recvs */
    ret = ibv_post_recv(endpoint->qps[qp_index].qp->lcl_qp, recv_wr, &recv_bad);
    if (OPAL_UNLIKELY(0 != ret)) {
        IBOFFLOAD_ERROR(("ibv_post_recv failed (%s), error: %s [%d], "
                         "qp_index - %d.\n",
                          ibv_get_device_name(device->dev.ib_dev),
                          strerror(errno), ret, qp_index));

        return -1;
    }

    /* decresing numbers of free recv wqe */
    --endpoint->qps[qp_index].rd_wqe;

    OPAL_THREAD_UNLOCK(&recv_wrs->lock);

    IBOFFLOAD_VERBOSE(10, ("Return success: "
                          "endpoint %p, qp_index %d, dest_rank %d",
                           endpoint, qp_index, dest_rank));

    return 1;
}

static inline __opal_attribute_always_inline__
   mca_bcol_iboffload_frag_t* mca_bcol_iboffload_get_preposted_recv_frag(
                            mca_bcol_iboffload_module_t *iboffload,
                            int source, int qp_index)
{
    mca_bcol_iboffload_frag_t *frag;
    mca_bcol_iboffload_endpoint_t *endpoint = iboffload->endpoints[source];

    frag = mca_bcol_iboffload_component.qp_infos[qp_index].get_preposted_recv(endpoint, qp_index);

    /* do we want to run prepost */
    IBOFFLOAD_VERBOSE(10, ("source - %d, qp_index - %d; "
                          "allocating preposted addr %p.\n",
                           source, qp_index,  (void *) frag->sg_entry.addr));

    if (OPAL_LIKELY(NULL != frag)) {
        frag->next = NULL;
    }

    return frag;
}

END_C_DECLS

#endif /* MCA_BCOL_IBOFFLOAD_ENDPOINT_H */
