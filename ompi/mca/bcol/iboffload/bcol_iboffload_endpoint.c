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

#include <infiniband/mverbs.h>

#include "ompi/constants.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/common/ofacm/connect.h"

#include "opal/threads/mutex.h"
#include "opal/class/opal_object.h"

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_device.h"
#include "bcol_iboffload_endpoint.h"

static void mca_bcol_iboffload_endpoint_construct(mca_bcol_iboffload_endpoint_t *ep)
{
    ep->iboffload_module = NULL;
    ep->ibnet_proc = NULL;

    ep->qps = (mca_bcol_iboffload_endpoint_qp_t *)
              calloc(mca_bcol_iboffload_component.num_qps,
                     sizeof(mca_bcol_iboffload_endpoint_qp_t));

    ep->index = 0;
    OBJ_CONSTRUCT(&ep->endpoint_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ep->pending_frags, opal_list_t);

    memset(ep->recv_cq, 0, IBOFFLOAD_CQ_LAST * sizeof(ep->recv_cq[0]));
    memset(&ep->qp_config, 0, sizeof(ompi_common_ofacm_base_qp_config_t));

    ep->cpc_context = NULL;

    memset(&ep->remote_zero_rdma_addr, 0, sizeof(mca_bcol_iboffload_rdma_info_t));
    memset(&ep->remote_rdma_block, 0, sizeof(mca_bcol_iboffload_rem_rdma_block_t));

    ep->need_toset_remote_rdma_info = false;
}

static void mca_bcol_iboffload_endpoint_destruct(mca_bcol_iboffload_endpoint_t *ep)
{
    int qp_index, num_qps, i;
    ompi_free_list_item_t *item;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    num_qps = cm->num_qps;

    IBOFFLOAD_VERBOSE(10, ("Destruct: ep - %p, ep->index - %d", ep, ep->index));

    if (NULL != ep->qps) {
        for (qp_index = 0; qp_index < num_qps; ++qp_index) {
            do {
                item = (ompi_free_list_item_t *)
                    opal_list_remove_first(&ep->qps[qp_index].preposted_frags);
                if(OPAL_LIKELY(NULL != item)) {
                    OMPI_FREE_LIST_RETURN(&ep->device->frags_free[qp_index], item);
                }
            } while (NULL != item);

            OBJ_DESTRUCT(&ep->qps[qp_index].preposted_frags);
        }

        free(ep->qps);
    }

    OBJ_DESTRUCT(&ep->endpoint_lock);
    OBJ_DESTRUCT(&ep->pending_frags);

    /* If the CPC has an endpoint_finalize function, call it */
    if (NULL != ep->endpoint_cpc->cbm_endpoint_finalize) {
        ep->endpoint_cpc->cbm_endpoint_finalize(ep->cpc_context);
    }

    for (i = 0; i < IBOFFLOAD_CQ_LAST; i++) {
        if (NULL != ep->recv_cq[i]) {
            if (ibv_destroy_cq(ep->recv_cq[i])) {
                IBOFFLOAD_ERROR(("Endpoint %x "
                            ", failed to destroy CQ, errno says %s",
                            ep, strerror(errno)));
            }
        }
    }
}

OBJ_CLASS_INSTANCE(mca_bcol_iboffload_endpoint_t,
        opal_list_item_t,
        mca_bcol_iboffload_endpoint_construct,
        mca_bcol_iboffload_endpoint_destruct);

/* Pasha: Add some error message here */

/*
 * Called when the CPC has established a connection on an endpoint
 */
static void mca_bcol_iboffload_endpoint_invoke_error(void *context)
{
    mca_bcol_iboffload_endpoint_t *endpoint = (mca_bcol_iboffload_endpoint_t *) context;
    IBOFFLOAD_ERROR(("Getting error on endpoint - %p!", endpoint));
}


/* Pasha: Need to add more logic here */
static void mca_bcol_iboffload_endpoint_cpc_complete(void *context)
{
    mca_bcol_iboffload_endpoint_t *endpoint = (mca_bcol_iboffload_endpoint_t *) context;

    IBOFFLOAD_VERBOSE(10, ("Endpoint - %p for comm rank %d: CPC complete.\n",
                           endpoint, endpoint->iboffload_module->ibnet->super.group_list[endpoint->index]));

    if (OMPI_SUCCESS !=
            mca_bcol_iboffload_exchange_rem_addr(endpoint)) {
        IBOFFLOAD_ERROR(("endpoint - %p, "
                    "remote addr exchange error.\n", endpoint));
    }
    /* The connection is correctly setup. Now we can decrease the
       event trigger. */
    opal_progress_event_users_decrement();
}

/* Vasily: Need to add more logic here */
int mca_bcol_iboffload_endpoint_post_recvs(void *context)
{
    int qp_index, rc, num_qps;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    mca_bcol_iboffload_endpoint_t *endpoint =
                             (mca_bcol_iboffload_endpoint_t *) context;

    IBOFFLOAD_VERBOSE(10, ("endpoint - %p, post of %d recvs !",
                            endpoint, cm->qp_infos[0].rd_num));
    /* TODO Pasha - fix later */
    num_qps = cm->num_qps;
    for (qp_index = 0; qp_index < num_qps; ++qp_index) {
        rc = mca_bcol_iboffload_prepost_recv(endpoint, qp_index,
                                             cm->qp_infos[qp_index].rd_num);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            /* Pasha: Need to add more failure logic */
            IBOFFLOAD_ERROR(("Failed to prepost recv fragments "
                             "on qp index %d, return code - %d",
                              qp_index, rc));

            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

/* The function go over each ibnet proc and creates endpoint for each one */
int mca_bcol_iboffloads_create_endpoints(mca_sbgp_ibnet_connection_group_info_t *cgroup,
        mca_bcol_iboffload_module_t *module) {
    uint32_t i;
    mca_bcol_iboffload_endpoint_t *ep;

    if (NULL == cgroup || NULL == module) {
        IBOFFLOAD_ERROR(("Bad parameters for create endpoints function."));
        return OMPI_ERROR;
    }

    module->num_endpoints = cgroup->num_procs;
    module->endpoints = (mca_bcol_iboffload_endpoint_t **)
                            calloc(module->num_endpoints,
                                   sizeof(mca_bcol_iboffload_endpoint_t *));
    if (NULL == module->endpoints) {
        IBOFFLOAD_ERROR(("Error memory allocation for endpoints array"
                         ", errno says %s", strerror(errno)));
        return OMPI_ERROR;
    }

    IBOFFLOAD_VERBOSE(10, ("iboffload - %p, num of endpoints - %d.\n",
                            module, module->num_endpoints));
/* Ishai: No need to open so many endpoints. We are not talking with all procs */
    for (i = 0; i < cgroup->num_procs; i++) {
        ep = OBJ_NEW(mca_bcol_iboffload_endpoint_t);
        /* check qp memory allocation */
        if (NULL == ep->qps) {
            IBOFFLOAD_ERROR(("Failed to allocate memory for qps"));
            return OMPI_ERROR;
        }
        /* init new endpoint */
        ep->index = i;
        ep->iboffload_module = module;
        /* saving the device for the destruction - iboffload module amy not exist than */
        ep->device = ep->iboffload_module->device;
        ep->ibnet_proc = (mca_sbgp_ibnet_proc_t *)
            opal_pointer_array_get_item(cgroup->ibnet_procs, i);
        if (NULL == ep->ibnet_proc) {
            IBOFFLOAD_ERROR(("Failed to get proc pointer, for index %d", i));
            return OMPI_ERROR;
        }

        if (OMPI_SUCCESS !=
            mca_bcol_iboffload_endpoint_init(ep)) {
            IBOFFLOAD_ERROR(("Failed to init endpoint - %p", ep));
            return OMPI_ERROR;
        }

        IBOFFLOAD_VERBOSE(10, ("Endpoint - %p, ep index - %d, iboffload - %p, "
                               "cpc contex - %p.\n", ep, ep->index,
                                ep->iboffload_module, ep->cpc_context));

        /* Add the new endpoint to array of endpoints */
        module->endpoints[i] = ep;
    }

    /* Pasha: Need to add better clean-up here */
    return OMPI_SUCCESS;
}

static int config_qps(mca_bcol_iboffload_endpoint_t *ep)
{
    int qp_index;
    int ret = OMPI_SUCCESS;

    ompi_common_ofacm_base_qp_config_t *qp_config = &ep->qp_config;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    qp_config->num_srqs = 0;
    qp_config->srq_num = NULL;

    qp_config->num_qps = cm->num_qps;

    qp_config->init_attr = (struct ibv_qp_init_attr *)
            calloc(qp_config->num_qps, sizeof(struct ibv_qp_init_attr));

    if (NULL == qp_config->init_attr) {
        IBOFFLOAD_ERROR(("Failed allocate memory for qp init attributes"));
        ret = OMPI_ERR_OUT_OF_RESOURCE;

        goto config_qps_exit;
    }

    qp_config->attr = (struct ibv_qp_attr *)
        calloc(qp_config->num_qps, sizeof(struct ibv_qp_attr));

    if (OPAL_UNLIKELY(NULL == qp_config->attr)) {
        IBOFFLOAD_ERROR(("Failed allocate memory for qp attributes"));
        ret = OMPI_ERR_OUT_OF_RESOURCE;

        goto config_qps_exit;
    }

    /* we must to specify that the qps are special */
    qp_config->init_attr_mask = (uint32_t *)
        calloc(qp_config->num_qps, sizeof(uint32_t));

    if (OPAL_UNLIKELY(NULL == qp_config->init_attr_mask)) {
        IBOFFLOAD_ERROR(("Failed allocate memory for qp mask."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;

        goto config_qps_exit;
    }

    /* qp_config->rtr_attr_mask = qp_config->rts_attr_mask = NULL; */

    qp_config->rtr_attr_mask = (uint32_t *)
        calloc(qp_config->num_qps, sizeof(uint32_t));

    if (OPAL_UNLIKELY(NULL == qp_config->rtr_attr_mask)) {
        IBOFFLOAD_ERROR(("Failled allocate memory for qp rtr attributes mask."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;

        goto config_qps_exit;
    }

    qp_config->rts_attr_mask = (uint32_t *)
        calloc(qp_config->num_qps, sizeof(uint32_t));

    if (OPAL_UNLIKELY(NULL == qp_config->rts_attr_mask)) {
        IBOFFLOAD_ERROR(("Failled allocate memory for qp rts attributes mask."));
        ret = OMPI_ERR_OUT_OF_RESOURCE;

        goto config_qps_exit;
    }

    for (qp_index = 0; qp_index < qp_config->num_qps; ++qp_index) {
        mca_bcol_iboffload_config_qps_fn_t config_qp =
                                 cm->qp_infos[qp_index].config_qp;

        if (NULL != config_qp) {
            config_qp(qp_index, ep, qp_config);
        }
    }

config_qps_exit:
    return ret;
}

/* The fucntion is called for endpoints
 * with MCA_COMMON_OFACM_USER_CUSTOM state only,
 * we need a OPAL_THREAD_LOCK before call to this function */
int mca_bcol_iboffload_endpoint_init(mca_bcol_iboffload_endpoint_t *ep)
{
    int qp_index, cq_index, num_qps;
    ompi_common_ofacm_base_module_t *cpc;

    mca_bcol_iboffload_device_t *device = ep->iboffload_module->device;

    mca_sbgp_ibnet_connection_group_info_t *cgroup =
                &ep->iboffload_module->ibnet->cgroups[ep->iboffload_module->cgroup_index];

    for (cq_index = 0; cq_index < IBOFFLOAD_CQ_LAST; cq_index++) {
        if (OMPI_SUCCESS !=
                mca_bcol_iboffload_adjust_cq(device, &ep->recv_cq[cq_index])) {
            IBOFFLOAD_ERROR(("Error creating CQ for %s errno says %s",
                        ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
            /* OBJ_RELEASE(ep); */ /* Vasily: What must we do in this case ??? */
            return OMPI_ERROR;
        }
    }

    if (OPAL_UNLIKELY(OMPI_SUCCESS != config_qps(ep))) {
        IBOFFLOAD_ERROR(("Error configure QPs for endpoint %x errno says %s",
                                                           ep, strerror(errno)));
        return OMPI_ERROR;
    }

    /* Adding here one more redirection in critical path. Need to think
     * what is the best way to prevent it */

    IBOFFLOAD_VERBOSE(10, ("Endpoint - %p, rem port - %d", ep,
            ep->ibnet_proc->remote_ports_info[BCOL_IBOFFLOAD_ENDPOINT_PORT_IDX(cgroup, ep)].id));

    cpc = ep->ibnet_proc->remote_ports_info[BCOL_IBOFFLOAD_ENDPOINT_PORT_IDX(cgroup, ep)].local_cpc;
    ep->endpoint_cpc = cpc; /* caching pointer to cpc */

    if (NULL != cpc->cbm_endpoint_init) {
        ep->cpc_context = cpc->cbm_endpoint_init(
                ep->ibnet_proc->ompi_proc,
                &ep->qp_config,
                device->ib_pd,
                ep->iboffload_module->subnet_id,
                ep->iboffload_module->ibnet->group_id,
                ep->iboffload_module->lid,
                /* Remote lid of target module */
                ep->ibnet_proc->remote_ports_info[BCOL_IBOFFLOAD_ENDPOINT_PORT_IDX(cgroup, ep)].lid,
                ep->index,   /* user context index */
                (void *) ep,  /* user context */
                cpc,
                mca_bcol_iboffload_endpoint_cpc_complete,
                mca_bcol_iboffload_endpoint_invoke_error,
                mca_bcol_iboffload_endpoint_post_recvs);

        if (OPAL_UNLIKELY(NULL == ep->cpc_context)) {
            IBOFFLOAD_ERROR(("Endpoint - %p, failed to init context", ep));
            /* OBJ_RELEASE(ep); */ /* Vasily: What must we do in this case ??? */
            return OMPI_ERROR;
        }

        /* Updating remote port info */
        num_qps = mca_bcol_iboffload_component.num_qps;

        ep->remote_info = &ep->cpc_context->remote_info;
        for (qp_index = 0; qp_index < num_qps; ++qp_index) {
            ep->qps[qp_index].qp = &ep->cpc_context->qps[qp_index];
        }
    }

    return OMPI_SUCCESS;
}
