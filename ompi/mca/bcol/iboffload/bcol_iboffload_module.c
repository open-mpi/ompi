/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>

#include <infiniband/mqe.h>
#include <infiniband/verbs.h>
#include <infiniband/mverbs.h>

#include "opal/util/arch.h"
#include "opal/include/opal/types.h"
#include "opal/datatype/opal_datatype.h"

#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/mpool/grdma/mpool_grdma.h"
#include "ompi/mca/coll/ml/coll_ml_allocation.h"

#include "bcol_iboffload.h"
#include "bcol_iboffload_frag.h"
#include "bcol_iboffload_task.h"
#include "bcol_iboffload_bcast.h"
#include "bcol_iboffload_device.h"
#include "bcol_iboffload_collreq.h"
#include "bcol_iboffload_collfrag.h"
#include "bcol_iboffload_endpoint.h"

static int init_rdma_buf_desc(mca_bcol_iboffload_rdma_buffer_desc_t **desc, void *base_addr, uint32_t num_banks,
        uint32_t num_buffers_per_bank, uint32_t size_buffer, uint32_t header_size);

static int set_endpoint_remote_rdma_info(mca_bcol_iboffload_endpoint_t *ep, mca_bcol_iboffload_rdma_info_t *remote_rdma_info);

static void
mca_bcol_iboffload_module_construct(mca_bcol_iboffload_module_t *module)
{
    int i;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    /* set all to zero */
    module->group_size      = 0;
    module->segment_size    = 0;
    module->collective_tag  = 0;
    module->ibnet           = NULL;
    module->cgroup_index    = 0;

    module->num_endpoints   = 0;
    module->endpoints       = NULL;

    /* initi the previous sequence number */
    module->prev_sequence_num = -1;

    switch (cm->barrier_mode) {
        case (0): module->barrier_algth =
                  mca_bcol_iboffload_barrier_intra_recursive_doubling_start;
                  break;
        case (1): module->barrier_algth =
                  mca_bcol_iboffload_barrier_intra_recursive_knomial_start;
                  break;
        default: module->barrier_algth = NULL;
    }

    module->allreduce_algth = mca_bcol_iboffload_allreduce_first_call;
    module->fanin_algth     = mca_bcol_iboffload_new_style_fanin_first_call;
    module->fanout_algth    = mca_bcol_iboffload_new_style_fanout_first_call;
    module->memsync_algth   = mca_bcol_iboffload_nb_memory_service_barrier_start;

    memset(module->mq, 0, sizeof(module->mq[0]) * BCOL_IBOFFLOAD_MQ_NUM);
    memset(module->alg_task_consump, 0, sizeof(uint32_t) * LAST_ALG);
    memset(module->connection_status, 0, sizeof(bool) * LAST_ALG);

    for (i = 0; i < BCOL_IBOFFLOAD_MQ_NUM; i++) {
        module->mq_credit[i] = mca_bcol_iboffload_component.max_mqe_tasks;
    }

    module->super.bcol_component =
                (mca_bcol_base_component_t *) &mca_bcol_iboffload_component;

    /* We need two MQ's tasks for exchange with remote addresses */
    module->alg_task_consump[REMOTE_EXCHANGE_ALG] += 2;

    module->power_of_2_ranks = 0;
    /* it is safe to set all the remote block to zero */
    memset(&module->rdma_block, 0, sizeof(mca_bcol_iboffload_local_rdma_block_t));

    module->super.list_n_connected = NULL;

    OBJ_CONSTRUCT(&module->collfrag_pending, opal_list_t);
}

static void
mca_bcol_iboffload_module_destruct(mca_bcol_iboffload_module_t *module)
{
    int i = 0;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    IBOFFLOAD_VERBOSE(10, ("Module - %p: start to destroy; "
                           "pending queue size - %d.\n",
                            module, opal_list_get_size(&module->collfrag_pending)));

    /* Make sure that we done with all pending collective frags */
    while (opal_list_get_size(&module->collfrag_pending) > 0) {
        opal_progress();
    }

    OBJ_DESTRUCT(&module->collfrag_pending);

    IBOFFLOAD_VERBOSE(10, ("module->mq_credit - %d, cm->max_mqe_tasks - %d.\n",
                           module->mq_credit[0], cm->max_mqe_tasks));
    /* Make sure that you got completion on all outstanding collectives */
    for (i = 0; i < BCOL_IBOFFLOAD_MQ_NUM; i++) {
        while (module->mq_credit[i] != (int) cm->max_mqe_tasks) {
            opal_progress();
        }
    }

    IBOFFLOAD_VERBOSE(10, ("All credits were returned.\n"));

    if (NULL != module && NULL != module->mq) {
        for (i = 0; i < BCOL_IBOFFLOAD_MQ_NUM; i++) {
            if (0 != mqe_context_destroy(module->mq[i])) {
                IBOFFLOAD_ERROR(("Error destroying MQ for device (%s), error: %s\n",
                            ibv_get_device_name(module->device->dev.ib_dev), strerror(errno)));
            }
        }

        IBOFFLOAD_VERBOSE(10, ("MQ %d was destroyed.\n", i));
    }

    if (NULL != module->endpoints) {
        mca_bcol_iboffload_endpoint_t *ep;
        int qp_index, num_qps = cm->num_qps;

        for (i = 0; i < module->num_endpoints; ++i) {
            if (NULL != module->endpoints[i]) {
                    /* Make sure that we get completions on all outstanding send requests */
                    ep = module->endpoints[i];
                    for (qp_index = 0; qp_index < num_qps; ++qp_index) {
                        IBOFFLOAD_VERBOSE(10, ("qp_index - %d, ep->index - %d, "
                                               "ep->qps[qp_index].sd_wqe - %d, "
                                               "cm->qp_infos[qp_index].rd_num - %d.\n",
                                                qp_index, ep->index,
                                                ep->qps[qp_index].sd_wqe,
                                                cm->qp_infos[qp_index].rd_num));

                        while (ep->qps[qp_index].sd_wqe != cm->qp_infos[qp_index].rd_num) {
                            opal_progress();
                        }

                        IBOFFLOAD_VERBOSE(10, ("qp_index - %d, ep->index - %d; "
                                               "All sends were sent.\n",
                                                qp_index, ep->index));
                    }

                    OBJ_RELEASE(ep);
            }
        }

        free(module->endpoints);
    }

    netpatterns_free_recursive_doubling_tree_node(&module->n_exchange_tree);
    netpatterns_free_recursive_doubling_tree_node(&module->recursive_doubling_tree);

    OBJ_RELEASE(module->device->net_context);
    OBJ_RELEASE(module->device);

    if (NULL != module->super.list_n_connected) {
        free(module->super.list_n_connected);
        module->super.list_n_connected = NULL;
    }

    OBJ_DESTRUCT(&module->iovec_tasks_free);

    IBOFFLOAD_VERBOSE(10, ("module - %p was successfully destructed.\n", module));
}

OBJ_CLASS_INSTANCE(mca_bcol_iboffload_module_t,
                   mca_bcol_base_module_t,
                   mca_bcol_iboffload_module_construct,
                   mca_bcol_iboffload_module_destruct);

static int iboffload_init_port(struct mca_bcol_iboffload_device_t *device,
                               struct mca_bcol_iboffload_port_t *p)
{
    union ibv_gid gid;
    struct ibv_port_attr ib_port_attr;

    if (ibv_query_port(device->dev.ib_dev_context, p->id, &ib_port_attr)){
        IBOFFLOAD_ERROR(("Error getting port attributes for device %s "
                    "port number %d errno says %s",
                    ibv_get_device_name(device->dev.ib_dev), p->id, strerror(errno)));
        return OMPI_ERR_NOT_FOUND;
    }

    /* Set port data */
    p->lmc  = (1 << ib_port_attr.lmc);
    p->lid  = ib_port_attr.lid;
    p->stat = ib_port_attr.state;
    p->mtu  = ib_port_attr.active_mtu;

    IBOFFLOAD_VERBOSE(10, (" Setting port data (%s:%d) lid=%d, lmc=%d, stat=%d, mtu=%d\n",
                ibv_get_device_name(device->dev.ib_dev), p->id, p->lid,
                p->lmc, p->stat, p->mtu));

    if (0 != ibv_query_gid(device->dev.ib_dev_context, p->id, 0, &gid)) {
        IBOFFLOAD_ERROR(("ibv_query_gid failed (%s:%d)\n",
                    ibv_get_device_name(device->dev.ib_dev), p->id));
        return OMPI_ERR_NOT_FOUND;
    }

    /* set subnet data */
    p->subnet_id = ntoh64(gid.global.subnet_prefix);
    IBOFFLOAD_VERBOSE(10, ("my IB-only subnet_id for HCA %s port %d is %lx",
                ibv_get_device_name(device->dev.ib_dev), p->id, p->subnet_id));

    return OMPI_SUCCESS;
}

/* mpool allocation maybe changed in future, so lets keep it as separate function */
static int prepare_mpool(mca_bcol_iboffload_device_t *device)
{
    int ret = OMPI_SUCCESS;
    mca_mpool_base_resources_t resources;

    resources.reg_data = (void *) device;
    resources.sizeof_reg = sizeof(mca_bcol_iboffload_reg_t);

    resources.register_mem = mca_bcol_iboffload_register_mr;
    resources.deregister_mem = mca_bcol_iboffload_deregister_mr;

    device->mpool =
        mca_mpool_base_module_create(mca_bcol_iboffload_component.mpool_name,
                device, &resources);
    if (NULL == device->mpool){
        opal_output(0, "error creating IB memory pool for %s errno says %s\n",
                ibv_get_device_name(device->dev.ib_dev), strerror(errno));
        ret = OMPI_ERROR;
    }

    return ret;
}

/* Allocate device related resources: mpool, pd, cq, free_lists */
static int allocate_device_resources(mca_bcol_iboffload_device_t *device)
{
    int qp_index, num_qps, rc;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    void* dummy_mem = (void *) &device->dummy_mem[0];

    num_qps = cm->num_qps;

    /* We have some active ports, alloce pd */
    device->ib_pd = ibv_alloc_pd(device->dev.ib_dev_context);
    if (NULL == device->ib_pd){
        IBOFFLOAD_ERROR(("Error allocating protection domain for %s errno says %s",
                    ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        return OMPI_ERROR;
    }

    /* Pasha: allocate mpool here */
    if (OMPI_SUCCESS != prepare_mpool(device)) {
        return OMPI_ERROR;
    }

    /* Allocating free list of memory registered fragments */
    device->frags_free = (ompi_free_list_t *) calloc(
                                num_qps, sizeof(ompi_free_list_t));

    if (NULL == device->frags_free) {
        IBOFFLOAD_ERROR(("Error allocating memory for "
                         "frags array, dev: %s errno says %s",
                          ibv_get_device_name(device->dev.ib_dev),
                          strerror(errno)));

        return OMPI_ERROR;
    }

    for (qp_index = 0; qp_index < num_qps; ++qp_index) {
        mca_bcol_iboffload_alloc_qps_resource_fn_t alloc_resource =
                                      cm->qp_infos[qp_index].alloc_resource;

        if (NULL != alloc_resource) {
            if (OMPI_SUCCESS != alloc_resource(qp_index, device)) {
                return OMPI_ERROR;
            }
        }

    }

    if (OMPI_SUCCESS !=
            mca_bcol_iboffload_adjust_cq(device, &device->ib_cq)) {
        IBOFFLOAD_ERROR(("Error creating CQ for %s errno says %s",
                    ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS !=
            mca_bcol_iboffload_adjust_cq(device, &device->ib_mq_cq)) {
        IBOFFLOAD_ERROR(("Error creating mq CQ for %s errno says %s",
                    ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        return OMPI_ERROR;
    }

    rc = mca_bcol_iboffload_register_mr((void *) device, dummy_mem,
                                        sizeof(char) * BCOL_IBOFFLOAD_DUMMY_MEM_SIZE,
                                        &device->dummy_reg.base);

    if (OMPI_SUCCESS != rc) {
        IBOFFLOAD_ERROR(("Dummy memory registration failed for %s errno says %s",
                          ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        return OMPI_ERROR;
    }

    for (qp_index = 0; qp_index < num_qps; ++qp_index) {
        mca_bcol_iboffload_frag_t *frag = &device->dummy_frags[qp_index];

        memset(&frag->super.registration, 0, sizeof(mca_mpool_base_registration_t));
        OBJ_CONSTRUCT(frag, mca_bcol_iboffload_frag_t);

        frag->qp_index = qp_index;
        frag->type = MCA_BCOL_IBOFFLOAD_DUMMY_OWNER;

        frag->registration = &device->dummy_reg;

        frag->super.ptr = dummy_mem;
        frag->super.registration = &device->dummy_reg.base;

        frag->sg_entry.length = 0;
        frag->sg_entry.lkey = device->dummy_reg.mr->lkey;
        frag->sg_entry.addr = (uint64_t) (uintptr_t) dummy_mem;
    }

    return OMPI_SUCCESS;
}

/* Register memory */
int mca_bcol_iboffload_register_mr(void *reg_data, void *base, size_t size,
                                    mca_mpool_base_registration_t *reg)
{
    mca_bcol_iboffload_device_t *device = (mca_bcol_iboffload_device_t *) reg_data;
    mca_bcol_iboffload_reg_t *iboffload_reg = (mca_bcol_iboffload_reg_t *) reg;

    iboffload_reg->mr = ibv_reg_mr(device->ib_pd, base, size,
            IBV_ACCESS_LOCAL_WRITE |
            IBV_ACCESS_REMOTE_WRITE |
            IBV_ACCESS_REMOTE_READ);

    if (NULL == iboffload_reg->mr) {
        IBOFFLOAD_ERROR(("Device %s: %p addr, %d bytes registration failed.",
                          ibv_get_device_name(device->dev.ib_dev), base, size));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    IBOFFLOAD_VERBOSE(10, ("Device %s: memory register addr=%p, len=%d, mr - %p.",
                ibv_get_device_name(device->dev.ib_dev), base, size, iboffload_reg->mr));

    return OMPI_SUCCESS;
}

/* Deregister memory */
int mca_bcol_iboffload_deregister_mr(void *reg_data, mca_mpool_base_registration_t *reg)
{
    mca_bcol_iboffload_device_t *device = (mca_bcol_iboffload_device_t *) reg_data;
    mca_bcol_iboffload_reg_t *iboffload_reg = (mca_bcol_iboffload_reg_t *) reg;

    IBOFFLOAD_VERBOSE(10, ("Device %s: mr - %p.",
                ibv_get_device_name(device->dev.ib_dev), iboffload_reg->mr));

    if (NULL != iboffload_reg->mr) {
        if (ibv_dereg_mr(iboffload_reg->mr)) {
            IBOFFLOAD_ERROR(("Device %s: error unpinning iboffload memory errno says %s",
                        ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
            return OMPI_ERROR;
        }
    }

    IBOFFLOAD_VERBOSE(10, ("Device %s: memory deregister succeeded.",
                            ibv_get_device_name(device->dev.ib_dev)));

    iboffload_reg->mr = NULL;

    return OMPI_SUCCESS;
}

/* We need to keep separate registration function for
   ML list memory managment */
static int mca_bcol_iboffload_lmngr_register(void *context_data,
                                             void *base, size_t size,
                                             void **reg_desc)
{
    struct ibv_mr *mr;
    mca_bcol_iboffload_device_t *device =
                  (mca_bcol_iboffload_device_t *) context_data;

    mr = ibv_reg_mr(device->ib_pd, base, size,
            IBV_ACCESS_LOCAL_WRITE |
            IBV_ACCESS_REMOTE_WRITE |
            IBV_ACCESS_REMOTE_READ);

    if (NULL == mr) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    IBOFFLOAD_VERBOSE(10, ("Device %s: memory register addr=%p, len=%d",
                ibv_get_device_name(device->dev.ib_dev), base, size));

    *reg_desc = (void *) mr;

    /* Make sure that the addr stays the same */
    assert(mr->addr == base);

    return OMPI_SUCCESS;
}

static int mca_bcol_iboffload_lmngr_deregister(void *context_data, void *reg_desc)
{
    struct ibv_mr *mr = (struct ibv_mr *) reg_desc;
    mca_bcol_iboffload_device_t *device =
                          (mca_bcol_iboffload_device_t *) context_data;

    if (mr != NULL) {
        if (ibv_dereg_mr(mr)) {
            IBOFFLOAD_ERROR(("Device %s: error unpinning iboffload memory errno says %s",
                        ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

static int iboffload_start_device(mca_bcol_iboffload_device_t *device)
{
    int port_cnt, port, ret;
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

#if HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE
    if (IBV_TRANSPORT_IB != device->dev.ib_dev->transport_type) {
        IBOFFLOAD_VERBOSE(10, ("Skipping non IB device %s",
                    ibv_get_device_name(device->dev.ib_dev)));
        goto error;
    }
#endif

    /* Open device context */
    IBOFFLOAD_VERBOSE(10, ("Open IB device - %p", device->dev.ib_dev));

    device->dev.ib_dev_context = ibv_open_device(device->dev.ib_dev);
    if (NULL == device->dev.ib_dev_context) {
        IBOFFLOAD_ERROR(("Error obtaining device context for %s errno says %s",
                    ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        goto error;
    }

    if (ibv_query_device(device->dev.ib_dev_context, &device->ib_dev_attr)) {
        IBOFFLOAD_ERROR(("error obtaining device attributes for %s errno says %s",
                    ibv_get_device_name(device->dev.ib_dev), strerror(errno)));
        goto error;
    }

    port_cnt = device->ib_dev_attr.phys_port_cnt;
    if (0 == port_cnt) {
        goto error;
    }

    device->ports = (mca_bcol_iboffload_port_t *)
                         calloc(port_cnt, sizeof(mca_bcol_iboffload_port_t));
    if (NULL == device->ports) {
        goto error;
    }

    /* Note ports are 1 based (i >= 1) */
    for (port = 1; port <= port_cnt; port++) {
        int pi = port - 1; /* port array index starts from zero */

        struct ibv_port_attr ib_port_attr;
        memset(&ib_port_attr, 0, sizeof(ib_port_attr));

        if (ibv_query_port(device->dev.ib_dev_context, (uint8_t) port, &ib_port_attr)) {
            IBOFFLOAD_ERROR(("Error getting port attributes for device %s "
                        "port number %d errno says %s",
                        ibv_get_device_name(device->dev.ib_dev), port, strerror(errno)));
            continue;
        }

        if (IBV_PORT_ACTIVE == ib_port_attr.state) {
            /* Pasha: Need to think how we want to handle MTUs
            if (ib_port_attr.active_mtu < mca_bcol_iboffload_component.mtu){
                device->mtu = ib_port_attr.active_mtu;
            }
            */
            /* start to put port info */
            ++device->num_act_ports;
            device->ports[pi].id   = port;
            device->ports[pi].stat = ib_port_attr.state;
            device->ports[pi].mtu  = ib_port_attr.active_mtu;

            if (0 == cm->pkey_val) {
                ret = iboffload_init_port(device, &device->ports[pi]);
                if (OMPI_SUCCESS != ret) {
                    IBOFFLOAD_ERROR(("Device %s "
                                "port number %d , failed to init port, errno says %s",
                                ibv_get_device_name(device->dev.ib_dev),
                                port, strerror(errno)));
                    continue;
                }
            } else {
                uint16_t pkey, j;
                for (j = 0; j < device->ib_dev_attr.max_pkeys; j++) {
                    if (ibv_query_pkey(device->dev.ib_dev_context, (uint8_t) port, j, &pkey)) {
                        IBOFFLOAD_ERROR(("error getting pkey for index %d, device %s "
                                    "port number %d errno says %s",
                                    j, ibv_get_device_name(device->dev.ib_dev), port, strerror(errno)));
                        continue;
                    }
					
                    pkey = ntohs(pkey) & MCA_BCOL_IBOFFLOAD_PKEY_MASK;
                    if (pkey == cm->pkey_val) {
                        ret = iboffload_init_port(device, &device->ports[pi]);
                        if (OMPI_SUCCESS != ret) {
                            IBOFFLOAD_ERROR(("Device %s "
                                        "port number %d , failed to init port, errno says %s",
                                        ibv_get_device_name(device->dev.ib_dev),
                                        port, strerror(errno)));
                            continue;
                        }
                    }
                }
            }
        }
    }

    if (0 == device->num_act_ports) {
        goto error;
    }

    if (OMPI_SUCCESS != allocate_device_resources(device)) {
        goto error;
    }

    /* setup network context on device */
    device->net_context = OBJ_NEW(bcol_base_network_context_t);

    device->net_context->context_data = (void *) device;

    device->net_context->register_memory_fn = mca_bcol_iboffload_lmngr_register;
    device->net_context->deregister_memory_fn = mca_bcol_iboffload_lmngr_deregister;

    /* the device is ready now */
    device->activated = true;
    return OMPI_SUCCESS;

error:
    /* Pasha: need to add nice resource clean up */
    return OMPI_ERROR;
}
static void mca_bcol_iboffload_set_small_msg_thresholds(struct mca_bcol_base_module_t *super)
{
    mca_bcol_iboffload_module_t *iboffload_module =
                            (mca_bcol_iboffload_module_t *) super;

    /* Set the Bcast threshold, for IB it equals to ML buffer size */
    super->small_message_thresholds[BCOL_BCAST] =
                        iboffload_module->rdma_block.ml_mem_desc->size_buffer;

    if ((mca_bcol_iboffload_component.use_brucks_smsg_alltoall_rdma)
           || (mca_bcol_iboffload_component.use_brucks_smsg_alltoall_sr)) {
        /* Set the Alltoall threshold, for Bruck's algth we use 1.5 of the buff size */
        super->small_message_thresholds[BCOL_ALLTOALL] =
                        (iboffload_module->rdma_block.ml_mem_desc->size_buffer / 3) * 2;
    } else {
        /* Set the Alltoall threshold, for this case it equals to a half of the ML buffer size */
        super->small_message_thresholds[BCOL_ALLTOALL] =
                        iboffload_module->rdma_block.ml_mem_desc->size_buffer / 2;
    }

    /* Set the Allreduce threshold, for IB it equals to ML buffer size */
    super->small_message_thresholds[BCOL_ALLREDUCE] =
                        iboffload_module->rdma_block.ml_mem_desc->size_buffer;

    /* Set the Allgather threshold, for IB it equals to ML buffer size */
    super->small_message_thresholds[BCOL_ALLGATHER] =
                iboffload_module->rdma_block.ml_mem_desc->size_buffer /
                ompi_comm_size(iboffload_module->super.sbgp_partner_module->group_comm);
}

static int mca_bcol_iboffload_init_buffer_memory(struct mca_coll_ml_module_t *ml_module,
                                                 struct mca_bcol_base_module_t *bcol,
                                                 void *reg_data)
{
    mca_bcol_iboffload_module_t *iboffload_module = (mca_bcol_iboffload_module_t *) bcol;
    mca_bcol_iboffload_local_rdma_block_t *rdma_block = &iboffload_module->rdma_block;

    struct ml_memory_block_desc_t *desc = ml_module->payload_block;
    struct ibv_mr *mr = (struct ibv_mr *) desc->block->lmngr->reg_desc[bcol->context_index];
    int i;

    IBOFFLOAD_VERBOSE(10, ("mca_bcol_iboffload_init_buffer_memory was called"));

    /* Set rdma block data */
    rdma_block->ib_info.rkey = mr->rkey;
    rdma_block->ib_info.lkey = mr->lkey;

    rdma_block->ib_info.addr = (uint64_t) (uintptr_t) desc->block->base_addr;
    IBOFFLOAD_VERBOSE(10, ("Caching rkey %u lkey %u addr %p",
                rdma_block->ib_info.rkey,
                rdma_block->ib_info.lkey,
                rdma_block->ib_info.addr));

    /* cache ml mem desc tunings localy */
    rdma_block->bdesc.num_banks = desc->num_banks;
    rdma_block->bdesc.num_buffers_per_bank = desc->num_buffers_per_bank;
    rdma_block->bdesc.size_buffer = desc->size_buffer;
    rdma_block->bdesc.data_offset = ml_module->data_offset;

    IBOFFLOAD_VERBOSE(10, ("RDMA buffer configuration num banks %d num_per_bank %d size %d base addr %p",
                           mr->addr, desc->num_banks, desc->num_buffers_per_bank, desc->size_buffer));

    /* pointer to ml level descriptor */
    rdma_block->ml_mem_desc = desc;

    rdma_block->sync_counter = 0; /* reset the counter */
    /* Allocate and set bank block counters */
    for (i = 0; i < MCA_BCOL_IBOFFLOAD_BK_LAST; i++) {
        rdma_block->bank_buffer_counter[i] = (int *) calloc(rdma_block->bdesc.num_banks,
                sizeof(int));
        if (NULL == rdma_block->bank_buffer_counter[i]) {
            IBOFFLOAD_VERBOSE(10, ("Failed to allocate bank_block_counter\n"));
            return OMPI_ERROR;
        }
    }

    if (OMPI_SUCCESS != init_rdma_buf_desc(&rdma_block->bdesc.rdma_desc,
                desc->block->base_addr,
                rdma_block->bdesc.num_banks,
                rdma_block->bdesc.num_buffers_per_bank,
                rdma_block->bdesc.size_buffer,
                ml_module->data_offset)) {
        IBOFFLOAD_VERBOSE(10, ("Failed to allocate rdma memory descriptor\n"));
        return OMPI_ERROR;
    }

    /* The all data is now cached on module level. The
       real data exchange will happen during qp creation and
       data exchange */

    IBOFFLOAD_VERBOSE(10, ("ml_module = %p, iboffload_module = %p, ml_mem_desc = %p.\n",
                            ml_module, iboffload_module, rdma_block->ml_mem_desc));

    for (i = 0; i < iboffload_module->num_endpoints; ++i) {
        mca_bcol_iboffload_endpoint_t *ep = iboffload_module->endpoints[i];

        if (true == ep->need_toset_remote_rdma_info) {
            IBOFFLOAD_VERBOSE(10, ("ep %p index %d: postponed remote rdma block init.", ep, ep->index));
            if (OPAL_UNLIKELY(OMPI_SUCCESS !=
                          set_endpoint_remote_rdma_info(ep, ep->remote_rdma_info))) {
                return OMPI_ERROR;
            }
        }
    }

    /* Hack:
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       Work around for deadlock caused by connection setup
       for asyc service barrier. Asyc service barrier use own set of
       MQ and QP _BUT_ the exchange operation uses the MQ that is used for
       primary set of collectives operations like Allgahter, Barrier,etc.
       As result exchange wait operation could be pushed to primary MQ and
       cause dead-lock.
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       Create connection for service barrier and memory address exchange
       for ml buffers and asyc service barrier
     */
    /* This nasty hack was moved to ml discovery
    rc = mca_bcol_iboffload_rec_doubling_start_connections(iboffload_module);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }
     */

    return OMPI_SUCCESS;
}

static void load_func(mca_bcol_base_module_t *super)
{
    int fnc;

    /* Loading Memory managment functions */
    /* NULL means that mpool may decide about prefered memory allocate functions */
    /* super->memory_management_functions.malloc_fn = NULL;*/
    /* NULL means that mpool may decide about prefered memory release functions */
    /* super->memory_management_functions.free_fn = NULL; */

    /* JSL: setting the bcol_memory_init function to NULL, not sure what ib needs to do with
     * the ml_memory_block
     */
    super->bcol_memory_init = NULL;


    /* Loading collective functions */
    for (fnc = 0; fnc < BCOL_NUM_OF_FUNCTIONS; ++fnc) {
        super->bcol_function_table[fnc] = NULL;
    }

    super->bcol_function_init_table[BCOL_FANIN] = mca_bcol_iboffload_fanin_register;
    super->bcol_function_init_table[BCOL_FANOUT] = mca_bcol_iboffload_fanout_register;

    super->bcol_function_init_table[BCOL_BARRIER] = mca_bcol_iboffload_barrier_register;
    super->bcol_function_init_table[BCOL_BCAST] = mca_bcol_iboffload_bcast_register;
    /*super->bcol_function_init_table[BCOL_ALLTOALL] = mca_bcol_iboffload_alltoall_register;*/
    /*super->bcol_function_init_table[BCOL_ALLGATHER] = mca_bcol_iboffload_allgather_register;*/
    super->bcol_function_init_table[BCOL_SYNC] = mca_bcol_iboffload_memsync_register;
    /*super->bcol_function_init_table[BCOL_ALLREDUCE] = mca_bcol_iboffload_allreduce_register;*/

    super->bcol_memory_init = mca_bcol_iboffload_init_buffer_memory;

    /* Set thresholds */
    super->set_small_msg_thresholds = mca_bcol_iboffload_set_small_msg_thresholds;

    super->k_nomial_tree  = mca_bcol_iboffload_setup_knomial_tree;
}

int mca_bcol_iboffload_setup_knomial_tree(mca_bcol_base_module_t *super)
{
    int rc;
    mca_bcol_iboffload_module_t *ib_module = (mca_bcol_iboffload_module_t *) super;
    rc = netpatterns_setup_recursive_knomial_allgather_tree_node(
            ib_module->super.sbgp_partner_module->group_size,
            ib_module->super.sbgp_partner_module->my_index,
            mca_bcol_iboffload_component.k_nomial_radix,
            super->list_n_connected,
            &ib_module->knomial_allgather_tree);

    return rc;
}

static inline struct ibv_cq *ibv_create_cq_compat(struct ibv_context *context,
        int cqe, void *cq_context, struct ibv_comp_channel *channel,
        int comp_vector)
{
#if OMPI_IBV_CREATE_CQ_ARGS == 3
    return ibv_create_cq(context, cqe, channel);
#else
    return ibv_create_cq(context, cqe, cq_context, channel, comp_vector);
#endif
}

int mca_bcol_iboffload_adjust_cq(mca_bcol_iboffload_device_t *device,
                                                   struct ibv_cq **ib_cq)
{
    uint32_t cq_size = (uint32_t) mca_bcol_iboffload_component.cq_size;

    if (NULL == *ib_cq) {
        *ib_cq = ibv_create_cq_compat(device->dev.ib_dev_context, cq_size,
#if OMPI_ENABLE_PROGRESS_THREADS == 1
                device, device->ib_channel,
#else
                NULL, NULL,
#endif
                0);

        if (NULL == *ib_cq) {
            IBOFFLOAD_ERROR(("Device %s "
                        ", failed to create CQ, errno says %s",
                        ibv_get_device_name(device->dev.ib_dev), strerror(errno)));

            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}

static int init_recv_wr_manager(mca_bcol_iboffload_recv_wr_manager *recv_wr_manager)
{

    struct ibv_recv_wr *recv_wr = NULL;
    int ret = OMPI_SUCCESS, qp, wr, num_qps;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    num_qps = cm->num_qps;
    OPAL_THREAD_LOCK(&recv_wr_manager->lock);

    recv_wr_manager->recv_work_requests =
        (struct ibv_recv_wr **) calloc(num_qps, sizeof(struct ibv_recv_wr *));
    if (NULL == recv_wr_manager->recv_work_requests) {
        IBOFFLOAD_ERROR(("Failed to allocate memory for recv_wr_manager->recv_work_requests"));
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto error;
    }

    for (qp = 0; qp < num_qps; ++qp) {
        int recv_queue_size = cm->qp_infos[qp].rd_num;

        recv_wr_manager->recv_work_requests[qp] =
            (struct ibv_recv_wr *) calloc(recv_queue_size, sizeof(struct ibv_recv_wr));
        if (NULL == recv_wr_manager->recv_work_requests[qp]) {
            IBOFFLOAD_ERROR(("Failed to allocate memory for recv_wr_manager->recv_work_requests"));
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto error;
        }

        for (wr = 0; wr < recv_queue_size - 1; ++wr) {
            recv_wr = &recv_wr_manager->recv_work_requests[qp][wr];
            recv_wr->next = &recv_wr_manager->recv_work_requests[qp][wr + 1];
            /* init receive work request.
             * Real sg_list value we fill during receive prepost flow.
             * recv_wr->wr_id and recv_wr->sg_list is zero by default */
            recv_wr->wr_id   = 0;
            recv_wr->sg_list = NULL;
            recv_wr->num_sge = 1; /* single sge will be filled later */
        }

        recv_wr->next->num_sge = 1; /* for the last entry everything is null except the num_sge */
    }

error:
    OPAL_THREAD_UNLOCK(&recv_wr_manager->lock);
    return ret;
}

/* On first access to the component - allocate all memory resources */
static int component_first_usage(void)
{
    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    int ret = OMPI_SUCCESS;

    /* creating collfrag free list */
    OBJ_CONSTRUCT(&cm->collfrags_free, ompi_free_list_t);
    ret = ompi_free_list_init_new(&cm->collfrags_free,
                                  sizeof(mca_bcol_iboffload_collfrag_t),
                                  MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                  OBJ_CLASS(mca_bcol_iboffload_collfrag_t),
                                  0, MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                  cm->free_list_num,
                                  cm->free_list_max,
                                  cm->free_list_inc,
                                  NULL);
    if (OMPI_SUCCESS != ret) {
        IBOFFLOAD_ERROR(("Failed to allocate mwr_free %s:%d\n", __FILE__, __LINE__));
        return ret;
    }

    /* allocate free list of collective message requests */
    OBJ_CONSTRUCT(&cm->collreqs_free, ompi_free_list_t);
    ret = ompi_free_list_init_new(&cm->collreqs_free,
                                  sizeof(mca_bcol_iboffload_collreq_t),
                                  MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                  OBJ_CLASS(mca_bcol_iboffload_collreq_t),
                                  0, MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                  cm->free_list_num * 2,
                                  cm->free_list_max * 2,
                                  cm->free_list_inc * 2,
                                  NULL);
    if (OMPI_SUCCESS != ret) {
        IBOFFLOAD_ERROR(("Error creating free list, error: %s\n", strerror(errno)));
        goto release_collfrag;
    }

    OBJ_CONSTRUCT(&cm->tasks_free, ompi_free_list_t);
    ret =  ompi_free_list_init_new(&cm->tasks_free,
                                   sizeof(mca_bcol_iboffload_task_t),
                                   MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                   OBJ_CLASS(mca_bcol_iboffload_task_t),
                                   0, MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                   cm->free_list_num * 2,
                                   cm->free_list_max * 2,
                                   cm->free_list_inc * 2,
                                   NULL);
    if (OMPI_SUCCESS != ret) {
        IBOFFLOAD_ERROR(("Error creating free list, error: %s\n", strerror(errno)));
        goto release_collreq;
    }

    OBJ_CONSTRUCT(&cm->calc_tasks_free, ompi_free_list_t);
    ret =  ompi_free_list_init_ex_new(&cm->calc_tasks_free,
                                   sizeof(mca_bcol_iboffload_task_t),
                                   MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                   OBJ_CLASS(mca_bcol_iboffload_task_t),
                                   0, MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                   cm->free_list_num * 2,
                                   cm->free_list_max * 2,
                                   cm->free_list_inc * 2,
                                   NULL,
                                   mca_bcol_iboffload_calc_task_init,
                                   &cm->calc_tasks_free);
    if (OMPI_SUCCESS != ret) {
        IBOFFLOAD_ERROR(("Error creating free list, error: %s\n", strerror(errno)));
        goto release_collreq;
    }

    /* Initialization for frags that handle ML allocated memory,
       it is NO registration is required !
     */

    OBJ_CONSTRUCT(&cm->ml_frags_free, ompi_free_list_t);
    ret =  ompi_free_list_init_ex_new(&cm->ml_frags_free,
                                   sizeof(mca_bcol_iboffload_frag_t),
                                   MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                   OBJ_CLASS(mca_bcol_iboffload_frag_t),
                                   0, MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                   cm->free_list_num * 2,
                                   cm->free_list_max * 2,
                                   cm->free_list_inc * 2,
                                   NULL,
                                   mca_bcol_iboffload_ml_frag_init,
                                   NULL);
    if (OMPI_SUCCESS != ret) {
        IBOFFLOAD_ERROR(("Error creating free list, error: %s\n", strerror(errno)));
        goto release_collreq;
    }

    ret = init_recv_wr_manager(&cm->recv_wrs);
    if (OMPI_SUCCESS != ret){
        IBOFFLOAD_ERROR(("Failed to prepare recv wrs"));
        goto release_tasks;
    }

    cm->init_done = true;

    return OMPI_SUCCESS;

release_tasks:
    OBJ_DESTRUCT(&cm->tasks_free);
release_collreq:
    OBJ_DESTRUCT(&cm->collreqs_free);
release_collfrag:
    OBJ_DESTRUCT(&cm->collfrags_free);
    return ret;
}


/* query to see if some modules are available for use on the given
 * communicator, and if so, what it's priority is.
 */
mca_bcol_base_module_t **
mca_bcol_iboffload_comm_query(mca_sbgp_base_module_t *sbgp, int *num_modules)
{
    /* local variables */
    int i, mq_index, rc, my_rank = 0;
    struct mqe_context_attr mqe_attr;

    mca_sbgp_ibnet_module_t *ibnet = NULL;
    mca_bcol_base_module_t **iboffload_modules = NULL;
    mca_bcol_iboffload_module_t *iboffload_module = NULL;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;
    /* Bruck's alltoall iovec */
    size_t iovec_size;

    if (OPAL_UNLIKELY(false == cm->init_done)) {
        if (OMPI_SUCCESS != component_first_usage()) {
            return NULL;
        }
    }

    /* No group - no modules*/
    if (OPAL_UNLIKELY(NULL == sbgp)) {
        return NULL;
    }
    /*
     * This is activated only for intra-communicators
     */
    if (OPAL_UNLIKELY(OMPI_COMM_IS_INTER(sbgp->group_comm))) {
        return NULL;
    }

    ibnet = (mca_sbgp_ibnet_module_t *) sbgp;
    if (OPAL_UNLIKELY(0 == ibnet->num_cgroups)) {
        /* we have no connection group */
        return NULL;
    }

    my_rank = sbgp->my_index;

    iboffload_modules = (mca_bcol_base_module_t **) calloc
                        (ibnet->num_cgroups, sizeof(mca_bcol_base_module_t *));
    if (OPAL_UNLIKELY(NULL == iboffload_modules)) {
        return NULL;
    }

    /* Go through list of connection groups that we have on ibnet
     * and create bcol module for each one */
    *num_modules = 0;
    for (i = 0; i < ibnet->num_cgroups; i++) {
        mca_sbgp_ibnet_connection_group_info_t *cgroup =
            &ibnet->cgroups[i];

        iboffload_module = OBJ_NEW(mca_bcol_iboffload_module_t);

        iboffload_modules[i] = &(iboffload_module->super);

        /*
         * In fact the value == ibnet->num_cgroups in the end
         * of the loop, but we need always to know how many modules
         * release in the error case (under CLEANUP label)
         */

        (*num_modules)++;

        iboffload_module->cgroup_index = i;
        iboffload_module->group_size = ibnet->super.group_size;
        iboffload_module->log_group_size = lognum(iboffload_module->group_size);
        /* Put pointer to sbgp module */
        iboffload_module->super.sbgp_partner_module = sbgp;
        /* Put cgroup information on module */
        iboffload_module->ibnet = ibnet;

        iboffload_module->device = opal_pointer_array_get_item(&cm->devices, cgroup->device_index);

        IBOFFLOAD_VERBOSE(10, ("Iboffload module - %p uses "
                               "device - %p with index - %d.\n",
                                iboffload_module,
                                iboffload_module->device->dev.ib_dev,
                                cgroup->device_index));

        OBJ_RETAIN(iboffload_module->device);
        /* Pasha: Need to print NICE error in future */
        assert(NULL != iboffload_module->device);
        iboffload_module->port = cgroup->port;

        IBOFFLOAD_VERBOSE(10, ("Iboffload module - %p on local port %d.\n",
                               iboffload_module, iboffload_module->port));

        if (OPAL_UNLIKELY(!iboffload_module->device->activated)) {
            /* this device was never used before, need to activate it */
            if (OMPI_SUCCESS != iboffload_start_device(iboffload_module->device)) {
                OBJ_RELEASE(iboffload_module->device);
                goto CLEANUP;
            }
        }
        /* Set pointer to network contest on bcol base, we need it for ml
        memory managment */
        OBJ_RETAIN(iboffload_module->device->net_context);
        iboffload_module->super.network_context = iboffload_module->device->net_context;

        iboffload_module->subnet_id = iboffload_module->device->ports[iboffload_module->port - 1].subnet_id;
        iboffload_module->lid = iboffload_module->device->ports[iboffload_module->port - 1].lid;

        load_func(&iboffload_module->super);

        IBOFFLOAD_VERBOSE(10, ("Call for create endpoints for iboffload module %p,"
                              " cgroup num (index) %d.\n", iboffload_module, i));

        /* create endpoints and store its in the endpoints pointer of iboffload_module structer */
        if (OMPI_SUCCESS !=
                 mca_bcol_iboffloads_create_endpoints(cgroup, iboffload_module)) {
            goto CLEANUP;
        }

        memset(&mqe_attr, 0, sizeof(mqe_attr));
        mqe_attr.max_mqe_tasks = (uint32_t)mca_bcol_iboffload_component.max_mqe_tasks;
        mqe_attr.max_mq_size = (uint32_t)mca_bcol_iboffload_component.max_mq_size;
        mqe_attr.cq = iboffload_module->device->ib_mq_cq;

        /* ALL MQs have the same configuration */
        for (mq_index = 0; mq_index < BCOL_IBOFFLOAD_MQ_NUM; mq_index++) {
            iboffload_module->mq[mq_index] =
                mqe_context_create(iboffload_module->device->dev.ib_dev_context,
                        iboffload_module->device->ib_pd, &mqe_attr);
            if (OPAL_UNLIKELY(NULL == iboffload_module->mq[mq_index])) {
                IBOFFLOAD_ERROR(("Error creating MQ for device (%s), error: %s\n",
                            ibv_get_device_name(iboffload_module->device->dev.ib_dev), strerror(errno)));
                goto CLEANUP;
            }
        }

        /* Barrier initialization - recuresive doubling */
#if 1
        if (OMPI_SUCCESS !=
                    netpatterns_setup_recursive_doubling_tree_node(
                                iboffload_module->group_size, my_rank,
                                &iboffload_module->recursive_doubling_tree)) {
            IBOFFLOAD_ERROR(("Failed to setup recursive doubling tree,"
                             " error: %s\n", strerror(errno)));
            goto CLEANUP;
        }
#endif

        /* Barrier initialization - N exchange tree */
        if (OMPI_SUCCESS !=
                netpatterns_setup_recursive_doubling_n_tree_node(
                                iboffload_module->group_size, my_rank,
                                cm->exchange_tree_order,
                                &iboffload_module->n_exchange_tree)) {
            IBOFFLOAD_ERROR(("Failed to setup recursive doubling tree,"
                             " error: %s\n", strerror(errno)));
            goto CLEANUP;
        }


        /* Recursive K-ing initialization - Knomial exchange tree */
        if (OMPI_SUCCESS !=
                netpatterns_setup_recursive_knomial_tree_node(
                                iboffload_module->group_size, my_rank,
                                cm->knomial_tree_order,
                                &iboffload_module->knomial_exchange_tree)) {
            IBOFFLOAD_ERROR(("Failed to setup recursive Knomial tree,"
                             " error: %s\n", strerror(errno)));
            goto CLEANUP;
        }

        /* Manju Brucks alltoall temp iovec list */
        iovec_size = iboffload_module->group_size / 2 + iboffload_module->group_size % 2;
        iboffload_module->alltoall_iovec = (struct iovec *) malloc(sizeof(struct iovec)
                                                                   * iovec_size);
        iboffload_module->alltoall_recv_iovec = (struct iovec *) malloc(sizeof(struct iovec)
                                                                        * iovec_size);


        iboffload_module->k_alltoall_bruck_radix=cm->k_alltoall_bruck_radix;
        iboffload_module->tmp_buf_alignment=cm->tmp_buf_alignment;

#if 1 /* Disabling this code since it brakes all iboffload functionality */
        /* Sorry Pasha, gotta do this. Recursive K-ing allgather initialization - Knomial exchange tree */
        /*Pretty sure I need to pass in the communicator rank */
        /* I need to reindex this mess */
        /* this looks silly, I know but it allows for minimal changes to existing code */
        iboffload_module->comm_to_ibnet_map = sbgp->group_list;


#endif
#if 0
        if ( NULL == iboffload_module->comm_to_ibnet_map ) {
            IBOFFLOAD_ERROR(("Out of resources\n"));
            goto CLEANUP;
        }
        for( i = 0; i < iboffload_module->group_size; i++) {
            int j = 0;
            while( sbgp->group_list[j] != i){
                j++;
            }
            iboffload_module->comm_to_ibnet_map[i] = j;
        }
        /* that should take care of that */
        if (OMPI_SUCCESS !=
                netpatterns_setup_recursive_knomial_allgather_tree_node(
                                iboffload_module->group_size, sbgp->group_list[my_rank],
                                cm->k_nomial_radix, iboffload_module->super.list_n_connected,
                                &iboffload_module->knomial_allgather_tree)) {
            IBOFFLOAD_ERROR(("Failed to setup recursive Knomial tree,"
                             " error: %s\n", strerror(errno)));
            goto CLEANUP;
        }
#endif

        iboffload_module->power_of_2 =
            mca_bcol_iboffload_fls(iboffload_module->num_endpoints);
        iboffload_module->power_of_2_ranks =
                  (1 << iboffload_module->power_of_2);

        /* header into ml buffer, we don't support header for anyone other than shared memory
         * at the moment
         */
        iboffload_module->super.header_size = 0;

        iboffload_module->super.supported_mode = MCA_BCOL_BASE_ZERO_COPY |
                                                 MCA_BCOL_BASE_NO_ML_BUFFER_FOR_LARGE_MSG |
                                                 MCA_BCOL_BASE_NO_ML_BUFFER_FOR_BARRIER;

        rc = mca_bcol_base_bcol_fns_table_init(&(iboffload_module->super));
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            goto CLEANUP;
        }

        OBJ_CONSTRUCT(&iboffload_module->iovec_tasks_free, ompi_free_list_t);
        rc =  ompi_free_list_init_ex_new(&iboffload_module->iovec_tasks_free,
                                       sizeof(mca_bcol_iboffload_task_t),
                                       MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                       OBJ_CLASS(mca_bcol_iboffload_task_t),
                                       0, MCA_IBOFFLOAD_CACHE_LINE_SIZE,
                                       cm->free_list_num * 2,
                                       cm->free_list_max * 2,
                                       cm->free_list_inc * 2,
                                       NULL,
                                       mca_bcol_iboffload_iovec_task_init,
                                       iboffload_module);
        if (OMPI_SUCCESS != rc) {
            IBOFFLOAD_ERROR(("Error creating free list, error: %s\n", strerror(errno)));
            goto CLEANUP;
        }
    }

    IBOFFLOAD_VERBOSE(10, ("Finished with success, num of cgroups is %d, num of modules is %d.\n",
                           ibnet->num_cgroups, *num_modules));

    return iboffload_modules;

CLEANUP:
    for (i = 0; i < *num_modules; i++) {
        if (NULL != iboffload_modules[i]) {
            OBJ_RELEASE(iboffload_modules[i]);
        }
    }
    free(iboffload_modules);
    return NULL;
}

static int init_rdma_buf_desc(mca_bcol_iboffload_rdma_buffer_desc_t **desc, void *base_addr, uint32_t num_banks,
        uint32_t num_buffers_per_bank, uint32_t size_buffer, uint32_t header_size)
{
    uint32_t i, j, ci;
    mca_bcol_iboffload_rdma_buffer_desc_t *tmp_desc;

    IBOFFLOAD_VERBOSE(10, ("init_rdma_buf_desc base addr %p, num_n %d , "
                            "num_per_bank %d, size %d, header size %d",
                            base_addr, num_banks, num_buffers_per_bank,
                            size_buffer, header_size));
    *desc = (mca_bcol_iboffload_rdma_buffer_desc_t *)
                        calloc(num_banks * num_buffers_per_bank,
                               sizeof(mca_bcol_iboffload_rdma_buffer_desc_t));
    if (OPAL_UNLIKELY(NULL == *desc)) {
        IBOFFLOAD_ERROR(("Failed to allocate memory"));
        return OMPI_ERROR;
    }

    tmp_desc = *desc;

    for (i = 0; i < num_banks; i++) {
        for (j = 0; j < num_buffers_per_bank; j++) {
            ci = i * num_buffers_per_bank + j;
            tmp_desc[ci].generation_number = 0;
            tmp_desc[ci].bank_index = i;
            tmp_desc[ci].buffer_index = j;
            /*
             * iboffload don't have any header, but other bcols may to have. So
             * we need to take it in account.
             */
            tmp_desc[ci].data_addr = (void *)
                ((unsigned char *) base_addr + ci * size_buffer + header_size);
            IBOFFLOAD_VERBOSE(10, ("RDMA setup %d %d - %p", i, j, tmp_desc[ci].data_addr));
        }
    }

    return OMPI_SUCCESS;
}

static int set_endpoint_remote_rdma_info(mca_bcol_iboffload_endpoint_t *ep, mca_bcol_iboffload_rdma_info_t *remote_rdma_info)
{
    mca_bcol_iboffload_rem_rdma_block_t *rem_block = &ep->remote_rdma_block;

    /* We'll continue if -
        1. The module rdma_block is already initilized on this stage
        2. All peers have the same rdma block configuration that actually is
           define on ML level

       Otherwise set flag to init it lately.
    */
    if (NULL == ep->iboffload_module->rdma_block.ml_mem_desc) {
        IBOFFLOAD_VERBOSE(10, ("RDMA block information hasn't been inited yet."));
        ep->need_toset_remote_rdma_info = true;
        return OMPI_SUCCESS;
    }

    /* set the rdma addr for barrier */
    ep->remote_zero_rdma_addr = remote_rdma_info[0];

    IBOFFLOAD_VERBOSE(10, ("RDMA block information %p %d",
                remote_rdma_info[0].addr, remote_rdma_info[0].rkey));

    /* set the rdma block memory structs */
    rem_block->ib_info = remote_rdma_info[1];


    /* if we got some real data. lets init memory adress sctructures */
    if (0 != rem_block->ib_info.addr) {
        if (OMPI_SUCCESS != init_rdma_buf_desc(&rem_block->rdma_desc, (void *)rem_block->ib_info.addr,
                ep->iboffload_module->rdma_block.bdesc.num_banks,
                ep->iboffload_module->rdma_block.bdesc.num_buffers_per_bank,
                ep->iboffload_module->rdma_block.bdesc.size_buffer,
                /* remember, we use lkey to pass the data offset value */
                rem_block->ib_info.lkey)) {
            IBOFFLOAD_VERBOSE(10, ("Failed to allocate RDMA buffer descriptor"));
            return OMPI_ERROR;
        }
    }

    IBOFFLOAD_VERBOSE(10, ("endpoint - %p, recv barrier rdma: rem addr - %p, rem rkey - %d.\n",
                ep, ep->remote_zero_rdma_addr.addr, ep->remote_zero_rdma_addr.rkey));
    IBOFFLOAD_VERBOSE(10, ("endpoint - %p, recv ml rdma: rem addr - %p, rem rkey - %d.\n",
                ep, ep->remote_rdma_block.ib_info.addr, ep->remote_rdma_block.ib_info.rkey));

    return OMPI_SUCCESS;
}

static int unpack_endpoint_rdma_addr(void *callback_data)
{
    int rc;
    struct iovec payload_iovec;

    size_t max_size = 0;
    uint32_t out_size = 1;

    mca_bcol_iboffload_collfrag_t *coll_frag = (mca_bcol_iboffload_collfrag_t *) callback_data;
    mca_bcol_iboffload_collreq_t* collreq = coll_frag->coll_full_req;

    mca_bcol_iboffload_task_t *wait_task = (mca_bcol_iboffload_task_t *) coll_frag->signal_task_wr_id;

    mca_bcol_iboffload_frag_t *recv_frag = wait_task->frag;
    mca_bcol_iboffload_endpoint_t *ep = wait_task->endpoint;

    rc = opal_convertor_copy_and_prepare_for_recv(
                ompi_mpi_local_convertor,
                &opal_datatype_uint1,
                sizeof(mca_bcol_iboffload_rdma_info_t) * MAX_REMOTE_RDMA_INFO,
                ep->remote_rdma_info, 0,
                &collreq->recv_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return OMPI_ERROR;
    }

    payload_iovec.iov_base = (void*) (uintptr_t)
                         recv_frag->sg_entry.addr;

    payload_iovec.iov_len = sizeof(mca_bcol_iboffload_rdma_info_t) * MAX_REMOTE_RDMA_INFO;

    if (0 > opal_convertor_unpack(&collreq->recv_convertor,
               &payload_iovec, &out_size, &max_size)) {
            return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != set_endpoint_remote_rdma_info(ep, ep->remote_rdma_info)) {
        return OMPI_ERROR;
    }

    opal_convertor_cleanup(&collreq->send_convertor);
    opal_convertor_cleanup(&collreq->recv_convertor);

    return OMPI_SUCCESS;
}

/* RDMA addr exchange with rem proc */
int mca_bcol_iboffload_exchange_rem_addr(mca_bcol_iboffload_endpoint_t *ep)
{
    int rc;
    /* the [0] used for constant barrier rdma operations
       the [1] used for rdma block inforation exchange. The rdma
       block is used for RDMA operation over ML allocated memory */
    mca_bcol_iboffload_rdma_info_t remote_rdma_addr[MAX_REMOTE_RDMA_INFO];

    mca_bcol_iboffload_task_t *send_task,
                              *wait_task;

    mca_bcol_iboffload_frag_t *send_fragment,
                              *preposted_recv_frag;

    ompi_free_list_item_t *item;

    mca_bcol_iboffload_collreq_t  *coll_request;
    mca_bcol_iboffload_collfrag_t *coll_fragment;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    OMPI_FREE_LIST_WAIT(&cm->collreqs_free, item, rc);
    if (OMPI_SUCCESS != rc) {
        IBOFFLOAD_ERROR(("Failing for coll request free list waiting.\n"));
        return rc;
    }

    coll_request = (mca_bcol_iboffload_collreq_t *) item;

    coll_request->completion_cb_fn = unpack_endpoint_rdma_addr;
    /* For the exchange the progress_fn should be never used */
    coll_request->progress_fn = NULL;
    coll_request->module = ep->iboffload_module;
    coll_request->ml_buffer_index = MCA_COLL_ML_NO_BUFFER;
    coll_request->buffer_info[SBUF].offset = 0;
    coll_request->buffer_info[RBUF].offset = 0;
    coll_request->qp_index = MCA_BCOL_IBOFFLOAD_QP_REGULAR;
    /*
     * setup collective work request
     */

    /* get collective frag */
    coll_fragment = &coll_request->first_collfrag;
    mca_bcol_iboffload_collfrag_init(coll_fragment);

    coll_fragment->mq_credits = 2;
    coll_fragment->mq_index = COLL_MQ;
    coll_fragment->tail_next = &coll_fragment->to_post;
    /* overwrite mq index to run over service setup */

    /* Update the algorithm type in order to support credit mechanism */
    coll_fragment->alg = REMOTE_EXCHANGE_ALG;
    if (OPAL_UNLIKELY(false ==
                BCOL_IBOFFLOAD_MQ_HAVE_CREDITS(ep->iboffload_module,
                    coll_fragment->mq_index, 2))) {
        IBOFFLOAD_VERBOSE(10, ("There are not enough credits on MQ.\n"));

        goto out_of_resources;
    }

    /* set pointers for (coll frag) <-> (coll full request) */
    MCA_BCOL_IBOFFLOAD_SET_COLL_REQ_LINKS(coll_request, coll_fragment);

    remote_rdma_addr[0].addr =
           ep->iboffload_module->device->dummy_frags[MCA_BCOL_IBOFFLOAD_QP_BARRIER].sg_entry.addr;
    remote_rdma_addr[0].rkey =
           ep->iboffload_module->device->dummy_frags[MCA_BCOL_IBOFFLOAD_QP_BARRIER].registration->mr->rkey;

    if (NULL != ep->iboffload_module->rdma_block.ml_mem_desc) {
        remote_rdma_addr[1].addr = ep->iboffload_module->rdma_block.ib_info.addr;
        remote_rdma_addr[1].rkey = ep->iboffload_module->rdma_block.ib_info.rkey;
        /* Little bit ugly, but easy solution. The data_offset */
        remote_rdma_addr[1].lkey = ep->iboffload_module->rdma_block.bdesc.data_offset;
    } else {
        /* since it is no data lets send 0, so remote side will knox that no real
           data was send */
        remote_rdma_addr[1].addr = 0;
        remote_rdma_addr[1].rkey = 0;
        remote_rdma_addr[1].lkey = 0;
    }

    IBOFFLOAD_VERBOSE(10, ("endpoint - %p, sending barrier rdma: addr - %p, rkey - %d.\n",
                        ep, remote_rdma_addr[0].addr, remote_rdma_addr[0].rkey));
    IBOFFLOAD_VERBOSE(10, ("endpoint - %p, sending ml rdma: addr - %p, rkey - %d.\n",
                ep, remote_rdma_addr[1].addr, remote_rdma_addr[1].rkey));

    rc = opal_convertor_copy_and_prepare_for_send(
                ompi_mpi_local_convertor,
                &opal_datatype_uint1,
                sizeof(mca_bcol_iboffload_rdma_info_t) * MAX_REMOTE_RDMA_INFO,
                &remote_rdma_addr, 0,
                &coll_request->send_convertor);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        goto out_of_resources;
    }

    send_fragment = mca_bcol_iboffload_get_send_frag(
                coll_request, ep->index, coll_request->qp_index,
                sizeof(mca_bcol_iboffload_rdma_info_t) * MAX_REMOTE_RDMA_INFO,
                0, SBUF, MCA_BCOL_IBOFFLOAD_SEND_FRAG_CONVERT);
    if (OPAL_UNLIKELY(NULL == send_fragment)) {
        IBOFFLOAD_ERROR(("Failing for getting and packing send frag.\n"));
        goto out_of_resources;
    }

    send_task = mca_bcol_iboffload_get_send_task(ep->iboffload_module,
                                                 ep->index, coll_request->qp_index, send_fragment,
                                                 coll_fragment, INLINE);
    if (OPAL_UNLIKELY(NULL == send_task)) {
        IBOFFLOAD_ERROR(("Failing for getting send task.\n"));
        goto out_of_resources;
    }

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, send_task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, send_task);

    /* post wait */
    preposted_recv_frag = mca_bcol_iboffload_get_preposted_recv_frag(
            ep->iboffload_module, ep->index, coll_request->qp_index);
    if (OPAL_UNLIKELY(NULL == preposted_recv_frag)) {
        IBOFFLOAD_ERROR(("Exchaging: "
                           "Failing for getting prepost recv frag.\n"));
        goto out_of_resources;
    }

    wait_task = mca_bcol_iboffload_get_wait_task(ep->iboffload_module,
            ep->index, 1, preposted_recv_frag, coll_request->qp_index, NULL);

    if (OPAL_UNLIKELY(NULL == wait_task)) {
        IBOFFLOAD_VERBOSE(10, ("Exchanging: "
                           "Failing for getting wait task.\n"));
        goto out_of_resources;
    }

    MCA_BCOL_IBOFFLOAD_APPEND_TASK_TO_LIST(coll_fragment->task_next, wait_task);
    MCA_BCOL_IBOFFLOAD_APPEND_MQ_TASK_TO_LIST(coll_fragment->tail_next, wait_task);

    /* The last element must end with ZERO */
    wait_task->element.next = NULL;

    /* number of sends that need to be completed asynchronously */
    coll_fragment->n_sends = 1;
    SENDWR(send_task)->send_flags |= IBV_SEND_SIGNALED;

    /* finish initializing full message descriptor */
    coll_request->n_fragments  = 1;
    coll_request->n_frags_sent = 1;

    coll_request->n_frag_mpi_complete = 0;
    coll_request->n_frag_net_complete = 0;
    coll_request->user_handle_freed = false;

    wait_task->element.flags |= MQE_WR_FLAG_SIGNAL;
    coll_fragment->signal_task_wr_id =
                      (uint64_t) (uintptr_t) wait_task->element.wr_id;

    wait_task->element.wr_id = (uint64_t) (uintptr_t) coll_fragment;

    /* post the mwr */
    rc = mca_bcol_iboffload_post_mqe_tasks(coll_request->module, coll_fragment->to_post);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        IBOFFLOAD_VERBOSE(10, ("MQE task posting failing.\n"));
        /* Note: need to clean up */
        return rc;
    }

    coll_request->user_handle_freed = true;
    /* complete the exchange - progress releases full request descriptors */
    while (!BCOL_IS_COMPLETED(coll_request)) {
        opal_progress();
    }

    IBOFFLOAD_VERBOSE(10, ("RDMA addr exchange with comm rank: %d was finished.\n",
                           ep->iboffload_module->ibnet->super.group_list[ep->index]));

    return OMPI_SUCCESS;

out_of_resources:
    /* Release all resources */
    IBOFFLOAD_VERBOSE(10, ("RDMA addr exchange, adding collfrag to collfrag_pending.\n"));
    return mca_bcol_iboffload_free_resources_and_move_to_pending(coll_fragment, ep->iboffload_module);
}
