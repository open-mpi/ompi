/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2015 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006-2016 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2008-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2013-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <string.h>
#include "opal_stdint.h"
#include "opal/class/opal_bitmap.h"
#include "opal/util/output.h"
#include "opal/util/arch.h"
#include "opal/util/proc.h"
#include "opal/include/opal_stdint.h"
#include "opal/util/show_help.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/btl_base_error.h"

#if OPAL_ENABLE_FT_CR == 1
#include "opal/runtime/opal_cr.h"
#endif

#include "btl_openib_ini.h"

#include "btl_openib.h"
#include "btl_openib_frag.h"
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"
#include "btl_openib_xrc.h"
#include "btl_openib_async.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/mca/mpool/grdma/mpool_grdma.h"

#if OPAL_CUDA_SUPPORT
#include "opal/datatype/opal_datatype_cuda.h"
#include "opal/mca/common/cuda/common_cuda.h"
#endif /* OPAL_CUDA_SUPPORT */

#include "opal/util/sys_limits.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <math.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "opal/mca/hwloc/hwloc.h"

#ifndef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif

static mca_btl_base_registration_handle_t *mca_btl_openib_register_mem (mca_btl_base_module_t *btl,
                                                                        mca_btl_base_endpoint_t *endpoint,
                                                                        void *base, size_t size, uint32_t flags);
static int mca_btl_openib_deregister_mem (mca_btl_base_module_t *btl, mca_btl_base_registration_handle_t *handle);

mca_btl_openib_module_t mca_btl_openib_module = {
    .super = {
        .btl_component = &mca_btl_openib_component.super,
        .btl_add_procs = mca_btl_openib_add_procs,
        .btl_del_procs = mca_btl_openib_del_procs,
        .btl_finalize = mca_btl_openib_finalize,
        /* we need alloc free, pack */
        .btl_alloc = mca_btl_openib_alloc,
        .btl_free = mca_btl_openib_free,
        .btl_prepare_src = mca_btl_openib_prepare_src,
        .btl_send = mca_btl_openib_send,
        .btl_sendi = mca_btl_openib_sendi, /* send immediate */
        .btl_put = mca_btl_openib_put,
        .btl_get = mca_btl_openib_get,
        .btl_dump = mca_btl_base_dump,
        .btl_register_error = mca_btl_openib_register_error_cb, /* error call back registration */
        .btl_ft_event = mca_btl_openib_ft_event,
        .btl_register_mem = mca_btl_openib_register_mem,
        .btl_deregister_mem = mca_btl_openib_deregister_mem,
#if HAVE_DECL_IBV_ATOMIC_HCA
        .btl_atomic_fop = mca_btl_openib_atomic_fop,
        .btl_atomic_cswap = mca_btl_openib_atomic_cswap,
#endif
    }
};

char* const mca_btl_openib_transport_name_strings[MCA_BTL_OPENIB_TRANSPORT_SIZE] = {
    "MCA_BTL_OPENIB_TRANSPORT_IB",
    "MCA_BTL_OPENIB_TRANSPORT_IWARP",
    "MCA_BTL_OPENIB_TRANSPORT_RDMAOE",
    "MCA_BTL_OPENIB_TRANSPORT_UNKNOWN"
};

static int mca_btl_openib_finalize_resources(struct mca_btl_base_module_t* btl);

void mca_btl_openib_show_init_error(const char *file, int line,
                                    const char *func, const char *dev)
{
    if (ENOMEM == errno) {
        int ret;
        struct rlimit limit;
        char *str_limit = NULL;

#if HAVE_DECL_RLIMIT_MEMLOCK
        ret = getrlimit(RLIMIT_MEMLOCK, &limit);
#else
        ret = -1;
#endif
        if (0 != ret) {
            asprintf(&str_limit, "Unknown");
        } else if (limit.rlim_cur == RLIM_INFINITY) {
            asprintf(&str_limit, "unlimited");
        } else {
            asprintf(&str_limit, "%ld", (long)limit.rlim_cur);
        }

        opal_show_help("help-mpi-btl-openib.txt", "init-fail-no-mem",
                       true, opal_process_info.nodename,
                       file, line, func, dev, str_limit);

        if (NULL != str_limit) free(str_limit);
    } else {
        opal_show_help("help-mpi-btl-openib.txt", "init-fail-create-q",
                       true, opal_process_info.nodename,
                       file, line, func, strerror(errno), errno, dev);
    }
}

static inline struct ibv_cq *create_cq_compat(struct ibv_context *context,
        int cqe, void *cq_context, struct ibv_comp_channel *channel,
        int comp_vector)
{
#if OPAL_IBV_CREATE_CQ_ARGS == 3
    return ibv_create_cq(context, cqe, channel);
#else
    return ibv_create_cq(context, cqe, cq_context, channel, comp_vector);
#endif
}

static int adjust_cq(mca_btl_openib_device_t *device, const int cq)
{
    uint32_t cq_size = device->cq_size[cq];

    /* make sure we don't exceed the maximum CQ size and that we
     * don't size the queue smaller than otherwise requested
     */
     if(cq_size < mca_btl_openib_component.ib_cq_size[cq])
        cq_size = mca_btl_openib_component.ib_cq_size[cq];

    if(cq_size > (uint32_t)device->ib_dev_attr.max_cqe)
        cq_size = device->ib_dev_attr.max_cqe;

    if(NULL == device->ib_cq[cq]) {
        device->ib_cq[cq] = create_cq_compat(device->ib_dev_context, cq_size,
#if OPAL_ENABLE_PROGRESS_THREADS == 1
                device, device->ib_channel,
#else
                NULL, NULL,
#endif
                0);

        if (NULL == device->ib_cq[cq]) {
            mca_btl_openib_show_init_error(__FILE__, __LINE__, "ibv_create_cq",
                                           ibv_get_device_name(device->ib_dev));
            return OPAL_ERROR;
        }

#if OPAL_ENABLE_PROGRESS_THREADS == 1
        if(ibv_req_notify_cq(device->ib_cq[cq], 0)) {
            mca_btl_openib_show_init_error(__FILE__, __LINE__,
                                           "ibv_req_notify_cq",
                                           ibv_get_device_name(device->ib_dev));
            return OPAL_ERROR;
        }

        if (!device->progress) {
            int rc;
            device->progress = true;
            if(OPAL_SUCCESS != (rc = opal_thread_start(&device->thread))) {
                BTL_ERROR(("Unable to create progress thread, retval=%d", rc));
                return rc;
            }
        }
#endif
    }
#ifdef HAVE_IBV_RESIZE_CQ
    else if (cq_size > mca_btl_openib_component.ib_cq_size[cq]){
        int rc;
        rc = ibv_resize_cq(device->ib_cq[cq], cq_size);
        /* For ConnectX the resize CQ is not implemented and verbs returns -ENOSYS
         * but should return ENOSYS. So it is reason for abs */
        if(rc && ENOSYS != abs(rc)) {
            BTL_ERROR(("cannot resize completion queue, error: %d", rc));
            return OPAL_ERROR;
        }
    }
#endif

    return OPAL_SUCCESS;
}


/* In this function we check if the device supports srq limit
   event. We create the temporary srq, post some receive buffers - in
   order to prevent srq limit event immediately and call the
   "ibv_modify_srq" function. If a return value of the function not
   success => our decision that the device doesn't support this
   capability. */
static int check_if_device_support_modify_srq(mca_btl_openib_module_t *openib_btl)
{
    char buff;
    int rc = OPAL_SUCCESS;

    struct ibv_srq* dummy_srq = NULL;
    struct ibv_srq_attr modify_attr;

    struct ibv_sge sge_elem;
    struct ibv_recv_wr wr1, wr2, *bad_wr;

    struct ibv_srq_init_attr init_attr;
    memset(&init_attr, 0, sizeof(struct ibv_srq_init_attr));

    init_attr.attr.max_wr = 3;
    init_attr.attr.max_sge = 1;

    dummy_srq = ibv_create_srq(openib_btl->device->ib_pd, &init_attr);
    if(NULL == dummy_srq) {
        rc = OPAL_ERROR;
        return rc;
    }

    sge_elem.addr = (uint64_t)((uintptr_t) &buff);
    sge_elem.length = sizeof(buff);

    wr1.num_sge = wr2.num_sge = 1;
    wr1.sg_list = wr2.sg_list = &sge_elem;

    wr1.next = &wr2;
    wr2.next = NULL;

    if(ibv_post_srq_recv(dummy_srq, &wr1, &bad_wr)) {
        rc = OPAL_ERROR;
        goto destroy_dummy_srq;
    }

    modify_attr.max_wr = 2;
    modify_attr.max_sge = 1;
    modify_attr.srq_limit = 1;

    if(ibv_modify_srq(dummy_srq, &modify_attr, IBV_SRQ_LIMIT)) {
        rc = OPAL_ERR_NOT_SUPPORTED;
        goto destroy_dummy_srq;
    }

destroy_dummy_srq:
    if(ibv_destroy_srq(dummy_srq)) {
        rc = OPAL_ERROR;
    }

    return rc;
}

/*
 * create both the high and low priority completion queues
 * and the shared receive queue (if requested)
 */
static int create_srq(mca_btl_openib_module_t *openib_btl)
{
    int qp, rc = 0;
    int32_t rd_num, rd_curr_num;

    bool device_support_modify_srq = true;

    /* Check if our device supports modify srq ability */
    rc = check_if_device_support_modify_srq(openib_btl);
    if(OPAL_ERR_NOT_SUPPORTED == rc) {
        device_support_modify_srq = false;
    } else if(OPAL_SUCCESS != rc) {
        mca_btl_openib_show_init_error(__FILE__, __LINE__,
                    "ibv_create_srq",
                    ibv_get_device_name(openib_btl->device->ib_dev));
        return rc;
    }

    /* create the SRQ's */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        struct ibv_srq_init_attr attr;
        memset(&attr, 0, sizeof(struct ibv_srq_init_attr));

        if(!BTL_OPENIB_QP_TYPE_PP(qp)) {
            attr.attr.max_wr = mca_btl_openib_component.qp_infos[qp].rd_num +
                mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
            attr.attr.max_sge = 1;
            openib_btl->qps[qp].u.srq_qp.rd_posted = 0;
#if HAVE_XRC
            if(BTL_OPENIB_QP_TYPE_XRC(qp)) {
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
                struct ibv_srq_init_attr_ex attr_ex;
                memset(&attr_ex, 0, sizeof(struct ibv_srq_init_attr_ex));
                attr_ex.attr.max_wr = attr.attr.max_wr;
                attr_ex.attr.max_sge = attr.attr.max_sge;
                attr_ex.comp_mask = IBV_SRQ_INIT_ATTR_TYPE | IBV_SRQ_INIT_ATTR_XRCD |
                                    IBV_SRQ_INIT_ATTR_CQ | IBV_SRQ_INIT_ATTR_PD;
                attr_ex.srq_type = IBV_SRQT_XRC;
                attr_ex.xrcd = openib_btl->device->xrcd;
                attr_ex.cq = openib_btl->device->ib_cq[qp_cq_prio(qp)];
                attr_ex.pd = openib_btl->device->ib_pd;

                openib_btl->qps[qp].u.srq_qp.srq =
                ibv_create_srq_ex(openib_btl->device->ib_dev_context, &attr_ex);
#else
                openib_btl->qps[qp].u.srq_qp.srq =
                    ibv_create_xrc_srq(openib_btl->device->ib_pd,
                            openib_btl->device->xrc_domain,
                            openib_btl->device->ib_cq[qp_cq_prio(qp)], &attr);
#endif
            } else
#endif
            {
               opal_mutex_lock(&openib_btl->device->device_lock);
               openib_btl->qps[qp].u.srq_qp.srq =
                   ibv_create_srq(openib_btl->device->ib_pd, &attr);
               opal_mutex_unlock(&openib_btl->device->device_lock);
            }
            if (NULL == openib_btl->qps[qp].u.srq_qp.srq) {
                mca_btl_openib_show_init_error(__FILE__, __LINE__,
                                               "ibv_create_srq",
                                               ibv_get_device_name(openib_btl->device->ib_dev));
                return OPAL_ERROR;
            }

            {
                opal_mutex_t *lock = &mca_btl_openib_component.srq_manager.lock;
                opal_hash_table_t *srq_addr_table = &mca_btl_openib_component.srq_manager.srq_addr_table;

                opal_mutex_lock(lock);
                if (OPAL_SUCCESS != opal_hash_table_set_value_ptr(
                                srq_addr_table, &openib_btl->qps[qp].u.srq_qp.srq,
                                sizeof(struct ibv_srq*), (void*) openib_btl)) {
                    BTL_ERROR(("SRQ Internal error."
                            " Failed to add element to mca_btl_openib_component.srq_manager.srq_addr_table\n"));

                    opal_mutex_unlock(lock);
                    return OPAL_ERROR;
                }
                opal_mutex_unlock(lock);
            }
            rd_num = mca_btl_openib_component.qp_infos[qp].rd_num;
            rd_curr_num = openib_btl->qps[qp].u.srq_qp.rd_curr_num = mca_btl_openib_component.qp_infos[qp].u.srq_qp.rd_init;

            if(true == mca_btl_openib_component.enable_srq_resize &&
                                    true == device_support_modify_srq) {
                if(0 == rd_curr_num) {
                    openib_btl->qps[qp].u.srq_qp.rd_curr_num = 1;
                }

                openib_btl->qps[qp].u.srq_qp.rd_low_local = rd_curr_num - (rd_curr_num >> 2);
                openib_btl->qps[qp].u.srq_qp.srq_limit_event_flag = true;
            } else {
                openib_btl->qps[qp].u.srq_qp.rd_curr_num = rd_num;
                openib_btl->qps[qp].u.srq_qp.rd_low_local = mca_btl_openib_component.qp_infos[qp].rd_low;
                /* Not used in this case, but we don't need a garbage */
                mca_btl_openib_component.qp_infos[qp].u.srq_qp.srq_limit = 0;
                openib_btl->qps[qp].u.srq_qp.srq_limit_event_flag = false;
            }
        }
    }

    openib_btl->srqs_created = true;

    return OPAL_SUCCESS;
}

static int openib_btl_prepare(struct mca_btl_openib_module_t* openib_btl)
{
    int rc = OPAL_SUCCESS;
    opal_mutex_lock(&openib_btl->ib_lock);
    if (!openib_btl->srqs_created &&
            (mca_btl_openib_component.num_srq_qps > 0 ||
             mca_btl_openib_component.num_xrc_qps > 0)) {
        rc = create_srq(openib_btl);
    }
    opal_mutex_unlock(&openib_btl->ib_lock);
    return rc;
}


static int openib_btl_size_queues(struct mca_btl_openib_module_t* openib_btl)
{
    uint32_t send_cqes, recv_cqes;
    int rc = OPAL_SUCCESS, qp;
    mca_btl_openib_device_t *device = openib_btl->device;

    opal_mutex_lock(&openib_btl->ib_lock);
    /* figure out reasonable sizes for completion queues */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        if(BTL_OPENIB_QP_TYPE_SRQ(qp)) {
            send_cqes = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
            recv_cqes = mca_btl_openib_component.qp_infos[qp].rd_num;
        } else {
            send_cqes = (mca_btl_openib_component.qp_infos[qp].rd_num +
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv) * openib_btl->num_peers;
            recv_cqes = send_cqes;
        }

        opal_mutex_lock(&openib_btl->device->device_lock);
        openib_btl->device->cq_size[qp_cq_prio(qp)] += recv_cqes;
        openib_btl->device->cq_size[BTL_OPENIB_LP_CQ] += send_cqes;
        opal_mutex_unlock(&openib_btl->device->device_lock);
    }

    rc = adjust_cq(device, BTL_OPENIB_HP_CQ);
    if (OPAL_SUCCESS != rc) {
        goto out;
    }

    rc = adjust_cq(device, BTL_OPENIB_LP_CQ);
    if (OPAL_SUCCESS != rc) {
        goto out;
    }

out:
    opal_mutex_unlock(&openib_btl->ib_lock);
    return rc;
}

mca_btl_openib_transport_type_t mca_btl_openib_get_transport_type(mca_btl_openib_module_t* openib_btl)
{
/* If we have a driver with RDMAoE supporting as the device struct contains the same type (IB) for
   IBV_LINK_LAYER_INFINIBAND and IBV_LINK_LAYER_ETHERNET link layers and the single way
   to detect this fact is to check their link_layer fields in a port_attr struct.
   If our driver doesn't support this feature => the checking of transport type in device struct will be enough.
   If the driver doesn't support completely transport types =>
   our assumption that it is very old driver - that supports IB devices only */

#ifdef HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE
    switch(openib_btl->device->ib_dev->transport_type) {
        case IBV_TRANSPORT_IB:
#if HAVE_DECL_IBV_LINK_LAYER_ETHERNET
            switch(openib_btl->ib_port_attr.link_layer) {
                case IBV_LINK_LAYER_ETHERNET:
                    return MCA_BTL_OPENIB_TRANSPORT_RDMAOE;

                case IBV_LINK_LAYER_INFINIBAND:
                    return MCA_BTL_OPENIB_TRANSPORT_IB;
            /* It is not possible that a device struct contains
               IB transport and port was configured to IBV_LINK_LAYER_UNSPECIFIED */
                case IBV_LINK_LAYER_UNSPECIFIED:
                default:
                    return MCA_BTL_OPENIB_TRANSPORT_UNKNOWN;
            }
#endif
            return MCA_BTL_OPENIB_TRANSPORT_IB;

        case IBV_TRANSPORT_IWARP:
            return MCA_BTL_OPENIB_TRANSPORT_IWARP;

        case IBV_TRANSPORT_UNKNOWN:
        default:
            return MCA_BTL_OPENIB_TRANSPORT_UNKNOWN;
    }
#else
    return MCA_BTL_OPENIB_TRANSPORT_IB;
#endif
}

static int mca_btl_openib_tune_endpoint(mca_btl_openib_module_t* openib_btl,
                                            mca_btl_base_endpoint_t* endpoint)
{
    opal_btl_openib_ini_values_t values;
    char* recv_qps = NULL;
    int ret;

    if(mca_btl_openib_get_transport_type(openib_btl) != endpoint->rem_info.rem_transport_type) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "conflicting transport types", true,
                       opal_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev),
                       (openib_btl->device->ib_dev_attr).vendor_id,
                       (openib_btl->device->ib_dev_attr).vendor_part_id,
                       mca_btl_openib_transport_name_strings[mca_btl_openib_get_transport_type(openib_btl)],
                       opal_get_proc_hostname(endpoint->endpoint_proc->proc_opal),
                       endpoint->rem_info.rem_vendor_id,
                       endpoint->rem_info.rem_vendor_part_id,
                       mca_btl_openib_transport_name_strings[endpoint->rem_info.rem_transport_type]);

        return OPAL_ERROR;
    }

    memset(&values, 0, sizeof(opal_btl_openib_ini_values_t));
    ret = opal_btl_openib_ini_query(endpoint->rem_info.rem_vendor_id,
                          endpoint->rem_info.rem_vendor_part_id, &values);

    if (OPAL_SUCCESS != ret &&
        OPAL_ERR_NOT_FOUND != ret) {
        opal_show_help("help-mpi-btl-openib.txt",
                       "error in device init", true,
                       opal_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev));
        return ret;
    }

    if(openib_btl->device->mtu < endpoint->rem_info.rem_mtu) {
        endpoint->rem_info.rem_mtu = openib_btl->device->mtu;
    }

    endpoint->use_eager_rdma = openib_btl->device->use_eager_rdma &
                               endpoint->use_eager_rdma;

    /* Receive queues checking */

    /* In this check we assume that the command line or INI file parameters are the same
       for all processes on all machines. The assumption is correct for 99.9999% of users,
       if a user distributes different INI files or parameters for different node/procs,
       it is on his own responsibility */
    switch(mca_btl_openib_component.receive_queues_source) {
    case MCA_BASE_VAR_SOURCE_COMMAND_LINE:
    case MCA_BASE_VAR_SOURCE_ENV:
    case MCA_BASE_VAR_SOURCE_FILE:
    case MCA_BASE_VAR_SOURCE_SET:
    case MCA_BASE_VAR_SOURCE_OVERRIDE:
        break;

        /* If the queues configuration was set from command line
           (with --mca btl_openib_receive_queues parameter) => both sides have a same configuration */

        /* In this case the local queues configuration was gotten from INI file =>
           not possible that remote side got its queues configuration from command line =>
           (by prio) the configuration was set from INI file or (if not configure)
           by default queues configuration */
    case BTL_OPENIB_RQ_SOURCE_DEVICE_INI:
        if(NULL != values.receive_queues) {
            recv_qps = values.receive_queues;
        } else {
            recv_qps = mca_btl_openib_component.default_recv_qps;
        }

        if(0 != strcmp(mca_btl_openib_component.receive_queues,
                       recv_qps)) {
            opal_show_help("help-mpi-btl-openib.txt",
                           "unsupported queues configuration", true,
                           opal_process_info.nodename,
                           ibv_get_device_name(openib_btl->device->ib_dev),
                           (openib_btl->device->ib_dev_attr).vendor_id,
                           (openib_btl->device->ib_dev_attr).vendor_part_id,
                           mca_btl_openib_component.receive_queues,
                           opal_get_proc_hostname(endpoint->endpoint_proc->proc_opal),
                           endpoint->rem_info.rem_vendor_id,
                           endpoint->rem_info.rem_vendor_part_id,
                           recv_qps);

            return OPAL_ERROR;
        }
        break;

        /* If the local queues configuration was set
           by default queues => check all possible cases for remote side and compare */
    case  MCA_BASE_VAR_SOURCE_DEFAULT:
        if(NULL != values.receive_queues) {
            if(0 != strcmp(mca_btl_openib_component.receive_queues,
                           values.receive_queues)) {
                opal_show_help("help-mpi-btl-openib.txt",
                               "unsupported queues configuration", true,
                               opal_process_info.nodename,
                               ibv_get_device_name(openib_btl->device->ib_dev),
                               (openib_btl->device->ib_dev_attr).vendor_id,
                               (openib_btl->device->ib_dev_attr).vendor_part_id,
                               mca_btl_openib_component.receive_queues,
                               opal_get_proc_hostname(endpoint->endpoint_proc->proc_opal),
                               endpoint->rem_info.rem_vendor_id,
                               endpoint->rem_info.rem_vendor_part_id,
                               values.receive_queues);

                return OPAL_ERROR;
            }
        }
        break;
    }

    return OPAL_SUCCESS;
}

static int prepare_device_for_use (mca_btl_openib_device_t *device)
{
    mca_btl_openib_frag_init_data_t *init_data;
    int rc = OPAL_SUCCESS, length;

    opal_mutex_lock(&device->device_lock);

    if (device->ready_for_use) {
        goto exit;
    }

    /* For each btl module that we made - find every
       base device that doesn't have device->qps setup on it yet (remember
       that some modules may share the same device, so when going through
       to loop, we may hit a device that was already setup earlier in
       the loop).

       We may to call for prepare_device_for_use() only after adding the btl
       to mca_btl_openib_component.openib_btls, since the prepare_device_for_use
       adds device to async thread that require access to
       mca_btl_openib_component.openib_btls.
    */

    /* Setup the device qps info */
    device->qps = (mca_btl_openib_device_qp_t*)
        calloc(mca_btl_openib_component.num_qps,
               sizeof(mca_btl_openib_device_qp_t));
    if (NULL == device->qps) {
        BTL_ERROR(("Failed malloc: %s:%d", __FILE__, __LINE__));
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    for (int qp_index = 0 ; qp_index < mca_btl_openib_component.num_qps ; qp_index++) {
        OBJ_CONSTRUCT(&device->qps[qp_index].send_free, opal_free_list_t);
        OBJ_CONSTRUCT(&device->qps[qp_index].recv_free, opal_free_list_t);
    }

    device->got_fatal_event = false;
    device->got_port_event = false;
    mca_btl_openib_async_add_device (device);

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    /* Prepare data for thread, but not starting it */
    OBJ_CONSTRUCT(&device->thread, opal_thread_t);
    device->thread.t_run = mca_btl_openib_progress_thread;
    device->thread.t_arg = device;
    device->progress = false;
#endif

#if HAVE_XRC
    /* if user configured to run with XRC qp and the device doesn't
     * support it - we should ignore this device. Maybe we have another
     * one that has XRC support
     */
    if (!(device->ib_dev_attr.device_cap_flags & IBV_DEVICE_XRC) &&
            MCA_BTL_XRC_ENABLED) {
        opal_show_help("help-mpi-btl-openib.txt",
                "XRC on device without XRC support", true,
                mca_btl_openib_component.num_xrc_qps,
                ibv_get_device_name(device->ib_dev),
                opal_process_info.nodename);
        rc = OPAL_ERROR;
        goto exit;
    }

    if (MCA_BTL_XRC_ENABLED) {
        if (OPAL_SUCCESS != mca_btl_openib_open_xrc_domain(device)) {
            BTL_ERROR(("XRC Internal error. Failed to open xrc domain"));
            rc = OPAL_ERROR;
            goto exit;
        }
    }
#endif

    device->endpoints = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(device->endpoints, 10, INT_MAX, 10);
    opal_pointer_array_add(&mca_btl_openib_component.devices, device);
    if (mca_btl_openib_component.max_eager_rdma > 0 &&
        device->use_eager_rdma) {
        device->eager_rdma_buffers =
            (mca_btl_base_endpoint_t **) calloc((size_t) mca_btl_openib_component.max_eager_rdma * device->btls,
                                            sizeof(mca_btl_openib_endpoint_t*));
        if(NULL == device->eager_rdma_buffers) {
            BTL_ERROR(("Memory allocation fails"));
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
    }

    init_data = (mca_btl_openib_frag_init_data_t *) malloc(sizeof(mca_btl_openib_frag_init_data_t));
    if (NULL == init_data) {
        if (mca_btl_openib_component.max_eager_rdma > 0 &&
            device->use_eager_rdma) {
            /* cleanup */
            free (device->eager_rdma_buffers);
            device->eager_rdma_buffers = NULL;
        }
        BTL_ERROR(("Memory allocation fails"));
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    length = sizeof(mca_btl_openib_header_t) +
        sizeof(mca_btl_openib_footer_t) +
        sizeof(mca_btl_openib_eager_rdma_header_t);

    init_data->order = MCA_BTL_NO_ORDER;
    init_data->list = &device->send_free_control;

    rc = opal_free_list_init(&device->send_free_control,
                sizeof(mca_btl_openib_send_control_frag_t), opal_cache_line_size,
                OBJ_CLASS(mca_btl_openib_send_control_frag_t), length,
                mca_btl_openib_component.buffer_alignment,
                mca_btl_openib_component.ib_free_list_num, -1,
                mca_btl_openib_component.ib_free_list_inc,
                device->mpool, 0, NULL, mca_btl_openib_frag_init,
                init_data);
    if (OPAL_SUCCESS != rc) {
        /* If we're "out of memory", this usually means that we ran
           out of registered memory, so show that error message */
        if (OPAL_ERR_OUT_OF_RESOURCE == rc ||
            OPAL_ERR_TEMP_OUT_OF_RESOURCE == rc) {
            errno = ENOMEM;
            mca_btl_openib_show_init_error(__FILE__, __LINE__,
                                           "opal_free_list_init",
                                           ibv_get_device_name(device->ib_dev));
        }
        goto exit;
    }

    /* setup all the qps */
    for (int qp = 0 ; qp < mca_btl_openib_component.num_qps ; qp++) {
        init_data = (mca_btl_openib_frag_init_data_t *) malloc(sizeof(mca_btl_openib_frag_init_data_t));
        if (NULL == init_data) {
            BTL_ERROR(("Memory allocation fails"));
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        /* Initialize pool of send fragments */
        length = sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t) +
            sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_footer_t) +
            mca_btl_openib_component.qp_infos[qp].size;

        init_data->order = qp;
        init_data->list = &device->qps[qp].send_free;

        rc = opal_free_list_init (init_data->list,
                    sizeof(mca_btl_openib_send_frag_t), opal_cache_line_size,
                    OBJ_CLASS(mca_btl_openib_send_frag_t), length,
                    mca_btl_openib_component.buffer_alignment,
                    mca_btl_openib_component.ib_free_list_num,
                    mca_btl_openib_component.ib_free_list_max,
                    mca_btl_openib_component.ib_free_list_inc,
                    device->mpool, 0, NULL, mca_btl_openib_frag_init,
                    init_data);
        if (OPAL_SUCCESS != rc) {
            /* If we're "out of memory", this usually means that we
               ran out of registered memory, so show that error
               message */
            if (OPAL_ERR_OUT_OF_RESOURCE == rc ||
                OPAL_ERR_TEMP_OUT_OF_RESOURCE == rc) {
                errno = ENOMEM;
                mca_btl_openib_show_init_error(__FILE__, __LINE__,
                                               "opal_free_list_init",
                                               ibv_get_device_name(device->ib_dev));
            }
            goto exit;
        }

        init_data = (mca_btl_openib_frag_init_data_t *) malloc(sizeof(mca_btl_openib_frag_init_data_t));
        length = sizeof(mca_btl_openib_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t) +
            sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_footer_t) +
            mca_btl_openib_component.qp_infos[qp].size;

        init_data->order = qp;
        init_data->list = &device->qps[qp].recv_free;

        if(OPAL_SUCCESS != opal_free_list_init (init_data->list,
                    sizeof(mca_btl_openib_recv_frag_t), opal_cache_line_size,
                    OBJ_CLASS(mca_btl_openib_recv_frag_t),
                    length, mca_btl_openib_component.buffer_alignment,
                    mca_btl_openib_component.ib_free_list_num,
                    mca_btl_openib_component.ib_free_list_max,
                    mca_btl_openib_component.ib_free_list_inc,
                    device->mpool, 0, NULL, mca_btl_openib_frag_init,
                    init_data)) {
            rc = OPAL_ERROR;
            goto exit;
        }
    }

    device->ready_for_use = true;

exit:
    opal_mutex_unlock(&device->device_lock);
    return rc;
}

static int init_ib_proc_nolock(mca_btl_openib_module_t* openib_btl, mca_btl_openib_proc_t* ib_proc,
                        volatile mca_btl_base_endpoint_t **endpoint_ptr,
                        int local_port_cnt, int btl_rank)
{
    int rem_port_cnt, matching_port = -1, j, rc;
    mca_btl_base_endpoint_t *endpoint;
    opal_btl_openib_connect_base_module_t *local_cpc;
    opal_btl_openib_connect_base_module_data_t *remote_cpc_data;

    *endpoint_ptr = NULL;

    /* check if the remote proc has any ports that:
       - on the same subnet as the local proc, and
       - on that subnet, has a CPC in common with the local proc
    */

    rem_port_cnt = 0;
    BTL_VERBOSE(("got %d port_infos ", ib_proc->proc_port_count));
    for (j = 0; j < (int) ib_proc->proc_port_count; j++){
        BTL_VERBOSE(("got a subnet %016" PRIx64,
                     ib_proc->proc_ports[j].pm_port_info.subnet_id));
        if (ib_proc->proc_ports[j].pm_port_info.subnet_id ==
            openib_btl->port_info.subnet_id) {
            BTL_VERBOSE(("Got a matching subnet!"));
            if (rem_port_cnt == btl_rank) {
                matching_port = j;
            }
            rem_port_cnt++;
        }
    }

    if (0 == rem_port_cnt) {
        /* no use trying to communicate with this endpoint */
        BTL_VERBOSE(("No matching subnet id/CPC was found, moving on.. "));
        return OPAL_ERROR;
    }

    /* If this process has multiple ports on a single subnet ID,
       and the report proc also has multiple ports on this same
       subnet ID, the default connection pattern is:

                  LOCAL                   REMOTE PEER
             1st port on subnet X <--> 1st port on subnet X
             2nd port on subnet X <--> 2nd port on subnet X
             3nd port on subnet X <--> 3nd port on subnet X
             ...etc.

       Note that the port numbers may not be contiguous, and they
       may not be the same on either side.  Hence the "1st", "2nd",
       "3rd, etc. notation, above.

       Hence, if the local "rank" of this module's port on the
       subnet ID is greater than the total number of ports on the
       peer on this same subnet, then we have no match.  So skip
       this connection.  */
    if (rem_port_cnt < local_port_cnt && btl_rank >= rem_port_cnt) {
        BTL_VERBOSE(("Not enough remote ports on this subnet id, moving on.. "));
        return OPAL_ERROR;
    }

    /* Now that we have verified that we're on the same subnet and
       the remote peer has enough ports, see if that specific port
       on the peer has a matching CPC. */
    assert(btl_rank <= ib_proc->proc_port_count);
    assert(matching_port != -1);
    if (OPAL_SUCCESS !=
        opal_btl_openib_connect_base_find_match(openib_btl,
                                                &(ib_proc->proc_ports[matching_port]),
                                                &local_cpc,
                                                &remote_cpc_data)) {
        return OPAL_ERROR;
    }

    /* The btl_proc datastructure is shared by all IB BTL
     * instances that are trying to reach this destination.
     * Cache the peer instance on the btl_proc.
     */
    endpoint = OBJ_NEW(mca_btl_openib_endpoint_t);
    assert(((opal_object_t*)endpoint)->obj_reference_count == 1);
    if(NULL == endpoint) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }


#if HAVE_XRC
    if (MCA_BTL_XRC_ENABLED) {
        int rem_port_cnt = 0;
        for(j = 0; j < (int) ib_proc->proc_port_count; j++) {
            if(ib_proc->proc_ports[j].pm_port_info.subnet_id ==
                    openib_btl->port_info.subnet_id) {
                if (rem_port_cnt == btl_rank)
                    break;
                else
                    rem_port_cnt ++;
            }
        }

        assert(rem_port_cnt == btl_rank);
        /* Push the subnet/lid/jobid to xrc hash */
        rc = mca_btl_openib_ib_address_add_new(
                ib_proc->proc_ports[j].pm_port_info.lid,
                ib_proc->proc_ports[j].pm_port_info.subnet_id,
                ib_proc->proc_opal->proc_name.jobid, endpoint);
        if (OPAL_SUCCESS != rc ) {
            return OPAL_ERROR;
        }
    }
#endif
    mca_btl_openib_endpoint_init(openib_btl, endpoint,
                                 local_cpc,
                                 &(ib_proc->proc_ports[matching_port]),
                                 remote_cpc_data);

    rc = mca_btl_openib_proc_insert(ib_proc, endpoint);
    if (OPAL_SUCCESS != rc) {
        OBJ_RELEASE(endpoint);
        return OPAL_ERROR;
    }

     if(OPAL_SUCCESS != mca_btl_openib_tune_endpoint(openib_btl, endpoint)) {
        OBJ_RELEASE(endpoint);
        return OPAL_ERROR;
    }

    /* protect device because several endpoints for different ib_proc's
     * may be simultaneously initialized */
    opal_mutex_lock(&openib_btl->device->device_lock);
    endpoint->index = opal_pointer_array_add(openib_btl->device->endpoints, (void*)endpoint);
    opal_mutex_unlock(&openib_btl->device->device_lock);

    if( 0 > endpoint->index ) {
        OBJ_RELEASE(endpoint);
        return OPAL_ERROR;
    }

    /* Tell the selected CPC that it won.  NOTE: This call is
       outside of / separate from mca_btl_openib_endpoint_init()
       because this function likely needs the endpoint->index. */
    if (NULL != local_cpc->cbm_endpoint_init) {
        rc = local_cpc->cbm_endpoint_init(endpoint);
        if (OPAL_SUCCESS != rc) {
            OBJ_RELEASE(endpoint);
            return OPAL_ERROR;
        }
    }

    *endpoint_ptr = endpoint;
    return OPAL_SUCCESS;
}

static int get_openib_btl_params(mca_btl_openib_module_t* openib_btl, int *port_cnt_ptr)
{
    int port_cnt = 0, rank = -1, j;
    for(j=0; j < mca_btl_openib_component.ib_num_btls; j++){
        if(mca_btl_openib_component.openib_btls[j]->port_info.subnet_id
           == openib_btl->port_info.subnet_id) {
            if(openib_btl == mca_btl_openib_component.openib_btls[j]) {
                rank = port_cnt;
            }
            port_cnt++;
        }
    }
    *port_cnt_ptr = port_cnt;
    return rank;
}

/*
 *  add a proc to this btl module
 *    creates an endpoint that is setup on the
 *    first send to the endpoint
 */
int mca_btl_openib_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct opal_proc_t **procs,
    struct mca_btl_base_endpoint_t** peers,
    opal_bitmap_t* reachable)
{
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*)btl;
    size_t nprocs_new_loc = 0, nprocs_new = 0;
    int i,j, rc;
    int lcl_subnet_id_port_cnt = 0;
    int btl_rank = 0;
    volatile mca_btl_base_endpoint_t* endpoint;

    btl_rank = get_openib_btl_params(openib_btl, &lcl_subnet_id_port_cnt);
    if( 0 > btl_rank ){
        return OPAL_ERR_NOT_FOUND;
    }

#if HAVE_XRC
    if(MCA_BTL_XRC_ENABLED &&
            NULL == mca_btl_openib_component.ib_addr_table.ht_table) {
        if(OPAL_SUCCESS != opal_hash_table_init(
                    &mca_btl_openib_component.ib_addr_table, nprocs)) {
            BTL_ERROR(("XRC internal error. Failed to allocate ib_table"));
            return OPAL_ERROR;
        }
    }
#endif

    rc = prepare_device_for_use (openib_btl->device);
    if (OPAL_SUCCESS != rc) {
        BTL_ERROR(("could not prepare openib device for use"));
        return rc;
    }

    if (0 == openib_btl->num_peers) {
        /* ensure completion queues are created before attempting to
         * make a loop-back queue pair */
        rc = openib_btl_size_queues(openib_btl);
        if (OPAL_SUCCESS != rc) {
            BTL_ERROR(("error creating cqs"));
            return rc;
        }
    }

    /* prepare all proc's and account them properly */
    for (i = 0, nprocs_new_loc = 0 ; i < (int) nprocs; i++) {
        struct opal_proc_t* proc = procs[i];
        mca_btl_openib_proc_t* ib_proc;

#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
        /* Most current iWARP adapters (June 2008) cannot handle
           talking to other processes on the same host (!) -- so mark
           them as unreachable (need to use sm).  So for the moment,
           we'll just mark any local peer on an iWARP NIC as
           unreachable.  See trac ticket #1352. */
        if (IBV_TRANSPORT_IWARP == openib_btl->device->ib_dev->transport_type &&
            OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags)) {
            continue;
        }
#endif

        if(NULL == (ib_proc = mca_btl_openib_proc_get_locked(proc)) ) {
            /* if we don't have connection info for this process, it's
             * okay because some other method might be able to reach it,
             * so just mark it as unreachable by us */
            continue;
        }

        /* account this openib_btl in this proc */
        rc = mca_btl_openib_proc_reg_btl(ib_proc, openib_btl);

        opal_mutex_unlock( &ib_proc->proc_lock );

        switch( rc ){
        case OPAL_SUCCESS:
            /* this is a new process to this openib btl */
            nprocs_new++;
            if (OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags)) {
                nprocs_new_loc ++;
            }
            break;
        case OPAL_ERR_RESOURCE_BUSY:
            /* process was accounted earlier in this openib btl */
            break;
        default:
            /* unexpected error, e.g. out of mem */
            return rc;
        }
    }

    if (nprocs_new) {
        OPAL_THREAD_ADD32(&openib_btl->num_peers, nprocs_new);

        /* adjust cq sizes given the new procs */
        rc = openib_btl_size_queues (openib_btl);
        if (OPAL_SUCCESS != rc) {
            BTL_ERROR(("error creating cqs"));
            return rc;
        }
    }

    rc = openib_btl_prepare (openib_btl);
    if (OPAL_SUCCESS != rc) {
        BTL_ERROR(("could not prepare openib btl module for use"));
        return rc;
    }

    opal_mutex_lock(&openib_btl->device->device_lock);
    openib_btl->local_procs += nprocs_new_loc;
    if( 0 < nprocs_new_loc ){
        openib_btl->device->mem_reg_max = openib_btl->device->mem_reg_max_total / openib_btl->local_procs;
    }
    opal_mutex_unlock(&openib_btl->device->device_lock);

    /* prepare endpoints */
    for (i = 0, nprocs_new_loc = 0 ; i < (int) nprocs; i++) {
        struct opal_proc_t* proc = procs[i];
        mca_btl_openib_proc_t* ib_proc;
        bool found_existing = false;

        opal_output(-1, "add procs: adding proc %d", i);

#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
        /* Most current iWARP adapters (June 2008) cannot handle
           talking to other processes on the same host (!) -- so mark
           them as unreachable (need to use sm).  So for the moment,
           we'll just mark any local peer on an iWARP NIC as
           unreachable.  See trac ticket #1352. */
        if (IBV_TRANSPORT_IWARP == openib_btl->device->ib_dev->transport_type &&
            OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags)) {
            continue;
        }
#endif

        if(NULL == (ib_proc = mca_btl_openib_proc_get_locked(proc)) ) {
            /* if we don't have connection info for this process, it's
             * okay because some other method might be able to reach it,
             * so just mark it as unreachable by us */
            continue;
        }

        found_existing = false;

        for (j = 0 ; j < (int) ib_proc->proc_endpoint_count ; ++j) {
            endpoint = ib_proc->proc_endpoints[j];
            if (endpoint->endpoint_btl == openib_btl) {
                found_existing = true;
                break;
            }
        }

        if( !found_existing ) {
            rc = init_ib_proc_nolock(openib_btl, ib_proc, &endpoint,
                                     lcl_subnet_id_port_cnt, btl_rank);
            if( OPAL_SUCCESS == rc ){
                found_existing = true;
            }
        }
        opal_mutex_unlock( &ib_proc->proc_lock );

        if (found_existing) {
            if (reachable) {
                opal_bitmap_set_bit(reachable, i);
            }
            peers[i] = (mca_btl_base_endpoint_t*)endpoint;
        }

    }

    return OPAL_SUCCESS;
}

struct mca_btl_base_endpoint_t *mca_btl_openib_get_ep (struct mca_btl_base_module_t *btl, struct opal_proc_t *proc)
{
    mca_btl_openib_module_t *openib_btl = (mca_btl_openib_module_t *) btl;
    volatile mca_btl_base_endpoint_t *endpoint = NULL;
    int local_port_cnt = 0, btl_rank, rc;
    mca_btl_openib_proc_t *ib_proc;

    rc = prepare_device_for_use (openib_btl->device);
    if (OPAL_SUCCESS != rc) {
        BTL_ERROR(("could not prepare openib device for use"));
        return NULL;
    }

    if (NULL == (ib_proc = mca_btl_openib_proc_get_locked(proc))) {
        /* if we don't have connection info for this process, it's
         * okay because some other method might be able to reach it,
         * so just mark it as unreachable by us */
        return NULL;
    }

    rc = mca_btl_openib_proc_reg_btl(ib_proc, openib_btl);

    switch( rc ){
    case OPAL_SUCCESS:
        /* unlock first to avoid possible deadlocks */
        opal_mutex_unlock(&ib_proc->proc_lock);

        /* this is a new process to this openib btl
         * account this procs if need */
        OPAL_THREAD_ADD32(&openib_btl->num_peers, 1);
        rc = openib_btl_size_queues(openib_btl);
        if (OPAL_SUCCESS != rc) {
            BTL_ERROR(("error creating cqs"));
            return NULL;
        }

        if( OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags) ) {
            opal_mutex_lock(&openib_btl->ib_lock);
            openib_btl->local_procs += 1;
            openib_btl->device->mem_reg_max = openib_btl->device->mem_reg_max_total / openib_btl->local_procs;
            opal_mutex_unlock(&openib_btl->ib_lock);
        }

        /* lock process back */
        opal_mutex_lock(&ib_proc->proc_lock);
        break;
    case OPAL_ERR_RESOURCE_BUSY:
        /* process was accounted earlier in this openib btl */
        break;
    default:
        /* unexpected error, e.g. out of mem */
        BTL_ERROR(("Unexpected OPAL error %d", rc));
        return NULL;
    }

    rc = openib_btl_prepare(openib_btl);
    if (OPAL_SUCCESS != rc) {
        BTL_ERROR(("could not prepare openib btl structure for use"));
        goto exit;
    }

    for (size_t j = 0 ; j < ib_proc->proc_endpoint_count ; ++j) {
        endpoint = ib_proc->proc_endpoints[j];
        if (endpoint->endpoint_btl == openib_btl) {
            goto exit;
        }
    }

    endpoint = NULL;

    btl_rank = get_openib_btl_params(openib_btl, &local_port_cnt);
    if( 0 > btl_rank ){
        goto exit;
    }

    (void)init_ib_proc_nolock(openib_btl, ib_proc, &endpoint,
                            local_port_cnt, btl_rank);

exit:
    opal_mutex_unlock(&ib_proc->proc_lock);

    return (struct mca_btl_base_endpoint_t *)endpoint;
}

/*
 * delete the proc as reachable from this btl module
 */
int mca_btl_openib_del_procs(struct mca_btl_base_module_t* btl,
        size_t nprocs,
        struct opal_proc_t **procs,
        struct mca_btl_base_endpoint_t ** peers)
{
    int i, ep_index;
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;
    mca_btl_openib_endpoint_t* endpoint;

    for (i=0 ; i < (int) nprocs ; i++) {
        mca_btl_base_endpoint_t* del_endpoint = peers[i];
        for(ep_index=0;
            ep_index < opal_pointer_array_get_size(openib_btl->device->endpoints);
            ep_index++) {
            endpoint = (mca_btl_openib_endpoint_t *)
                opal_pointer_array_get_item(openib_btl->device->endpoints,
                        ep_index);
            if(!endpoint || endpoint->endpoint_btl != openib_btl) {
                continue;
            }
            if (endpoint == del_endpoint) {
                int j;
                BTL_VERBOSE(("in del_procs %d, setting another endpoint to null",
                             ep_index));
                /* remove the endpoint from eager_rdma_buffers */
                for (j=0; j<openib_btl->device->eager_rdma_buffers_count; j++) {
                    if (openib_btl->device->eager_rdma_buffers[j] == endpoint) {
                        /* should it be obj_reference_count == 2 ? */
                        assert(((opal_object_t*)endpoint)->obj_reference_count > 1);
                        OBJ_RELEASE(endpoint);
                        openib_btl->device->eager_rdma_buffers[j] = NULL;
                        /* can we simply break and leave the for loop ? */
                    }
                }
                opal_pointer_array_set_item(openib_btl->device->endpoints,
                        ep_index, NULL);
                assert(((opal_object_t*)endpoint)->obj_reference_count == 1);
                mca_btl_openib_proc_remove(procs[i], endpoint);
                OBJ_RELEASE(endpoint);
            }
        }
    }

    return OPAL_SUCCESS;
}

/*
 *Register callback function for error handling..
 */
int mca_btl_openib_register_error_cb(
                        struct mca_btl_base_module_t* btl,
                        mca_btl_base_module_error_cb_fn_t cbfunc)
{

    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;
    openib_btl->error_cb = cbfunc; /* stash for later */
    return OPAL_SUCCESS;
}

static inline mca_btl_base_descriptor_t *
ib_frag_alloc(mca_btl_openib_module_t *btl, size_t size, uint8_t order,
        uint32_t flags)
{
    int qp;
    opal_free_list_item_t* item = NULL;

    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
         if(mca_btl_openib_component.qp_infos[qp].size >= size) {
             item = opal_free_list_get (&btl->device->qps[qp].send_free);
             if(item)
                 break;
         }
    }
    if(NULL == item)
        return NULL;

    /* not all upper layer users set this */
    to_base_frag(item)->segment.seg_len = size;
    to_base_frag(item)->base.order = order;
    to_base_frag(item)->base.des_flags = flags;

    assert(to_send_frag(item)->qp_idx <= order);
    return &to_base_frag(item)->base;
}

/* check if pending fragment has enough space for coalescing */
static mca_btl_openib_send_frag_t *check_coalescing(opal_list_t *frag_list,
    opal_mutex_t *lock, struct mca_btl_base_endpoint_t *ep, size_t size,
    mca_btl_openib_coalesced_frag_t **cfrag)
{
    mca_btl_openib_send_frag_t *frag = NULL;

    if (opal_list_is_empty(frag_list))
        return NULL;

    OPAL_THREAD_LOCK(lock);
    if (!opal_list_is_empty(frag_list)) {
        int qp;
        size_t total_length;
        opal_list_item_t *i = opal_list_get_first(frag_list);
        frag = to_send_frag(i);
        if(to_com_frag(frag)->endpoint != ep ||
                MCA_BTL_OPENIB_FRAG_CONTROL == openib_frag_type(frag)) {
            OPAL_THREAD_UNLOCK(lock);
            return NULL;
        }

        total_length = size + frag->coalesced_length +
            to_base_frag(frag)->segment.seg_len +
            sizeof(mca_btl_openib_header_coalesced_t);

        qp = to_base_frag(frag)->base.order;

        if(total_length <= mca_btl_openib_component.qp_infos[qp].size) {
            /* make sure we can allocate a coalescing frag before returning success */
            *cfrag = alloc_coalesced_frag();
            if (OPAL_LIKELY(NULL != cfrag)) {
                (*cfrag)->send_frag = frag;
                (*cfrag)->sent = false;

                opal_list_remove_first(frag_list);
            } else {
                frag = NULL;
            }
        } else {
            frag = NULL;
        }
    }
    OPAL_THREAD_UNLOCK(lock);

    return frag;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 * @param size (IN) Size of segment to allocate
 *
 * When allocating a segment we pull a pre-alllocated segment
 * from one of two free lists, an eager list and a max list
 */
mca_btl_base_descriptor_t* mca_btl_openib_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    uint8_t order,
    size_t size,
    uint32_t flags)
{
    mca_btl_openib_module_t *obtl = (mca_btl_openib_module_t*)btl;
    int qp = frag_size_to_order(obtl, size);
    mca_btl_openib_send_frag_t *sfrag = NULL;
    mca_btl_openib_coalesced_frag_t *cfrag = NULL;

    assert(qp != MCA_BTL_NO_ORDER);

    if(mca_btl_openib_component.use_message_coalescing &&
       (flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        int prio = !(flags & MCA_BTL_DES_FLAGS_PRIORITY);

        sfrag = check_coalescing(&ep->qps[qp].no_wqe_pending_frags[prio],
                                 &ep->endpoint_lock, ep, size, &cfrag);

        if (NULL == sfrag) {
            if(BTL_OPENIB_QP_TYPE_PP(qp)) {
                sfrag = check_coalescing(&ep->qps[qp].no_credits_pending_frags[prio],
                                         &ep->endpoint_lock, ep, size, &cfrag);
            } else {
                sfrag = check_coalescing(
                        &obtl->qps[qp].u.srq_qp.pending_frags[prio],
                        &obtl->ib_lock, ep, size, &cfrag);
            }
        }
    }

    if (NULL == sfrag) {
        return ib_frag_alloc((mca_btl_openib_module_t*)btl, size, order, flags);
    }

    /* begin coalescing message */

    /* fix up new coalescing header if this is the first coalesced frag */
    if(sfrag->hdr != sfrag->chdr) {
        mca_btl_openib_control_header_t *ctrl_hdr;
        mca_btl_openib_header_coalesced_t *clsc_hdr;
        uint8_t org_tag;

        org_tag = sfrag->hdr->tag;
        sfrag->hdr = sfrag->chdr;
        ctrl_hdr = (mca_btl_openib_control_header_t*)(sfrag->hdr + 1);
        clsc_hdr = (mca_btl_openib_header_coalesced_t*)(ctrl_hdr + 1);
        sfrag->hdr->tag = MCA_BTL_TAG_IB;
        ctrl_hdr->type = MCA_BTL_OPENIB_CONTROL_COALESCED;
        clsc_hdr->tag = org_tag;
        clsc_hdr->size = to_base_frag(sfrag)->segment.seg_len;
        clsc_hdr->alloc_size = to_base_frag(sfrag)->segment.seg_len;
        if(ep->nbo)
            BTL_OPENIB_HEADER_COALESCED_HTON(*clsc_hdr);
        sfrag->coalesced_length = sizeof(mca_btl_openib_control_header_t) +
            sizeof(mca_btl_openib_header_coalesced_t);
        to_com_frag(sfrag)->sg_entry.addr = (uint64_t)(uintptr_t)sfrag->hdr;
    }

    cfrag->hdr = (mca_btl_openib_header_coalesced_t*)((unsigned char*)(sfrag->hdr + 1) +
                  sfrag->coalesced_length +
                  to_base_frag(sfrag)->segment.seg_len);
    cfrag->hdr = (mca_btl_openib_header_coalesced_t*)BTL_OPENIB_ALIGN_COALESCE_HDR(cfrag->hdr);
    cfrag->hdr->alloc_size = size;

    /* point coalesced frag pointer into a data buffer */
    to_base_frag(cfrag)->segment.seg_addr.pval = cfrag->hdr + 1;
    to_base_frag(cfrag)->segment.seg_len = size;

    /* NTH: there is no reason to append the coalesced fragment here. No more
     * fragments will be added until either send or free has been called on
     * the coalesced frag. */

    to_base_frag(cfrag)->base.des_flags = flags;

    return &to_base_frag(cfrag)->base;
}

/**
 * Return a segment
 *
 * Return the segment to the appropriate
 *  preallocated segment list
 */
int mca_btl_openib_free(
                    struct mca_btl_base_module_t* btl,
                    mca_btl_base_descriptor_t* des)
{
    /* reset those field on free so we will not have to do it on alloc */
    to_base_frag(des)->base.des_flags = 0;
    switch(openib_frag_type(des)) {
        case MCA_BTL_OPENIB_FRAG_SEND:
            to_send_frag(des)->hdr = (mca_btl_openib_header_t*)
                (((unsigned char*)to_send_frag(des)->chdr) +
                sizeof(mca_btl_openib_header_coalesced_t) +
                sizeof(mca_btl_openib_control_header_t));
            to_com_frag(des)->sg_entry.addr =
                (uint64_t)(uintptr_t)to_send_frag(des)->hdr;
            to_send_frag(des)->coalesced_length = 0;
            to_base_frag(des)->segment.seg_addr.pval =
                to_send_frag(des)->hdr + 1;
            assert(!opal_list_get_size(&to_send_frag(des)->coalesced_frags));
            /* fall through */
        default:
            break;
    }

    if (openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_COALESCED && !to_coalesced_frag(des)->sent) {
        mca_btl_openib_send_frag_t *sfrag = to_coalesced_frag(des)->send_frag;

        /* the coalesced fragment would have sent the original fragment but that
         * will not happen so send the fragment now */
        mca_btl_openib_endpoint_send(to_com_frag(sfrag)->endpoint, sfrag);
    }

    MCA_BTL_IB_FRAG_RETURN(des);

    return OPAL_SUCCESS;
}

/**
 * register user buffer or pack
 * data into pre-registered buffer and return a
 * descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 *
 * prepare source's behavior depends on the following:
 * Has a valid memory registration been passed to prepare_src?
 *    if so we attempt to use the pre-registered user-buffer, if the memory registration
 *    is too small (only a portion of the user buffer) then we must reregister the user buffer
 * Has the user requested the memory to be left pinned?
 *    if so we insert the memory registration into a memory tree for later lookup, we
 *    may also remove a previous registration if a MRU (most recently used) list of
 *    registrations is full, this prevents resources from being exhausted.
 * Is the requested size larger than the btl's max send size?
 *    if so and we aren't asked to leave the registration pinned, then we register the memory if
 *    the users buffer is contiguous
 * Otherwise we choose from two free lists of pre-registered memory in which to pack the data into.
 *
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    mca_btl_openib_com_frag_t *frag = NULL;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    void *ptr;

    assert(MCA_BTL_NO_ORDER == order);

    if (max_data + reserve > btl->btl_max_send_size) {
        max_data = btl->btl_max_send_size - reserve;
    }

    frag = (mca_btl_openib_com_frag_t *) mca_btl_openib_alloc (btl, endpoint, order,
                                                               max_data + reserve, flags);
    if (NULL == frag) {
        return NULL;
    }

    ptr = to_base_frag(frag)->segment.seg_addr.pval;

    iov.iov_len = max_data;
    iov.iov_base = (IOVBASE_TYPE *) ( (unsigned char*) ptr + reserve );
    (void) opal_convertor_pack(convertor, &iov, &iov_count, &max_data);

#if OPAL_CUDA_SUPPORT /* CUDA_ASYNC_SEND */
    /* If the convertor is copying the data asynchronously, then record an event
     * that will trigger the callback when it completes.  Mark descriptor as async.
     * No need for this in the case we are not sending any GPU data. */
    if ((convertor->flags & CONVERTOR_CUDA_ASYNC) && (0 != max_data)) {
        mca_common_cuda_record_dtoh_event("btl_openib", (mca_btl_base_descriptor_t *)frag);
        to_base_frag(frag)->base.des_flags = flags | MCA_BTL_DES_FLAGS_CUDA_COPY_ASYNC;
    }
#endif /* OPAL_CUDA_SUPPORT */

    *size = max_data;

    /* not all upper layer users set this */
    to_base_frag(frag)->segment.seg_len = max_data + reserve;

    return &to_base_frag(frag)->base;
}

static int mca_btl_openib_finalize_resources(struct mca_btl_base_module_t* btl) {
    mca_btl_openib_module_t* openib_btl;
    mca_btl_openib_endpoint_t* endpoint;
    int ep_index, i;
    int qp, rc = OPAL_SUCCESS;

    openib_btl = (mca_btl_openib_module_t*) btl;

    /* Sanity check */
    if( mca_btl_openib_component.ib_num_btls <= 0 ) {
        return OPAL_SUCCESS;
    }

    /* Release all QPs */
    if (NULL != openib_btl->device->endpoints) {
        for (ep_index=0;
             ep_index < opal_pointer_array_get_size(openib_btl->device->endpoints);
             ep_index++) {
            endpoint=(mca_btl_openib_endpoint_t *)opal_pointer_array_get_item(openib_btl->device->endpoints,
                                                                              ep_index);
            if(!endpoint) {
                BTL_VERBOSE(("In finalize, got another null endpoint"));
                continue;
            }
            if(endpoint->endpoint_btl != openib_btl) {
                continue;
            }
            for(i = 0; i < openib_btl->device->eager_rdma_buffers_count; i++) {
                if(openib_btl->device->eager_rdma_buffers[i] == endpoint) {
                    openib_btl->device->eager_rdma_buffers[i] = NULL;
                    OBJ_RELEASE(endpoint);
                }
            }
            opal_pointer_array_set_item(openib_btl->device->endpoints,
                                        ep_index, NULL);
            assert(((opal_object_t*)endpoint)->obj_reference_count == 1);
            OBJ_RELEASE(endpoint);
        }
    }

    /* Release SRQ resources */
    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        if(!BTL_OPENIB_QP_TYPE_PP(qp)) {
            MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(
                    &openib_btl->qps[qp].u.srq_qp.pending_frags[0]);
            MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(
                    &openib_btl->qps[qp].u.srq_qp.pending_frags[1]);
            if (NULL != openib_btl->qps[qp].u.srq_qp.srq) {
                opal_mutex_t *lock =
                             &mca_btl_openib_component.srq_manager.lock;

                opal_hash_table_t *srq_addr_table =
                            &mca_btl_openib_component.srq_manager.srq_addr_table;

                opal_mutex_lock(lock);
                if (OPAL_SUCCESS !=
                        opal_hash_table_remove_value_ptr(srq_addr_table,
                                    &openib_btl->qps[qp].u.srq_qp.srq,
                                    sizeof(struct ibv_srq *))) {
                    BTL_VERBOSE(("Failed to remove SRQ  %d entry from hash table.", qp));
                    rc = OPAL_ERROR;
                }
                opal_mutex_unlock(lock);
                if (0 != ibv_destroy_srq(openib_btl->qps[qp].u.srq_qp.srq)) {
                    BTL_VERBOSE(("Failed to close SRQ %d", qp));
                    rc = OPAL_ERROR;
                }
            }

            OBJ_DESTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[0]);
            OBJ_DESTRUCT(&openib_btl->qps[qp].u.srq_qp.pending_frags[1]);
        }
    }

    /* Finalize the CPC modules on this openib module */
    for (i = 0; i < openib_btl->num_cpcs; ++i) {
        if (NULL != openib_btl->cpcs[i]->cbm_finalize) {
            openib_btl->cpcs[i]->cbm_finalize(openib_btl, openib_btl->cpcs[i]);
        }
        free(openib_btl->cpcs[i]);
    }
    free(openib_btl->cpcs);

    /* Release device if there are no more users */
    if(!(--openib_btl->device->btls)) {
        OBJ_RELEASE(openib_btl->device);
    }

    if (NULL != openib_btl->qps) {
        free(openib_btl->qps);
    }

    return rc;
}


int mca_btl_openib_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_openib_module_t* openib_btl;
    int i, rc = OPAL_SUCCESS;

    openib_btl = (mca_btl_openib_module_t*) btl;

    /* Sanity check */
    if( mca_btl_openib_component.ib_num_btls <= 0 ) {
        return 0;
    }

    /* Remove the btl from component list */
    if ( mca_btl_openib_component.ib_num_btls > 0 ) {
        for(i = 0; i < mca_btl_openib_component.ib_num_btls; i++){
            if (mca_btl_openib_component.openib_btls[i] == openib_btl){
                if( OPAL_SUCCESS != (rc = mca_btl_openib_finalize_resources(btl) ) ) {
                    BTL_VERBOSE(("Failed to finalize resources"));
                }
                mca_btl_openib_component.openib_btls[i] =
                    mca_btl_openib_component.openib_btls[mca_btl_openib_component.ib_num_btls-1];
                break;
            }
        }
    }

    mca_btl_openib_component.ib_num_btls--;

    OBJ_DESTRUCT(&openib_btl->ib_lock);
    free(openib_btl);

    BTL_VERBOSE(("Success in closing BTL resources"));

    return rc;
}

/*
 *  Send immediate - Minimum function calls minimum checks, send the data ASAP.
 *  If BTL can't to send the messages imidiate, it creates messages descriptor
 *  returns it to PML.
 */
int mca_btl_openib_sendi( struct mca_btl_base_module_t* btl,
        struct mca_btl_base_endpoint_t* ep,
        struct opal_convertor_t* convertor,
        void* header,
        size_t header_size,
        size_t payload_size,
        uint8_t order,
        uint32_t flags,
        mca_btl_base_tag_t tag,
        mca_btl_base_descriptor_t** descriptor)
{
    mca_btl_openib_module_t *obtl = (mca_btl_openib_module_t*)btl;
    size_t size = payload_size + header_size;
    int qp = frag_size_to_order(obtl, size),
        prio = flags & MCA_BTL_DES_FLAGS_PRIORITY,
        ib_rc;
    bool do_rdma = false;
    opal_free_list_item_t* item = NULL;
    mca_btl_openib_frag_t *frag;
    mca_btl_openib_header_t *hdr;
    int send_signaled;
    int rc;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);

    if (OPAL_UNLIKELY(MCA_BTL_IB_CONNECTED != ep->endpoint_state)) {
        goto cant_send;
    }

    /* If it is pending messages on the qp - we can not send */
    if(OPAL_UNLIKELY(!opal_list_is_empty(&ep->qps[qp].no_wqe_pending_frags[prio]))) {
        goto cant_send;
    }

#if OPAL_CUDA_GDR_SUPPORT
    /* We do not want to use this path when we have GDR support */
    if (convertor->flags & CONVERTOR_CUDA) {
        goto cant_send;
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */

    /* Allocate WQE */
    if(OPAL_UNLIKELY(qp_get_wqe(ep, qp) < 0)) {
        goto cant_send_wqe;
    }

    /* Allocate fragment */
    item = opal_free_list_get (&obtl->device->qps[qp].send_free);
    if(OPAL_UNLIKELY(NULL == item)) {
        /* we don't return NULL because maybe later we will try to coalesce */
        goto cant_send_wqe;
    }
    frag = to_base_frag(item);
    hdr = to_send_frag(item)->hdr;

    /* eager rdma or send ? Check eager rdma credits */
    /* Note: Maybe we want to implement isend only for eager rdma ?*/
    rc = mca_btl_openib_endpoint_credit_acquire (ep, qp, prio, size, &do_rdma,
                                                 to_send_frag(frag), false);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        goto cant_send_frag;
    }

    frag->segment.seg_len = size;
    frag->base.order = qp;
    frag->base.des_flags = flags;
    hdr->tag = tag;
    to_com_frag(item)->endpoint = ep;

    /* put match header */
    memcpy(frag->segment.seg_addr.pval, header, header_size);

    /* Pack data */
    if(payload_size) {
        size_t max_data;
        struct iovec iov;
        uint32_t iov_count;
        /* pack the data into the supplied buffer */
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)frag->segment.seg_addr.pval + header_size);
        iov.iov_len  = max_data = payload_size;
        iov_count    = 1;

        (void)opal_convertor_pack( convertor, &iov, &iov_count, &max_data);

        assert(max_data == payload_size);
    }

#if BTL_OPENIB_FAILOVER_ENABLED
    send_signaled = 1;
#else
    send_signaled = qp_need_signal(ep, qp, payload_size + header_size, do_rdma);
#endif
    ib_rc = post_send(ep, to_send_frag(item), do_rdma, send_signaled);

    if (!ib_rc) {
        if (0 == send_signaled) {
            MCA_BTL_IB_FRAG_RETURN(frag);
        }
#if BTL_OPENIB_FAILOVER_ENABLED
        else {
            /* Return up in case needed for failover */
            *descriptor = (struct mca_btl_base_descriptor_t *) frag;
        }
#endif
        OPAL_THREAD_UNLOCK(&ep->endpoint_lock);

        return OPAL_SUCCESS;
    }

    /* Failed to send, do clean up all allocated resources */
    if (ep->nbo) {
        BTL_OPENIB_HEADER_NTOH(*hdr);
    }

    mca_btl_openib_endpoint_credit_release (ep, qp, do_rdma, to_send_frag(frag));

cant_send_frag:
    MCA_BTL_IB_FRAG_RETURN(frag);
cant_send_wqe:
    qp_put_wqe (ep, qp);
cant_send:
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
    /* We can not send the data directly, so we just return descriptor */
    if (NULL != descriptor) {
        *descriptor = mca_btl_openib_alloc(btl, ep, order, size, flags);
    }

    return OPAL_ERR_RESOURCE_BUSY;
}
/*
 *  Initiate a send.
 */

int mca_btl_openib_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    mca_btl_base_tag_t tag)

{
    mca_btl_openib_send_frag_t *frag;

    assert(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_SEND ||
            openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_COALESCED);

    if(openib_frag_type(des) == MCA_BTL_OPENIB_FRAG_COALESCED) {
        frag = to_coalesced_frag(des)->send_frag;

        /* save coalesced fragment on a main fragment; we will need it after send
         * completion to free it and to call upper layer callback */
        opal_list_append(&frag->coalesced_frags, (opal_list_item_t*) des);
        frag->coalesced_length += to_coalesced_frag(des)->hdr->alloc_size +
                                   sizeof(mca_btl_openib_header_coalesced_t);

        to_coalesced_frag(des)->sent = true;
        to_coalesced_frag(des)->hdr->tag = tag;
        to_coalesced_frag(des)->hdr->size = des->des_segments->seg_len;
        if(ep->nbo)
            BTL_OPENIB_HEADER_COALESCED_HTON(*to_coalesced_frag(des)->hdr);
    } else {
        frag = to_send_frag(des);
        to_com_frag(des)->endpoint = ep;
        frag->hdr->tag = tag;
    }

    des->des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

    return mca_btl_openib_endpoint_send(ep, frag);
}

static mca_btl_base_registration_handle_t *mca_btl_openib_register_mem (mca_btl_base_module_t *btl,
                                                                        mca_btl_base_endpoint_t *endpoint,
                                                                        void *base, size_t size, uint32_t flags)
{
    mca_btl_openib_reg_t *reg;
    uint32_t mflags = 0;
    int access_flags = flags & MCA_BTL_REG_FLAG_ACCESS_ANY;
    int rc;

#if OPAL_CUDA_GDR_SUPPORT
    if (flags & MCA_BTL_REG_FLAG_CUDA_GPU_MEM) {
        mflags |= MCA_MPOOL_FLAGS_CUDA_GPU_MEM;
    }
#endif /* OPAL_CUDA_GDR_SUPPORT */

    rc = btl->btl_mpool->mpool_register (btl->btl_mpool, base, size, mflags, access_flags,
                                         (mca_mpool_base_registration_t **) &reg);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc || NULL == reg)) {
        return NULL;
    }

    return &reg->btl_handle;
}

static int mca_btl_openib_deregister_mem (mca_btl_base_module_t *btl, mca_btl_base_registration_handle_t *handle)
{
    mca_btl_openib_reg_t *reg = (mca_btl_openib_reg_t *)((intptr_t) handle - offsetof (mca_btl_openib_reg_t, btl_handle));

    btl->btl_mpool->mpool_deregister (btl->btl_mpool, (mca_mpool_base_registration_t *) reg);

    return OPAL_SUCCESS;
}

#if OPAL_ENABLE_FT_CR == 0
int mca_btl_openib_ft_event(int state) {
    return OPAL_SUCCESS;
}
#else
int mca_btl_openib_ft_event(int state) {
    int i;

    if(OPAL_CRS_CHECKPOINT == state) {
        /* Continue must reconstruct the routes (including modex), since we
         * have to tear down the devices completely. */
        opal_cr_continue_like_restart = true;

        /*
         * To keep the node from crashing we need to call ibv_close_device
         * before the checkpoint is taken. To do this we need to tear
         * everything down, and rebuild it all on continue/restart. :(
         */

        /* Shutdown all modules
         * - Do this backwards since the openib_finalize function also loops
         *   over this variable.
         */
        for (i = 0; i < mca_btl_openib_component.ib_num_btls; ++i ) {
            mca_btl_openib_finalize_resources( &(mca_btl_openib_component.openib_btls[i])->super);
        }

        mca_btl_openib_component.devices_count = 0;
        mca_btl_openib_component.ib_num_btls = 0;
        OBJ_DESTRUCT(&mca_btl_openib_component.ib_procs);

        opal_btl_openib_connect_base_finalize();
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ; /* Cleared by forcing the modex, no work needed */
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OPAL_SUCCESS;
}

#endif /* OPAL_ENABLE_FT_CR */
