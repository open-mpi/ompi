/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2009 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/dss/dss.h"
#include "opal_stdint.h"
#include "orte/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/opal_sos.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "ompi/mca/dpm/dpm.h"

#include "btl_openib.h"
#include "btl_openib_endpoint.h" 
#include "btl_openib_proc.h"
#include "connect/connect.h"
#include "orte/util/show_help.h"

#include <infiniband/mad.h>
#include <iba/ib_types.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

typedef enum {
    ENDPOINT_CONNECT_REQUEST,
    ENDPOINT_CONNECT_RESPONSE,
    ENDPOINT_CONNECT_ACK
} connect_message_type_t;

#define SL_NOT_PRESENT                0x7F
#define MAX_GET_SL_REC_RETRIES        20
#define GET_SL_REC_RETRIES_TIMEOUT_MS 2000000

#define IB_SA_QPN                     1
#define IB_GLOBAL_QKEY                0x80010000UL
#define IB_MGMT_BASE_VERSION          1
#define IB_SA_TID_GET_PATH_REC_0      0xCA000000UL
#define IB_SA_TID_GET_PATH_REC_1      0xBEEF0000UL

#ifdef __WINDOWS__
  #pragma pack(push)
  #pragma pack(1)
#endif

#ifdef __WINDOWS__
  #pragma pack(pop)
#endif

static struct mca_btl_openib_sa_qp_cache {
    /* There will be a MR with the one send and receive buffer together */
    /* The send buffer is first, the receive buffer is second */
    /* The receive buffer in a UD queue pair needs room for the 40 byte GRH */
    /* The buffers are first in the structure for page alignment */
    char     send_recv_buffer[sizeof(ib_sa_mad_t) * 2 + 40];
    struct   mca_btl_openib_sa_qp_cache *next;
    struct   ibv_context *context;
    char     *device_name;
    uint32_t port_num;
    struct   ibv_qp *qp;
    struct   ibv_ah *ah;
    struct   ibv_cq *cq;
    struct   ibv_mr *mr;
    struct   ibv_pd *pd;
    struct   ibv_recv_wr rwr;
    struct   ibv_sge rsge;
    uint8_t  sl_values[65536];
} *sa_qp_cache = 0;

static int oob_priority = 50;
static bool rml_recv_posted = false;

static void oob_component_register(void);
static int oob_component_query(mca_btl_openib_module_t *openib_btl, 
                               ompi_btl_openib_connect_base_module_t **cpc);
static int oob_component_finalize(void);

static int oob_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                    mca_btl_base_endpoint_t *endpoint);
static int reply_start_connect(mca_btl_openib_endpoint_t *endpoint,
                               mca_btl_openib_rem_info_t *rem_info);
static int set_remote_info(mca_btl_base_endpoint_t* endpoint,
                           mca_btl_openib_rem_info_t* rem_info);
static int qp_connect_all(mca_btl_base_endpoint_t* endpoint);
static int qp_create_all(mca_btl_base_endpoint_t* endpoint);
static int qp_create_one(mca_btl_base_endpoint_t* endpoint, int qp,
        struct ibv_srq *srq, uint32_t max_recv_wr, uint32_t max_send_wr);
static int send_connect_data(mca_btl_base_endpoint_t* endpoint, 
                             uint8_t message_type);

static void rml_send_cb(int status, orte_process_name_t* endpoint, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata);
static void rml_recv_cb(int status, orte_process_name_t* process_name, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata);
static int init_ud_qp(struct ibv_context *context_arg,
                      struct mca_btl_openib_sa_qp_cache *cache);
static void init_sa_mad(struct mca_btl_openib_sa_qp_cache *cache,
                        ib_sa_mad_t *sag,
                        struct ibv_send_wr *swr,
                        struct ibv_sge *ssge,
                        uint16_t lid,
                        uint16_t rem_lid);
static int get_pathrecord_info(struct mca_btl_openib_sa_qp_cache *cache,
                               ib_sa_mad_t *sag,
                               ib_sa_mad_t *sar,
                               struct ibv_send_wr *swr,
                               uint16_t lid,
                               uint16_t rem_lid);
static int init_device(struct ibv_context *context_arg,
                       struct mca_btl_openib_sa_qp_cache *cache,
                       uint32_t port_num);
static int get_pathrecord_sl(struct ibv_context *context_arg,
                             uint32_t port_num,
                             uint16_t lid,
                             uint16_t rem_lid);
static void free_sa_qp_cache(void);

/*
 * The "component" struct -- the top-level function pointers for the
 * oob connection scheme.
 */
ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_oob = {
    "oob",
    /* Register */
    oob_component_register,
    /* Init */
    NULL,
    /* Query */
    oob_component_query,
    /* Finalize */
    oob_component_finalize,
};

/* Open - this functions sets up any oob specific commandline params */
static void oob_component_register(void)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_oob_priority",
                           "The selection method priority for oob",
                           false, false, oob_priority, &oob_priority);

    if (oob_priority > 100) {
        oob_priority = 100;
    } else if (oob_priority < -1) {
        oob_priority = -1;
    }
}

/*
 * Init function.  Post non-blocking RML receive to accept incoming
 * connection requests.
 */
static int oob_component_query(mca_btl_openib_module_t *btl, 
                               ompi_btl_openib_connect_base_module_t **cpc)
{
    int rc;

    /* If we have the transport_type member, check to ensure we're on
       IB (this CPC will not work with iWarp).  If we do not have the
       transport_type member, then we must be < OFED v1.2, and
       therefore we must be IB. */   
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
    if (BTL_OPENIB_CONNECT_BASE_CHECK_IF_NOT_IB(btl)) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: oob CPC only supported on InfiniBand; skipped on  %s:%d",
                            ibv_get_device_name(btl->device->ib_dev),
                            btl->port_num);
        return OMPI_ERR_NOT_SUPPORTED;
    }
#endif

    if (mca_btl_openib_component.num_xrc_qps > 0) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: oob CPC not supported with XRC receive queues, please try xoob CPC; skipped on %s:%d",
                            ibv_get_device_name(btl->device->ib_dev),
                            btl->port_num);
        return OMPI_ERR_NOT_SUPPORTED;
    }
    /* If this btl supports OOB, then post the RML message.  But
       ensure to only post it *once*, because another btl may have
       come in before this and already posted it. */
    if (!rml_recv_posted) {
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, 
                                     OMPI_RML_TAG_OPENIB,
                                     ORTE_RML_PERSISTENT,
                                     rml_recv_cb,
                                     NULL);
        if (ORTE_SUCCESS != rc) {
            opal_output_verbose(5, mca_btl_base_output,
                                "openib BTL: oob CPC system error %d (%s)",
                                rc, opal_strerror(rc));
            return rc;
        }
        rml_recv_posted = true;
    }

    *cpc = (ompi_btl_openib_connect_base_module_t *) malloc(sizeof(ompi_btl_openib_connect_base_module_t));
    if (NULL == *cpc) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, OMPI_RML_TAG_OPENIB);
        rml_recv_posted = false;
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: oob CPC system error (malloc failed)");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    (*cpc)->data.cbm_component = &ompi_btl_openib_connect_oob;
    (*cpc)->data.cbm_priority = oob_priority;
    (*cpc)->data.cbm_modex_message = NULL;
    (*cpc)->data.cbm_modex_message_len = 0;

    (*cpc)->cbm_endpoint_init = NULL;
    (*cpc)->cbm_start_connect = oob_module_start_connect;
    (*cpc)->cbm_endpoint_finalize = NULL;
    (*cpc)->cbm_finalize = NULL;
    (*cpc)->cbm_uses_cts = false;

    opal_output_verbose(5, mca_btl_base_output,
                        "openib BTL: oob CPC available for use on %s:%d",
                        ibv_get_device_name(btl->device->ib_dev),
                        btl->port_num);
    return OMPI_SUCCESS;
}

/*
 * Connect function.  Start initiation of connections to a remote
 * peer.  We send our Queue Pair information over the RML/OOB
 * communication mechanism.  On completion of our send, a send
 * completion handler is called.
 */
static int oob_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                    mca_btl_base_endpoint_t *endpoint)
{
    int rc;

    if (OMPI_SUCCESS != (rc = qp_create_all(endpoint))) {
        return rc;
    }

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
    if (OMPI_SUCCESS !=
        (rc = send_connect_data(endpoint, ENDPOINT_CONNECT_REQUEST))) {
        BTL_ERROR(("error sending connect request, error code %d", rc)); 
        return rc;
    }

    return OMPI_SUCCESS;
}

static void free_sa_qp_cache(void)
{
    struct mca_btl_openib_sa_qp_cache *cache, *tmp;

    cache = sa_qp_cache;
    while (NULL != cache) {
        /* free cache data */
        if (cache->device_name)
            free(cache->device_name);
        if (NULL != cache->qp)
            ibv_destroy_qp(cache->qp);
        if (NULL != cache->ah)
            ibv_destroy_ah(cache->ah);
        if (NULL != cache->cq)
            ibv_destroy_cq(cache->cq);
        if (NULL != cache->mr)
            ibv_dereg_mr(cache->mr);
        if (NULL != cache->pd)
            ibv_dealloc_pd(cache->pd);
        tmp = cache->next;
        free(cache);
        cache = tmp;
    }
}

/*
 * Component finalize function.  Cleanup RML non-blocking receive.
 */
static int oob_component_finalize(void)
{
    if (rml_recv_posted) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, OMPI_RML_TAG_OPENIB);
        rml_recv_posted = false;
    }

    free_sa_qp_cache();

    return OMPI_SUCCESS;
}

/**************************************************************************/

/*
 * Reply to a `start - connect' message
 */
static int reply_start_connect(mca_btl_openib_endpoint_t *endpoint,
                               mca_btl_openib_rem_info_t *rem_info)
{
    int rc;

    BTL_VERBOSE(("Initialized QPs, LID = %d",
                 ((mca_btl_openib_module_t*)endpoint->endpoint_btl)->lid));

    /* Create local QP's and post receive resources */
    if (OMPI_SUCCESS != (rc = qp_create_all(endpoint))) {
        return rc;
    }

    /* Set the remote side info */
    set_remote_info(endpoint, rem_info);
    
    /* Connect to remote endpoint qp's */
    if (OMPI_SUCCESS != (rc = qp_connect_all(endpoint))) {
        return rc;
    }

    /* Send connection info over to remote endpoint */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECT_ACK;
    if (OMPI_SUCCESS !=
        (rc = send_connect_data(endpoint, ENDPOINT_CONNECT_RESPONSE))) {
        BTL_ERROR(("error in endpoint send connect request error code is %d",
                   rc));
        return rc;
    }
    return OMPI_SUCCESS;
}


static int set_remote_info(mca_btl_base_endpoint_t* endpoint,
                           mca_btl_openib_rem_info_t* rem_info)
{
    /* copy the rem_info stuff */
    memcpy(&((mca_btl_openib_endpoint_t*) endpoint)->rem_info, 
           rem_info, sizeof(mca_btl_openib_rem_info_t)); 
    
    /* copy over the rem qp info */
    memcpy(endpoint->rem_info.rem_qps,
           rem_info->rem_qps, sizeof(mca_btl_openib_rem_qp_info_t) * 
           mca_btl_openib_component.num_qps);
    
    BTL_VERBOSE(("Setting QP info,  LID = %d", endpoint->rem_info.rem_lid));
    return ORTE_SUCCESS;

}


/*
 * Connect the local ends of all qp's to the remote side
 */
static int qp_connect_all(mca_btl_openib_endpoint_t *endpoint)
{
    int i, rc;
    mca_btl_openib_module_t* openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;

    for (i = 0; i < mca_btl_openib_component.num_qps; i++) {
        struct ibv_qp_attr attr;
        struct ibv_qp* qp = endpoint->qps[i].qp->lcl_qp;
        enum ibv_mtu mtu = (enum ibv_mtu) ((openib_btl->device->mtu < endpoint->rem_info.rem_mtu) ?
            openib_btl->device->mtu : endpoint->rem_info.rem_mtu) ;

        memset(&attr, 0, sizeof(attr));
        attr.qp_state           = IBV_QPS_RTR;
        attr.path_mtu           = mtu;
        attr.dest_qp_num        = endpoint->rem_info.rem_qps[i].rem_qp_num;
        attr.rq_psn             = endpoint->rem_info.rem_qps[i].rem_psn;
        attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
        attr.min_rnr_timer  = mca_btl_openib_component.ib_min_rnr_timer;
        attr.ah_attr.is_global     = 0;
        attr.ah_attr.dlid          = endpoint->rem_info.rem_lid;
        attr.ah_attr.src_path_bits = openib_btl->src_path_bits;
        attr.ah_attr.port_num      = openib_btl->port_num;
        attr.ah_attr.sl = mca_btl_openib_component.ib_service_level;
        /* if user enable ib_path_record_service_level - dynamically get the sl from PathRecord */
        if (0 != mca_btl_openib_component.ib_path_record_service_level) {
            rc = get_pathrecord_sl(qp->context,
                                   attr.ah_attr.port_num,
                                   openib_btl->lid,
                                   attr.ah_attr.dlid);
            if (rc < 0) {
                free_sa_qp_cache();
                return rc;
            }
            attr.ah_attr.sl = rc;
        }
        /* JMS to be filled in later dynamically */
        attr.ah_attr.static_rate   = 0;

        if (mca_btl_openib_component.verbose) {
            BTL_OUTPUT(("Set MTU to IBV value %d (%s bytes)", mtu,
                        (mtu == IBV_MTU_256) ? "256" :
                        (mtu == IBV_MTU_512) ? "512" :
                        (mtu == IBV_MTU_1024) ? "1024" :
                        (mtu == IBV_MTU_2048) ? "2048" :
                        (mtu == IBV_MTU_4096) ? "4096" :
                        "unknown (!)"));
        }

        if (ibv_modify_qp(qp, &attr,
                          IBV_QP_STATE              |
                          IBV_QP_AV                 |
                          IBV_QP_PATH_MTU           |
                          IBV_QP_DEST_QPN           |
                          IBV_QP_RQ_PSN             |
                          IBV_QP_MAX_DEST_RD_ATOMIC |
                          IBV_QP_MIN_RNR_TIMER)) {
            BTL_ERROR(("error modifing QP to RTR errno says %s",
                       strerror(errno)));
            return OMPI_ERROR; 
        }
        attr.qp_state       = IBV_QPS_RTS;
        attr.timeout        = mca_btl_openib_component.ib_timeout;
        attr.retry_cnt      = mca_btl_openib_component.ib_retry_count;
        /* On PP QPs we have SW flow control, no need for rnr retries. Setting
         * it to zero helps to catch bugs */
        attr.rnr_retry      = BTL_OPENIB_QP_TYPE_PP(i) ? 0 :
            mca_btl_openib_component.ib_rnr_retry;
        attr.sq_psn         = endpoint->qps[i].qp->lcl_psn;
        attr.max_rd_atomic  = mca_btl_openib_component.ib_max_rdma_dst_ops;
        if (ibv_modify_qp(qp, &attr,
                          IBV_QP_STATE              |
                          IBV_QP_TIMEOUT            |
                          IBV_QP_RETRY_CNT          |
                          IBV_QP_RNR_RETRY          |
                          IBV_QP_SQ_PSN             |
                          IBV_QP_MAX_QP_RD_ATOMIC)) {
            BTL_ERROR(("error modifying QP to RTS errno says %s",
                       strerror(errno)));
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Create the local side of all the qp's.  The remote sides will be
 * connected later.
 */
static int qp_create_all(mca_btl_base_endpoint_t* endpoint)
{
    int qp, rc, pp_qp_num = 0;
    int32_t rd_rsv_total = 0;

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp)
        if(BTL_OPENIB_QP_TYPE_PP(qp)) {
            rd_rsv_total +=
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
            pp_qp_num++;
        }

    /* if there is no pp QPs we still need reserved WQE for eager rdma flow
     * control */
    if(0 == pp_qp_num && true == endpoint->use_eager_rdma)
        pp_qp_num = 1;

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) { 
        struct ibv_srq *srq = NULL;
        uint32_t max_recv_wr, max_send_wr;
        int32_t rd_rsv, rd_num_credits;

        /* QP used for SW flow control need some additional recourses */
        if(qp == mca_btl_openib_component.credits_qp) {
            rd_rsv = rd_rsv_total;
            rd_num_credits = pp_qp_num;
        } else {
            rd_rsv = rd_num_credits = 0;
        }

        if(BTL_OPENIB_QP_TYPE_PP(qp)) {
            max_recv_wr = mca_btl_openib_component.qp_infos[qp].rd_num + rd_rsv;
            max_send_wr = mca_btl_openib_component.qp_infos[qp].rd_num +
                rd_num_credits;
        } else {
            srq = endpoint->endpoint_btl->qps[qp].u.srq_qp.srq;
            /* no receives are posted to SRQ qp */
            max_recv_wr = 0;
            max_send_wr = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max
                + rd_num_credits;
        }

        rc = qp_create_one(endpoint, qp, srq, max_recv_wr, max_send_wr);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    /* Now that all the qp's are created locally, post some receive
       buffers, setup credits, etc. */
    return mca_btl_openib_endpoint_post_recvs(endpoint);
}


/* Returns max inlne size for qp #N */
static uint32_t max_inline_size(int qp, mca_btl_openib_device_t *device)
{
    if (mca_btl_openib_component.qp_infos[qp].size <= device->max_inline_data) {
        /* If qp message size is smaller than max_inline_data,
         * we should enable inline messages */
        return mca_btl_openib_component.qp_infos[qp].size;
    } else if (mca_btl_openib_component.rdma_qp == qp || 0 == qp) {
        /* If qp message size is bigger that max_inline_data, we
         * should enable inline messages only for RDMA QP (for PUT/GET
         * fin messages) and for the first qp */
        return device->max_inline_data;
    }
    /* Otherway it is no reason for inline */
    return 0;
}

/*
 * Create the local side of one qp.  The remote side will be connected
 * later.
 */
static int qp_create_one(mca_btl_base_endpoint_t* endpoint, int qp, 
        struct ibv_srq *srq, uint32_t max_recv_wr, uint32_t max_send_wr)
{
    mca_btl_openib_module_t *openib_btl = endpoint->endpoint_btl;
    struct ibv_qp *my_qp;
    struct ibv_qp_init_attr init_attr;
    struct ibv_qp_attr attr;
    size_t req_inline;

    memset(&init_attr, 0, sizeof(init_attr));
    memset(&attr, 0, sizeof(attr));

    init_attr.qp_type = IBV_QPT_RC;
    init_attr.send_cq = openib_btl->device->ib_cq[BTL_OPENIB_LP_CQ];
    init_attr.recv_cq = openib_btl->device->ib_cq[qp_cq_prio(qp)];
    init_attr.srq     = srq;
    init_attr.cap.max_inline_data = req_inline = 
        max_inline_size(qp, openib_btl->device);
    init_attr.cap.max_send_sge = 1;
    init_attr.cap.max_recv_sge = 1; /* we do not use SG list */
    if(BTL_OPENIB_QP_TYPE_PP(qp)) {
        init_attr.cap.max_recv_wr  = max_recv_wr;
    } else {
        init_attr.cap.max_recv_wr  = 0;
    }
    init_attr.cap.max_send_wr  = max_send_wr;

    my_qp = ibv_create_qp(openib_btl->device->ib_pd, &init_attr); 
    
    if (NULL == my_qp) { 
        BTL_ERROR(("error creating qp errno says %s", strerror(errno))); 
        return OMPI_ERROR; 
    }
    endpoint->qps[qp].qp->lcl_qp = my_qp;

    if (init_attr.cap.max_inline_data < req_inline) {
        endpoint->qps[qp].ib_inline_max = init_attr.cap.max_inline_data;
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", true, orte_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev),
                       openib_btl->port_num,
                       req_inline, init_attr.cap.max_inline_data);
    } else {
        endpoint->qps[qp].ib_inline_max = req_inline;
    }
    
    attr.qp_state        = IBV_QPS_INIT;
    attr.pkey_index      = openib_btl->pkey_index;
    attr.port_num        = openib_btl->port_num;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;

    if (ibv_modify_qp(endpoint->qps[qp].qp->lcl_qp, 
                      &attr, 
                      IBV_QP_STATE | 
                      IBV_QP_PKEY_INDEX | 
                      IBV_QP_PORT | 
                      IBV_QP_ACCESS_FLAGS )) { 
        BTL_ERROR(("error modifying qp to INIT errno says %s", strerror(errno))); 
        return OMPI_ERROR; 
    } 

    /* Setup meta data on the endpoint */
    endpoint->qps[qp].qp->lcl_psn = lrand48() & 0xffffff;
    endpoint->qps[qp].credit_frag = NULL;

    return OMPI_SUCCESS;
}


/*
 * RML send connect information to remote endpoint
 */
static int send_connect_data(mca_btl_base_endpoint_t* endpoint, 
                             uint8_t message_type)
{
    opal_buffer_t* buffer = OBJ_NEW(opal_buffer_t);
    int rc;
    
    if (NULL == buffer) {
         ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
         return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */ 
    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT8));
    rc = opal_dss.pack(buffer, &message_type, 1, OPAL_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT64));
    rc = opal_dss.pack(buffer, &endpoint->subnet_id, 1, OPAL_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (message_type != ENDPOINT_CONNECT_REQUEST) {
        /* send the QP connect request info we respond to */
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer,
                           &endpoint->rem_info.rem_qps[0].rem_qp_num, 1,
                           OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &endpoint->rem_info.rem_lid, 1, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    if (message_type != ENDPOINT_CONNECT_ACK) {
        int qp;
        /* stuff all the QP info into the buffer */
        for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) { 
            BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &endpoint->qps[qp].qp->lcl_qp->qp_num,
                               1, OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &endpoint->qps[qp].qp->lcl_psn, 1,
                               OPAL_UINT32); 
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->lid, 1, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->device->mtu, 1,
                OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->index, 1, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* send to remote endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_ompi->proc_name, 
                                 buffer, OMPI_RML_TAG_OPENIB, 0,
                                 rml_send_cb, NULL);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    BTL_VERBOSE(("Sent QP Info, LID = %d, SUBNET = %" PRIx64 "\n",
                 endpoint->endpoint_btl->lid, 
                 endpoint->subnet_id));

    return OMPI_SUCCESS;
}


/*
 * Callback when we have finished RML sending the connect data to a
 * remote peer
 */
static void rml_send_cb(int status, orte_process_name_t* endpoint, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata)
{
    OBJ_RELEASE(buffer);
}


/*
 * Non blocking RML recv callback.  Read incoming QP and other info,
 * and if this endpoint is trying to connect, reply with our QP info,
 * otherwise try to modify QP's and establish reliable connection
 */
static void rml_recv_cb(int status, orte_process_name_t* process_name, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata)
{
    mca_btl_openib_proc_t *ib_proc;
    mca_btl_openib_endpoint_t *ib_endpoint = NULL;
    int endpoint_state;
    int rc;
    uint32_t i, lcl_qp = 0;
    uint16_t lcl_lid = 0;
    int32_t cnt = 1;
    mca_btl_openib_rem_info_t rem_info;
    uint8_t message_type;
    bool master;
    
    /* start by unpacking data first so we know who is knocking at 
       our door */ 
    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
    rc = opal_dss.unpack(buffer, &message_type, &cnt, OPAL_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        mca_btl_openib_endpoint_invoke_error(NULL);
        return;
    }
    
    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT64));
    rc = opal_dss.unpack(buffer, &rem_info.rem_subnet_id, &cnt, OPAL_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        mca_btl_openib_endpoint_invoke_error(NULL);
        return;
    }
    
    if (ENDPOINT_CONNECT_REQUEST != message_type) {
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &lcl_qp, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, &lcl_lid, &cnt, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
    }
    if (ENDPOINT_CONNECT_ACK != message_type) {
        int qp; 
        /* get ready for the data */
        rem_info.rem_qps = 
            (mca_btl_openib_rem_qp_info_t*) malloc(sizeof(mca_btl_openib_rem_qp_info_t) * 
                                                   mca_btl_openib_component.num_qps);
        
        /* unpack all the qp info */
        for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) { 
            BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &rem_info.rem_qps[qp].rem_qp_num, &cnt,
                                 OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
            BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &rem_info.rem_qps[qp].rem_psn, &cnt,
                                 OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                mca_btl_openib_endpoint_invoke_error(NULL);
                return;
            }
        }
        
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, &rem_info.rem_lid, &cnt, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &rem_info.rem_mtu, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &rem_info.rem_index, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_openib_endpoint_invoke_error(NULL);
            return;
        }
    }
    
    BTL_VERBOSE(("Received QP Info,  LID = %d, SUBNET = %" PRIx64 "\n",
                 rem_info.rem_lid, 
                 rem_info.rem_subnet_id));
    
    master = orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_NAME,
                                    process_name) > 0 ? true : false;
    
    /* Need to protect the ib_procs list */
    OPAL_THREAD_LOCK(&mca_btl_openib_component.ib_lock);

    for (ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
        ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
        ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {
        bool found = false;
        
        if (orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                   &ib_proc->proc_ompi->proc_name, process_name) != OPAL_EQUAL) {
            continue;
        }
        
        if (ENDPOINT_CONNECT_REQUEST != message_type) {
            /* This is a reply message. Try to get the endpoint
               instance the reply belongs to */
            for (i = 0; i < ib_proc->proc_endpoint_count; i++) { 
                ib_endpoint = ib_proc->proc_endpoints[i];
                if (ib_endpoint->qps[0].qp->lcl_qp != NULL &&
                    lcl_lid == ib_endpoint->endpoint_btl->lid &&
                    lcl_qp == ib_endpoint->qps[0].qp->lcl_qp->qp_num &&
                    rem_info.rem_subnet_id == ib_endpoint->subnet_id) {
                    found = true;
                    break;
                }
            }
        } else {
            /* This is new connection request. If this is master try
               to find endpoint in a connecting state. If this is
               slave try to find  endpoint in closed state and
               initiate connection back */
            mca_btl_openib_endpoint_t *ib_endpoint_found = NULL;
            int master_first_closed = -1;

            for (i = 0; i < ib_proc->proc_endpoint_count; i++) { 
                ib_endpoint = ib_proc->proc_endpoints[i];
                if (ib_endpoint->subnet_id != rem_info.rem_subnet_id ||
                   (ib_endpoint->endpoint_state != MCA_BTL_IB_CONNECTING
                    && ib_endpoint->endpoint_state != MCA_BTL_IB_CLOSED))
                    continue;
                found = true;
                ib_endpoint_found = ib_endpoint;

                if (master && -1 == master_first_closed &&
                    MCA_BTL_IB_CLOSED == ib_endpoint->endpoint_state ) {
                    /* capture in case no endpoint in connecting state */
                    master_first_closed = i;
                }

                if ((master &&
                     MCA_BTL_IB_CONNECTING == ib_endpoint->endpoint_state) ||
                    (!master &&
                     MCA_BTL_IB_CLOSED == ib_endpoint->endpoint_state))
                    break; /* Found one. No point to continue */
            }
            ib_endpoint = ib_endpoint_found;
            
            if (found && master &&
                MCA_BTL_IB_CLOSED == ib_endpoint->endpoint_state ) {
                /* since this is master and no endpoints found in
                 * connecting state use the first endpoint found
                 * in closed state */
                ib_endpoint = ib_proc->proc_endpoints[master_first_closed];
            }

            /* if this is slave and there is no endpoints in closed
               state then all connection are already in progress so
               just ignore this connection request */
            if (found && !master &&
                MCA_BTL_IB_CLOSED != ib_endpoint->endpoint_state) {
		OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
                return;
            }
        }
        
        if (!found) {
            BTL_ERROR(("can't find suitable endpoint for this peer\n")); 
            mca_btl_openib_endpoint_invoke_error(NULL);
	    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
            return; 
        }
        
        OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
        endpoint_state = ib_endpoint->endpoint_state;
        
        /* Update status */
        switch (endpoint_state) {
        case MCA_BTL_IB_CLOSED :
            /* We had this connection closed before.  The endpoint is
               trying to connect. Move the status of this connection
               to CONNECTING, and then reply with our QP
               information */
            if (master) {
                rc = reply_start_connect(ib_endpoint, &rem_info);
            } else {
                rc = oob_module_start_connect(ib_endpoint->endpoint_local_cpc, 
                                              ib_endpoint);
            }
            
            if (OMPI_SUCCESS != rc) {
                BTL_ERROR(("error in endpoint reply start connect"));
                mca_btl_openib_endpoint_invoke_error(ib_endpoint);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                break;
            }
            
            /* As long as we expect a message from the peer (in order
               to setup the connection) let the event engine pool the
               RML events. Note: we increment it once peer active
               connection. */
            opal_progress_event_users_increment();
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;
             
        case MCA_BTL_IB_CONNECTING :
            set_remote_info(ib_endpoint, &rem_info);
            if (OMPI_SUCCESS != (rc = qp_connect_all(ib_endpoint))) {
                BTL_ERROR(("endpoint connect error: %d", rc)); 
                mca_btl_openib_endpoint_invoke_error(ib_endpoint);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
                break;
            }
           
            if (master) {
                ib_endpoint->endpoint_state = MCA_BTL_IB_WAITING_ACK;

                /* Send him an ACK */
                send_connect_data(ib_endpoint, ENDPOINT_CONNECT_RESPONSE);
                OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            } else {
                send_connect_data(ib_endpoint, ENDPOINT_CONNECT_ACK);
                /* Tell main BTL that we're done */
                mca_btl_openib_endpoint_cpc_complete(ib_endpoint);
                /* cpc complete unlock the endpoint */
             }
            break;
            
        case MCA_BTL_IB_WAITING_ACK:
            /* Tell main BTL that we're done */
            mca_btl_openib_endpoint_cpc_complete(ib_endpoint);
            /* cpc complete unlock the endpoint */
            break;
            
        case MCA_BTL_IB_CONNECT_ACK:
            send_connect_data(ib_endpoint, ENDPOINT_CONNECT_ACK);
            /* Tell main BTL that we're done */
            mca_btl_openib_endpoint_cpc_complete(ib_endpoint);
            /* cpc complete unlock the endpoint */
            break;
            
        case MCA_BTL_IB_CONNECTED:
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;

        case MCA_BTL_IB_FAILED:
            /* This connection has been put in the failed state
             * so just ignore the connection message. */
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;

        default :
            BTL_ERROR(("Invalid endpoint state %d", endpoint_state));
            mca_btl_openib_endpoint_invoke_error(ib_endpoint);
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
        }
        break;
    }
    OPAL_THREAD_UNLOCK(&mca_btl_openib_component.ib_lock);
}

static int init_ud_qp(struct ibv_context *context_arg,
                      struct mca_btl_openib_sa_qp_cache *cache)
{
    struct ibv_qp_init_attr iattr;
    struct ibv_qp_attr mattr;
    int rc;

    /* create cq */
    cache->cq = ibv_create_cq(cache->context, 4, NULL, NULL, 0);
    if (NULL == cache->cq) {
        orte_show_help("help-mpi-btl-openib.txt", "init-fail-create-q",
                true, orte_process_info.nodename,
                __FILE__, __LINE__, "ibv_create_cq",
                strerror(errno), errno,
                ibv_get_device_name(context_arg->device));
        return OMPI_ERROR;
    }

    /* create qp */
    memset(&iattr, 0, sizeof(iattr));
    iattr.send_cq = cache->cq;
    iattr.recv_cq = cache->cq;
    iattr.cap.max_send_wr = 2;
    iattr.cap.max_recv_wr = 2;
    iattr.cap.max_send_sge = 1;
    iattr.cap.max_recv_sge = 1;
    iattr.qp_type = IBV_QPT_UD;
    cache->qp = ibv_create_qp(cache->pd, &iattr);
    if (NULL == cache->qp) {
        BTL_ERROR(("error creating qp %s (%d)", strerror(errno), errno));
        return OMPI_ERROR;
    }

    /* modify qp to IBV_QPS_INIT */
    memset(&mattr, 0, sizeof(mattr));
    mattr.qp_state = IBV_QPS_INIT;
    mattr.port_num = cache->port_num;
    mattr.qkey = IB_GLOBAL_QKEY;
    rc = ibv_modify_qp(cache->qp, &mattr,
            IBV_QP_STATE              |
            IBV_QP_PKEY_INDEX         |
            IBV_QP_PORT               |
            IBV_QP_QKEY);
    if (rc) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_INIT errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    /* modify qp to IBV_QPS_RTR */
    memset(&mattr, 0, sizeof(mattr));
    mattr.qp_state = IBV_QPS_RTR;
    rc = ibv_modify_qp(cache->qp, &mattr, IBV_QP_STATE);
    if (rc) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_RTR errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    /* modify qp to IBV_QPS_RTS */
    mattr.qp_state = IBV_QPS_RTS;
    rc = ibv_modify_qp(cache->qp, &mattr, IBV_QP_STATE | IBV_QP_SQ_PSN);
    if (rc) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_RTR errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
static void init_sa_mad(struct mca_btl_openib_sa_qp_cache *cache,
                        ib_sa_mad_t *sag,
                        struct ibv_send_wr *swr,
                        struct ibv_sge *ssge,
                        uint16_t lid,
                        uint16_t rem_lid)
{
    ib_path_rec_t *path_record;

    path_record = (ib_path_rec_t*)sag->data;

    memset(sag, 0, sizeof(*sag));
    memset(swr, 0, sizeof(*swr));
    memset(ssge, 0, sizeof(*ssge));

    sag->base_ver   = IB_MGMT_BASE_VERSION;
    sag->mgmt_class = IB_SA_CLASS;
    sag->class_ver  = 2;
    sag->method     = IB_MAD_METHOD_GET;
    sag->attr_id    = htons(IB_SA_ATTR_PATHRECORD);
    sag->trans_id   =
        ((IB_SA_TID_GET_PATH_REC_1 + rem_lid) |
         ((IB_SA_TID_GET_PATH_REC_0 + cache->qp->qp_num)<<32));
    sag->comp_mask =
        IB_PR_COMPMASK_DLID | IB_PR_COMPMASK_SLID;
    path_record->dlid = htons(rem_lid);
    path_record->slid = htons(lid);

    swr->sg_list = ssge;
    swr->num_sge = 1;
    swr->opcode = IBV_WR_SEND;
    swr->wr.ud.ah = cache->ah;
    swr->wr.ud.remote_qpn = IB_SA_QPN;
    swr->wr.ud.remote_qkey = IB_GLOBAL_QKEY;
    swr->send_flags = IBV_SEND_SIGNALED | IBV_SEND_SOLICITED;

    ssge->addr = (uint64_t)(void *)sag;
    ssge->length = sizeof(*sag);
    ssge->lkey = cache->mr->lkey;
}

static int get_pathrecord_info(struct mca_btl_openib_sa_qp_cache *cache,
                               ib_sa_mad_t *sag,
                               ib_sa_mad_t *sar,
                               struct ibv_send_wr *swr,
                               uint16_t lid,
                               uint16_t rem_lid)
{
    struct ibv_send_wr *bswr;
    struct ibv_wc wc;
    struct timeval get_sl_rec_last_sent, get_sl_rec_last_poll;
    struct ibv_recv_wr *brwr;
    int got_sl_value, get_sl_rec_retries, rc, ne, i;
    ib_path_rec_t *path_record = (ib_path_rec_t*)sar->data;

    got_sl_value = 0;
    get_sl_rec_retries = 0;

    rc = ibv_post_recv(cache->qp, &(cache->rwr), &brwr);
    if (0 != rc) {
        BTL_ERROR(("error posing receive on QP[%x] errno says: %s [%d]",
                   cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    while (0 == got_sl_value) {
        rc = ibv_post_send(cache->qp, swr, &bswr);
        if (0 != rc) {
            BTL_ERROR(("error posting send on QP[%x] errno says: %s [%d]",
                       cache->qp->qp_num, strerror(errno), errno));
            return OMPI_ERROR;
        }
        gettimeofday(&get_sl_rec_last_sent, NULL);

        while (0 == got_sl_value) {
            ne = ibv_poll_cq(cache->cq, 1, &wc);
            if (ne > 0
                    && IBV_WC_SUCCESS == wc.status
                    && IBV_WC_RECV == wc.opcode
                    && wc.byte_len >= sizeof(*sar)
                    && sar->trans_id == sag->trans_id) {
                if (0 == sar->status
                        && path_record->slid == htons(lid)
                        && path_record->dlid == htons(rem_lid)) {
                    /* Everything matches, so we have the desired SL */
                    cache->sl_values[rem_lid] =
                        ib_path_rec_sl(path_record);
                    got_sl_value = 1; /* still must repost recieve buf */
                } else {
                    /* Probably bad status, unlikely bad lid match. We will */
                    /* ignore response and let it time out so that we do a  */
                    /* retry, but after a delay. We must make a new TID so  */
                    /* the SM doesn't see it as the same request.           */
                    sag->trans_id += 0x10000;
                }
                rc = ibv_post_recv(cache->qp, &(cache->rwr), &brwr);
                if (0 != rc) {
                    BTL_ERROR(("error posing receive on QP[%x] errno says: %s [%d]",
                               cache->qp->qp_num, strerror(errno), errno));
                    return OMPI_ERROR;
                }
            } else if (0 == ne) {    /* poll did not find anything */
                gettimeofday(&get_sl_rec_last_poll, NULL);
                i = get_sl_rec_last_poll.tv_sec - get_sl_rec_last_sent.tv_sec;
                i = (i * 1000000) +
                    get_sl_rec_last_poll.tv_usec - get_sl_rec_last_sent.tv_usec;
                if (i > GET_SL_REC_RETRIES_TIMEOUT_MS) {
                    get_sl_rec_retries++;
                    BTL_VERBOSE(("[%d/%d] retries to get PathRecord",
                            get_sl_rec_retries, MAX_GET_SL_REC_RETRIES));
                    if (get_sl_rec_retries > MAX_GET_SL_REC_RETRIES) {
                        BTL_ERROR(("No response from SA after %d retries",
                                MAX_GET_SL_REC_RETRIES));
                        return OMPI_ERROR;
                    }
                    break;  /* retransmit request */
                }
                usleep(100);  /* otherwise pause before polling again */
            } else if (ne < 0) {
                BTL_ERROR(("error polling CQ with %d: %s\n",
                    ne, strerror(errno)));
                return OMPI_ERROR;
            }
        }
    }
    return 0;
}

static int init_device(struct ibv_context *context_arg,
                       struct mca_btl_openib_sa_qp_cache *cache,
                       uint32_t port_num)
{
    struct ibv_ah_attr aattr;
    struct ibv_port_attr pattr;
    int rc;

    cache->context = ibv_open_device(context_arg->device);
    if (NULL == cache->context) {
        BTL_ERROR(("error obtaining device context for %s errno says %s",
                    ibv_get_device_name(context_arg->device), strerror(errno)));
        return OMPI_ERROR;
    }
    cache->device_name = strdup(ibv_get_device_name(cache->context->device));
    cache->port_num = port_num;

    /* init all sl_values to be SL_NOT_PRESENT */
    memset(&cache->sl_values, SL_NOT_PRESENT, sizeof(cache->sl_values));

    cache->next = sa_qp_cache;
    sa_qp_cache = cache;

    /* allocate the protection domain for the device */
    cache->pd = ibv_alloc_pd(cache->context);
    if (NULL == cache->pd) {
        BTL_ERROR(("error allocating protection domain for %s errno says %s",
                    ibv_get_device_name(context_arg->device), strerror(errno)));
        return OMPI_ERROR;
    }

    /* register memory region */
    cache->mr = ibv_reg_mr(cache->pd, cache->send_recv_buffer,
            sizeof(cache->send_recv_buffer),
            IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_LOCAL_WRITE);
    if (NULL == cache->mr) {
        BTL_ERROR(("error registering memory region, errno says %s", strerror(errno)));
        return OMPI_ERROR;
    }

    /* init the ud qp */
    rc = init_ud_qp(context_arg, cache);
    if (OMPI_ERROR == rc) {
        return OMPI_ERROR;
    }

    rc = ibv_query_port(cache->context, cache->port_num, &pattr);
    if (rc) {
        BTL_ERROR(("error getting port attributes for device %s "
                    "port number %d errno says %s",
                    ibv_get_device_name(context_arg->device),
                    cache->port_num, strerror(errno)));
        return OMPI_ERROR;
    }

    /* create address handle  */
    memset(&aattr, 0, sizeof(aattr));
    aattr.dlid = pattr.sm_lid;
    aattr.sl = pattr.sm_sl;
    aattr.port_num = cache->port_num;
    cache->ah = ibv_create_ah(cache->pd, &aattr);
    if (NULL == cache->ah) {
        BTL_ERROR(("error creating address handle: %s", strerror(errno)));
        return OMPI_ERROR;
    }

    memset(&(cache->rwr), 0, sizeof(cache->rwr));
    cache->rwr.num_sge = 1;
    cache->rwr.sg_list = &(cache->rsge);
    memset(&(cache->rsge), 0, sizeof(cache->rsge));
    cache->rsge.addr = (uint64_t)(void *)
        (cache->send_recv_buffer + sizeof(ib_sa_mad_t));
    cache->rsge.length = sizeof(ib_sa_mad_t) + 40;
    cache->rsge.lkey = cache->mr->lkey;

    return 0;
}

static int get_pathrecord_sl(struct ibv_context *context_arg,
                             uint32_t port_num,
                             uint16_t lid,
                             uint16_t rem_lid)
{
    struct ibv_send_wr swr;
    ib_sa_mad_t *sag, *sar;
    struct ibv_sge ssge;
    struct mca_btl_openib_sa_qp_cache *cache;
    long page_size = sysconf(_SC_PAGESIZE);
    int rc;

    /* search for a cached item */
    for (cache = sa_qp_cache; cache; cache = cache->next) {
        if (0 == strcmp(cache->device_name,
                    ibv_get_device_name(context_arg->device))
                && cache->port_num == port_num) {
            break;
        }
    }

    if (NULL == cache) {
        /* init new cache */
        if (posix_memalign((void **)(&cache), page_size,
                    sizeof(struct mca_btl_openib_sa_qp_cache))) {
            BTL_ERROR(("error in posix_memalign SA cache"));
            return OMPI_ERROR;
        }
        /* one time setup for each device/port combination */
        rc = init_device(context_arg, cache, port_num);
        if (0 != rc) {
            return rc;
        }
    }

    /* if the destination lid SL value is not in the cache, go get it */
    if (SL_NOT_PRESENT == cache->sl_values[rem_lid]) {
        /* sag is first buffer, where we build the SA Get request to send */
        sag = (ib_sa_mad_t *)(cache->send_recv_buffer);

        init_sa_mad(cache, sag, &swr, &ssge, lid, rem_lid);

        /* sar is the receive buffer (40 byte GRH) */
        sar = (ib_sa_mad_t *)(cache->send_recv_buffer + sizeof(ib_sa_mad_t) + 40);

        rc = get_pathrecord_info(cache, sag, sar, &swr, lid, rem_lid);
        if (0 != rc) {
            return rc;
        }
    }

    /* now all we do is send back the value laying around */
    return cache->sl_values[rem_lid];
}
