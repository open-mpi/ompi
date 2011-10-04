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
#include "btl_wv.h"
#include "btl_wv_endpoint.h" 
#include "btl_wv_proc.h"
#include "connect/connect.h"
#include "orte/util/show_help.h"
#include <rdma/winverbs.h>
#include <malloc.h>

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
#define IB_MGMT_CLASS_SUBN_ADM        0x03
#define IB_MGMT_METHOD_GET            0x01
#define IB_SA_TID_GET_PATH_REC_0      0xCA000000UL
#define IB_SA_TID_GET_PATH_REC_1      0xBEEF0000UL
#define IB_PATH_REC_SL_MASK           0x000F
#define IB_SA_ATTR_PATH_REC           0x35
#define IB_SA_PATH_REC_DLID           (1<<4)
#define IB_SA_PATH_REC_SLID           (1<<5)

struct ib_mad_hdr {
    uint8_t   base_version;
    uint8_t   mgmt_class;
    uint8_t   class_version;
    uint8_t   method;
    uint16_t  status;
    uint16_t  class_spec;
    uint32_t  tid[2];
    uint16_t  attr_id;
    uint16_t  resv;
    uint32_t  attr_mod;
};

struct ib_rmpp_hdr {
    uint32_t  raw[3];
};

struct ib_sa_hdr {
    uint32_t sm_key[2];
    uint16_t reserved;
    uint16_t attrib_offset;
    uint32_t comp_mask[2];
};

typedef union _ib_gid {
    uint8_t raw[16];
    struct _ib_gid_unicast {
        uint64_t prefix;
        uint64_t interface_id;
    } unicast;
    struct _ib_gid_multicast {
        uint8_t header[2];
        uint8_t raw_group_id[14];
    } multicast;
} ib_gid_t;

struct ib_path_record {
    uint64_t service_id;
    ib_gid_t dgit;
    ib_gid_t sgit;
    uint16_t dlid;
    uint16_t slid;
    uint32_t hop_flow_raw;
    uint8_t  tclass;
    uint8_t  num_path;
    uint16_t pkey;
    uint8_t  reserved1;
    uint8_t  qos_class_sl;
    uint8_t  mtu;
    uint8_t  rate;
    uint32_t preference__packet_lifetime__packet_lifetime_selector;
    uint32_t reserved2[35];
};

union ib_sa_data {
    struct ib_path_record path_record;
};

struct ib_mad_sa {
    struct ib_mad_hdr mad_hdr;
    struct ib_rmpp_hdr rmpp_hdr;
    struct ib_sa_hdr sa_hdr;
    union  ib_sa_data sa_data;
};

static struct mca_btl_wv_sa_qp_cache {
    /* There will be a MR with the one send and receive buffer together */
    /* The send buffer is first, the receive buffer is second */
    /* The receive buffer in a UD queue pair needs room for the 40 byte GRH */
    /* The buffers are first in the structure for page alignment */
    char     send_recv_buffer[sizeof(struct ib_mad_sa) * 2 + 40];
    struct   mca_btl_wv_sa_qp_cache *next;
    struct   wv_context *context;
    char     *device_name;
    uint32_t port_num;
    struct   wv_qp *qp;
    struct   wv_ah *ah;
    struct   wv_cq *cq;
    struct   wv_mr *mr;
    struct   wv_pd *pd;
    struct   wv_recv_wr rwr;
    WV_SGE   rsge;
    char     sl_values[65536];
} *sa_qp_cache = 0;

static int oob_priority = 50;
static bool rml_recv_posted = false;

static void oob_component_register(void);
static int oob_component_query(mca_btl_wv_module_t *wv_btl, 
                               ompi_btl_wv_connect_base_module_t **cpc);
static int oob_component_finalize(void);

static int oob_module_start_connect(ompi_btl_wv_connect_base_module_t *cpc,
                                    mca_btl_base_endpoint_t *endpoint);
static int reply_start_connect(mca_btl_wv_endpoint_t *endpoint,
                               mca_btl_wv_rem_info_t *rem_info);
static int set_remote_info(mca_btl_base_endpoint_t* endpoint,
                           mca_btl_wv_rem_info_t* rem_info);
static int qp_connect_all(mca_btl_base_endpoint_t* endpoint);
static int qp_create_all(mca_btl_base_endpoint_t* endpoint);
static int qp_create_one(mca_btl_base_endpoint_t* endpoint, int qp,
        struct wv_srq *srq, uint32_t max_recv_wr, uint32_t max_send_wr);
static int send_connect_data(mca_btl_base_endpoint_t* endpoint, 
                             uint8_t message_type);

static void rml_send_cb(int status, orte_process_name_t* endpoint, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata);
static void rml_recv_cb(int status, orte_process_name_t* process_name, 
                        opal_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata);
static int init_ud_qp(struct wv_context *context_arg,
                      struct mca_btl_wv_sa_qp_cache *cache);
static void init_sa_mad(struct mca_btl_wv_sa_qp_cache *cache,
                        struct ib_mad_sa *sag,
                        WV_SEND_REQUEST  *swr,
                        WV_SGE *ssge,
                        uint16_t lid,
                        uint16_t rem_lid);
static int get_pathrecord_info(struct mca_btl_wv_sa_qp_cache *cache,
                               struct ib_mad_sa *sag,
                               struct ib_mad_sa *sar,
                               WV_SEND_REQUEST  *swr,
                               uint16_t lid,
                               uint16_t rem_lid);
static int init_device(struct wv_context *context_arg,
                       struct mca_btl_wv_sa_qp_cache *cache,
                       uint32_t port_num);
static int get_pathrecord_sl(struct wv_context *context_arg,
                             uint32_t port_num,
                             uint16_t lid,
                             uint16_t rem_lid);


/*
 * The "component" struct -- the top-level function pointers for the
 * oob connection scheme.
 */
ompi_btl_wv_connect_base_component_t ompi_btl_wv_connect_oob = {
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
    mca_base_param_reg_int(&mca_btl_wv_component.super.btl_version,
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
static int oob_component_query(mca_btl_wv_module_t *btl, 
                               ompi_btl_wv_connect_base_module_t **cpc)
{
    int rc;
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
                                "wv BTL: oob CPC system error %d (%s)",
                                rc, opal_strerror(rc));
            return rc;
        }
        rml_recv_posted = true;
    }

    *cpc = (ompi_btl_wv_connect_base_module_t *) malloc(sizeof(ompi_btl_wv_connect_base_module_t));
    if (NULL == *cpc) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, OMPI_RML_TAG_OPENIB);
        rml_recv_posted = false;
        opal_output_verbose(5, mca_btl_base_output,
                            "wv BTL: oob CPC system error (malloc failed)");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    (*cpc)->data.cbm_component = &ompi_btl_wv_connect_oob;
    (*cpc)->data.cbm_priority = oob_priority;
    (*cpc)->data.cbm_modex_message = NULL;
    (*cpc)->data.cbm_modex_message_len = 0;

    (*cpc)->cbm_endpoint_init = NULL;
    (*cpc)->cbm_start_connect = oob_module_start_connect;
    (*cpc)->cbm_endpoint_finalize = NULL;
    (*cpc)->cbm_finalize = NULL;
    (*cpc)->cbm_uses_cts = false;

    opal_output_verbose(5, mca_btl_base_output,
                        "wv BTL: oob CPC available for use on %s:%d",
                        btl->device->ib_dev->name, 
                        btl->port_num);
    return OMPI_SUCCESS;
}

/*
 * Connect function.  Start initiation of connections to a remote
 * peer.  We send our Queue Pair information over the RML/OOB
 * communication mechanism.  On completion of our send, a send
 * completion handler is called.
 */
static int oob_module_start_connect(ompi_btl_wv_connect_base_module_t *cpc,
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

/*
 * Component finalize function.  Cleanup RML non-blocking receive.
 */
static int oob_component_finalize(void)
{
    if (rml_recv_posted) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, OMPI_RML_TAG_OPENIB);
        rml_recv_posted = false;
    }

    return OMPI_SUCCESS;
}


/*
 * Reply to a `start - connect' message
 */
static int reply_start_connect(mca_btl_wv_endpoint_t *endpoint,
                               mca_btl_wv_rem_info_t *rem_info)
{
    int rc;

    BTL_VERBOSE(("Initialized QPs, LID = %d",
                 ((mca_btl_wv_module_t*)endpoint->endpoint_btl)->lid));

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
                           mca_btl_wv_rem_info_t* rem_info)
{
    /* copy the rem_info stuff */
    memcpy(&((mca_btl_wv_endpoint_t*) endpoint)->rem_info, 
           rem_info, sizeof(mca_btl_wv_rem_info_t)); 
    
    /* copy over the rem qp info */
    memcpy(endpoint->rem_info.rem_qps,
           rem_info->rem_qps, sizeof(mca_btl_wv_rem_qp_info_t) * 
           mca_btl_wv_component.num_qps);
    
    BTL_VERBOSE(("Setting QP info,  LID = %d", endpoint->rem_info.rem_lid));
    return OMPI_SUCCESS;
}


/*
 * Connect the local ends of all qp's to the remote side
 */
static int qp_connect_all(mca_btl_wv_endpoint_t *endpoint)
{
    int i,rc;
    mca_btl_wv_module_t* wv_btl =
        (mca_btl_wv_module_t*)endpoint->endpoint_btl;
    HRESULT hr = 0;
    WV_QP_ATTRIBUTES wv_qp_attr;

    for (i = 0; i < mca_btl_wv_component.num_qps; i++) {
        struct wv_qp* qp = endpoint->qps[i].qp->lcl_qp;
        enum wv_mtu mtu = (enum wv_mtu) ((wv_btl->device->mtu < endpoint->rem_info.rem_mtu) ?
            wv_btl->device->mtu : endpoint->rem_info.rem_mtu) ;
        if (mca_btl_wv_component.verbose) {
            BTL_OUTPUT(("Set MTU to WV value %d (%s bytes)", mtu,
                        (mtu == WV_MTU_256) ? "256" :
                        (mtu == WV_MTU_512) ? "512" :
                        (mtu == WV_MTU_1024) ? "1024" :
                        (mtu == WV_MTU_2048) ? "2048" :
                        (mtu == WV_MTU_4096) ? "4096" :
                        "unknown (!)"));
        }
        /* ready to receive(RTR) */
        memset(&wv_qp_attr,0,sizeof wv_qp_attr);
        wv_qp_attr.QpState = WvQpStateRtr;
        wv_qp_attr.DestinationQpn = htonl(endpoint->rem_info.rem_qps[i].rem_qp_num);
        wv_qp_attr.ReceivePsn = htonl(endpoint->rem_info.rem_qps[i].rem_psn);
        wv_qp_attr.AddressVector.Route.Valid = 0;
        wv_qp_attr.AddressVector.DLid = htons((uint16_t)endpoint->rem_info.rem_lid);
        wv_qp_attr.AddressVector.ServiceLevel = mca_btl_wv_component.ib_service_level;
        wv_qp_attr.AddressVector.SourcePathBits = (uint8_t)wv_btl->src_path_bits;
        wv_qp_attr.AddressVector.PortNumber = (uint8_t)wv_btl->port_num;
        wv_qp_attr.PathMtu = (0x80<<mtu);
        wv_qp_attr.RnrNakTimeout = mca_btl_wv_component.ib_min_rnr_timer;
        if(mca_btl_wv_component.ib_path_rec_service_level > 0) {
            rc = get_pathrecord_sl(qp->context,
                                   wv_qp_attr.AddressVector.PortNumber,
                                   wv_btl->lid,
                                   wv_qp_attr.AddressVector.DLid);
            if(OMPI_ERROR == rc) {
                return OMPI_ERROR;
            }
            wv_qp_attr.AddressVector.ServiceLevel = rc;
        }
        int rtr_mask = WV_QP_ATTR_STATE|WV_QP_ATTR_AV|WV_QP_ATTR_DESTINATION_QPN
                        |WV_QP_ATTR_RECEIVE_PSN|WV_QP_ATTR_RESPONDER_RESOURCES
                        |WV_QP_ATTR_RNR_NAK_TIMEOUT;
        hr = qp->handle->Modify(&wv_qp_attr,rtr_mask,NULL);
        if(FAILED(hr)) {
            BTL_ERROR(("error modifing QP to RTR errno says %s",
                    strerror(errno)));
            return OMPI_ERROR;
        }else {
            qp->state = WvQpStateRtr;
        }

        /* ready to send(RTS) */
        memset(&wv_qp_attr,0,sizeof wv_qp_attr);
        wv_qp_attr.QpState = WvQpStateRts;
        wv_qp_attr.SendPsn = htonl(endpoint->qps[i].qp->lcl_psn);
        wv_qp_attr.PathMtu = (0x80<<0);
        wv_qp_attr.LocalAckTimeout = mca_btl_wv_component.ib_timeout;
        wv_qp_attr.SequenceErrorRetryCount = mca_btl_wv_component.ib_retry_count;
        wv_qp_attr.RnrRetryCount = BTL_WV_QP_TYPE_PP(i) ? 0 :
        mca_btl_wv_component.ib_rnr_retry;
        int rts_mask = WV_QP_ATTR_STATE|WV_QP_ATTR_ACK_TIMEOUT|WV_QP_ATTR_RNR_RETRY_COUNT
                        |WV_QP_ATTR_SEND_PSN|WV_QP_ATTR_INITIATOR_DEPTH;
        hr = qp->handle->Modify(&wv_qp_attr,rts_mask,NULL);
        if(FAILED(hr)) {
            BTL_ERROR(("error modifying QP to RTS errno says %s",
                        strerror(errno)));
            return OMPI_ERROR;
        }else {
            qp->state = WvQpStateRts;
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

    for (qp = 0; qp < mca_btl_wv_component.num_qps; ++qp)
        if(BTL_WV_QP_TYPE_PP(qp)) {
            rd_rsv_total +=
                mca_btl_wv_component.qp_infos[qp].u.pp_qp.rd_rsv;
            pp_qp_num++;
        }

    /* if there is no pp QPs we still need reserved WQE for eager rdma flow
     * control */
    if(0 == pp_qp_num && true == endpoint->use_eager_rdma)
        pp_qp_num = 1;

    for (qp = 0; qp < mca_btl_wv_component.num_qps; ++qp) { 
        struct wv_srq *srq = NULL;
        uint32_t max_recv_wr, max_send_wr;
        int32_t rd_rsv, rd_num_credits;

        /* QP used for SW flow control need some additional recourses */
        if(qp == mca_btl_wv_component.credits_qp) {
            rd_rsv = rd_rsv_total;
            rd_num_credits = pp_qp_num;
        } else {
            rd_rsv = rd_num_credits = 0;
        }

        if(BTL_WV_QP_TYPE_PP(qp)) {
            max_recv_wr = mca_btl_wv_component.qp_infos[qp].rd_num + rd_rsv;
            max_send_wr = mca_btl_wv_component.qp_infos[qp].rd_num +
                rd_num_credits;
        } else {
            srq = endpoint->endpoint_btl->qps[qp].u.srq_qp.srq;
            /* no receives are posted to SRQ qp */
            max_recv_wr = 0;
            max_send_wr = mca_btl_wv_component.qp_infos[qp].u.srq_qp.sd_max
                + rd_num_credits;
        }

        rc = qp_create_one(endpoint, qp, srq, max_recv_wr, max_send_wr);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    /* Now that all the qp's are created locally, post some receive
       buffers, setup credits, etc. */
    return mca_btl_wv_endpoint_post_recvs(endpoint);
}


/* Returns max inlne size for qp #N */
static uint32_t max_inline_size(int qp, mca_btl_wv_device_t *device)
{
    if (mca_btl_wv_component.qp_infos[qp].size <= device->max_inline_data) {
        /* If qp message size is smaller than max_inline_data,
         * we should enable inline messages */
        return mca_btl_wv_component.qp_infos[qp].size;
    } else if (mca_btl_wv_component.rdma_qp == qp || 0 == qp) {
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
        struct wv_srq *srq, uint32_t max_recv_wr, uint32_t max_send_wr)
{
    mca_btl_wv_module_t *wv_btl = endpoint->endpoint_btl;
    struct wv_qp *my_qp;
    size_t req_inline;
    HRESULT hr;
    WV_QP_CREATE create;
    WV_QP_ATTRIBUTES wv_qp_attr;
    WV_QP_ATTRIBUTES wv_attr;
    req_inline = max_inline_size(qp, wv_btl->device);  
    my_qp = (struct wv_qp*)malloc(sizeof(wv_qp));
    create.pSendCq = wv_btl->device->ib_cq[BTL_WV_LP_CQ]->handle;
    create.pReceiveCq = wv_btl->device->ib_cq[qp_cq_prio(qp)]->handle;
    create.pSharedReceiveQueue = (srq != NULL) ? srq->handle : NULL;
    create.Context = my_qp; 
    create.SendDepth = max_send_wr;
    create.SendSge = 1;
    create.ReceiveDepth = BTL_WV_QP_TYPE_PP(qp) ? max_recv_wr : 0; 
    create.ReceiveSge = 1;
    create.MaxInlineSend = max_inline_size(qp, wv_btl->device);
    create.InitiatorDepth = 0;
    create.ResponderResources = 0;
    create.QpType = WvQpTypeRc;
    create.QpFlags = 0;
    hr = wv_btl->device->ib_pd->handle->CreateConnectQueuePair(&create,&my_qp->conn_handle);
    if(FAILED(hr)) {
        BTL_ERROR(("error creating qp errno says %s", strerror(errno))); 
    } 

    my_qp->conn_handle->QueryInterface(IID_IWVQueuePair,(LPVOID*)&my_qp->handle);
    my_qp->context = wv_btl->device->ib_pd->context;
    my_qp->qp_context = NULL;
    my_qp->send_cq = wv_btl->device->ib_cq[BTL_WV_LP_CQ];
    my_qp->recv_cq = wv_btl->device->ib_cq[qp_cq_prio(qp)];
    my_qp->srq = srq;
    my_qp->state = WvQpStateReset;
    my_qp->qp_type = WvQpTypeRc;
    hr = my_qp->handle->Query(&wv_attr);
    if(FAILED(hr)) {
            BTL_ERROR(("error creating qp errno says %s", strerror(errno))); 
    }
    my_qp->qp_num = ntohl(wv_attr.Qpn);   
    endpoint->qps[qp].qp->lcl_qp = my_qp;
    endpoint->qps[qp].ib_inline_max = req_inline; 
    memset(&wv_qp_attr,0,sizeof wv_qp_attr);
    wv_qp_attr.QpState = WvQpStateInit;
    wv_qp_attr.PkeyIndex = wv_btl->pkey_index;
    wv_qp_attr.AddressVector.PortNumber = wv_btl->port_num;
    wv_qp_attr.AccessFlags = WV_ACCESS_REMOTE_WRITE | WV_ACCESS_REMOTE_READ;
    wv_qp_attr.PathMtu = (0x80<<0);
    int attr_mask = WV_QP_ATTR_STATE|WV_QP_ATTR_PKEY_INDEX|WV_QP_ATTR_PORT_NUMBER|WV_QP_ATTR_ACCESS_FLAGS;
    hr = endpoint->qps[qp].qp->lcl_qp->handle->Modify(&wv_qp_attr,attr_mask,NULL);
    if(FAILED(hr)) {
        BTL_ERROR(("error modifying qp to INIT errno says %s", strerror(errno)));
        return OMPI_ERROR;
    }else {
        endpoint->qps[qp].qp->lcl_qp->state = WvQpStateInit;
    }
    
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
         return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* pack the info in the send buffer */ 
    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT8));
    rc = opal_dss.pack(buffer, &message_type, 1, OPAL_UINT8);
    if (OPAL_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT64));
    rc = opal_dss.pack(buffer, &endpoint->subnet_id, 1, OPAL_UINT64);
    if (OPAL_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (message_type != ENDPOINT_CONNECT_REQUEST) {
        /* send the QP connect request info we respond to */
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer,
                           &endpoint->rem_info.rem_qps[0].rem_qp_num, 1,
                           OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &endpoint->rem_info.rem_lid, 1, OPAL_UINT16);
        if (OPAL_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    if (message_type != ENDPOINT_CONNECT_ACK) {
        int qp;
        /* stuff all the QP info into the buffer */
        for (qp = 0; qp < mca_btl_wv_component.num_qps; qp++) { 
            BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &endpoint->qps[qp].qp->lcl_qp->qp_num,
                               1, OPAL_UINT32);
            if (OPAL_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
            rc = opal_dss.pack(buffer, &endpoint->qps[qp].qp->lcl_psn, 1,
                               OPAL_UINT32); 
            if (OPAL_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT16));
        rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->lid, 1, OPAL_UINT16);
        if (OPAL_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->endpoint_btl->device->mtu, 1,
                OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("packing %d of %d\n", 1, OPAL_UINT32));
        rc = opal_dss.pack(buffer, &endpoint->index, 1, OPAL_UINT32);
        if (OPAL_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* send to remote endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, 
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
    mca_btl_wv_proc_t *ib_proc;
    mca_btl_wv_endpoint_t *ib_endpoint = NULL;
    int endpoint_state;
    int rc;
    uint32_t i, lcl_qp = 0;
    uint16_t lcl_lid = 0;
    int32_t cnt = 1;
    mca_btl_wv_rem_info_t rem_info;
    uint8_t message_type;
    bool master;
    
    /* start by unpacking data first so we know who is knocking at 
       our door */ 
    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT8));
    rc = opal_dss.unpack(buffer, &message_type, &cnt, OPAL_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        mca_btl_wv_endpoint_invoke_error(NULL);
        return;
    }
    
    BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT64));
    rc = opal_dss.unpack(buffer, &rem_info.rem_subnet_id, &cnt, OPAL_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        mca_btl_wv_endpoint_invoke_error(NULL);
        return;
    }
    
    if (ENDPOINT_CONNECT_REQUEST != message_type) {
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &lcl_qp, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_wv_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, &lcl_lid, &cnt, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_wv_endpoint_invoke_error(NULL);
            return;
        }
    }
    if (ENDPOINT_CONNECT_ACK != message_type) {
        int qp; 
        /* get ready for the data */
        rem_info.rem_qps = 
            (mca_btl_wv_rem_qp_info_t*) malloc(sizeof(mca_btl_wv_rem_qp_info_t) * 
                                                   mca_btl_wv_component.num_qps);
        
        /* unpack all the qp info */
        for (qp = 0; qp < mca_btl_wv_component.num_qps; ++qp) { 
            BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &rem_info.rem_qps[qp].rem_qp_num, &cnt,
                                 OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                mca_btl_wv_endpoint_invoke_error(NULL);
                return;
            }
            BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
            rc = opal_dss.unpack(buffer, &rem_info.rem_qps[qp].rem_psn, &cnt,
                                 OPAL_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                mca_btl_wv_endpoint_invoke_error(NULL);
                return;
            }
        }
        
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT16));
        rc = opal_dss.unpack(buffer, &rem_info.rem_lid, &cnt, OPAL_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_wv_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &rem_info.rem_mtu, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_wv_endpoint_invoke_error(NULL);
            return;
        }
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, OPAL_UINT32));
        rc = opal_dss.unpack(buffer, &rem_info.rem_index, &cnt, OPAL_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            mca_btl_wv_endpoint_invoke_error(NULL);
            return;
        }
    }
    
    BTL_VERBOSE(("Received QP Info,  LID = %d, SUBNET = %" PRIx64 "\n",
                 rem_info.rem_lid, 
                 rem_info.rem_subnet_id));
    
    master = orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_NAME,
                                    process_name) > 0 ? true : false;
    
    /* Need to protect the ib_procs list */
    OPAL_THREAD_LOCK(&mca_btl_wv_component.ib_lock);

    for (ib_proc = (mca_btl_wv_proc_t*)
            opal_list_get_first(&mca_btl_wv_component.ib_procs);
        ib_proc != (mca_btl_wv_proc_t*)
            opal_list_get_end(&mca_btl_wv_component.ib_procs);
        ib_proc  = (mca_btl_wv_proc_t*)opal_list_get_next(ib_proc)) {
        bool found = false;
        
        if (orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                   &ib_proc->proc_guid, process_name) != OPAL_EQUAL) {
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
            mca_btl_wv_endpoint_t *ib_endpoint_found = NULL;
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
        OPAL_THREAD_UNLOCK(&mca_btl_wv_component.ib_lock);
                return;
            }
        }
        
        if (!found) {
            BTL_ERROR(("can't find suitable endpoint for this peer\n")); 
            mca_btl_wv_endpoint_invoke_error(NULL);
        OPAL_THREAD_UNLOCK(&mca_btl_wv_component.ib_lock);
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
                mca_btl_wv_endpoint_invoke_error(ib_endpoint);
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
                mca_btl_wv_endpoint_invoke_error(ib_endpoint);
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
                mca_btl_wv_endpoint_cpc_complete(ib_endpoint);
                /* cpc complete unlock the endpoint */
             }
            break;
            
        case MCA_BTL_IB_WAITING_ACK:
            /* Tell main BTL that we're done */
            mca_btl_wv_endpoint_cpc_complete(ib_endpoint);
            /* cpc complete unlock the endpoint */
            break;
            
        case MCA_BTL_IB_CONNECT_ACK:
            send_connect_data(ib_endpoint, ENDPOINT_CONNECT_ACK);
            /* Tell main BTL that we're done */
            mca_btl_wv_endpoint_cpc_complete(ib_endpoint);
            /* cpc complete unlock the endpoint */
            break;
            
        case MCA_BTL_IB_CONNECTED:
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;

        case MCA_BTL_IB_FAILED:
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;

            default :
            BTL_ERROR(("Invalid endpoint state %d", endpoint_state));
            mca_btl_wv_endpoint_invoke_error(ib_endpoint);
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
        }
        break;
    }
    OPAL_THREAD_UNLOCK(&mca_btl_wv_component.ib_lock);
}

static int init_ud_qp(struct wv_context *context_arg,
                      struct mca_btl_wv_sa_qp_cache *cache)
{
    HRESULT hr;
    SIZE_T entries;
    WV_QP_CREATE create;
    WV_QP_ATTRIBUTES wv_attr;
    WV_QP_ATTRIBUTES wv_qp_attr;

    /* create cq */
    cache->cq = (struct wv_cq*)malloc(sizeof(struct wv_cq));
    cache->cq->context = cache->context;
    cache->cq->channel = NULL;
    cache->cq->cq_context = NULL;
    cache->cq->notify_cnt = 0;
    cache->cq->ack_cnt = 0;
    entries = 4;
    hr = cache->context->device_if->CreateCompletionQueue(&entries,&cache->cq->handle);
    cache->cq->cqe = (uint32_t) entries;
    if (NULL == cache->cq) {
        BTL_ERROR(("error creating cq, errno says %s", strerror(errno)));
        orte_show_help("help-mpi-btl-wv.txt", "init-fail-create-q",
                true, orte_process_info.nodename,
                __FILE__, __LINE__, "create_cq",
                strerror(errno), errno,
                context_arg->device->name);
        return OMPI_ERROR;
    }

    /* create qp */
    memset(&create,0,sizeof(create));
    create.pSendCq = cache->cq->handle;
    create.pReceiveCq = cache->cq->handle;
    create.Context = cache->qp;
    create.SendDepth = 2;
    create.SendSge = 1;
    create.ReceiveDepth = 2;
    create.ReceiveSge =1;
    create.QpType = WvQpTypeUd;
    hr = cache->pd->handle->CreateDatagramQueuePair(&create,&cache->qp->ud_handle);
    if(FAILED(hr)) {
        BTL_ERROR(("error create qp error says %s",strerror(errno)));
        return OMPI_ERROR;
    }
    cache->qp->ud_handle->QueryInterface(IID_IWVQueuePair,(LPVOID*)&cache->qp->handle);
    cache->qp->context = cache->pd->context;
    cache->qp->send_cq = cache->cq;
    cache->qp->recv_cq = cache->cq;
    cache->qp->state = WvQpStateReset;
    cache->qp->qp_type = WvQpTypeUd;
    hr = cache->qp->handle->Query(&wv_attr);
    if(FAILED(hr)) {
        BTL_ERROR(("error creating qp errno says %s",strerror(errno)));
        return OMPI_ERROR;
    }
    cache->qp->qp_num = ntohl(wv_attr.Qpn);

    /* modify qp to INIT */
    memset(&wv_qp_attr, 0, sizeof(wv_qp_attr));
    wv_qp_attr.QpState = WvQpStateInit;
    wv_qp_attr.AddressVector.PortNumber = cache->port_num;
    wv_qp_attr.Qkey = htonl(IB_GLOBAL_QKEY);
    wv_qp_attr.PathMtu = (0x80<<0);
    int init_mask = WV_QP_ATTR_STATE | WV_QP_ATTR_PKEY_INDEX 
                    | WV_QP_ATTR_PORT_NUMBER | WV_QP_ATTR_QKEY;
    hr = cache->qp->handle->Modify(&wv_qp_attr,init_mask,NULL);
    if(FAILED(hr)) {
        BTL_ERROR(("Error modifying QP[%x] to QPS_INIT errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    /* modify qp to RTR */
    memset(&wv_qp_attr, 0, sizeof(wv_qp_attr));
    wv_qp_attr.QpState = WvQpStateRtr;
    int rtr_mask = WV_QP_ATTR_STATE;
    hr = cache->qp->handle->Modify(&wv_qp_attr,rtr_mask,NULL);
    if(FAILED(hr)) {
        BTL_ERROR(("Error modifying QP[%x] to QPS_RTR errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }

    /* modify qp to RTS */
    memset(&wv_qp_attr, 0, sizeof(wv_qp_attr));
    wv_qp_attr.QpState = WvQpStateRts;
    int rts_mask = WV_QP_ATTR_STATE | WV_QP_ATTR_SEND_PSN;
    hr = cache->qp->handle->Modify(&wv_qp_attr,rtr_mask,NULL);
    if(FAILED(hr)) {
        BTL_ERROR(("Error modifying QP[%x] to QPS_RTS errno says: %s [%d]",
                    cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }
}

static void init_sa_mad(struct mca_btl_wv_sa_qp_cache *cache,
                        struct ib_mad_sa *sag,
                        WV_SEND_REQUEST *swr,
                        WV_SGE *ssge,
                        uint16_t lid,
                        uint16_t rem_lid)
{
    memset(sag, 0, sizeof(*sag));
    memset(swr, 0, sizeof(*swr));
    memset(ssge, 0, sizeof(*ssge));

    sag->mad_hdr.base_version = IB_MGMT_BASE_VERSION;
    sag->mad_hdr.mgmt_class = IB_MGMT_CLASS_SUBN_ADM;
    sag->mad_hdr.class_version = 2;
    sag->mad_hdr.method = IB_MGMT_METHOD_GET;
    sag->mad_hdr.attr_id = htons (IB_SA_ATTR_PATH_REC);
    sag->mad_hdr.tid[0] = IB_SA_TID_GET_PATH_REC_0 + cache->qp->qp_num;
    sag->mad_hdr.tid[1] = IB_SA_TID_GET_PATH_REC_1 + rem_lid;
    sag->sa_hdr.comp_mask[1] =
        htonl(IB_SA_PATH_REC_DLID | IB_SA_PATH_REC_SLID);
    sag->sa_data.path_record.dlid = htons(rem_lid);
    sag->sa_data.path_record.slid = htons(lid);

    swr->pSgl = ssge;
    swr->nSge = 1;
    swr->Opcode = WvSend;
    swr->Wr.Datagram.AhKey = (ULONG_PTR)cache->ah;
    swr->Wr.Datagram.DestinationQpn= IB_SA_QPN;
    swr->Wr.Datagram.DestinationQkey = IB_GLOBAL_QKEY;
    swr->Flags = WV_SEND_SIGNALED | WV_SEND_SOLICITED;

    ssge->pAddress = (void *)sag;
    ssge->Length = sizeof(*sag);
    ssge->Lkey = cache->mr->lkey;
}

static int get_pathrecord_info(struct mca_btl_wv_sa_qp_cache *cache,
                               struct ib_mad_sa *sag,
                               struct ib_mad_sa *sar,
                               WV_SEND_REQUEST *swr,
                               uint16_t lid,
                               uint16_t rem_lid)
{
    WV_SEND_REQUEST *bswr;
    WV_COMPLETION wc;
    struct timeval get_sl_rec_last_sent, get_sl_rec_last_poll;
    int got_sl_value, get_sl_rec_retries, ne, i;
    HRESULT hr;
    got_sl_value = 0;
    get_sl_rec_retries = 0;

    while (0 == got_sl_value) {
        hr = cache->qp->handle->PostSend((WV_SEND_REQUEST*)swr,(WV_SEND_REQUEST**)&bswr);
        if (FAILED(hr)) {
            BTL_ERROR(("error posing send on QP[%x] errno says: %s [%d]",
                       cache->qp->qp_num, strerror(errno), errno));
            return OMPI_ERROR;
        }
        gettimeofday(&get_sl_rec_last_sent, NULL);

        while (0 == got_sl_value) {
            ne = (int)cache->cq->handle->Poll(&wc,1);
            if (ne > 0
                    && wc.Status == WvWcSuccess
                    && wc.Opcode == WvReceive
                    && wc.Length >= sizeof(*sar)
                    && sar->mad_hdr.tid[0] == sag->mad_hdr.tid[0]
                    && sar->mad_hdr.tid[1] == sag->mad_hdr.tid[1]) {
                if (0 == sar->mad_hdr.status
                        && sar->sa_data.path_record.slid == htons(lid)
                        && sar->sa_data.path_record.dlid == htons(rem_lid)) {
                    /* Everything matches, so we have the desired SL */
                    cache->sl_values[rem_lid] =
                        sar->sa_data.path_record.qos_class_sl & IB_PATH_REC_SL_MASK;
                    got_sl_value = 1; /* still must repost recieve buf */
                } else {
                    /* Probably bad status, unlikely bad lid match. We will */
                    /* ignore response and let it time out so that we do a  */
                    /* retry, but after a delay. We must make a new TID so  */
                    /* the SM doesn't see it as the same request.           */
                    sag->mad_hdr.tid[1] += 0x10000;
                }
                hr = cache->qp->handle->PostReceive(cache->rwr.wr_id,cache->rwr.sg_list,
                                                    cache->rwr.num_sge);
                if (FAILED(hr)) {
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
                Sleep(100);  /* otherwise pause before polling again */
            } else if (ne < 0) {
                BTL_ERROR(("error polling CQ with %d: %s\n",
                    ne, strerror(errno)));
                return OMPI_ERROR;
            }
        }
    }
    return 0;
}

static int init_device(struct wv_context *context_arg,
                       struct mca_btl_wv_sa_qp_cache *cache,
                       uint32_t port_num)
{
    WV_ADDRESS_VECTOR aattr;
    WV_PORT_ATTRIBUTES pattr;
    HRESULT hr;
    int i, rc;
    struct wverbs_device *wvdev;
    struct wverbs_context *wvcontext;
    IWVProvider *provider;

    wvdev = CONTAINING_RECORD(context_arg->device, struct wverbs_device, device);
    wvcontext = (struct wverbs_context*)malloc(sizeof(struct wverbs_context));
    if(wvcontext == NULL) {
        return NULL;
    }
    memcpy(&wvcontext->device, wvdev, sizeof(struct wverbs_device));
    wvcontext->port = (struct wverbs_port*)malloc(wvdev->phys_port_cnt*sizeof(struct wverbs_port));
    WvGetObject(IID_IWVProvider,(LPVOID*)&provider);
    provider->OpenDevice(wvdev->guid, &wvcontext->context.device_if);
    for(i=0; i < wvdev->phys_port_cnt; i++) {
        wvcontext->port[i].port_num = (uint8_t) i + 1;
    }
    cache->context = &wvcontext->context;
    if (NULL == cache->context) {
        BTL_ERROR(("error obtaining device context for %s errno says %s",
                   context_arg->device->name, strerror(errno)));
        return OMPI_ERROR;
    }
    cache->device_name = strdup(cache->context->device->name);
    cache->port_num = port_num;

    /* init all sl_values to be SL_NOT_PRESENT */
    memset(&cache->sl_values, SL_NOT_PRESENT, sizeof(cache->sl_values));

    cache->next = sa_qp_cache;
    sa_qp_cache = cache;

    /* allocate the protection domain for the device */
    cache->pd->context = cache->context;
    cache->context->device_if->AllocateProtectionDomain(&cache->pd->handle);
    if (NULL == cache->pd) {
        BTL_ERROR(("error allocating protection domain for %s errno says %s",
            context_arg->device->name, strerror(errno)));
        return OMPI_ERROR;
    }

    /* register memory region */
    cache->mr->context = cache->context;
    cache->mr->pd = cache->pd;
    cache->mr->addr = cache->send_recv_buffer;
    cache->mr->length = sizeof(cache->send_recv_buffer);
    cache->pd->handle->RegisterMemory(cache->mr->addr, cache->mr->length,
                       WV_ACCESS_LOCAL_WRITE|WV_ACCESS_REMOTE_WRITE, NULL,
                       (WV_MEMORY_KEYS *)&cache->mr->lkey);
    cache->mr->rkey = ntohl(cache->mr->rkey);
    if (NULL == cache->mr) {
        BTL_ERROR(("error registering memory region, errno says %s", strerror(errno)));
        return OMPI_ERROR;
    }

    /* init the ud qp */
    rc = init_ud_qp(context_arg, cache);
    if (OMPI_ERROR == rc) {
        return OMPI_ERROR;
    }

    hr = cache->context->device_if->QueryPort(cache->port_num, &pattr);
    if (FAILED(hr)) {
        BTL_ERROR(("error getting port attributes for device %s "
                    "port number %d errno says %s",
                    context_arg->device->name,
                    cache->port_num, strerror(errno)));
        return OMPI_ERROR;
    }

    /* create address handle  */
    memset(&aattr, 0, sizeof(aattr));
    aattr.DLid = ntohs(pattr.SmLid);
    aattr.ServiceLevel = pattr.SmSl;
    aattr.PortNumber = cache->port_num;
    cache->pd->handle->CreateAddressHandle(&aattr,&cache->ah->handle,&cache->ah->key);
    if (NULL == cache->ah) {
        BTL_ERROR(("error creating address handle: %s", strerror(errno)));
        return OMPI_ERROR;
    }

    memset(&(cache->rwr), 0, sizeof(cache->rwr));
    cache->rwr.num_sge = 1;
    cache->rwr.sg_list = &(cache->rsge);
    memset(&(cache->rsge), 0, sizeof(cache->rsge));
    cache->rsge.pAddress = (void *)
        (cache->send_recv_buffer + sizeof(struct ib_mad_sa));
    cache->rsge.Length = sizeof(struct ib_mad_sa) + 40;
    cache->rsge.Lkey = cache->mr->lkey;

    hr = cache->qp->handle->PostReceive(cache->rwr.wr_id, cache->rwr.sg_list,
                                        cache->rwr.num_sge);
    if (FAILED(hr)) {
        BTL_ERROR(("error posing receive on QP[%x] errno says: %s [%d]",
                   cache->qp->qp_num, strerror(errno), errno));
        return OMPI_ERROR;
    }
    return 0;
}

static int get_pathrecord_sl(struct wv_context *context_arg,
                             uint32_t port_num,
                             uint16_t lid,
                             uint16_t rem_lid)
{
    WV_SEND_REQUEST swr;
    struct ib_mad_sa *sag, *sar;
    WV_SGE ssge;
    struct mca_btl_wv_sa_qp_cache *cache;
    long page_size = sysconf(_SC_PAGESIZE);
    int rc;

    /* search for a cached item */
    for (cache = sa_qp_cache; cache; cache = cache->next) {
        if (strcmp(cache->device_name,
                context_arg->device->name) == 0
                && cache->port_num == port_num) {
            break;
        }
    }

    if (NULL == cache) {
        /* init new cache */
        cache = (mca_btl_wv_sa_qp_cache*)_aligned_malloc(page_size, 
                    sizeof(struct mca_btl_wv_sa_qp_cache));
        if(cache == NULL) {
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
        sag = (ib_mad_sa *)(cache->send_recv_buffer);

        init_sa_mad(cache, sag, &swr, &ssge, lid, rem_lid);

        /* sar is the receive buffer (40 byte GRH) */
        sar = (ib_mad_sa *)(cache->send_recv_buffer + sizeof(struct ib_mad_sa) + 40);

        rc = get_pathrecord_info(cache, sag, sar, &swr, lid, rem_lid);
        if (0 != rc) {
            return rc;
        }
    }

    /* now all we do is send back the value laying around */
    return cache->sl_values[rem_lid];
}

