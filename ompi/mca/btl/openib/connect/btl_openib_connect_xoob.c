/*
 * Copyright (c) 2007      Mellanox Technologies.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "orte/mca/ns/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss.h"

#include "btl_openib.h"
#include "btl_openib_endpoint.h" 
#include "btl_openib_proc.h"
#include "btl_openib_xrc.h"
#include "connect/connect.h"

static int xoob_init(void);
static int xoob_start_connect(mca_btl_base_endpoint_t *e);
static int xoob_finalize(void);

/*
 * The "module" struct -- the top-level function pointers for the xoob
 * connection scheme.
 */
ompi_btl_openib_connect_base_funcs_t ompi_btl_openib_connect_xoob = {
    "xoob",
    /* No need for "open */
    NULL,
    /* Init */
    xoob_init,
    /* Connect */
    xoob_start_connect,
    /* Finalize */
    xoob_finalize,
};

#if HAVE_XRC

typedef enum {
    SEND,
    RECV
} xoob_qp_type;

typedef enum {
    ENDPOINT_XOOB_CONNECT_REQUEST,
    ENDPOINT_XOOB_CONNECT_RESPONSE,
    ENDPOINT_XOOB_CONNECT_XRC_REQUEST,
    ENDPOINT_XOOB_CONNECT_XRC_RESPONSE
} connect_message_type_t;

#define XOOB_TAG (ORTE_RML_TAG_DYNAMIC - 1)

#define XOOB_SET_REMOTE_INFO(EP, INFO)                                    \
do {                                                                      \
    /* copy the rem_info stuff */                                         \
    EP.rem_lid       = INFO.rem_lid;                                      \
    EP.rem_subnet_id = INFO.rem_subnet_id;                                \
    EP.rem_mtu       = INFO.rem_mtu;                                      \
    EP.rem_index     = INFO.rem_index;                                    \
    memcpy((void*)EP.rem_qps, (void*)INFO.rem_qps,                        \
            sizeof(mca_btl_openib_rem_qp_info_t));                        \
    /* copy the rem_info stuff */                                         \
    memcpy((void*)EP.rem_srqs, (void*)INFO.rem_srqs,                      \
            sizeof(mca_btl_openib_rem_srq_info_t) *                       \
            mca_btl_openib_component.num_xrc_qps);                        \
} while (0)

/* remote data processing */
static mca_btl_openib_endpoint_t* xoob_find_endpoint(orte_process_name_t* process_name, 
        uint64_t subnet_id, uint16_t lid, uint8_t message_type);

/* send/recv connection data */
static int xoob_reply_first_connect(mca_btl_openib_endpoint_t *endpoint,
        mca_btl_openib_rem_info_t *rem_info);
static int xoob_send_connect_data(mca_btl_base_endpoint_t* endpoint, 
        uint8_t message_type);
static int xoob_receive_connect_data(mca_btl_openib_rem_info_t *info, uint16_t *lid,
        uint8_t *message_type, orte_buffer_t* buffer);

/* func that take care for qp creations */
static int xoob_qp_create(mca_btl_base_endpoint_t* endpoint, xoob_qp_type type);
/* static int xoob_qp_connect(mca_btl_openib_endpoint_t *endpoint, xoob_qp_type type); */
static int xoob_qp_connect(mca_btl_openib_endpoint_t *endpoint, xoob_qp_type type, mca_btl_openib_rem_info_t *rem_info);

static void xoob_rml_send_cb(int status, orte_process_name_t* endpoint, 
                        orte_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata);
static void xoob_rml_recv_cb(int status, orte_process_name_t* process_name, 
                        orte_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata);

static int init_rem_info(mca_btl_openib_rem_info_t *rem_info);
static void free_rem_info(mca_btl_openib_rem_info_t *rem_info);
/*
 * Init function.  Post non-blocking RML receive to accept incoming
 * connection requests.
 */
static int xoob_init(void)
{
    int rc;

    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, 
                                 XOOB_TAG,
                                 ORTE_RML_PERSISTENT,
                                 xoob_rml_recv_cb,
                                 NULL);
    return (ORTE_SUCCESS == rc) ? OMPI_SUCCESS : rc;
}

/*
 * Connect function.  Start initiation of connections to a remote
 * peer.  We send our Queue Pair information over the RML/OOB
 * communication mechanism.  On completion of our send, a send
 * completion handler is called.
 */
static int xoob_start_connect(mca_btl_base_endpoint_t *endpoint)
{
    int rc = OMPI_SUCCESS;

    OPAL_THREAD_LOCK(&endpoint->ib_addr->addr_lock);
    switch (endpoint->ib_addr->status) {
        case MCA_BTL_IB_ADDR_CLOSED:
            BTL_VERBOSE(("XOOB. The IB addr: sid %d lid %d"
                        "in MCA_BTL_IB_ADDR_CLOSED status,"
                        " sending ENDPOINT_XOOB_CONNECT_REQUEST\n",
                        endpoint->ib_addr->subnet_id,endpoint->ib_addr->lid));
            if (OMPI_SUCCESS != (rc = xoob_qp_create(endpoint, SEND))) {
                break;
            }

            /* Send connection info over to remote endpoint */
            endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
            endpoint->ib_addr->status = MCA_BTL_IB_ADDR_CONNECTING;
            if (OMPI_SUCCESS !=
                    (rc = xoob_send_connect_data(endpoint, ENDPOINT_XOOB_CONNECT_REQUEST))) {
                BTL_ERROR(("error sending connect request, error code %d", rc)); 
            }
            break;
        case MCA_BTL_IB_ADDR_CONNECTING:
            BTL_VERBOSE(("XOOB. The IB addr: sid %d lid %d"
                        "in MCA_BTL_IB_ADDR_CONNECTING status,"
                        " Subscribing to this address\n",
                        endpoint->ib_addr->subnet_id,endpoint->ib_addr->lid));
            /* some body already connectng to this machine, lets wait */
            opal_list_append(&endpoint->ib_addr->pending_ep, (opal_list_item_t*)endpoint);
            endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
            break;
        case MCA_BTL_IB_ADDR_CONNECTED:
            /* so we have the send qp, we just need the recive site.
             * Send request for SRQ numbers */
            BTL_VERBOSE(("XOOB. The IB addr: sid %d lid %d"
                        "in MCA_BTL_IB_ADDR_CONNECTED status,"
                        " sending ENDPOINT_XOOB_CONNECT_XRC_REQUEST\n",
                        endpoint->ib_addr->subnet_id,endpoint->ib_addr->lid));
            endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
            if (OMPI_SUCCESS !=
                    (rc = xoob_send_connect_data(endpoint, ENDPOINT_XOOB_CONNECT_XRC_REQUEST))) {
                BTL_ERROR(("error sending xrc connect request, error code %d", rc)); 
            }
            break;
        default :
            BTL_ERROR(("XOOB: Invalid endpoint status %d", endpoint->ib_addr->status));
    } 
    OPAL_THREAD_UNLOCK(&endpoint->ib_addr->addr_lock);
    return rc;
}

/*
 * Finalize function.  Cleanup RML non-blocking receive.
 */
static int xoob_finalize(void)
{
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, XOOB_TAG);
    return OMPI_SUCCESS;
}

/*
 * Reply to a `start - connect' message
 */
static int xoob_reply_first_connect(mca_btl_openib_endpoint_t *endpoint,
                               mca_btl_openib_rem_info_t *rem_info)
{
    int rc;

    BTL_VERBOSE(("Initialized QPs, LID = %d",
                 ((mca_btl_openib_module_t*)endpoint->endpoint_btl)->lid));

    /* Create local QP's and post receive resources */
    if (OMPI_SUCCESS != (rc = xoob_qp_create(endpoint, RECV))) {
        return rc;
    }
    
    /* Connect to remote endpoint qp's */
    if (OMPI_SUCCESS != (rc = xoob_qp_connect(endpoint, RECV, rem_info))) {
        return rc;
    }

    if (OMPI_SUCCESS !=
        (rc = xoob_send_connect_data(endpoint, ENDPOINT_XOOB_CONNECT_RESPONSE))) {
        BTL_ERROR(("error in endpoint send connect request error code is %d",
                   rc));
        return rc;
    }
    return OMPI_SUCCESS;
}

/*
 * Create the local side of all the qp's.  The remote sides will be
 * connected later.
 */
static int xoob_qp_create(mca_btl_base_endpoint_t* endpoint, xoob_qp_type type)
{
    int prio = BTL_OPENIB_LP_CQ; /* all send completions go to low prio CQ */
    uint32_t send_wr;
    struct ibv_qp **ib_qp;
    uint32_t *psn;
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr attr;

    mca_btl_openib_module_t *openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;
    
    /* Prepare QP structs */
    if (SEND == type) {
        BTL_VERBOSE(("XOOB. Creating Send QP\n"));
        ib_qp = &endpoint->qps[0].qp->lcl_qp;
        psn = &endpoint->qps[0].qp->lcl_psn;
        /* reserve additional wr for eager rdma credit management */
        send_wr = endpoint->ib_addr->qp->sd_wqe +
            (mca_btl_openib_component.use_eager_rdma ?
             mca_btl_openib_component.max_eager_rdma : 0);
    } else {
        BTL_VERBOSE(("XOOB. Creating Recv QP\n"));
        assert(NULL == endpoint->xrc_recv_qp);
        ib_qp = &endpoint->xrc_recv_qp;
        psn = &endpoint->xrc_recv_psn;
        /* this QP is not used for send so no need for send queue */
        send_wr = 0;
    }
    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr)); 
    memset(&attr, 0, sizeof(struct ibv_qp_attr)); 

    qp_init_attr.send_cq = qp_init_attr.recv_cq = openib_btl->hca->ib_cq[prio];

    /* no need recv queue; receives are posted to srq */
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_send_wr = send_wr;

    qp_init_attr.cap.max_send_sge = mca_btl_openib_component.ib_sg_list_size;
    /* this one is ignored by driver */
    qp_init_attr.cap.max_recv_sge = mca_btl_openib_component.ib_sg_list_size;
    qp_init_attr.qp_type = IBV_QPT_XRC;
    qp_init_attr.xrc_domain = openib_btl->hca->xrc_domain;
    *ib_qp = ibv_create_qp(openib_btl->hca->ib_pd, &qp_init_attr); 

    if (NULL == *ib_qp) { 
        BTL_ERROR(("error creating qp errno says %s", strerror(errno))); 
        return OMPI_ERROR; 
    }
    openib_btl->ib_inline_max = qp_init_attr.cap.max_inline_data; 

    attr.qp_state = IBV_QPS_INIT; 
    attr.pkey_index = openib_btl->pkey_index;
    attr.port_num = openib_btl->port_num; 
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ; 

    if (ibv_modify_qp(*ib_qp, &attr,
                      IBV_QP_STATE |
                      IBV_QP_PKEY_INDEX |
                      IBV_QP_PORT |
                      IBV_QP_ACCESS_FLAGS )) {
        BTL_ERROR(("error modifying qp to INIT errno says %s",
                    strerror(errno))); 
        return OMPI_ERROR; 
    } 

    /* Setup meta data on the endpoint */
    *psn = lrand48() & 0xffffff;

    /* Now that all the qp's are created locally, post some receive
       buffers, setup credits, etc. */
    return mca_btl_openib_endpoint_post_recvs(endpoint);
}

/*
 * Connect the local ends of qp to the remote side
 */
static int xoob_qp_connect(mca_btl_openib_endpoint_t *endpoint, xoob_qp_type type, mca_btl_openib_rem_info_t *rem_info)
{
    struct ibv_qp* qp;
    struct ibv_qp_attr attr;
    uint32_t psn;
    mca_btl_openib_module_t* openib_btl =
        (mca_btl_openib_module_t*)endpoint->endpoint_btl;

    if (SEND == type) {
        BTL_VERBOSE(("XOOB. Connecting Send QP\n"));
        assert(NULL != endpoint->qps);
        qp = endpoint->qps[0].qp->lcl_qp;
        psn = endpoint->qps[0].qp->lcl_psn;
    } else {
        BTL_VERBOSE(("XOOB. Connecting Recv QP\n"));
        assert(NULL != endpoint->xrc_recv_qp);
        qp = endpoint->xrc_recv_qp;
        psn = endpoint->xrc_recv_psn;
    }

    memset(&attr, 0, sizeof(attr));
    attr.qp_state           = IBV_QPS_RTR;
    attr.path_mtu = (openib_btl->hca->mtu < endpoint->rem_info.rem_mtu) ? 
        openib_btl->hca->mtu : rem_info->rem_mtu;
    attr.dest_qp_num        = rem_info->rem_qps->rem_qp_num;
    attr.rq_psn             = rem_info->rem_qps->rem_psn;
    attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
    attr.min_rnr_timer  = mca_btl_openib_component.ib_min_rnr_timer;
    attr.ah_attr.is_global     = 0;
    attr.ah_attr.dlid          = rem_info->rem_lid;
    attr.ah_attr.sl            = mca_btl_openib_component.ib_service_level;
    attr.ah_attr.src_path_bits = openib_btl->src_path_bits;
    attr.ah_attr.port_num      = openib_btl->port_num;
    attr.ah_attr.static_rate   = 0;

    if (mca_btl_openib_component.verbose) {
        BTL_VERBOSE(("Set MTU to IBV value %d (%s bytes)", attr.path_mtu,
                    (attr.path_mtu == IBV_MTU_256) ? "256" :
                    (attr.path_mtu == IBV_MTU_512) ? "512" :
                    (attr.path_mtu == IBV_MTU_1024) ? "1024" :
                    (attr.path_mtu == IBV_MTU_2048) ? "2048" :
                    (attr.path_mtu == IBV_MTU_4096) ? "4096" :
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
    attr.rnr_retry      = mca_btl_openib_component.ib_rnr_retry;
    attr.sq_psn         = psn;
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

    return OMPI_SUCCESS;
}

/* Receive connect information to remote endpoint */
static int xoob_receive_connect_data(mca_btl_openib_rem_info_t *info, uint16_t *lid,
        uint8_t *message_type, orte_buffer_t* buffer)
{
    int cnt = 1, rc, srq;

    /* Recv standart header */
    BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT8));
    rc = orte_dss.unpack(buffer, message_type, &cnt, ORTE_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return OMPI_ERROR;
    }
    BTL_VERBOSE(("XOOB Recv unpack Message type  = %d", *message_type));

    BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT64));
    rc = orte_dss.unpack(buffer, &info->rem_subnet_id, &cnt, ORTE_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return OMPI_ERROR;
    }
    BTL_VERBOSE(("XOOB Recv unpack sid  = %d", info->rem_subnet_id));

    BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT16));
    rc = orte_dss.unpack(buffer, &info->rem_lid, &cnt, ORTE_UINT16);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return OMPI_ERROR;
    }
    BTL_VERBOSE(("XOOB Recv unpack lid  = %d", info->rem_lid));

    /* Till now we got the standart header, now we continue to recieve data for
     * different packet types 
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == *message_type ||
            ENDPOINT_XOOB_CONNECT_RESPONSE == *message_type) {
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT32));
        rc = orte_dss.unpack(buffer, &info->rem_qps->rem_qp_num, &cnt,
                ORTE_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("XOOB Recv unpack remote qp  = %d", info->rem_qps->rem_qp_num));

        BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT32));
        rc = orte_dss.unpack(buffer, &info->rem_qps->rem_psn, &cnt,
                ORTE_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("XOOB Recv unpack remote psn = %d", info->rem_qps->rem_psn));

        BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT32));
        rc = orte_dss.unpack(buffer, &info->rem_mtu, &cnt, ORTE_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("XOOB Recv unpack remote mtu = %d", info->rem_mtu));
    }

    if (ENDPOINT_XOOB_CONNECT_REQUEST == *message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_REQUEST == *message_type) {
        /* unpack requested lid info */
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT16));
        rc = orte_dss.unpack(buffer, lid, &cnt, ORTE_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("XOOB Recv unpack requested lid = %d", *lid));
    }

    if (ENDPOINT_XOOB_CONNECT_RESPONSE == *message_type || 
            ENDPOINT_XOOB_CONNECT_XRC_RESPONSE == *message_type) {
        BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT32));
        rc = orte_dss.unpack(buffer, &info->rem_index, &cnt, ORTE_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return OMPI_ERROR;
        }
        BTL_VERBOSE(("XOOB Recv unpack remote index = %d", info->rem_index));

        for (srq = 0; srq < mca_btl_openib_component.num_xrc_qps; srq++) { 
            BTL_VERBOSE(("unpacking %d of %d\n", cnt, ORTE_UINT32));
            rc = orte_dss.unpack(buffer, &info->rem_srqs[srq].rem_srq_num, &cnt, ORTE_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return OMPI_ERROR;
            }
            BTL_VERBOSE(("XOOB Recv unpack remote index srq num[%d]= %d", srq, info->rem_srqs[srq].rem_srq_num));
        }
    }
    return OMPI_SUCCESS;
}
/*
 * send connect information to remote endpoint
 */
static int xoob_send_connect_data(mca_btl_base_endpoint_t* endpoint, 
        uint8_t message_type)
{
    orte_buffer_t* buffer = OBJ_NEW(orte_buffer_t);
    int rc, srq;

    if (NULL == buffer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* Bulding standart header that we use in all messages:
     * - Message type,
     * - Our subnet id
     * - Our LID
     */
    /* pack the info in the send buffer */ 
    BTL_VERBOSE(("XOOB Send pack Message type = %d", message_type));
    BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT8));
    rc = orte_dss.pack(buffer, &message_type, 1, ORTE_UINT8);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("XOOB Send pack sid = %d", endpoint->subnet_id));
    BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT64));
    rc = orte_dss.pack(buffer, &endpoint->subnet_id, 1, ORTE_UINT64);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("XOOB Send pack lid = %d", endpoint->endpoint_btl->lid));
    BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT16));
    rc = orte_dss.pack(buffer, &endpoint->endpoint_btl->lid, 1, ORTE_UINT16);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Now we append to standart header additional information
     * that is required for full (open qp,etc..) connect request and response:
     * - qp_num of first qp
     * - psn of first qp
     * - MTU
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type ||
            ENDPOINT_XOOB_CONNECT_RESPONSE == message_type) {
        struct ibv_qp *qp;
        uint32_t psn;

        if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type) {
            qp = endpoint->qps[0].qp->lcl_qp;
            psn = endpoint->qps[0].qp->lcl_psn;
        } else {
            qp = endpoint->xrc_recv_qp;
            psn = endpoint->xrc_recv_psn;
        }
        /* stuff all the QP info into the buffer */
        /* we need to send only one QP */
        BTL_VERBOSE(("XOOB Send pack qp num = %d", qp->qp_num));
        BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT32));
        rc = orte_dss.pack(buffer, &qp->qp_num, 1, ORTE_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        BTL_VERBOSE(("XOOB Send pack lpsn = %d", psn));
        BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT32));
        rc = orte_dss.pack(buffer, &psn, 1, ORTE_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        BTL_VERBOSE(("XOOB Send pack mtu = %d", endpoint->endpoint_btl->hca->mtu));
        BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT32));
        rc = orte_dss.pack(buffer, &endpoint->endpoint_btl->hca->mtu, 1,
                ORTE_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* We append to header above additional information
     * that is required for full & XRC connect request:
     * - The lid ob btl on remote site that we want to connect 
     */
    if (ENDPOINT_XOOB_CONNECT_REQUEST == message_type ||
            ENDPOINT_XOOB_CONNECT_XRC_REQUEST == message_type) {
        /* when we are sending request we add remote lid that we want to connect */

        BTL_VERBOSE(("XOOB Send pack remote lid = %d", endpoint->ib_addr->lid));
        BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT16));
        rc = orte_dss.pack(buffer, &endpoint->ib_addr->lid, 1, ORTE_UINT16);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    } 

    /* We append to header above additional information
     * that is required for full & XRC connect response:
     * - index of our endpoint
     * - array of xrc-srq numbers
     */
    if (ENDPOINT_XOOB_CONNECT_RESPONSE == message_type || 
            ENDPOINT_XOOB_CONNECT_XRC_RESPONSE == message_type) {
        /* we need to send the endpoint index for immidate send */
        BTL_VERBOSE(("XOOB Send pack index = %d", endpoint->index));
        BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT32));
        rc = orte_dss.pack(buffer, &endpoint->index, 1, ORTE_UINT32);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* on response we add all SRQ numbers */
        for (srq = 0; srq < mca_btl_openib_component.num_xrc_qps; srq++) { 
            BTL_VERBOSE(("XOOB Send pack srq[%d] num  = %d", srq, endpoint->endpoint_btl->qps[srq].u.srq_qp.srq->xrc_srq_num));
            BTL_VERBOSE(("packing %d of %d\n", 1, ORTE_UINT32));
            rc = orte_dss.pack(buffer, &endpoint->endpoint_btl->qps[srq].u.srq_qp.srq->xrc_srq_num,
                    1, ORTE_UINT32);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    /* send to remote endpoint */
    rc = orte_rml.send_buffer_nb(&endpoint->endpoint_proc->proc_guid, 
            buffer, XOOB_TAG, 0,
            xoob_rml_send_cb, NULL);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    BTL_VERBOSE(("XOOB Send QP Info, LID = %d, SUBNET = %016x\n, Message type = %d",
                endpoint->endpoint_btl->lid, 
                endpoint->subnet_id,
                message_type));

    return OMPI_SUCCESS;
}


/*
 * Callback when we have finished RML sending the connect data to a
 * remote peer
 */
static void xoob_rml_send_cb(int status, orte_process_name_t* endpoint, 
                        orte_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata)
{
    OBJ_RELEASE(buffer);
}

static mca_btl_openib_endpoint_t* xoob_find_endpoint(orte_process_name_t* process_name, 
        uint64_t subnet_id, uint16_t lid, uint8_t message_type)
{
    size_t i;
    mca_btl_openib_proc_t *ib_proc;
    mca_btl_openib_endpoint_t *ib_endpoint = NULL;
    bool found = false;

    BTL_VERBOSE(("XOOB. searching for ep and proc with follow parameters:"
                "jobid %d, vpid %d, sid %d, lid %d",
                process_name->jobid, process_name->vpid, subnet_id, lid));
    /* find ibproc */
    for (ib_proc = (mca_btl_openib_proc_t*)
            opal_list_get_first(&mca_btl_openib_component.ib_procs);
            ib_proc != (mca_btl_openib_proc_t*)
            opal_list_get_end(&mca_btl_openib_component.ib_procs);
            ib_proc  = (mca_btl_openib_proc_t*)opal_list_get_next(ib_proc)) {
        if (orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                    &ib_proc->proc_guid, process_name) == ORTE_EQUAL) {
            found = true;
            break;
        }
    }
    /* we found our ib_proc, lets find endpoint now */
    if (found) {
        for (i = 0; i < ib_proc->proc_endpoint_count; i++) {
            ib_endpoint = ib_proc->proc_endpoints[i];
            /* we need to check different 
             * lid for different message type */
            if (ENDPOINT_XOOB_CONNECT_RESPONSE || ENDPOINT_XOOB_CONNECT_XRC_RESPONSE) {
                /* response message */
                if (ib_endpoint->subnet_id == subnet_id &&
                        ib_endpoint->ib_addr->lid == lid) {
                    break; /* Found one */
                }
            } else {
                /* request message */
                if (ib_endpoint->subnet_id == subnet_id &&
                        ib_endpoint->endpoint_btl->lid == lid) {
                    break; /* Found one */
                }
            }
        }
        if (NULL == ib_endpoint) {
                BTL_ERROR(("can't find suitable endpoint for this peer\n")); 
        }
    } else {
            BTL_ERROR(("can't find suitable endpoint for this peer\n")); 
    }

    return ib_endpoint;
}

/*
 * Non blocking RML recv callback.  Read incoming QP and other info,
 * and if this endpoint is trying to connect, reply with our QP info,
 * otherwise try to modify QP's and establish reliable connection
 */
static void xoob_rml_recv_cb(int status, orte_process_name_t* process_name, 
                        orte_buffer_t* buffer, orte_rml_tag_t tag, 
                        void* cbdata)
{
    int rc;
    uint8_t message_type;
    uint16_t requested_lid = 0;
    mca_btl_openib_rem_info_t rem_info;
    mca_btl_openib_endpoint_t *ib_endpoint = NULL;

    if ( OMPI_SUCCESS != init_rem_info(&rem_info)) {
        return;
    }

    /* Get data. */
    if ( OMPI_SUCCESS != xoob_receive_connect_data(&rem_info, &requested_lid, &message_type, buffer)) {
        BTL_ERROR(("XOOB. Failed to read data\n"));
        return;
    }

    /* Processing message */
    switch (message_type) {
        case ENDPOINT_XOOB_CONNECT_REQUEST:
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_REQUEST: lid %d, sid %d\n",
                        rem_info.rem_lid, 
                        rem_info.rem_subnet_id));
            ib_endpoint = xoob_find_endpoint(process_name,rem_info.rem_subnet_id,
                    requested_lid, message_type);
            if ( NULL == ib_endpoint) {
                BTL_ERROR(("XOOB. Got ENDPOINT_XOOB_CONNECT_REQUEST."
                            " Failed to find endpoint with subnet %d and LID %d",
                            rem_info.rem_subnet_id,requested_lid));
                return; 
            }
            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
            /* we should create qp and send the info + srq to requestor */
            rc = xoob_reply_first_connect(ib_endpoint, &rem_info);
            if (OMPI_SUCCESS != rc) {
                BTL_ERROR(("error in endpoint reply start connect"));
                return; 
            }
            /* enable pooling for this btl */
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;
        case ENDPOINT_XOOB_CONNECT_XRC_REQUEST:
            /* pasha we don't need the remote lid here ??*/
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_XRC_REQUEST: lid %d, sid %d\n",
                        rem_info.rem_lid, 
                        rem_info.rem_subnet_id));
            ib_endpoint = xoob_find_endpoint(process_name,rem_info.rem_subnet_id,
                    requested_lid, message_type);
            if ( NULL == ib_endpoint) {
                BTL_ERROR(("XOOB. Got ENDPOINT_XOOB_CONNECT_XRC_REQUEST."
                            " Failed to find endpoint with subnet %d and LID %d",
                            rem_info.rem_subnet_id,requested_lid));
                return; 
            }
            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
            rc = xoob_send_connect_data(ib_endpoint, ENDPOINT_XOOB_CONNECT_XRC_RESPONSE);
            if (OMPI_SUCCESS != rc) {
                BTL_ERROR(("error in endpoint reply start connect"));
                return; 
            }
            /* enable pooling for this btl */
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;
        case ENDPOINT_XOOB_CONNECT_RESPONSE:
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_RESPONSE: lid %d, sid %d\n",
                        rem_info.rem_lid, 
                        rem_info.rem_subnet_id));
            ib_endpoint = xoob_find_endpoint(process_name, rem_info.rem_subnet_id,
                    rem_info.rem_lid, message_type);
            if ( NULL == ib_endpoint) { 
                BTL_ERROR(("Xoob. Got ENDPOINT_XOOB_CONNECT_RESPONSE."
                            " Failed to find endpoint with subnet %d and LID %d",
                            rem_info.rem_subnet_id,rem_info.rem_lid));
                return; 
            }
            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
            /* we got all the data qp+srq. switch the endpoint to connect mode */
            XOOB_SET_REMOTE_INFO(ib_endpoint->rem_info, rem_info);
            BTL_VERBOSE(("rem_info: lid %d, sid %d ep %d %d",
                        rem_info.rem_lid, 
                        rem_info.rem_subnet_id,ib_endpoint->rem_info.rem_lid,ib_endpoint->rem_info.rem_subnet_id));
            if (OMPI_SUCCESS != xoob_qp_connect(ib_endpoint, SEND, &rem_info)) {
                BTL_ERROR(("XOOB: Failed to connect  endpoint\n"));
                return;
            }
            mca_btl_openib_endpoint_connected(ib_endpoint);
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;
        case ENDPOINT_XOOB_CONNECT_XRC_RESPONSE:
            BTL_VERBOSE(("Received ENDPOINT_XOOB_CONNECT_XRC_REQUEST: lid %d, sid %d\n",
                        rem_info.rem_lid, 
                        rem_info.rem_subnet_id));
            ib_endpoint = xoob_find_endpoint(process_name, rem_info.rem_subnet_id, 
                    rem_info.rem_lid, message_type);
            if ( NULL == ib_endpoint) { 
                BTL_ERROR(("XOOB. Got ENDPOINT_XOOB_CONNECT_XRC_RESPONSE."
                            " Failed to find endpoint with subnet %d and LID %d",
                            rem_info.rem_subnet_id,rem_info.rem_lid));
                return; 
            }
            OPAL_THREAD_LOCK(&ib_endpoint->endpoint_lock);
            /* we got srq numbers on our request */
            XOOB_SET_REMOTE_INFO(ib_endpoint->rem_info, rem_info);
            mca_btl_openib_endpoint_connected(ib_endpoint);
            OPAL_THREAD_UNLOCK(&ib_endpoint->endpoint_lock);
            break;
        default :
            BTL_ERROR(("XOOB: Invalid message type %d", message_type));
    }

    free_rem_info(&rem_info);
}

static int init_rem_info(mca_btl_openib_rem_info_t *rem_info)
{
    rem_info->rem_qps = (mca_btl_openib_rem_qp_info_t*)malloc(sizeof(mca_btl_openib_rem_qp_info_t));
    if (NULL == rem_info->rem_qps) {
        BTL_ERROR(("XOOB. Failed to allocate memory for remote QP data\n"));
        return OMPI_ERROR;
    }
    rem_info->rem_srqs = (mca_btl_openib_rem_srq_info_t*)malloc(sizeof(mca_btl_openib_rem_srq_info_t) *
            mca_btl_openib_component.num_xrc_qps);
    if (NULL == rem_info->rem_srqs) {
        BTL_ERROR(("XOOB. Failed to allocate memory for remote SRQ data\n"));
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

static void free_rem_info(mca_btl_openib_rem_info_t *rem_info)
{
    if (NULL != rem_info->rem_qps) {
        free(rem_info->rem_qps);
    }
    if (NULL != rem_info->rem_srqs) {
        free(rem_info->rem_srqs);
    }
}

#else
/* In case if the XRC was disabled during compilation we will print message and return error */
static int xoob_init(void)
{
    printf("xoob init\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int xoob_start_connect(mca_btl_base_endpoint_t *e)
{
    printf("xoob start connect\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}

static int xoob_finalize(void)
{
    printf("xoob finalize\n");
    return OMPI_ERR_NOT_IMPLEMENTED;
}
#endif
