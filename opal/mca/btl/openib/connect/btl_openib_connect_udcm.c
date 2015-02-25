/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * The UD connection module creates and listens on a unconnected
 * datagram (UD) queue pair (QP) for connections requests.
 *
 * There are two ways an RC connection can be established by UD:
 *  1. One side starts a connection and the request is received before
 *    the receiving side starts a connection. (One sided)
 *  2. Both sides send a request before either receives a request.
 *    (Simulaneous).
 *
 * The protocol for case 1 looks like:
 *             peer1       peer2
 *                 |       |
 *         CONNECT |------>|
 *                 |       | move QPs to RTS
 *                 |       | post rc receive
 *                 |<------| CONNECT
 * move QPs to RTS |       |
 *    post rc send |       |
 *                 |<------| COMPLETE
 *        COMPLETE |------>|
 *
 * The protocol for case 2 looks like:
 *             peer1       peer2
 *                 |       |
 *         CONNECT |<----->| CONNECT
 * move QPs to RTS |       | move QPs to RTS
 *    post rc send |       | post rc recv
 *        COMPLETE |<----->| COMPLETE
 *
 */

#include "opal_config.h"

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <infiniband/verbs.h>
#include <signal.h>

#include <pthread.h>

#include "opal/util/show_help.h"
#include "opal/util/proc.h"
#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal/util/alfg.h"
#include "opal_stdint.h"

#include "btl_openib_endpoint.h"
#include "btl_openib_proc.h"
#include "btl_openib_fd.h"
#include "btl_openib_async.h"
#include "connect/connect.h"

#include "opal/mca/mpool/grdma/mpool_grdma.h"
#include "opal/util/sys_limits.h"

#if (ENABLE_DYNAMIC_SL)
#include "connect/btl_openib_connect_sl.h"
#endif

#if HAVE_XRC
#include "btl_openib_xrc.h"
#endif

/*--------------------------------------------------------------------*/

/*
 * Message that this CPC includes in the modex.  Filed are laid out in
 * order to avoid holes.
 */
typedef struct {
    /** The qp_num we are listening on (this alone may be sufficient for
        matching the endpoint) */
    uint32_t mm_qp_num;
    /** The LID that we're listening on; it also identifies the source
        endpoint when an UD CM request arrives */
    uint16_t mm_lid;
    /** The port number of this port, also used to locate the source
        endpoint when an UD CM request arrives */
    uint8_t mm_port_num;
} modex_msg_t;

/*
 * The UD module (i.e., the base module plus more meta data required
 * by this CPC)
 */
typedef struct udcm_module {
    opal_btl_openib_connect_base_module_t cpc;

    /* This mutex must be held by any thread modifying
       the module directly */
    opal_mutex_t        cm_lock;

    /* Signal callbacks and threads that this module
       is exiting */
    bool                cm_exiting;

    /* UD QP this module is listening on */
    struct ibv_qp *listen_qp;

    /* Work request completion queues */
    struct ibv_cq *cm_send_cq, *cm_recv_cq;

    /* Completion channel for receive completions */
    struct ibv_comp_channel *cm_channel;

    /* Memory register for cm_buffer */
    struct ibv_mr *cm_mr;

    /* All buffers (grh + receive, send) */
    char          *cm_buffer;

    /* Pointer to send buffer (near end of cm_buffer) */
    char          *cm_send_buffer;

    /* Length of largest message */
    size_t msg_length;

    /* timeout thread */
    opal_mutex_t cm_timeout_lock;

    /* Messages waiting for ack */
    opal_list_t  flying_messages;

    /* This mutex must be held when calling ibv_post_send
       or waiting on cm_send_cq */
    opal_mutex_t cm_send_lock;

    /* Receive queue */
    opal_mutex_t cm_recv_msg_queue_lock;
    opal_list_t  cm_recv_msg_queue;
    bool         cm_message_event_active;

    /* The associated BTL */
    struct mca_btl_openib_module_t *btl;

    /* This module's modex message */
    modex_msg_t modex;

    /** The channel is being monitored */
    bool channel_monitored;
} udcm_module_t;

/*
 * Per-endpoint UD data
 */
typedef struct {
    /* Lock for IPC between threads in the ud CPC */
    opal_mutex_t   udep_lock;

    struct ibv_ah *ah;

    bool           sent_req, recv_req, recv_resp, recv_comp;

    /* Has this endpoint's data been initialized */
    bool           udep_initialized, udep_created_qps;
} udcm_endpoint_t;

typedef struct udcm_qp_t {
    uint32_t qp_num;
    uint32_t psn;
} udcm_qp_t;

typedef enum udcm_message_type {
    UDCM_MESSAGE_CONNECT    = 100,
    UDCM_MESSAGE_COMPLETE   = 101,
    UDCM_MESSAGE_REJECT     = 102,
#if HAVE_XRC
    UDCM_MESSAGE_XCONNECT   = 103,
    UDCM_MESSAGE_XRESPONSE  = 104,
    UDCM_MESSAGE_XCONNECT2  = 105,
    UDCM_MESSAGE_XRESPONSE2 = 106,
#endif
    UDCM_MESSAGE_ACK        = 107
} udcm_message_type_t;

typedef enum {
    UDCM_REJ_REMOTE_ERROR       = -1,
    UDCM_REJ_ALREADY_CONNECTED  = -2,
#if HAVE_XRC
    UDCM_REJ_NOT_READY          = -3,
#endif
} udcm_reject_reason_t;

typedef struct udcm_msg_hdr {
    uint8_t type;

    /* ack context */
    uintptr_t rem_ctx;

    /* endpoint local to the sender */
    mca_btl_base_endpoint_t *rem_ep;
    /* endpoint local to the receiver */
    mca_btl_base_endpoint_t *lcl_ep;

    union {
        /* UDCM_MESSAGE_CONNECT */
        struct msg_connect {
            int32_t rem_ep_index;
            uint8_t rem_port_num;
        } req;
        /* UDCM_MESSAGE_REJECT */
        struct msg_reject {
            int32_t reason;
        } rej;
#if HAVE_XRC
        /* UDCM_MESSAGE_XCONNECT, UDCM_MESSAGE_XCONNECT2 */
        struct msg_xrc_connect {
            int32_t  rem_ep_index;
            uint8_t  rem_port_num;
            uint32_t rem_qp_num;
            uint32_t rem_psn;
        } xreq;
        /* UDCM_MESSAGE_XRESPONSE */
        struct msg_xrc_response {
            int32_t  rem_ep_index;
            uint32_t rem_qp_num;
            uint32_t rem_psn;
        } xres;
#endif
    } data;
} udcm_msg_hdr_t;

typedef struct udcm_msg_t {
    udcm_msg_hdr_t hdr;

    /* If the message type is UDCM_MESSAGE_CONNECT,
       UDCM_MESSAGE_XRESPONSE, or UDCM_MESSAGE_XRESPONSE2
       then queue pair/srq data will follow the header */
    udcm_qp_t qps[];
} udcm_msg_t;

typedef struct udcm_message_recv {
    opal_list_item_t super;

    udcm_msg_hdr_t msg_hdr;
} udcm_message_recv_t;

static OBJ_CLASS_INSTANCE(udcm_message_recv_t, opal_list_item_t,
                          NULL, NULL);

typedef struct udcm_message_sent {
    opal_list_item_t         super;

    udcm_msg_t              *data;
    size_t                   length;
    mca_btl_base_endpoint_t *endpoint;

    int                      tries;
    opal_event_t             event;
    bool                     event_active;
} udcm_message_sent_t;

static void udcm_sent_message_constructor (udcm_message_sent_t *);
static void udcm_sent_message_destructor (udcm_message_sent_t *);
static OBJ_CLASS_INSTANCE(udcm_message_sent_t, opal_list_item_t,
                          udcm_sent_message_constructor,
                          udcm_sent_message_destructor);

#define UDCM_ENDPOINT_MODULE(ep) ((udcm_module_t *)(ep)->endpoint_local_cpc)
#define UDCM_ENDPOINT_DATA(ep) ((udcm_endpoint_t *)(ep)->endpoint_local_cpc_data)
#define UDCM_ENDPOINT_REM_MODEX(ep)                                        \
    (((modex_msg_t *)(ep)->endpoint_remote_cpc_data->cbm_modex_message))

/*--------------------------------------------------------------------*/

static void udcm_component_register(void);
static int udcm_component_query(mca_btl_openib_module_t *btl, 
                                opal_btl_openib_connect_base_module_t **cpc);
static int udcm_component_finalize(void);

/* Module methods */
static int udcm_endpoint_init(struct mca_btl_base_endpoint_t *lcl_ep);
static int udcm_module_start_connect(opal_btl_openib_connect_base_module_t *cpc,
                                     mca_btl_base_endpoint_t *lcl_ep);
static int udcm_endpoint_finalize(struct mca_btl_base_endpoint_t *lcl_ep);
static int udcm_endpoint_init_data (mca_btl_base_endpoint_t *lcl_ep);
static int udcm_rc_qp_create_all (mca_btl_base_endpoint_t *lcl_ep);
static int udcm_module_finalize(mca_btl_openib_module_t *btl,
                                opal_btl_openib_connect_base_module_t *cpc);

static void *udcm_cq_event_dispatch(int fd, int flags, void *context);
static void *udcm_message_callback (void *context);

static void udcm_set_message_timeout (udcm_message_sent_t *message);
static void udcm_cancel_message_timeout (udcm_message_sent_t *message);

static int udcm_module_init (udcm_module_t *m, mca_btl_openib_module_t *btl);

static int udcm_module_create_listen_qp (udcm_module_t *m);
static void udcm_module_destroy_listen_qp (udcm_module_t *m);

static int udcm_module_allocate_buffers (udcm_module_t *m);
static void udcm_module_destroy_buffers (udcm_module_t *m);

static int udcm_module_post_all_recvs (udcm_module_t *m);

static int udcm_send_request (mca_btl_base_endpoint_t *lcl_ep,
                              mca_btl_base_endpoint_t *rem_ep);

static void udcm_send_timeout (evutil_socket_t fd, short event, void *arg);
static int udcm_finish_connection (mca_btl_openib_endpoint_t *lcl_ep);
static int udcm_rc_qps_to_rts(mca_btl_openib_endpoint_t *lcl_ep);

/* XRC support */
#if HAVE_XRC
static int udcm_xrc_start_connect (opal_btl_openib_connect_base_module_t *cpc,
                                  mca_btl_base_endpoint_t *lcl_ep);
static int udcm_xrc_restart_connect (mca_btl_base_endpoint_t *lcl_ep);
static int udcm_xrc_send_qp_connect (mca_btl_openib_endpoint_t *lcl_ep, uint32_t rem_qp_num, uint32_t rem_psn);
static int udcm_xrc_send_qp_create (mca_btl_base_endpoint_t *lcl_ep);
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
static int udcm_xrc_recv_qp_connect (mca_btl_openib_endpoint_t *lcl_ep, uint32_t qp_num);
#else
static int udcm_xrc_recv_qp_connect (mca_btl_openib_endpoint_t *lcl_ep);
#endif
static int udcm_xrc_recv_qp_create (mca_btl_openib_endpoint_t *lcl_ep, uint32_t rem_qp_num, uint32_t rem_psn);
static int udcm_xrc_send_request (mca_btl_base_endpoint_t *lcl_ep, mca_btl_base_endpoint_t *rem_ep,
                                  uint8_t msg_type);
static int udcm_xrc_send_xresponse (mca_btl_base_endpoint_t *lcl_ep, mca_btl_base_endpoint_t *rem_ep,
                                    uint8_t msg_type);
static int udcm_xrc_handle_xconnect (mca_btl_openib_endpoint_t *lcl_ep, udcm_msg_hdr_t *msg_hdr);
static int udcm_xrc_handle_xresponse (mca_btl_openib_endpoint_t *lcl_ep, udcm_msg_hdr_t *msg_hdr);
#endif


/*--------------------------------------------------------------------*/

#define UDCM_MIN_RECV_COUNT 512
#define UDCM_MIN_TIMEOUT    500000

#define UDCM_SEND_CQ_SIZE   512

#define UDCM_WR_RECV_ID  0x20000000ll
#define UDCM_WR_SEND_ID  0x10000000ll
#define UDCM_WR_ACK_ID   0x10000000ll
#define UDCM_WR_DIR_MASK 0x30000000ll

/* Useless 40 bytes of data that proceeds received scatter gather data.
   Can we get rid of this? */
#define UDCM_GRH_SIZE (sizeof (struct ibv_grh))

/* Priority of this connection module */
static int udcm_priority;

/* Number of receive work requests to post */
static int udcm_recv_count;
static int udcm_max_retry;

/* Message ACK timeout in usec */
static int udcm_timeout;

/* seed for rand_r. remove me when opal gets a random number generator */
/* Uses the OPAL ALFG RNG */
static uint32_t udcm_random_seed = 0;
static opal_rng_buff_t udcm_rand_buff;

static struct timeval udcm_timeout_tv;

/*******************************************************************
 * Component
 *******************************************************************/

/* mark: udcm component */

opal_btl_openib_connect_base_component_t opal_btl_openib_connect_udcm = {
    "udcm",
    udcm_component_register,
    NULL,
    udcm_component_query,
    udcm_component_finalize
};

static void udcm_component_register(void)
{
    /* the priority is initialized in the declaration above */
    udcm_priority = 63;
    (void) mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                           "connect_udcm_priority", "Priority of the udcm "
                                           "connection method", MCA_BASE_VAR_TYPE_INT, NULL,
                                           0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &udcm_priority);

    udcm_recv_count = UDCM_MIN_RECV_COUNT;
    (void) mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                           "connect_udcm_recv_count", "Number of registered "
                                           "buffers to post", MCA_BASE_VAR_TYPE_INT, NULL,
                                           0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &udcm_recv_count);

    udcm_timeout = UDCM_MIN_TIMEOUT;
    (void) mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                           "connect_udcm_timeout", "Ack timeout for udcm "
                                           "connection messages", MCA_BASE_VAR_TYPE_INT, NULL,
                                           0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &udcm_timeout);

    udcm_max_retry = 25;
    (void) mca_base_component_var_register(&mca_btl_openib_component.super.btl_version,
                                           "connect_udcm_max_retry", "Maximum number of times "
                                           "to retry sending a udcm connection message",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &udcm_max_retry);
}

static int udcm_component_query(mca_btl_openib_module_t *btl, 
                                opal_btl_openib_connect_base_module_t **cpc)
{
    udcm_module_t *m = NULL;
    int rc = OPAL_ERR_NOT_SUPPORTED;

    do {
        /* If we do not have struct ibv_device.transport_device, then
           we're in an old version of OFED that is IB only (i.e., no
           iWarp), so we can safely assume that we can use this CPC. */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE) && HAVE_DECL_IBV_LINK_LAYER_ETHERNET
        if (BTL_OPENIB_CONNECT_BASE_CHECK_IF_NOT_IB(btl)) {
            BTL_VERBOSE(("UD CPC only supported on InfiniBand; skipped on %s:%d",
                         ibv_get_device_name(btl->device->ib_dev),
                         btl->port_num));
            break;
        }
#endif

        /* Allocate the module struct.  Use calloc so that it's safe to
           finalize the module if something goes wrong. */
        m = calloc(1, sizeof(*m));
        if (NULL == m) {
            BTL_ERROR(("malloc failed!"));
            rc = OPAL_ERR_OUT_OF_RESOURCE;
            break;
        }

        if (udcm_priority > 100) {
            udcm_priority = 100;
        } else if (udcm_priority < 0) {
            udcm_priority = 0;
        }

        if (UDCM_MIN_RECV_COUNT > udcm_recv_count) {
            udcm_recv_count = UDCM_MIN_RECV_COUNT;
        }

        if (UDCM_MIN_TIMEOUT > udcm_timeout) {
            udcm_timeout = UDCM_MIN_TIMEOUT;
        }

        rc = udcm_module_init (m, btl);
        if (OPAL_SUCCESS != rc) {
            break;
        }

        /* seed the random number generator */
        udcm_random_seed = time (NULL);
        opal_srand(&udcm_rand_buff,udcm_random_seed);
        /* All done */
        *cpc = (opal_btl_openib_connect_base_module_t *) m;
        BTL_VERBOSE(("available for use on %s:%d",
                     ibv_get_device_name(btl->device->ib_dev),
                     btl->port_num));

        return OPAL_SUCCESS;
    } while (0);

    udcm_module_finalize(btl, (opal_btl_openib_connect_base_module_t *) m);
    if (OPAL_ERR_NOT_SUPPORTED == rc) {
        BTL_VERBOSE(("unavailable for use on %s:%d; skipped",
                     ibv_get_device_name(btl->device->ib_dev),
                     btl->port_num));
    } else {
        BTL_VERBOSE(("unavailable for use on %s:%d; fatal error %d (%s)",
                     ibv_get_device_name(btl->device->ib_dev), 
                     btl->port_num, rc, 
                     opal_strerror(rc)));
    }

    return rc;
}

static int udcm_component_finalize(void)
{
    return OPAL_SUCCESS;
}

/*--------------------------------------------------------------------*/

/*******************************************************************
 * Module
 *******************************************************************/

/* mark: udcm module */

#if HAVE_XRC
static int udcm_endpoint_init_self_xrc (struct mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    int rc;

    opal_mutex_lock (&udep->udep_lock);
    do {
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
        rc = udcm_xrc_recv_qp_connect (lcl_ep, lcl_ep->qps[0].qp->lcl_qp->qp_num);
#else
        lcl_ep->xrc_recv_qp_num = lcl_ep->qps[0].qp->lcl_qp->qp_num;
        rc = udcm_xrc_recv_qp_connect (lcl_ep);
#endif
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("error connecting loopback XRC receive queue pair"));
            break;
        }

        rc = mca_btl_openib_endpoint_post_recvs (lcl_ep);
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("error posting receives for loopback queue pair"));
            break;
        }

        rc = udcm_xrc_recv_qp_create (lcl_ep, lcl_ep->qps[0].qp->lcl_qp->qp_num,
                                      lcl_ep->qps[0].qp->lcl_psn);
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("error creating loopback XRC receive queue pair"));
            break;
        }

        rc = udcm_xrc_send_qp_connect (lcl_ep, lcl_ep->qps[0].qp->lcl_qp->qp_num,
                                       lcl_ep->qps[0].qp->lcl_psn);
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("error creating loopback XRC send queue pair"));
            break;
        }

        lcl_ep->endpoint_state = MCA_BTL_IB_CONNECTED;

        rc = udcm_finish_connection (lcl_ep);
    } while (0);
    opal_mutex_unlock (&udep->udep_lock);

    return rc;
}
#endif

static int udcm_endpoint_init_self (struct mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    int rc;

    opal_mutex_lock (&udep->udep_lock);
    do {
        if (OPAL_SUCCESS != (rc = udcm_endpoint_init_data (lcl_ep))) {
            BTL_VERBOSE(("error initializing loopback endpoint cpc data"));
            break;
        }

        if (OPAL_SUCCESS != (rc = udcm_rc_qp_create_all (lcl_ep))) {
            BTL_VERBOSE(("error initializing loopback endpoint qps"));
            break;
        }

        /* save queue pair info */
        lcl_ep->rem_info.rem_index = lcl_ep->index;

        for (int i = 0 ; i <  mca_btl_openib_component.num_qps ; ++i) {
            lcl_ep->rem_info.rem_qps[i].rem_psn = lcl_ep->qps[i].qp->lcl_psn;
            lcl_ep->rem_info.rem_qps[i].rem_qp_num = lcl_ep->qps[i].qp->lcl_qp->qp_num;
        }

        if (OPAL_SUCCESS != (rc = udcm_rc_qps_to_rts (lcl_ep))) {
            BTL_VERBOSE(("error moving loopback endpoint qps to RTS"));
            break;
        }

        lcl_ep->endpoint_state = MCA_BTL_IB_CONNECTED;

        rc = udcm_finish_connection (lcl_ep);

        return OPAL_SUCCESS;
    } while (0);
    opal_mutex_unlock (&udep->udep_lock);

    return rc;
}

static int udcm_endpoint_init (struct mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = lcl_ep->endpoint_local_cpc_data = 
        calloc(1, sizeof(udcm_endpoint_t));
    if (NULL == udep) {
        BTL_ERROR(("malloc failed!"));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    OBJ_CONSTRUCT(&udep->udep_lock, opal_mutex_t);

    if (lcl_ep->endpoint_proc->proc_opal == opal_proc_local_get ()) {
        /* go ahead and try to create a loopback queue pair */
#if HAVE_XRC
        if (mca_btl_openib_component.num_xrc_qps > 0) {
            return udcm_endpoint_init_self_xrc (lcl_ep);
        } else
#endif
            return udcm_endpoint_init_self (lcl_ep);
    }

    return OPAL_SUCCESS;
}

static int udcm_endpoint_finalize(struct mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    
    /* Free the stuff we allocated in udcm_endpoint_init */
    if (NULL != udep) {
        if (udep->ah) {
            ibv_destroy_ah(udep->ah);
        }

        OBJ_DESTRUCT(&udep->udep_lock);

        free(lcl_ep->endpoint_local_cpc_data);
        lcl_ep->endpoint_local_cpc_data = NULL;
    }

    return OPAL_SUCCESS;
}

static int udcm_module_init (udcm_module_t *m, mca_btl_openib_module_t *btl)
{
    int rc = OPAL_ERR_NOT_SUPPORTED;

    BTL_VERBOSE(("created cpc module %p for btl %p",
                 (void*)m, (void*)btl));

    m->btl = btl;

    /* Create completion channel */
    m->cm_channel = ibv_create_comp_channel (btl->device->ib_dev_context);
    if (NULL == m->cm_channel) {
        BTL_VERBOSE(("error creating ud completion channel"));
        return OPAL_ERR_NOT_SUPPORTED;
    }

    /* Create completion queues */
    m->cm_recv_cq = ibv_create_cq (btl->device->ib_dev_context,
                                   udcm_recv_count, NULL,
                                   m->cm_channel, 0);
    if (NULL == m->cm_recv_cq) {
        BTL_VERBOSE(("error creating ud recv completion queue"));
        return OPAL_ERR_NOT_SUPPORTED;
    }

    m->cm_send_cq = ibv_create_cq (btl->device->ib_dev_context,
                                   UDCM_SEND_CQ_SIZE, NULL, NULL, 0);
    if (NULL == m->cm_send_cq) {
        BTL_VERBOSE(("error creating ud send completion queue"));
        return OPAL_ERR_NOT_SUPPORTED;
    }

    if (0 != (rc = udcm_module_allocate_buffers (m))) {
        BTL_VERBOSE(("error allocating cm buffers"));
        return rc;
    }

    if (0 != (rc = udcm_module_create_listen_qp (m))) {
        BTL_VERBOSE(("error creating UD QP"));
        return rc;
    }

    if (0 != (rc = udcm_module_post_all_recvs (m))) {
        BTL_VERBOSE(("error posting receives"));
        return rc;
    }

    /* UD CM initialized properly.  So fill in the rest of the CPC
       module. */
    m->cpc.data.cbm_component = &opal_btl_openib_connect_udcm;
    m->cpc.data.cbm_priority = udcm_priority;
    m->cpc.data.cbm_modex_message = &m->modex;

    /* Initialize module modex */
    m->modex.mm_lid         = btl->lid;
    m->modex.mm_port_num    = btl->port_num;
    m->modex.mm_qp_num      = m->listen_qp->qp_num;

    BTL_VERBOSE(("my modex = LID: %d, Port: %d, QPN: %d",
                 m->modex.mm_lid, m->modex.mm_port_num,
                 m->modex.mm_qp_num));

    m->cpc.data.cbm_modex_message_len = sizeof(m->modex);

    /* Initialize module */
    m->cpc.cbm_endpoint_init     = udcm_endpoint_init;
    m->cpc.cbm_start_connect     = udcm_module_start_connect;
    m->cpc.cbm_endpoint_finalize = udcm_endpoint_finalize;
    m->cpc.cbm_finalize          = udcm_module_finalize;

    m->cpc.cbm_uses_cts = false;

    m->cm_exiting = false;

    /* Monitor the fd associated with the completion channel */
    opal_btl_openib_fd_monitor(m->cm_channel->fd, OPAL_EV_READ,
                               udcm_cq_event_dispatch, m);
    m->channel_monitored = true;

    OBJ_CONSTRUCT(&m->cm_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&m->cm_send_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&m->cm_recv_msg_queue, opal_list_t);
    OBJ_CONSTRUCT(&m->cm_recv_msg_queue_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&m->flying_messages, opal_list_t);

    OBJ_CONSTRUCT(&m->cm_timeout_lock, opal_mutex_t);

    udcm_timeout_tv.tv_sec  = udcm_timeout / 1000000;
    udcm_timeout_tv.tv_usec = udcm_timeout - 1000000 *
        udcm_timeout_tv.tv_sec;

    m->cm_message_event_active = false;

    /* Finally, request CQ notification */
    if (0 != ibv_req_notify_cq (m->cm_recv_cq, 0)) {
        BTL_VERBOSE(("error requesting recv completions"));
        return OPAL_ERROR;
    }

    /* Ready to use */

    return OPAL_SUCCESS;
}

static int
udcm_module_start_connect(opal_btl_openib_connect_base_module_t *cpc,
                          mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    int rc = OPAL_SUCCESS;

    BTL_VERBOSE(("endpoint %p (lid %d, ep index %d)", 
                 (void*)lcl_ep, lcl_ep->endpoint_btl->port_info.lid,
                 lcl_ep->index));

#if HAVE_XRC
    if (mca_btl_openib_component.num_xrc_qps > 0) {
        return udcm_xrc_start_connect (cpc, lcl_ep);
    }
#endif


    opal_mutex_lock (&udep->udep_lock);

    if (MCA_BTL_IB_CLOSED != lcl_ep->endpoint_state) {
        opal_mutex_unlock (&udep->udep_lock);
        BTL_VERBOSE(("already ongoing %p. state = %d",
                     (void *) lcl_ep, lcl_ep->endpoint_state));
        return OPAL_SUCCESS;
    }

    do {
        opal_atomic_wmb ();

        lcl_ep->endpoint_state = MCA_BTL_IB_CONNECTING;

        if (OPAL_SUCCESS != (rc = udcm_endpoint_init_data (lcl_ep))) {
            BTL_VERBOSE(("error initializing endpoint cpc data"));
            break;
        }

        if (OPAL_SUCCESS != (rc = udcm_rc_qp_create_all (lcl_ep))) {
            BTL_VERBOSE(("error initializing endpoint qps"));
            break;
        }

        rc = udcm_send_request (lcl_ep, NULL);
    } while (0);

    opal_mutex_unlock (&udep->udep_lock);

    return rc;
}

static void *udcm_unmonitor(int fd, int flags, void *context)
{
    volatile int *barrier = (volatile int *)context;

    *barrier = 1;

    return NULL;
}

static int udcm_module_finalize(mca_btl_openib_module_t *btl,
                                opal_btl_openib_connect_base_module_t *cpc)
{
    udcm_module_t *m = (udcm_module_t *) cpc;
    opal_list_item_t *item;
    volatile int barrier = 0;

    if (NULL == m) {
        return OPAL_SUCCESS;
    }

    m->cm_exiting = true;

    if (m->channel_monitored) {
        /* stop monitoring the channel's fd before destroying the listen qp */
        opal_btl_openib_fd_unmonitor(m->cm_channel->fd, udcm_unmonitor, (void *)&barrier);

        while (0 == barrier) {
            sched_yield();
        }
    }

    opal_mutex_lock (&m->cm_lock);

    opal_mutex_lock (&m->cm_recv_msg_queue_lock);

    /* clear message queue */
    while ((item = opal_list_remove_first(&m->cm_recv_msg_queue))) {
        OBJ_RELEASE(item);
    }

    opal_mutex_unlock (&m->cm_recv_msg_queue_lock);

    OBJ_DESTRUCT(&m->cm_recv_msg_queue);

    opal_mutex_lock (&m->cm_timeout_lock);
    while ((item = opal_list_remove_first(&m->flying_messages))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&m->flying_messages);
    opal_mutex_unlock (&m->cm_timeout_lock);

    BTL_VERBOSE(("destroying listing thread"));

    /* destroy the listen queue pair. this will cause ibv_get_cq_event to
       return. */
    udcm_module_destroy_listen_qp (m);

    udcm_module_destroy_buffers (m);

    if (m->cm_send_cq) {
        if (0 != ibv_destroy_cq (m->cm_send_cq)) {
            BTL_VERBOSE(("failed to destroy send CQ. errno = %d",
                         errno));
        }
    }

    if (m->cm_recv_cq) {
        if (0 != ibv_destroy_cq (m->cm_recv_cq)) {
            BTL_VERBOSE(("failed to destroy recv CQ. errno = %d",
                         errno));
        }
    }

    if (m->cm_channel) {
        if (0 != ibv_destroy_comp_channel (m->cm_channel)) {
            BTL_VERBOSE(("failed to completion channel. errno = %d",
                         errno));
        }

        m->cm_channel = NULL;
    }

    opal_mutex_unlock (&m->cm_lock);
    OBJ_DESTRUCT(&m->cm_send_lock);
    OBJ_DESTRUCT(&m->cm_lock);
    OBJ_DESTRUCT(&m->cm_recv_msg_queue_lock);
    OBJ_DESTRUCT(&m->cm_timeout_lock);

    return OPAL_SUCCESS;
}

/*--------------------------------------------------------------------*/

static int udcm_module_create_listen_qp (udcm_module_t *m)
{
    struct ibv_qp_init_attr init_attr;
    struct ibv_qp_attr attr;
    struct ibv_qp *qp;

    BTL_VERBOSE(("creating listen QP on port %d", m->btl->port_num));

    /* create the UD keypair */
    memset(&init_attr, 0, sizeof(init_attr));

    init_attr.qp_type = IBV_QPT_UD;

    init_attr.send_cq = m->cm_send_cq;
    init_attr.recv_cq = m->cm_recv_cq;

    init_attr.cap.max_send_sge    = 1;
    init_attr.cap.max_recv_sge    = 1;

    init_attr.cap.max_recv_wr = udcm_recv_count;
    init_attr.cap.max_send_wr = 1;

    qp = ibv_create_qp(m->btl->device->ib_pd, &init_attr); 
    if (NULL == qp) {
        BTL_VERBOSE(("could not create UD listen queue pair"));
        return OPAL_ERROR;
    }
    /* end: create the UD queue pair */

    /* move the UD QP into the INIT state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state   = IBV_QPS_INIT;
    attr.pkey_index = m->btl->pkey_index;
    attr.port_num   = m->btl->port_num;
    attr.qkey       = 0;

    if (0 != ibv_modify_qp(qp, &attr, IBV_QP_STATE | IBV_QP_PKEY_INDEX
                           | IBV_QP_PORT | IBV_QP_QKEY)) {
        BTL_ERROR(("error modifying qp to INIT errno says %s",
                   strerror(errno)));
        return OPAL_ERROR;
    } 

    /* Move listen QP to RTR */
    attr.qp_state = IBV_QPS_RTR;

    if (0 != ibv_modify_qp(qp, &attr, IBV_QP_STATE)) {
        BTL_ERROR(("error modifing QP to RTR errno says %s",
                   strerror(errno)));
        return OPAL_ERROR;
    }


    /* Move listen QP to RTS */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_RTS;
    attr.sq_psn = 0;

    if (0 != ibv_modify_qp(qp, &attr, IBV_QP_STATE | IBV_QP_SQ_PSN)) {
        BTL_ERROR(("error modifing QP to RTS errno says %s; errno=%d",
                   strerror(errno), errno));
        return OPAL_ERROR;
    }

    m->listen_qp = qp;

    BTL_VERBOSE(("listening for connections on lid %d, qpn %d",
                 m->btl->lid, qp->qp_num));

    return OPAL_SUCCESS;
}

static void udcm_module_destroy_listen_qp (udcm_module_t *m)
{
    struct ibv_qp_attr attr;
    struct ibv_wc wc;

    if (NULL == m->listen_qp) {
        return;
    }

    if (mca_btl_openib_component.use_async_event_thread &&
        -1 != mca_btl_openib_component.async_pipe[1]) {
        /* Tell the openib async thread to ignore ERR state on the QP
           we are about to manually set the ERR state on */
        mca_btl_openib_async_cmd_t async_command;
        async_command.a_cmd = OPENIB_ASYNC_IGNORE_QP_ERR;
        async_command.qp = m->listen_qp;
        if (write(mca_btl_openib_component.async_pipe[1],
                  &async_command, sizeof(mca_btl_openib_async_cmd_t))<0){
            BTL_ERROR(("Failed to write to pipe [%d]",errno));
            return;
        }
        /* wait for ok from thread */
        if (OPAL_SUCCESS !=
                        btl_openib_async_command_done(OPENIB_ASYNC_IGNORE_QP_ERR)) {
            BTL_ERROR(("Command to openib async thread to ignore QP ERR state failed"));
        }
    }

    do {
        /* Move listen QP into the ERR state to cancel all outstanding
           work requests */
        memset(&attr, 0, sizeof(attr));
        attr.qp_state = IBV_QPS_ERR;
        attr.sq_psn = 0;

        BTL_VERBOSE(("Setting qp to err state %p", (void *)m->listen_qp));

        if (0 != ibv_modify_qp(m->listen_qp, &attr, IBV_QP_STATE)) {
            BTL_VERBOSE(("error modifying qp to ERR. errno = %d",
                         errno));
            break;
        }

        while (ibv_poll_cq (m->cm_recv_cq, 1, &wc) > 0);

        /* move the QP into the RESET state */
        memset(&attr, 0, sizeof(attr));
        attr.qp_state = IBV_QPS_RESET;

        if (0 != ibv_modify_qp(m->listen_qp, &attr, IBV_QP_STATE)) {
            BTL_VERBOSE(("error modifying qp to RESET. errno = %d",
                         errno));
            break;
        }
    } while (0);

    if (0 != ibv_destroy_qp (m->listen_qp)) {
        BTL_VERBOSE(("error destroying listen qp. errno = %d",
                     errno));
    }

    m->listen_qp = NULL;
}

static int udcm_module_allocate_buffers (udcm_module_t *m)
{
    size_t total_size;

    m->msg_length   = sizeof (udcm_msg_hdr_t) +
        mca_btl_openib_component.num_qps * sizeof (udcm_qp_t);

    total_size = (udcm_recv_count + 1) * (m->msg_length +
                                          UDCM_GRH_SIZE);

    m->cm_buffer = NULL;
    posix_memalign ((void **)&m->cm_buffer, (size_t)opal_getpagesize(),
                    total_size);
    if (NULL == m->cm_buffer) {
        BTL_ERROR(("malloc failed! errno = %d", errno));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* mark buffer memory as initialized for valgrind's sake */
    memset (m->cm_buffer, 0, total_size);

    m->cm_mr = ibv_reg_mr (m->btl->device->ib_pd, m->cm_buffer,
                           total_size, IBV_ACCESS_LOCAL_WRITE |
                           IBV_ACCESS_REMOTE_WRITE);
    if (NULL == m->cm_mr) {
        BTL_ERROR(("failed to register memory. errno = %d", errno));
        return OPAL_ERROR;
    }

    m->cm_send_buffer = m->cm_buffer + ((UDCM_GRH_SIZE +
                                         m->msg_length) *
                                         udcm_recv_count);

    return 0;
}

static void udcm_module_destroy_buffers (udcm_module_t *m)
{
    if (m->cm_mr) {
        if (0 != ibv_dereg_mr (m->cm_mr)) {
            BTL_VERBOSE(("failed to deregister memory. errno = %d",
                         errno));
        }
        m->cm_mr = NULL;
    }

    if (m->cm_buffer) {
        free (m->cm_buffer);
    }
}

static inline char *udcm_module_get_recv_buffer (udcm_module_t *m,
                                                 int msg_num, bool skip_grh)
{
    return m->cm_buffer + msg_num * (m->msg_length + UDCM_GRH_SIZE) +
    skip_grh * UDCM_GRH_SIZE;
}

static inline char *udcm_module_get_send_buffer (udcm_module_t *m)
{
    return m->cm_send_buffer;
}

static int udcm_module_post_one_recv (udcm_module_t *m, int msg_num)
{
    char *recv_buffer = udcm_module_get_recv_buffer (m, msg_num, 0);
    struct ibv_recv_wr wr, *bad_wr;
    struct ibv_sge sge;
    int rc;

    /* GRH + request data*/
    sge.addr   = (uintptr_t) recv_buffer;
    sge.length = UDCM_GRH_SIZE + m->msg_length;
    sge.lkey   = m->cm_mr->lkey;

    wr.next    = NULL;
    wr.wr_id   = UDCM_WR_RECV_ID | (uint64_t)msg_num;
    wr.sg_list = &sge;
    wr.num_sge = 1;

    rc = ibv_post_recv (m->listen_qp, &wr, &bad_wr);
    if (0 != rc) {
        BTL_VERBOSE(("error posting receive. errno = %d", errno));
    }

    return (0 == rc) ? OPAL_SUCCESS : OPAL_ERROR;
}

static int udcm_module_post_all_recvs (udcm_module_t *m)
{
    int i, rc;

    for (i = 0 ; i < udcm_recv_count ; ++i) {
        if (0 != (rc = udcm_module_post_one_recv (m, i))) {
            return rc;
        }
    }

    return 0;
}


/*--------------------------------------------------------------------*/

/* mark: helper functions */

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

/* Using OPAL's Additive Lagged Fibonacci RNG */
static inline uint32_t udcm_random (void)
{
    return opal_rand(&udcm_rand_buff);
}

/* mark: rc helper functions */

static inline int udcm_rc_qp_to_init (struct ibv_qp *qp,
                                      mca_btl_openib_module_t *btl)
{
    enum ibv_qp_attr_mask attr_mask;
    struct ibv_qp_attr attr;

    memset(&attr, 0, sizeof(attr));
    attr.qp_state        = IBV_QPS_INIT;
    attr.pkey_index      = btl->pkey_index;
    attr.port_num        = btl->port_num;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;
#if HAVE_DECL_IBV_ATOMIC_HCA
    attr.qp_access_flags |= IBV_ACCESS_REMOTE_ATOMIC;
#endif
    attr_mask = IBV_QP_STATE | IBV_QP_PKEY_INDEX | IBV_QP_PORT |
        IBV_QP_ACCESS_FLAGS;

    if (0 != ibv_modify_qp(qp, &attr, attr_mask)) {
        BTL_ERROR(("error modifying qp to INIT errno says %s",
                   strerror(errno)));
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static inline int udcm_rc_qp_to_rtr (mca_btl_base_endpoint_t *lcl_ep,
                                     int qp_index)
{
    struct ibv_qp *qp = lcl_ep->qps[qp_index].qp->lcl_qp;
    mca_btl_openib_module_t *btl = lcl_ep->endpoint_btl;
    struct ibv_qp_attr attr;
    enum ibv_mtu mtu;
    int rc;

    mtu = (btl->device->mtu < lcl_ep->rem_info.rem_mtu) ?
        btl->device->mtu : lcl_ep->rem_info.rem_mtu;

    /* Move the QP into the RTR state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state           = IBV_QPS_RTR;
    /* Setup attributes */
    attr.path_mtu           = mtu;
    attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
    attr.min_rnr_timer      = mca_btl_openib_component.ib_min_rnr_timer;
    attr.dest_qp_num        = lcl_ep->rem_info.rem_qps[qp_index].rem_qp_num;
    attr.rq_psn             = lcl_ep->rem_info.rem_qps[qp_index].rem_psn;

    attr.ah_attr.is_global     = 0;
    attr.ah_attr.dlid          = lcl_ep->rem_info.rem_lid;
    attr.ah_attr.src_path_bits = btl->src_path_bits;
    attr.ah_attr.port_num      = btl->port_num;
    attr.ah_attr.sl            = mca_btl_openib_component.ib_service_level;
    attr.ah_attr.static_rate   = 0;

#if (ENABLE_DYNAMIC_SL)
    /* if user enabled dynamic SL, get it from PathRecord */
    if (0 != mca_btl_openib_component.ib_path_record_service_level) {
        int rc = btl_openib_connect_get_pathrecord_sl(qp->context,
                                                      attr.ah_attr.port_num,
                                                      btl->lid,
                                                      attr.ah_attr.dlid);
        if (OPAL_ERROR == rc) {
            return OPAL_ERROR;
        }
        attr.ah_attr.sl = rc;
    }
#endif

    rc = ibv_modify_qp(qp, &attr, IBV_QP_STATE | IBV_QP_PATH_MTU |
                       IBV_QP_MAX_DEST_RD_ATOMIC | IBV_QP_MIN_RNR_TIMER |
                       IBV_QP_RQ_PSN | IBV_QP_AV | IBV_QP_DEST_QPN);
    if (OPAL_UNLIKELY(0 != rc)) {
        BTL_ERROR(("error modifing QP to RTR errno says %s", strerror(errno)));
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static inline int udcm_rc_qp_to_rts (mca_btl_base_endpoint_t *lcl_ep,
                                     int qp_index)
{
    struct ibv_qp *qp = lcl_ep->qps[qp_index].qp->lcl_qp;
    struct ibv_qp_attr attr;
    int rc;

    BTL_VERBOSE(("transitioning QP %p to RTS", (void *)qp));

    /* Move the QP into the RTS state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state       = IBV_QPS_RTS;
    attr.timeout        = mca_btl_openib_component.ib_timeout;
    attr.retry_cnt      = mca_btl_openib_component.ib_retry_count;
    /* On PP QPs we have SW flow control, no need for rnr retries. Setting
     * it to zero helps to catch bugs */
    attr.rnr_retry      = BTL_OPENIB_QP_TYPE_PP(qp_index) ? 0 :
        mca_btl_openib_component.ib_rnr_retry;
    attr.sq_psn         = lcl_ep->qps[qp_index].qp->lcl_psn;
    attr.max_rd_atomic  = mca_btl_openib_component.ib_max_rdma_dst_ops;

    rc = ibv_modify_qp(qp, &attr, IBV_QP_STATE | IBV_QP_TIMEOUT |
                       IBV_QP_RETRY_CNT | IBV_QP_RNR_RETRY | IBV_QP_SQ_PSN |
                       IBV_QP_MAX_QP_RD_ATOMIC);
    if (OPAL_UNLIKELY(0 != rc)) {
        BTL_ERROR(("error modifing QP %p to RTS errno says %s",
                   (void *) qp, strerror(errno)));
        return OPAL_ERROR;
    }

    BTL_VERBOSE(("successfully set RTS"));

    return OPAL_SUCCESS;
}

/*--------------------------------------------------------------------*/

/*
 * We have received information about the remote peer's QP; move the
 * local QP from INIT to RTS through RTR.
 */
static int udcm_rc_qps_to_rts(mca_btl_openib_endpoint_t *lcl_ep)
{
    int rc;

    for (int qp = 0 ; qp < mca_btl_openib_component.num_qps ; ++qp) {
        if (lcl_ep->qps[qp].qp->lcl_qp->state == IBV_QPS_RTS) {
            continue;
        }

        rc = udcm_rc_qp_to_rtr (lcl_ep, qp);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_VERBOSE(("failed moving QP to RTR"));
            return rc;
        }

        rc = udcm_rc_qp_to_rts (lcl_ep, qp);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            BTL_VERBOSE(("failed moving QP to RTS"));
            return rc;
        }
    }

    /* Ensure that all the writes back to the endpoint and associated
     * data structures have completed */
    opal_atomic_wmb();
    mca_btl_openib_endpoint_post_recvs(lcl_ep);

    /* All done */
    return OPAL_SUCCESS;
}

/*
 * Create the local side of one qp.  The remote side will be connected
 * later.
 */
static int udcm_rc_qp_create_one(udcm_module_t *m, mca_btl_base_endpoint_t* lcl_ep,
                              int qp, struct ibv_srq *srq, uint32_t max_recv_wr,
                              uint32_t max_send_wr)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    struct ibv_qp_init_attr init_attr;
    size_t req_inline;
    int rc;

    memset(&init_attr, 0, sizeof(init_attr));

    init_attr.qp_type = IBV_QPT_RC;
    init_attr.send_cq = m->btl->device->ib_cq[BTL_OPENIB_LP_CQ];
    init_attr.recv_cq = m->btl->device->ib_cq[qp_cq_prio(qp)];
    init_attr.srq     = srq;
    init_attr.cap.max_inline_data = req_inline = 
        max_inline_size(qp, m->btl->device);
    init_attr.cap.max_send_sge = 1;
    init_attr.cap.max_recv_sge = 1; /* we do not use SG list */
    if(BTL_OPENIB_QP_TYPE_PP(qp)) {
        init_attr.cap.max_recv_wr  = max_recv_wr;
    } else {
        init_attr.cap.max_recv_wr  = 0;
    }
    init_attr.cap.max_send_wr  = max_send_wr;

    while (NULL == (lcl_ep->qps[qp].qp->lcl_qp = ibv_create_qp(m->btl->device->ib_pd,
                                                               &init_attr))) {
        /* NTH: this process may be out of registered memory. try evicting an item from
           the lru of this btl's mpool */
        if (false == mca_mpool_grdma_evict (m->btl->super.btl_mpool)) {
            break;
        }
    }

    if (NULL == lcl_ep->qps[qp].qp->lcl_qp) {
        opal_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "ibv_create_qp failed", true, opal_process_info.nodename,
                       ibv_get_device_name(m->btl->device->ib_dev),
                       "Reliable connected (RC)");

        return OPAL_ERROR;
    }

    if (init_attr.cap.max_inline_data < req_inline) {
        lcl_ep->qps[qp].ib_inline_max = init_attr.cap.max_inline_data;
        opal_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", true, opal_process_info.nodename,
                       ibv_get_device_name(m->btl->device->ib_dev),
                       m->btl->port_num, req_inline,
                       init_attr.cap.max_inline_data);
    } else {
        lcl_ep->qps[qp].ib_inline_max = req_inline;
    }

    /* Setup meta data on the endpoint */
    lcl_ep->qps[qp].qp->lcl_psn = udcm_random () & 0x00ffffff;
    lcl_ep->qps[qp].credit_frag = NULL;

    rc = udcm_rc_qp_to_init (lcl_ep->qps[qp].qp->lcl_qp, m->btl);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    /* If we have already received a request go ahead and move to
       RTS. */
    if (udep->recv_req) {
        rc = udcm_rc_qp_to_rtr (lcl_ep, qp);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            return rc;
        }

        return udcm_rc_qp_to_rts (lcl_ep, qp);
    }

    return OPAL_SUCCESS;
}

/*
 * Create the local side of all the qp's.  The remote sides will be
 * connected later.
 * NTH: This code is common to (and repeated by) all non-XRC cpcs.
 */
static int udcm_rc_qp_create_all (mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    int qp, rc, pp_qp_num = 0;
    int32_t rd_rsv_total = 0;

    if (udep->udep_created_qps)
        return OPAL_SUCCESS;

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) {
        if (BTL_OPENIB_QP_TYPE_PP(qp)) {
            rd_rsv_total +=
                mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv;
            pp_qp_num++;
        }
    }

    /* if there is no pp QPs we still need reserved WQE for eager rdma flow
     * control */
    if (0 == pp_qp_num && true == lcl_ep->use_eager_rdma) {
        pp_qp_num = 1;
    }

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) { 
        struct ibv_srq *srq = NULL;
        uint32_t max_recv_wr, max_send_wr;
        int32_t rd_rsv, rd_num_credits;

        /* QP used for SW flow control need some additional recourses */
        if (qp == mca_btl_openib_component.credits_qp) {
            rd_rsv = rd_rsv_total;
            rd_num_credits = pp_qp_num;
        } else {
            rd_rsv = rd_num_credits = 0;
        }

        if (BTL_OPENIB_QP_TYPE_PP(qp)) {
            max_recv_wr = mca_btl_openib_component.qp_infos[qp].rd_num + 
                rd_rsv;
            max_send_wr = mca_btl_openib_component.qp_infos[qp].rd_num +
                rd_num_credits;
        } else {
            srq = lcl_ep->endpoint_btl->qps[qp].u.srq_qp.srq;

            max_recv_wr = mca_btl_openib_component.qp_infos[qp].rd_num
                + rd_rsv;
            max_send_wr = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max
                + rd_num_credits;
        }

        /* Go create the actual qp */
        rc = udcm_rc_qp_create_one (m, lcl_ep, qp, srq, max_recv_wr, max_send_wr);
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("error creating qp %d for endpoint %p", qp, (void *) lcl_ep));
            return rc;
        }
    }

    /* All done! */
    udep->udep_created_qps = true;

    return OPAL_SUCCESS;
}

/* mark: endpoint helper functions */

/* JMS: optimization target -- can we send something in private
 data to find the proc directly instead of having to search
 through *all* procs? */
static mca_btl_openib_endpoint_t *udcm_find_endpoint (opal_pointer_array_t *endpoints,
                                                      uint32_t qp_num, uint16_t lid,
                                                      udcm_msg_hdr_t *msg_hdr)
{
    uint8_t port_num;
    int i;

    port_num = msg_hdr->data.req.rem_port_num;

    for (i = 0 ; i < opal_pointer_array_get_size (endpoints) ; ++i) {
        mca_btl_openib_endpoint_t *endpoint;
        modex_msg_t *msg;

        endpoint = (mca_btl_openib_endpoint_t *)
        opal_pointer_array_get_item (endpoints, i);
        if (NULL == endpoint) {
            continue;
        }

        msg = UDCM_ENDPOINT_REM_MODEX(endpoint);

        if (msg->mm_qp_num == qp_num && msg->mm_port_num == port_num &&
            msg->mm_lid == lid)
            return endpoint;
    }

    BTL_ERROR(("could not find endpoint with port: %d, lid: %d, msg_type: %d",
               port_num, lid, msg_hdr->type));

    return NULL;
}

static int udcm_endpoint_init_data (mca_btl_base_endpoint_t *lcl_ep)
{
    modex_msg_t *remote_msg = UDCM_ENDPOINT_REM_MODEX(lcl_ep);
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    struct ibv_ah_attr ah_attr;
    int rc = OPAL_SUCCESS;

    do {
        if (udep->udep_initialized)
            break;

        /* Cache an address handle for this endpoint */
        memset(&ah_attr, 0, sizeof(ah_attr));

        ah_attr.dlid          = lcl_ep->rem_info.rem_lid;
        ah_attr.port_num      = remote_msg->mm_port_num;
        ah_attr.sl            = mca_btl_openib_component.ib_service_level;
        ah_attr.src_path_bits = lcl_ep->endpoint_btl->src_path_bits;

        udep->ah = ibv_create_ah (lcl_ep->endpoint_btl->device->ib_pd, &ah_attr);
        if (!udep->ah) {
            rc = OPAL_ERROR;
            break;
        }
    } while (0);

    if (OPAL_SUCCESS == rc) {
        udep->udep_initialized = true;
    }

    return rc;
}

/* mark: ud send */

static inline int udcm_wait_for_send_completion (udcm_module_t *m)
{
    struct ibv_wc wc;
    int rc;

    do {
        rc = ibv_poll_cq (m->cm_send_cq, 1, &wc);
        if (0 > rc) {
            BTL_VERBOSE(("send failed"));
            return OPAL_ERROR;
        } else if (0 == rc) {
            continue;
        } else if (IBV_WC_SUCCESS != wc.status) {
            BTL_ERROR(("send failed with verbs status %d", wc.status));
            return OPAL_ERROR;
        }

        break;
    } while (1);

    return OPAL_SUCCESS;
}

static int udcm_post_send (mca_btl_base_endpoint_t *lcl_ep, void *data,
                           int length, int lkey)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    volatile static int msg_num = 0;
    struct ibv_send_wr wr, *bad_wr;
    struct ibv_sge sge;
    int rc;

    /* NTH: need to lock here or we run into problems (slowness) */
    opal_mutex_lock(&m->cm_send_lock);

    if (0 == lkey) {
        /* copy the message into the registered send buffer */
        sge.addr   = (uintptr_t) udcm_module_get_send_buffer (m);
        sge.length = length;
        sge.lkey   = m->cm_mr->lkey;

        memcpy ((uintptr_t *)sge.addr, data, length);
    } else {
        sge.addr   = (uintptr_t) data;
        sge.length = length;
        sge.lkey   = lkey;
    }

    wr.wr_id      = UDCM_WR_SEND_ID | msg_num++;
    wr.next       = NULL;
    wr.sg_list    = &sge;
    wr.num_sge    = 1;
    wr.opcode     = IBV_WR_SEND;
    wr.send_flags = IBV_SEND_SOLICITED | IBV_SEND_SIGNALED;
    wr.wr.ud.ah   = udep->ah;

    wr.wr.ud.remote_qpn  = UDCM_ENDPOINT_REM_MODEX(lcl_ep)->mm_qp_num;
    wr.wr.ud.remote_qkey = 0;

    rc = ibv_post_send (m->listen_qp, &wr, &bad_wr);
    if (0 != rc) {
        BTL_VERBOSE(("error posting send. errno: %d", errno));
    } else {
        rc = udcm_wait_for_send_completion (m);
    }

    opal_mutex_unlock (&m->cm_send_lock);

    return rc;
}

/* mark: message allocation */

static int udcm_new_message (mca_btl_base_endpoint_t *lcl_ep,
                             mca_btl_base_endpoint_t *rem_ep, uint8_t type,
                             size_t length, udcm_message_sent_t **msgp)
{
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    udcm_message_sent_t *message;

    message = OBJ_NEW(udcm_message_sent_t);
    if (NULL == message) {
        BTL_ERROR(("malloc failed!"));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    message->data = calloc (m->msg_length, 1);
    if (NULL == message->data) {
        OBJ_RELEASE(message);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    message->length        = length;

    message->data->hdr.rem_ep  = lcl_ep;
    message->data->hdr.lcl_ep  = rem_ep;
    message->data->hdr.type    = type;
    message->data->hdr.rem_ctx = (uintptr_t) message;

    message->endpoint = lcl_ep;

    udcm_set_message_timeout (message);

    opal_atomic_wmb ();

    *msgp = message;

    BTL_VERBOSE(("created message %p with type %d", (void *) message, type));

    return OPAL_SUCCESS;
}

/* mark: rc message functions */

/*
 * Allocate a CM request structure and initialize some common fields
 * (that are independent of the specific QP, etc.)
 */
static int udcm_send_request (mca_btl_base_endpoint_t *lcl_ep,
                              mca_btl_base_endpoint_t *rem_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    udcm_message_sent_t *msg;
    int i, rc;

    BTL_VERBOSE(("sending request for endpoint %p", (void *) lcl_ep));

    udep->sent_req = true;

    if (0 != (rc = udcm_new_message (lcl_ep, rem_ep, UDCM_MESSAGE_CONNECT,
                                     m->msg_length, &msg))) {
        return rc;
    }

    msg->data->hdr.data.req.rem_ep_index = htonl(lcl_ep->index);
    msg->data->hdr.data.req.rem_port_num = m->modex.mm_port_num;

    for (i = 0 ; i < mca_btl_openib_component.num_qps ; ++i) {
        msg->data->qps[i].psn    = htonl(lcl_ep->qps[i].qp->lcl_psn);
        msg->data->qps[i].qp_num = htonl(lcl_ep->qps[i].qp->lcl_qp->qp_num);
    }

    if (0 != (rc = udcm_post_send (lcl_ep, msg->data, m->msg_length, 0))) {
        BTL_VERBOSE(("error posting REQ"));

        udcm_cancel_message_timeout (msg);

        return rc;
    }

    return 0;
}

static int udcm_send_complete (mca_btl_base_endpoint_t *lcl_ep,
                               mca_btl_base_endpoint_t *rem_ep)
{
    udcm_message_sent_t *msg;
    int rc;

    if (0 != (rc = udcm_new_message (lcl_ep, rem_ep, UDCM_MESSAGE_COMPLETE,
                                     sizeof (udcm_msg_hdr_t), &msg))) {
        return rc;
    }

    rc = udcm_post_send (lcl_ep, msg->data, sizeof (udcm_msg_hdr_t), 0);
    if (0 != rc) {
        BTL_VERBOSE(("error posting complete"));

        udcm_cancel_message_timeout (msg);

        return rc;
    }

    return 0;
}

static int udcm_send_reject (mca_btl_base_endpoint_t *lcl_ep,
                             mca_btl_base_endpoint_t *rem_ep,
                             int rej_reason)
{
    udcm_message_sent_t *msg;
    int rc;

    if (0 != (rc = udcm_new_message (lcl_ep, rem_ep, UDCM_MESSAGE_REJECT,
                                     sizeof (udcm_msg_hdr_t), &msg))) {
        return rc;
    }

    msg->data->hdr.data.rej.reason  = htonl(rej_reason);

    rc = udcm_post_send (lcl_ep, msg->data, sizeof (udcm_msg_hdr_t), 0);
    if (0 != rc) {
        BTL_VERBOSE(("error posting rejection"));

        udcm_cancel_message_timeout (msg);

        return rc;
    }

    return 0;
}

static int udcm_send_ack (mca_btl_base_endpoint_t *lcl_ep, uintptr_t rem_ctx)
{
    udcm_msg_hdr_t hdr;

    BTL_VERBOSE(("sending ack for message %p on ep %p", (void *) rem_ctx, (void *) lcl_ep));

    hdr.type         = UDCM_MESSAGE_ACK;
    hdr.rem_ctx      = rem_ctx;

    return udcm_post_send (lcl_ep, &hdr, sizeof (hdr), 0);
}

static int udcm_handle_ack (udcm_module_t *m, const uintptr_t ctx, const uint16_t slid,
                            const uint32_t rem_qp)
{
    udcm_message_sent_t *msg, *next;
    bool found = false;

    opal_mutex_lock (&m->cm_timeout_lock);

    BTL_VERBOSE(("got ack for message %p from slid 0x%04x qp 0x%08x", (void *) ctx, slid,
                 rem_qp));

    /* verify that the message is still active */
    OPAL_LIST_FOREACH_SAFE(msg, next, &m->flying_messages, udcm_message_sent_t) {
        if ((uintptr_t) msg != ctx) {
            continue;
        }

        BTL_VERBOSE(("found matching message"));
        found = true;

        /* mark that this event is not active anymore */
        msg->event_active = false;

        /* there is a possibility this event is being handled by another thread right now. it
         * should be safe to activate the event even in this case. the callback will handle
         * releasing the message. this is done to avoid a race between the message handling
         * thread and the thread progressing libevent. if the message handler is ever put
         * in the event base then it will be safe to just release the message here but that
         * is not the case atm. */
        opal_event_active (&msg->event, 0, 0);

        break;
    }

    if (!found) {
        BTL_VERBOSE(("message %p not found in the list of flying messages", (void *) ctx));
    }

    opal_mutex_unlock (&m->cm_timeout_lock);

    return OPAL_SUCCESS;
}

/* mark: rc message handling */

static int udcm_handle_connect(mca_btl_openib_endpoint_t *lcl_ep,
                               mca_btl_openib_endpoint_t *rem_ep)
{
    udcm_reject_reason_t rej_reason = UDCM_REJ_REMOTE_ERROR;
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    int rc = OPAL_ERROR;

    do {
        if (NULL == udep) {
            break;
        }

        opal_mutex_lock (&udep->udep_lock);

        if (true == udep->recv_req) {
            /* this endpoint is already connected */
            BTL_VERBOSE(("already connected"));
            rc = OPAL_SUCCESS;
            rej_reason = UDCM_REJ_ALREADY_CONNECTED;
            break;
        }

        udep->recv_req = true;

        opal_atomic_wmb ();
        if (MCA_BTL_IB_CLOSED == lcl_ep->endpoint_state) {
            lcl_ep->endpoint_state = MCA_BTL_IB_CONNECTING;
        }

        if (OPAL_SUCCESS != (rc = udcm_rc_qp_create_all (lcl_ep))) {
            BTL_VERBOSE(("error initializing endpoint qps"));
            break;
        }

        rc = udcm_rc_qps_to_rts (lcl_ep);
        if (OPAL_SUCCESS != rc) {
            break;
        }

        if (false == udep->sent_req) {
            rc = udcm_send_request (lcl_ep, rem_ep);

            if (OPAL_SUCCESS != rc) {
                break;
            }
        }

        rc = udcm_send_complete (lcl_ep, rem_ep);
        if (OPAL_SUCCESS != rc) {
            break;
        }

        if (udep->recv_comp) {
            udcm_finish_connection (lcl_ep);
        }

        opal_mutex_unlock (&udep->udep_lock);

        return OPAL_SUCCESS;
    } while (0);

    opal_mutex_unlock (&udep->udep_lock);

    /* Reject the request */
    BTL_VERBOSE(("rejecting request for reason %d", rej_reason));

    udcm_send_reject (lcl_ep, rem_ep, rej_reason);

    if (OPAL_SUCCESS != rc) {
        /* Communicate to the upper layer that the connection on this
           endpoint has failed */
        mca_btl_openib_endpoint_invoke_error (lcl_ep);
    }

    return rc;
}

static int udcm_handle_reject(mca_btl_openib_endpoint_t *lcl_ep,
                              udcm_msg_hdr_t *msg_hdr)
{
    int32_t reason = ntohl(msg_hdr->data.rej.reason);

    BTL_VERBOSE(("reject received: reason %d", reason));

    if (UDCM_REJ_ALREADY_CONNECTED == reason) {
        return OPAL_SUCCESS;
    }
#if HAVE_XRC
    else if (UDCM_REJ_NOT_READY == reason) {
        return udcm_xrc_restart_connect (lcl_ep);
    }
#endif

    /* Communicate to the upper layer that the connection on this
       endpoint has failed */
    mca_btl_openib_endpoint_invoke_error (lcl_ep);

    return OPAL_ERR_NOT_FOUND;
}

static int udcm_finish_connection (mca_btl_openib_endpoint_t *lcl_ep)
{
    BTL_VERBOSE(("finishing connection for endpoint %p.", (void *) lcl_ep));

    /* Ensure that all the writes back to the endpoint and associated
       data structures have completed */
    opal_atomic_wmb();

    mca_btl_openib_endpoint_cpc_complete(lcl_ep);

    return OPAL_SUCCESS;
}

static int udcm_handle_complete (mca_btl_openib_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);

    udep->recv_comp = true;
    if (udep->recv_req) {
        udcm_finish_connection (lcl_ep);
    } else {
        OPAL_THREAD_UNLOCK(&lcl_ep->endpoint_lock);
    }

    return OPAL_SUCCESS;
}

/* mark: message processing */

static int udcm_process_messages (struct ibv_cq *event_cq, udcm_module_t *m)
{
    mca_btl_openib_endpoint_t *lcl_ep;
    int msg_num, i, count;
    udcm_msg_t *message = NULL;
    udcm_message_recv_t *item;
    struct ibv_wc wc[20];
    udcm_endpoint_t *udep;
    uint64_t dir;

    memset(wc, 0, sizeof(wc));

    count = ibv_poll_cq (event_cq, 20, wc);
    if (count < 0)
        return count;

    for (i = 0 ; i < count ; i++) {
        dir = wc[i].wr_id & UDCM_WR_DIR_MASK;

        BTL_VERBOSE(("WC: wr_id: 0x%016" PRIu64 ", status: %d, opcode: 0x%x, byte_len: %x, imm_data: 0x%08x, "
                     "qp_num: 0x%08x, src_qp: 0x%08x, wc_flags: 0x%x, slid: 0x%04x",
                     wc[i].wr_id, wc[i].status, wc[i].opcode, wc[i].byte_len,
                     wc[i].imm_data, wc[i].qp_num, wc[i].src_qp, wc[i].wc_flags, wc[i].slid));

        if (UDCM_WR_RECV_ID != dir) {
            opal_output (0, "unknown packet");
            continue;
        }

        msg_num = (int)(wc[i].wr_id & (~UDCM_WR_DIR_MASK));

        if (IBV_WC_SUCCESS != wc[i].status) {
            BTL_ERROR(("recv work request for buffer %d failed, code = %d",
                         msg_num, wc[i].status));
            count = -1;
            break;
        }

        message = (udcm_msg_t *) udcm_module_get_recv_buffer (m, msg_num, true);

        if (UDCM_MESSAGE_ACK == message->hdr.type) {
            /* ack! */
            udcm_handle_ack (m, message->hdr.rem_ctx, wc[i].slid, wc[i].src_qp);
            udcm_module_post_one_recv (m, msg_num);

            continue;
        }

        lcl_ep = message->hdr.lcl_ep;

        if (NULL == lcl_ep) {
            lcl_ep = udcm_find_endpoint (m->btl->device->endpoints, wc[i].src_qp,
                                         wc[i].slid, &message->hdr);
        }

        if (NULL == lcl_ep ) {
            /* cant find associated endpoint */
            BTL_ERROR(("could not find associated endpoint."));
            udcm_module_post_one_recv (m, msg_num);

            continue;
        }

        message->hdr.lcl_ep = lcl_ep;

        BTL_VERBOSE(("received message. type: %u, lcl_ep = %p, rem_ep = %p, "
                     "src qpn = %d, length = %d, local buffer # = %d",
                     message->hdr.type, (void *) message->hdr.lcl_ep, (void *) message->hdr.rem_ep,
                     wc[i].src_qp, wc[i].byte_len, msg_num));

        udep = UDCM_ENDPOINT_DATA(lcl_ep);

        if (NULL == udep) {
            /* Endpoint was not initialized or was finalized */
            udcm_module_post_one_recv (m, msg_num);
            continue;
        }

        opal_mutex_lock (&udep->udep_lock);

        /* Need to ensure endpoint data is initialized before sending the ack */
        if (OPAL_SUCCESS != udcm_endpoint_init_data (lcl_ep)) {
            BTL_ERROR(("could not initialize cpc data for endpoint"));
            udcm_module_post_one_recv (m, msg_num);
            opal_mutex_unlock (&udep->udep_lock);
            continue;
        }

        /* save message data in the endpoint */
        if (UDCM_MESSAGE_CONNECT == message->hdr.type) {
            /* Save remote queue pair information */
            int num_qps = mca_btl_openib_component.num_qps;

            lcl_ep->rem_info.rem_index = ntohl(message->hdr.data.req.rem_ep_index);

            for (int qp_index = 0 ; qp_index < num_qps ; ++qp_index) {
                /* Save these numbers on the endpoint for reference. */
                lcl_ep->rem_info.rem_qps[qp_index].rem_psn    =
                    ntohl(message->qps[qp_index].psn);
                lcl_ep->rem_info.rem_qps[qp_index].rem_qp_num =
                    ntohl(message->qps[qp_index].qp_num);
            }
        }

#if HAVE_XRC
        else if (UDCM_MESSAGE_XRESPONSE == message->hdr.type ||
                 UDCM_MESSAGE_XRESPONSE2 == message->hdr.type) {
            /* save remote srq information */
            int num_srqs = mca_btl_openib_component.num_xrc_qps;

            lcl_ep->rem_info.rem_index = ntohl(message->hdr.data.xres.rem_ep_index);

            for (int i = 0 ; i < num_srqs ; ++i) {
                lcl_ep->rem_info.rem_srqs[i].rem_srq_num = ntohl(message->qps[i].qp_num);
                BTL_VERBOSE(("Received srq[%d] num = %d", i, lcl_ep->rem_info.rem_srqs[i].rem_srq_num));
            }

            if (UDCM_MESSAGE_XRESPONSE == message->hdr.type) {
                /* swap response header data */
                message->hdr.data.xres.rem_psn = ntohl(message->hdr.data.xres.rem_psn);
                message->hdr.data.xres.rem_qp_num = ntohl(message->hdr.data.xres.rem_qp_num);

                /* save remote qp information not included in the XRESPONSE2 message */
                lcl_ep->rem_info.rem_qps[0].rem_psn    = message->hdr.data.xres.rem_psn;
                lcl_ep->rem_info.rem_qps[0].rem_qp_num = message->hdr.data.xres.rem_qp_num;

                BTL_VERBOSE(("Received remote qp: %d, psn: %d", lcl_ep->rem_info.rem_qps[0].rem_qp_num,
                             lcl_ep->rem_info.rem_qps[0].rem_psn))

                /* update ib_addr with remote qp number */
                lcl_ep->ib_addr->remote_xrc_rcv_qp_num = lcl_ep->rem_info.rem_qps[0].rem_qp_num;
            }
        } else if (UDCM_MESSAGE_XCONNECT == message->hdr.type ||
                   UDCM_MESSAGE_XCONNECT2 == message->hdr.type) {
            lcl_ep->rem_info.rem_index = ntohl(message->hdr.data.xreq.rem_ep_index);

            /* swap request header data */
            message->hdr.data.xreq.rem_qp_num = ntohl(message->hdr.data.xreq.rem_qp_num);
            message->hdr.data.xreq.rem_psn = ntohl(message->hdr.data.xreq.rem_psn);

            if (UDCM_MESSAGE_XCONNECT2 == message->hdr.type) {
                /* save the qp number for unregister */
#if ! OPAL_HAVE_CONNECTX_XRC_DOMAINS
                lcl_ep->xrc_recv_qp_num = message->hdr.data.xreq.rem_qp_num;
#endif

            }
        }
#endif

        opal_mutex_unlock (&udep->udep_lock);

        item = OBJ_NEW(udcm_message_recv_t);

        /* Copy just the message header */
        memcpy (&item->msg_hdr, &message->hdr, sizeof (message->hdr));

        opal_mutex_lock(&m->cm_recv_msg_queue_lock);
        opal_list_append (&m->cm_recv_msg_queue, &item->super);
        opal_mutex_unlock(&m->cm_recv_msg_queue_lock);

        udcm_send_ack (lcl_ep, message->hdr.rem_ctx);

        /* Repost the receive */
        udcm_module_post_one_recv (m, msg_num);
    }

    opal_mutex_lock (&m->cm_recv_msg_queue_lock);
    if (opal_list_get_size (&m->cm_recv_msg_queue) &&
        !m->cm_message_event_active) {
        m->cm_message_event_active = true;
        opal_btl_openib_fd_run_in_main (udcm_message_callback, (void *) m);
    }
    opal_mutex_unlock (&m->cm_recv_msg_queue_lock);

    return count;
}

static void *udcm_cq_event_dispatch(int fd, int flags, void *context)
{
    udcm_module_t *m = (udcm_module_t *) context;
    struct ibv_cq *event_cq = m->cm_recv_cq;
    void *event_context;
    int rc;

    opal_mutex_lock (&m->cm_lock);

    do {
        if (OPAL_UNLIKELY(NULL == m || NULL == m->cm_channel)) {
            break;
        }

        rc = ibv_get_cq_event (m->cm_channel, &event_cq, &event_context);

        if (0 != rc || NULL == event_cq) {
            break;
        }

        /* acknowlege the event */
        ibv_ack_cq_events (event_cq, 1);

        if (m->cm_exiting) {
            break;
        }

        rc = udcm_process_messages (event_cq, m);
        if (rc < 0) {
            BTL_VERBOSE(("error processing incomming messages"));
            break;
        }

        if (ibv_req_notify_cq(event_cq, 0)) {
            BTL_VERBOSE(("error asking for cq notifications"));
        }
    } while (0);

    opal_mutex_unlock (&m->cm_lock);

    return NULL;
}

static void *udcm_message_callback (void *context)
{
    udcm_module_t *m = (udcm_module_t *) context;
    udcm_message_recv_t *item;

    BTL_VERBOSE(("running message thread"));

    opal_mutex_lock(&m->cm_recv_msg_queue_lock);
    while ((item = (udcm_message_recv_t *)
            opal_list_remove_first (&m->cm_recv_msg_queue))) {
        mca_btl_openib_endpoint_t *lcl_ep = item->msg_hdr.lcl_ep;
        opal_mutex_unlock(&m->cm_recv_msg_queue_lock);

        OPAL_THREAD_LOCK(&lcl_ep->endpoint_lock);

        switch (item->msg_hdr.type) {
        case UDCM_MESSAGE_CONNECT:
            udcm_handle_connect (lcl_ep, item->msg_hdr.rem_ep);
            OPAL_THREAD_UNLOCK(&lcl_ep->endpoint_lock);
            break;
        case UDCM_MESSAGE_REJECT:
            udcm_handle_reject (lcl_ep, &item->msg_hdr);
            OPAL_THREAD_UNLOCK(&lcl_ep->endpoint_lock);
            break;
        case UDCM_MESSAGE_COMPLETE:
            udcm_handle_complete (lcl_ep);
            break;
#if HAVE_XRC
        case UDCM_MESSAGE_XRESPONSE2:
            udcm_finish_connection (lcl_ep);
            break;
        case UDCM_MESSAGE_XRESPONSE:
            /* udcm_handle_xresponse will call mca_btl_openib_endpoint_cpc_complete
               which will drop the thread lock */
            udcm_xrc_handle_xresponse (lcl_ep, &item->msg_hdr);
            break;
        case UDCM_MESSAGE_XCONNECT:
        case UDCM_MESSAGE_XCONNECT2:
            udcm_xrc_handle_xconnect (lcl_ep, &item->msg_hdr);
            OPAL_THREAD_UNLOCK(&lcl_ep->endpoint_lock);
            break;
#endif
        default:
            BTL_VERBOSE(("unknown message type"));
        }

        OBJ_RELEASE (item);

        opal_mutex_lock(&m->cm_recv_msg_queue_lock);
    }

    BTL_VERBOSE(("exiting message thread"));

    m->cm_message_event_active = false;
    opal_mutex_unlock(&m->cm_recv_msg_queue_lock);

    return NULL;
}

/* mark: udcm_message_sent_t class */

static void udcm_sent_message_constructor (udcm_message_sent_t *message)
{
    memset ((char *)message + sizeof (message->super), 0,
            sizeof (*message) - sizeof (message->super));
    opal_event_evtimer_set(opal_event_base, &message->event, udcm_send_timeout, message);
}

static void udcm_sent_message_destructor (udcm_message_sent_t *message)
{
    if (message->data) {
        free (message->data);
    }

    if (message->event_active) {
        opal_event_evtimer_del (&message->event);
        message->event_active = false;
    }
}

/* mark: message timeout code */
/* Message timeouts */
static void udcm_send_timeout (evutil_socket_t fd, short event, void *arg)
{
    udcm_message_sent_t *msg = (udcm_message_sent_t *) arg;
    mca_btl_base_endpoint_t *lcl_ep = msg->endpoint;
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);

    opal_mutex_lock (&m->cm_timeout_lock);
    opal_list_remove_item (&m->flying_messages, &msg->super);
    opal_mutex_unlock (&m->cm_timeout_lock);

    if (m->cm_exiting || !msg->event_active) {
        /* we are exiting or the event is no longer valid */
        OBJ_RELEASE(msg);
        return;
    }

    msg->event_active = false;

    do {
        BTL_VERBOSE(("send for message to 0x%04x:0x%08x timed out",
                     UDCM_ENDPOINT_REM_MODEX(lcl_ep)->mm_lid,
                     UDCM_ENDPOINT_REM_MODEX(lcl_ep)->mm_qp_num));

        /* This happens from time to time at the end of a run (probably due to a
           lost ack on the completion message). */
        if (NULL == lcl_ep->endpoint_local_cpc_data ||
            MCA_BTL_IB_CONNECTED == lcl_ep->endpoint_state ||
            m->cm_exiting) {
            OBJ_RELEASE (msg);
            break;
        }

        if (msg->tries == udcm_max_retry) {
            opal_output (0, "too many retries sending message to 0x%04x:0x%08x, giving up",
                         UDCM_ENDPOINT_REM_MODEX(lcl_ep)->mm_lid,
                         UDCM_ENDPOINT_REM_MODEX(lcl_ep)->mm_qp_num);

            /* We are running in the timeout thread. Invoke the error in the
               main thread */
            opal_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error,
                                           lcl_ep);
            break;
        }

        msg->tries++;

        udcm_set_message_timeout (msg);

        if (0 != udcm_post_send (lcl_ep, msg->data, msg->length, 0)) {
            BTL_VERBOSE(("error reposting message"));
            opal_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error,
                                           lcl_ep);
            break;
        }
    } while (0);
}

static void udcm_set_message_timeout (udcm_message_sent_t *message)
{
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(message->endpoint);

    BTL_VERBOSE(("activating timeout for message %p", (void *) message));

    opal_mutex_lock (&m->cm_timeout_lock);

    opal_list_append (&m->flying_messages, &message->super);

    /* start the event */
    opal_event_evtimer_add (&message->event, &udcm_timeout_tv);
    message->event_active = true;

    opal_mutex_unlock (&m->cm_timeout_lock);
}

static void udcm_cancel_message_timeout (udcm_message_sent_t *message)
{
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(message->endpoint);

    BTL_VERBOSE(("cancelling timeout for message %p", (void *) message));

    opal_mutex_lock (&m->cm_timeout_lock);

    opal_list_remove_item (&m->flying_messages, &message->super);

    /* start the event */
    opal_event_evtimer_del (&message->event);
    message->event_active = false;

    opal_mutex_unlock (&m->cm_timeout_lock);
}

/* mark: xrc connection support */

/* XRC support functions */
#if HAVE_XRC
static int udcm_xrc_start_connect (opal_btl_openib_connect_base_module_t *cpc,
                                   mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    int rc = OPAL_SUCCESS;

    opal_mutex_lock (&udep->udep_lock);
    opal_mutex_lock (&lcl_ep->ib_addr->addr_lock);

    if (OPAL_SUCCESS != (rc = udcm_endpoint_init_data (lcl_ep))) {
        BTL_VERBOSE(("error initializing endpoint cpc data"));
        opal_mutex_unlock (&udep->udep_lock);
        opal_mutex_unlock (&lcl_ep->ib_addr->addr_lock);
        return rc;
    }

    lcl_ep->endpoint_state = MCA_BTL_IB_CONNECTING;

    BTL_VERBOSE(("The IB addr: sid %" PRIx64 " lid %d with status %d, "
                 "subscribing to this address", lcl_ep->ib_addr->subnet_id,
                 lcl_ep->ib_addr->status, lcl_ep->ib_addr->lid));

    switch (lcl_ep->ib_addr->status) {
    case MCA_BTL_IB_ADDR_CLOSED:
        if (OPAL_SUCCESS != (rc = udcm_xrc_send_qp_create(lcl_ep))) {
            break;
        }

        /* Send connection info over to remote endpoint */
        lcl_ep->ib_addr->status = MCA_BTL_IB_ADDR_CONNECTING;
        if (OPAL_SUCCESS != (rc = udcm_xrc_send_request (lcl_ep, NULL, UDCM_MESSAGE_XCONNECT))) {
            BTL_ERROR(("Error sending connect request, error code %d", rc));
        }
        break;
    case MCA_BTL_IB_ADDR_CONNECTING:
        /* somebody already connectng to this machine, lets wait */
        opal_list_append(&lcl_ep->ib_addr->pending_ep, &lcl_ep->super);
        break;
    case MCA_BTL_IB_ADDR_CONNECTED:
        /* so we have the send qp, we just need the receive site.
         * Send request for SRQ numbers */

        if (OPAL_SUCCESS != (rc = udcm_xrc_send_request (lcl_ep, NULL, UDCM_MESSAGE_XCONNECT2))) {
            BTL_ERROR(("error sending xrc connect request, error code %d", rc));
        }
        break;
    default:
        BTL_ERROR(("Invalid endpoint status %d", lcl_ep->ib_addr->status));
    }

    opal_mutex_unlock (&lcl_ep->ib_addr->addr_lock);
    opal_mutex_unlock (&udep->udep_lock);

    return rc;
}

/* In case if XRC recv qp was closed and sender still don't know about it
 * we need close the qp, reset the ib_adrr status to CLOSED and start everything
 * from scratch.
 */
static int udcm_xrc_restart_connect (mca_btl_base_endpoint_t *lcl_ep)
{
    opal_mutex_lock (&lcl_ep->ib_addr->addr_lock);

    BTL_VERBOSE(("Restart connection for IB addr: sid %" PRIx64 " lid %d, with status "
                 "%d, resetting and starting from scratch", lcl_ep->ib_addr->subnet_id,
                 lcl_ep->ib_addr->lid, lcl_ep->ib_addr->status));

    if (MCA_BTL_IB_ADDR_CONNECTED == lcl_ep->ib_addr->status) {
        /* so we have the send qp, we just need the recive site.
         * Send request for SRQ numbers */
        /* Switching back to closed and starting from scratch */
        lcl_ep->ib_addr->status = MCA_BTL_IB_ADDR_CLOSED;
        /* destroy the qp */
        /* the reciver site was alredy closed so all pending list must be clean ! */
        assert (opal_list_is_empty(&lcl_ep->qps->no_wqe_pending_frags[0]));
        assert (opal_list_is_empty(&lcl_ep->qps->no_wqe_pending_frags[1]));

        if (ibv_destroy_qp (lcl_ep->qps[0].qp->lcl_qp))
            BTL_ERROR(("Failed to destroy QP. errno %d", errno));
    }

    opal_mutex_unlock (&lcl_ep->ib_addr->addr_lock);

    /* udcm_xrc_start_connect () should automaticly handle all other cases */
    return udcm_xrc_start_connect (NULL, lcl_ep);
}

/* mark: xrc send qp */

/* Send qp connect */
static int udcm_xrc_send_qp_connect (mca_btl_openib_endpoint_t *lcl_ep, uint32_t rem_qp_num, uint32_t rem_psn)
{
    mca_btl_openib_module_t *openib_btl = lcl_ep->endpoint_btl;
    struct ibv_qp_attr attr;
    struct ibv_qp *qp;
    uint32_t psn;
    int ret;

    BTL_VERBOSE(("Connecting send qp: %p, remote qp: %d", (void *)lcl_ep->qps[0].qp->lcl_qp,
                 rem_qp_num));
    assert(NULL != lcl_ep->qps);
    qp = lcl_ep->qps[0].qp->lcl_qp;
    psn = lcl_ep->qps[0].qp->lcl_psn;


    memset(&attr, 0, sizeof(attr));
    attr.qp_state           = IBV_QPS_RTR;
    attr.path_mtu = (openib_btl->device->mtu < lcl_ep->rem_info.rem_mtu) ?
        openib_btl->device->mtu : lcl_ep->rem_info.rem_mtu;
    attr.dest_qp_num        = rem_qp_num;
    attr.rq_psn             = rem_psn;
    attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
    attr.min_rnr_timer  = mca_btl_openib_component.ib_min_rnr_timer;
    attr.ah_attr.is_global     = 0;
    attr.ah_attr.dlid          = lcl_ep->rem_info.rem_lid;
    attr.ah_attr.src_path_bits = openib_btl->src_path_bits;
    attr.ah_attr.port_num      = openib_btl->port_num;
    attr.ah_attr.static_rate   = 0;
    attr.ah_attr.sl            = mca_btl_openib_component.ib_service_level;

#if (ENABLE_DYNAMIC_SL)
    /* if user enabled dynamic SL, get it from PathRecord */
    if (0 != mca_btl_openib_component.ib_path_record_service_level) {
        int rc = btl_openib_connect_get_pathrecord_sl(qp->context,
                                                      attr.ah_attr.port_num,
                                                      openib_btl->lid,
                                                      attr.ah_attr.dlid);
        if (OPAL_ERROR == rc) {
            return OPAL_ERROR;
        }
        attr.ah_attr.sl = rc;
    }
#endif

    if (mca_btl_openib_component.verbose) {
        BTL_VERBOSE(("Set MTU to IBV value %d (%s bytes)", attr.path_mtu,
                     (attr.path_mtu == IBV_MTU_256) ? "256" :
                     (attr.path_mtu == IBV_MTU_512) ? "512" :
                     (attr.path_mtu == IBV_MTU_1024) ? "1024" :
                     (attr.path_mtu == IBV_MTU_2048) ? "2048" :
                     (attr.path_mtu == IBV_MTU_4096) ? "4096" :
                     "unknown (!)"));
    }
    ret = ibv_modify_qp(qp, &attr,
                IBV_QP_STATE              |
                IBV_QP_AV                 |
                IBV_QP_PATH_MTU           |
                IBV_QP_DEST_QPN           |
                IBV_QP_RQ_PSN             |
                IBV_QP_MAX_DEST_RD_ATOMIC |
                        IBV_QP_MIN_RNR_TIMER);
    if (ret) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_RTR errno says: %s [%d]",
                   qp->qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    attr.qp_state       = IBV_QPS_RTS;
    attr.timeout        = mca_btl_openib_component.ib_timeout;
    attr.retry_cnt      = mca_btl_openib_component.ib_retry_count;
    attr.rnr_retry      = mca_btl_openib_component.ib_rnr_retry;
    attr.sq_psn         = psn;
    attr.max_rd_atomic  = mca_btl_openib_component.ib_max_rdma_dst_ops;
    ret = ibv_modify_qp(qp, &attr,
            IBV_QP_STATE              |
            IBV_QP_TIMEOUT            |
            IBV_QP_RETRY_CNT          |
            IBV_QP_RNR_RETRY          |
            IBV_QP_SQ_PSN             |
                        IBV_QP_MAX_QP_RD_ATOMIC);
    if (ret) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_RTS errno says: %s [%d]",
                   qp->qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

/* Create XRC send qp */
static int udcm_xrc_send_qp_create (mca_btl_base_endpoint_t *lcl_ep)
{
    int prio = BTL_OPENIB_LP_CQ; /* all send completions go to low prio CQ */
    uint32_t send_wr;
    struct ibv_qp **qp;
    uint32_t *psn;
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    struct ibv_qp_init_attr_ex qp_init_attr;
#else
    struct ibv_qp_init_attr qp_init_attr;
#endif
    struct ibv_qp_attr attr;
    int ret;
    size_t req_inline;

    mca_btl_openib_module_t *openib_btl =
        (mca_btl_openib_module_t*)lcl_ep->endpoint_btl;

    /* Prepare QP structs */
    BTL_VERBOSE(("creating xrc send qp"));
    qp = &lcl_ep->qps[0].qp->lcl_qp;
    psn = &lcl_ep->qps[0].qp->lcl_psn;

    /* reserve additional wr for eager rdma credit management */
    send_wr = lcl_ep->ib_addr->qp->sd_wqe +
        (mca_btl_openib_component.use_eager_rdma ?
         mca_btl_openib_component.max_eager_rdma : 0);
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr_ex));
#else
    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr));
#endif
    memset(&attr, 0, sizeof(struct ibv_qp_attr));

    qp_init_attr.send_cq = qp_init_attr.recv_cq = openib_btl->device->ib_cq[prio];

    /* no need recv queue; receives are posted to srq */
    qp_init_attr.cap.max_recv_wr = 0;
    qp_init_attr.cap.max_send_wr = send_wr;
    qp_init_attr.cap.max_inline_data = req_inline =
        openib_btl->device->max_inline_data;
    qp_init_attr.cap.max_send_sge = 1;
    /* this one is ignored by driver */
    qp_init_attr.cap.max_recv_sge = 1; /* we do not use SG list */
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    qp_init_attr.qp_type = IBV_QPT_XRC_SEND;
    qp_init_attr.comp_mask = IBV_QP_INIT_ATTR_PD;
    qp_init_attr.pd = openib_btl->device->ib_pd;
    *qp = ibv_create_qp_ex(openib_btl->device->ib_dev_context, &qp_init_attr);
#else
    qp_init_attr.qp_type = IBV_QPT_XRC;
    qp_init_attr.xrc_domain = openib_btl->device->xrc_domain;
    *qp = ibv_create_qp(openib_btl->device->ib_pd, &qp_init_attr);
#endif
    if (NULL == *qp) {
        opal_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "ibv_create_qp failed", true,
                       opal_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev),
                       "Reliable connected (XRC)");
        return OPAL_ERROR;
    }

    if (qp_init_attr.cap.max_inline_data < req_inline) {
        lcl_ep->qps[0].ib_inline_max = qp_init_attr.cap.max_inline_data;
        opal_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", opal_process_info.nodename,
                       ibv_get_device_name(openib_btl->device->ib_dev),
                       openib_btl->port_num,
                       req_inline, qp_init_attr.cap.max_inline_data);
    } else {
        lcl_ep->qps[0].ib_inline_max = req_inline;
    }

    attr.qp_state = IBV_QPS_INIT;
    attr.pkey_index = openib_btl->pkey_index;
    attr.port_num = openib_btl->port_num;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;
#if HAVE_DECL_IBV_ATOMIC_HCA
    attr.qp_access_flags |= IBV_ACCESS_REMOTE_ATOMIC;
#endif
    ret = ibv_modify_qp(*qp, &attr,
                      IBV_QP_STATE |
                      IBV_QP_PKEY_INDEX |
                      IBV_QP_PORT |
                        IBV_QP_ACCESS_FLAGS );
    if (ret) {
        BTL_ERROR(("Error modifying QP[%x] to IBV_QPS_INIT errno says: %s [%d]",
                   (*qp)->qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }

    /* Setup meta data on the endpoint */
    *psn = udcm_random () & 0x00ffffff;

    /* Now that all the qp's are created locally, post some receive
       buffers, setup credits, etc. */
    return mca_btl_openib_endpoint_post_recvs(lcl_ep);
}

/* mark: xrc receive qp */

/* Recv qp connect */
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
static int udcm_xrc_recv_qp_connect (mca_btl_openib_endpoint_t *lcl_ep, uint32_t qp_num)
#else
static int udcm_xrc_recv_qp_connect (mca_btl_openib_endpoint_t *lcl_ep)
#endif
{
    mca_btl_openib_module_t *openib_btl = lcl_ep->endpoint_btl;

#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    struct ibv_qp_open_attr attr;
    memset(&attr, 0, sizeof(struct ibv_qp_open_attr));
    attr.comp_mask = IBV_QP_OPEN_ATTR_NUM | IBV_QP_OPEN_ATTR_XRCD | IBV_QP_OPEN_ATTR_TYPE;
    attr.qp_num = qp_num;
    attr.qp_type = IBV_QPT_XRC_RECV;
    attr.xrcd = openib_btl->device->xrcd;
    BTL_VERBOSE(("Connecting Recv QP\n"));
    lcl_ep->xrc_recv_qp = ibv_open_qp(openib_btl->device->ib_dev_context, &attr);
    if (NULL == lcl_ep->xrc_recv_qp) { /* failed to regester the qp, so it is already die and we should create new one */
       /* Return NOT READY !!!*/
        BTL_ERROR(("Failed to register qp_num: %d , get error: %s (%d)\n. Replying with RNR",
                   lcl_ep->xrc_recv_qp->qp_num, strerror(errno), errno));
        return OPAL_ERROR;
    } else {
        BTL_VERBOSE(("Connected to XRC Recv qp [%d]", lcl_ep->xrc_recv_qp->qp_num));
        return OPAL_SUCCESS;
    }
#else
    int ret;
    BTL_VERBOSE(("Connecting receive qp: %d", lcl_ep->xrc_recv_qp_num));
    ret = ibv_reg_xrc_rcv_qp(openib_btl->device->xrc_domain, lcl_ep->xrc_recv_qp_num);
    if (ret) { /* failed to regester the qp, so it is already die and we should create new one */
        /* Return NOT READY !!!*/
        lcl_ep->xrc_recv_qp_num = 0;
        BTL_ERROR(("Failed to register qp_num: %d , get error: %s (%d). Replying with RNR",
                   lcl_ep->xrc_recv_qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }
#endif

    return OPAL_SUCCESS;
}

/* Recv qp create */
static int udcm_xrc_recv_qp_create (mca_btl_openib_endpoint_t *lcl_ep, uint32_t rem_qp_num, uint32_t rem_psn)
{
    mca_btl_openib_module_t* openib_btl = lcl_ep->endpoint_btl;
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    struct ibv_qp_init_attr_ex qp_init_attr;
#else
    struct ibv_qp_init_attr qp_init_attr;
#endif
    struct ibv_qp_attr attr;
    int ret;

    BTL_VERBOSE(("creating xrc receive qp"));

#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr_ex));
    qp_init_attr.qp_type = IBV_QPT_XRC_RECV;
    qp_init_attr.comp_mask = IBV_QP_INIT_ATTR_XRCD;
    qp_init_attr.xrcd = openib_btl->device->xrcd;
    lcl_ep->xrc_recv_qp = ibv_create_qp_ex(openib_btl->device->ib_dev_context,
                                           &qp_init_attr);
    if (NULL == lcl_ep->xrc_recv_qp) {
        BTL_ERROR(("Error creating XRC recv QP, errno says: %s [%d]",
                    strerror(errno), errno));
        return OPAL_ERROR;
    }
#else
    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr));
    /* Only xrc_domain is required, all other are ignored */
    qp_init_attr.xrc_domain = openib_btl->device->xrc_domain;
    ret = ibv_create_xrc_rcv_qp(&qp_init_attr, &lcl_ep->xrc_recv_qp_num);
    if (ret) {
        BTL_ERROR(("Error creating XRC recv QP[%x], errno says: %s [%d]",
                   lcl_ep->xrc_recv_qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }
#endif

    memset(&attr, 0, sizeof(struct ibv_qp_attr));
    attr.qp_state = IBV_QPS_INIT;
    attr.pkey_index = openib_btl->pkey_index;
    attr.port_num = openib_btl->port_num;
    attr.qp_access_flags = IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ;

#if HAVE_DECL_IBV_ATOMIC_HCA
    attr.qp_access_flags |= IBV_ACCESS_REMOTE_ATOMIC;
#endif

#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    ret = ibv_modify_qp(lcl_ep->xrc_recv_qp,
            &attr,
            IBV_QP_STATE|
            IBV_QP_PKEY_INDEX|
            IBV_QP_PORT|
            IBV_QP_ACCESS_FLAGS);
    if (ret) {
        BTL_ERROR(("Error modifying XRC recv QP to IBV_QPS_INIT, errno says: %s [%d]",
                    strerror(ret), ret));
        return OPAL_ERROR;
    }
#else
    ret = ibv_modify_xrc_rcv_qp(openib_btl->device->xrc_domain,
                                lcl_ep->xrc_recv_qp_num, &attr,
                                IBV_QP_STATE | IBV_QP_PKEY_INDEX |
                                IBV_QP_PORT | IBV_QP_ACCESS_FLAGS);
    if (ret) {
        BTL_ERROR(("Error modifying XRC recv QP[%x] to IBV_QPS_INIT, errno says: %s [%d]",
                   lcl_ep->xrc_recv_qp_num, strerror(ret), ret));
        while(1);
        return OPAL_ERROR;
    }
#endif

    memset(&attr, 0, sizeof(struct ibv_qp_attr));
    attr.qp_state           = IBV_QPS_RTR;
    attr.path_mtu = (openib_btl->device->mtu < lcl_ep->rem_info.rem_mtu) ?
        openib_btl->device->mtu : lcl_ep->rem_info.rem_mtu;
    attr.dest_qp_num        = rem_qp_num;
    attr.rq_psn             = rem_psn;
    attr.max_dest_rd_atomic = mca_btl_openib_component.ib_max_rdma_dst_ops;
    attr.min_rnr_timer  = mca_btl_openib_component.ib_min_rnr_timer;
    attr.ah_attr.is_global     = 0;
    attr.ah_attr.dlid          = lcl_ep->rem_info.rem_lid;
    attr.ah_attr.src_path_bits = openib_btl->src_path_bits;
    attr.ah_attr.port_num      = openib_btl->port_num;
    attr.ah_attr.static_rate   = 0;
    attr.ah_attr.sl            = mca_btl_openib_component.ib_service_level;

#if (ENABLE_DYNAMIC_SL)
    /* if user enabled dynamic SL, get it from PathRecord */
    if (0 != mca_btl_openib_component.ib_path_record_service_level) {
        int rc = btl_openib_connect_get_pathrecord_sl(
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
                                                      openib_btl->device->xrcd->context,
#else
                                                      openib_btl->device->xrc_domain->context,
#endif
                                                      attr.ah_attr.port_num,
                                                      openib_btl->lid,
                                                      attr.ah_attr.dlid);
        if (OPAL_ERROR == rc) {
            return OPAL_ERROR;
        }
        attr.ah_attr.sl = rc;
    }
#endif

#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
    ret = ibv_modify_qp(lcl_ep->xrc_recv_qp,
            &attr,
            IBV_QP_STATE|
            IBV_QP_AV|
            IBV_QP_PATH_MTU|
            IBV_QP_DEST_QPN|
            IBV_QP_RQ_PSN|
            IBV_QP_MAX_DEST_RD_ATOMIC|
            IBV_QP_MIN_RNR_TIMER);
    if (ret) {
        BTL_ERROR(("Error modifying XRC recv QP to IBV_QPS_RTR, errno says: %s [%d]",
                    strerror(ret), ret));
        return OPAL_ERROR;
    }
#else
    ret = ibv_modify_xrc_rcv_qp(openib_btl->device->xrc_domain,
                                lcl_ep->xrc_recv_qp_num,
                                &attr,
            IBV_QP_STATE|
            IBV_QP_AV|
            IBV_QP_PATH_MTU|
            IBV_QP_DEST_QPN|
            IBV_QP_RQ_PSN|
            IBV_QP_MAX_DEST_RD_ATOMIC|
                                IBV_QP_MIN_RNR_TIMER);
    if (ret) {
        BTL_ERROR(("Error modifying XRC recv QP[%x] to IBV_QPS_RTR, errno says: %s [%d]",
                   lcl_ep->xrc_recv_qp_num, strerror(ret), ret));
        return OPAL_ERROR;
    }
#endif
    if (APM_ENABLED) {
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
        mca_btl_openib_load_apm(lcl_ep->xrc_recv_qp, lcl_ep);
#else
        mca_btl_openib_load_apm_xrc_rcv(lcl_ep->xrc_recv_qp_num, lcl_ep);
#endif
    }

    return OPAL_SUCCESS;
}

/* mark: xrc message functions */

static int udcm_xrc_send_request (mca_btl_base_endpoint_t *lcl_ep, mca_btl_base_endpoint_t *rem_ep,
                                  uint8_t msg_type)
{
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    udcm_message_sent_t *msg;
    int rc;

    assert (UDCM_MESSAGE_XCONNECT == msg_type || UDCM_MESSAGE_XCONNECT2 == msg_type);

    BTL_VERBOSE(("sending xrc request for endpoint %p", (void *) lcl_ep));

    if (0 != (rc = udcm_new_message (lcl_ep, rem_ep, msg_type,
                                     sizeof (udcm_msg_hdr_t), &msg))) {
        return rc;
    }

    msg->data->hdr.data.req.rem_ep_index = htonl(lcl_ep->index);
    msg->data->hdr.data.req.rem_port_num = m->modex.mm_port_num;

    if (UDCM_MESSAGE_XCONNECT == msg_type) {
        BTL_VERBOSE(("Sending XConnect with qp: %d, psn: %d", lcl_ep->qps[0].qp->lcl_qp->qp_num,
                     lcl_ep->qps[0].qp->lcl_psn));
        msg->data->hdr.data.xreq.rem_qp_num = htonl(lcl_ep->qps[0].qp->lcl_qp->qp_num);
        msg->data->hdr.data.xreq.rem_psn    = htonl(lcl_ep->qps[0].qp->lcl_psn);
    } else {
        BTL_VERBOSE(("Sending XConnect2 with qp: %d", lcl_ep->ib_addr->remote_xrc_rcv_qp_num));
        msg->data->hdr.data.xreq.rem_qp_num = htonl(lcl_ep->ib_addr->remote_xrc_rcv_qp_num);
    }

    if (0 != (rc = udcm_post_send (lcl_ep, msg->data, sizeof (udcm_msg_hdr_t), 0))) {
        BTL_VERBOSE(("error posting XREQ"));

        udcm_cancel_message_timeout (msg);

        return rc;
    }

    return 0;
}

static int udcm_xrc_send_xresponse (mca_btl_base_endpoint_t *lcl_ep, mca_btl_base_endpoint_t *rem_ep,
                                    uint8_t msg_type)
{
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    udcm_message_sent_t *msg;
    int rc;

    assert (UDCM_MESSAGE_XRESPONSE == msg_type || UDCM_MESSAGE_XRESPONSE2 == msg_type);

    if (0 != (rc = udcm_new_message (lcl_ep, rem_ep, msg_type, m->msg_length, &msg))) {
        return rc;
    }

    msg->data->hdr.data.xres.rem_ep_index = htonl(lcl_ep->index);

    if (UDCM_MESSAGE_XRESPONSE == msg_type) {
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
        BTL_VERBOSE(("Sending qp: %d, psn: %d", lcl_ep->xrc_recv_qp->qp_num, lcl_ep->xrc_recv_psn));
        msg->data->hdr.data.xres.rem_qp_num = htonl(lcl_ep->xrc_recv_qp->qp_num);
        msg->data->hdr.data.xres.rem_psn    = htonl(lcl_ep->xrc_recv_psn);
#else
        BTL_VERBOSE(("Sending qp: %d, psn: %d", lcl_ep->xrc_recv_qp_num, lcl_ep->xrc_recv_psn));
        msg->data->hdr.data.xres.rem_qp_num = htonl(lcl_ep->xrc_recv_qp_num);
        msg->data->hdr.data.xres.rem_psn    = htonl(lcl_ep->xrc_recv_psn);
#endif
    }

    for (int i = 0; i < mca_btl_openib_component.num_xrc_qps; ++i) {
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
        uint32_t srq_num;
        if (ibv_get_srq_num(lcl_ep->endpoint_btl->qps[i].u.srq_qp.srq, &srq_num)) {
            BTL_ERROR(("BTL openib XOOB internal error: can't get srq num"));
        }
        BTL_VERBOSE(("Sending srq[%d] num  = %d", i, srq_num));
        msg->data->qps[i].qp_num = htonl(srq_num);
#else
        BTL_VERBOSE(("Sending srq[%d] num  = %d", i, lcl_ep->endpoint_btl->qps[i].u.srq_qp.srq->xrc_srq_num));
        msg->data->qps[i].qp_num = htonl(lcl_ep->endpoint_btl->qps[i].u.srq_qp.srq->xrc_srq_num);
#endif
    }

    rc = udcm_post_send (lcl_ep, msg->data, m->msg_length, 0);
    if (0 != rc) {
        BTL_VERBOSE(("error posting complete"));

        udcm_cancel_message_timeout (msg);

        return rc;
    }

    return 0;
}

/* mark: xrc message handling */

static int udcm_xrc_handle_xconnect (mca_btl_openib_endpoint_t *lcl_ep, udcm_msg_hdr_t *msg_hdr)
{
    udcm_reject_reason_t rej_reason = UDCM_REJ_REMOTE_ERROR;
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    int response_type;
    int rc = OPAL_ERROR;

    do {
        if (NULL == udep) {
            break;
        }

        if (udep->recv_req) {
            /* duplicate request */
            return OPAL_SUCCESS;
        }

        udep->recv_req = true;

        opal_mutex_lock (&udep->udep_lock);

        if (UDCM_MESSAGE_XCONNECT2 == msg_hdr->type) {
            response_type = UDCM_MESSAGE_XRESPONSE2;
#if OPAL_HAVE_CONNECTX_XRC_DOMAINS
            rc = udcm_xrc_recv_qp_connect (lcl_ep, msg_hdr->data.xreq.rem_qp_num);
#else
            rc = udcm_xrc_recv_qp_connect (lcl_ep);
#endif
            if (OPAL_SUCCESS != rc) {
                /* return not ready. remote side will retry */
                rej_reason = UDCM_REJ_NOT_READY;
                break;
            }
        }

        /* prepost receives */
        rc = mca_btl_openib_endpoint_post_recvs (lcl_ep);
        if (OPAL_SUCCESS != rc) {
            break;
        }

        /* Create local QP's and post receive resources */
        if (UDCM_MESSAGE_XCONNECT == msg_hdr->type) {
            BTL_VERBOSE(("Initialized QPs, LID = %d", ((mca_btl_openib_module_t *) lcl_ep->endpoint_btl)->lid));

            response_type = UDCM_MESSAGE_XRESPONSE;

            rc = udcm_xrc_recv_qp_create (lcl_ep, msg_hdr->data.xreq.rem_qp_num, msg_hdr->data.xreq.rem_psn);
            if (OPAL_SUCCESS != rc) {
                break;
            }
        }

        rc = udcm_xrc_send_xresponse (lcl_ep, msg_hdr->rem_ep, response_type);
        if (OPAL_SUCCESS != rc) {
            break;
        }

        opal_mutex_unlock (&udep->udep_lock);

        return OPAL_SUCCESS;
    } while (0);

    opal_mutex_unlock (&udep->udep_lock);

    /* Reject the request */
    BTL_VERBOSE(("rejecting request for reason %d", rej_reason));

    udcm_send_reject (lcl_ep, msg_hdr->rem_ep, rej_reason);

    if (OPAL_SUCCESS != rc) {
        /* Communicate to the upper layer that the connection on this
           endpoint has failed */
        mca_btl_openib_endpoint_invoke_error (lcl_ep);
    }

    return rc;
}

static int udcm_xrc_handle_xresponse (mca_btl_openib_endpoint_t *lcl_ep, udcm_msg_hdr_t *msg_hdr)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    int rc;

    BTL_VERBOSE(("finishing xrc connection for endpoint %p.", (void *) lcl_ep));

    /* duplicate message */
    if (udep->recv_resp) {
        return OPAL_SUCCESS;
    }

    udep->recv_resp = true;

    rc = udcm_xrc_send_qp_connect (lcl_ep, msg_hdr->data.xres.rem_qp_num, msg_hdr->data.xres.rem_psn);
    if (OPAL_SUCCESS != rc) {
        mca_btl_openib_endpoint_invoke_error (lcl_ep);
    }

    return udcm_finish_connection (lcl_ep);
}
#endif
