/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.  All
 *                         rights reserved.
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
 * This connection method uses a two-step process:
 * Step 1 (CONNECT):
 *   A connect request is sent/received using an unconnected
 *   datagram queue pair.
 * Step 2 (SYNC):
 *   The connection is then synced by sending a 0-byte request
 *   on a per-peer rc queue pair.
 *    * This step is required to avoid a race condition between
 *      the last UD message and the first RC BTL message.
 *
 * There are two ways a connection can be established by UD:
 *  1. One side starts a connection and the request is received before
 *    the receiving side starts a connection. (One sided)
 *  2. Both sides send a request before either receives a request.
 *    (Simulaneous). The sync sender/receiver are determined by
 *    each peer's lid and listen qp.
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
 *            SYNC |<----->| SYNC
 *                 |       |
 *
 * The protocol for case 2 looks like:
 *             peer1       peer2
 *                 |       |
 *         CONNECT |<----->| CONNECT
 * move QPs to RTS |       | move QPs to RTS
 *    post rc send |       | post rc recv
 *            SYNC |<----->| SYNC
 *
 */

#include "ompi_config.h"

#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <infiniband/verbs.h>
#include <signal.h>

#include <pthread.h>

#include "opal/util/output.h"
#include "opal/util/error.h"
#include "opal_stdint.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/show_help.h"

#include "btl_openib_endpoint.h"
#include "btl_openib_proc.h"
#include "btl_openib_fd.h"
#include "connect/connect.h"

#if (ENABLE_DYNAMIC_SL)
#include "connect/btl_openib_connect_sl.h"
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
    ompi_btl_openib_connect_base_module_t cpc;

    /* This mutex must be held by any thread modifying
       the module directly */
    opal_mutex_t        cm_lock;

    /* Signal callbacks and threads that this module
       is exiting */
    bool                cm_exiting;

    /* UD QP this module is listening on */
    struct ibv_qp *listen_qp;

    /* Work request completion queues */
    struct ibv_cq *cm_send_cq, *cm_recv_cq, *cm_sync_cq;

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
    pthread_t       cm_timeout_thread;
    pthread_cond_t  cm_timeout_cond;
    pthread_mutex_t cm_timeout_lock;

    /* Messages waiting for ack */
    opal_list_t  flying_messages;

    /* This mutex must be held when calling ibv_post_send
       or waiting on cm_send_cq */
    opal_mutex_t cm_send_lock;

    /* Receive queue */
    opal_mutex_t cm_recv_msg_queue_lock;
    opal_list_t  cm_recv_msg_queue;
    bool         cm_message_event_active;

    /* ID of next outgoing message */
    uint32_t     next_message_id;

    /* The associated BTL */
    struct mca_btl_openib_module_t *btl;

    /* This module's modex message */
    modex_msg_t modex;
} udcm_module_t;

/*
 * Per-endpoint UD data
 */
typedef struct {
    /* Lock for IPC between threads in the ud CPC */
    opal_mutex_t   udep_lock;

    struct ibv_ah *ah;
    struct ibv_qp *sync_qp;
    uint32_t       sync_psn;
    uint32_t       rem_sync_qpn, rem_sync_psn;

    bool           sent_req, recv_req;
    bool           lcl_init, rem_init;

    /* Has this endpoint's data been initialized */
    bool           udep_initialized, udep_created_qps;
} udcm_endpoint_t;

typedef struct udcm_qp_t {
    uint32_t qp_num;
    uint32_t psn;
} udcm_qp_t;

typedef enum udcm_message_type {
    UDCM_MESSAGE_CONNECT    = 100,
    UDCM_MESSAGE_REJECT     = 101
} udcm_message_type_t;

typedef enum {
    UDCM_REJ_REMOTE_ERROR,
    UDCM_REJ_ALREADY_CONNECTED
} udcm_reject_reason_t;

typedef struct udcm_msg_hdr {
    udcm_message_type_t type;
    uint32_t id;

    /* endpoint local to the sender */
    mca_btl_base_endpoint_t *rem_ep;
    /* endpoint local to the receiver */
    mca_btl_base_endpoint_t *lcl_ep;

    union {
	/* UDCM_MESSAGE_CONNECT */
	struct msg_connect {
	    int rem_ep_index;
	    uint8_t rem_port_num;
	} req;
	/* UDCM_MESSAGE_REJECT */
	struct msg_reject {
	    int reason;
	} rej;
    } data;

    /* If the message type is UDCM_MESSAGE_CONNECT then
       QPs will follow the header */
    /* udcm_qps_t qps[]; */ /* uncomment if we ever move to C99 */
} udcm_msg_hdr_t;

typedef struct udcm_message_recv {
    opal_list_item_t super;

    udcm_msg_hdr_t msg_hdr;
} udcm_message_recv_t;

static OBJ_CLASS_INSTANCE(udcm_message_recv_t, opal_list_item_t,
			  NULL, NULL);

typedef struct udcm_message_sent {
    opal_list_item_t         super;

    udcm_msg_hdr_t            *data;
    size_t                   length;
    mca_btl_base_endpoint_t *endpoint;

    int                      tries;
    struct timeval           expiration;
} udcm_message_sent_t;

static void udcm_sent_message_constructor (udcm_message_sent_t *);
static void udcm_sent_message_destructor (udcm_message_sent_t *);
static OBJ_CLASS_INSTANCE(udcm_message_sent_t, opal_list_item_t,
			  udcm_sent_message_constructor,
			  udcm_sent_message_destructor);

#define UDCM_ENDPOINT_MODULE(ep) ((udcm_module_t *)(ep)->endpoint_local_cpc)
#define UDCM_ENDPOINT_DATA(ep) ((udcm_endpoint_t *)(ep)->endpoint_local_cpc_data)
#define UDCM_ENDPOINT_REM_MODEX(ep)					\
    (((modex_msg_t *)(ep)->endpoint_remote_cpc_data->cbm_modex_message))

/*--------------------------------------------------------------------*/

static void udcm_component_register(void);
static int udcm_component_query(mca_btl_openib_module_t *btl, 
                                ompi_btl_openib_connect_base_module_t **cpc);
static int udcm_component_finalize(void);

/* Module methods */
static int udcm_endpoint_init(struct mca_btl_base_endpoint_t *lcl_ep);
static int udcm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                     mca_btl_base_endpoint_t *lcl_ep);
static int udcm_endpoint_finalize(struct mca_btl_base_endpoint_t *lcl_ep);
static int udcm_endpoint_init_data (mca_btl_base_endpoint_t *lcl_ep);
static int udcm_endpoint_init_qps (mca_btl_base_endpoint_t *lcl_ep);
static int udcm_module_finalize(mca_btl_openib_module_t *btl,
                                ompi_btl_openib_connect_base_module_t *cpc);

static void *udcm_cq_event_dispatch(int fd, int flags, void *context);
static void *udcm_message_callback (void *context);

static void udcm_set_message_timeout (udcm_message_sent_t *message);

static int udcm_module_init (udcm_module_t *m, mca_btl_openib_module_t *btl);

static int udcm_module_start_timeout_thread (udcm_module_t *m);
static void *udcm_module_timeout_start (void *context);

static int udcm_module_create_listen_qp (udcm_module_t *m);
static void udcm_module_destroy_listen_qp (udcm_module_t *m);

static int udcm_module_allocate_buffers (udcm_module_t *m);
static void udcm_module_destroy_buffers (udcm_module_t *m);

static int udcm_module_post_all_recvs (udcm_module_t *m);
static bool i_initiate(udcm_module_t *m, mca_btl_openib_endpoint_t *lcl_ep);

static int udcm_send_request (mca_btl_base_endpoint_t *lcl_ep,
			      mca_btl_base_endpoint_t *rem_ep);

/*--------------------------------------------------------------------*/

#define UDCM_MIN_RECV_COUNT 1024
#define UDCM_MIN_TIMEOUT    1000000

#define UDCM_SEND_CQ_SIZE   1024
#define UDCM_SYNC_CQ_SIZE   1024

#define UDCM_WR_RECV_ID  0x20000000ll
#define UDCM_WR_SEND_ID  0x10000000ll
#define UDCM_WR_DIR_MASK 0x30000000ll

/* Useless 40 bytes of data that proceeds received scatter gather data.
   Can we get rid of this? */
#define UDCM_GRH_SIZE (sizeof (struct ibv_grh))

/* Priority of this connection module */
static int udcm_priority        = 27;

/* Number of receive work requests to post */
static int udcm_recv_count      = UDCM_MIN_RECV_COUNT;
static int udcm_max_retry       = 10;

/* Message ACK timeout */
static int udcm_timeout         = UDCM_MIN_TIMEOUT; /* usec */

static struct timeval udcm_timeout_tv;

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

static inline struct ibv_cq *create_cq_compat(struct ibv_context *context,
					      int cqe, void *cq_context,
					      struct ibv_comp_channel *channel,
					      int comp_vector)
{
#if OMPI_IBV_CREATE_CQ_ARGS == 3
    return ibv_create_cq(context, cqe, channel);
#else
    return ibv_create_cq(context, cqe, cq_context, channel, comp_vector);
#endif
}

/*******************************************************************
 * Component
 *******************************************************************/

ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_udcm = {
    "udcm",
    udcm_component_register,
    NULL,
    udcm_component_query,
    udcm_component_finalize
};

static void udcm_component_register(void)
{
    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_udcm_priority",
                           "The selection method priority for ud",
                           false, false, udcm_priority, &udcm_priority);
    if (udcm_priority > 100) {
        udcm_priority = 100;
    } else if (udcm_priority < -1) {
        udcm_priority = 0;
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
			   "connect_udcm_recv_count",
			   "Number of connection buffers to post",
			   false, false, udcm_recv_count,
			   &udcm_recv_count);
    if (UDCM_MIN_RECV_COUNT > udcm_recv_count) {
	udcm_recv_count = UDCM_MIN_RECV_COUNT;
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
			   "connect_udcm_timeout",
			   "Microseconds to wait for ud connection response",
			   false, false, udcm_timeout,
			   &udcm_timeout);
    if (UDCM_MIN_TIMEOUT > udcm_timeout) {
	udcm_timeout = UDCM_MIN_TIMEOUT;
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
			   "connect_udcm_max_retry",
			   "Maximum number of times to retry sending a connection message",
			   false, false, udcm_max_retry, &udcm_max_retry);
}

static int udcm_component_query(mca_btl_openib_module_t *btl, 
				ompi_btl_openib_connect_base_module_t **cpc)
{
    udcm_module_t *m = NULL;
    int rc = OMPI_ERR_NOT_SUPPORTED;

    do {
	/* If we do not have struct ibv_device.transport_device, then
	   we're in an old version of OFED that is IB only (i.e., no
	   iWarp), so we can safely assume that we can use this CPC. */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
	if (BTL_OPENIB_CONNECT_BASE_CHECK_IF_NOT_IB(btl)) {
	    BTL_VERBOSE(("UD CPC only supported on InfiniBand; skipped on %s:%d",
			 ibv_get_device_name(btl->device->ib_dev),
			 btl->port_num));
	    break;
	}
#endif

#if HAVE_XRC
	if (mca_btl_openib_component.num_xrc_qps > 0) {
	    BTL_VERBOSE(("UD CPC does not support XRC QPs (yet)"));
	    break;
	}
#endif

	/* Allocate the module struct.  Use calloc so that it's safe to
	   finalize the module if something goes wrong. */
	m = calloc(1, sizeof(*m));
	if (NULL == m) {
	    BTL_ERROR(("malloc failed!"));
	    rc = OMPI_ERR_OUT_OF_RESOURCE;
	    break;
	}

	rc = udcm_module_init (m, btl);
	if (OMPI_SUCCESS != rc) {
	    break;
	}

	/* All done */
	*cpc = (ompi_btl_openib_connect_base_module_t *) m;
	BTL_VERBOSE(("available for use on %s:%d",
		     ibv_get_device_name(btl->device->ib_dev),
		     btl->port_num));

	return OMPI_SUCCESS;
    } while (0);

    udcm_module_finalize(btl, (ompi_btl_openib_connect_base_module_t *) m);
    if (OMPI_ERR_NOT_SUPPORTED == rc) {
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
    return OMPI_SUCCESS;
}

/*--------------------------------------------------------------------*/

/*******************************************************************
 * Module
 *******************************************************************/

static int udcm_endpoint_init (struct mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = lcl_ep->endpoint_local_cpc_data = 
        calloc(1, sizeof(udcm_endpoint_t));
    if (NULL == udep) {
        BTL_ERROR(("malloc failed!"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    OBJ_CONSTRUCT(&udep->udep_lock, opal_mutex_t);

    return OMPI_SUCCESS;
}

static int udcm_endpoint_finalize(struct mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    
    /* Free the stuff we allocated in udcm_endpoint_init */
    if (NULL != udep) {
	if (udep->ah) {
	    ibv_destroy_ah(udep->ah);
	}

	if (udep->sync_qp) {
	    ibv_destroy_qp(udep->sync_qp);
	}

	OBJ_DESTRUCT(&udep->udep_lock);

	free(lcl_ep->endpoint_local_cpc_data);
        lcl_ep->endpoint_local_cpc_data = NULL;
    }

    return OMPI_SUCCESS;
}

static int udcm_module_init (udcm_module_t *m, mca_btl_openib_module_t *btl)
{
    int rc = OMPI_ERR_NOT_SUPPORTED;

    BTL_VERBOSE(("created cpc module %p for btl %p",
		 (void*)m, (void*)btl));

    m->btl = btl;

    /* Create completion channel */
    m->cm_channel = ibv_create_comp_channel (btl->device->ib_dev_context);
    if (NULL == m->cm_channel) {
	BTL_VERBOSE(("error creating ud completion channel"));
	return OMPI_ERR_NOT_SUPPORTED;
    }

    /* Create completion queues */
    m->cm_recv_cq = create_cq_compat (btl->device->ib_dev_context,
				      udcm_recv_count, NULL,
				      m->cm_channel, 0);

    if (NULL == m->cm_recv_cq) {
	BTL_VERBOSE(("error creating ud recv completion queue"));
	return OMPI_ERR_NOT_SUPPORTED;
    }

    m->cm_send_cq = create_cq_compat (btl->device->ib_dev_context,
				      UDCM_SEND_CQ_SIZE, NULL, NULL, 0);
    if (NULL == m->cm_send_cq) {
	BTL_VERBOSE(("error creating ud send completion queue"));
	return OMPI_ERR_NOT_SUPPORTED;
    }

    m->cm_sync_cq = create_cq_compat (btl->device->ib_dev_context,
				      UDCM_SYNC_CQ_SIZE, NULL,
				      m->cm_channel, 0);
    if (NULL == m->cm_sync_cq) {
	BTL_VERBOSE(("error creating ud sync completion queue"));
	return OMPI_ERR_NOT_SUPPORTED;
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
    m->cpc.data.cbm_component = &ompi_btl_openib_connect_udcm;
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
    ompi_btl_openib_fd_monitor(m->cm_channel->fd, OPAL_EV_READ,
			       udcm_cq_event_dispatch, m);

    OBJ_CONSTRUCT(&m->cm_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&m->cm_send_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&m->cm_recv_msg_queue, opal_list_t);
    OBJ_CONSTRUCT(&m->cm_recv_msg_queue_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&m->flying_messages, opal_list_t);

    pthread_mutex_init (&m->cm_timeout_lock, NULL);

    /* start timeout thread */
    pthread_cond_init (&m->cm_timeout_cond, NULL);
    if (0 != (rc = udcm_module_start_timeout_thread (m))) {
	return rc;
    }

    udcm_timeout_tv.tv_sec  = udcm_timeout / 1000000;
    udcm_timeout_tv.tv_usec = udcm_timeout - 1000000 *
	udcm_timeout_tv.tv_sec;

    m->cm_message_event_active = false;

    /* Finally, request CQ notification */
    if (0 != ibv_req_notify_cq (m->cm_recv_cq, 0)) {
	BTL_VERBOSE(("error requesting recv completions"));
	return OMPI_ERROR;
    }

    if (0 != ibv_req_notify_cq (m->cm_sync_cq, 0)) {
	BTL_VERBOSE(("error requesting sync completions"));
	return OMPI_ERROR;
    }

    /* Ready to use */

    return OMPI_SUCCESS;
}

static int
udcm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
			  mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    int rc = OMPI_SUCCESS;

    BTL_VERBOSE(("endpoint %p (lid %d, ep index %d)", 
		 (void*)lcl_ep, lcl_ep->endpoint_btl->port_info.lid,
		 lcl_ep->index));

    opal_mutex_lock (&udep->udep_lock);

    do {
	opal_atomic_wmb ();

	if (MCA_BTL_IB_CLOSED != lcl_ep->endpoint_state) {
	    BTL_VERBOSE(("already ongoing %p. state = %d",
			 (void *) lcl_ep, lcl_ep->endpoint_state));
	    break;
	}

	lcl_ep->endpoint_state = MCA_BTL_IB_CONNECTING;

	if (OMPI_SUCCESS != (rc = udcm_endpoint_init_data (lcl_ep))) {
	    BTL_VERBOSE(("error initializing endpoint cpc data"));
	    break;
	}

	if (OMPI_SUCCESS != (rc = udcm_endpoint_init_qps (lcl_ep))) {
	    BTL_VERBOSE(("error initializing endpoint qps"));
	    break;
	}

	udep->lcl_init = true;

	rc = udcm_send_request (lcl_ep, NULL);
    } while (0);

    opal_mutex_unlock (&udep->udep_lock);

    return rc;
}

static int udcm_module_finalize(mca_btl_openib_module_t *btl,
				ompi_btl_openib_connect_base_module_t *cpc)
{
    udcm_module_t *m = (udcm_module_t *) cpc;
    opal_list_item_t *item;

    if (NULL == m) {
	return OMPI_SUCCESS;
    }

    m->cm_exiting = true;

    opal_mutex_lock (&m->cm_lock);

    opal_mutex_lock (&m->cm_recv_msg_queue_lock);

    /* clear message queue */
    while ((item = opal_list_remove_first(&m->cm_recv_msg_queue))) {
	OBJ_RELEASE(item);
    }

    opal_mutex_unlock (&m->cm_recv_msg_queue_lock);

    OBJ_DESTRUCT(&m->cm_recv_msg_queue);

    pthread_mutex_lock (&m->cm_timeout_lock);
    while ((item = opal_list_remove_first(&m->flying_messages))) {
	OBJ_RELEASE(item);
    }

    /* wake up timeout thread */
    pthread_cond_signal (&m->cm_timeout_cond);

    pthread_mutex_unlock (&m->cm_timeout_lock);

    pthread_join (m->cm_timeout_thread, NULL);

    OBJ_DESTRUCT(&m->flying_messages);

    BTL_VERBOSE(("destroying listing thread"));

    /* destroy the listen queue pair. this will cause ibv_get_cq_event to
       return. */
    udcm_module_destroy_listen_qp (m);

    /* stop monitoring the channel's fd */
    ompi_btl_openib_fd_unmonitor(m->cm_channel->fd, NULL, NULL);

    udcm_module_destroy_buffers (m);

    if (m->cm_sync_cq) {
	if (0 != ibv_destroy_cq (m->cm_sync_cq)) {
	    BTL_VERBOSE(("failed to destroy sync CQ. errno = %d",
			 errno));
	}
    }

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

    pthread_mutex_destroy (&m->cm_timeout_lock);
    pthread_cond_destroy (&m->cm_timeout_cond);

    return OMPI_SUCCESS;
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
	return OMPI_ERROR;
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
	return OMPI_ERROR;
    } 

    /* Move listen QP to RTR */
    attr.qp_state = IBV_QPS_RTR;

    if (0 != ibv_modify_qp(qp, &attr, IBV_QP_STATE)) {
	BTL_ERROR(("error modifing QP to RTR errno says %s",
		   strerror(errno)));
	return OMPI_ERROR; 
    }


    /* Move listen QP to RTS */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state = IBV_QPS_RTS;
    attr.sq_psn = 0;

    if (0 != ibv_modify_qp(qp, &attr, IBV_QP_STATE | IBV_QP_SQ_PSN)) {
	BTL_ERROR(("error modifing QP to RTS errno says %s; errno=%d",
		   strerror(errno), errno));
	return OMPI_ERROR; 
    }

    m->listen_qp = qp;

    BTL_VERBOSE(("listening for connections on lid %d, qpn %d",
		 m->btl->lid, qp->qp_num));

    return OMPI_SUCCESS;
}

static void udcm_module_destroy_listen_qp (udcm_module_t *m)
{
    enum ibv_qp_attr_mask attr_mask;
    struct ibv_qp_attr attr;
    struct ibv_wc wc;

    if (NULL == m->listen_qp) {
	return;
    }

    do {
	/* Move listen QP into the ERR state to cancel all outstanding
	   work requests */
	memset(&attr, 0, sizeof(attr));
	attr.qp_state = IBV_QPS_RTS;
	attr.sq_psn = 0;
	attr_mask = IBV_QP_STATE | IBV_QP_SQ_PSN;

	attr.qp_state = IBV_QPS_ERR;

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

    /* Send one extra qp for the sync qp */
    m->msg_length   = sizeof (udcm_msg_hdr_t) +
	(mca_btl_openib_component.num_qps + 1) * sizeof (udcm_qp_t);

    total_size = (udcm_recv_count + 1) * (m->msg_length +
					  UDCM_GRH_SIZE);

    m->cm_buffer = NULL;
    posix_memalign ((void **)&m->cm_buffer, sysconf(_SC_PAGESIZE),
		    total_size);
    if (NULL == m->cm_buffer) {
        BTL_ERROR(("malloc failed! errno = %d", errno));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* mark buffer memory as initialized for valgrind's sake */
    memset (m->cm_buffer, 0, total_size);

    m->cm_mr = ibv_reg_mr (m->btl->device->ib_pd, m->cm_buffer,
			   total_size, IBV_ACCESS_LOCAL_WRITE |
			   IBV_ACCESS_REMOTE_WRITE);
    if (NULL == m->cm_mr) {
	BTL_ERROR(("failed to register memory. errno = %d", errno));
	return OMPI_ERROR;
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

    return (0 == rc) ? OMPI_SUCCESS : OMPI_ERROR;
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
static bool i_initiate(udcm_module_t *m, mca_btl_openib_endpoint_t *ep)
{
    modex_msg_t *msg = UDCM_ENDPOINT_REM_MODEX(ep);

    if (m->modex.mm_lid == msg->mm_lid &&
	m->modex.mm_qp_num == msg->mm_qp_num) {
	opal_output (0, "BAD THINGS");
    }

    return
        (m->modex.mm_lid == msg->mm_lid &&
         m->modex.mm_qp_num < msg->mm_qp_num) ? true :
        (m->modex.mm_lid < msg->mm_lid) ? true : false;
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
    attr_mask = IBV_QP_STATE | IBV_QP_PKEY_INDEX | IBV_QP_PORT |
	IBV_QP_ACCESS_FLAGS;

    if (0 != ibv_modify_qp(qp, &attr, attr_mask)) {
	BTL_ERROR(("error modifying qp to INIT errno says %s",
		   strerror(errno)));
	return OMPI_ERROR; 
    }

    return OMPI_SUCCESS;
}

static int udcm_create_sync_qp (mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    struct ibv_qp_init_attr init_attr;
    int rc;

    memset (&init_attr, 0, sizeof (init_attr));

    init_attr.qp_type = IBV_QPT_RC;
    init_attr.send_cq = m->cm_sync_cq;
    init_attr.recv_cq = m->cm_sync_cq;
    init_attr.cap.max_send_sge = 1;
    init_attr.cap.max_recv_sge = 1;
    init_attr.cap.max_recv_wr  = 1;
    init_attr.cap.max_send_wr  = 1;

    udep->sync_qp = ibv_create_qp(m->btl->device->ib_pd, &init_attr);
    if (NULL == udep->sync_qp) {
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "ibv_create_qp failed", true, orte_process_info.nodename,
                       ibv_get_device_name(m->btl->device->ib_dev),
		       "Reliable connected (RC)");

        return OMPI_ERROR;
    }

    /* Move the QP into the INIT state */
    rc = udcm_rc_qp_to_init (udep->sync_qp, m->btl);
    if (OMPI_SUCCESS != rc) {
	ibv_destroy_qp (udep->sync_qp);
	udep->sync_qp = NULL;
	return rc;
    }

    udep->sync_psn = lrand48() & 0xffffff;

    return OMPI_SUCCESS;
}

static int udcm_sync_qp_to_rts (mca_btl_base_endpoint_t *lcl_ep)
{
    mca_btl_openib_module_t *btl = lcl_ep->endpoint_btl;
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    struct ibv_qp_attr attr;
    enum ibv_mtu mtu;
    int rc;

    mtu = (btl->device->mtu < lcl_ep->rem_info.rem_mtu) ?
        btl->device->mtu : lcl_ep->rem_info.rem_mtu;

    BTL_VERBOSE(("transitioning sync QP %p to RTS", (void *) udep->sync_qp));

    /* Move the QP into the RTR state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state           = IBV_QPS_RTR;
    /* Setup attributes */
    attr.path_mtu           = mtu;
    attr.dest_qp_num        = udep->rem_sync_qpn;
    attr.rq_psn             = udep->rem_sync_psn;
    attr.max_dest_rd_atomic = 0;
    attr.min_rnr_timer      = 14;

    attr.ah_attr.is_global     = 0;
    attr.ah_attr.dlid          = lcl_ep->rem_info.rem_lid;
    attr.ah_attr.src_path_bits = btl->src_path_bits;
    attr.ah_attr.port_num      = btl->port_num;
    attr.ah_attr.sl            = mca_btl_openib_component.ib_service_level;

    rc = ibv_modify_qp(udep->sync_qp, &attr,IBV_QP_STATE | IBV_QP_PATH_MTU |
		       IBV_QP_RQ_PSN | IBV_QP_AV | IBV_QP_DEST_QPN |
		       IBV_QP_MAX_DEST_RD_ATOMIC | IBV_QP_MIN_RNR_TIMER);
    if (OPAL_UNLIKELY(0 != rc)) {
	BTL_ERROR(("error modifing QP to RTR errno says %s",
		   strerror(errno)));

	return OMPI_ERROR; 
    }

    BTL_VERBOSE(("transitioning sync QP %p to RTS", (void *)udep->sync_qp));

    /* Move the QP into the RTS state */
    memset(&attr, 0, sizeof(attr));
    attr.qp_state       = IBV_QPS_RTS;
    attr.sq_psn         = udep->sync_psn;
    attr.timeout        = 14;
    attr.retry_cnt      = 7;
    attr.rnr_retry      = 7;
    attr.max_rd_atomic  = 0;

    rc = ibv_modify_qp(udep->sync_qp, &attr, IBV_QP_STATE | IBV_QP_SQ_PSN |
		       IBV_QP_TIMEOUT | IBV_QP_RETRY_CNT |
		       IBV_QP_MAX_QP_RD_ATOMIC | IBV_QP_RNR_RETRY);
    if (OPAL_UNLIKELY(0 != rc)) {
	BTL_ERROR(("error modifing QP %p to RTS errno says %s",
		   (void *) udep->sync_qp, strerror(errno)));
	return OMPI_ERROR; 
    }

    return OMPI_SUCCESS;
}

/*
 * Create the local side of one qp.  The remote side will be connected
 * later.
 */
static int udcm_qp_create_one(udcm_module_t *m, mca_btl_base_endpoint_t* lcl_ep,
			      int qp, struct ibv_srq *srq, uint32_t max_recv_wr,
			      uint32_t max_send_wr)
{
    struct ibv_qp_init_attr init_attr;
    size_t req_inline;

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

    lcl_ep->qps[qp].qp->lcl_qp = ibv_create_qp(m->btl->device->ib_pd,
					       &init_attr);
    if (NULL == lcl_ep->qps[qp].qp->lcl_qp) { 
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "ibv_create_qp failed", true, orte_process_info.nodename,
                       ibv_get_device_name(m->btl->device->ib_dev),
		       "Reliable connected (RC)");

        return OMPI_ERROR;
    }

    if (init_attr.cap.max_inline_data < req_inline) {
        lcl_ep->qps[qp].ib_inline_max = init_attr.cap.max_inline_data;
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", true, orte_process_info.nodename,
                       ibv_get_device_name(m->btl->device->ib_dev),
                       m->btl->port_num, req_inline,
		       init_attr.cap.max_inline_data);
    } else {
        lcl_ep->qps[qp].ib_inline_max = req_inline;
    }

    /* Setup meta data on the endpoint */
    lcl_ep->qps[qp].qp->lcl_psn = lrand48() & 0xffffff;
    lcl_ep->qps[qp].credit_frag = NULL;

    return OMPI_SUCCESS;
}

/*
 * Create the local side of all the qp's.  The remote sides will be
 * connected later.
 * NTH: This code is common to (and repeated by) all non-XRC cpcs.
 */
static int udcm_qp_create_all(mca_btl_base_endpoint_t *lcl_ep, udcm_module_t *m)
{
    int qp, rc, pp_qp_num = 0;
    int32_t rd_rsv_total = 0;

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
        rc = udcm_qp_create_one (m, lcl_ep, qp, srq, max_recv_wr, max_send_wr);
        if (OMPI_SUCCESS != rc) {
            return rc;
        }
    }

    /* All done! */

    return OMPI_SUCCESS;
}

static inline int udcm_wait_for_send_completion (udcm_module_t *m)
{
    struct ibv_wc wc;
    int rc;

    do {
	rc = ibv_poll_cq (m->cm_send_cq, 1, &wc);
	if (0 > rc) {
	    BTL_VERBOSE(("send failed"));
	    return OMPI_ERROR;
	} else if (0 == rc) {
	    continue;
	} else if (IBV_WC_SUCCESS != wc.status) {
	    BTL_ERROR(("send failed with verbs status %d", wc.status));
	    return OMPI_ERROR;
	}

	break;
    } while (1);

    return OMPI_SUCCESS;
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

	memcpy ((void *)sge.addr, data, length);
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

static int udcm_new_message (mca_btl_base_endpoint_t *lcl_ep,
			     mca_btl_base_endpoint_t *rem_ep, int type,
			     size_t length, udcm_message_sent_t **msgp)
{
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    udcm_message_sent_t *message;

    message = OBJ_NEW(udcm_message_sent_t);
    if (NULL == message) {
	BTL_ERROR(("malloc failed!"));
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    message->data = calloc (m->msg_length, 1);
    if (NULL == message->data) {
	OBJ_RELEASE(message);
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    message->length       = length;

    message->data->rem_ep = lcl_ep;
    message->data->lcl_ep = rem_ep;
    message->data->type   = type;

    message->endpoint = lcl_ep;

    opal_atomic_wmb ();
    opal_mutex_lock(&m->cm_lock);
    message->data->id = m->next_message_id++;
    opal_mutex_unlock(&m->cm_lock);

    *msgp = message;

    BTL_VERBOSE(("created message %d", message->data->id));

    return OMPI_SUCCESS;
}

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
    udcm_qp_t *qps;
    int i, rc;

    BTL_VERBOSE(("sending request for endpoint %p", (void *) lcl_ep));

    udep->sent_req = true;

    if (0 != (rc = udcm_new_message (lcl_ep, rem_ep, UDCM_MESSAGE_CONNECT,
				     m->msg_length, &msg))) {
	return rc;
    }

    msg->data->data.req.rem_ep_index = lcl_ep->index;
    msg->data->data.req.rem_port_num = m->modex.mm_port_num;

    qps = (udcm_qp_t *)(msg->data + 1);

    for (i = 0 ; i < mca_btl_openib_component.num_qps ; ++i) {
	qps[i].psn    = lcl_ep->qps[i].qp->lcl_psn;
	qps[i].qp_num = lcl_ep->qps[i].qp->lcl_qp->qp_num;
	/* NTH: TODO -- add XRC support */
    }
  
    qps[mca_btl_openib_component.num_qps].psn    = udep->sync_psn;
    qps[mca_btl_openib_component.num_qps].qp_num = udep->sync_qp->qp_num;

    if (0 != (rc = udcm_post_send (lcl_ep, msg->data, m->msg_length, 0))) {
	BTL_VERBOSE(("error posting REQ"));
	return rc;
    }

    udcm_set_message_timeout (msg);

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

    msg->data->data.rej.reason  = rej_reason;

    rc = udcm_post_send (lcl_ep, msg->data, sizeof (udcm_msg_hdr_t), 0);
    if (0 != rc) {

	BTL_VERBOSE(("error posting rejection"));
	return rc;
    }

    udcm_set_message_timeout (msg);

    return 0;
}

static int udcm_send_ack (mca_btl_base_endpoint_t *lcl_ep, uint32_t id)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    struct ibv_send_wr wr, *bad_wr;
    int rc;

    /* NTH: need to lock here or we run into problems */
    opal_mutex_lock(&m->cm_send_lock);

    wr.wr_id      = UDCM_WR_SEND_ID;
    wr.next       = NULL;
    wr.num_sge    = 0;
    wr.opcode     = IBV_WR_SEND_WITH_IMM;
    wr.send_flags = IBV_SEND_SOLICITED | IBV_SEND_SIGNALED;
    wr.wr.ud.ah   = udep->ah;

    wr.imm_data   = id;

    wr.wr.ud.remote_qpn  = UDCM_ENDPOINT_REM_MODEX(lcl_ep)->mm_qp_num;
    wr.wr.ud.remote_qkey = 0;

    rc = ibv_post_send (m->listen_qp, &wr, &bad_wr);
    if (0 != rc) {
	BTL_VERBOSE(("error posting ack. errno: %d, rc: %d", errno, rc));
    } else {
	rc = udcm_wait_for_send_completion (m);
    }

    opal_mutex_unlock (&m->cm_send_lock);

    return rc;
}

static int udcm_handle_ack (udcm_module_t *m, const uint32_t id)
{
    opal_list_item_t *item;
    udcm_message_sent_t *msg = NULL;

    pthread_mutex_lock (&m->cm_timeout_lock);

    BTL_VERBOSE(("got ack for message %d", id));

    for (item = opal_list_get_first (&m->flying_messages) ;
	 item != opal_list_get_end (&m->flying_messages) ;
	 item = opal_list_get_next (item)) {
	msg = (udcm_message_sent_t *) item;

	if (msg->data && id == msg->data->id) {
	    opal_list_remove_item (&m->flying_messages, item);
	    
	    break;
	}
	msg = NULL;
    }

    pthread_mutex_unlock (&m->cm_timeout_lock);

    return OMPI_SUCCESS;
}

static int udcm_endpoint_init_data (mca_btl_base_endpoint_t *lcl_ep)
{
    modex_msg_t *remote_msg = UDCM_ENDPOINT_REM_MODEX(lcl_ep);
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    struct ibv_ah_attr ah_attr;
    int rc = OMPI_SUCCESS;

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
	    rc = OMPI_ERROR;
	    break;
	}
    } while (0);

    if (OMPI_SUCCESS == rc) {
	udep->udep_initialized = true;
    }

    return rc;
}

static int udcm_endpoint_init_qps (mca_btl_base_endpoint_t *lcl_ep)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    int rc = OMPI_SUCCESS;

    do {
	if (udep->udep_created_qps)
	    break;

	/* Make QPs */
	if (OMPI_SUCCESS != (rc = udcm_qp_create_all(lcl_ep, m))) {
	    BTL_VERBOSE(("error creating qps for endpoint %p",
		       (void *) lcl_ep));
	    break;
	}

	if (OMPI_SUCCESS != (rc = udcm_create_sync_qp (lcl_ep))) {
	    BTL_VERBOSE(("could not create sync qp for endpoint %p",
		       (void *) lcl_ep));
	    break;
	}	
    } while (0);

    if (OMPI_SUCCESS == rc) {
	udep->udep_created_qps = true;
    }

    return rc;
}
/*--------------------------------------------------------------------*/

/*
 * We have received information about the remote peer's QP; move the
 * local QP from INIT to RTS through RTR.
 */
static int udcm_qps_to_rts(mca_btl_openib_endpoint_t *lcl_ep)
{
    mca_btl_openib_module_t *btl = lcl_ep->endpoint_btl;
    struct ibv_qp_attr attr;
    enum ibv_mtu mtu;
    int qp_index, rc;

    mtu = (btl->device->mtu < lcl_ep->rem_info.rem_mtu) ?
        btl->device->mtu : lcl_ep->rem_info.rem_mtu;

    BTL_VERBOSE(("Set MTU to IBV value %d (%s bytes)", mtu,
                 (mtu == IBV_MTU_256) ? "256" :
                 (mtu == IBV_MTU_512) ? "512" :
                 (mtu == IBV_MTU_1024) ? "1024" :
                 (mtu == IBV_MTU_2048) ? "2048" :
                 (mtu == IBV_MTU_4096) ? "4096" :
                 "unknown (!)"));

    for (qp_index = 0 ; qp_index < mca_btl_openib_component.num_qps ;
	 ++qp_index) {
	struct ibv_qp *qp = lcl_ep->qps[qp_index].qp->lcl_qp;

	rc = udcm_rc_qp_to_init (qp, lcl_ep->endpoint_btl);
	if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
	    break;
	}

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
            if (OMPI_ERROR == rc) {
                return OMPI_ERROR;
            }
            attr.ah_attr.sl = rc;
        }
#endif

	rc = ibv_modify_qp(qp, &attr, IBV_QP_STATE | IBV_QP_PATH_MTU |
			   IBV_QP_MAX_DEST_RD_ATOMIC | IBV_QP_MIN_RNR_TIMER |
			   IBV_QP_RQ_PSN | IBV_QP_AV | IBV_QP_DEST_QPN);
	if (OPAL_UNLIKELY(0 != rc)) {
	    BTL_ERROR(("error modifing QP to RTR errno says %s", strerror(errno)));
	    return OMPI_ERROR; 
	}

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
	    return OMPI_ERROR; 
	}

	BTL_VERBOSE(("successfully set RTS"));
    }

    /* All done */
    return OMPI_SUCCESS;
}

/* JMS: optimization target -- can we send something in private
   data to find the proc directly instead of having to search
   through *all* procs? */
static mca_btl_openib_endpoint_t *udcm_find_endpoint (opal_pointer_array_t *endpoints,
						      uint32_t qp_num, uint8_t port_num,
						      uint16_t lid)
{
    int i;

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

    return NULL;
}

static int udcm_setup_qps (mca_btl_base_endpoint_t *lcl_ep)
{
    int rc;

    if (OMPI_SUCCESS != (rc = udcm_qps_to_rts (lcl_ep))) {
	BTL_VERBOSE(("failed moving QP(s) to RTS"));

	return rc;
    }

    if (OMPI_SUCCESS != (rc = udcm_sync_qp_to_rts (lcl_ep))) {
	BTL_VERBOSE(("failed to move sync QP to RTS"));

	return rc;
    }

    /* Ensure that all the writes back to the endpoint and associated
       data structures have completed */
    opal_atomic_wmb();
    mca_btl_openib_endpoint_post_recvs(lcl_ep);

    return OMPI_SUCCESS;
}

static int udcm_sync_connection (mca_btl_openib_endpoint_t *lcl_ep, bool i_send)
{
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    struct ibv_send_wr send_wr, *bad_send_wr;
    struct ibv_recv_wr recv_wr, *bad_recv_wr;
    int rc;

    if (NULL == udep->sync_qp) {
	/* already synced? */
	return OMPI_SUCCESS;
    }

    BTL_VERBOSE(("syncing connection for local endpoint %p", (void *) lcl_ep));

    opal_mutex_lock(&m->cm_send_lock);

    if (i_send) {
	send_wr.wr_id      = (uint64_t)lcl_ep;
	send_wr.next       = NULL;
	send_wr.sg_list    = NULL;
	send_wr.num_sge    = 0;
	send_wr.opcode     = IBV_WR_SEND;
	send_wr.send_flags = IBV_SEND_SIGNALED | IBV_SEND_SOLICITED;

	rc = ibv_post_send (udep->sync_qp, &send_wr, &bad_send_wr);
	if (0 != rc) {
	    opal_output (0, "error posting sync send wr. errno = %d", errno);
	    return OMPI_ERROR;
	}
    } else {
	recv_wr.wr_id      = (uint64_t)lcl_ep;
	recv_wr.next       = NULL;
	recv_wr.sg_list    = NULL;
	recv_wr.num_sge    = 0;

	rc = ibv_post_recv (udep->sync_qp, &recv_wr, &bad_recv_wr);
	if (0 != rc) {
	    opal_output (0, "error posting sync recv wr. errno = %d", errno);
	    return OMPI_ERROR;
	}
    }

    opal_mutex_unlock(&m->cm_send_lock);

    return OMPI_SUCCESS;
}

static int udcm_handle_connect(mca_btl_openib_endpoint_t *lcl_ep,
			       mca_btl_openib_endpoint_t *rem_ep)
{
    udcm_reject_reason_t rej_reason = UDCM_REJ_REMOTE_ERROR;
    udcm_endpoint_t *udep = UDCM_ENDPOINT_DATA(lcl_ep);
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(lcl_ep);
    int rc = OMPI_ERROR;
    bool i_send;

    do {
	if (NULL == udep) {
	    break;
	}

	opal_mutex_lock (&udep->udep_lock);

	if (true == udep->recv_req) {
	    /* this endpoint is already */
	    BTL_VERBOSE(("already connected"));
	    rc = OMPI_SUCCESS;
	    rej_reason = UDCM_REJ_ALREADY_CONNECTED;
	    break;
	}

	udep->recv_req = true;

	opal_atomic_wmb ();
	if (MCA_BTL_IB_CLOSED == lcl_ep->endpoint_state) {
	    lcl_ep->endpoint_state = MCA_BTL_IB_CONNECTING;
	}

	if (OMPI_SUCCESS != (rc = udcm_endpoint_init_qps (lcl_ep))) {
	    BTL_VERBOSE(("error initializing endpoint qps"));
	    break;
	}

	rc = udcm_setup_qps (lcl_ep);
	if (OMPI_SUCCESS != rc) {
	    break;
	}

	if (false == udep->lcl_init) {
	    /* Remote initiated. local should receive */
	    i_send = false;
	} else if (false == udep->rem_init) {
	    /* Local initiated. remote should receive */
	    i_send = true;
	} else {
	    /* Simultaneous connection attempt. Break tie. */
	    i_send = i_initiate (m, lcl_ep);
	}

	/* post receive (passive) or send (active) */
	rc = udcm_sync_connection (lcl_ep, i_send);
	if (OMPI_SUCCESS != rc) {
	    break;
	}

	if (false == udep->sent_req) {
	    rc = udcm_send_request (lcl_ep, rem_ep);

	    if (OMPI_SUCCESS != rc) {
		break;
	    }
	}

	opal_mutex_unlock (&udep->udep_lock);

	return OMPI_SUCCESS;
    } while (0);

    opal_mutex_unlock (&udep->udep_lock);

    /* Reject the request */
    BTL_VERBOSE(("rejecting request for reason %d", rej_reason));

    udcm_send_reject (lcl_ep, rem_ep, rej_reason);

    if (OMPI_SUCCESS != rc) {
	/* Communicate to the upper layer that the connection on this
	   endpoint has failed */
	mca_btl_openib_endpoint_invoke_error (lcl_ep);
    }

    return rc;
}

static int udcm_handle_reject(mca_btl_openib_endpoint_t *lcl_ep,
			      udcm_msg_hdr_t *msg_hdr)
{
    BTL_VERBOSE(("reject received: reason %d", msg_hdr->data.rej.reason));

    if (UDCM_REJ_ALREADY_CONNECTED == msg_hdr->data.rej.reason) {
	return OMPI_SUCCESS;
    }

    /* Communicate to the upper layer that the connection on this
       endpoint has failed */
    mca_btl_openib_endpoint_invoke_error (lcl_ep);

    return OMPI_ERR_NOT_FOUND;
}

static int udcm_finish_connection (mca_btl_openib_endpoint_t *lcl_ep)
{
    BTL_VERBOSE(("finishing connection for endpoint %p.", (void *) lcl_ep));

    /* Ensure that all the writes back to the endpoint and associated
       data structures have completed */
    opal_atomic_wmb();

    /* Need to hold the endpoint lock before calling cpc_complete */
    OPAL_THREAD_LOCK(&lcl_ep->endpoint_lock);
    mca_btl_openib_endpoint_cpc_complete(lcl_ep);

    return OMPI_SUCCESS;
}

static int udcm_process_messages (struct ibv_cq *event_cq, udcm_module_t *m)
{
    mca_btl_openib_endpoint_t *lcl_ep;
    int msg_num, i, count, qp_index;
    udcm_msg_hdr_t *msg_hdr = NULL;
    udcm_message_recv_t *item;
    struct ibv_wc wc[20];
    udcm_endpoint_t *udep;
    uint64_t dir;
    bool rem_init;

    count = ibv_poll_cq (event_cq, 20, wc);
    if (count < 0)
	return count;

    for (i = 0 ; i < count && !m->cm_exiting ; i++) {
	dir = wc[i].wr_id & UDCM_WR_DIR_MASK;

	if (UDCM_WR_RECV_ID != dir) {
	    opal_output (0, "unknown packet");
	    continue;
	}

	msg_num = (int)(wc[i].wr_id & (~UDCM_WR_DIR_MASK));

	if (IBV_WC_SUCCESS != wc[i].status) {
	    BTL_VERBOSE(("recv work request for buffer %d failed, code = %d",
			 msg_num, wc[i].status));
	    count = -1;
	    break;
	}

	msg_hdr = (udcm_msg_hdr_t *) udcm_module_get_recv_buffer (m, msg_num, true);

	if (wc[i].wc_flags & IBV_WC_WITH_IMM) {
	    /* ack! */
	    udcm_handle_ack (m, wc[i].imm_data);
	    udcm_module_post_one_recv (m, msg_num);

	    continue;
	}

	lcl_ep = msg_hdr->lcl_ep;

	if (NULL == lcl_ep) {
	    lcl_ep = udcm_find_endpoint (m->btl->device->endpoints,
					 wc[i].src_qp,
					 msg_hdr->data.req.rem_port_num,
					 wc[i].slid);
	    rem_init = true;
	} else {
	    rem_init = false;
	}

	if (NULL == lcl_ep ) {
	    /* cant find associated endpoint */
	    BTL_ERROR(("could not find associated endpoint."));
	    udcm_module_post_one_recv (m, msg_num);

	    continue;
	}

	msg_hdr->lcl_ep = lcl_ep;

	BTL_VERBOSE(("received message. type: %d, lcl_ep = %p, rem_ep = %p, "
		     "src qpn = %d, length = %d, local buffer # = %d",
		     msg_hdr->type, (void *) msg_hdr->lcl_ep, (void *) msg_hdr->rem_ep,
		     wc[i].src_qp, wc[i].byte_len, msg_num));
	
	udep = UDCM_ENDPOINT_DATA(lcl_ep);

	if (NULL == udep) {
	    /* Endpoint was not initialized or was finalized */
	    udcm_module_post_one_recv (m, msg_num);
	    continue;
	}

	opal_mutex_lock (&udep->udep_lock);

	/* Need to ensure endpoint data is initialized before sending the ack */
	if (OMPI_SUCCESS != udcm_endpoint_init_data (lcl_ep)) {
	    BTL_ERROR(("could not initialize cpc data for endpoint"));
	    udcm_module_post_one_recv (m, msg_num);
	    continue;
	}

	if (UDCM_MESSAGE_CONNECT == msg_hdr->type) {
	    /* Save remote queue pair information */
	    udcm_qp_t *rem_qps = (udcm_qp_t *)(msg_hdr + 1);

	    udep->rem_init = rem_init;
	    lcl_ep->rem_info.rem_index = msg_hdr->data.req.rem_ep_index;

	    for (qp_index = 0 ; qp_index < mca_btl_openib_component.num_qps ;
		 ++qp_index) {
		/* Save these numbers on the endpoint for reference. */
		lcl_ep->rem_info.rem_qps[qp_index].rem_psn    =
		    rem_qps[qp_index].psn;
		lcl_ep->rem_info.rem_qps[qp_index].rem_qp_num =
		    rem_qps[qp_index].qp_num;
	    }

	    udep->rem_sync_qpn = rem_qps[qp_index].qp_num;
	    udep->rem_sync_psn = rem_qps[qp_index].psn;
	}

	opal_mutex_unlock (&udep->udep_lock);

	item = OBJ_NEW(udcm_message_recv_t);

	/* Copy just the message header */
	memcpy (&item->msg_hdr, msg_hdr, sizeof (item->msg_hdr));

	opal_mutex_lock(&m->cm_recv_msg_queue_lock);
	opal_list_append (&m->cm_recv_msg_queue, &item->super);
	opal_mutex_unlock(&m->cm_recv_msg_queue_lock);
	
	udcm_send_ack (lcl_ep, msg_hdr->id);

	/* Repost the receive */
	udcm_module_post_one_recv (m, msg_num);
    }

    if (!m->cm_exiting) {
	opal_mutex_lock (&m->cm_recv_msg_queue_lock);
	if (opal_list_get_size (&m->cm_recv_msg_queue) &&
	    !m->cm_message_event_active) {
	    m->cm_message_event_active = true;
 	    ompi_btl_openib_fd_run_in_main (udcm_message_callback, (void *) m);
	}
	opal_mutex_unlock (&m->cm_recv_msg_queue_lock);
    }

    return count;
}

static int udcm_process_sync (udcm_module_t *m)
{
    struct ibv_cq *event_cq = m->cm_sync_cq;
    mca_btl_openib_endpoint_t *lcl_ep;
    struct ibv_wc wc[20];
    udcm_endpoint_t *udep;
    int rc, i, count;

    count = ibv_poll_cq (event_cq, 20, wc);
    if (count < 0)
	return count;

    for (i = 0 ; i < count && !m->cm_exiting ; i++) {
	lcl_ep = (mca_btl_openib_endpoint_t *) wc[i].wr_id;
	udep   = UDCM_ENDPOINT_DATA(lcl_ep);

	BTL_VERBOSE(("sync complete for endpoint %p", (void *) lcl_ep));

	if (IBV_WC_SUCCESS != wc[i].status) {
	    opal_output (0, "sync work completion failed with status %d",
			 wc[i].status);
	    continue;
	}

	udcm_finish_connection (lcl_ep);

	rc = ibv_destroy_qp (udep->sync_qp);
	if (0 != rc) {
	    opal_output (0, "error. could not destroy sync qp");
	}
	udep->sync_qp = NULL;
    }

    return 0;
}

static void *udcm_cq_event_dispatch(int fd, int flags, void *context)
{
    udcm_module_t *m = (udcm_module_t *) context;
    struct ibv_cq *event_cq = m->cm_recv_cq;
    void *event_context;
    int rc;

    assert (NULL != m && NULL != m->cm_channel);

    rc = ibv_get_cq_event (m->cm_channel, &event_cq, &event_context);

    if (0 != rc || NULL == event_cq) {
	return NULL;
    }

    /* acknowlege the event */
    ibv_ack_cq_events (event_cq, 1);

    if (event_cq == m->cm_recv_cq) {
	rc = udcm_process_messages (event_cq, m);
	if (rc < 0) {
	    BTL_VERBOSE(("error processing incomming messages"));
	    return NULL;
	}
    } else {
	rc = udcm_process_sync (m);
	if (rc < 0) {
	    BTL_VERBOSE(("error processing incomming messages"));
	    return NULL;
	}	
    }

    if (false == m->cm_exiting) {
	if (ibv_req_notify_cq(event_cq, 0)) {
	    BTL_VERBOSE(("error asking for cq notifications"));
	}
    }

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
	    break;
	case UDCM_MESSAGE_REJECT:
	    udcm_handle_reject (lcl_ep, &item->msg_hdr);
	    break;
	default:
	    BTL_VERBOSE(("unknown message type"));
	}

	OPAL_THREAD_UNLOCK(&lcl_ep->endpoint_lock);

	OBJ_RELEASE (item);
	
	opal_mutex_lock(&m->cm_recv_msg_queue_lock);
    }

    BTL_VERBOSE(("exiting message thread"));

    m->cm_message_event_active = false;
    opal_mutex_unlock(&m->cm_recv_msg_queue_lock);

    return NULL;
}

static void udcm_sent_message_constructor (udcm_message_sent_t *message)
{
    memset ((char *)message + sizeof (message->super), 0,
	    sizeof (*message) - sizeof (message->super));
}

static void udcm_sent_message_destructor (udcm_message_sent_t *message)
{
    if (message->data) {
	free (message->data);
    }
}


/* Message timeouts */
static void udcm_send_timeout (udcm_module_t *m, udcm_message_sent_t *msg)
{
    mca_btl_base_endpoint_t *lcl_ep = msg->endpoint;

    do {
	BTL_VERBOSE(("send for message %d timed out (msg = %p)", msg->data->id,
		     (void *) msg));

	/* This happens from time to time at the end of a run (probably die to a
	   lost ack) */
	if (NULL == lcl_ep->endpoint_local_cpc_data) {
	    OBJ_RELEASE (msg);
	    break;
	}

	if (msg->tries == udcm_max_retry) {
	    opal_output (0, "too many retries sending message to %d, giving up",
			 UDCM_ENDPOINT_REM_MODEX(lcl_ep)->mm_qp_num);

	    /* We are running in the timeout thread. Invoke the error in the
	       main thread */
	    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error,
					   lcl_ep);
	    break;
	}

	msg->tries++;

	if (0 != udcm_post_send (lcl_ep, msg->data, msg->length, 0)) {
	    BTL_VERBOSE(("error reposting message"));
	    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error,
					   lcl_ep);
	    break;
	}

	udcm_set_message_timeout (msg);
    } while (0);
}

static void udcm_set_message_timeout (udcm_message_sent_t *message)
{
    udcm_module_t *m = UDCM_ENDPOINT_MODULE(message->endpoint);
    struct timeval now;

    gettimeofday (&now, NULL);
    timeradd(&now, &udcm_timeout_tv, &message->expiration);

    pthread_mutex_lock (&m->cm_timeout_lock);
    opal_list_append (&m->flying_messages, &message->super);

    if (opal_list_get_size (&m->flying_messages) == 1) {
	pthread_cond_signal (&m->cm_timeout_cond);
    }

    pthread_mutex_unlock (&m->cm_timeout_lock);
}

static void *udcm_module_timeout_start (void *context)
{
    udcm_module_t *m = (udcm_module_t *) context;
    struct timespec ts = {0, 10000000};
    udcm_message_sent_t *cur_message;
    struct timeval now;

    pthread_mutex_lock (&m->cm_timeout_lock);
    do {
	while (!m->cm_exiting && 0 == opal_list_get_size (&m->flying_messages)) {
 	    pthread_cond_wait (&m->cm_timeout_cond, &m->cm_timeout_lock);
	}

	if (m->cm_exiting) {
	    break;
	}

	do {
	    if (0 == opal_list_get_size (&m->flying_messages)) {
		break;
	    }

	    pthread_mutex_unlock (&m->cm_timeout_lock);
	    /* sleep for a little while (10000 ms) */
	    nanosleep (&ts, NULL);
	    pthread_mutex_lock (&m->cm_timeout_lock);

	    if (m->cm_exiting) {
		break;
	    }

	    do {
		gettimeofday (&now, NULL);
		cur_message = (udcm_message_sent_t *)
		    opal_list_get_first (&m->flying_messages);

		if (opal_list_get_end (&m->flying_messages) == &cur_message->super ||
		    timercmp(&cur_message->expiration, &now, >)) {
		    break;
		}

		cur_message = (udcm_message_sent_t *)
		    opal_list_remove_first (&m->flying_messages);


		pthread_mutex_unlock (&m->cm_timeout_lock);
		udcm_send_timeout (m, cur_message);
		pthread_mutex_lock (&m->cm_timeout_lock);
	    } while (!m->cm_exiting);
	} while (!m->cm_exiting);
    } while (!m->cm_exiting);

    pthread_mutex_unlock (&m->cm_timeout_lock);

    return NULL;
}

static int udcm_module_start_timeout_thread (udcm_module_t *m)
{
    if (pthread_create (&m->cm_timeout_thread, NULL, udcm_module_timeout_start,
			(void *) m)) {
	BTL_VERBOSE(("error creating ud timeout thread"));
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
