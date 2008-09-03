/*
 * Copyright (c) 2007-2008 Cisco, Inc.  All rights reserved.
 * Copyright (c) 2007-2008 Chelsio, Inc. All rights reserved.
 * Copyright (c) 2008      Mellanox Technologies. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <rdma/rdma_cma.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <net/if.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <dirent.h>
#include <malloc.h>

#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "orte/util/show_help.h"

#include "btl_openib_fd.h"
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"
#include "connect/connect.h"
#include "btl_openib_iwarp.h"
#include "orte/util/show_help.h"

/* JMS to be removed: see #1264 */
#undef event

static void rdmacm_component_register(void);
static int rdmacm_component_init(void);
static int rdmacm_component_query(mca_btl_openib_module_t *openib_btl,
                                  ompi_btl_openib_connect_base_module_t **cpc);
static int rdmacm_component_destroy(void);

ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_rdmacm = {
    "rdmacm",
    rdmacm_component_register,
    rdmacm_component_init,
    rdmacm_component_query,
    rdmacm_component_destroy
};

typedef struct {
    bool server;
    mca_btl_openib_endpoint_t *endpoint;
    mca_btl_openib_module_t *openib_btl;
    struct ibv_cq *dummy_cq;
    struct rdma_cm_id **id;
    uint32_t ipaddr;
    uint16_t tcp_port;
} rdmacm_contents_t;

typedef struct {
    uint32_t ipaddr;
    uint16_t tcp_port;
} message_t;

typedef struct {
    int rdmacm_counter;
} rdmacm_endpoint_local_cpc_data_t;

typedef struct {
    rdmacm_contents_t *local;
    mca_btl_openib_endpoint_t *endpoint;
    uint8_t qpnum;
} id_contexts_t;

typedef struct {
    uint32_t rem_index;
    uint16_t rem_port;
    uint8_t qpnum;
} conn_message_t;

typedef struct {
    opal_list_item_t super;
    rdmacm_contents_t *item;
} list_item_t;

static OBJ_CLASS_INSTANCE(list_item_t, opal_list_item_t, NULL, NULL);

static opal_list_t server_list;
static opal_list_t client_list;
static struct rdma_event_channel *event_channel = NULL;
static int rdmacm_priority = 30;
static uint16_t rdmacm_port = 0;
static uint32_t rdmacm_addr = 0;

#define RDMACM_RESOLVE_ADDR_TIMEOUT 2000

/* Open - this functions sets up any rdma_cm specific commandline params */
static void rdmacm_component_register(void)
{
    int port, addr;

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdmacm_priority",
                           "The selection method priority for rdma_cm",
                           false, false, rdmacm_priority, &rdmacm_priority);

    if (rdmacm_priority > 100) {
        rdmacm_priority = 100;
    } else if (rdmacm_priority < 0) {
        rdmacm_priority = 0;
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdmacm_addr",
                           "The selection method address for rdma_cm",
                           false, false, rdmacm_addr, &addr);
    /*FIXME -sanity check these values */
    if (0 != addr) {
        rdmacm_addr = (uint32_t) addr;
    }

    mca_base_param_reg_int(&mca_btl_openib_component.super.btl_version,
                           "connect_rdmacm_port",
                           "The selection method port for rdma_cm",
                           false, false, rdmacm_port, &port);
    /*FIXME -sanity check these values */
    if (0 != port) {
        rdmacm_port = (uint16_t) port;
    }
}

/* This function traverses the list of endpoints associated with the device
 * and determines which of them the remote side is attempting to connect
 * to.  This is determined based on the local endpoint's modex message
 * recevied and the IP address and port associated with the rdma_cm
 * event id
 */
static mca_btl_openib_endpoint_t *rdmacm_find_endpoint(rdmacm_contents_t *local,
                                                       struct rdma_cm_id *id,
                                                       uint16_t rem_port)
{
    int i;
    mca_btl_openib_endpoint_t *ep = NULL;
    opal_pointer_array_t *endpoints = local->openib_btl->device->endpoints;

    for (i = 0; i < opal_pointer_array_get_size(endpoints); i++) {
        mca_btl_openib_endpoint_t *ib_endpoint;
        message_t *message;
        struct sockaddr *peeraddr;
        uint32_t peeripaddr;

        ib_endpoint = opal_pointer_array_get_item(endpoints, i);
        if (NULL == ib_endpoint) {
            continue;
        }

        message = ib_endpoint->endpoint_remote_cpc_data->cbm_modex_message;
        peeraddr = rdma_get_peer_addr(id);
        peeripaddr = ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;

        BTL_VERBOSE(("message ipaddr = %x port %d, rdma_get_peer_addr = %x",
                     message->ipaddr, message->tcp_port, peeripaddr));
        if (message->ipaddr == peeripaddr && message->tcp_port == rem_port) {
            ep = ib_endpoint;
            break;
        }
    }

    if (NULL == ep)
        BTL_ERROR(("can't find suitable endpoint for this peer"));

    return ep;
}

static void rdmacm_cleanup(rdmacm_contents_t *local,
                           struct rdma_cm_id *id,
                           uint32_t num)
{
    if (NULL == id) {
        return;
    }

    free(id->context);
    id->context = NULL;

    rdma_destroy_id(id);
    if (!local->server && local->id && local->id[num]) {
        local->id[num] = NULL;
    }
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


static int rdmacm_setup_qp(rdmacm_contents_t *local,
                           mca_btl_openib_endpoint_t *endpoint,
                           struct rdma_cm_id *id,
                           int qpnum)
{
    struct ibv_qp_init_attr attr;
    struct ibv_qp *qp;
    struct ibv_srq *srq = NULL;
    int credits = 0, reserved = 0, max_recv_wr, max_send_wr;
    size_t req_inline;

    if (qpnum == mca_btl_openib_component.credits_qp) {
        int i;

        for (i = 0; i < mca_btl_openib_component.num_qps; i++) {
            reserved += mca_btl_openib_component.qp_infos[i].u.pp_qp.rd_rsv;
        }
        credits = mca_btl_openib_component.num_qps;
    }

    if (BTL_OPENIB_QP_TYPE_PP(qpnum)) {
        max_recv_wr = mca_btl_openib_component.qp_infos[qpnum].rd_num + reserved;
        max_send_wr = mca_btl_openib_component.qp_infos[qpnum].rd_num + credits;
    } else {
        srq = endpoint->endpoint_btl->qps[qpnum].u.srq_qp.srq;
        max_recv_wr = reserved;
        max_send_wr = mca_btl_openib_component.qp_infos[qpnum].u.srq_qp.sd_max + credits;
    }

    memset(&attr, 0, sizeof(attr));
    attr.qp_type = IBV_QPT_RC;
    attr.send_cq = local->openib_btl->device->ib_cq[BTL_OPENIB_LP_CQ];
    attr.recv_cq = local->openib_btl->device->ib_cq[qp_cq_prio(qpnum)];
    attr.srq = srq;
    /* Add one for the CTS receive frag that will be posted */
    attr.cap.max_recv_wr = max_recv_wr + 1;
    attr.cap.max_send_wr = max_send_wr;
    attr.cap.max_inline_data = req_inline = 
        max_inline_size(qpnum, local->openib_btl->device);
    attr.cap.max_send_sge = 1;
    attr.cap.max_recv_sge = 1; /* we do not use SG list */

    qp = ibv_create_qp(local->openib_btl->device->ib_pd, &attr);
    if (NULL == qp) {
        BTL_ERROR(("Failed to create qp with %d", qp));
        goto out;
    }

    endpoint->qps[qpnum].qp->lcl_qp = qp;
    if (attr.cap.max_inline_data < req_inline) {
        endpoint->qps[qpnum].ib_inline_max = attr.cap.max_inline_data;
        orte_show_help("help-mpi-btl-openib-cpc-base.txt",
                       "inline truncated", orte_process_info.nodename,
                       ibv_get_device_name(local->openib_btl->device->ib_dev),
                       req_inline, attr.cap.max_inline_data);
    } else {
        endpoint->qps[qpnum].ib_inline_max = req_inline;
    }
    id->qp = qp;

    return 0;

out:
    return 1;
}

static int rdmacm_client_connect_one(rdmacm_contents_t *local,
                                     message_t *message,
                                     int num)
{
    struct sockaddr_in din;
    id_contexts_t *context;
    int rc;

    /* We'll need to access some data in the event handler.  We can
     * encapsulate it in this data struct and attach it to the id being
     * created below.  The event->id will contain this same pointer.
     */
    context = malloc(sizeof(id_contexts_t));
    if (NULL == context) {
        BTL_ERROR(("malloc error"));
        goto out;
    }

    context->local = local;
    context->qpnum = num;
    context->endpoint = local->endpoint;

    rc = rdma_create_id(event_channel, &local->id[num], context, RDMA_PS_TCP);
    if (0 != rc) {
        BTL_ERROR(("Failed to create a rdma id with %d", rc));
        goto out1;
    }

    memset(&din, 0, sizeof(din));
    din.sin_family = AF_INET;
    din.sin_addr.s_addr = message->ipaddr;
    din.sin_port = message->tcp_port;

    /* Once the route to the remote system is discovered, a
     * RDMA_CM_EVENT_ADDR_RESOLVED event will occur on the local event
     * handler.
     */
    OPAL_OUTPUT((0, "Resolving id: 0x%x", local->id[num]));
    rc = rdma_resolve_addr(local->id[num],
                           NULL,
                           (struct sockaddr *)&din,
                           RDMACM_RESOLVE_ADDR_TIMEOUT);
    if (0 != rc) {
        BTL_ERROR(("Failed to resolve the remote address with %d", rc));
        goto out1;
    }

    return OMPI_SUCCESS;

out1:
    rdmacm_cleanup(local, local->id[num], num);
out:
    return OMPI_ERROR;
}

static char *stringify(uint32_t addr)
{
    char *line;
    asprintf(&line, "%d.%d.%d.%d", 
             addr & 0xff,
             (addr >> 8) & 0xff,
             (addr >> 16) & 0xff,
             (addr >> 24));
    return line;
}

/* To avoid all kinds of nasty race conditions, we only allow
 * connections to be made in one direction.  So use a simple
 * (arbitrary) test to decide which direction is allowed to initiate
 * the connection: the process with the lower IP address wins.  If the
 * IP addresses are the same (i.e., the MPI procs are on the same
 * node), then the process with the lower TCP port wins.
 */
static bool i_initiate(uint32_t local_ipaddr, uint16_t local_port,
                       uint32_t remote_ipaddr, uint16_t remote_port)
{
    char *a = stringify(local_ipaddr);
    char *b = stringify(remote_ipaddr);
    
    if (local_ipaddr > remote_ipaddr ||
        (local_ipaddr == remote_ipaddr && local_port < remote_port)) {
        OPAL_OUTPUT((0, "i_initiate (I WIN): local ipaddr %s, remote ipaddr %s",
                     a, b));
        free(a);
        free(b);
        return true;
    } else {
        OPAL_OUTPUT((0, "i_initiate (I lose): local ipaddr %s, remote ipaddr %s",
                     a, b));
        free(a);
        free(b);
        return false;
    }
}

static int rdmacm_client_connect(rdmacm_contents_t *local, message_t *message)
{
    int rc, qp;

    /* If we're not the initiator, allocate an extra ID for the bogus
       QP that we expect to be rejected */
    qp = mca_btl_openib_component.num_qps;
    if (!local->endpoint->endpoint_initiator) {
        ++qp;
    }
    local->id = calloc(qp, sizeof(struct rdma_cm_id *));
    if (NULL == local->id) {
        BTL_ERROR(("malloc error"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* If we're the initiator, then open all the QPs */
    if (local->endpoint->endpoint_initiator) {
        for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
            rc = rdmacm_client_connect_one(local, message, qp);
            if (OMPI_SUCCESS != rc) {
                BTL_ERROR(("rdmacm_client_connect_one error (real QP %d)", 
                           qp));
                goto out;
            }
        }
    }
    /* Otherwise, only open 1 QP that we expect to be rejected */
    else {
        rc = rdmacm_client_connect_one(local, message, qp - 1);
        if (OMPI_SUCCESS != rc) {
            BTL_ERROR(("rdmacm_client_connect_one error (bogus QP)"));
            goto out;
        }
    }

    return OMPI_SUCCESS;

out:
   for (; qp >= 0; qp--) {
       if (NULL != local->id[qp]) {
           rdmacm_cleanup(local, local->id[qp], qp);
       }
   }

   return rc;
}

/* Connect method called by the upper layers to connect the local
 * endpoint to the remote endpoint by creating QP(s) to connect the two.
 * Already holding endpoint lock when this function is called.
 */
static int rdmacm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                       mca_btl_base_endpoint_t *endpoint)
{
    rdmacm_contents_t *local;
    message_t *message, *local_message;
    int rc;

    /* Don't use the CPC to get the message, because this function is
       invoked from the event_handler (to intitiate connections in the
       Right direction), where we don't have the CPC, so it'll be
       NULL. */
    local_message = 
        (message_t *) endpoint->endpoint_local_cpc->data.cbm_modex_message;
    message = (message_t *)
        endpoint->endpoint_remote_cpc_data->cbm_modex_message;

    BTL_VERBOSE(("Connecting to remote ip addr = %x, port = %d  ep state = %d",
                 message->ipaddr, message->tcp_port, endpoint->endpoint_state));

    if (MCA_BTL_IB_CONNECTED == endpoint->endpoint_state ||
        MCA_BTL_IB_CONNECTING == endpoint->endpoint_state ||
        MCA_BTL_IB_CONNECT_ACK == endpoint->endpoint_state) {
        return OMPI_SUCCESS;
    }

    /* Set the endpoint state to "connecting" (this function runs in
       the main MPI thread; not the service thread, so we can set the
       endpoint_state here). */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;

    local = calloc(1, sizeof(rdmacm_contents_t));
    if (NULL == local) {
        BTL_ERROR(("malloc of local failed"));
        goto out;
    }

    local->openib_btl = endpoint->endpoint_btl;
    local->endpoint = endpoint;
    local->server = false;
    /* Populate the port information with the local port the server is
     * listening on instead of the ephemerial port this client is
     * connecting with.  This port is used to determine which endpoint
     * is being connected from, in the case where there are multiple
     * listeners on the local system.
     */
    local->ipaddr = local_message->ipaddr;
    local->tcp_port = local_message->tcp_port;

    /* Are we the initiator?  Or do we expect this connect request to
       be rejected? */
    endpoint->endpoint_initiator = 
        i_initiate(local->ipaddr, local->tcp_port, 
                   message->ipaddr, message->tcp_port);
    OPAL_OUTPUT((0, "Start connect; ep=0x%x (0x%x), I %s the initiator to %s",
                 endpoint,
                 endpoint->endpoint_local_cpc,
                 endpoint->endpoint_initiator ? "am" : "am NOT",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname));

    rc = rdmacm_client_connect(local, message);
    if (0 != rc) {
        BTL_ERROR(("rdmacm_client_connect error"));
        goto out;
    }

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
}

/* The server thread will handle the incoming connection requests and
 * allow them or reject them based on a unidirectional connection
 * method.  The choonections are allowed based on the IP address and
 * port values.  This determination is arbitrary, but is uniform in
 * allowing the connections only in 1 direction.  If the connection in
 * the requestion is disallowed by this rule, then the server will
 * reject the connection and make its own in the proper direction.
 */
static int handle_connect_request(rdmacm_contents_t *local,
                                  struct rdma_cm_event *event)
{
    mca_btl_openib_endpoint_t *endpoint;
    struct rdma_conn_param conn_param;
    message_t *message;
    conn_message_t msg;
    int rc = -1, qpnum;
    uint32_t rem_index;
    uint16_t rem_port;

    qpnum = ((conn_message_t *)event->param.conn.private_data)->qpnum;
    rem_port = ((conn_message_t *)event->param.conn.private_data)->rem_port;
    rem_index = ((conn_message_t *)event->param.conn.private_data)->rem_index;

    /* Determine which endpoint the remote side is trying to connect to */
    endpoint = rdmacm_find_endpoint(local, event->id, rem_port);
    if (NULL == endpoint) {
        BTL_ERROR(("Failed to find endpoint"));
        goto out;
    }

    message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    endpoint->endpoint_initiator = 
        i_initiate(local->ipaddr, local->tcp_port,
                   message->ipaddr, rem_port);

    BTL_VERBOSE(("ep state = %d, local ipaddr = %x, remote ipaddr = %x, local port = %d, remote port = %d",
                 endpoint->endpoint_state, local->ipaddr, message->ipaddr, 
                 local->tcp_port, rem_port));

    OPAL_OUTPUT((0, "in handle_connect_request; ep=0x%x (0x%x), I still %s the initiator to %s",
                 endpoint,
                 endpoint->endpoint_local_cpc,
                 endpoint->endpoint_initiator ? "am" : "am NOT",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname));
    if (endpoint->endpoint_initiator) {
        int race = 1;

        OPAL_OUTPUT((0, "Received a connect request from an endpoint in the wrong direction"));

        /* This will cause a event on the remote system.  By passing in
         * a value in the second arg of rdma_reject, the remote side
         * can check for this to know if it was an intentional reject or
         * a reject based on an error.
         */
        rc = rdma_reject(event->id, &race, sizeof(int));
        if (0 != rc) {
            BTL_ERROR(("rdma_reject failed %d", rc));
            goto out;
        }

        OPAL_OUTPUT((0, "Starting connection in other direction"));
        rdmacm_module_start_connect(NULL, endpoint);

        return 0;
    }

    /* Set the endpoint_state to "CONNECTING".  This is running
       in the service thread, so we need to do a write barrier. */
    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
    opal_atomic_wmb();

    endpoint->rem_info.rem_index = rem_index;

    /* Setup QP for new connection */
    BTL_VERBOSE(("ACCEPTING src port = %d, dst port = %d, qpnum = %d",
                 rdma_get_src_port(event->id), rdma_get_dst_port(event->id), qpnum));

    rc = rdmacm_setup_qp(local, endpoint, event->id, qpnum);
    if (0 != rc) {
        BTL_ERROR(("rdmacm_setup_qp error %d", rc));
        goto out;
    }

    /* Post a single receive buffer on the smallest QP for the CTS
       protocol */
    if (mca_btl_openib_component.credits_qp == qpnum) {
        struct ibv_recv_wr *bad_wr, *wr;

        assert(NULL != endpoint->endpoint_cts_frag);
        wr = &to_recv_frag(endpoint->endpoint_cts_frag)->rd_desc;
        assert(NULL != wr);
        wr->next = NULL;

        if (0 != ibv_post_recv(endpoint->qps[qpnum].qp->lcl_qp, 
                               wr, &bad_wr)) {
            BTL_ERROR(("failed to post CTS recv buffer"));
            goto out1;
        }
    }

    /* Since the event id is already created, we cannot add this
     * information in the normal way.  Instead we must reference its
     * location and put the data there so that it can be accessed later.
     */
    event->id->context = malloc(sizeof(id_contexts_t));
    if (NULL == event->id->context) {
        BTL_ERROR(("malloc error"));
        goto out1;
    }

    ((id_contexts_t *)event->id->context)->local = local;
    ((id_contexts_t *)event->id->context)->qpnum = qpnum;
    ((id_contexts_t *)event->id->context)->endpoint = endpoint;

    memset(&conn_param, 0, sizeof(conn_param));
    conn_param.responder_resources = 1;
    conn_param.initiator_depth = 1;
    conn_param.private_data = &msg;
    conn_param.private_data_len = sizeof(conn_message_t);

    msg.qpnum = qpnum;
    msg.rem_index = endpoint->index;

    /* Accepting the connection will result in a
     * RDMA_CM_EVENT_ESTABLISHED event on both the client and server
     * side.
     */
    rc = rdma_accept(event->id, &conn_param);
    if (0 != rc) {
        BTL_ERROR(("rdma_accept error %d", rc));
        goto out2;
    }

    return 0;

out2:
    free(event->id->context);
out1:
    ibv_destroy_qp(endpoint->qps[qpnum].qp->lcl_qp);
out:
    return -1;
}

static void *rdmacm_unmonitor(int fd, int flags, void *context)
{
    if (NULL != event_channel) {
        rdma_destroy_event_channel(event_channel);
        event_channel = NULL;
    }

    return NULL;
}

static void rdmacm_destroy(rdmacm_contents_t *local)
{
    int i;
    bool last = true;

    for (i = 0; i < mca_btl_openib_component.num_qps; i++) {
        if (NULL != local->id[i] || local->server) {
            last = false;
            break;
        }
    }

    if (last) {
        free(local->id);
        local->id = NULL;
        free(local);
    }
}

static void rdmacm_server_cleanup(rdmacm_contents_t *local)
{
        rdma_destroy_id(local->id[0]);
        free(local->id);
        local->id = NULL;
        free(local);
}

static int rdmacm_connection_shutdown(struct mca_btl_base_endpoint_t *endpoint)
{
    opal_list_item_t *item;

    BTL_VERBOSE(("Start disconnecting..."));

    if (NULL == endpoint) {
        BTL_ERROR(("Attempting to shutdown a NULL endpoint"));
        return 0;
    }

    /* Determine which id cooreleates to the endpoint we should now be
     * shutting down.  By disconnecting instead of simply destroying the
     * QPs,we are shutting down in a more graceful way thus preventing
     * errors on the line.
     */
    for (item = opal_list_get_first(&client_list);
         item != opal_list_get_end(&client_list);
         item = opal_list_get_next(item)) {
        list_item_t *cli = (list_item_t *)item;

        if (endpoint == cli->item->endpoint) {
            int i;
            for (i = 0; i < mca_btl_openib_component.num_qps; i++)
                if (NULL != cli->item->id[i] &&
                    NULL != cli->item->id[i]->qp &&
                    NULL != cli->item->endpoint->qps) {
                    opal_output(0, "Freeing rdmacm id %p",
                                cli->item->id[i]);
                    rdma_disconnect(cli->item->id[i]);
                    /* JMS shouldn't be necessary */
                    cli->item->id[i] = NULL;
                }
        }
    }

    return 0;
}

/*
 * Callback (from main thread) when the endpoint has been connected
 */
static void *local_endpoint_cpc_complete(void *context)
{
    mca_btl_openib_endpoint_t *endpoint = (mca_btl_openib_endpoint_t *)context;

    OPAL_OUTPUT((0, "local_endpoint_cpc_complete to %s",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname));
    mca_btl_openib_endpoint_cpc_complete(endpoint);

    return NULL;
}

static int rdmacm_connect_endpoint(rdmacm_contents_t *local, struct rdma_cm_event *event)
{
    rdmacm_endpoint_local_cpc_data_t *data;
    mca_btl_openib_endpoint_t *endpoint;
    message_t *message;

    if (local->server) {
        endpoint = ((id_contexts_t *)event->id->context)->endpoint;
        OPAL_OUTPUT((0, "Server CPC complete to %s",
                     endpoint->endpoint_proc->proc_ompi->proc_hostname));
    } else {
        list_item_t *li;
        uint32_t rem_index;

        rem_index = ((conn_message_t *)event->param.conn.private_data)->rem_index;

        endpoint = local->endpoint;
        local->endpoint->rem_info.rem_index = rem_index;

        li = OBJ_NEW(list_item_t);
        if (NULL == li) {
            BTL_ERROR(("malloc error"));
            return -1;
        }
        li->item = local;
        opal_list_append(&client_list, &(li->super));
        OPAL_OUTPUT((0, "Client CPC complete to %s",
                     endpoint->endpoint_proc->proc_ompi->proc_hostname));
    }
    if (NULL == endpoint) {
        BTL_ERROR(("Can't find endpoint"));
        return -1;
    }
    data = (rdmacm_endpoint_local_cpc_data_t *)endpoint->endpoint_local_cpc_data;

    /* Only notify the upper layers after the last QP has been connected */
    if (++data->rdmacm_counter < mca_btl_openib_component.num_qps) {
        BTL_VERBOSE(("%s count == %d", local->server?"server":"client", data->rdmacm_counter));
        return 0;
    }

    OPAL_OUTPUT((0, "in connect_endpoint; ep=0x%x (0x%x), I still %s the initiator to %s",
                 endpoint,
                 endpoint->endpoint_local_cpc,
                 endpoint->endpoint_initiator ? "am" : "am NOT",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname));
    message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    BTL_VERBOSE(("%s connected!!! local %x remote %x state = %d",
                 local->server?"server":"client",
                 local->ipaddr,
                 message->ipaddr,
                 endpoint->endpoint_state));

    ompi_btl_openib_fd_run_in_main(local_endpoint_cpc_complete, endpoint);

    return 0;
}

static int resolve_route(rdmacm_contents_t *local, int num)
{
    int rc;

    /* Resolve the route to the remote system.  Once established, the
     * local system will get a RDMA_CM_EVENT_ROUTE_RESOLVED event.
     */
    rc = rdma_resolve_route(local->id[num], RDMACM_RESOLVE_ADDR_TIMEOUT);
    if (0 != rc) {
        BTL_ERROR(("Failed to resolve the route with %d", rc));
        goto out;
    }

    return 0;

out:
    rdmacm_cleanup(local, local->id[num], num);
    return -1;
}

static int create_dummy_cq(rdmacm_contents_t *local, mca_btl_openib_module_t *openib_btl)
{
    local->dummy_cq = ibv_create_cq(openib_btl->device->ib_dev_context, 1, NULL, NULL, 0);
    if (NULL == local->dummy_cq) {
        BTL_ERROR(("dummy_cq not created"));
        goto out;
    }

    return 0;
out:
    return -1;
}

static int create_dummy_qp(rdmacm_contents_t *local, struct rdma_cm_id *id, int qpnum)
{
    struct ibv_qp_init_attr attr;
    struct ibv_qp *qp;

    memset(&attr, 0, sizeof(attr));
    attr.qp_type = IBV_QPT_RC;
    attr.send_cq = local->dummy_cq;
    attr.recv_cq = local->dummy_cq;
    attr.cap.max_recv_wr = 1;
    attr.cap.max_send_wr = 1;
    attr.cap.max_send_sge = 1;
    attr.cap.max_recv_sge = 1;

    qp = ibv_create_qp(local->openib_btl->device->ib_pd, &attr);
    if (NULL == qp) {
        BTL_ERROR(("Failed to create qp with %d", qp));
        goto out;
    }
    id->qp = qp;

    BTL_VERBOSE(("dummy qp created %p", qp));

    return 0;

out:
    return -1;
}

static int finish_connect(rdmacm_contents_t *local, int num)
{
    struct rdma_conn_param conn_param;
    conn_message_t msg;
    int rc;

    struct sockaddr *peeraddr, *localaddr;
    uint32_t localipaddr, remoteipaddr;
    uint16_t remoteport;

    remoteport = rdma_get_dst_port(local->id[num]);
    peeraddr = rdma_get_peer_addr(local->id[num]);
    remoteipaddr = ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;

    localaddr = rdma_get_local_addr(local->id[num]);
    localipaddr = ((struct sockaddr_in *)localaddr)->sin_addr.s_addr;

    /* If we're the initiator, then setup the QP's and post the CTS
       message buffer */
    if (local->endpoint->endpoint_initiator) {
        rc = rdmacm_setup_qp(local, local->endpoint, local->id[num], num);
        if (0 != rc) {
            BTL_ERROR(("rdmacm_setup_qp error %d", rc));
            goto out;
        }

        if (mca_btl_openib_component.credits_qp == num) {
            /* Post a single receive buffer on the smallest QP for the CTS
               protocol */
            
            struct ibv_recv_wr *bad_wr, *wr;
            assert(NULL != local->endpoint->endpoint_cts_frag);
            wr = &to_recv_frag(local->endpoint->endpoint_cts_frag)->rd_desc;
            assert(NULL != wr);
            wr->next = NULL;
            
            if (0 != ibv_post_recv(local->endpoint->qps[num].qp->lcl_qp, 
                                   wr, &bad_wr)) {
                BTL_ERROR(("failed to post CTS recv buffer"));
                goto out1;
            }
        }
    } else {
        /* If we are establishing a connection in the "wrong" direction,
         * setup a dummy CQ and QP and do NOT post any recvs on them.
         * Otherwise this will screwup the recv accounting and will
         * result in not posting recvs when you really really wanted to.
         * All of the dummy cq and qps will be cleaned up on the reject
         * event.
         */
        rc = create_dummy_cq(local, local->openib_btl);
        if (0 != rc) {
            BTL_ERROR(("create_dummy_cq error %d", rc));
            goto out;
        }

        rc = create_dummy_qp(local, local->id[num], num);
        if (0 != rc) {
            BTL_ERROR(("create_dummy_qp error %d", rc));
            goto out;
        }
    }

    memset(&conn_param, 0, sizeof(conn_param));
    conn_param.responder_resources = 1;
    conn_param.initiator_depth = 1;
    conn_param.retry_count = 10;
    conn_param.private_data = &msg;
    conn_param.private_data_len = sizeof(conn_message_t);

    msg.qpnum = num;
    msg.rem_index = local->endpoint->index;
    msg.rem_port = local->tcp_port;

    /* Now all of the local setup has been done.  The remote system
     * should now get a RDMA_CM_EVENT_CONNECT_REQUEST event to further
     * the setup of the QP.
     */
    OPAL_OUTPUT((0, "in finish_connect; ep=0x%x (0x%x), I still %s the initiator to %s",
                 local->endpoint,
                 local->endpoint->endpoint_local_cpc,
                 local->endpoint->endpoint_initiator ? "am" : "am NOT",
                 local->endpoint->endpoint_proc->proc_ompi->proc_hostname));
    rc = rdma_connect(local->id[num], &conn_param);
    if (0 != rc) {
        BTL_ERROR(("rdma_connect Failed with %d", rc));
        goto out1;
    }

    return 0;

out1:
    ibv_destroy_qp(local->id[num]->qp);
out:
    rdmacm_cleanup(local, local->id[num], num);

    return -1;
}

static int event_handler(struct rdma_cm_event *event)
{
    rdmacm_contents_t *local;
    struct sockaddr *peeraddr, *localaddr;
    uint32_t peeripaddr, localipaddr;
    int rc = -1, qpnum;

    if (NULL == event->id->context)
        return rc;

    local = ((id_contexts_t *)event->id->context)->local;
    qpnum = ((id_contexts_t *)event->id->context)->qpnum;
    localaddr = rdma_get_local_addr(event->id);
    peeraddr = rdma_get_peer_addr(event->id);
    localipaddr = ((struct sockaddr_in *)localaddr)->sin_addr.s_addr;
    peeripaddr = ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;

    BTL_VERBOSE(("%s event_handler -- %s, status = %d to %x",
                local->server?"server":"client",
                rdma_event_str(event->event),
                event->status,
                peeripaddr));

    switch (event->event) {
    case RDMA_CM_EVENT_ADDR_RESOLVED:
        OPAL_OUTPUT((0, "Address resolved: ID 0x%x", local->id[qpnum]));
        rc = resolve_route(local, qpnum);
        break;

    case RDMA_CM_EVENT_ROUTE_RESOLVED:
        OPAL_OUTPUT((0, "Route resolved: ID 0x%x", local->id[qpnum]));
        local->ipaddr = localipaddr;
        rc = finish_connect(local, qpnum);
        break;

    case RDMA_CM_EVENT_CONNECT_REQUEST:
        OPAL_OUTPUT((0, "Incoming connect request: 0x%x", local->id[qpnum]));
        rc = handle_connect_request(local, event);
        break;

    case RDMA_CM_EVENT_ESTABLISHED:
        OPAL_OUTPUT((0, "Connection established: 0x%x", local->id[qpnum]));
        rc = rdmacm_connect_endpoint(local, event);
        break;

    case RDMA_CM_EVENT_DISCONNECTED:
        rdmacm_cleanup(local, event->id, qpnum);
        rc = 0;
        break;

    case RDMA_CM_EVENT_REJECTED:
        if ((NULL != event->param.conn.private_data) &&
            (1 == *((int *)event->param.conn.private_data))) {
            BTL_VERBOSE(("A good reject! for qp %d", qpnum));
            if (NULL != local->id[qpnum]->qp) {
                ibv_destroy_qp(local->id[qpnum]->qp);
                local->id[qpnum]->qp = NULL;
            }
            if (NULL != local->dummy_cq) {
                ibv_destroy_cq(local->dummy_cq);
            }
            rdmacm_cleanup(local, local->id[qpnum], qpnum);
            rc = 0;
        }
        break;

    case RDMA_CM_EVENT_UNREACHABLE:
    case RDMA_CM_EVENT_CONNECT_RESPONSE:
    case RDMA_CM_EVENT_ADDR_ERROR:
    case RDMA_CM_EVENT_ROUTE_ERROR:
    case RDMA_CM_EVENT_CONNECT_ERROR:
    case RDMA_CM_EVENT_DEVICE_REMOVAL:
    default:
        rc = -1;
    }

    return rc;
}

static inline void rdmamcm_event_error(struct rdma_cm_event *event)
{
    mca_btl_base_endpoint_t *endpoint = NULL;

    if (event->id->context) {
        endpoint = ((id_contexts_t *)event->id->context)->local->endpoint;
    }

    ompi_btl_openib_fd_run_in_main(mca_btl_openib_endpoint_invoke_error, 
                                   endpoint);
}

static void *rdmacm_event_dispatch(int fd, int flags, void *context)
{
    struct rdma_cm_event *event, ecopy;
    void *data = NULL;
    int rc;

    /* blocks until next cm_event */
    rc = rdma_get_cm_event(event_channel, &event);
    if (0 != rc) {
        BTL_ERROR(("rdma_get_cm_event error %d", rc));
        goto err;
    }

    /* If the incomming event is not acked in a sufficient amount of
     * time, there will be a timeout error and the connection will be
     * torndown.  Also, the act of acking the event destroys the
     * included data in the event.  In certain circumstances, the time
     * it takes to handle a incoming event could approach or exceed this
     * time.  To prevent this from happening, we will copy the event and
     * all of its data, ack the event, and process the copy of the
     * event.
     */
    memcpy(&ecopy, event, sizeof(struct rdma_cm_event));
    if (event->param.conn.private_data_len > 0) {
        data = malloc(event->param.conn.private_data_len);
        if (NULL == data) {
           BTL_ERROR(("error mallocing memory"));
           goto err;
        }
        memcpy(data, event->param.conn.private_data, event->param.conn.private_data_len);
        ecopy.param.conn.private_data = data;
    }
    rdma_ack_cm_event(event);

    rc = event_handler(&ecopy);
    if (0 != rc) {
        BTL_ERROR(("Error event_handler -- %s, status = %d",
                    rdma_event_str(ecopy.event),
                    ecopy.status));

        if (NULL != data)
            free(data);

        goto err;
    }

    if (NULL != data)
        free(data);

    return NULL;
err:
    rdmamcm_event_error(&ecopy);
    return NULL;
}

/* CPC init function - Setup all globals here */
static int rdmacm_init(mca_btl_openib_endpoint_t *endpoint)
{
    void *data;

    data = calloc(1, sizeof(rdmacm_endpoint_local_cpc_data_t));
    if (NULL == data) {
        BTL_ERROR(("malloc failed"));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    endpoint->endpoint_local_cpc_data = data;

    return OMPI_SUCCESS;
}

static int ipaddrcheck(rdmacm_contents_t *server, 
                       mca_btl_openib_module_t *openib_btl)
{
    uint32_t ipaddr;
    bool already_exists = false;
    opal_list_item_t *item;
    int server_tcp_port = rdma_get_src_port(server->id[0]);

    /* Look up the IP address of this device/port */
    ipaddr = 
        mca_btl_openib_rdma_get_ipv4addr(openib_btl->device->ib_dev_context, 
                                         openib_btl->port_num);
    if (0 == ipaddr) {
        BTL_VERBOSE(("openib BTL: rdmacm CPC unable to find IP address for %s", ibv_get_device_name(openib_btl->device->ib_dev)));
        return OMPI_ERR_NOT_FOUND;
    }

    /* Ok, we found the IP address of this device/port.  Have we
       already see this IP address/TCP port before? */
    for (item = opal_list_get_first(&server_list); 
         item != opal_list_get_end(&server_list); 
         item = opal_list_get_next(item)) {
        list_item_t *pitem = (list_item_t *)item;
        BTL_VERBOSE(("paddr = %x, ipaddr addr = %x", 
                     pitem->item->ipaddr, ipaddr));
        if (pitem->item->ipaddr == ipaddr &&
            pitem->item->tcp_port == server_tcp_port) {
            BTL_VERBOSE(("addr %x already exists", ipaddr));
            already_exists = true;
            break;
        }
    }

    /* If we haven't seen it before, save it */
    if (!already_exists) {
        server->ipaddr = ipaddr;
        server->tcp_port = server_tcp_port;
    }

    return already_exists ? OMPI_ERROR : OMPI_SUCCESS;
}

static int create_message(rdmacm_contents_t *server, mca_btl_openib_module_t *openib_btl, ompi_btl_openib_connect_base_module_data_t *data)
{
    message_t *message;

    message = malloc(sizeof(message_t));
    if (NULL == message) {
        BTL_ERROR(("malloc Failed"));
        goto out;
    }

    message->ipaddr = server->ipaddr;
    message->tcp_port = server->tcp_port;

    BTL_VERBOSE(("Message IP address is %x, port %d", message->ipaddr, message->tcp_port));
    data->cbm_modex_message = message;
    data->cbm_modex_message_len = sizeof(message_t);

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
}

/* This function determines if the RDMACM is a possible cpc method and
 * sets it up accordingly.
 */
static int rdmacm_component_query(mca_btl_openib_module_t *openib_btl, ompi_btl_openib_connect_base_module_t **cpc)
{
    rdmacm_contents_t *server = NULL;
    struct sockaddr_in sin;
    list_item_t *li;
    id_contexts_t *context;
    int rc;

    /* RDMACM is not supported if we have any XRC QPs */
    if (mca_btl_openib_component.num_xrc_qps > 0) {
        BTL_VERBOSE(("rdmacm CPC not supported with XRC receive queues, please try xoob CPC; skipped"));
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto out;
    }

    BTL_VERBOSE(("rdmacm_component_query"));

    *cpc = malloc(sizeof(ompi_btl_openib_connect_base_module_t));
    if (NULL == *cpc) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    (*cpc)->data.cbm_component = &ompi_btl_openib_connect_rdmacm;
    (*cpc)->data.cbm_priority = rdmacm_priority;
    (*cpc)->cbm_endpoint_init = rdmacm_init;
    (*cpc)->cbm_start_connect = rdmacm_module_start_connect;
    (*cpc)->cbm_endpoint_finalize = rdmacm_connection_shutdown;
    (*cpc)->cbm_finalize = NULL;
    /* Setting uses_cts=true also guarantees that we'll only be
       selected if QP 0 is PP */
    (*cpc)->cbm_uses_cts = true;

    server = malloc(sizeof(rdmacm_contents_t));
    if (NULL == server) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out1;
    }

    server->id = malloc(sizeof(struct rdma_cm_id *));
    if (NULL == server->id) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out2;
    }

    server->server = true;
    server->openib_btl = openib_btl;

    context = malloc(sizeof(id_contexts_t));
    if (NULL == context) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC system error (malloc failed)");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out3;
    }

    context->local = server;
    context->qpnum = 0;

    rc = rdma_create_id(event_channel, &server->id[0], context, RDMA_PS_TCP);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC failed to create ID");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out4;
    }

    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = rdmacm_addr;
    sin.sin_port = rdmacm_port;

    /* Bind the rdmacm server to the local IP address and an ephemerial
     * port or one specified by a comand arg.
     */
    rc = rdma_bind_addr(server->id[0], (struct sockaddr *)&sin);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to bind to address");
        rc = OMPI_ERR_UNREACH;
        goto out5;
    }

    /* Verify that the device has a valid IP address on it, or we
       cannot use the cpc */
    rc = ipaddrcheck(server, openib_btl);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm IP address not found on port");
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto out5;
    }

    /* Listen on the specified address/port with the rdmacm, limit the
       amount of incoming connections to 1024 */
    /* FIXME - 1024 should be (num of connectors *
       mca_btl_openib_component.num_qps) */
    rc = rdma_listen(server->id[0], 1024);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to listen");
        rc = OMPI_ERR_UNREACH;
        goto out5;
    }

    rc = create_message(server, openib_btl, &(*cpc)->data);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to create message");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out5;
    }

    li = OBJ_NEW(list_item_t);
    if (NULL== li) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to add to list");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out6;
    }
    li->item = server;
    opal_list_append(&server_list, &(li->super));

    opal_output_verbose(5, mca_btl_base_output,
                        "openib BTL: rdmacm CPC available for use on %s",
                        ibv_get_device_name(openib_btl->device->ib_dev));
    return OMPI_SUCCESS;

out6:
    free(&(*cpc)->data.cbm_modex_message);
out5:
    rdma_destroy_id(server->id[0]);
out4:
    free(context);
out3:
    free(server->id);
out2:
    free(server);
out1:
    free(*cpc);
out:
    if (OMPI_ERR_NOT_SUPPORTED == rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unavailable for use on %s; skipped",
                            ibv_get_device_name(openib_btl->device->ib_dev));
    } else {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rmacm CPC unavailable for use on %s; fatal error %d (%s)",
                            ibv_get_device_name(openib_btl->device->ib_dev), rc,
                            opal_strerror(rc));
    }
    return rc;
}

static int rdmacm_component_destroy(void)
{
    opal_list_item_t *item;
    int rc;

    BTL_VERBOSE(("rdmacm_component_destroy"));

    if (0 != opal_list_get_size(&client_list)) {
        for (item = opal_list_get_first(&client_list);
             item != opal_list_get_end(&client_list);
             item = opal_list_get_next(item)) {
            rdmacm_contents_t *local = ((list_item_t *)item)->item;

            rdmacm_destroy(local);
            opal_list_remove_item(&client_list, item);
        }
    }

    if (0 != opal_list_get_size(&server_list)) {
        for (item = opal_list_get_first(&server_list);
             item != opal_list_get_end(&server_list);
             item = opal_list_get_next(item)) {
            rdmacm_contents_t *local = ((list_item_t *)item)->item;

            rdmacm_server_cleanup(local);
            opal_list_remove_item(&server_list, item);
        }
    }

    if (NULL != event_channel) {
        rc = ompi_btl_openib_fd_unmonitor(event_channel->fd, rdmacm_unmonitor, NULL);
        if (OMPI_SUCCESS != rc)
            BTL_ERROR(("Error disabling fd monitor"));
    }

    mca_btl_openib_free_rdma_addr_list();

    return OMPI_SUCCESS;
}

static int rdmacm_component_init(void)
{
    int rc;

    OBJ_CONSTRUCT(&server_list, opal_list_t);
    OBJ_CONSTRUCT(&client_list, opal_list_t);

    rc = mca_btl_openib_build_rdma_addr_list();
    if (-1 == rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to find any valid IP address");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    event_channel = rdma_create_event_channel();
    if (NULL == event_channel) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC failed to create channel");
        return OMPI_ERR_UNREACH;
    }

    /* Start monitoring the fd associated with the cm_device */
    ompi_btl_openib_fd_monitor(event_channel->fd, OPAL_EV_READ,
                               rdmacm_event_dispatch, NULL);

    return OMPI_SUCCESS;
}
