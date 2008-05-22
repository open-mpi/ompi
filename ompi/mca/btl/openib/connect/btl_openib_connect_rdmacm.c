/*
 * Copyright (c) 2007-2008 Cisco, Inc.  All rights reserved.
 * Copyright (c) 2007-2008 Chelsio, Inc. All rights reserved.
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
#include "orte/util/output.h"

#include "btl_openib_fd.h"
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"
#include "connect/connect.h"
#include "btl_openib_iwarp.h"

/* JMS to be removed: see #1264 */
#undef event

static void rdmacm_component_register(void);
static int rdmacm_component_query(mca_btl_openib_module_t *openib_btl,
                                  ompi_btl_openib_connect_base_module_t **cpc);
static int rdmacm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                       mca_btl_base_endpoint_t *endpoint);
static int rdmacm_component_destroy(void);
static int rdmacm_component_init(void);

ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_rdmacm = {
    "rdmacm",
    rdmacm_component_register,
    rdmacm_component_init,
    rdmacm_component_query,
    rdmacm_component_destroy
};

struct rdmacm_contents {
    bool server;
    mca_btl_openib_endpoint_t *endpoint;
    mca_btl_openib_module_t *openib_btl;
    struct ibv_cq *dummy_cq;
    struct rdma_cm_id **id;
    uint32_t ipaddr;
    uint16_t tcp_port;
};

struct message {
    uint32_t ipaddr;
    uint16_t tcp_port;
};

struct rdmacm_endpoint_local_cpc_data {
    int rdmacm_counter;
};

struct id_contexts {
    struct rdmacm_contents *local;
    mca_btl_openib_endpoint_t *endpoint;
    uint8_t qpnum;
};

struct conn_message {
    uint32_t rem_index;
    uint16_t rem_port;
    uint8_t qpnum;
};

struct list_item {
    opal_list_item_t super;
    struct rdmacm_contents *item;
};
typedef struct list_item list_item_t;

static OBJ_CLASS_INSTANCE(list_item_t, opal_list_item_t, NULL, NULL);

static opal_list_t server_list;
static opal_list_t client_list;
static struct rdma_event_channel *event_channel = NULL;
static int rdmacm_priority = 30;
static uint16_t rdmacm_port = 0;
static uint32_t rdmacm_addr = 0;

#define RDMA_RESOLVE_ADDR_TIMEOUT 2000

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

/* This function traverses the list of endpoints associated with the hca
 * and determines which of them the remote side is attempting to connect
 * to.  This is determined based on the local endpoint's modex message
 * recevied and the IP address and port associated with the rdma_cm
 * event id
 */
static mca_btl_openib_endpoint_t *rdmacm_find_endpoint(struct rdmacm_contents *local,
                                                       struct rdma_cm_id *id,
                                                       uint16_t rem_port)
{
    int i;
    mca_btl_openib_endpoint_t *ep = NULL;
    opal_pointer_array_t *endpoints = local->openib_btl->hca->endpoints;

    for (i = 0; i < opal_pointer_array_get_size(endpoints); i++) {
        mca_btl_openib_endpoint_t *ib_endpoint;
        struct message *message;
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

static void rdmacm_cleanup(struct rdmacm_contents *local,
                           struct rdma_cm_id *id,
                           uint32_t num)
{
    if (NULL == id)
        return;

    free(id->context);
    id->context = NULL;

    rdma_destroy_id(id);
    if (!local->server && local->id && local->id[num]) {
        local->id[num] = NULL;
    }
}

static int rdmacm_setup_qp(struct rdmacm_contents *local,
                           mca_btl_openib_endpoint_t *endpoint,
                           struct rdma_cm_id *id,
                           int qpnum)
{
    struct ibv_qp_init_attr attr;
    struct ibv_qp *qp;
    struct ibv_srq *srq = NULL;
    int credits = 0, reserved = 0, max_recv_wr, max_send_wr;

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
    attr.send_cq = local->openib_btl->hca->ib_cq[BTL_OPENIB_LP_CQ];
    attr.recv_cq = local->openib_btl->hca->ib_cq[qp_cq_prio(qpnum)];
    attr.srq = srq;
    attr.cap.max_recv_wr = max_recv_wr;
    attr.cap.max_send_wr = max_send_wr;
    attr.cap.max_send_sge = mca_btl_openib_component.ib_sg_list_size;
    attr.cap.max_recv_sge = mca_btl_openib_component.ib_sg_list_size;

    qp = ibv_create_qp(local->openib_btl->hca->ib_pd, &attr);
    if (NULL == qp) {
        BTL_ERROR(("Failed to create qp with %d", qp));
        goto out;
    }

    endpoint->qps[qpnum].qp->lcl_qp = qp;
    id->qp = qp;

    /* After creating the qp, the driver will write the max_inline_data
     * in the attributes.  Update the btl with this data.
     */
    local->openib_btl->ib_inline_max = attr.cap.max_inline_data;

    return 0;

out:
    return 1;
}

static int rdma_client_connect_one(struct rdmacm_contents *local,
                                   struct message *message,
                                   int num)
{
    struct sockaddr_in din;
    struct id_contexts *context;
    int rc;

    /* We'll need to access some data in the event handler.  We can
     * encapsulate it in this data struct and attach it to the id being
     * created below.  The event->id will contain this same pointer.
     */
    context = malloc(sizeof(struct id_contexts));
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
    rc = rdma_resolve_addr(local->id[num],
                           NULL,
                           (struct sockaddr *)&din,
                           RDMA_RESOLVE_ADDR_TIMEOUT);
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

static int rdma_client_connect(struct rdmacm_contents *local, struct message *message)
{
    int rc, qp;

    local->id = malloc(sizeof(struct rdma_cm_id *) * mca_btl_openib_component.num_qps);
    if (NULL == local->id) {
        BTL_ERROR(("malloc error"));
        return OMPI_ERROR;
    }

    for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        rc = rdma_client_connect_one(local, message, qp);
        if (OMPI_SUCCESS != rc) {
            BTL_ERROR(("rdma_client_connect_one error"));
            goto out;
        }
    }

    return OMPI_SUCCESS;

out:
   for (; qp >= 0; qp--) {
       rdmacm_cleanup(local, local->id[qp], qp);
   }
   return OMPI_ERROR;
}

/* Connect method called by the upper layers to connect the local
 * endpoint to the remote endpoint by creating QP(s) to connect the two.
 * Already holding endpoint lock when this function is called.
 */
static int rdmacm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                       mca_btl_base_endpoint_t *endpoint)
{
    struct rdmacm_contents *client;
    struct message *message;
    int rc;

    message = (struct message *)endpoint->endpoint_remote_cpc_data->cbm_modex_message;

    BTL_VERBOSE(("Connecting to remote ip addr = %x, port = %d  ep state = %d",
                 message->ipaddr, message->tcp_port, endpoint->endpoint_state));

    if (MCA_BTL_IB_CONNECTED == endpoint->endpoint_state ||
        MCA_BTL_IB_CONNECTING == endpoint->endpoint_state ||
        MCA_BTL_IB_CONNECT_ACK == endpoint->endpoint_state) {
        return OMPI_SUCCESS;
    }

    endpoint->endpoint_state = MCA_BTL_IB_CONNECT_ACK;

    client = calloc(1, sizeof(struct rdmacm_contents));
    if (NULL == client) {
        BTL_ERROR(("malloc of client failed"));
        goto out;
    }

    client->openib_btl = endpoint->endpoint_btl;
    client->endpoint = endpoint;
    client->server = false;
    /* Populate the port information with the local port the server is
     * listening on instead of the ephemerial port this client is
     * connecting with.  This port is used to determine which endpoint
     * is being connected from, in the isntance where there are
     * multiple listeners on the local system.
     */
    client->tcp_port = ((struct message *)endpoint->endpoint_local_cpc->data.cbm_modex_message)->tcp_port;

    rc = rdma_client_connect(client, message);
    if (0 != rc) {
        BTL_ERROR(("rdma_client_connect error"));
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
static int handle_connect_request(struct rdmacm_contents *local,
                                  struct rdma_cm_event *event)
{
    mca_btl_openib_endpoint_t *endpoint;
    struct rdma_conn_param conn_param;
    struct message *message;
    struct conn_message msg;
    int rc = -1, qpnum;
    uint32_t rem_index;
    uint16_t rem_port;

    qpnum = ((struct conn_message *)event->param.conn.private_data)->qpnum;
    rem_port = ((struct conn_message *)event->param.conn.private_data)->rem_port;
    rem_index = ((struct conn_message *)event->param.conn.private_data)->rem_index;

    /* Determine which endpoint the remote side is trying to connect to */
    endpoint = rdmacm_find_endpoint(local, event->id, rem_port);
    if (NULL == endpoint) {
        BTL_ERROR(("Failed to find endpoint"));
        return -1;
    }

    message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;

    BTL_VERBOSE(("ep state = %d, local ipaddr = %x, remote ipaddr = %x port %d",
                 endpoint->endpoint_state, local->ipaddr, message->ipaddr, rem_port));

    if ((local->ipaddr > message->ipaddr && local->tcp_port > rem_port) ||
        local->ipaddr > message->ipaddr) {
        int race = 1;

        BTL_VERBOSE(("Received a connect request from an endpoint in the wrong direction"));

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

        /* If there are multiple QPs attempting to connect from the
         * wrong direction, only make one call to
         * rdmacm_module_start_connect to connect in the proper
         * direction, as it will connect to the remote side with the
         * correct number of QPs.
         */
        if (0 == qpnum) {
            rdmacm_module_start_connect(NULL, endpoint);
        }

        return 0;
    }

    endpoint->rem_info.rem_index = rem_index;

    /* Setup QP for new connection */
    BTL_VERBOSE(("ACCEPTING src port = %d, dst port = %d, qpnum = %d",
                 rdma_get_src_port(event->id), rdma_get_dst_port(event->id), qpnum));

    rc = rdmacm_setup_qp(local, endpoint, event->id, qpnum);
    if (0 != rc) {
        BTL_ERROR(("rdmacm_setup_qp error %d", rc));
        goto out;
    }

    /* Recvs must be posted prior to accepting the rdma connection.
     * Otherwise, it is possible to get data before there are recvs to
     * put it, which for iWARP will result in tearing down of the
     * connection.
     */
    if (BTL_OPENIB_QP_TYPE_PP(qpnum)) {
        rc = mca_btl_openib_endpoint_post_rr(endpoint, qpnum);
    } else {
        rc = mca_btl_openib_post_srr(endpoint->endpoint_btl, qpnum);
    }
    if (OMPI_SUCCESS != rc) {
        BTL_ERROR(("mca_btl_openib_endpoint_post_rr_nolock error %d", rc));
        goto out1;
    }

    /* Since the event id is already created, we cannot add this
     * information in the normal way.  Instead we must reference its
     * location and put the data there so that it can be access later.
     */
    event->id->context = malloc(sizeof(struct id_contexts));
    if (NULL == event->id->context) {
        BTL_ERROR(("malloc error"));
        goto out1;
    }

    ((struct id_contexts *)event->id->context)->local = local;
    ((struct id_contexts *)event->id->context)->qpnum = qpnum;
    ((struct id_contexts *)event->id->context)->endpoint = endpoint;

    memset(&conn_param, 0, sizeof(conn_param));
    conn_param.responder_resources = 1;
    conn_param.initiator_depth = 1;
    conn_param.private_data = &msg;
    conn_param.private_data_len = sizeof(struct conn_message);

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

static void rdmacm_destroy(struct rdmacm_contents *local)
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

static void rdmacm_server_cleanup(struct rdmacm_contents *local)
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
        struct list_item *cli = (struct list_item *)item;

        if (endpoint == cli->item->endpoint) {
            int i;
            for (i = 0; i < mca_btl_openib_component.num_qps; i++)
                if (NULL != cli->item->id[i] &&
                    NULL != cli->item->id[i]->qp &&
                    NULL != cli->item->endpoint->qps) {
                        rdma_disconnect(cli->item->id[i]);
                }
        }
    }

    return 0;
}

/*
 * Callback (from main thread) when the endpoint has been connected
 */
static void *local_endpoint_connected(void *context)
{
    mca_btl_openib_endpoint_t *endpoint = (mca_btl_openib_endpoint_t *)context;

    mca_btl_openib_endpoint_connected(endpoint);

    return NULL;
}

static int rdmacm_connect_endpoint(struct rdmacm_contents *local, struct rdma_cm_event *event)
{
    struct rdmacm_endpoint_local_cpc_data *data;
    mca_btl_openib_endpoint_t *endpoint;
    struct message *message;

    if (local->server)
        endpoint = ((struct id_contexts *)event->id->context)->endpoint;
    else {
        struct list_item *li;
        uint32_t rem_index;

        rem_index = ((struct conn_message *)event->param.conn.private_data)->rem_index;

        endpoint = local->endpoint;
        local->endpoint->rem_info.rem_index = rem_index;

        li = OBJ_NEW(list_item_t);
        if (NULL == li) {
            BTL_ERROR(("malloc error"));
            return -1;
        }
        li->item = local;
        opal_list_append(&client_list, &(li->super));
    }
    if (NULL == endpoint) {
        BTL_ERROR(("Can't find endpoint"));
        return -1;
    }
    data = (struct rdmacm_endpoint_local_cpc_data *)endpoint->endpoint_local_cpc_data;

    /* Only notify the upper layers after the last QO has been connected */
    if (++data->rdmacm_counter < mca_btl_openib_component.num_qps) {
        BTL_VERBOSE(("%s count == %d", local->server?"server":"client", data->rdmacm_counter));
        return 0;
    }

    message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    BTL_VERBOSE(("%s connected!!! local %x remote %x state = %d",
                 local->server?"server":"client",
                 local->ipaddr,
                 message->ipaddr,
                 endpoint->endpoint_state));

    ompi_btl_openib_fd_schedule(local_endpoint_connected, endpoint);

    return 0;
}

static int start_connect(struct rdmacm_contents *local, int num)
{
    int rc;

    /* Resolve the route to the remote system.  Onced established, the
     * local system will get a RDMA_CM_EVENT_ROUTE_RESOLVED event.
     */
    rc = rdma_resolve_route(local->id[num], RDMA_RESOLVE_ADDR_TIMEOUT);
    if (0 != rc) {
        BTL_ERROR(("Failed to resolve the route with %d", rc));
        goto out;
    }

    return 0;

out:
    rdmacm_cleanup(local, local->id[num], num);

    return -1;
}

static int create_dummy_cq(struct rdmacm_contents *local, mca_btl_openib_module_t *openib_btl)
{
    local->dummy_cq = ibv_create_cq(openib_btl->hca->ib_dev_context, 1, NULL, NULL, 0);
    if (NULL == local->dummy_cq) {
        BTL_ERROR(("dummy_cq not created"));
        goto out;
    }

    return 0;
out:
    return -1;
}

static int create_dummy_qp(struct rdmacm_contents *local, struct rdma_cm_id *id, int qpnum)
{
    struct ibv_qp_init_attr attr;
    struct ibv_qp *qp;

    /* create the qp via rdma_create_qp() */
    memset(&attr, 0, sizeof(attr));
    attr.qp_type = IBV_QPT_RC;
    attr.send_cq = local->dummy_cq;
    attr.recv_cq = local->dummy_cq;
    attr.cap.max_recv_wr = 1;
    attr.cap.max_send_wr = 1;
    attr.cap.max_send_sge = 1;
    attr.cap.max_recv_sge = 1;

    qp = ibv_create_qp(local->openib_btl->hca->ib_pd, &attr);
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

static int finish_connect(struct rdmacm_contents *local, int num)
{
    struct rdma_conn_param conn_param;
    struct sockaddr *peeraddr, *localaddr;
    uint32_t localipaddr, remoteipaddr;
    uint16_t remoteport;
    struct conn_message msg;
    int rc;

    remoteport = rdma_get_dst_port(local->id[num]);
    localaddr = rdma_get_local_addr(local->id[num]);
    peeraddr = rdma_get_peer_addr(local->id[num]);
    localipaddr = ((struct sockaddr_in *)localaddr)->sin_addr.s_addr;
    remoteipaddr = ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;

    if ((localipaddr == remoteipaddr && local->tcp_port <= remoteport) ||
        localipaddr > remoteipaddr) {
        rc = rdmacm_setup_qp(local, local->endpoint, local->id[num], num);
        if (0 != rc) {
            BTL_ERROR(("rdmacm_setup_qp error %d", rc));
            goto out;
        }

        if (BTL_OPENIB_QP_TYPE_PP(num)) {
            rc = mca_btl_openib_endpoint_post_rr(local->endpoint, num);
        } else {
            rc = mca_btl_openib_post_srr(local->endpoint->endpoint_btl, num);
        }
        if (OMPI_SUCCESS != rc) {
            BTL_ERROR(("mca_btl_openib_endpoint_post_rr_nolock error %d", rc));
            goto out1;
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
    conn_param.private_data_len = sizeof(struct conn_message);

    msg.qpnum = num;
    msg.rem_index = local->endpoint->index;
    msg.rem_port = local->tcp_port;

    BTL_VERBOSE(("Connecting from %x, port %d to %x", localipaddr, msg.rem_port, remoteipaddr));

    /* Now all of the local setup has been done.  The remote system
     * should now get a RDMA_CM_EVENT_CONNECT_REQUEST event to further
     * the setup of the QP.
     */
    rc = rdma_connect(local->id[num], &conn_param);
    if (0 != rc) {
        BTL_ERROR(("rdma_connect Failed with %d", rc));
        goto out1;
    }

    return 0;

out1:
    ibv_destroy_qp(local->endpoint->qps[num].qp->lcl_qp);
out:
    rdmacm_cleanup(local, local->id[num], num);

    return -1;
}

static int rdma_event_handler(struct rdma_cm_event *event)
{
    struct rdmacm_contents *local;
    struct sockaddr *peeraddr, *localaddr;
    uint32_t peeripaddr, localipaddr;
    int rc = -1, qpnum;

    if (NULL == event->id->context)
        return rc;

    local = ((struct id_contexts *)event->id->context)->local;
    qpnum = ((struct id_contexts *)event->id->context)->qpnum;
    localaddr = rdma_get_local_addr(event->id);
    peeraddr = rdma_get_peer_addr(event->id);
    localipaddr = ((struct sockaddr_in *)localaddr)->sin_addr.s_addr;
    peeripaddr = ((struct sockaddr_in *)peeraddr)->sin_addr.s_addr;

    BTL_VERBOSE(("%s rdma_event_handler -- %s, status = %d to %x",
                local->server?"server":"client",
                rdma_event_str(event->event),
                event->status,
                peeripaddr));

    switch (event->event) {
    case RDMA_CM_EVENT_ADDR_RESOLVED:
        rc = start_connect(local, qpnum);
        break;

    case RDMA_CM_EVENT_ROUTE_RESOLVED:
        local->ipaddr = localipaddr;
        rc = finish_connect(local, qpnum);
        break;

    case RDMA_CM_EVENT_CONNECT_REQUEST:
        rc = handle_connect_request(local, event);
        break;

    case RDMA_CM_EVENT_ESTABLISHED:
        rc = rdmacm_connect_endpoint(local, event);
        break;

    case RDMA_CM_EVENT_DISCONNECTED:
        rdmacm_cleanup(local, event->id, qpnum);
        rc = 0;
        break;

    case RDMA_CM_EVENT_REJECTED:
        if ((NULL != event->param.conn.private_data) && (1 == *((int *)event->param.conn.private_data))) {
            BTL_VERBOSE(("A good reject! for qp %d", qpnum));
            if (NULL != local->id[qpnum]->qp) {
                ibv_destroy_qp(local->id[qpnum]->qp);
                local->id[qpnum]->qp = NULL;
            }
            if (NULL != local->dummy_cq)
                ibv_destroy_cq(local->dummy_cq);
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

static void *rdmacm_event_dispatch(int fd, int flags, void *context)
{
    struct rdma_cm_event *event, ecopy;
    void *data = NULL;
    int rc;

    /* blocks until next cm_event */
    rc = rdma_get_cm_event(event_channel, &event);
    if (0 != rc) {
        BTL_ERROR(("rdma_get_cm_event error %d", rc));
        return NULL;
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
           return NULL;
        }
        memcpy(data, event->param.conn.private_data, event->param.conn.private_data_len);
        ecopy.param.conn.private_data = data;
    }
    rdma_ack_cm_event(event);

    rc = rdma_event_handler(&ecopy);
    if (0 != rc) {
        BTL_ERROR(("Error rdma_event_handler -- %s, status = %d",
                    rdma_event_str(ecopy.event),
                    ecopy.status));
    }

    if (NULL != data)
        free(data);

    return NULL;
}

/* CPC init function - Setup all globals here */
static int rdmacm_init(mca_btl_openib_endpoint_t *endpoint)
{
    void *data;

    data = calloc(1, sizeof(struct rdmacm_endpoint_local_cpc_data));
    if (NULL == data) {
        BTL_ERROR(("malloc failed"));
        goto out;
    }
    endpoint->endpoint_local_cpc_data = data;

    return 0;
out:
    return -1;
}

static int ipaddrcheck(struct rdmacm_contents *server, 
                       mca_btl_openib_module_t *openib_btl)
{
    uint32_t ipaddr;
    bool already_exists = false;
    opal_list_item_t *item;
    int server_tcp_port = rdma_get_src_port(server->id[0]);

    /* Look up the IP address of this device/port */
    ipaddr = 
        mca_btl_openib_rdma_get_ipv4addr(openib_btl->hca->ib_dev_context, 
                                         openib_btl->port_num);
    if (0 == ipaddr) {
        BTL_VERBOSE(("openib BTL: rdmacm CPC unable to find IP address for %s", ibv_get_device_name(openib_btl->hca->ib_dev)));
        return OMPI_ERR_NOT_FOUND;
    }

    /* Ok, we found the IP address of this device/port.  Have we
       already see this IP address/TCP port before? */
    for (item = opal_list_get_first(&server_list); 
         item != opal_list_get_end(&server_list); 
         item = opal_list_get_next(item)) {
        struct list_item *pitem = (struct list_item *)item;
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

static int create_message(struct rdmacm_contents *server, mca_btl_openib_module_t *openib_btl, ompi_btl_openib_connect_base_module_data_t *data)
{
    struct message *message;

    message = malloc(sizeof(struct message));
    if (NULL == message) {
        BTL_ERROR(("malloc Failed"));
        goto out;
    }

    message->ipaddr = server->ipaddr;
    message->tcp_port = server->tcp_port;

    BTL_VERBOSE(("Message IP address is %x, port %d", message->ipaddr, message->tcp_port));
    data->cbm_modex_message = message;
    data->cbm_modex_message_len = sizeof(struct message);

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
}

/* This function determines if the RDMACM is a possible cpc method and
 * sets it up accordingly.
 */
static int rdmacm_component_query(mca_btl_openib_module_t *openib_btl, ompi_btl_openib_connect_base_module_t **cpc)
{
    struct rdmacm_contents *server = NULL;
    struct sockaddr_in sin;
    struct list_item *li;
    struct id_contexts *context;
    int rc;

    /* RDMACM is not supported if we have any XRC QPs */
    if (mca_btl_openib_component.num_xrc_qps > 0) {
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC not supported with XRC receive queues; skipped");
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

    server = malloc(sizeof(struct rdmacm_contents));
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

    context = malloc(sizeof(struct id_contexts));
    if (NULL == context) {
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC system error (malloc failed)");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out3;
    }

    context->local = server;
    context->qpnum = 0;

    rc = rdma_create_id(event_channel, &server->id[0], context, RDMA_PS_TCP);
    if (0 != rc) {
        orte_output_verbose(5, mca_btl_base_output,
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
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to bind to address");
        rc = OMPI_ERR_UNREACH;
        goto out5;
    }

    /* Verify that the HCA has a valid IP address on it, or we cannot
       use the cpc */
    rc = ipaddrcheck(server, openib_btl);
    if (0 != rc) {
        orte_output_verbose(5, mca_btl_base_output,
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
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to listen");
        rc = OMPI_ERR_UNREACH;
        goto out5;
    }

    rc = create_message(server, openib_btl, &(*cpc)->data);
    if (0 != rc) {
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to create message");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out5;
    }

    li = OBJ_NEW(list_item_t);
    if (NULL== li) {
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to add to list");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out6;
    }
    li->item = server;
    opal_list_append(&server_list, &(li->super));

    orte_output_verbose(5, mca_btl_base_output,
                        "openib BTL: rdmacm CPC available for use on %s",
                        ibv_get_device_name(openib_btl->hca->ib_dev));
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
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unavailable for use on %s; skipped",
                            ibv_get_device_name(openib_btl->hca->ib_dev));
    } else {
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rmacm CPC unavailable for use on %s; fatal error %d (%s)",
                            ibv_get_device_name(openib_btl->hca->ib_dev), rc,
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
            struct rdmacm_contents *local = ((struct list_item *)item)->item;

            rdmacm_destroy(local);
            opal_list_remove_item(&client_list, item);
        }
    }

    if (0 != opal_list_get_size(&server_list)) {
        for (item = opal_list_get_first(&server_list);
             item != opal_list_get_end(&server_list);
             item = opal_list_get_next(item)) {
            struct rdmacm_contents *local = ((struct list_item *)item)->item;

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
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to find any valid IP address");
        return OMPI_ERR_NOT_SUPPORTED;
    }

    event_channel = rdma_create_event_channel();
    if (NULL == event_channel) {
        orte_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC failed to create channel");
        return OMPI_ERR_UNREACH;
    }

    /* Start monitoring the fd associated with the cm_device */
    ompi_btl_openib_fd_monitor(event_channel->fd, OPAL_EV_READ,
                               rdmacm_event_dispatch, NULL);

    return OMPI_SUCCESS;
}
