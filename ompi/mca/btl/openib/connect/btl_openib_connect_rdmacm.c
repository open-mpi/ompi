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
#include <ifaddrs.h>
#include <stdio.h>
#include <malloc.h>

#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"

#include "btl_openib_fd.h"
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"
#include "connect/connect.h"

/* JMS to be removed: see #1264 */
#undef event

static void rdmacm_component_register(void);
static int rdmacm_component_query(mca_btl_openib_module_t *openib_btl, 
                                   ompi_btl_openib_connect_base_module_t **cpc);

static int rdmacm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                        mca_btl_base_endpoint_t *endpoint);
static uint32_t rdma_get_ipv4addr(struct ibv_context *verbs, uint8_t port);
static int rdmacm_component_destroy(void);
static int rdmacm_component_init(void);

/* 
 * The cruft below maintains the linked list of rdma ipv4 addresses and their
 * associated rdma device names and device port numbers.  
 */
struct rdma_addr_list {
    uint32_t              addr;
    uint32_t              subnet;
    char                  addr_str[16];
    char                  dev_name[IBV_SYSFS_NAME_MAX];
    uint8_t               dev_port;
    struct rdma_addr_list *next;
};
static struct rdma_addr_list *myaddrs;
static int build_rdma_addr_list(void);
static void free_rdma_addr_list(void);

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
    uint16_t port;
};

struct message {
    uint32_t ipaddr;
    uint16_t port;
};

struct rdmacm_endpoint_local_cpc_data {
    int rdmacm_counter;
    opal_mutex_t endpoint_state_lock;
};

struct id_contexts {
    struct rdmacm_contents *local;
    mca_btl_openib_endpoint_t *endpoint;
    uint8_t qpnum;
};

struct list_item {
    struct list_item *next;
    struct rdmacm_contents *item;
}; 

struct conn_message {
    uint32_t rem_index; 
    uint16_t rem_port;
    uint8_t qpnum;
};

static struct list_item *server_list_head = NULL;
static struct list_item *server_list_tail = NULL;
static struct list_item *client_list_head = NULL;
static struct list_item *client_list_tail = NULL;
static struct rdma_event_channel *event_channel = NULL;
static int rdmacm_priority = 30;
static uint16_t rdmacm_port = 0;
static uint32_t rdmacm_addr = 0;

#define RDMA_RESOLVE_ADDR_TIMEOUT 2000

static int list_add(struct rdmacm_contents *client, struct list_item **head, struct list_item **tail)
{
    struct list_item *temp;

    temp = malloc(sizeof(struct list_item));
    if (NULL == temp) {
        BTL_ERROR(("malloc error"));
        return 1;
    }

    temp->item = client;
    temp->next = NULL;
    if (NULL != *tail)
        (*tail)->next = temp;
    *tail = temp;
 
    if (NULL == *head) {
        *head = temp;
    }

    return 0;
}

static struct rdmacm_contents *list_del(struct list_item **head)
{
    struct rdmacm_contents *temp;
    struct list_item *temp_item;

    if (NULL == *head)
        return NULL;

    temp_item = *head;
    temp = (*head)->item;
    *head = (*head)->next;
    free(temp_item);

    return temp;
}

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

static mca_btl_openib_endpoint_t *rdmacm_find_endpoint(struct rdmacm_contents *local, struct rdma_cm_id *id, uint16_t rem_port)
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
                     message->ipaddr, message->port, peeripaddr));
        if (message->ipaddr == peeripaddr && message->port == rem_port) {
            ep = ib_endpoint;
            break;
        }
    }

    if (NULL == ep)
        BTL_ERROR(("can't find suitable endpoint for this peer"));

    return ep;
}

static void rdmacm_cleanup(struct rdmacm_contents *local, struct rdma_cm_id *id, uint32_t num)
{
    BTL_VERBOSE(("Calling cleanup for %s, qp = %d", local->server?"server":"client", num));

    if (NULL == id)
        return;

    free(id->context);
    id->context = NULL;

    rdma_destroy_id(id);
    if (!local->server && local->id && local->id[num]) {
        local->id[num] = NULL;
    }
}

static int rdmacm_setup_qp(struct rdmacm_contents *local, mca_btl_openib_endpoint_t *endpoint, struct rdma_cm_id *id, int qpnum)
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

    /* create the qp via rdma_create_qp() */
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

    local->openib_btl->ib_inline_max = attr.cap.max_inline_data;

    BTL_VERBOSE(("QP#%d is %p", qpnum, endpoint->qps[qpnum].qp->lcl_qp));

    return 0;

out:
    return 1;
}

static int rdma_client_connect_one(struct rdmacm_contents *local, struct message *message, int num)
{
    struct sockaddr_in din;
    int rc;

    /* create rdma_cm_id */
    rc = rdma_create_id(event_channel, &local->id[num], (void *)(unsigned long)num, RDMA_PS_TCP);
    if (0 != rc) {
        BTL_ERROR(("Failed to create a rdma id with %d", rc));
        goto out;
    }

    local->id[num]->context = malloc(sizeof(struct id_contexts));
    if (NULL == local->id[num]->context) {
        BTL_ERROR(("malloc error"));
        goto out;
    }

    ((struct id_contexts *)local->id[num]->context)->local = local;
    ((struct id_contexts *)local->id[num]->context)->qpnum = num;
    ((struct id_contexts *)local->id[num]->context)->endpoint = local->endpoint;

    BTL_VERBOSE(("Attemping to send to remote port %d ipaddr %x", message->port, message->ipaddr));

    memset(&din, 0, sizeof(din));
    din.sin_family = AF_INET;
    din.sin_addr.s_addr = message->ipaddr;
    din.sin_port = message->port;

    /* resolve the remote address via rdma_resolve_addr() */
    rc = rdma_resolve_addr(local->id[num], NULL, (struct sockaddr *)&din, RDMA_RESOLVE_ADDR_TIMEOUT);
    if (0 != rc) {
        BTL_ERROR(("Failed to resolve the remote address with %d", rc));
        goto out;
    }

    return OMPI_SUCCESS;

out:
    rdmacm_cleanup(local, local->id[num], num);

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

static int rdmacm_module_start_connect(ompi_btl_openib_connect_base_module_t *cpc,
                                        mca_btl_base_endpoint_t *endpoint)
{
    int rc;
    struct rdmacm_contents *client;
    struct message *message = (struct message *)endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    opal_mutex_t *endpoint_state_lock = &((struct rdmacm_endpoint_local_cpc_data *)endpoint->endpoint_local_cpc_data)->endpoint_state_lock;

    /*Already holding endpoint lock when this is called */
    OPAL_THREAD_LOCK(endpoint_state_lock);

    BTL_VERBOSE(("remote ip addr = %x, port = %d  ep state = %d", message->ipaddr, message->port, endpoint->endpoint_state));
    BTL_VERBOSE(("%p local message = %x %d", endpoint->endpoint_local_cpc->data.cbm_modex_message, ((struct message *)endpoint->endpoint_local_cpc->data.cbm_modex_message)->ipaddr, ((struct message *)endpoint->endpoint_local_cpc->data.cbm_modex_message)->port));

    if (MCA_BTL_IB_CONNECTED == endpoint->endpoint_state ||
        MCA_BTL_IB_CONNECTING == endpoint->endpoint_state ||
        MCA_BTL_IB_CONNECT_ACK == endpoint->endpoint_state) {
        OPAL_THREAD_UNLOCK(endpoint_state_lock);
        return OMPI_SUCCESS;
    }

    endpoint->endpoint_state = MCA_BTL_IB_CONNECT_ACK;
    OPAL_THREAD_UNLOCK(endpoint_state_lock);

    client = calloc(1, sizeof(struct rdmacm_contents));
    if (NULL == client) {
        BTL_ERROR(("malloc of client failed"));
        goto out;
    }

    client->openib_btl = endpoint->endpoint_btl;
    client->endpoint = endpoint;
    client->server = false;
    client->port = ((struct message *)endpoint->endpoint_local_cpc->data.cbm_modex_message)->port;

    rc = rdma_client_connect(client, message);
    if (0 != rc) {
        BTL_ERROR(("rdma_client_connect error"));
        goto out;
    }

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
}

static int handle_connect_request(struct rdmacm_contents *local, struct rdma_cm_event *event)
{
    struct rdma_conn_param conn_param;
    int rc = -1, qpnum;
    struct message *message;
    mca_btl_openib_endpoint_t *endpoint;
    opal_mutex_t *endpoint_state_lock;
    struct conn_message msg;
    uint16_t rem_port;

    qpnum = ((struct conn_message *)event->param.conn.private_data)->qpnum;
    rem_port = ((struct conn_message *)event->param.conn.private_data)->rem_port;

    endpoint = rdmacm_find_endpoint(local, event->id, rem_port);
    if (NULL == endpoint) {
        BTL_ERROR(("Failed to find endpoint"));
        return -1;
    }
    endpoint_state_lock = &((struct rdmacm_endpoint_local_cpc_data *)endpoint->endpoint_local_cpc_data)->endpoint_state_lock;

    OPAL_THREAD_LOCK(endpoint_state_lock);

    message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;

    BTL_VERBOSE(("ep state = %d, local ipaddr = %x, remote ipaddr = %x port %d", endpoint->endpoint_state, local->ipaddr, message->ipaddr, rem_port));

    /* See if a race is occurring between the two sides attempting to connect to each other at the same time */
    if ((local->ipaddr > message->ipaddr && local->port > rem_port) ||
        local->ipaddr > message->ipaddr) {
        int race = 1;

        OPAL_THREAD_UNLOCK(endpoint_state_lock);
        BTL_VERBOSE(("No soup today.....back of line! ep state = %d", endpoint->endpoint_state));

        rc = rdma_reject(event->id, &race, sizeof(int));
        if (0 != rc) {
            BTL_ERROR(("rdma_reject failed %d", rc));
            goto out;
        }

        if (0 == qpnum) {
            rdmacm_module_start_connect(NULL, endpoint);
        }

        return 0;
    }

    endpoint->endpoint_state = MCA_BTL_IB_CONNECTING;
    OPAL_THREAD_UNLOCK(endpoint_state_lock);

    endpoint->rem_info.rem_index = ((struct conn_message *)event->param.conn.private_data)->rem_index;

    /* Setup QP for new connection */
    BTL_VERBOSE(("ACCEPTING src port = %d, dst port = %d, qpnum = %d", rdma_get_src_port(event->id), rdma_get_dst_port(event->id), qpnum));

    rc = rdmacm_setup_qp(local, endpoint, event->id, qpnum);
    if (0 != rc) {
        BTL_ERROR(("rdmacm_setup_qp error %d", rc));
        goto out; 
    }

    if (BTL_OPENIB_QP_TYPE_PP(qpnum)) {
        rc = mca_btl_openib_endpoint_post_rr(endpoint, qpnum);
    } else {
        rc = mca_btl_openib_post_srr(endpoint->endpoint_btl, qpnum);
    }
    if (OMPI_SUCCESS != rc) {
        BTL_ERROR(("mca_btl_openib_endpoint_post_rr_nolock error %d", rc));
        goto out1; 
    }

    memset(&conn_param, 0, sizeof(conn_param));
    conn_param.responder_resources = 1;
    conn_param.initiator_depth = 1;
    conn_param.private_data = &msg;
    conn_param.private_data_len = sizeof(struct conn_message);

    msg.qpnum = qpnum;
    msg.rem_index = endpoint->index;

    event->id->context = malloc(sizeof(struct id_contexts));
    if (NULL == event->id->context) {
        BTL_ERROR(("malloc error"));
        goto out1;
    }

    ((struct id_contexts *)event->id->context)->local = local;
    ((struct id_contexts *)event->id->context)->qpnum = qpnum;
    ((struct id_contexts *)event->id->context)->endpoint = endpoint;

    rc = rdma_accept(event->id, &conn_param);
    if (0 != rc) {
        BTL_ERROR(("rdma_accept error %d", rc));
        goto out2;
    }

    BTL_VERBOSE(("Exiting handle_connect_request, qpnum = %d ", qpnum));
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
    BTL_VERBOSE(("rdmacm_event_dispatch unregistering..."));

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

    BTL_VERBOSE(("cleaning done"));
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
    struct list_item *temp;

    BTL_VERBOSE(("Start disconnecting..."));

    if (NULL == endpoint) {
        BTL_ERROR(("Attempting to shutdown a NULL endpoint"));
        return 0;
    }

    for (temp = client_list_head; NULL != temp; temp = temp->next) {
        if (endpoint == temp->item->endpoint) {
            int i;
            for (i = 0; i < mca_btl_openib_component.num_qps; i++)
                if (NULL != temp->item->id[i] &&
                    NULL != temp->item->id[i]->qp &&
                    NULL != temp->item->endpoint->qps) {
                        rdma_disconnect(temp->item->id[i]);
                }
        }
    }

    BTL_VERBOSE(("Finished disconnecting"));
    return 0;
}

/*
 * Callback (from main thread) when the endpoint has been connected
 */
static void *local_endpoint_connected(void *context)
{
    mca_btl_openib_endpoint_t *endpoint = (mca_btl_openib_endpoint_t *)context;

    mca_btl_openib_endpoint_connected(endpoint);
    BTL_VERBOSE(("endpoint connected"));

    return NULL;
}

static int rdmacm_connect_endpoint(struct rdmacm_contents *local, struct rdma_cm_event *event)
{
    struct message *message;
    mca_btl_openib_endpoint_t *endpoint;

    if (local->server)
        endpoint = ((struct id_contexts *)event->id->context)->endpoint;
    else {
        int rc;

        endpoint = local->endpoint;
        local->endpoint->rem_info.rem_index = ((struct conn_message *)event->param.conn.private_data)->rem_index;

        rc = list_add(local, &client_list_head, &client_list_tail);
        if (0 != rc) {
            BTL_ERROR(("list_add error"));
            return -1;
        }
    }
    if (NULL == endpoint) {
        BTL_ERROR(("Can't find endpoint"));
        return -1;
    }

    if (++((struct rdmacm_endpoint_local_cpc_data *)endpoint->endpoint_local_cpc_data)->rdmacm_counter < mca_btl_openib_component.num_qps) {
        BTL_VERBOSE(("%s count == %d", local->server?"server":"client", ((struct rdmacm_endpoint_local_cpc_data *)endpoint->endpoint_local_cpc_data)->rdmacm_counter));
        return 0;
    }

    message = endpoint->endpoint_remote_cpc_data->cbm_modex_message;
    if (local->server) {
        BTL_VERBOSE(("zzzzzzzzz! local %x remote %x state = %d", local->ipaddr, message->ipaddr, endpoint->endpoint_state));
    } else {
        BTL_VERBOSE(("boiiiiiiing! local %x remote %x state = %d", local->ipaddr, message->ipaddr, endpoint->endpoint_state));
    }

    ompi_btl_openib_fd_schedule(local_endpoint_connected, endpoint);

    return 0;
}

static int start_connect(struct rdmacm_contents *local, int num)
{
    int rc;
    opal_mutex_t *endpoint_state_lock = &((struct rdmacm_endpoint_local_cpc_data *)local->endpoint->endpoint_local_cpc_data)->endpoint_state_lock;

    OPAL_THREAD_LOCK(endpoint_state_lock);
    if (MCA_BTL_IB_CONNECT_ACK != local->endpoint->endpoint_state) {
        OPAL_THREAD_UNLOCK(endpoint_state_lock);
        BTL_VERBOSE(("Connection race.  EP state = %d", local->endpoint->endpoint_state));
        return 0;
    }
    OPAL_THREAD_UNLOCK(endpoint_state_lock);

    /* resolve the remote route via rdma_resolve_route() */
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

    if ((localipaddr == remoteipaddr && local->port <= remoteport) ||
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
    msg.rem_port = local->port;

    BTL_VERBOSE(("Connecting from %x, port %d to %x", localipaddr, msg.rem_port, remoteipaddr));

    /* connect via rdma_connect() */
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
    int rc;
    void *temp = NULL;

    /* blocks until next cm_event */
    rc = rdma_get_cm_event(event_channel, &event);
    if (0 != rc) {
        BTL_ERROR(("rdma_get_cm_event error %d", rc));
        return NULL;
    }

    memcpy(&ecopy, event, sizeof(struct rdma_cm_event));
    if (event->param.conn.private_data_len > 0) {
        temp = malloc(event->param.conn.private_data_len);
        if (NULL == temp) {
           BTL_ERROR(("error mallocing memory"));
           return NULL;
        }
        memcpy(temp, event->param.conn.private_data, event->param.conn.private_data_len);
        ecopy.param.conn.private_data = temp;
    }
    rdma_ack_cm_event(event);

    rc = rdma_event_handler(&ecopy);
    if (0 != rc) {
        BTL_ERROR(("Error rdma_event_handler -- %s, status = %d",
                    rdma_event_str(ecopy.event),
                    ecopy.status));
    }

    if (NULL != temp)
        free(temp);

    return NULL;
}

static int rdmacm_query(mca_btl_openib_hca_t *hca)
{
    struct in_addr inetaddr;
    struct ibv_device_attr attr;
    int i, rc;
    bool valid_ip = 0;

    /* It's not an error if these things fail; perhaps RDMA CM is not
       supported on this HCA.  So just gracefully return "sorry,
       Charlie" */
    rc = build_rdma_addr_list();
    if (-1 == rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to find IP address for %s",
                            ibv_get_device_name(hca->ib_dev));
        return OMPI_ERR_NOT_SUPPORTED;
    }

    rc = ibv_query_device(hca->ib_dev_context, &attr);
    if (-1 == rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC system error (verbs failure)");
        return OMPI_ERROR;
    }

    for (i = 1; i <= attr.phys_port_cnt; i++) {
        inetaddr.s_addr = rdma_get_ipv4addr(hca->ib_dev_context, i);
        BTL_VERBOSE(("dev %s port %d ipaddr %s", hca->ib_dev->name, i, inet_ntoa(inetaddr)));
        if (0 != inetaddr.s_addr) {
            valid_ip = 1;
        }
    }

    if (valid_ip == 1) {
        return rdmacm_priority;
    }

    opal_output_verbose(5, mca_btl_base_output,
                        "openib BTL: rdmacm CPC unable to find IP address for %s",
                        ibv_get_device_name(hca->ib_dev));
    return OMPI_ERR_NOT_SUPPORTED;
}

static int rdmacm_init(mca_btl_openib_endpoint_t *endpoint)
{
    void *temp;

    temp = calloc(1, sizeof(struct rdmacm_endpoint_local_cpc_data));
    if (NULL == temp) {
        BTL_ERROR(("malloc failed"));
        goto out;
    }
    endpoint->endpoint_local_cpc_data = temp;

    return 0;
out:
    return -1;
}

static int ipaddrcheck(struct rdmacm_contents *server, mca_btl_openib_module_t *openib_btl)
{
    int rc, i;
    struct ibv_device_attr attr;

    rc = ibv_query_device(openib_btl->hca->ib_dev_context, &attr);
    if (-1 == rc) {
        BTL_ERROR(("ibv_query_device failed"));
        goto out;
    }

    for (i = 0; i < attr.phys_port_cnt; i++) {
        struct list_item *pitem;
        bool found = false;
        uint32_t temp = rdma_get_ipv4addr(openib_btl->hca->ib_dev_context, i+1);

        for (pitem = server_list_head; NULL != pitem; pitem = pitem->next) {
            BTL_VERBOSE(("paddr = %x, temp addr = %x", pitem->item->ipaddr, temp));
            if (pitem->item->ipaddr == temp || 0 == temp) {
                BTL_VERBOSE(("addr %x already exists", temp));
                found = true;
                break;
            } 
        }

        if (!found) {
            server->ipaddr = temp;
            server->port = rdma_get_src_port(server->id[0]);
            break;
        }
    }
    if (0 == server->ipaddr) {
        BTL_ERROR(("No IP address found"));
        goto out;
    }

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
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
    message->port = server->port;

    BTL_VERBOSE(("Message IP address is %x, port %d", message->ipaddr, message->port));
    data->cbm_modex_message = message;
    data->cbm_modex_message_len = sizeof(struct message);

    return OMPI_SUCCESS;

out:
    return OMPI_ERROR;
}

static int rdmacm_component_query(mca_btl_openib_module_t *openib_btl, 
                                   ompi_btl_openib_connect_base_module_t **cpc)
{
    struct rdmacm_contents *server = NULL;
    struct sockaddr_in sin;
    int rc;

    /* RDMACM is not supported if we have any XRC QPs */
    if (mca_btl_openib_component.num_xrc_qps > 0) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC not supported with XRC receive queues; skipped");
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto out;
    }

    rc = rdmacm_query(openib_btl->hca);
    if (rc < 0) {
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
        goto out;
    }

    server->id = malloc(sizeof(struct rdma_cm_id *));
    if (NULL == server->id) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    server->server = true;
    server->openib_btl = openib_btl;

    /* create an rdma_cm_id */
    rc = rdma_create_id(event_channel, &server->id[0], NULL, RDMA_PS_TCP);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC failed to create ID");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    server->id[0]->context = malloc(sizeof(struct id_contexts));
    if (NULL == server->id[0]->context) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC system error (malloc failed)");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    ((struct id_contexts *)server->id[0]->context)->local = server;
    ((struct id_contexts *)server->id[0]->context)->qpnum = 0;

    /* rdma_bind() */
    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = rdmacm_addr;
    sin.sin_port = rdmacm_port;

    rc = rdma_bind_addr(server->id[0], (struct sockaddr *) &sin);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to bind to address");
        rc = OMPI_ERR_UNREACH;
        goto out;
    }

    rc = ipaddrcheck(server, openib_btl);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm IP address not found on port");
        rc = OMPI_ERR_NOT_SUPPORTED;
        goto out;
    }

    /* rdma_listen() */
    /* FIXME - 1024 should be (num of connectors * mca_btl_openib_component.num_qps) */
    rc = rdma_listen(server->id[0], 1024);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to listen");
        rc = OMPI_ERR_UNREACH;
        goto out;
    }

    rc = create_message(server, openib_btl, &(*cpc)->data);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to create message");
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    rc = list_add(server, &server_list_head, &server_list_tail);
    if (0 != rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unable to add to list");
        goto out;
    }

    opal_output_verbose(5, mca_btl_base_output,
                        "openib BTL: rdmacm CPC available for use on %s",
                        ibv_get_device_name(openib_btl->hca->ib_dev));
    return OMPI_SUCCESS;

out:
    if (OMPI_ERR_NOT_SUPPORTED == rc) {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rdmacm CPC unavailable for use on %s; skipped",
                            ibv_get_device_name(openib_btl->hca->ib_dev));
    } else {
        opal_output_verbose(5, mca_btl_base_output,
                            "openib BTL: rmacm CPC unavailable for use on %s; fatal error %d (%s)",
                            ibv_get_device_name(openib_btl->hca->ib_dev), rc, 
                            opal_strerror(rc));
    }
    if (NULL != server) {
        rdmacm_server_cleanup(server);
    }
    return rc;
}

static int rdmacm_component_destroy(void)
{
    struct rdmacm_contents *local;
    int rc;

    while (NULL != (local = list_del(&client_list_head))) {
        rdmacm_destroy(local);
    }

    while (NULL != (local = list_del(&server_list_head))) {
        rdmacm_server_cleanup(local);
    }

    if (NULL != event_channel) {
        rc = ompi_btl_openib_fd_unmonitor(event_channel->fd, rdmacm_unmonitor, NULL);
        if (OMPI_SUCCESS != rc)
            BTL_ERROR(("Error disabling fd monitor"));
    }

    free_rdma_addr_list();

    BTL_VERBOSE(("rdmacm_component_destroy"));
    return OMPI_SUCCESS;
}

static int rdmacm_component_init(void)
{
    int rc;

    rc = build_rdma_addr_list();
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

uint64_t get_iwarp_subnet_id(struct ibv_device *ib_dev)
{
    struct rdma_addr_list *addr;

    for (addr = myaddrs; addr; addr = addr->next) {
        if (!strcmp(addr->dev_name, ib_dev->name)) {
            return addr->subnet;
        }
    }

    return 0;
}

static uint32_t rdma_get_ipv4addr(struct ibv_context *verbs, uint8_t port)
{
    struct rdma_addr_list *addr;

    for (addr = myaddrs; addr; addr = addr->next) {
        if (!strcmp(addr->dev_name, verbs->device->name) && 
            port == addr->dev_port) {
            return addr->addr;
        }
    }
    return 0;
}

static int dev_specified(char *name, uint32_t ipaddr, int port)
{
    char **list;

    if (NULL != mca_btl_openib_component.if_include) {
        int i;

        list = opal_argv_split(mca_btl_openib_component.if_include, ',');
        for (i = 0; NULL != list[i]; i++) {
            char **temp = opal_argv_split(list[i], ':');
            if (0 == strcmp(name, temp[0]) &&
                (NULL == temp[1] || port == atoi(temp[1]))) {
                return 0;
            }
        }

        return 1;
    }

    if (NULL != mca_btl_openib_component.if_exclude) {
        int i;

        list = opal_argv_split(mca_btl_openib_component.if_exclude, ',');
        for (i = 0; NULL != list[i]; i++) {
            char **temp = opal_argv_split(list[i], ':');
            if (0 == strcmp(name, temp[0]) &&
                (NULL == temp[1] || port == atoi(temp[1]))) {
                return 1;
            }
        }
    }

    return 0;
}

static int add_rdma_addr(struct ifaddrs *ifa)
{
    struct sockaddr_in *sinp;
    struct rdma_cm_id *cm_id;
    struct rdma_event_channel *ch;
    int rc = OMPI_SUCCESS;
    struct rdma_addr_list *myaddr;

    ch = rdma_create_event_channel();
    if (NULL == ch) {
        BTL_ERROR(("failed creating event channel"));
        rc = OMPI_ERROR;
        goto out1;
    }

    rc = rdma_create_id(ch, &cm_id, NULL, RDMA_PS_TCP);
    if (rc) {
        BTL_ERROR(("rdma_create_id returned %d", rc));
        rc = OMPI_ERROR;
        goto out2;
    }

    rc = rdma_bind_addr(cm_id, ifa->ifa_addr);
    if (rc) {
        rc = OMPI_SUCCESS;
        goto out3;
    }

    if (!cm_id->verbs ||
        0 == ((struct sockaddr_in *)ifa->ifa_addr)->sin_addr.s_addr ||
        dev_specified(cm_id->verbs->device->name, ((struct sockaddr_in *)ifa->ifa_addr)->sin_addr.s_addr, cm_id->port_num)) {
        goto out3;
    }

    myaddr = malloc(sizeof *myaddr);
    if (NULL == myaddr) {
        BTL_ERROR(("malloc failed!"));
        rc = OMPI_ERROR;
        goto out3;
    }

    sinp = (struct sockaddr_in *)ifa->ifa_addr;
    myaddr->addr = sinp->sin_addr.s_addr;
    myaddr->subnet = myaddr->addr & ((struct sockaddr_in *)ifa->ifa_netmask)->sin_addr.s_addr;
    inet_ntop(sinp->sin_family, &sinp->sin_addr, 
            myaddr->addr_str, sizeof myaddr->addr_str);
    memcpy(myaddr->dev_name, cm_id->verbs->device->name, IBV_SYSFS_NAME_MAX);
    myaddr->dev_port = cm_id->port_num;
    BTL_VERBOSE(("adding addr %s dev %s port %d to rdma_addr_list", 
                 myaddr->addr_str, myaddr->dev_name, myaddr->dev_port));

    myaddr->next = myaddrs;
    myaddrs = myaddr;

out3:
    rdma_destroy_id(cm_id);
out2:
    rdma_destroy_event_channel(ch);
out1:
    return rc;
}
  
static int build_rdma_addr_list(void)
{
    int rc;
    struct ifaddrs *ifa_list, *ifa;

    rc = getifaddrs(&ifa_list);
    if (-1 == rc) {
        return OMPI_ERROR;
    }

    ifa = ifa_list;
    while (ifa) {
        if (ifa->ifa_addr->sa_family == AF_INET) {
            rc = add_rdma_addr(ifa);
            if (OMPI_SUCCESS != rc) {
                break;
            }
        }
        ifa = ifa->ifa_next;
    }
    freeifaddrs(ifa_list);
    return rc;
}
  
static void free_rdma_addr_list(void)
{
    struct rdma_addr_list *addr, *tmp;

    addr = myaddrs;
    while (addr) {
        tmp = addr->next;
        free(addr);
        addr = tmp;
    }
}
