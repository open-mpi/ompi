/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/proc_info.h"

#include "orte/mca/routed/routed.h"

#include "oob_ud.h"

#define min(a,b) ((a) < (b) ? (a) : (b))

static int mca_oob_ud_module_fini (void);

mca_oob_t mca_oob_ud_module = {
    mca_oob_ud_module_init,
    mca_oob_ud_module_fini,

    mca_oob_ud_get_addr,
    mca_oob_ud_set_addr,

    mca_oob_ud_ping,

    mca_oob_ud_send_nb,

    mca_oob_ud_recv_nb,
    mca_oob_ud_recv_cancel,

    mca_oob_ud_ft_event
};

void mca_oob_ud_device_construct (mca_oob_ud_device_t *device);
void mca_oob_ud_device_destruct (mca_oob_ud_device_t *device);

OBJ_CLASS_INSTANCE(mca_oob_ud_device_t, opal_list_item_t,
                   mca_oob_ud_device_construct,
                   mca_oob_ud_device_destruct);


void mca_oob_ud_port_construct (mca_oob_ud_port_t *port);
void mca_oob_ud_port_destruct (mca_oob_ud_port_t *port);

OBJ_CLASS_INSTANCE(mca_oob_ud_port_t, opal_list_item_t,
                   mca_oob_ud_port_construct,
                   mca_oob_ud_port_destruct);

/* uri must be at least 27 bytes in size */
void mca_oob_ud_port_get_uri (mca_oob_ud_port_t *port, char *uri)
{
    sprintf (uri, "ud://%u.%u.%u", port->listen_qp.ib_qp->qp_num,
             port->lid, port->port_num);
}

char *mca_oob_ud_get_addr (void)
{
    /* NTH: qp_num - 32 bits (10), lid - 16 bits (5), port - 8 bits (3) + ud:// + 3 .'s + \0 = 27 chars */
    char *contact_info = (char *) calloc(opal_list_get_size(&mca_oob_ud_component.ud_devices) * 27, 1);
    char *ptr = contact_info;
    opal_list_item_t *item, *port_item;
    *ptr = 0;

    for (item = opal_list_get_first (&mca_oob_ud_component.ud_devices) ;
         item != opal_list_get_end (&mca_oob_ud_component.ud_devices) ;
         item = opal_list_get_next (item)) {

        mca_oob_ud_device_t *device = (mca_oob_ud_device_t *) item;

        for (port_item = opal_list_get_first (&device->ports);
             port_item != opal_list_get_end (&device->ports);
             port_item = opal_list_get_next (port_item)) {

            if (ptr != contact_info) {
                ptr += sprintf (ptr, ";");
            }

            mca_oob_ud_port_get_uri ((mca_oob_ud_port_t *) port_item, ptr);
            ptr += strlen (ptr);
        }
    }

    OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:get_addr contact information: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), contact_info));

    return contact_info;
}

int mca_oob_ud_set_addr (const orte_process_name_t *name, const char *uri)
{
    mca_oob_ud_peer_t *peer = NULL;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:set_addr: setting location for peer %s from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(name), uri));


    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_lock);

    (void) mca_oob_ud_peer_lookup (name, &peer);

    if (NULL == uri) {
        if (NULL != peer) {
            mca_oob_ud_peer_release (peer);
        }

        peer = NULL;
    } else if (NULL == peer) {
        peer = mca_oob_ud_peer_from_uri (uri);
        if (NULL == peer) {
            return ORTE_ERR_BAD_PARAM;
        }
    } else {
        rc = mca_oob_ud_peer_update_with_uri (peer, uri);

        if (ORTE_SUCCESS != rc) {
            return rc;
        }
    }

    if (NULL != peer) {
        peer->peer_name = *name;
        peer->needs_notification = true;
    }

    opal_hash_table_set_value_uint64(&mca_oob_ud_component.ud_peers,
                                     orte_util_hash_name(name),
                                     (void *)peer);

    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_lock);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_listen_create (mca_oob_ud_port_t *port) {
    return mca_oob_ud_qp_init (&port->listen_qp, port, port->device->ib_channel, NULL, false);
}

/* mca_oob_ud_listen_destroy:
 *
 * Destory the listen queue pair associated with a port.
 */
static int mca_oob_ud_listen_destroy (mca_oob_ud_port_t *port)
{
    if (NULL == port || NULL == port->listen_qp.ib_qp) {
        return ORTE_SUCCESS;
    }

    OBJ_DESTRUCT(&port->listen_qp);

    return ORTE_SUCCESS;
}

static inline int mca_oob_ud_port_recv_start (mca_oob_ud_port_t *port)
{
    int i, rc;

    rc = mca_oob_ud_qp_to_rts (&port->listen_qp);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((1, mca_oob_base_output, "%s oob:ud:port_recv_start posting"
                         "%d message buffers", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         mca_oob_ud_component.ud_recv_buffer_count));

    for (i = 0 ; i < mca_oob_ud_component.ud_recv_buffer_count ; ++i) {
        rc = mca_oob_ud_port_post_one_recv (port, i);
        if (ORTE_SUCCESS != rc) {
            return rc;
        }
    }

    rc = ibv_req_notify_cq (port->listen_qp.ib_recv_cq, 0);
    if (0 != rc) {
        opal_output (0, "%s oob:ud:port_recv_start error requesting completion"
                     "notifications. rc = %d, errno = %d",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), rc, errno);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static inline int mca_oob_ud_alloc_reg_mem (struct ibv_pd *pd, mca_oob_ud_reg_mem_t *reg_mem,
                                            const int buffer_len)
{
    reg_mem->len = buffer_len;
    reg_mem->ptr = NULL;
    reg_mem->mr  = NULL;

    OPAL_OUTPUT_VERBOSE ((5, mca_oob_base_output, "%s oob:ud:alloc_reg_mem allocing and"
                          "registering %d bytes of memory with pd %p",
                          ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), buffer_len, (void *) pd));

    posix_memalign ((void **)&reg_mem->ptr, sysconf(_SC_PAGESIZE), buffer_len);
    if (NULL == reg_mem->ptr) {
        opal_output (0, "%s oob:ud:alloc_reg_mem malloc failed! errno = %d",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    memset (reg_mem->ptr, 0, buffer_len);

    reg_mem->mr = ibv_reg_mr (pd, reg_mem->ptr, buffer_len, IBV_ACCESS_LOCAL_WRITE);
    if (NULL == reg_mem->mr) {
        opal_output (0, "%s oob:ud:alloc_reg_mem failed to register memory. errno = %d",
                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static inline void mca_oob_ud_free_reg_mem (mca_oob_ud_reg_mem_t *reg_mem)
{
    if (reg_mem->mr) {
        (void) ibv_dereg_mr (reg_mem->mr);
    }

    if (reg_mem->ptr) {
        free (reg_mem->ptr);
    }

    memset (reg_mem, 0, sizeof (mca_oob_ud_reg_mem_t));
}

static int mca_oob_ud_port_alloc_buffers (mca_oob_ud_port_t *port) {
    int total_buffer_count = mca_oob_ud_component.ud_recv_buffer_count +
        mca_oob_ud_component.ud_send_buffer_count;
    opal_list_item_t *item;
    int rc, i;

    rc = mca_oob_ud_alloc_reg_mem (port->device->ib_pd, &port->grh_buf,
                                   mca_oob_ud_component.ud_recv_buffer_count * sizeof (struct ibv_grh));
    if (ORTE_SUCCESS != rc) {
        return rc;
    }


    rc = mca_oob_ud_alloc_reg_mem (port->device->ib_pd, &port->msg_buf,
                                   total_buffer_count * port->mtu);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    rc = opal_free_list_init (&port->free_msgs, sizeof (mca_oob_ud_msg_t),
                              OBJ_CLASS(mca_oob_ud_msg_t), mca_oob_ud_component.ud_send_buffer_count,
                              mca_oob_ud_component.ud_send_buffer_count, 0);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    for (i = 0, item = opal_list_get_first (&port->free_msgs.super) ;
         item != opal_list_get_end (&port->free_msgs.super) ;
         item = opal_list_get_next (item), ++i) {
        char *ptr = port->msg_buf.ptr + (i + mca_oob_ud_component.ud_recv_buffer_count) *
            port->mtu;

        mca_oob_ud_msg_init ((mca_oob_ud_msg_t *) item, port,
                             ptr, port->msg_buf.mr);
    }

    return rc;
}

int mca_oob_ud_port_post_one_recv (mca_oob_ud_port_t *port, int msg_num)
{
    char *grh_buf = port->grh_buf.ptr + msg_num * sizeof (struct ibv_grh);
    char *msg_buf = port->msg_buf.ptr + msg_num * port->mtu;
    struct ibv_recv_wr wr;
    struct ibv_sge sge[2];

    /* GRH */
    mca_oob_ud_fill_sge(sge, grh_buf, sizeof (struct ibv_grh), port->grh_buf.mr->lkey);

    /* message */
    mca_oob_ud_fill_sge(sge + 1, msg_buf, port->mtu, port->msg_buf.mr->lkey);

    mca_oob_ud_fill_recv_wr (&wr, sge, 2);
    wr.wr_id   = MCA_OOB_UD_RECV_WR | (uint64_t)msg_num;

    return mca_oob_ud_qp_post_recv (&port->listen_qp, &wr);
}

static bool module_has_been_inited = false;

int mca_oob_ud_module_init (void)
{
    opal_list_item_t *item, *item2;
    int rc;
    bool found_one = false;

    /* protect against repeat inits */
    if (module_has_been_inited) {
        return ORTE_SUCCESS;
    }
    module_has_been_inited = true;

    OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:init initializing oob/openib. # of devices = %u",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (unsigned int) opal_list_get_size (&mca_oob_ud_component.ud_devices)));

    for (item = opal_list_get_first (&mca_oob_ud_component.ud_devices);
         item != opal_list_get_end (&mca_oob_ud_component.ud_devices);
         item = opal_list_get_next (item)) {
        mca_oob_ud_device_t *device = (mca_oob_ud_device_t *) item;

        /* start monitoring the device for completions */
        for (item2 = opal_list_get_first (&device->ports) ; 
             item2 != opal_list_get_end (&device->ports) ;
             item2 = opal_list_get_next (item2)) {
            mca_oob_ud_port_t *port = (mca_oob_ud_port_t *) item2;

            rc = mca_oob_ud_listen_create (port);
            if (0 != rc) {
                continue;
            }

            rc = mca_oob_ud_port_alloc_buffers (port);
            if (ORTE_SUCCESS != rc) {
                mca_oob_ud_listen_destroy (port);
                continue;
            }

            rc = opal_free_list_init (&port->data_qps,
                                      sizeof (mca_oob_ud_qp_t),
                                      OBJ_CLASS(mca_oob_ud_qp_t),
                                      mca_oob_ud_component.ud_min_qp,
                                      mca_oob_ud_component.ud_max_qp,
                                      2);
            if (OPAL_SUCCESS != rc) {
                mca_oob_ud_listen_destroy (port);
                continue;
            }

            rc = mca_oob_ud_port_recv_start (port);
            if (ORTE_SUCCESS != rc) {
                mca_oob_ud_listen_destroy (port);
                continue;
            }

            /* NTH: only supports one port for now */
            found_one = true;

            /* NTH: since we only support one port start monitoring now */
            mca_oob_ud_event_start_monitor (device);

            break;
        }
    }

    if (!found_one) {
        return ORTE_ERR_NOT_FOUND;
    }

    return ORTE_SUCCESS;
}

static void mca_oob_ud_cancel_all_in_list (opal_list_t *list)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (list))) {
        ((mca_oob_ud_req_t *)item)->req_list = NULL;
        mca_oob_ud_req_abort ((mca_oob_ud_req_t *) item);
    }
}

static void mca_oob_ud_empty_list (opal_list_t *list)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (list))) {
        OBJ_RELEASE(item);
    }
}

static int mca_oob_ud_module_fini (void)
{
    mca_oob_ud_peer_t *peer;
    opal_list_item_t *item;
    uint64_t key;
    void *node;
    int rc;

    OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:fini entering",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_lock);
    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_match_lock);

    if (ORTE_VPID_INVALID != ORTE_PROC_MY_PARENT->vpid) {
        if (ORTE_SUCCESS == mca_oob_ud_peer_lookup (ORTE_PROC_MY_PARENT, &peer) && NULL != peer) {
            mca_oob_ud_peer_handle_end (peer);
        }
    }

    /* abort active receives */
    mca_oob_ud_cancel_all_in_list (&mca_oob_ud_component.ud_active_recvs);
    mca_oob_ud_cancel_all_in_list (&mca_oob_ud_component.ud_active_sends);
    mca_oob_ud_cancel_all_in_list (&mca_oob_ud_component.ud_pending_recvs);

    mca_oob_ud_empty_list (&mca_oob_ud_component.ud_unexpected_recvs);
    mca_oob_ud_empty_list (&mca_oob_ud_component.ud_event_queued_reqs);

    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_match_lock);

    rc = opal_hash_table_get_first_key_uint64 (&mca_oob_ud_component.ud_peers, &key,
                                               (void **) &peer, &node);
    if (OPAL_SUCCESS == rc) {
        do {
            if (NULL != peer) {
                mca_oob_ud_peer_release (peer);
            }
            rc = opal_hash_table_get_next_key_uint64 (&mca_oob_ud_component.ud_peers, &key,
                                                      (void **) &peer, node, &node);
        } while (OPAL_SUCCESS == rc);
    }

    opal_hash_table_remove_all (&mca_oob_ud_component.ud_peers);

    for (item = opal_list_get_first (&mca_oob_ud_component.ud_devices);
         item != opal_list_get_end (&mca_oob_ud_component.ud_devices);
         item = opal_list_get_next (item)) {
        mca_oob_ud_event_stop_monitor ((mca_oob_ud_device_t *) item);
    }

    mca_oob_ud_empty_list (&mca_oob_ud_component.ud_devices);
    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_lock);

    return 0;
}

void mca_oob_ud_device_construct (mca_oob_ud_device_t *device)
{
    memset((char *) device + sizeof (device->super), 0, sizeof (*device) - sizeof (device->super));

    OBJ_CONSTRUCT(&device->ports, opal_list_t);
}

void mca_oob_ud_device_destruct (mca_oob_ud_device_t *device)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (&device->ports))) {
        OBJ_RELEASE(item);
    }

    if (device->ib_pd) {
        (void) ibv_dealloc_pd (device->ib_pd);
    }

    if (device->ib_channel) {
        (void) ibv_destroy_comp_channel (device->ib_channel);
    }

    if (device->ib_context) {
        (void) ibv_close_device (device->ib_context);
    }

    OBJ_DESTRUCT(&device->ports);

    memset (device, 0, sizeof (mca_oob_ud_device_t));
}

void mca_oob_ud_port_construct (mca_oob_ud_port_t *port)
{
    memset((char *) port + sizeof (port->super), 0, sizeof (*port) - sizeof (port->super));

    OBJ_CONSTRUCT(&port->data_qps, opal_free_list_t);
    OBJ_CONSTRUCT(&port->free_msgs, opal_free_list_t);
    OBJ_CONSTRUCT(&port->listen_qp, opal_free_list_item_t);
}

void mca_oob_ud_port_destruct (mca_oob_ud_port_t *port)
{
    (void) mca_oob_ud_listen_destroy (port);
    OBJ_DESTRUCT(&port->data_qps);
    OBJ_DESTRUCT(&port->free_msgs);

    mca_oob_ud_free_reg_mem (&port->grh_buf);
    mca_oob_ud_free_reg_mem (&port->msg_buf);
}

int mca_oob_ud_ft_event(int state) {
    return ORTE_SUCCESS;
}

int mca_oob_ud_register_iov (struct iovec *iov, int count, struct ibv_mr **ib_mr,
                             struct ibv_pd *ib_pd, unsigned int mtu, int *sge_countp,
                             int *wr_countp, int *data_lenp)
{
    int data_len, iov_index, sge_count;
    unsigned int packet_size = 0;

    *wr_countp  = 0;
    *data_lenp  = 0;
    *sge_countp = 0;

    for (iov_index = 0, data_len = 0, sge_count = 0 ; iov_index < count ; ++iov_index) {
        unsigned int iov_left = iov[iov_index].iov_len;

        data_len += iov_left;

        sge_count++;

        do {
            unsigned int to_trans = min (iov_left, mtu - packet_size);

            packet_size = (to_trans < iov_left) ? 0 : packet_size + to_trans;
            iov_left    -= to_trans;

            if (0 == packet_size && iov_left) {
                sge_count++;
            }
        } while (iov_left);

        /* register buffers */
        if (NULL == ib_mr[iov_index]) {
            ib_mr[iov_index] = ibv_reg_mr (ib_pd,
                                           iov[iov_index].iov_base,
                                           iov[iov_index].iov_len,
                                           IBV_ACCESS_LOCAL_WRITE |
                                           IBV_ACCESS_REMOTE_WRITE);
            if (NULL == ib_mr[iov_index]) {
                /* Ruh-roh */
                opal_output (0, "%s oob:ud:register_iov error registering memory. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    *wr_countp  = (data_len + mtu - 1) / mtu;
    *sge_countp = sge_count;
    *data_lenp  = data_len;

    return ORTE_SUCCESS;
}
