/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2008-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Bull SAS.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"

#include <infiniband/verbs.h>
#include <fcntl.h>
#include <sys/poll.h>
#include <unistd.h>
#include <errno.h>

#include "opal/util/show_help.h"
#include "opal/util/proc.h"

#include "opal/mca/btl/base/base.h"
#include "btl_openib.h"
#include "btl_openib_mca.h"
#include "btl_openib_async.h"
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"

static opal_list_t ignore_qp_err_list;
static opal_mutex_t ignore_qp_err_list_lock;
static int32_t btl_openib_async_device_count = 0;

struct mca_btl_openib_async_poll {
    int active_poll_size;
    int poll_size;
    struct pollfd *async_pollfd;
};
typedef struct mca_btl_openib_async_poll mca_btl_openib_async_poll;

typedef struct {
    opal_list_item_t super;
    struct ibv_qp *qp;
} mca_btl_openib_qp_list;

OBJ_CLASS_INSTANCE(mca_btl_openib_qp_list, opal_list_item_t, NULL, NULL);

static const char *openib_event_to_str (enum ibv_event_type event);

/* Function converts event to string (name)
 * Open Fabris don't have function that do this job :(
 */
static const char *openib_event_to_str (enum ibv_event_type event)
{
    switch (event) {
    case IBV_EVENT_CQ_ERR:
        return "IBV_EVENT_CQ_ERR";
    case IBV_EVENT_QP_FATAL:
        return "IBV_EVENT_QP_FATAL";
    case IBV_EVENT_QP_REQ_ERR:
        return "IBV_EVENT_QP_REQ_ERR";
    case IBV_EVENT_QP_ACCESS_ERR:
        return "IBV_EVENT_QP_ACCESS_ERR";
    case IBV_EVENT_PATH_MIG:
        return "IBV_EVENT_PATH_MIG";
    case IBV_EVENT_PATH_MIG_ERR:
        return "IBV_EVENT_PATH_MIG_ERR";
    case IBV_EVENT_DEVICE_FATAL:
        return "IBV_EVENT_DEVICE_FATAL";
    case IBV_EVENT_SRQ_ERR:
        return "IBV_EVENT_SRQ_ERR";
    case IBV_EVENT_PORT_ERR:
        return "IBV_EVENT_PORT_ERR";
    case IBV_EVENT_COMM_EST:
        return "IBV_EVENT_COMM_EST";
    case IBV_EVENT_PORT_ACTIVE:
        return "IBV_EVENT_PORT_ACTIVE";
    case IBV_EVENT_SQ_DRAINED:
        return "IBV_EVENT_SQ_DRAINED";
    case IBV_EVENT_LID_CHANGE:
        return "IBV_EVENT_LID_CHANGE";
    case IBV_EVENT_PKEY_CHANGE:
        return "IBV_EVENT_PKEY_CHANGE";
    case IBV_EVENT_SM_CHANGE:
        return "IBV_EVENT_SM_CHANGE";
    case IBV_EVENT_QP_LAST_WQE_REACHED:
        return "IBV_EVENT_QP_LAST_WQE_REACHED";
#if HAVE_DECL_IBV_EVENT_CLIENT_REREGISTER
    case IBV_EVENT_CLIENT_REREGISTER:
        return "IBV_EVENT_CLIENT_REREGISTER";
#endif
    case IBV_EVENT_SRQ_LIMIT_REACHED:
        return "IBV_EVENT_SRQ_LIMIT_REACHED";
    default:
        return "UNKNOWN";
    }
}
/* QP to endpoint */
static mca_btl_openib_endpoint_t * qp2endpoint(struct ibv_qp *qp, mca_btl_openib_device_t *device)
{
    mca_btl_openib_endpoint_t *ep;
    int  ep_i, qp_i;
    for(ep_i = 0; ep_i < opal_pointer_array_get_size(device->endpoints); ep_i++) {
        ep = opal_pointer_array_get_item(device->endpoints, ep_i);
        for(qp_i = 0; qp_i < mca_btl_openib_component.num_qps; qp_i++) {
            if (qp == ep->qps[qp_i].qp->lcl_qp)
                return ep;
        }
    }
    return NULL;
}

#if OPAL_HAVE_CONNECTX_XRC
/* XRC recive QP to endpoint */
static mca_btl_openib_endpoint_t * xrc_qp2endpoint(uint32_t qp_num, mca_btl_openib_device_t *device)
{
    mca_btl_openib_endpoint_t *ep;
    int  ep_i;
    for(ep_i = 0; ep_i < opal_pointer_array_get_size(device->endpoints); ep_i++) {
        ep = opal_pointer_array_get_item(device->endpoints, ep_i);
        if (qp_num == ep->xrc_recv_qp_num)
            return ep;
    }
    return NULL;
}
#endif

/* Function inits mca_btl_openib_async_poll */

/* The main idea of resizing SRQ algorithm -
   We create a SRQ with size = rd_num, but for efficient usage of resources
   the number of WQEs that we post = rd_curr_num < rd_num and this value is
   increased (by needs) in IBV_EVENT_SRQ_LIMIT_REACHED event handler (i.e. in this function),
   the event will thrown by device if number of WQEs in SRQ will be less than srq_limit */
static int btl_openib_async_srq_limit_event(struct ibv_srq* srq)
{
    int qp, rc = OPAL_SUCCESS;
    mca_btl_openib_module_t *openib_btl = NULL;

    opal_mutex_t *lock = &mca_btl_openib_component.srq_manager.lock;
    opal_hash_table_t *srq_addr_table = &mca_btl_openib_component.srq_manager.srq_addr_table;

    opal_mutex_lock(lock);

    if (OPAL_SUCCESS != opal_hash_table_get_value_ptr(srq_addr_table,
                            &srq, sizeof(struct ibv_srq*), (void*) &openib_btl)) {
        /* If there isn't any element with the key in the table =>
           we assume that SRQ was destroyed and don't serve the event */
        goto srq_limit_event_exit;
    }

    for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        if (!BTL_OPENIB_QP_TYPE_PP(qp)) {
            if(openib_btl->qps[qp].u.srq_qp.srq == srq) {
                break;
            }
        }
    }

    if(qp >= mca_btl_openib_component.num_qps) {
        BTL_ERROR(("Open MPI tried to access a shared receive queue (SRQ) on the device %s that was not found.  This should not happen, and is a fatal error.  Your MPI job will now abort.\n", ibv_get_device_name(openib_btl->device->ib_dev)));
        rc = OPAL_ERROR;
        goto srq_limit_event_exit;
    }

    /* dynamically re-size the SRQ to be larger */
    openib_btl->qps[qp].u.srq_qp.rd_curr_num <<= 1;

    if(openib_btl->qps[qp].u.srq_qp.rd_curr_num >=
                         mca_btl_openib_component.qp_infos[qp].rd_num) {
        openib_btl->qps[qp].u.srq_qp.rd_curr_num = mca_btl_openib_component.qp_infos[qp].rd_num;
        openib_btl->qps[qp].u.srq_qp.rd_low_local = mca_btl_openib_component.qp_infos[qp].rd_low;

        openib_btl->qps[qp].u.srq_qp.srq_limit_event_flag = false;

        goto srq_limit_event_exit;
    }

    openib_btl->qps[qp].u.srq_qp.rd_low_local <<= 1;
    openib_btl->qps[qp].u.srq_qp.srq_limit_event_flag = true;

srq_limit_event_exit:
    opal_mutex_unlock(lock);
    return rc;
}

/* Function handle async device events */
static void btl_openib_async_device (int fd, short flags, void *arg)
{
    mca_btl_openib_device_t *device = (mca_btl_openib_device_t *) arg;
    struct ibv_async_event event;
    int event_type;

    if (ibv_get_async_event((struct ibv_context *)device->ib_dev_context,&event) < 0) {
        if (EWOULDBLOCK != errno) {
            BTL_ERROR(("Failed to get async event"));
        }

        return;
    }

    event_type = event.event_type;
#if OPAL_HAVE_CONNECTX_XRC
    /* is it XRC event ?*/
    bool xrc_event = false;
    if (IBV_XRC_QP_EVENT_FLAG & event.event_type) {
        xrc_event = true;
        /* Clean the bitnd handel as usual */
        event_type ^= IBV_XRC_QP_EVENT_FLAG;
    }
#endif
    switch(event_type) {
    case IBV_EVENT_PATH_MIG:
        BTL_ERROR(("Alternative path migration event reported"));
        if (APM_ENABLED) {
            BTL_ERROR(("Trying to find additional path..."));
#if OPAL_HAVE_CONNECTX_XRC
            if (xrc_event)
                mca_btl_openib_load_apm_xrc_rcv(event.element.xrc_qp_num,
                                                xrc_qp2endpoint(event.element.xrc_qp_num, device));
            else
#endif
                mca_btl_openib_load_apm(event.element.qp,
                                        qp2endpoint(event.element.qp, device));
        }
        break;
    case IBV_EVENT_DEVICE_FATAL:
        /* Set the flag to fatal */
        device->got_fatal_event = true;
        /* It is not critical to protect the counter */
        OPAL_THREAD_ADD32(&mca_btl_openib_component.error_counter, 1);
        /* fall through */
    case IBV_EVENT_CQ_ERR:
    case IBV_EVENT_QP_FATAL:
        if (event_type == IBV_EVENT_QP_FATAL) {
            mca_btl_openib_qp_list *qp_item;
            bool in_ignore_list = false;

            BTL_VERBOSE(("QP is in err state %p", (void *)event.element.qp));

            /* look through ignore list */
            opal_mutex_lock (&ignore_qp_err_list_lock);
            OPAL_LIST_FOREACH(qp_item, &ignore_qp_err_list, mca_btl_openib_qp_list) {
                if (qp_item->qp == event.element.qp) {
                    BTL_VERBOSE(("QP %p is in error ignore list",
                                 (void *)event.element.qp));
                    in_ignore_list = true;
                    break;
                }
            }
            opal_mutex_unlock (&ignore_qp_err_list_lock);

            if (in_ignore_list) {
                break;
            }
        }
        /* fall through */
    case IBV_EVENT_QP_REQ_ERR:
    case IBV_EVENT_QP_ACCESS_ERR:
    case IBV_EVENT_PATH_MIG_ERR:
    case IBV_EVENT_SRQ_ERR:
        opal_show_help("help-mpi-btl-openib.txt", "of error event",
                       true,opal_process_info.nodename, (int)getpid(),
                       event_type,
                       openib_event_to_str((enum ibv_event_type)event_type));
        break;
    case IBV_EVENT_PORT_ERR:
        opal_show_help("help-mpi-btl-openib.txt", "of error event",
                       true,opal_process_info.nodename, (int)getpid(),
                       event_type,
                       openib_event_to_str((enum ibv_event_type)event_type));
        /* Set the flag to indicate port error */
        device->got_port_event = true;
        OPAL_THREAD_ADD32(&mca_btl_openib_component.error_counter, 1);
        break;
    case IBV_EVENT_COMM_EST:
    case IBV_EVENT_PORT_ACTIVE:
    case IBV_EVENT_SQ_DRAINED:
    case IBV_EVENT_LID_CHANGE:
    case IBV_EVENT_PKEY_CHANGE:
    case IBV_EVENT_SM_CHANGE:
    case IBV_EVENT_QP_LAST_WQE_REACHED:
#if HAVE_DECL_IBV_EVENT_CLIENT_REREGISTER
    case IBV_EVENT_CLIENT_REREGISTER:
#endif
        break;
        /* The event is signaled when number of prepost receive WQEs is going
           under predefined threshold - srq_limit */
    case IBV_EVENT_SRQ_LIMIT_REACHED:
        (void) btl_openib_async_srq_limit_event (event.element.srq);

        break;
    default:
        opal_show_help("help-mpi-btl-openib.txt", "of unknown event",
                       true,opal_process_info.nodename, (int)getpid(),
                       event_type);
    }

    ibv_ack_async_event(&event);
}

static void apm_update_attr(struct ibv_qp_attr *attr, enum ibv_qp_attr_mask *mask)
{
    *mask = IBV_QP_ALT_PATH|IBV_QP_PATH_MIG_STATE;
    attr->alt_ah_attr.dlid = attr->ah_attr.dlid + 1;
    attr->alt_ah_attr.src_path_bits = attr->ah_attr.src_path_bits + 1;
    attr->alt_ah_attr.static_rate = attr->ah_attr.static_rate;
    attr->alt_ah_attr.sl = attr->ah_attr.sl;
    attr->alt_pkey_index = attr->pkey_index;
    attr->alt_port_num = attr->port_num;
    attr->alt_timeout = attr->timeout;
    attr->path_mig_state = IBV_MIG_REARM;
    BTL_VERBOSE(("New APM LMC loaded: alt_src_port:%d, dlid: %d, src_bits %d, old_src_bits: %d, old_dlid %d",
                attr->alt_port_num, attr->alt_ah_attr.dlid,
                attr->alt_ah_attr.src_path_bits, attr->ah_attr.src_path_bits, attr->ah_attr.dlid));
}

static int apm_update_port(mca_btl_openib_endpoint_t *ep,
        struct ibv_qp_attr *attr, enum ibv_qp_attr_mask *mask)
{
    size_t port_i;
    uint16_t apm_lid = 0;

    if (attr->port_num == ep->endpoint_btl->apm_port) {
        /* all ports were used */
        BTL_ERROR(("APM: already all ports were used port_num %d apm_port %d",
                    attr->port_num, ep->endpoint_btl->apm_port));
        return OPAL_ERROR;
    }
    /* looking for alternatve lid on remote site */
    for(port_i = 0; port_i < ep->endpoint_proc->proc_port_count; port_i++) {
        if (ep->endpoint_proc->proc_ports[port_i].pm_port_info.lid == attr->ah_attr.dlid - mca_btl_openib_component.apm_lmc) {
            apm_lid = ep->endpoint_proc->proc_ports[port_i].pm_port_info.apm_lid;
        }
    }
    if (0 == apm_lid) {
        /* APM was disabled on one of site ? */
        BTL_VERBOSE(("APM: Was disabled ? dlid %d %d %d", attr->ah_attr.dlid, attr->ah_attr.src_path_bits, ep->endpoint_btl->src_path_bits));
        return OPAL_ERROR;
    }
    /* We guess cthat the LMC is the same on all ports */
    attr->alt_ah_attr.static_rate = attr->ah_attr.static_rate;
    attr->alt_ah_attr.sl = attr->ah_attr.sl;
    attr->alt_pkey_index = attr->pkey_index;
    attr->alt_timeout = attr->timeout;
    attr->path_mig_state = IBV_MIG_REARM;
    *mask = IBV_QP_ALT_PATH|IBV_QP_PATH_MIG_STATE;

    attr->alt_port_num = ep->endpoint_btl->apm_port;
    attr->alt_ah_attr.src_path_bits = ep->endpoint_btl->src_path_bits;
    attr->alt_ah_attr.dlid = apm_lid;

    BTL_VERBOSE(("New APM port loaded: alt_src_port:%d, dlid: %d, src_bits: %d:%d, old_dlid %d",
                attr->alt_port_num, attr->alt_ah_attr.dlid,
                attr->ah_attr.src_path_bits, attr->alt_ah_attr.src_path_bits,
                attr->ah_attr.dlid));
    return OPAL_SUCCESS;
}

/* Load new dlid to the QP */
void mca_btl_openib_load_apm(struct ibv_qp *qp, mca_btl_openib_endpoint_t *ep)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr attr;
    enum ibv_qp_attr_mask mask = 0;
    struct mca_btl_openib_module_t *btl;

    BTL_VERBOSE(("APM: Loading alternative path"));
    assert (NULL != ep);
    btl = ep->endpoint_btl;

    if (ibv_query_qp(qp, &attr, mask, &qp_init_attr))
        BTL_ERROR(("Failed to ibv_query_qp, qp num: %d", qp->qp_num));

    if (mca_btl_openib_component.apm_lmc &&
            attr.ah_attr.src_path_bits - btl->src_path_bits < mca_btl_openib_component.apm_lmc) {
        BTL_VERBOSE(("APM LMC: src: %d btl_src: %d lmc_max: %d",
                    attr.ah_attr.src_path_bits,
                    btl->src_path_bits,
                    mca_btl_openib_component.apm_lmc));
        apm_update_attr(&attr, &mask);
    } else {
        if (mca_btl_openib_component.apm_ports) {
            /* Try to migrate to next port */
            if (OPAL_SUCCESS != apm_update_port(ep, &attr, &mask))
                return;
        } else {
            BTL_ERROR(("Failed to load alternative path, all %d were used",
                        attr.ah_attr.src_path_bits - btl->src_path_bits));
        }
    }

    if (ibv_modify_qp(qp, &attr, mask))
        BTL_ERROR(("Failed to ibv_query_qp, qp num: %d, errno says: %s (%d)",
                   qp->qp_num, strerror(errno), errno));
}

#if OPAL_HAVE_CONNECTX_XRC
void mca_btl_openib_load_apm_xrc_rcv(uint32_t qp_num, mca_btl_openib_endpoint_t *ep)
{
    struct ibv_qp_init_attr qp_init_attr;
    struct ibv_qp_attr attr;
    enum ibv_qp_attr_mask mask = 0;
    struct mca_btl_openib_module_t *btl;

    BTL_VERBOSE(("APM XRC: Loading alternative path"));
    assert (NULL != ep);
    btl = ep->endpoint_btl;

    if (ibv_query_xrc_rcv_qp(btl->device->xrc_domain, qp_num, &attr, mask, &qp_init_attr))
        BTL_ERROR(("Failed to ibv_query_qp, qp num: %d", qp_num));

    if (mca_btl_openib_component.apm_lmc &&
            attr.ah_attr.src_path_bits - btl->src_path_bits < mca_btl_openib_component.apm_lmc) {
        apm_update_attr(&attr, &mask);
    } else {
        if (mca_btl_openib_component.apm_ports) {
            /* Try to migrate to next port */
            if (OPAL_SUCCESS != apm_update_port(ep, &attr, &mask))
                return;
        } else {
            BTL_ERROR(("Failed to load alternative path, all %d were used",
                        attr.ah_attr.src_path_bits - btl->src_path_bits));
        }
    }

    ibv_modify_xrc_rcv_qp(btl->device->xrc_domain, qp_num, &attr, mask);

    /* Maybe the qp already was modified by other process - ignoring error */
}
#endif

int mca_btl_openib_async_init (void)
{
    if (!mca_btl_openib_component.use_async_event_thread ||
        mca_btl_openib_component.async_evbase) {
        return OPAL_SUCCESS;
    }

    mca_btl_openib_component.async_evbase = opal_progress_thread_init (NULL);

    OBJ_CONSTRUCT(&ignore_qp_err_list, opal_list_t);
    OBJ_CONSTRUCT(&ignore_qp_err_list_lock, opal_mutex_t);

    /* Set the error counter to zero */
    mca_btl_openib_component.error_counter = 0;

    return OPAL_SUCCESS;
}

void mca_btl_openib_async_fini (void)
{
    if (mca_btl_openib_component.async_evbase) {
        OPAL_LIST_DESTRUCT(&ignore_qp_err_list);
        OBJ_DESTRUCT(&ignore_qp_err_list_lock);
        opal_progress_thread_finalize (NULL);
        mca_btl_openib_component.async_evbase = NULL;
    }
}

void mca_btl_openib_async_add_device (mca_btl_openib_device_t *device)
{
    if (mca_btl_openib_component.async_evbase) {
        if (1 == OPAL_THREAD_ADD32 (&btl_openib_async_device_count, 1)) {
            mca_btl_openib_async_init ();
        }
        opal_event_set (mca_btl_openib_component.async_evbase, &device->async_event,
                        device->ib_dev_context->async_fd, OPAL_EV_READ | OPAL_EV_PERSIST,
                        btl_openib_async_device, device);
        opal_event_add (&device->async_event, 0);
    }
}

void mca_btl_openib_async_rem_device (mca_btl_openib_device_t *device)
{
    if (mca_btl_openib_component.async_evbase) {
        opal_event_del (&device->async_event);
        if (0 == OPAL_THREAD_ADD32 (&btl_openib_async_device_count, -1)) {
            mca_btl_openib_async_fini ();
        }
    }
}

void mca_btl_openib_async_add_qp_ignore (struct ibv_qp *qp)
{
    if (mca_btl_openib_component.async_evbase) {
        mca_btl_openib_qp_list *new_qp = OBJ_NEW(mca_btl_openib_qp_list);
        if (OPAL_UNLIKELY(NULL == new_qp)) {
            /* can allocate a small object. not much more can be done */
            return;
        }

        BTL_VERBOSE(("Ignoring errors on QP %p", (void *) qp));
        new_qp->qp = qp;
        opal_mutex_lock (&ignore_qp_err_list_lock);
        opal_list_append (&ignore_qp_err_list, (opal_list_item_t *) new_qp);
        opal_mutex_unlock (&ignore_qp_err_list_lock);
    }
}
