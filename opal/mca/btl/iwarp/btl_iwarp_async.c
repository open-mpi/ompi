/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2008-2009 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2007-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved
 * Copyright (c) 2013-2018 Los Alamos National Security, LLC. All rights
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
#include "btl_iwarp.h"
#include "btl_iwarp_mca.h"
#include "btl_iwarp_async.h"
#include "btl_iwarp_proc.h"
#include "btl_iwarp_endpoint.h"

static opal_list_t ignore_qp_err_list;
static opal_mutex_t ignore_qp_err_list_lock;
static int32_t btl_iwarp_async_device_count = 0;

struct mca_btl_iwarp_async_poll {
    int active_poll_size;
    int poll_size;
    struct pollfd *async_pollfd;
};
typedef struct mca_btl_iwarp_async_poll mca_btl_iwarp_async_poll;

typedef struct {
    opal_list_item_t super;
    struct ibv_qp *qp;
} mca_btl_iwarp_qp_list;

OBJ_CLASS_INSTANCE(mca_btl_iwarp_qp_list, opal_list_item_t, NULL, NULL);

static const char *iwarp_event_to_str (enum ibv_event_type event);

/* Function converts event to string (name)
 * Open Fabris don't have function that do this job :(
 */
static const char *iwarp_event_to_str (enum ibv_event_type event)
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

/* Function inits mca_btl_iwarp_async_poll */

/* The main idea of resizing SRQ algorithm -
   We create a SRQ with size = rd_num, but for efficient usage of resources
   the number of WQEs that we post = rd_curr_num < rd_num and this value is
   increased (by needs) in IBV_EVENT_SRQ_LIMIT_REACHED event handler (i.e. in this function),
   the event will thrown by device if number of WQEs in SRQ will be less than srq_limit */
static int btl_iwarp_async_srq_limit_event(struct ibv_srq* srq)
{
    int qp, rc = OPAL_SUCCESS;
    mca_btl_iwarp_module_t *iwarp_btl = NULL;

    opal_mutex_t *lock = &mca_btl_iwarp_component.srq_manager.lock;
    opal_hash_table_t *srq_addr_table = &mca_btl_iwarp_component.srq_manager.srq_addr_table;

    opal_mutex_lock(lock);

    if (OPAL_SUCCESS != opal_hash_table_get_value_ptr(srq_addr_table,
                            &srq, sizeof(struct ibv_srq*), (void*) &iwarp_btl)) {
        /* If there isn't any element with the key in the table =>
           we assume that SRQ was destroyed and don't serve the event */
        goto srq_limit_event_exit;
    }

    for(qp = 0; qp < mca_btl_iwarp_component.num_qps; qp++) {
        if (!BTL_IWARP_QP_TYPE_PP(qp)) {
            if(iwarp_btl->qps[qp].u.srq_qp.srq == srq) {
                break;
            }
        }
    }

    if(qp >= mca_btl_iwarp_component.num_qps) {
        BTL_ERROR(("Open MPI tried to access a shared receive queue (SRQ) on the device %s that was not found.  This should not happen, and is a fatal error.  Your MPI job will now abort.\n", ibv_get_device_name(iwarp_btl->device->ib_dev)));
        rc = OPAL_ERROR;
        goto srq_limit_event_exit;
    }

    /* dynamically re-size the SRQ to be larger */
    iwarp_btl->qps[qp].u.srq_qp.rd_curr_num <<= 1;

    if(iwarp_btl->qps[qp].u.srq_qp.rd_curr_num >=
                         mca_btl_iwarp_component.qp_infos[qp].rd_num) {
        iwarp_btl->qps[qp].u.srq_qp.rd_curr_num = mca_btl_iwarp_component.qp_infos[qp].rd_num;
        iwarp_btl->qps[qp].u.srq_qp.rd_low_local = mca_btl_iwarp_component.qp_infos[qp].rd_low;

        iwarp_btl->qps[qp].u.srq_qp.srq_limit_event_flag = false;

        goto srq_limit_event_exit;
    }

    iwarp_btl->qps[qp].u.srq_qp.rd_low_local <<= 1;
    iwarp_btl->qps[qp].u.srq_qp.srq_limit_event_flag = true;

srq_limit_event_exit:
    opal_mutex_unlock(lock);
    return rc;
}

/* Function handle async device events */
static void btl_iwarp_async_device (int fd, short flags, void *arg)
{
    mca_btl_iwarp_device_t *device = (mca_btl_iwarp_device_t *) arg;
    struct ibv_async_event event;
    int event_type;

    if (ibv_get_async_event((struct ibv_context *)device->ib_dev_context,&event) < 0) {
        if (EWOULDBLOCK != errno) {
            BTL_ERROR(("Failed to get async event"));
        }

        return;
    }

    event_type = event.event_type;
    switch(event_type) {
    case IBV_EVENT_DEVICE_FATAL:
        /* Set the flag to fatal */
        device->got_fatal_event = true;
        /* It is not critical to protect the counter */
        OPAL_THREAD_ADD_FETCH32(&mca_btl_iwarp_component.error_counter, 1);
        /* fall through */
    case IBV_EVENT_CQ_ERR:
    case IBV_EVENT_QP_FATAL:
        if (event_type == IBV_EVENT_QP_FATAL) {
            mca_btl_iwarp_qp_list *qp_item;
            bool in_ignore_list = false;

            BTL_VERBOSE(("QP is in err state %p", (void *)event.element.qp));

            /* look through ignore list */
            opal_mutex_lock (&ignore_qp_err_list_lock);
            OPAL_LIST_FOREACH(qp_item, &ignore_qp_err_list, mca_btl_iwarp_qp_list) {
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
        opal_show_help("help-mpi-btl-iwarp.txt", "of error event",
                       true,opal_process_info.nodename, (int)getpid(),
                       event_type,
                       iwarp_event_to_str((enum ibv_event_type)event_type));
        break;
    case IBV_EVENT_PORT_ERR:
        opal_show_help("help-mpi-btl-iwarp.txt", "of error event",
                       true,opal_process_info.nodename, (int)getpid(),
                       event_type,
                       iwarp_event_to_str((enum ibv_event_type)event_type));
        /* Set the flag to indicate port error */
        device->got_port_event = true;
        OPAL_THREAD_ADD_FETCH32(&mca_btl_iwarp_component.error_counter, 1);
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
        (void) btl_iwarp_async_srq_limit_event (event.element.srq);

        break;
    default:
        opal_show_help("help-mpi-btl-iwarp.txt", "of unknown event",
                       true,opal_process_info.nodename, (int)getpid(),
                       event_type);
    }

    ibv_ack_async_event(&event);
}

int mca_btl_iwarp_async_init (void)
{
    if (!mca_btl_iwarp_component.use_async_event_thread ||
        mca_btl_iwarp_component.async_evbase) {
        return OPAL_SUCCESS;
    }

    mca_btl_iwarp_component.async_evbase = opal_progress_thread_init (NULL);

    OBJ_CONSTRUCT(&ignore_qp_err_list, opal_list_t);
    OBJ_CONSTRUCT(&ignore_qp_err_list_lock, opal_mutex_t);

    /* Set the error counter to zero */
    mca_btl_iwarp_component.error_counter = 0;

    return OPAL_SUCCESS;
}

void mca_btl_iwarp_async_fini (void)
{
    if (mca_btl_iwarp_component.async_evbase) {
        OPAL_LIST_DESTRUCT(&ignore_qp_err_list);
        OBJ_DESTRUCT(&ignore_qp_err_list_lock);
        opal_progress_thread_finalize (NULL);
        mca_btl_iwarp_component.async_evbase = NULL;
    }
}

void mca_btl_iwarp_async_add_device (mca_btl_iwarp_device_t *device)
{
    if (mca_btl_iwarp_component.async_evbase) {
        if (1 == OPAL_THREAD_ADD_FETCH32 (&btl_iwarp_async_device_count, 1)) {
            mca_btl_iwarp_async_init ();
        }
        opal_event_set (mca_btl_iwarp_component.async_evbase, &device->async_event,
                        device->ib_dev_context->async_fd, OPAL_EV_READ | OPAL_EV_PERSIST,
                        btl_iwarp_async_device, device);
        opal_event_add (&device->async_event, 0);
    }
}

void mca_btl_iwarp_async_rem_device (mca_btl_iwarp_device_t *device)
{
    if (mca_btl_iwarp_component.async_evbase) {
        opal_event_del (&device->async_event);
        if (0 == OPAL_THREAD_ADD_FETCH32 (&btl_iwarp_async_device_count, -1)) {
            mca_btl_iwarp_async_fini ();
        }
    }
}

void mca_btl_iwarp_async_add_qp_ignore (struct ibv_qp *qp)
{
    if (mca_btl_iwarp_component.async_evbase) {
        mca_btl_iwarp_qp_list *new_qp = OBJ_NEW(mca_btl_iwarp_qp_list);
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
