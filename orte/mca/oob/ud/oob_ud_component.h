/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#if !defined(MCA_OOB_UD_COMPONENT_H)
#define MCA_OOB_UD_COMPONENT_H

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/class/opal_bitmap.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/routed/routed.h"
#include "oob_ud.h"
#include "oob_ud_send.h"
#include "oob_ud_ping.h"

/**
 *  OOB UD Component
 */

 /**
 *  OOB USOCK Component
 */
typedef struct {
    mca_oob_base_component_t super;          /**< base OOB component */

    opal_list_t       ud_devices;
    opal_list_t       ud_active_recvs;
    opal_list_t       ud_active_sends;
    opal_list_t       ud_event_queued_reqs;
    opal_list_t       ud_event_processing_msgs;

    opal_event_t      ud_complete_event;

    opal_mutex_t      ud_lock;

    int               ud_min_qp;
    int               ud_max_qp;

    int               ud_recv_buffer_count;
    int               ud_send_buffer_count;

    opal_mutex_t      ud_match_lock;

    int               ud_max_retries;       /**< max number of retries before declaring peer gone */
    int               ud_timeout_usec;      /**< timeout in microsecond between peer retries */

} mca_oob_ud_component_t;

ORTE_MODULE_DECLSPEC extern mca_oob_ud_component_t mca_oob_ud_component;

#endif //MCA_OOB_UD_COMPONENT_H
