/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "opal/util/output.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "orte/mca/iof/base/iof_base_fragment.h"

/**
 * timer callback out of the event loop
 */

static void orte_iof_base_timer_cb(int fd, short flags, void *cbdata)
{
    int *flushed = (int*)cbdata;
    OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    *flushed = 1;
    opal_condition_signal(&orte_iof_base.iof_condition);
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
}


/*
 * flush output streams and block until there is no pending I/O
 * on any of the current endpoints.
 */

int orte_iof_base_flush(void)
{
    opal_list_item_t* item;
    opal_event_t ev;
    struct timeval tv = { 0, 0 };
    int flushed = 0;
    size_t pending;
    static int32_t lock = 0;

    if(OPAL_THREAD_ADD32(&lock,1) > 1) {
        OPAL_THREAD_ADD32(&lock,-1);
        return ORTE_SUCCESS;
    }

    /* flush any pending output */
    fflush(NULL);

    /* force all file descriptors to be progressed at least once,
     * wait on a timer callback to be called out of the event loop
    */

    if(opal_event_progress_thread() == false) {
        OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
        opal_evtimer_set(&ev, orte_iof_base_timer_cb, &flushed);
        opal_event_add(&ev, &tv);
        while(0 == flushed) {
            opal_condition_wait(&orte_iof_base.iof_condition, &orte_iof_base.iof_lock);
        }
    } else {
        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
        OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
    }
    orte_iof_base.iof_waiting++;

    /* wait for all of the endpoints to reach an idle state */
    pending = opal_list_get_size(&orte_iof_base.iof_endpoints);
    while(pending > 0) {
        pending = 0;
        for(item = opal_list_get_first(&orte_iof_base.iof_endpoints);
            item != opal_list_get_end(&orte_iof_base.iof_endpoints);
            item =  opal_list_get_next(item)) {
            orte_iof_base_endpoint_t* endpoint = (orte_iof_base_endpoint_t*)item;
            if(orte_iof_base_endpoint_pending(endpoint)) {
                pending++;
            }
        }
        if(pending != 0) {
            if(opal_event_progress_thread() == false) {
                opal_condition_wait(&orte_iof_base.iof_condition, &orte_iof_base.iof_lock);
            } else {
                OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
                opal_event_loop(OPAL_EVLOOP_ONCE);
                OPAL_THREAD_LOCK(&orte_iof_base.iof_lock);
            }
        }
    }
    orte_iof_base.iof_waiting--;
    OPAL_THREAD_UNLOCK(&orte_iof_base.iof_lock);
    OPAL_THREAD_ADD32(&lock,-1);
    return ORTE_SUCCESS;
}

