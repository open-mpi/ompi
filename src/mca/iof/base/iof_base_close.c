/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "event/event.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"


static void mca_iof_base_timer_cb(int fd, short flags, void *cbdata)
{
    int *flushed = (int*)cbdata;
    OMPI_THREAD_LOCK(&mca_iof_base.iof_lock);
    *flushed = 1;
    ompi_condition_signal(&mca_iof_base.iof_condition);
    OMPI_THREAD_UNLOCK(&mca_iof_base.iof_lock);
}


int mca_iof_base_close(void)
{
    ompi_list_item_t* item;
    ompi_event_t ev;
    struct timeval tv = { 0, 0 };
    int flushed = 0;
    int closed = 0;

    /* flush any pending output */
    fflush(NULL);

    /* wait until event loop has been progressed at least once */
    OMPI_THREAD_LOCK(&mca_iof_base.iof_lock);
    ompi_evtimer_set(&ev, mca_iof_base_timer_cb, &flushed);
    ompi_event_add(&ev, &tv);
    while(flushed == 0)
        ompi_condition_wait(&mca_iof_base.iof_condition, &mca_iof_base.iof_lock);

    /* attempt to close all of the endpoints */
    item = ompi_list_get_first(&mca_iof_base.iof_endpoints);
    while(item != ompi_list_get_end(&mca_iof_base.iof_endpoints)) {
        ompi_list_item_t* next = ompi_list_get_next(item);
        mca_iof_base_endpoint_t* endpoint = (mca_iof_base_endpoint_t*)item;
        mca_iof_base_endpoint_close(endpoint);
        item = next;
    }

    /* wait for all to flush output and change to closed state */
    while(closed != ompi_list_get_size(&mca_iof_base.iof_endpoints)) {
        closed = 0;
        for(item = ompi_list_get_first(&mca_iof_base.iof_endpoints);
            item != ompi_list_get_end(&mca_iof_base.iof_endpoints);
            item =  ompi_list_get_next(item)) {
            mca_iof_base_endpoint_t* endpoint = (mca_iof_base_endpoint_t*)item;
            if(endpoint->ep_state == MCA_IOF_EP_CLOSED) {
                closed++;
            }
        }
        if(closed != ompi_list_get_size(&mca_iof_base.iof_endpoints)) {
            ompi_condition_wait(&mca_iof_base.iof_condition, &mca_iof_base.iof_lock);
        }
    }
    OMPI_THREAD_UNLOCK(&mca_iof_base.iof_lock);

    /* shutdown any remaining opened components */
    if (0 != ompi_list_get_size(&mca_iof_base.iof_components_opened)) {
        mca_base_components_close(mca_iof_base.iof_output, 
                              &mca_iof_base.iof_components_opened, NULL);
    }

    /* final cleanup of resources */
    OMPI_THREAD_LOCK(&mca_iof_base.iof_lock);
    while((item = ompi_list_remove_first(&mca_iof_base.iof_endpoints)) != NULL) {
        OBJ_RELEASE(item);
    }
    OMPI_THREAD_UNLOCK(&mca_iof_base.iof_lock);
    return OMPI_SUCCESS;
}

