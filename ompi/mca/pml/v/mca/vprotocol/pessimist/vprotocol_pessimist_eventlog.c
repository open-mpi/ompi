/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "vprotocol_pessimist_eventlog.h"


void vprotocol_pessimist_matching_replay(int *src) {
#if OMPI_ENABLE_DEBUG
    vprotocol_pessimist_clock_t max = 0;
#endif
    mca_vprotocol_pessimist_event_t *event;

    /* searching this request in the event list */
    for(event = (mca_vprotocol_pessimist_event_t *) opal_list_get_first(&mca_vprotocol_pessimist.replay_events);
        event != (mca_vprotocol_pessimist_event_t *) opal_list_get_end(&mca_vprotocol_pessimist.replay_events);
        event = (mca_vprotocol_pessimist_event_t *) opal_list_get_next(event))
    {
        vprotocol_pessimist_matching_event_t *mevent;
        
        if(VPROTOCOL_PESSIMIST_EVENT_TYPE_MATCHING != event->type) continue;        
        mevent = &(event->u_event.e_matching);
        if(mevent->reqid == mca_vprotocol_pessimist.clock)
        {
            /* this is the event to replay */
            V_OUTPUT_VERBOSE(70, "pessimist: replay\tmatch\t%"PRIpclock"\trecv is forced from %d", mevent->reqid, mevent->src);
            (*src) = mevent->src;
            opal_list_remove_item(&mca_vprotocol_pessimist.replay_events, 
                                  (opal_list_item_t *) event);
            VPESSIMIST_EVENT_RETURN(event);
        }   
#if OMPI_ENABLE_DEBUG
        else if(mevent->reqid > max) 
            max = mevent->reqid;                         
    }
    /* not forcing a ANY SOURCE event whose recieve clock is lower than max
     * is a bug indicating we have missed an event during logging ! */
    assert(((*src) != MPI_ANY_SOURCE) || (mca_vprotocol_pessimist.clock > max));
#else
    }
#endif
}

void vprotocol_pessimist_delivery_replay(size_t n, ompi_request_t **reqs,
                                         int *outcount, int *index, 
                                         ompi_status_public_t *status) {
    mca_vprotocol_pessimist_event_t *event;

    for(event = (mca_vprotocol_pessimist_event_t *) opal_list_get_first(&mca_vprotocol_pessimist.replay_events);
        event != (mca_vprotocol_pessimist_event_t *) opal_list_get_end(&mca_vprotocol_pessimist.replay_events);
        event = (mca_vprotocol_pessimist_event_t *) opal_list_get_next(event))
    {
        vprotocol_pessimist_delivery_event_t *devent; 

        if(VPROTOCOL_PESSIMIST_EVENT_TYPE_DELIVERY != event->type) continue;
        devent = &(event->u_event.e_delivery);
        if(devent->probeid < mca_vprotocol_pessimist.clock)
        {
            /* this particular test have to return no request completed yet */
            V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%"PRIpclock"\tnone", mca_vprotocol_pessimist.clock);
            *index = MPI_UNDEFINED;
            *outcount = 0;
            mca_vprotocol_pessimist.clock++;
            /* This request have to stay in the queue until probeid matches */
            return;
        }
        else if(devent->probeid == mca_vprotocol_pessimist.clock)
        {
            int i;
            for(i = 0; i < (int) n; i++)
            {
                if(VPESSIMIST_FTREQ(reqs[i])->reqid == devent->reqid)
                {
                    V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%"PRIpclock"\t%"PRIpclock, devent->probeid, devent->reqid);
                    opal_list_remove_item(&mca_vprotocol_pessimist.replay_events,
                                          (opal_list_item_t *) event);
                    VPESSIMIST_EVENT_RETURN(event);
                    *index = i;
                    *outcount = 1;
                    mca_vprotocol_pessimist.clock++;
                    ompi_request_wait(&reqs[i], status);
                    return;
                }
            }
            V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%"PRIpclock"\tnone", mca_vprotocol_pessimist.clock);
            assert(devent->reqid == 0); /* make sure we don't missed a request */
            *index = MPI_UNDEFINED;
            *outcount = 0;
            mca_vprotocol_pessimist.clock++;
            opal_list_remove_item(&mca_vprotocol_pessimist.replay_events,
                                  (opal_list_item_t *) event);
            VPESSIMIST_EVENT_RETURN(event);
            return;
        }
    }
    V_OUTPUT_VERBOSE(50, "pessimist:\treplay\tdeliver\t%"PRIpclock"\tnot forced", mca_vprotocol_pessimist.clock);
}
