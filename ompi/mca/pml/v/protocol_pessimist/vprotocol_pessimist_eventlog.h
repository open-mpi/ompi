/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef __VPROTOCOL_PESSIMIST_EVENTLOG_H__
#define __VPROTOCOL_PESSIMIST_EVENTLOG_H__

#include "ompi_config.h"
#include "vprotocol_pessimist.h"
#include "vprotocol_pessimist_request.h"

/*******************************************************************************
  * ANY_SOURCE MATCHING
  */

/** Adds a matching event for this request in the event list for any ANY_SOURCE
  * recv. This event have to be updated later by 
  * VPROTOCOL_PESSIMIST_MATCHING_LOG_FINALIZE
  * req (IN/OUT): posted RECV request (mca_pml_base_request_t *)
  *   VPESSIMIST_REQ(req) is updated to keep track of the associated event
  */
#define VPROTOCOL_PESSIMIST_MATCHING_LOG_PREPARE(REQ) do {                    \
  if(((mca_pml_base_request_t *) REQ)->req_peer == MPI_ANY_SOURCE)            \
  {                                                                           \
    mca_vprotocol_pessimist_event_t *event;                                   \
                                                                              \
    VPESSIMIST_MATCHING_EVENT_NEW(event);                                     \
    event->req = (mca_pml_base_request_t *) REQ;                              \
    VPESSIMIST_RECV_REQ(REQ)->event = event;                                  \
    opal_list_append(&mca_vprotocol_pessimist.pending_events,                 \
                     (opal_list_item_t *) event);                             \
  }                                                                           \
} while(0)

/** Updates the actual value of a matching event
  * req(IN/OUT): the matched recv request
  *   VPESSIMIST_REQ(req) is updated to remove link to event
  */
#define VPROTOCOL_PESSIMIST_MATCHING_LOG_FINALIZE(REQ) do {                   \
  if(VPESSIMIST_REQ(REQ)->event)                                              \
  {                                                                           \
    mca_vprotocol_pessimist_event_t *event;                                   \
    vprotocol_pessimist_matching_event_t *mevent;                             \
                                                                              \
    V_OUTPUT_VERBOSE(70, "pessimist:\tlog\tmatch\t%x\tsrc %d\tseq %llu", VPESSIMIST_REQ(REQ)->reqid, (REQ)->req_ompi.req_status.MPI_SOURCE, (long long) (REQ)->req_sequence); \
    event = VPESSIMIST_RECV_REQ(REQ)->event;                                  \
    mevent =  &(event->u_event.e_matching);                                   \
    mevent->reqid = VPESSIMIST_RECV_REQ(REQ)->reqid;                          \
    mevent->src = (REQ)->req_ompi.req_status.MPI_SOURCE;                      \
    VPESSIMIST_RECV_REQ(REQ)->event = NULL;                                   \
    event->req = NULL;                                                        \
  }                                                                           \
} while(0)

#define VPROTOCOL_PESSIMIST_SEND_BUFFER() do {                                \
  if(mca_vprotocol_pessimist.event_buffer_length)                             \
  {                                                                           \
/*    write(2, mca_vprotocol_pessimist.event_buffer,                          \
             mca_vprotocol_pessimist.event_buffer_length *                    \
                sizeof(vprotocol_pessimist_mem_event_t));                    */ \
    mca_vprotocol_pessimist.event_buffer_length = 0;                          \
  }                                                                           \
} while(0)

#define VPROTOCOL_PESSIMIST_EVENT_FLUSH() do {                                \
  if(!opal_list_is_empty(&mca_vprotocol_pessimist.pending_events))            \
  {                                                                           \
    mca_vprotocol_pessimist_event_t *event;                                   \
    mca_vprotocol_pessimist_event_t *prv_event;                               \
                                                                              \
    for( event =                                                              \
            (mca_vprotocol_pessimist_event_t *)                               \
            opal_list_get_first(&mca_vprotocol_pessimist.pending_events);     \
         event !=                                                             \
            (mca_vprotocol_pessimist_event_t *)                               \
            opal_list_get_end(&mca_vprotocol_pessimist.pending_events);       \
         event =                                                              \
            (mca_vprotocol_pessimist_event_t *)                               \
            opal_list_get_next(event))                                        \
    {                                                                         \
      if(event->u_event.e_matching.src == -1)                                 \
      {                                                                       \
        /* check if that request have been matched now and update the event */\
        /* this assert make sure the negative source trick is fine          */\
        assert(event->type == VPROTOCOL_PESSIMIST_EVENT_TYPE_MATCHING);       \
        if(event->req->req_ompi.req_status.MPI_SOURCE == -1)                  \
        {                                                                     \
          V_OUTPUT_VERBOSE(101, "pessimist:\tlog\tel\t%x\tfrom %d\tnot matched yet", event->u_event.e_matching.reqid, event->u_event.e_matching.src); \
          continue;                                                           \
        }                                                                     \
        event->u_event.e_matching.src =                                       \
            event->req->req_ompi.req_status.MPI_SOURCE;                       \
      }                                                                       \
      /* Send this event to EL */                                             \
      V_OUTPUT_VERBOSE(100, "pessimist:\tlog\tel\t%x\tfrom %d\tsent to EL", event->u_event.e_matching.reqid, event->u_event.e_matching.src); \
      mca_vprotocol_pessimist.event_buffer[mca_vprotocol_pessimist.event_buffer_length++] = \
            event->u_event;                                                   \
      if(mca_vprotocol_pessimist.event_buffer_length ==                       \
         mca_vprotocol_pessimist.event_buffer_max_length)                     \
        VPROTOCOL_PESSIMIST_SEND_BUFFER();                                    \
      prv_event = (mca_vprotocol_pessimist_event_t *) opal_list_remove_item(  \
                      &mca_vprotocol_pessimist.pending_events,                \
                      (opal_list_item_t *) event);                            \
      VPESSIMIST_EVENT_RETURN(event);                                         \
      event = prv_event;                                                      \
    }                                                                         \
  }                                                                           \
  VPROTOCOL_PESSIMIST_SEND_BUFFER();                                          \
} while(0)


/** Replay matching order according to event list during recovery
 * src (IN/OUT): the requested source. If it is ANY_SOURCE it is changed to 
 *               the matched source at first run. 
 * comm (IN): the communicator's context id is used to know the next unique 
 *            request id that will be allocated by PML
 */
#define VPROTOCOL_PESSIMIST_MATCHING_REPLAY(src) do {                         \
  if(mca_vprotocol_pessimist.replay && ((src) == MPI_ANY_SOURCE))             \
    vprotocol_pessimist_matching_replay(&(src));                              \
} while(0)  

static inline void vprotocol_pessimist_matching_replay(int *src)
{                                                       
#if OMPI_ENABLE_DEBUG
  vprotocol_pessimist_clock_t max = 0;
#endif
  mca_vprotocol_pessimist_event_t *event;                                        
                                                                               
  /* searching this request in the event list */                             
  for(event = (mca_vprotocol_pessimist_event_t *) opal_list_get_first(&mca_vprotocol_pessimist.replay_events); 
      event != (mca_vprotocol_pessimist_event_t *) opal_list_get_end(&mca_vprotocol_pessimist.replay_events); 
      event = (mca_vprotocol_pessimist_event_t *) opal_list_get_next(event))     
  {                                                                          
    vprotocol_pessimist_matching_event_t *mevent = &(event->u_event.e_matching);
    if(mevent->reqid == mca_vprotocol_pessimist.clock)
    {                                                                        
      /* this is the event to replay */                                      
      V_OUTPUT_VERBOSE(70, "pessimist: replay\tmatch\t%x\trecv is forced from %d", mevent->reqid, mevent->src); 
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
  assert(((*src) != MPI_ANY_SOURCE) ||                                        
         (mca_vprotocol_pessimist.clock > max));       
#else
  }                                                                          
#endif
}

/*******************************************************************************
  * WAIT/TEST-SOME/ANY & PROBES 
  */

/** Store the delivered request after a non deterministic delivery
 * req (IN): the delivered request (pml_base_request_t *)
 */ 
#define VPROTOCOL_PESSIMIST_DELIVERY_LOG(req) do {                            \
  mca_vprotocol_pessimist_event_t *event;                                     \
  vprotocol_pessimist_delivery_event_t *devent;                               \
                                                                              \
  if(req == NULL)                                                             \
  {                                                                           \
    V_OUTPUT_VERBOSE(70, "pessimist:\tlog\tdeliver\t%x\tnone", mca_vprotocol_pessimist.clock); \
    event = (mca_vprotocol_pessimist_event_t*)opal_list_get_last(&mca_vprotocol_pessimist.pending_events);      \
    if(event->type == VPROTOCOL_PESSIMIST_EVENT_TYPE_DELIVERY &&              \
       event->u_event.e_delivery.reqid == 0)                                  \
    {                                                                         \
      /* consecutive probes not delivering anything are merged */             \
      event->u_event.e_delivery.probeid = mca_vprotocol_pessimist.clock++;    \
    }                                                                         \
    else                                                                      \
    {                                                                         \
      VPESSIMIST_DELIVERY_EVENT_NEW(event);                                   \
      devent = &(event->u_event.e_delivery);                                  \
      devent->probeid = mca_vprotocol_pessimist.clock++;                      \
      devent->reqid = 0;                                                      \
      opal_list_append(&mca_vprotocol_pessimist.pending_events,               \
                       (opal_list_item_t *) event);                           \
    }                                                                         \
  }                                                                           \
  else                                                                        \
  {                                                                           \
    V_OUTPUT_VERBOSE(70, "pessimist:\tlog\tdeliver\t%x\treq %x", mca_vprotocol_pessimist.clock, VPESSIMIST_REQ(req)->reqid); \
    VPESSIMIST_DELIVERY_EVENT_NEW(event);                                     \
    devent = &(event->u_event.e_delivery);                                    \
    devent->probeid = mca_vprotocol_pessimist.clock++;                        \
    devent->reqid = VPESSIMIST_REQ(req)->reqid;                               \
    opal_list_append(&mca_vprotocol_pessimist.pending_events,                 \
                     (opal_list_item_t *) event);                             \
  }                                                                           \
} while(0)

/** Enforces a particular request to be delivered considering the current
  * event clock
  * n (IN): the number of input requests
  * reqs (IN): the set of considered requests (pml_base_request_t *)
  * i (IN/OUT): index of the delivered request
  * c (IN/OUT): counter for number of delivered requests (currently only 0 or 1)
  * status (IN/OUT): status of the delivered request
  */
#define VPROTOCOL_PESSIMIST_DELIVERY_REPLAY(n, reqs, i, c, status) do {       \
  if(mca_vprotocol_pessimist.replay)                                          \
  {                                                                           \
    mca_vprotocol_pessimist_event_t *event;                                   \
                                                                              \
    for(event = (mca_vprotocol_pessimist_event_t *) opal_list_get_first(&mca_vprotocol_pessimist.replay_events); \
        event != (mca_vprotocol_pessimist_event_t *) opal_list_get_end(&mca_vprotocol_pessimist.replay_events); \
        event = (mca_vprotocol_pessimist_event_t *) opal_list_get_next(event)) \
    {                                                                         \
      vprotocol_pessimist_delivery_event_t *devent = &(event->u_event.e_delivery); \
                                                                              \
      if(event->type == VPROTOCOL_PESSIMIST_EVENT_TYPE_MATCHING) continue;    \
      if(devent->probeid < mca_vprotocol_pessimist.clock)                     \
      {                                                                       \
        /* this particular test have to return no request completed yet */    \
        V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%x\tnone", mca_vprotocol_pessimist.clock); \
        (i) = MPI_UNDEFINED;                                                  \
        (c) = 0;                                                              \
        mca_vprotocol_pessimist.clock++;                                      \
        return OMPI_SUCCESS;                                                  \
      }                                                                       \
      else if(devent->probeid == mca_vprotocol_pessimist.clock)               \
      {                                                                       \
        V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%x\t%x", devent->probeid, devent->reqid); \
        for((i) = 0; (i) < (n); (i)++)                                        \
        {                                                                     \
          if(VPESSIMIST_REQ(reqs[i])->reqid == devent->reqid)                 \
          {                                                                   \
            opal_list_remove_item(&mca_vprotocol_pessimist.replay_events,     \
                                  (opal_list_item_t *) event);                \
            VPESSIMIST_EVENT_RETURN(event);                                   \
            (c) = 1;                                                          \
            mca_vprotocol_pessimist.clock++;                                  \
            return ompi_request_wait(&reqs[i], status);                       \
          }                                                                   \
        }                                                                     \
        V_OUTPUT_VERBOSE(70, "pessimist:\treplay\tdeliver\t%x\tnone", mca_vprotocol_pessimist.clock); \
        assert(devent->reqid == 0);                                           \
        (i) = MPI_UNDEFINED;                                                  \
        (c) = 0;                                                              \
        mca_vprotocol_pessimist.clock++;                                      \
        opal_list_remove_item(&mca_vprotocol_pessimist.replay_events,         \
                              (opal_list_item_t *) event);                    \
        VPESSIMIST_EVENT_RETURN(event);                                       \
        return OMPI_SUCCESS;                                                  \
      }                                                                       \
    }                                                                         \
    V_OUTPUT_VERBOSE(50, "pessimist:\treplay\tdeliver\t%x\tnot forced", mca_vprotocol_pessimist.clock); \
  }                                                                           \
} while(0)

#endif /* __VPROTOCOL_PESSIMIST_EVENTLOG_H__ */
