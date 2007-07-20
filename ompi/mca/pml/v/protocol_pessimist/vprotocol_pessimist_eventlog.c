#include "ompi_config.h"
#include "vprotocol_pessimist.h"


#if 0
static inline void replay_delivery_order(
  struct mca_ptl_base_module_t* ptl,
  mca_ptl_base_recv_request_t* req,
  size_t bytes_received,
  size_t bytes_delivered)
{
  vprotocol_pessimist_delivery_pending_t *delivery;
  vprotocol_pessimist_event_t *event;

  /* store it as finished but not delivered message in the list */
  delivery = OBJ_NEW(vprotocol_pessimist_delivery_pending_t);
  delivery->ptl = ptl;
  delivery->req = req;
  delivery->bytes_received = bytes_received;
  delivery->bytes_delivered = bytes_delivered;
  opal_list_append(&mca_vprotocol_pessimist.replay_delivery_pendings, (opal_list_item_t *) delivery);

  /* deliver as many message as possible in the delivery pending list */
scanpendings: 
  if(opal_list_is_empty(&mca_vprotocol_pessimist.replay_events))
  {
    /* there is no more events to be replayed, leaving replay mode */
    OPAL_OUTPUT((mca_pml_v_output, "Leaving replay mode"));
    mca_vprotocol_pessimist.replay = false;
    /* deliver (and log) any message still waiting in delivery_pendings */
    while(! opal_list_is_empty(&mca_vprotocol_pessimist.replay_delivery_pendings))
    {
      delivery = (vprotocol_pessimist_delivery_pending_t *) opal_list_remove_first(&mca_vprotocol_pessimist.replay_delivery_pendings);
      log_event(delivery->req);
      OPAL_OUTPUT((mca_pml_v_output, "deliver\trecv %d:%lx\tfrom %d\ttag %d\tsize %d\tmatched src %d", 
                                    delivery->req->req_recv.req_base.req_comm->c_contextid, (long) delivery->req->req_recv.req_base.req_sequence,
                                    delivery->req->req_recv.req_base.req_peer, delivery->req->req_recv.req_base.req_tag, delivery->req->req_recv.req_base.req_count, 
                                    delivery->req->req_recv.req_base.req_ompi.req_status.MPI_SOURCE));
      mca_pml_v.host_pml_recv_progress(delivery->ptl, delivery->req, delivery->bytes_received, delivery->bytes_delivered);
      OBJ_RELEASE(delivery);
    } 
  }
  else 
  {
    /* search the event to replay in the delivery pending list */
    event = (vprotocol_pessimist_event_t *) opal_list_get_first(&mca_vprotocol_pessimist.replay_events);
    
    for(delivery = (vprotocol_pessimist_delivery_pending_t *) opal_list_get_first(&mca_vprotocol_pessimist.replay_delivery_pendings);
        delivery != (vprotocol_pessimist_delivery_pending_t *) opal_list_get_end(&mca_vprotocol_pessimist.replay_delivery_pendings);
        delivery = (vprotocol_pessimist_delivery_pending_t *) opal_list_get_next(delivery))
    {
      if((delivery->req->req_recv.req_base.req_comm->c_contextid == event->contextid) && 
          (delivery->req->req_recv.req_base.req_sequence == event->rclock))
      {
        /* this is the request matching the first event to be replayed, let's go */
        OPAL_OUTPUT((mca_pml_v_output, "deliver\trecv %d:%lx\tfrom %d\ttag %d\tsize %d\tmatched src %d", 
                                      delivery->req->req_recv.req_base.req_comm->c_contextid, (long) delivery->req->req_recv.req_base.req_sequence,
                                      delivery->req->req_recv.req_base.req_peer, delivery->req->req_recv.req_base.req_tag, delivery->req->req_recv.req_base.req_count, 
                                      delivery->req->req_recv.req_base.req_ompi.req_status.MPI_SOURCE));
        mca_pml_v.host_pml_recv_progress(delivery->ptl, delivery->req, delivery->bytes_received, delivery->bytes_delivered);
          
        opal_list_remove_first(&mca_vprotocol_pessimist.replay_events);
        OBJ_RELEASE(event);

        opal_list_remove_item(&mca_vprotocol_pessimist.replay_delivery_pendings, (opal_list_item_t *) delivery);
        OBJ_RELEASE(delivery);
  
        /* the first event has been delivered, let's see if the second can be delivered now */
        goto scanpendings;
      }
    }
    /* the event to be replayed is not in the delivery_pendings list, we've delivered as much as we can for now */
  }
}

#endif /* 0 */
