#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ns/base/base.h"
#include "ptl_ib.h"
#include "ptl_ib_addr.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_sendfrag.h"

static void mca_ptl_ib_peer_construct(mca_ptl_base_peer_t* module_peer);
static void mca_ptl_ib_peer_destruct(mca_ptl_base_peer_t* module_peer);

OBJ_CLASS_INSTANCE(mca_ptl_ib_peer_t, 
        ompi_list_item_t, mca_ptl_ib_peer_construct, 
        mca_ptl_ib_peer_destruct);

/*
 * Initialize state of the peer instance.
 */

static void mca_ptl_ib_peer_construct(mca_ptl_base_peer_t* module_peer)
{
    module_peer->peer_module = 0;
    module_peer->peer_proc = 0;
    module_peer->peer_addr = 0;
    module_peer->peer_send_frag = 0;
    module_peer->peer_recv_frag = 0;
    module_peer->peer_send_event.ev_flags = 0;
    module_peer->peer_recv_event.ev_flags = 0;
    module_peer->peer_state = MCA_PTL_IB_NOT_OPENED;
    module_peer->peer_retries = 0;
    OBJ_CONSTRUCT(&module_peer->peer_frags, ompi_list_t);
    OBJ_CONSTRUCT(&module_peer->peer_send_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&module_peer->peer_recv_lock, ompi_mutex_t);
}

static void mca_ptl_ib_peer_destruct(mca_ptl_base_peer_t* module_peer)
{
}
