/*
 * $HEADER$
 */
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "include/types.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_elan.h"
#include "ptl_elan_peer.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"
static void
mca_ptl_elan_peer_construct (mca_ptl_elan_peer_t * ptl_peer)
{
    ptl_peer->peer_ptl = NULL;
    ptl_peer->peer_proc = NULL;
    ptl_peer->peer_addr = NULL;
    ptl_peer->peer_state = MCA_PTL_ELAN_CLOSED;
    ptl_peer->num_credits = 0;  /* Number of credits for the local PTL */
    ptl_peer->max_credits = 0;  /* Number of credits for the local PTL */
    ptl_peer->resending = 0;    /* A resending stage, no more new dma's */
    ptl_peer->num_resend = 0;   /* How many times I have retried */
    ptl_peer->known_alive_time = 0;
}

/* Cleanup any resources held by the peer. */
static void
mca_ptl_elan_peer_destruct (mca_ptl_elan_peer_t * ptl_peer)
{
    mca_ptl_elan_proc_remove (ptl_peer->peer_proc, ptl_peer);
    /*mca_ptl_elan_peer_close(ptl_peer); */
}

ompi_class_t mca_ptl_elan_peer_t_class = {
    "mca_elan_ptl_peer_t",
    OBJ_CLASS (ompi_list_item_t),
    (ompi_construct_t) mca_ptl_elan_peer_construct,
    (ompi_destruct_t) mca_ptl_elan_peer_destruct
};
