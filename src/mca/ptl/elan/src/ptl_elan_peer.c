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
    ptl_peer->peer_vp = -1;
    ptl_peer->peer_rails = 0;
    ptl_peer->num_credits = 0;  /* Number of credits for the local PTL */
    ptl_peer->max_credits = 0;  /* Number of credits for the local PTL */
    ptl_peer->resending = 0;    /* A resending stage, no more new dma's */
    ptl_peer->num_resends = 0;   /* How many times I have retried */
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
