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
#include "ptl_mx.h"
#include "ptl_mx_peer.h"

static void mca_ptl_mx_peer_construct(mca_ptl_base_peer_t* ptl_peer);
static void mca_ptl_mx_peer_destruct(mca_ptl_base_peer_t* ptl_peer);

OBJ_CLASS_INSTANCE(
    mca_ptl_mx_peer_t,
    ompi_list_item_t,
    mca_ptl_mx_peer_construct,
    mca_ptl_mx_peer_destruct);


/*
 * Initialize state of the peer instance.
 */

static void mca_ptl_mx_peer_construct(mca_ptl_base_peer_t* ptl_peer)
{
    ptl_peer->peer_ptl = NULL;
    ptl_peer->peer_nbo = false;
}

/*
 * Cleanup any resources held by the peer.
 */

static void mca_ptl_mx_peer_destruct(mca_ptl_base_peer_t* ptl_peer)
{
}

