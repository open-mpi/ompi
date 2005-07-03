/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
/**
 * @file
 */
#ifndef MCA_PTL_ELAN_PEER_H
#define MCA_PTL_ELAN_PEER_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "opal/class/opal_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "ptl_elan_frag.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 *  An abstraction that represents a connection to a peer process.
 */
struct mca_ptl_elan_peer_t {
    opal_list_item_t            super;

    struct mca_ptl_elan_module_t *peer_ptl;    
    struct mca_ptl_elan_proc_t *peer_proc; 

    int     peer_vp;
    int     peer_rails;
    int     num_credits;  /* Got to be an arry for rails */
    int     max_credits;  /* Got to be an arry for rails */
    int     resending;  
    int     num_resends;
};
typedef struct mca_ptl_elan_peer_t mca_ptl_elan_peer_t;

OBJ_CLASS_DECLARATION(mca_ptl_elan_peer_t); 

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

