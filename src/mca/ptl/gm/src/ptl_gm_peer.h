/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_PEER_H
#define MCA_PTL_GM_PEER_H

#include "class/ompi_list.h"
#include "include/types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Structure used to publish GM id information to peers.
 */
struct mca_ptl_gm_addr_t {
    unsigned int global_id;
    unsigned int local_id;
    unsigned int port_id;
};

typedef struct mca_ptl_gm_addr_t mca_ptl_gm_addr_t;

/**
 *  An abstraction that represents a connection to a peer process.
 */
struct mca_ptl_gm_peer_t {
    ompi_list_item_t super;
    struct mca_ptl_gm_module_t* peer_ptl;
    struct mca_ptl_gm_proc_t*   peer_proc;
    struct mca_ptl_gm_addr_t*   peer_addr;   /**< address of peer */
    unsigned int                global_id;
    unsigned int                port_number;
    unsigned int                local_id;
    int                         num_credits;
    int                         max_credits;
    int                         resending;
    int                         num_resend;
    bool                        get_started;
};
typedef struct mca_ptl_gm_peer_t mca_ptl_gm_peer_t;

OBJ_CLASS_DECLARATION(mca_ptl_gm_peer_t);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
