/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_ADDR_H
#define MCA_PTL_GM_ADDR_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>


/**
 * Structure used to publish GM id information to peers.
 */
struct mca_ptl_gm_addr_t {
    unsigned int global_id;
    unsigned int local_id;
    unsigned int port_id;
};

typedef struct mca_ptl_gm_addr_t mca_ptl_gm_addr_t;

#endif
