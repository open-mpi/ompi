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
/** @file:
 *
 *  Contains header used by tcp oob.
 */

#ifndef _MCA_OOB_TCP_ADDR_H_
#define _MCA_OOB_TCP_ADDR_H_

#include "ompi_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "dps/dps.h"
#include "class/ompi_object.h"
#include "mca/ns/ns_types.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 * Address info published to registry
 */
struct mca_oob_tcp_addr_t {
    ompi_object_t super;
    orte_process_name_t addr_name;
    uint32_t addr_count;               
    uint32_t addr_next;
    uint32_t addr_alloc;
    struct sockaddr_in* addr_inet;    
};
typedef struct mca_oob_tcp_addr_t mca_oob_tcp_addr_t;

OBJ_CLASS_DECLARATION(mca_oob_tcp_addr_t);

/**
 * Unpack the contact information posted by the peer.
 */

mca_oob_tcp_addr_t* mca_oob_tcp_addr_unpack(orte_buffer_t*);

/**
 * Pack this hosts addressing info into a buffer for posting
 * into the registry.
 */

int mca_oob_tcp_addr_pack(orte_buffer_t*);

/**
 *
 */

int mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t*, const struct sockaddr_in*);

/**
 * 
 */

int mca_oob_tcp_addr_get_next(mca_oob_tcp_addr_t*, struct sockaddr_in*);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif 

