/*
 * $HEADER$
 */
/** @file:
 *
 *  Contains header used by tcp oob.
 */

#ifndef _MCA_OOB_TCP_ADDR_H_
#define _MCA_OOB_TCP_ADDR_H_

#include "ompi_config.h"
#include <sys/types.h>
#include <netinet/in.h>
#include "class/ompi_object.h"
#include "util/pack.h"
#include "mca/ns/ns.h"

/**
 * Address info published to registry
 */
struct mca_oob_tcp_addr_t {
    ompi_object_t super;
    ompi_process_name_t addr_name;
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

mca_oob_tcp_addr_t* mca_oob_tcp_addr_unpack(ompi_buffer_t);

/**
 * Pack this hosts addressing info into a buffer for posting
 * into the registry.
 */

void mca_oob_tcp_addr_pack(ompi_buffer_t);

/**
 *
 */

int mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t*, const struct sockaddr_in*);

/**
 * 
 */

int mca_oob_tcp_addr_get_next(mca_oob_tcp_addr_t*, struct sockaddr_in*);

#endif 

