/*
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
