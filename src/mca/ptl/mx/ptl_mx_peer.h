/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_MX_PEER_H
#define MCA_PTL_MX_PEER_H

#include "ompi_config.h"


/**
 *  An abstraction that represents a a peer process.
*/
struct mca_ptl_base_peer_t {
    ompi_list_item_t peer_item;
};
typedef struct mca_ptl_base_peer_t mca_ptl_base_peer_t;
typedef struct mca_ptl_base_peer_t mca_ptl_mx_peer_t;

OBJ_CLASS_DECLARATION(mca_ptl_mx_peer_t);


#endif

