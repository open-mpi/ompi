/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_PEER_H
#define MCA_PTL_GM_PEER_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"



/**
 *  An abstraction that represents a connection to a peer process.
 */
struct mca_ptl_gm_peer_t {
    ompi_list_item_t super;

    struct mca_ptl_gm_t *peer_ptl;
    struct mca_ptl_gm_proc_t *peer_proc;
    struct mca_ptl_gm_addr_t *peer_addr;   /**< address of peer */

    int         num_credits;
    int         max_credits;
    int         resending;
    int         num_resend;
};
typedef struct mca_ptl_gm_peer_t mca_ptl_gm_peer_t;

OBJ_CLASS_DECLARATION (mca_ptl_gm_peer_t);

#endif
