/*
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
#include "class/ompi_list.h"
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
    ompi_list_item_t            super;

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

