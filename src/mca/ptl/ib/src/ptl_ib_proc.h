#ifndef MCA_PTL_IB_PROC_H
#define MCA_PTL_IB_PROC_H

#include "mca/ns/ns.h"
#include "class/ompi_object.h"
#include "proc/proc.h"
#include "ptl_ib.h"
#include "ptl_ib_vapi.h"
#include "ptl_ib_addr.h"
#include "ptl_ib_peer.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(mca_ptl_ib_proc_t);

/**
 * Represents the state of a remote process and the set of addresses
 * that it exports. Also cache an instance of mca_ptl_base_peer_t for
 * each
 * PTL instance that attempts to open a connection to the process.
 */
struct mca_ptl_ib_proc_t {
    ompi_list_item_t super;                  
    /**< allow proc to be placed on a list */

    ompi_proc_t *proc_ompi;                  
    /**< pointer to corresponding ompi_proc_t */

    ompi_process_name_t proc_guid;           
    /**< globally unique identifier for the process */

    size_t proc_addr_count;                  
    /**< number of addresses published by peer */

    struct mca_ptl_base_peer_t **proc_peers; 
    /**< array of peers that have been created to access this proc */    

    size_t proc_peer_count;                  
    /**< number of peers */

    ompi_mutex_t proc_lock;                  
    /**< lock to protect against concurrent access to proc state */
};
typedef struct mca_ptl_ib_proc_t mca_ptl_ib_proc_t;

mca_ptl_ib_proc_t* mca_ptl_ib_proc_create(ompi_proc_t* ompi_proc);
int mca_ptl_ib_proc_insert(mca_ptl_ib_proc_t*, mca_ptl_base_peer_t*);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
