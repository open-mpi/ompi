/* 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_ELAN_PROC_H
#define MCA_PTL_ELAN_PROC_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_object.h"
#include "proc/proc.h"
#include "ptl_elan.h"
#include "ptl_elan_peer.h"

extern ompi_class_t mca_ptl_elan_proc_t_class;

/**
 *  Represents the state of a remote process. Also cache an instance 
 *  of mca_ptl_base_peer_t for each
 *  PTL instance that attempts to open a connection to the process.
 */
struct mca_ptl_elan_proc_t {
    ompi_list_item_t super;   /**< allow proc to be placed on a list */
    ompi_proc_t *proc_ompi;   /**< pointer to corresponding ompi_proc_t */
    struct mca_ptl_base_peer_t **proc_peers; /**< array of peers */
    size_t proc_peer_count;                  /**< number of peers */
    void*  proc_uuid;    /**< univeral unique identifier the process */
    size_t univ_size;    /**< size of the universe */
    ompi_mutex_t proc_lock;   /**< lock to for proc state */
};
typedef struct mca_ptl_elan_proc_t mca_ptl_elan_proc_t;

mca_ptl_elan_proc_t* mca_ptl_elan_proc_create(ompi_proc_t* ompi_proc);
mca_ptl_elan_proc_t* mca_ptl_elan_proc_lookup(void *guid, size_t size);

static inline mca_ptl_elan_proc_t* mca_ptl_elan_proc_local(void) 
{
    return NULL;
}

int  mca_ptl_elan_proc_insert(mca_ptl_elan_proc_t *, mca_ptl_elan_peer_t *);
int  mca_ptl_elan_proc_remove(mca_ptl_elan_proc_t *, mca_ptl_elan_peer_t *);
bool mca_ptl_elan_proc_accept(mca_ptl_elan_proc_t *, 
	struct sockaddr_in *, int sd);


#endif
