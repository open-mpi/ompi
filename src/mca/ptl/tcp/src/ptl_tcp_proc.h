/* 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_TCP_PROC_H
#define MCA_PTL_TCP_PROC_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "mca/ns/ns.h"
#include "class/ompi_object.h"
#include "proc/proc.h"
#include "ptl_tcp.h"
#include "ptl_tcp_peer.h"

extern ompi_class_t mca_ptl_tcp_proc_t_class;


/**
 *  Represents the state of a remote process and the set of addresses
 *  that it exports. Also cache an instance of mca_ptl_base_peer_t for each
 *  PTL instance that attempts to open a connection to the process.
 */
struct mca_ptl_tcp_proc_t {
    ompi_list_item_t super;                  /**< allow proc to be placed on a list */
    ompi_proc_t *proc_ompi;                  /**< pointer to corresponding ompi_proc_t */
    ompi_process_name_t proc_guid;           /**< globally unique identifier for the process */
    struct mca_ptl_tcp_addr_t *proc_addrs;   /**< array of addresses published by peer */
    size_t proc_addr_count;                  /**< number of addresses published by peer */
    struct mca_ptl_base_peer_t **proc_peers; /**< array of peers that have been created to access this proc */
    size_t proc_peer_count;                  /**< number of peers */
    ompi_mutex_t proc_lock;                  /**< lock to protect against concurrent access to proc state */
};
typedef struct mca_ptl_tcp_proc_t mca_ptl_tcp_proc_t;


mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_create(ompi_proc_t* ompi_proc);
mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_lookup(const ompi_process_name_t*);


/**
 * Inlined function to return local TCP proc instance.
 */

static inline mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_local(void) 
{
    if(NULL == mca_ptl_tcp_component.tcp_local)
        mca_ptl_tcp_component.tcp_local = mca_ptl_tcp_proc_create(ompi_proc_local());
    return mca_ptl_tcp_component.tcp_local;
}

int  mca_ptl_tcp_proc_insert(mca_ptl_tcp_proc_t*, mca_ptl_base_peer_t*);
int  mca_ptl_tcp_proc_remove(mca_ptl_tcp_proc_t*, mca_ptl_base_peer_t*);
bool mca_ptl_tcp_proc_accept(mca_ptl_tcp_proc_t*, struct sockaddr_in*, int sd);

#endif

