/* @file
 *
 * $HEADER$
 */

#ifndef MCA_PTL_TCP_PROC_H
#define MCA_PTL_TCP_PROC_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "lam/lfc/object.h"
#include "mpi/proc/proc.h"
#include "ptl_tcp.h"
#include "ptl_tcp_peer.h"

extern lam_class_info_t mca_ptl_tcp_proc_t_class_info;


/**
 *  Represents the state of a remote process and the set of addresses
 *  that it exports. Also cache an instance or mca_ptl_base_peer_t for each
 *  PTL instance that attempts to open a connection to the process.
 */
struct mca_ptl_tcp_proc_t {
    lam_list_item_t super;
    lam_proc_t *proc_lam;
    void*  proc_guid; 
    size_t proc_guid_size;
    struct mca_ptl_tcp_addr_t *proc_addrs;
    size_t proc_addr_count;
    struct mca_ptl_base_peer_t **proc_peers;
    size_t proc_peer_count;
    lam_mutex_t proc_lock;
};
typedef struct mca_ptl_tcp_proc_t mca_ptl_tcp_proc_t;


mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_create(lam_proc_t* lam_proc);
mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_lookup(void *guid, size_t size);


static inline mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_local(void) 
{
    if(NULL == mca_ptl_tcp_module.tcp_local)
        mca_ptl_tcp_module.tcp_local = mca_ptl_tcp_proc_create(lam_proc_local());
    return mca_ptl_tcp_module.tcp_local;
}

int  mca_ptl_tcp_proc_insert(mca_ptl_tcp_proc_t*, mca_ptl_base_peer_t*);
int  mca_ptl_tcp_proc_remove(mca_ptl_tcp_proc_t*, mca_ptl_base_peer_t*);
bool mca_ptl_tcp_proc_accept(mca_ptl_tcp_proc_t*, struct sockaddr_in*, int sd);

#endif

