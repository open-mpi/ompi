/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "include/sys/atomic.h"
#include "class/ompi_proc_table.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/ns/ns_types.h"
#include "ptl_tcp.h"
#include "ptl_tcp_addr.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_proc.h"


static void mca_ptl_tcp_proc_construct(mca_ptl_tcp_proc_t* proc);
static void mca_ptl_tcp_proc_destruct(mca_ptl_tcp_proc_t* proc);

OBJ_CLASS_INSTANCE(
    mca_ptl_tcp_proc_t,
    ompi_list_item_t,
    mca_ptl_tcp_proc_construct,
    mca_ptl_tcp_proc_destruct
);
 

/**
 * Initialize tcp proc instance 
 */

void mca_ptl_tcp_proc_construct(mca_ptl_tcp_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_addrs = 0;
    proc->proc_addr_count = 0;
    proc->proc_peers = 0;
    proc->proc_peer_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);
}


/*
 * Cleanup tcp proc instance
 */

void mca_ptl_tcp_proc_destruct(mca_ptl_tcp_proc_t* proc)
{
    /* remove from list of all proc instances */
    OMPI_THREAD_LOCK(&mca_ptl_tcp_component.tcp_lock);
    ompi_hash_table_remove_proc(&mca_ptl_tcp_component.tcp_procs, &proc->proc_name);
    OMPI_THREAD_UNLOCK(&mca_ptl_tcp_component.tcp_lock);

    /* release resources */
    if(NULL != proc->proc_peers) 
        free(proc->proc_peers);
    OBJ_DESTRUCT(&proc->proc_lock);
}


/*
 *  Create a TCP process structure. There is a one-to-one correspondence
 *  between a ompi_proc_t and a mca_ptl_tcp_proc_t instance. We cache additional
 *  data (specifically the list of mca_ptl_tcp_peer_t instances, and publiched 
 *  addresses) associated w/ a given destination on this datastructure.
 */

mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_create(ompi_proc_t* ompi_proc)
{
    int rc;
    size_t size;
    mca_ptl_tcp_proc_t* ptl_proc;

    OMPI_THREAD_LOCK(&mca_ptl_tcp_component.tcp_lock);
    ptl_proc = (mca_ptl_tcp_proc_t*)ompi_hash_table_get_proc(
         &mca_ptl_tcp_component.tcp_procs, &ompi_proc->proc_name);
    if(NULL != ptl_proc) {
        OMPI_THREAD_UNLOCK(&mca_ptl_tcp_component.tcp_lock);
        return ptl_proc;
     }

    ptl_proc = OBJ_NEW(mca_ptl_tcp_proc_t);
    if(NULL == ptl_proc)
        return NULL;
    ptl_proc->proc_ompi = ompi_proc;
    ptl_proc->proc_name = ompi_proc->proc_name;

    /* add to hash table of all proc instance */
    ompi_hash_table_set_proc(
        &mca_ptl_tcp_component.tcp_procs, 
        &ptl_proc->proc_name, 
         ptl_proc);
    OMPI_THREAD_UNLOCK(&mca_ptl_tcp_component.tcp_lock);

    /* lookup tcp parameters exported by this proc */
    rc = mca_base_modex_recv( &mca_ptl_tcp_component.super.ptlm_version, 
			      ompi_proc, 
			      (void**)&ptl_proc->proc_addrs, 
			      &size);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_ptl_tcp_proc_create: mca_base_modex_recv: failed with return value=%d", rc);
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }
    if(0 != (size % sizeof(mca_ptl_tcp_addr_t))) {
        ompi_output(0, "mca_ptl_tcp_proc_create: mca_base_modex_recv: invalid size %d\n", size);
        return NULL;
    }
    ptl_proc->proc_addr_count = size / sizeof(mca_ptl_tcp_addr_t);

    /* allocate space for peer array - one for each exported address */
    ptl_proc->proc_peers = (mca_ptl_base_peer_t**)
        malloc(ptl_proc->proc_addr_count * sizeof(mca_ptl_base_peer_t*));
    if(NULL == ptl_proc->proc_peers) {
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }
    if(NULL == mca_ptl_tcp_component.tcp_local && ompi_proc == ompi_proc_local())
        mca_ptl_tcp_component.tcp_local = ptl_proc;
    return ptl_proc;
}


/*
 * Look for an existing TCP process instance based on the globally unique 
 * process identifier.
 */
mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_lookup(const orte_process_name_t *name)
{
    mca_ptl_tcp_proc_t* proc;
    OMPI_THREAD_LOCK(&mca_ptl_tcp_component.tcp_lock);
    proc = (mca_ptl_tcp_proc_t*)ompi_hash_table_get_proc(
         &mca_ptl_tcp_component.tcp_procs, name);
    OMPI_THREAD_UNLOCK(&mca_ptl_tcp_component.tcp_lock);
    return proc;
}

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

/*
 * Note that this routine must be called with the lock on the process already 
 * held.  Insert a ptl instance into the proc array and assign it an address.
 */
int mca_ptl_tcp_proc_insert(mca_ptl_tcp_proc_t* ptl_proc, mca_ptl_base_peer_t* ptl_peer)
{
    struct mca_ptl_tcp_module_t *ptl_tcp = ptl_peer->peer_ptl;
    size_t i;
    unsigned long net1;

    /* insert into peer array */ 
    ptl_peer->peer_proc = ptl_proc;
    ptl_proc->proc_peers[ptl_proc->proc_peer_count++] = ptl_peer;

    net1 = ptl_tcp->ptl_ifaddr.sin_addr.s_addr & ptl_tcp->ptl_ifmask.sin_addr.s_addr;
    /*
     * Look through the proc instance for an address that is on the 
     * directly attached network. If we don't find one, pick the first
     * unused address.
    */
    for(i=0; i<ptl_proc->proc_addr_count; i++) {
        mca_ptl_tcp_addr_t* peer_addr = ptl_proc->proc_addrs + i;
        unsigned long net2 = peer_addr->addr_inet.s_addr & ptl_tcp->ptl_ifmask.sin_addr.s_addr;
        if(peer_addr->addr_inuse != 0)
            continue;
        if(net1 == net2) {
            ptl_peer->peer_addr = peer_addr;
            break;
        } else if(ptl_peer->peer_addr != 0)
            ptl_peer->peer_addr = peer_addr;
    }
    /* Make sure there is a common interface */
    if( NULL != ptl_peer->peer_addr ) {
        ptl_peer->peer_addr->addr_inuse++;
        return OMPI_SUCCESS;
    }
    return OMPI_ERR_UNREACH;
}

/*
 * Remove a peer from the proc array and indicate the address is
 * no longer in use.
 */

int mca_ptl_tcp_proc_remove(mca_ptl_tcp_proc_t* ptl_proc, mca_ptl_base_peer_t* ptl_peer)
{
    size_t i;
    OMPI_THREAD_LOCK(&ptl_proc->proc_lock);
    for(i=0; i<ptl_proc->proc_peer_count; i++) {
        if(ptl_proc->proc_peers[i] == ptl_peer) {
            memmove(ptl_proc->proc_peers+i, ptl_proc->proc_peers+i+1,
                (ptl_proc->proc_peer_count-i-1)*sizeof(mca_ptl_base_peer_t*));
            if(--ptl_proc->proc_peer_count == 0) {
                OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
                OBJ_RELEASE(ptl_proc);
                return OMPI_SUCCESS;
            }
            ptl_peer->peer_addr->addr_inuse--;
            break;
        }
    }
    OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
    return OMPI_SUCCESS;
}


/*
 * loop through all available PTLs for one matching the source address
 * of the request.
 */
bool mca_ptl_tcp_proc_accept(mca_ptl_tcp_proc_t* ptl_proc, struct sockaddr_in* addr, int sd)
{
    size_t i;
    OMPI_THREAD_LOCK(&ptl_proc->proc_lock);
    for(i=0; i<ptl_proc->proc_peer_count; i++) {
        mca_ptl_base_peer_t* ptl_peer = ptl_proc->proc_peers[i];
        if(mca_ptl_tcp_peer_accept(ptl_peer, addr, sd)) {
            OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
            return true;
        }
    }
    OMPI_THREAD_UNLOCK(&ptl_proc->proc_lock);
    return false;
}


