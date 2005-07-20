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
#include "class/orte_proc_table.h"
#include "mca/pml/base/pml_base_module_exchange.h"
#include "ptl_mx.h"
#include "ptl_mx_peer.h"
#include "ptl_mx_proc.h"


static void mca_ptl_mx_proc_construct(mca_ptl_mx_proc_t* proc);
static void mca_ptl_mx_proc_destruct(mca_ptl_mx_proc_t* proc);

OBJ_CLASS_INSTANCE(
    mca_ptl_mx_proc_t,
    opal_list_item_t,
    mca_ptl_mx_proc_construct,
    mca_ptl_mx_proc_destruct
);
 

/**
 * Initialize mx proc instance 
 */

void mca_ptl_mx_proc_construct(mca_ptl_mx_proc_t* proc)
{
    proc->proc_ompi = NULL;
    proc->proc_addrs = NULL;
    proc->proc_addr_count = 0;
    proc->proc_peers = NULL;
    proc->proc_peer_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);
}


/*
 * Cleanup mx proc instance
 */

void mca_ptl_mx_proc_destruct(mca_ptl_mx_proc_t* proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_ptl_mx_component.mx_lock);
    orte_hash_table_remove_proc(&mca_ptl_mx_component.mx_procs, &proc->proc_name);
    OPAL_THREAD_UNLOCK(&mca_ptl_mx_component.mx_lock);

    /* release resources */
    if(NULL != proc->proc_peers) 
        free(proc->proc_peers);
}


/*
 *  Create a MX process structure. There is a one-to-one correspondence
 *  between a ompi_proc_t and a mca_ptl_mx_proc_t instance. We cache additional
 *  data (specifically the list of mca_ptl_mx_peer_t instances, and publiched 
 *  addresses) associated w/ a given destination on this datastructure.
 */

mca_ptl_mx_proc_t* mca_ptl_mx_proc_create(ompi_proc_t* ompi_proc)
{
    int rc;
    size_t size;
    mca_ptl_mx_proc_t* ptl_proc;

    OPAL_THREAD_LOCK(&mca_ptl_mx_component.mx_lock);
    ptl_proc = (mca_ptl_mx_proc_t*)orte_hash_table_get_proc(
         &mca_ptl_mx_component.mx_procs, &ompi_proc->proc_name);
    if(NULL != ptl_proc) {
        OPAL_THREAD_UNLOCK(&mca_ptl_mx_component.mx_lock);
        return ptl_proc;
    }

    ptl_proc = OBJ_NEW(mca_ptl_mx_proc_t);
    if(NULL == ptl_proc)
        return NULL;
    ptl_proc->proc_ompi = ompi_proc;
    ptl_proc->proc_name = ompi_proc->proc_name;

    /* add to hash table of all proc instance */
    orte_hash_table_set_proc(
        &mca_ptl_mx_component.mx_procs, 
        &ptl_proc->proc_name, 
         ptl_proc);
    OPAL_THREAD_UNLOCK(&mca_ptl_mx_component.mx_lock);

    /* lookup mx parameters exported by this proc */
    rc = mca_base_modex_recv(
        &mca_ptl_mx_component.super.ptlm_version, 
        ompi_proc, 
        (void**)&ptl_proc->proc_addrs, 
        &size);
    if(rc != OMPI_SUCCESS) {
        opal_output(0, "mca_ptl_mx_proc_create: mca_base_modex_recv: failed with return value=%d", rc);
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }
    if(0 != (size % sizeof(mca_ptl_mx_endpoint_t))) {
        opal_output(0, "mca_ptl_mx_proc_create: mca_base_modex_recv: invalid size %d\n", size);
        return NULL;
    }
    ptl_proc->proc_addr_count = size / sizeof(mca_ptl_mx_endpoint_t);

    /* allocate space for peer array - one for each exported address */
    ptl_proc->proc_peers = (mca_ptl_mx_peer_t**)
        malloc(ptl_proc->proc_addr_count * sizeof(mca_ptl_base_peer_t*));
    if(NULL == ptl_proc->proc_peers) {
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }
    return ptl_proc;
}


/*
 * Look for an existing MX process instance based on the globally unique 
 * process identifier.
 */
mca_ptl_mx_proc_t* mca_ptl_mx_proc_lookup(const orte_process_name_t *name)
{
    mca_ptl_mx_proc_t* proc;
    OPAL_THREAD_LOCK(&mca_ptl_mx_component.mx_lock);
    proc = (mca_ptl_mx_proc_t*)orte_hash_table_get_proc(
         &mca_ptl_mx_component.mx_procs, name);
    OPAL_THREAD_UNLOCK(&mca_ptl_mx_component.mx_lock);
    return proc;
}


/*
 * Note that this routine must be called with the lock on the process already 
 * held.  Insert a ptl instance into the proc array and assign it an address.
 */
int mca_ptl_mx_proc_insert(mca_ptl_mx_proc_t* ptl_proc, mca_ptl_base_peer_t* ptl_peer)
{
    mx_return_t mx_status;
    mca_ptl_mx_endpoint_t* remote = &(ptl_proc->proc_addrs[ptl_proc->proc_peer_count]);
    int num_retry = 0;
    /* insert into peer array */ 
    ptl_peer->peer_proc = ptl_proc;
    ptl_proc->proc_peers[ptl_proc->proc_peer_count] = ptl_peer;

    /* construct the remote endpoint addr */
 retry_connect:
    mx_status = mx_connect( ptl_peer->peer_ptl->mx_endpoint, remote->nic_id, remote->endpoint_id,
                            mca_ptl_mx_component.mx_filter, 1, &(ptl_peer->peer_addr) );
    if( OMPI_SUCCESS != mx_status ) {
        if( MX_TIMEOUT == mx_status )
            if( num_retry++ < 5 )
                goto retry_connect;
        opal_output( 0, "mx_connect fail for peer %d remote %lx %d filter %x with error %s\n",
                     ptl_proc->proc_peer_count,
                     remote->nic_id, remote->endpoint_id, mca_ptl_mx_component.mx_filter,
                     mx_strerror(mx_status) );
        return OMPI_ERROR;
    }
    ptl_proc->proc_peer_count++;
    return OMPI_SUCCESS;
}

/*
 * Remove a peer from the proc array and indicate the address is
 * no longer in use.
 */

int mca_ptl_mx_proc_remove(mca_ptl_mx_proc_t* ptl_proc, mca_ptl_base_peer_t* ptl_peer)
{
    size_t i;
    OPAL_THREAD_LOCK(&ptl_proc->proc_lock);
    for(i=0; i<ptl_proc->proc_peer_count; i++) {
        if(ptl_proc->proc_peers[i] == ptl_peer) {
            memmove(ptl_proc->proc_peers+i, ptl_proc->proc_peers+i+1,
                (ptl_proc->proc_peer_count-i-1)*sizeof(mca_ptl_mx_peer_t*));
            ptl_proc->proc_peer_count--;
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&ptl_proc->proc_lock);
    return OMPI_SUCCESS;
}

