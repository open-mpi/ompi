/*
 * $HEADER$
 */
#include <string.h>
#include "atomic.h"
#include "lfc/lam_hash_table.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_tcp.h"
#include "ptl_tcp_addr.h"
#include "ptl_tcp_peer.h"
#include "ptl_tcp_proc.h"


static void mca_ptl_tcp_proc_construct(mca_ptl_tcp_proc_t* proc);
static void mca_ptl_tcp_proc_destruct(mca_ptl_tcp_proc_t* proc);
static mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_lookup_lam(lam_proc_t* lam_proc);

lam_class_t  mca_ptl_tcp_proc_t_class = {
    "mca_ptl_tcp_proc_t",
    OBJ_CLASS(lam_list_item_t),
    (lam_construct_t)mca_ptl_tcp_proc_construct,
    (lam_destruct_t)mca_ptl_tcp_proc_destruct
};
 

/**
 * Initialize tcp proc instance 
 */

void mca_ptl_tcp_proc_construct(mca_ptl_tcp_proc_t* proc)
{
    proc->proc_lam = 0;
    proc->proc_addrs = 0;
    proc->proc_addr_count = 0;
    proc->proc_peers = 0;
    proc->proc_peer_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, lam_mutex_t);

    /* add to list of all proc instance */
    THREAD_LOCK(&mca_ptl_tcp_module.tcp_lock);
    lam_list_append(&mca_ptl_tcp_module.tcp_procs, &proc->super);
    THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
}


/*
 * Cleanup tcp proc instance
 */

void mca_ptl_tcp_proc_destruct(mca_ptl_tcp_proc_t* proc)
{
    /* remove from list of all proc instances */
    THREAD_LOCK(&mca_ptl_tcp_module.tcp_lock);
    lam_list_remove_item(&mca_ptl_tcp_module.tcp_procs, &proc->super);
    THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);

    /* release resources */
    if(NULL != proc->proc_peers) 
        free(proc->proc_peers);
    if(NULL != proc->proc_guid)
        free(proc->proc_guid);
}


/*
 *  Create a TCP process structure. There is a one-to-one correspondence
 *  between a lam_proc_t and a mca_ptl_tcp_proc_t instance. We cache additional
 *  data (specifically the list of mca_ptl_tcp_peer_t instances, and publiched 
 *  addresses) associated w/ a given destination on this datastructure.
 */

mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_create(lam_proc_t* lam_proc)
{
    int rc;
    size_t size = strlen(lam_proc->proc_job) + 1;
    uint32_t vpid = htonl(lam_proc->proc_vpid);

    mca_ptl_tcp_proc_t* ptl_proc = mca_ptl_tcp_proc_lookup_lam(lam_proc);
    if(ptl_proc != NULL)
        return ptl_proc;

    ptl_proc = OBJ_NEW(mca_ptl_tcp_proc_t);
    ptl_proc->proc_lam = lam_proc;

    /* build a unique identifier (of arbitrary size) to represent the proc */
    ptl_proc->proc_guid_size = size + sizeof(uint32_t);
    ptl_proc->proc_guid = malloc(ptl_proc->proc_guid_size);
    if(ptl_proc->proc_guid == 0) {
        OBJ_RELEASE(ptl_proc);
        return 0;
    }
    memcpy(ptl_proc->proc_guid, lam_proc->proc_job, size);
    memcpy(((unsigned char*)ptl_proc->proc_guid)+size, &vpid, sizeof(uint32_t));

    /* lookup tcp parameters exported by this proc */
    rc = mca_base_modex_recv(
        &mca_ptl_tcp_module.super.ptlm_version, 
        lam_proc, 
        (void**)&ptl_proc->proc_addrs, 
        &size);
    if(rc != LAM_SUCCESS) {
        lam_output(0, "mca_ptl_tcp_proc_create: mca_base_modex_recv: failed with return value=%d", rc);
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }
    if(0 != (size % sizeof(mca_ptl_tcp_addr_t))) {
        lam_output(0, "mca_ptl_tcp_proc_create: mca_base_modex_recv: invalid size %d\n", size);
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
    if(NULL == mca_ptl_tcp_module.tcp_local && lam_proc == lam_proc_local())
        mca_ptl_tcp_module.tcp_local = ptl_proc;
    return ptl_proc;
}

/*
 * Look for an existing TCP process instances based on the associated
 * lam_proc_t instance.
 */
static mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_lookup_lam(lam_proc_t* lam_proc)
{
    mca_ptl_tcp_proc_t* tcp_proc;
    THREAD_LOCK(&mca_ptl_tcp_module.tcp_lock);
    for(tcp_proc  = (mca_ptl_tcp_proc_t*)lam_list_get_first(&mca_ptl_tcp_module.tcp_procs);
        tcp_proc != (mca_ptl_tcp_proc_t*)lam_list_get_end(&mca_ptl_tcp_module.tcp_procs);
        tcp_proc  = (mca_ptl_tcp_proc_t*)lam_list_get_next(tcp_proc)) {
        if(tcp_proc->proc_lam == lam_proc) {
            THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
            return tcp_proc;
        }
    }
    THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
    return NULL;
}


/*
 * Look for an existing TCP process instance based on the globally unique 
 * process identifier.
 */
mca_ptl_tcp_proc_t* mca_ptl_tcp_proc_lookup(void *guid, size_t size)
{
    mca_ptl_tcp_proc_t* tcp_proc;
    THREAD_LOCK(&mca_ptl_tcp_module.tcp_lock);
    for(tcp_proc  = (mca_ptl_tcp_proc_t*)lam_list_get_first(&mca_ptl_tcp_module.tcp_procs);
        tcp_proc != (mca_ptl_tcp_proc_t*)lam_list_get_end(&mca_ptl_tcp_module.tcp_procs);
        tcp_proc  = (mca_ptl_tcp_proc_t*)lam_list_get_next(tcp_proc)) {
        if(tcp_proc->proc_guid_size == size && memcmp(tcp_proc->proc_guid, guid, size) == 0) {
            THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
            return tcp_proc;
        }
    }
    THREAD_UNLOCK(&mca_ptl_tcp_module.tcp_lock);
    return NULL;
}


/*
 * Note that this routine must be called with the lock on the process already 
 * held.  Insert a ptl instance into the proc array and assign it an address.
 */
int mca_ptl_tcp_proc_insert(mca_ptl_tcp_proc_t* ptl_proc, mca_ptl_base_peer_t* ptl_peer)
{
    struct mca_ptl_tcp_t *ptl_tcp = ptl_peer->peer_ptl;
    size_t i;

    /* insert into peer array */ 
    ptl_peer->peer_proc = ptl_proc;
    ptl_proc->proc_peers[ptl_proc->proc_peer_count++] = ptl_peer;
                                                                                                                 
    /*
     * Look through the proc instance for an address that is on the 
     * directly attached network. If we don't find one, pick the first
     * unused address.
    */
    for(i=0; i<ptl_proc->proc_addr_count; i++) {
        mca_ptl_tcp_addr_t* peer_addr = ptl_proc->proc_addrs + i;
        unsigned long net1 = ptl_tcp->ptl_ifaddr.sin_addr.s_addr & ptl_tcp->ptl_ifmask.sin_addr.s_addr;
        unsigned long net2 = peer_addr->addr_inet.s_addr & ptl_tcp->ptl_ifmask.sin_addr.s_addr;
        if(peer_addr->addr_inuse != 0)
            continue;
        if(net1 == net2) {
            ptl_peer->peer_addr = peer_addr;
            break;
        } else if(ptl_peer->peer_addr != 0)
            ptl_peer->peer_addr = peer_addr;
    }
    ptl_peer->peer_addr->addr_inuse++;
    return LAM_SUCCESS;
}

/*
 * Remove a peer from the proc array and indicate the address is
 * no longer in use.
 */

int mca_ptl_tcp_proc_remove(mca_ptl_tcp_proc_t* ptl_proc, mca_ptl_base_peer_t* ptl_peer)
{
    size_t i;
    THREAD_LOCK(&ptl_proc->proc_lock);
    for(i=0; i<ptl_proc->proc_peer_count; i++) {
        if(ptl_proc->proc_peers[i] == ptl_peer) {
            memmove(&ptl_proc->proc_peers+i,ptl_proc->proc_peers+i+1,
                (ptl_proc->proc_peer_count-i)*sizeof(mca_ptl_base_peer_t*));
        }
    }
    ptl_proc->proc_peer_count--;
    ptl_peer->peer_addr->addr_inuse--;
    THREAD_UNLOCK(&ptl_proc->proc_lock);
    return LAM_SUCCESS;
}


/*
 * loop through all available PTLs for one matching the source address
 * of the request.
 */
bool mca_ptl_tcp_proc_accept(mca_ptl_tcp_proc_t* ptl_proc, struct sockaddr_in* addr, int sd)
{
    size_t i;
    THREAD_LOCK(&ptl_proc->proc_lock);
    for(i=0; i<ptl_proc->proc_peer_count; i++) {
        mca_ptl_base_peer_t* ptl_peer = ptl_proc->proc_peers[i];
        if(mca_ptl_tcp_peer_accept(ptl_peer, addr, sd)) {
            THREAD_UNLOCK(&ptl_proc->proc_lock);
            return true;
        }
    }
    THREAD_UNLOCK(&ptl_proc->proc_lock);
    return false;
}


