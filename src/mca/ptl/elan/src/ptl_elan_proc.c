/*
 * $HEADER$
 */
#include <string.h>
#include "atomic.h"
#include "class/ompi_hash_table.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_elan.h"
#include "ptl_elan_peer.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_priv.h"

static void     mca_ptl_elan_proc_construct (mca_ptl_elan_proc_t * proc);
static void     mca_ptl_elan_proc_destruct (mca_ptl_elan_proc_t * proc);
static mca_ptl_elan_proc_t *mca_ptl_elan_proc_lookup_ompi (ompi_proc_t *
                                                         ompi_proc);

ompi_class_t    mca_ptl_elan_proc_t_class = {
    "mca_ptl_elan_proc_t",
    OBJ_CLASS (ompi_list_item_t),
    (ompi_construct_t) mca_ptl_elan_proc_construct,
    (ompi_destruct_t) mca_ptl_elan_proc_destruct
};


/**
 * Initialize elan proc instance 
 */

void mca_ptl_elan_proc_construct (mca_ptl_elan_proc_t * proc)
{
    proc->proc_ompi = NULL;
    proc->proc_addrs = NULL;
    proc->proc_addr_count = 0;
    proc->proc_peers = NULL;
    proc->proc_peer_count = 0;
    proc->proc_guid.cellid = 0;
    proc->proc_guid.jobid = 0;
    proc->proc_guid.procid = 0;

    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);

    /* add to list of all proc instance */
    OMPI_THREAD_LOCK(&mca_ptl_elan_module.elan_lock);
    ompi_list_append(&mca_ptl_elan_module.elan_procs, &proc->super);
    OMPI_THREAD_UNLOCK(&mca_ptl_elan_module.elan_lock);

    return;
}


/*
 * Cleanup elan proc instance
 */

void mca_ptl_elan_proc_destruct (mca_ptl_elan_proc_t * proc)
{
    /* remove from list of all proc instances */
    OMPI_THREAD_LOCK(&mca_ptl_elan_module.elan_lock);
    ompi_list_remove_item(&mca_ptl_elan_module.elan_procs, &proc->super);
    OMPI_THREAD_UNLOCK(&mca_ptl_elan_module.elan_lock);

    /* release resources */
    if(NULL != proc->proc_peers) 
        free(proc->proc_peers);

    return;
}

/*
 *  Create a ELAN process structure. There is a one-to-one correspondence
 *  between a ompi_proc_t and a mca_ptl_elan_proc_t instance. 
 *  We cache additional data (specifically the list 
 *  of mca_ptl_elan_peer_t instances, and publiched 
 *  addresses) associated w/ a given destination on this datastructure.
 */

mca_ptl_elan_proc_t *mca_ptl_elan_proc_create (ompi_proc_t * ompi_proc)
{
    int rc;
    size_t size;
    mca_ptl_elan_proc_t* ptl_proc;
   
    ptl_proc = mca_ptl_elan_proc_lookup_ompi(ompi_proc);
    if(ptl_proc != NULL)
        return ptl_proc;

    ptl_proc = OBJ_NEW(mca_ptl_elan_proc_t);
    ptl_proc->proc_ompi = ompi_proc;
    ptl_proc->proc_guid = ompi_proc->proc_name;

    rc = mca_base_modex_recv( &mca_ptl_elan_module.super.ptlm_version, 
            ompi_proc, (void**)&ptl_proc->proc_addrs, &size);

    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "[%s:%d] mca_base_modex_recv failed to recv data \n",
               __FILE__, __LINE__);
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }

    if(0 != (size % sizeof(mca_ptl_elan_addr_t))) {
        ompi_output(0, "[%s:%d] invalid received data size %d\n", size);
        return NULL;
    }
    ptl_proc->proc_addr_count = size / sizeof(mca_ptl_elan_addr_t);

    /* allocate space for peer array - one for each exported address */
    ptl_proc->proc_peers = (mca_ptl_elan_peer_t**)
        malloc(ptl_proc->proc_addr_count * sizeof(mca_ptl_elan_peer_t*));

    if(NULL == ptl_proc->proc_peers) {
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }

    if(NULL == mca_ptl_elan_module.elan_local 
            && ompi_proc == ompi_proc_local()) {
        mca_ptl_elan_module.elan_local = ptl_proc;
    }
    return ptl_proc;
}

/*
 * Look for an existing ELAN process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_ptl_elan_proc_t *
mca_ptl_elan_proc_lookup_ompi (ompi_proc_t *ompi_proc)
{
    mca_ptl_elan_proc_t* elan_proc;

    OMPI_THREAD_LOCK(&mca_ptl_elan_module.elan_lock);

    elan_proc  = (mca_ptl_elan_proc_t*)
        ompi_list_get_first(&mca_ptl_elan_module.elan_procs);

    for( ; elan_proc != (mca_ptl_elan_proc_t*)
            ompi_list_get_end(&mca_ptl_elan_module.elan_procs);
            elan_proc  = (mca_ptl_elan_proc_t*)ompi_list_get_next(elan_proc)) {
        if(elan_proc->proc_ompi == ompi_proc) {
            OMPI_THREAD_UNLOCK(&mca_ptl_elan_module.elan_lock);
            return elan_proc;
        }
    }
    OMPI_THREAD_UNLOCK(&mca_ptl_elan_module.elan_lock);

    return NULL;
}


/*
 * Look for an existing ELAN process instance based on the globally unique 
 * process identifier.
 */
mca_ptl_elan_proc_t *mca_ptl_elan_proc_lookup (void *guid, size_t size)
{
    return NULL;
}


/*
 * Note that this routine must be called with the lock on the process already 
 * held.  Insert a ptl instance into the proc array and assign it an address.
 */
int mca_ptl_elan_proc_insert (mca_ptl_elan_proc_t * ptl_proc,
                             mca_ptl_elan_peer_t * ptl_peer)
{
    int    i;
    struct mca_ptl_elan_t *ptl_elan;
   
    ptl_elan = ptl_peer->peer_ptl;
    ptl_peer->peer_proc = ptl_proc;
    ptl_proc->proc_peers[ptl_proc->proc_peer_count++] = ptl_peer;

    /* Look through the proc instance for an address that is on the 
     * directly attached network. If we don't find one, pick the first
     * unused address. */

    for(i=0; i<ptl_proc->proc_addr_count; i++) {

        unsigned vp_local;
        unsigned vp_remote;
        mca_ptl_elan_addr_t* peer_addr;

        peer_addr = ptl_proc->proc_addrs + i;
        if(peer_addr->addr_inuse != 0) {
            continue;
        }

        vp_local  = ptl_elan->elan_vp;
        vp_remote = peer_addr->elan_vp;

        if(vp_local = vp_remote) {
            ptl_peer->peer_addr = peer_addr;
            break;
        } else if(ptl_peer->peer_addr != 0) {
            ptl_peer->peer_addr = peer_addr;
        }
    }

    ptl_peer->peer_addr->addr_inuse++;

    return OMPI_SUCCESS;
}

/*
 * Remove a peer from the proc array and indicate the address is
 * no longer in use.
 */

int mca_ptl_elan_proc_remove (mca_ptl_elan_proc_t * ptl_proc,
                              mca_ptl_elan_peer_t * ptl_peer)
{
    return OMPI_SUCCESS;
}


/*
 * loop through all available PTLs for one matching the source address
 * of the request.
 */
bool mca_ptl_elan_proc_accept (mca_ptl_elan_proc_t * ptl_proc,
                              struct sockaddr_in * addr, int sd)
{
    return false;
}
