#include "include/atomic.h"
#include "class/ompi_hash_table.h"
#include "mca/base/mca_base_module_exchange.h"

#include "ptl_ib.h"
#include "ptl_ib_vapi.h"
#include "ptl_ib_proc.h"

/*
 * Look for an existing IB process instances based on the associated
 * ompi_proc_t instance.
 */
/*
mca_ptl_ib_proc_t* mca_ptl_ib_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_ptl_ib_proc_t* ib_proc;
    OMPI_THREAD_LOCK(&mca_ptl_ib_module.ib_lock);
    for(ib_proc = (mca_ptl_ib_proc_t*)
            ompi_list_get_first(&mca_ptl_ib_module.ib_procs);
            ib_proc != (mca_ptl_ib_proc_t*)
            ompi_list_get_end(&mca_ptl_ib_module.ib_procs);
            ib_proc  = (mca_ptl_ib_proc_t*)ompi_list_get_next(ib_proc)) {
        if(ib_proc->proc_ompi == ompi_proc) {
            OMPI_THREAD_UNLOCK(&mca_ptl_ib_module.ib_lock);
            return ib_proc;
        }
    }
    OMPI_THREAD_UNLOCK(&mca_ptl_ib_module.ib_lock);
    return NULL;
}
*/

/*
 * Create a IB process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_ptl_ib_proc_t instance. We cache
 * additional data (specifically the list of mca_ptl_ib_peer_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_ptl_ib_proc_t* mca_ptl_ib_proc_create(ompi_proc_t* ompi_proc)
{
    int rc;
    size_t size;

    mca_ptl_ib_proc_t* ptl_proc = NULL;

    /*
    mca_ptl_ib_proc_t* ptl_proc = 
        mca_ptl_ib_proc_lookup_ompi(ompi_proc);

    if(ptl_proc != NULL) {
        return ptl_proc;
    }
    */

    ptl_proc = OBJ_NEW(mca_ptl_ib_proc_t);

    ptl_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary
     * size) to represent the proc */
    ptl_proc->proc_guid = ompi_proc->proc_name;

    D_PRINT("Creating proc for %d\n", ompi_proc->proc_name.vpid);

    /* lookup ib parameters exported by
     * this proc */
    rc = mca_base_modex_recv(
            &mca_ptl_ib_module.super.ptlm_version,
            ompi_proc,
            (void**)&ptl_proc->proc_addrs,
            &size);

    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_ptl_ib_proc_create: mca_base_modex_recv: "
                "failed with return value=%d", rc);
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }

    if(0 != (size % sizeof(mca_ptl_ib_ud_addr_t))) {
        ompi_output(0, "mca_ptl_ib_proc_create: mca_base_modex_recv: "
                "invalid size %d\n", size);
        return NULL;
    }

    ptl_proc->proc_addr_count = size / sizeof(mca_ptl_ib_ud_addr_t);

    /* allocate space for peer array - one for
     * each exported address
     */
    ptl_proc->proc_peers = (mca_ptl_base_peer_t**)
        malloc(ptl_proc->proc_addr_count * sizeof(mca_ptl_base_peer_t*));
    if(NULL == ptl_proc->proc_peers) {
        OBJ_RELEASE(ptl_proc);
        return NULL;
    }

    D_PRINT("returning from proc_create\n");

    return ptl_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a ptl instance into the proc array and assign 
 * it an address.
 */
int mca_ptl_ib_proc_insert(mca_ptl_ib_proc_t* ptl_proc, 
        mca_ptl_base_peer_t* ptl_peer)
{
    struct mca_ptl_ib_t *ptl_ib = ptl_peer->peer_ptl;
    int i;

    /* insert into peer array */
    ptl_peer->peer_proc = ptl_proc;
    ptl_proc->proc_peers[ptl_proc->proc_peer_count++] = ptl_peer;

    /*
     * Look through the proc instance for an address that is on the
     * directly attached network. If we don't find one, pick the first
     * unused address.
     */

    for(i=0; i<ptl_proc->proc_addr_count; i++) {
        /*
        mca_ptl_ib_ud_addr_t* peer_addr = ptl_proc->proc_addrs + i;
        */

#if 0
        unsigned long net1 = ptl_ib->ptl_ifaddr.sin_addr.s_addr & ptl_ib->ptl_ifmask.sin_addr.s_addr;
        unsigned long net2 = peer_addr->addr_inet.s_addr & ptl_ib->ptl_ifmask.sin_addr.s_addr;

        if(peer_addr->addr_inuse != 0)
            continue;
        if(net1 == net2) {
            ptl_peer->peer_addr = peer_addr;
            break;
        } else if(ptl_peer->peer_addr != 0)
            ptl_peer->peer_addr = peer_addr;
#endif
    }
    /*
    ptl_peer->peer_addr->addr_inuse++;
    */
    return OMPI_SUCCESS;
}
