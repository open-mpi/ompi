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
    return;
}


/*
 * Cleanup elan proc instance
 */

void mca_ptl_elan_proc_destruct (mca_ptl_elan_proc_t * proc)
{
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
    return NULL;
}

/*
 * Look for an existing ELAN process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_ptl_elan_proc_t *
mca_ptl_elan_proc_lookup_ompi (ompi_proc_t *ompi_proc)
{
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
