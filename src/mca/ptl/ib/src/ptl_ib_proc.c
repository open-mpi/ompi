#include "ompi_config.h"

#include "class/ompi_hash_table.h"
#include "mca/base/mca_base_module_exchange.h"

#include "ptl_ib.h"
#include "ptl_ib_vapi.h"
#include "ptl_ib_proc.h"

static void mca_ptl_ib_proc_construct(mca_ptl_ib_proc_t* proc);
static void mca_ptl_ib_proc_destruct(mca_ptl_ib_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_ptl_ib_proc_t, 
        ompi_list_item_t, mca_ptl_ib_proc_construct, 
        mca_ptl_ib_proc_destruct);

void mca_ptl_ib_proc_construct(mca_ptl_ib_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_addr_count = 0;
    proc->proc_peers = 0;
    proc->proc_peer_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);
    /* add to list of all proc instance */
    OMPI_THREAD_LOCK(&mca_ptl_ib_component.ib_lock);
    ompi_list_append(&mca_ptl_ib_component.ib_procs, &proc->super);
    OMPI_THREAD_UNLOCK(&mca_ptl_ib_component.ib_lock);
}

/*
 * Cleanup ib proc instance
 */

void mca_ptl_ib_proc_destruct(mca_ptl_ib_proc_t* proc)
{
    /* remove from list of all proc instances */
    OMPI_THREAD_LOCK(&mca_ptl_ib_component.ib_lock);
    ompi_list_remove_item(&mca_ptl_ib_component.ib_procs, &proc->super);
    OMPI_THREAD_UNLOCK(&mca_ptl_ib_component.ib_lock);

    /* release resources */
    if(NULL != proc->proc_peers) {
        free(proc->proc_peers);
    }
}


/*
 * Look for an existing IB process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_ptl_ib_proc_t* mca_ptl_ib_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_ptl_ib_proc_t* ib_proc;

    OMPI_THREAD_LOCK(&mca_ptl_ib_component.ib_lock);

    for(ib_proc = (mca_ptl_ib_proc_t*)
            ompi_list_get_first(&mca_ptl_ib_component.ib_procs);
            ib_proc != (mca_ptl_ib_proc_t*)
            ompi_list_get_end(&mca_ptl_ib_component.ib_procs);
            ib_proc  = (mca_ptl_ib_proc_t*)ompi_list_get_next(ib_proc)) {

        if(ib_proc->proc_ompi == ompi_proc) {
            OMPI_THREAD_UNLOCK(&mca_ptl_ib_component.ib_lock);
            return ib_proc;
        }

    }

    OMPI_THREAD_UNLOCK(&mca_ptl_ib_component.ib_lock);

    return NULL;
}

/*
 * Create a IB process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_ptl_ib_proc_t instance. We cache
 * additional data (specifically the list of mca_ptl_ib_peer_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_ptl_ib_proc_t* mca_ptl_ib_proc_create(ompi_proc_t* ompi_proc)
{
    size_t size;
    mca_ptl_ib_proc_t* module_proc = NULL;

    /* Check if we have already created a IB proc
     * structure for this ompi process */
    module_proc = mca_ptl_ib_proc_lookup_ompi(ompi_proc);

    if(module_proc != NULL) {

        /* Gotcha! */
        return module_proc;
    }

    /* Oops! First time, gotta create a new IB proc
     * out of the ompi_proc ... */

    module_proc = OBJ_NEW(mca_ptl_ib_proc_t);

    /* Initialize number of peer */
    module_proc->proc_peer_count = 0;

    module_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary
     * size) to represent the proc */
    module_proc->proc_guid = ompi_proc->proc_name;

    /* IB module doesn't have addresses exported at
     * initialization, so the addr_count is set to one. */
    module_proc->proc_addr_count = 1;


    /* XXX: Right now, there can be only 1 peer associated
     * with a proc. Needs a little bit change in 
     * mca_ptl_ib_proc_t to allow on demand increasing of
     * number of peers for this proc */

    module_proc->proc_peers = (mca_ptl_base_peer_t**)
        malloc(module_proc->proc_addr_count * sizeof(mca_ptl_base_peer_t*));

    if(NULL == module_proc->proc_peers) {
        OBJ_RELEASE(module_proc);
        return NULL;
    }
    return module_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a ptl instance into the proc array and assign 
 * it an address.
 */
int mca_ptl_ib_proc_insert(mca_ptl_ib_proc_t* module_proc, 
        mca_ptl_base_peer_t* module_peer)
{
    /* insert into peer array */
    module_peer->peer_proc = module_proc;
    module_proc->proc_peers[module_proc->proc_peer_count++] = module_peer;

    return OMPI_SUCCESS;
}
