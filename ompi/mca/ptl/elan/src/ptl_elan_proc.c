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
#include "class/ompi_hash_table.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_elan.h"
#include "ptl_elan_peer.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_priv.h"

static void mca_ptl_elan_proc_construct (mca_ptl_elan_proc_t * proc);
static void mca_ptl_elan_proc_destruct (mca_ptl_elan_proc_t * proc);
static mca_ptl_elan_proc_t *mca_ptl_elan_proc_lookup_ompi (ompi_proc_t *
                                                           ompi_proc);

ompi_class_t mca_ptl_elan_proc_t_class = {
    "mca_ptl_elan_proc_t",
    OBJ_CLASS (ompi_list_item_t),
    (ompi_construct_t) mca_ptl_elan_proc_construct,
    (ompi_destruct_t) mca_ptl_elan_proc_destruct
};


/**
 * Initialize elan proc instance 
 */

void
mca_ptl_elan_proc_construct (mca_ptl_elan_proc_t * proc)
{
    proc->proc_ompi = NULL;
    proc->proc_addrs = NULL;
    proc->proc_addr_count = 0;
    proc->proc_peers = NULL;
    proc->proc_peer_count = 0;
    proc->proc_guid.cellid = 0;
    proc->proc_guid.jobid = 0;
    proc->proc_guid.vpid = 0;

    OBJ_CONSTRUCT (&proc->proc_lock, ompi_mutex_t);

    /* add to list of all proc instance */
    OMPI_THREAD_LOCK (&mca_ptl_elan_component.elan_lock);
    ompi_list_append (&mca_ptl_elan_component.elan_procs, &proc->super);
    OMPI_THREAD_UNLOCK (&mca_ptl_elan_component.elan_lock);

    return;
}


/*
 * Cleanup elan proc instance
 */

void
mca_ptl_elan_proc_destruct (mca_ptl_elan_proc_t * proc)
{
    /* remove from list of all proc instances */
    OMPI_THREAD_LOCK (&mca_ptl_elan_component.elan_lock);
    ompi_list_remove_item (&mca_ptl_elan_component.elan_procs, &proc->super);
    OMPI_THREAD_UNLOCK (&mca_ptl_elan_component.elan_lock);

    /* release resources */
    if (NULL != proc->proc_peers)
        free (proc->proc_peers);

    return;
}

/*
 *  Create a ELAN process structure. There is a one-to-one correspondence
 *  between a ompi_proc_t and a mca_ptl_elan_proc_t instance. 
 *  We cache additional data (specifically the list 
 *  of mca_ptl_elan_peer_t instances, and publiched 
 *  addresses) associated w/ a given destination on this datastructure.
 */

mca_ptl_elan_proc_t *
mca_ptl_elan_proc_create (ompi_proc_t * ompi_proc)
{
    int         rc;
    size_t      size;
    mca_ptl_elan_proc_t *ptl_proc;

    ptl_proc = mca_ptl_elan_proc_lookup_ompi (ompi_proc);

    if (ptl_proc != NULL) {
        return ptl_proc;
    }

    ptl_proc = OBJ_NEW (mca_ptl_elan_proc_t);
    ptl_proc->proc_ompi = ompi_proc;
    ptl_proc->proc_guid = ompi_proc->proc_name;

    /* Extract exposed addresses from remote proc */
    rc = mca_base_modex_recv (&mca_ptl_elan_component.super.ptlm_version,
                              ompi_proc, (void **) &ptl_proc->proc_addrs,
                              &size);

    if (rc != OMPI_SUCCESS) {
        ompi_output (0,
                     "[%s:%d] mca_base_modex_recv failed to recv data \n",
                     __FILE__, __LINE__);
        OBJ_RELEASE (ptl_proc);
        return NULL;
    }

    if (0 != (size % sizeof (mca_ptl_elan_addr_t))) {
        ompi_output (0, "[%s:%d] invalid received data size %d\n", 
                     __FILE__, __LINE__, size);
        return NULL;
    }
    ptl_proc->proc_addr_count = size / sizeof (mca_ptl_elan_addr_t);

    /* allocate space for peer array - one for each exported address */
    ptl_proc->proc_peers = (mca_ptl_elan_peer_t **)
        malloc (ptl_proc->proc_addr_count *
                sizeof (mca_ptl_elan_peer_t *));

    if (NULL == ptl_proc->proc_peers) {
        OBJ_RELEASE (ptl_proc);
        ompi_output (0, "[%s:%d] unable to allocate peer procs \n"
                     __FILE__, __LINE__);
        return NULL;
    }

    if (NULL == mca_ptl_elan_component.elan_local
        && ompi_proc == ompi_proc_local ()) {
        mca_ptl_elan_component.elan_local = ptl_proc;
    }
    return ptl_proc;
}

/*
 * Look for an existing ELAN process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_ptl_elan_proc_t *
mca_ptl_elan_proc_lookup_ompi (ompi_proc_t * ompi_proc)
{
    mca_ptl_elan_proc_t *elan_proc;

    OMPI_THREAD_LOCK (&mca_ptl_elan_component.elan_lock);

    elan_proc = (mca_ptl_elan_proc_t *)
        ompi_list_get_first (&mca_ptl_elan_component.elan_procs);

    for (; elan_proc != (mca_ptl_elan_proc_t *)
         ompi_list_get_end (&mca_ptl_elan_component.elan_procs);
         elan_proc =
         (mca_ptl_elan_proc_t *) ompi_list_get_next (elan_proc)) {
        if (elan_proc->proc_ompi == ompi_proc) {
            OMPI_THREAD_UNLOCK (&mca_ptl_elan_component.elan_lock);
            return elan_proc;
        }
    }
    OMPI_THREAD_UNLOCK (&mca_ptl_elan_component.elan_lock);

    return NULL;
}


/*
 * Look for an existing ELAN process instance based on the globally unique 
 * process identifier.
 */
mca_ptl_elan_proc_t *
mca_ptl_elan_proc_lookup (void *guid,
                          size_t size)
{
    return NULL;
}

/*
 * Remove a peer from the proc array and indicate the address is
 * no longer in use.
 */

int
mca_ptl_elan_proc_remove (mca_ptl_elan_proc_t * ptl_proc,
                          mca_ptl_elan_peer_t * ptl_peer)
{
    return OMPI_SUCCESS;
}


/*
 * loop through all available PTLs for one matching the source address
 * of the request.
 */
bool
mca_ptl_elan_proc_accept (mca_ptl_elan_proc_t * ptl_proc,
                          struct sockaddr_in * addr,
                          int sd)
{
    return false;
}
