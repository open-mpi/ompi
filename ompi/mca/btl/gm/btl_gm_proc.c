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

#include "class/ompi_hash_table.h"
#include "mca/pml/base/pml_base_module_exchange.h"

#include "btl_gm.h"
#include "btl_gm_proc.h"

static void mca_btl_gm_proc_construct(mca_btl_gm_proc_t* proc);
static void mca_btl_gm_proc_destruct(mca_btl_gm_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_gm_proc_t, 
        ompi_list_item_t, mca_btl_gm_proc_construct, 
        mca_btl_gm_proc_destruct);

void mca_btl_gm_proc_construct(mca_btl_gm_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_addr_count = 0;
    proc->proc_endpoints = 0;
    proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);
    /* add to list of all proc instance */
    OMPI_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
    ompi_list_append(&mca_btl_gm_component.gm_procs, &proc->super);
    OMPI_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_gm_proc_destruct(mca_btl_gm_proc_t* proc)
{
    /* remove from list of all proc instances */
    OMPI_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
    ompi_list_remove_item(&mca_btl_gm_component.gm_procs, &proc->super);
    OMPI_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);

    /* release resources */
    if(NULL != proc->proc_endpoints) {
        free(proc->proc_endpoints);
    }
}


/*
 * Look for an existing GM process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_gm_proc_t* mca_btl_gm_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_gm_proc_t* gm_proc;

    OMPI_THREAD_LOCK(&mca_btl_gm_component.gm_lock);

    for(gm_proc = (mca_btl_gm_proc_t*)
            ompi_list_get_first(&mca_btl_gm_component.gm_procs);
            gm_proc != (mca_btl_gm_proc_t*)
            ompi_list_get_end(&mca_btl_gm_component.gm_procs);
            gm_proc  = (mca_btl_gm_proc_t*)ompi_list_get_next(gm_proc)) {

        if(gm_proc->proc_ompi == ompi_proc) {
            OMPI_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
            return gm_proc;
        }

    }

    OMPI_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);

    return NULL;
}

/*
 * Create a GM process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_gm_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_gm_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_gm_proc_t* mca_btl_gm_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_gm_proc_t* gm_proc = NULL;
    size_t size;
    int rc;

    /* Check if we have already created a GM proc
     * structure for this ompi process */
    gm_proc = mca_btl_gm_proc_lookup_ompi(ompi_proc);
    if(gm_proc != NULL) {
        return gm_proc;
    }

    /* create a new gm proc out of the ompi_proc ... */
    gm_proc = OBJ_NEW(mca_btl_gm_proc_t);
    gm_proc->proc_endpoint_count = 0;
    gm_proc->proc_ompi = ompi_proc;
    gm_proc->proc_guid = ompi_proc->proc_name;

    /* query for the peer address info */

    rc = mca_base_modex_recv(
        &mca_btl_gm_component.super.btl_version,
        ompi_proc,
        (void*)&gm_proc->proc_addrs,
        &size); 
    if(OMPI_SUCCESS != rc) {
        ompi_output(0, "[%s:%d] mca_base_modex_recv failed for peer [%d,%d,%d]",
            __FILE__,__LINE__,ORTE_NAME_ARGS(&ompi_proc->proc_name));
        OBJ_RELEASE(gm_proc);
        return NULL;
    }

    if((size % sizeof(mca_btl_gm_addr_t)) != 0) {
        ompi_output(0, "[%s:%d] invalid gm address for peer [%d,%d,%d]",
            __FILE__,__LINE__,ORTE_NAME_ARGS(&ompi_proc->proc_name));
        OBJ_RELEASE(gm_proc);
        return NULL;
    }

    gm_proc->proc_addr_count = sizeof(mca_btl_gm_addr_t)/size;
    gm_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
        malloc(gm_proc->proc_addr_count * sizeof(mca_btl_base_endpoint_t*));
    if(NULL == gm_proc->proc_endpoints) {
        OBJ_RELEASE(gm_proc);
        return NULL;
    }
    return gm_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_gm_proc_insert(mca_btl_gm_proc_t* gm_proc, 
        mca_btl_base_endpoint_t* gm_endpoint)
{
    /* insert into endpoint array */
    if(gm_proc->proc_addr_count <= gm_proc->proc_endpoint_count)
        return OMPI_ERR_OUT_OF_RESOURCE;
    gm_endpoint->endpoint_proc = gm_proc;
    gm_endpoint->endpoint_addr = gm_proc->proc_addrs[gm_proc->proc_endpoint_count];
    gm_proc->proc_endpoints[gm_proc->proc_endpoint_count] = gm_endpoint;
    gm_proc->proc_endpoint_count++;
    return OMPI_SUCCESS;
}

