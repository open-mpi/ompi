/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_hash_table.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"

#include "btl_udapl.h"
#include "btl_udapl_endpoint.h"
#include "btl_udapl_proc.h"

static void mca_btl_udapl_proc_construct(mca_btl_udapl_proc_t* proc);
static void mca_btl_udapl_proc_destruct(mca_btl_udapl_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_udapl_proc_t, 
        opal_list_item_t, mca_btl_udapl_proc_construct, 
        mca_btl_udapl_proc_destruct);

void mca_btl_udapl_proc_construct(mca_btl_udapl_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_addr_count = 0;
    proc->proc_endpoints = 0;
    proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);

    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);
    opal_list_append(&mca_btl_udapl_component.udapl_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
}


/*
 * Cleanup uDAPL proc instance
 */

void mca_btl_udapl_proc_destruct(mca_btl_udapl_proc_t* proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);
    opal_list_remove_item(&mca_btl_udapl_component.udapl_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);

    /* release resources */
    if(NULL != proc->proc_endpoints) {
        free(proc->proc_endpoints);
    }
}


/*
 * Look for an existing uDAPL process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_udapl_proc_t* mca_btl_udapl_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_udapl_proc_t* udapl_proc;

    OPAL_THREAD_LOCK(&mca_btl_udapl_component.udapl_lock);

    for(udapl_proc = (mca_btl_udapl_proc_t*)
            opal_list_get_first(&mca_btl_udapl_component.udapl_procs);
            udapl_proc != (mca_btl_udapl_proc_t*)
            opal_list_get_end(&mca_btl_udapl_component.udapl_procs);
            udapl_proc  = (mca_btl_udapl_proc_t*)opal_list_get_next(udapl_proc)) {

        if(udapl_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);
            return udapl_proc;
        }

    }

    OPAL_THREAD_UNLOCK(&mca_btl_udapl_component.udapl_lock);

    return NULL;
}

/*
 * Create a uDAPL process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_udapl_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_udapl_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_udapl_proc_t* mca_btl_udapl_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_udapl_proc_t* udapl_proc = NULL;
    size_t size;
    int rc;

    /* Check if we have already created a uDAPL proc
     * structure for this ompi process */
    udapl_proc = mca_btl_udapl_proc_lookup_ompi(ompi_proc);
    if(udapl_proc != NULL) {
        return udapl_proc;
    }

    /* create a new udapl proc out of the ompi_proc ... */
    udapl_proc = OBJ_NEW(mca_btl_udapl_proc_t);
    udapl_proc->proc_endpoint_count = 0;
    udapl_proc->proc_ompi = ompi_proc;
    udapl_proc->proc_guid = ompi_proc->proc_name;

    /* query for the peer address info */
    rc = mca_pml_base_modex_recv(
                 &mca_btl_udapl_component.super.btl_version,
                 ompi_proc,
                 (void*)&udapl_proc->proc_addrs,
                 &size); 
    if(OMPI_SUCCESS != rc) {
        opal_output(0, "[%s:%d] mca_pml_base_modex_recv failed for peer [%lu,%lu,%lu]",
            __FILE__,__LINE__,ORTE_NAME_ARGS(&ompi_proc->proc_name));
        OBJ_RELEASE(udapl_proc);
        return NULL;
    }

    if((size % sizeof(mca_btl_udapl_addr_t)) != 0) {
        opal_output(0, "[%s:%d] invalid udapl address for peer [%lu,%lu,%lu]",
            __FILE__,__LINE__,ORTE_NAME_ARGS(&ompi_proc->proc_name));
        OBJ_RELEASE(udapl_proc);
        return NULL;
    }

    udapl_proc->proc_addr_count = size/sizeof(mca_btl_udapl_addr_t);
    if (0 == udapl_proc->proc_addr_count) {
        udapl_proc->proc_endpoints = NULL;
    } else {
        udapl_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
            malloc(udapl_proc->proc_addr_count * sizeof(mca_btl_base_endpoint_t*));
    }
    if(NULL == udapl_proc->proc_endpoints) {
        OBJ_RELEASE(udapl_proc);
        return NULL;
    }
    return udapl_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_udapl_proc_insert(
    mca_btl_udapl_proc_t* udapl_proc, 
    mca_btl_base_endpoint_t* udapl_endpoint)
{
    /* insert into endpoint array */
    if(udapl_proc->proc_endpoint_count > udapl_proc->proc_addr_count)
        return OMPI_ERR_OUT_OF_RESOURCE;

    udapl_endpoint->endpoint_proc = udapl_proc;
    udapl_endpoint->endpoint_addr =
            udapl_proc->proc_addrs[udapl_proc->proc_endpoint_count];
   
    udapl_proc->proc_endpoints[udapl_proc->proc_endpoint_count] = udapl_endpoint;
    udapl_proc->proc_endpoint_count++;
    return OMPI_SUCCESS;
}

