/*
 * Copyright (c) 2008-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_hash_table.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "btl_sicortex.h"
#include "btl_sicortex_proc.h"

static void mca_btl_sicortex_proc_construct(mca_btl_sicortex_proc_t* proc);
static void mca_btl_sicortex_proc_destruct(mca_btl_sicortex_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_sicortex_proc_t, 
        opal_list_item_t, mca_btl_sicortex_proc_construct, 
        mca_btl_sicortex_proc_destruct);

void mca_btl_sicortex_proc_construct(mca_btl_sicortex_proc_t* sicortex_proc)
{
    sicortex_proc->proc_ompi = 0;
    sicortex_proc->proc_addr_count = 0;
    sicortex_proc->proc_endpoints = 0;
    sicortex_proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&sicortex_proc->proc_lock, opal_mutex_t);
    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_sicortex_component.sicortex_lock);
    opal_list_append(&mca_btl_sicortex_component.sicortex_procs, &sicortex_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_sicortex_component.sicortex_lock);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_sicortex_proc_destruct(mca_btl_sicortex_proc_t* sicortex_proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_sicortex_component.sicortex_lock);
    opal_list_remove_item(&mca_btl_sicortex_component.sicortex_procs, &sicortex_proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_sicortex_component.sicortex_lock);

    /* release resources */
    if(NULL != sicortex_proc->proc_endpoints) {
        free(sicortex_proc->proc_endpoints);
    }
}


/*
 * Look for an existing SICORTEX process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_sicortex_proc_t* mca_btl_sicortex_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_sicortex_proc_t* sicortex_proc;

    OPAL_THREAD_LOCK(&mca_btl_sicortex_component.sicortex_lock);

    for(sicortex_proc = (mca_btl_sicortex_proc_t*)
            opal_list_get_first(&mca_btl_sicortex_component.sicortex_procs);
            sicortex_proc != (mca_btl_sicortex_proc_t*)
            opal_list_get_end(&mca_btl_sicortex_component.sicortex_procs);
            sicortex_proc  = (mca_btl_sicortex_proc_t*)opal_list_get_next(sicortex_proc)) {

        if(sicortex_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_sicortex_component.sicortex_lock);
            return sicortex_proc;
        }

    }

    OPAL_THREAD_UNLOCK(&mca_btl_sicortex_component.sicortex_lock);

    return NULL;
}

/*
 * Create a SICORTEX process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_sicortex_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_sicortex_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_sicortex_proc_t* mca_btl_sicortex_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_sicortex_proc_t* module_proc = NULL;
    int rc;
    size_t size;

    /* Check if we have already created a SICORTEX proc
     * structure for this ompi process */
    module_proc = mca_btl_sicortex_proc_lookup_ompi(ompi_proc);

    if(module_proc != NULL) {
        return module_proc;
    }

    /* Oops! First time, gotta create a new SICORTEX proc
     * out of the ompi_proc ... */

    module_proc = OBJ_NEW(mca_btl_sicortex_proc_t);

    /* Initialize number of peer */
    module_proc->proc_endpoint_count = 0;

    module_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary
     * size) to represent the proc */
    module_proc->proc_guid = ompi_proc->proc_name;
    rc = ompi_modex_recv(
                     &mca_btl_sicortex_component.super.btl_version,
		     ompi_proc,
		     (void*)&module_proc->proc_addrs,
		     &size);

    if(OMPI_SUCCESS != rc) {
    	OBJ_RELEASE(module_proc);
	return NULL;
    }

    if((size % sizeof(uint64_t))!= 0 ){
	OBJ_RELEASE(module_proc);
	return NULL;
    }


    module_proc->proc_addr_count = size/sizeof(uint64_t);
    if(0 == module_proc->proc_addr_count){
	module_proc->proc_endpoints = NULL;
    }else{
	module_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
		malloc(module_proc->proc_addr_count * sizeof(mca_btl_base_endpoint_t*));
    }
    

    if(NULL == module_proc->proc_endpoints) {
        OBJ_RELEASE(module_proc);
        return NULL;
    }
    return module_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_sicortex_proc_insert(mca_btl_sicortex_proc_t* module_proc, 
        mca_btl_base_endpoint_t* module_endpoint)
{
    /* insert into endpoint array */
    module_endpoint->route = (peer_struct_t*)malloc(sizeof(peer_struct_t));
    module_endpoint->route->context_id = module_proc->proc_addrs[module_proc->proc_endpoint_count];
    module_endpoint->route->route_handles[0]=((module_proc->proc_endpoint_count*3)+0)<<3;
    module_endpoint->route->route_handles[1]=((module_proc->proc_endpoint_count*3)+1)<<3;
    module_endpoint->route->route_handles[2]=((module_proc->proc_endpoint_count*3)+2)<<3;
    module_endpoint->endpoint_proc = module_proc;
    module_proc->proc_endpoints[module_proc->proc_endpoint_count++] = module_endpoint;

    return OMPI_SUCCESS;
}
