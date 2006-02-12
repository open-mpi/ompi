/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "opal/class/opal_hash_table.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"

#include "btl_mvapi.h"
#include "btl_mvapi_proc.h"

static void mca_btl_mvapi_proc_construct(mca_btl_mvapi_proc_t* proc);
static void mca_btl_mvapi_proc_destruct(mca_btl_mvapi_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_mvapi_proc_t, 
        opal_list_item_t, mca_btl_mvapi_proc_construct, 
        mca_btl_mvapi_proc_destruct);

void mca_btl_mvapi_proc_construct(mca_btl_mvapi_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_port_count = 0;
    proc->proc_endpoints = 0;
    proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);
    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_mvapi_component.ib_lock);
    opal_list_append(&mca_btl_mvapi_component.ib_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_mvapi_component.ib_lock);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_mvapi_proc_destruct(mca_btl_mvapi_proc_t* proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_mvapi_component.ib_lock);
    opal_list_remove_item(&mca_btl_mvapi_component.ib_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_mvapi_component.ib_lock);

    /* release resources */
    if(NULL != proc->proc_endpoints) {
        free(proc->proc_endpoints);
    }
}


/*
 * Look for an existing IB process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_mvapi_proc_t* mca_btl_mvapi_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_mvapi_proc_t* ib_proc;

    OPAL_THREAD_LOCK(&mca_btl_mvapi_component.ib_lock);

    for(ib_proc = (mca_btl_mvapi_proc_t*)
            opal_list_get_first(&mca_btl_mvapi_component.ib_procs);
            ib_proc != (mca_btl_mvapi_proc_t*)
            opal_list_get_end(&mca_btl_mvapi_component.ib_procs);
            ib_proc  = (mca_btl_mvapi_proc_t*)opal_list_get_next(ib_proc)) {

        if(ib_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_mvapi_component.ib_lock);
            return ib_proc;
        }

    }

    OPAL_THREAD_UNLOCK(&mca_btl_mvapi_component.ib_lock);

    return NULL;
}

/*
 * Create a IB process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_mvapi_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_mvapi_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_mvapi_proc_t* mca_btl_mvapi_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_mvapi_proc_t* mvapi_proc = NULL;
    size_t size; 
    int rc; 

    
    /* Check if we have already created a IB proc
     * structure for this ompi process */
    mvapi_proc = mca_btl_mvapi_proc_lookup_ompi(ompi_proc);

    if(mvapi_proc != NULL) {

        /* Gotcha! */
        return mvapi_proc;
    }

    /* Oops! First time, gotta create a new IB proc
     * out of the ompi_proc ... */

    mvapi_proc = OBJ_NEW(mca_btl_mvapi_proc_t);

    /* Initialize number of peer */
    mvapi_proc->proc_endpoint_count = 0;

    mvapi_proc->proc_ompi = ompi_proc;

    /* build a unique identifier (of arbitrary
     * size) to represent the proc */
    mvapi_proc->proc_guid = ompi_proc->proc_name;

    /* query for the peer address info */ 
    rc = mca_pml_base_modex_recv(
                                 &mca_btl_mvapi_component.super.btl_version, 
                                 ompi_proc, 
                                 (void*)&mvapi_proc->proc_ports, 
                                 &size
                                 ); 
    
    

    if(OMPI_SUCCESS != rc) {
        opal_output(0, "[%s:%d] mca_pml_base_modex_recv failed for peer [%d,%d,%d]",
            __FILE__,__LINE__,ORTE_NAME_ARGS(&ompi_proc->proc_name));
        OBJ_RELEASE(mvapi_proc);
        return NULL;
    }

    if((size % sizeof(mca_btl_mvapi_port_info_t)) != 0) {
        opal_output(0, "[%s:%d] invalid mvapi address for peer [%d,%d,%d]",
            __FILE__,__LINE__,ORTE_NAME_ARGS(&ompi_proc->proc_name));
        OBJ_RELEASE(mvapi_proc);
        return NULL;
    }


    mvapi_proc->proc_port_count = size/sizeof(mca_btl_mvapi_port_info_t);
    
    
    if (0 == mvapi_proc->proc_port_count) {
        mvapi_proc->proc_endpoints = NULL;
    } else {
        mvapi_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
            malloc(mvapi_proc->proc_port_count * sizeof(mca_btl_base_endpoint_t*));
    }

    if(NULL == mvapi_proc->proc_endpoints) {
        OBJ_RELEASE(mvapi_proc);
        return NULL;
    }
    return mvapi_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_mvapi_proc_insert(mca_btl_mvapi_proc_t* mvapi_proc, 
                              mca_btl_base_endpoint_t* mvapi_endpoint)
{
        
    /* insert into endpoint array */
    if(mvapi_proc->proc_port_count <= mvapi_proc->proc_endpoint_count) 
        return OMPI_ERR_OUT_OF_RESOURCE; 
    
    mvapi_endpoint->endpoint_proc = mvapi_proc;
    mvapi_proc->proc_endpoints[mvapi_proc->proc_endpoint_count++] = mvapi_endpoint;
    
    return OMPI_SUCCESS;
}
