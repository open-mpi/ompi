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

#include "btl_gm.h"
#include "btl_gm_proc.h"

static void mca_btl_gm_proc_construct(mca_btl_gm_proc_t* proc);
static void mca_btl_gm_proc_destruct(mca_btl_gm_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_gm_proc_t, 
        opal_list_item_t, mca_btl_gm_proc_construct, 
        mca_btl_gm_proc_destruct);

void mca_btl_gm_proc_construct(mca_btl_gm_proc_t* proc)
{
    proc->proc_ompi = 0;
    proc->proc_addr_count = 0;
    proc->proc_endpoints = 0;
    proc->proc_endpoint_count = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);
    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
    opal_list_append(&mca_btl_gm_component.gm_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
}

/*
 * Cleanup ib proc instance
 */

void mca_btl_gm_proc_destruct(mca_btl_gm_proc_t* proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
    opal_list_remove_item(&mca_btl_gm_component.gm_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);

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

    OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);

    for(gm_proc = (mca_btl_gm_proc_t*)
            opal_list_get_first(&mca_btl_gm_component.gm_procs);
            gm_proc != (mca_btl_gm_proc_t*)
            opal_list_get_end(&mca_btl_gm_component.gm_procs);
            gm_proc  = (mca_btl_gm_proc_t*)opal_list_get_next(gm_proc)) {

        if(gm_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);
            return gm_proc;
        }

    }

    OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);

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
    size_t i, size;
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
    rc = mca_pml_base_modex_recv(
                 &mca_btl_gm_component.super.btl_version,
                 ompi_proc,
                 (void*)&gm_proc->proc_addrs,
                 &size); 
    if(OMPI_SUCCESS != rc) {
        opal_output(0, "[%s:%d] mca_pml_base_modex_recv failed for peer [%d,%d,%d]",
            __FILE__,__LINE__,ORTE_NAME_ARGS(&ompi_proc->proc_name));
        OBJ_RELEASE(gm_proc);
        return NULL;
    }

    if((size % sizeof(mca_btl_gm_addr_t)) != 0) {
        opal_output(0, "[%s:%d] invalid gm address for peer [%d,%d,%d]",
            __FILE__,__LINE__,ORTE_NAME_ARGS(&ompi_proc->proc_name));
        OBJ_RELEASE(gm_proc);
        return NULL;
    }

    gm_proc->proc_addr_count = size/sizeof(mca_btl_gm_addr_t);
    if (0 == gm_proc->proc_addr_count) {
        gm_proc->proc_endpoints = NULL;
    } else {
        gm_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
            malloc(gm_proc->proc_addr_count * sizeof(mca_btl_base_endpoint_t*));
    }
    if(NULL == gm_proc->proc_endpoints) {
        OBJ_RELEASE(gm_proc);
        return NULL;
    }

    for (i = 0 ; i < gm_proc->proc_addr_count; ++i) {
        MCA_BTL_GM_ADDR_NTOH(gm_proc->proc_addrs[i]);
    }

    return gm_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
 * it an address.
 */
int mca_btl_gm_proc_insert(
    mca_btl_gm_proc_t* gm_proc, 
    mca_btl_base_endpoint_t* gm_endpoint)
{
    mca_btl_gm_module_t* gm_btl = gm_endpoint->endpoint_btl;

    /* insert into endpoint array */
    if(gm_proc->proc_addr_count <= gm_proc->proc_endpoint_count)
        return OMPI_ERR_OUT_OF_RESOURCE;
    gm_endpoint->endpoint_proc = gm_proc;
    gm_endpoint->endpoint_addr = gm_proc->proc_addrs[gm_proc->proc_endpoint_count];

    OPAL_THREAD_LOCK(&mca_btl_gm_component.gm_lock);
#if GM_API_VERSION > 0x200
    if (GM_SUCCESS != gm_global_id_to_node_id(
        gm_btl->port,
        gm_endpoint->endpoint_addr.global_id,
        &gm_endpoint->endpoint_addr.node_id)) {
        opal_output( 0, "[%s:%d] error in converting global to local id \n",
            __FILE__, __LINE__ );
        return OMPI_ERROR;
    }
    if(mca_btl_gm_component.gm_debug > 0) {
        opal_output(0, "[%d,%d,%d] mapped global id %lu to node id %lu\n", 
            ORTE_NAME_ARGS(orte_process_info.my_name),
            gm_endpoint->endpoint_addr.global_id,
            gm_endpoint->endpoint_addr.node_id);
    }
#else
    gm_endpoint->gm_addr.node_id = gm_host_name_to_node_id( gm_btl->gm_port,
        gm_endpoint->gm_addr.global_id);
    if( GM_NO_SUCH_NODE_ID == gm_endpoint->gm_addr.node_id ) {
        ompi_output( 0, "[%s:%d] unable to convert the remote host name (%s) to a host id",
            __FILE__, __LINE__, gm_endpoint->gm_addr.global_id);
        return OMPI_ERROR;
    }
#endif  /* GM_API_VERSION > 0x200 */
    OPAL_THREAD_UNLOCK(&mca_btl_gm_component.gm_lock);

    gm_proc->proc_endpoints[gm_proc->proc_endpoint_count] = gm_endpoint;
    gm_proc->proc_endpoint_count++;
    return OMPI_SUCCESS;
}

