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

#include "btl_mx.h"
#include "btl_mx_proc.h"

static void mca_btl_mx_proc_construct(mca_btl_mx_proc_t* proc);
static void mca_btl_mx_proc_destruct(mca_btl_mx_proc_t* proc);

OBJ_CLASS_INSTANCE(mca_btl_mx_proc_t, 
        opal_list_item_t, mca_btl_mx_proc_construct, 
        mca_btl_mx_proc_destruct);

void mca_btl_mx_proc_construct(mca_btl_mx_proc_t* proc)
{
    proc->proc_ompi           = 0;
    proc->proc_addr_index     = 0;
    proc->proc_endpoints      = NULL;
    proc->proc_endpoint_count = 0;
    proc->mx_peers_count      = 0;
    proc->mx_peers            = NULL;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);
    /* add to list of all proc instance */
    OPAL_THREAD_LOCK(&mca_btl_mx_component.mx_lock);
    opal_list_append(&mca_btl_mx_component.mx_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_mx_component.mx_lock);
}

/*
 * Cleanup MX proc instance
 */

void mca_btl_mx_proc_destruct(mca_btl_mx_proc_t* proc)
{
    /* remove from list of all proc instances */
    OPAL_THREAD_LOCK(&mca_btl_mx_component.mx_lock);
    opal_list_remove_item(&mca_btl_mx_component.mx_procs, &proc->super);
    OPAL_THREAD_UNLOCK(&mca_btl_mx_component.mx_lock);

    /* release resources */
    if( NULL != proc->proc_endpoints ) {
        free(proc->proc_endpoints);
        proc->proc_endpoints = NULL;
    }
    if( NULL != proc->mx_peers ) {
        free(proc->mx_peers);
        proc->mx_peers = NULL;
    }
}


/*
 * Look for an existing MX process instances based on the associated
 * ompi_proc_t instance.
 */
static mca_btl_mx_proc_t* mca_btl_mx_proc_lookup_ompi(ompi_proc_t* ompi_proc)
{
    mca_btl_mx_proc_t* mx_proc;

    OPAL_THREAD_LOCK(&mca_btl_mx_component.mx_lock);

    for( mx_proc = (mca_btl_mx_proc_t*)opal_list_get_first(&mca_btl_mx_component.mx_procs);
         mx_proc != (mca_btl_mx_proc_t*)opal_list_get_end(&mca_btl_mx_component.mx_procs);
         mx_proc  = (mca_btl_mx_proc_t*)opal_list_get_next(mx_proc) ) {

        if(mx_proc->proc_ompi == ompi_proc) {
            OPAL_THREAD_UNLOCK(&mca_btl_mx_component.mx_lock);
            return mx_proc;
        }

    }

    OPAL_THREAD_UNLOCK(&mca_btl_mx_component.mx_lock);

    return NULL;
}

/*
 * Create a MX process structure. There is a one-to-one correspondence
 * between a ompi_proc_t and a mca_btl_mx_proc_t instance. We cache
 * additional data (specifically the list of mca_btl_mx_endpoint_t instances, 
 * and published addresses) associated w/ a given destination on this
 * datastructure.
 */

mca_btl_mx_proc_t* mca_btl_mx_proc_create(ompi_proc_t* ompi_proc)
{
    mca_btl_mx_proc_t* module_proc = NULL;

    /* Check if we have already created a MX proc
     * structure for this ompi process */
    module_proc = mca_btl_mx_proc_lookup_ompi(ompi_proc);
    if( module_proc != NULL ) {
        /* Gotcha! */
        return module_proc;
    }

    /* Oops! First time, gotta create a new MX proc
     * out of the ompi_proc ... */

    module_proc = OBJ_NEW(mca_btl_mx_proc_t);

    module_proc->proc_ompi      = ompi_proc;

    return module_proc;
}


/*
 * Note that this routine must be called with the lock on the process
 * already held.  Insert a btl instance into the proc array and assign 
* it an address.
 */
int mca_btl_mx_proc_insert( mca_btl_mx_proc_t* module_proc, 
                            mca_btl_mx_endpoint_t* module_endpoint )
{
    mca_btl_mx_addr_t  *mx_peers;
    int rc;
    size_t size, i;

    /* query for the peer address info */
    rc = mca_pml_base_modex_recv( &mca_btl_mx_component.super.btl_version,
                                  module_proc->proc_ompi, (void*)&mx_peers, &size );
    if( OMPI_SUCCESS != rc ) {
        opal_output( 0, "mca_pml_base_modex_recv failed for peer [%d,%d,%d]",
                     ORTE_NAME_ARGS(&module_proc->proc_ompi->proc_name) );
        OBJ_RELEASE(module_proc);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if( (size % sizeof(mca_btl_mx_addr_t)) != 0 ) {
        opal_output( 0, "invalid mx address for peer [%d,%d,%d]",
                     ORTE_NAME_ARGS(&module_proc->proc_ompi->proc_name) );
        OBJ_RELEASE(module_proc);
        return OMPI_ERROR;
    }
    module_proc->mx_peers_count = size / sizeof(mca_btl_mx_addr_t);
    if( 0 == module_proc->mx_peers_count ) {  /* no available connection */
        return OMPI_ERROR;
    }

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
    for (i = 0 ; i < module_proc->mx_peers_count ; ++i) {
        BTL_MX_ADDR_NTOH(mx_peers[i]);
    }
#endif

    module_proc->status = MCA_BTL_MX_NOT_CONNECTED;
    module_proc->mx_peers = mx_peers;

    if( NULL == module_proc->proc_endpoints ) {
        module_proc->proc_endpoints = (mca_btl_base_endpoint_t**)
            malloc(module_proc->mx_peers_count * sizeof(mca_btl_base_endpoint_t*));
        if( NULL == module_proc->proc_endpoints ) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }
    /* insert into endpoint array */
    module_endpoint->endpoint_proc = module_proc;

    return OMPI_SUCCESS;
}

int mca_btl_mx_proc_connect( mca_btl_mx_endpoint_t* module_endpoint )
{
    int num_retry = 0, i;
    mx_return_t mx_status;
    mx_endpoint_addr_t mx_remote_addr;
    mca_btl_mx_proc_t* module_proc = module_endpoint->endpoint_proc;

    for( i = module_proc->proc_addr_index; i < module_proc->mx_peers_count; i++ ) {
        
    retry_connect:
        mx_status = mx_connect( module_endpoint->endpoint_btl->mx_endpoint,
                                module_proc->mx_peers[i].nic_id, module_proc->mx_peers[i].endpoint_id,
                                mca_btl_mx_component.mx_filter, mca_btl_mx_component.mx_timeout, &mx_remote_addr );
        if( MX_SUCCESS != mx_status ) {
            if( MX_TIMEOUT == mx_status )
                if( num_retry++ < mca_btl_mx_component.mx_connection_retries )
                    goto retry_connect;
            {
                char peer_name[MX_MAX_HOSTNAME_LEN];

                if( MX_SUCCESS != mx_nic_id_to_hostname( module_proc->mx_peers[i].nic_id, peer_name ) )
                    sprintf( peer_name, "unknown %lx nic_id", (long)module_proc->mx_peers[i].nic_id );

                opal_output( 0, "mx_connect fail for %s(%dth remote address) with key %x (error %s)\n", 
                             peer_name, i, mca_btl_mx_component.mx_filter, mx_strerror(mx_status) );
            }
            continue;
        }
        module_endpoint->mx_peer.nic_id      = module_proc->mx_peers[i].nic_id;
        module_endpoint->mx_peer.endpoint_id = module_proc->mx_peers[i].endpoint_id;
        module_endpoint->mx_peer_addr        = mx_remote_addr;
        module_proc->proc_addr_index         = i;
        module_proc->status                  = MCA_BTL_MX_CONNECTED;
        break;
    }

    if( i == module_proc->mx_peers_count ) {  /* no available connection */
        module_proc->status = MCA_BTL_MX_NOT_REACHEABLE;
        return OMPI_ERROR;
    }

    module_proc->proc_endpoints[module_proc->proc_endpoint_count++] = module_endpoint;
    return OMPI_SUCCESS;
}
