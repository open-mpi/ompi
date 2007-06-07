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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/common/mx/common_mx.h"

#include "btl_mx.h"
#include "btl_mx_frag.h"
#include "btl_mx_endpoint.h" 
#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h"

mca_btl_mx_component_t mca_btl_mx_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_1,

            "mx", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_mx_component_open,  /* component open */
            mca_btl_mx_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_btl_mx_component_init,  
        mca_btl_mx_component_progress,
    }
};

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_mx_component_open(void)
{
    int tmp;

    /* initialize state */
    mca_btl_mx_component.mx_num_btls = 0;
    mca_btl_mx_component.mx_btls = NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_procs, opal_list_t);
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "max_btls",
                            "Maximum number of accepted Myrinet cards",
                            false, false, 1, &mca_btl_mx_component.mx_max_btls );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "timeout",
                            "Timeout for connections",
                            false, false, 10000, &mca_btl_mx_component.mx_timeout );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "retries",
                            "Number of retries for each new connection before considering the peer as unreacheable",
                            false, false, 20, &mca_btl_mx_component.mx_connection_retries );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "filter",
                            "Unique ID for the application (used to connect to the peers)",
                            false, false, 0xdeadbeef, &mca_btl_mx_component.mx_filter );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "shared_mem",
                            "Enable the MX support for shared memory",
                            false, true, 0, &mca_btl_mx_component.mx_support_sharedmem );

    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "free_list_num",
                            "Number of allocated default request",
                            false, false, 8, &mca_btl_mx_component.mx_free_list_num );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "free_list_inc",
                            "Number of request we allocate each time we miss some",
                            false, false, 32, &mca_btl_mx_component.mx_free_list_inc );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "free_list_max",
                            "Maximum number of request this device is allowed to allocate",
                            false, false, 128, &mca_btl_mx_component.mx_free_list_max );
    /* The ompi_free_list has a problem if the (max - num) is not
     * divisible by the increament. So make sure it is ...
     */
    if( (mca_btl_mx_component.mx_free_list_max - mca_btl_mx_component.mx_free_list_num) %
        mca_btl_mx_component.mx_free_list_inc ) {
        int overhead = (mca_btl_mx_component.mx_free_list_max - mca_btl_mx_component.mx_free_list_num) %
            mca_btl_mx_component.mx_free_list_inc;
        mca_btl_mx_component.mx_free_list_max -= overhead;
    }

    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "max_posted_recv",
                            "Number of received posted in advance. Increasing this number for communication bound application can lead to visible improvement in performances",
                            false, false, 16, &mca_btl_mx_component.mx_max_posted_recv );

    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "exclusivity",
                            "Priority compared with the others devices (used only when several devices are available",
                            false, false, 50, (int*) &mca_btl_mx_module.super.btl_exclusivity );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "first_frag_size",
                            "Size of the first fragment for the rendez-vous protocol over MX",
                            true, true, 16*1024 - 20, &tmp);
    mca_btl_mx_module.super.btl_eager_limit = tmp;
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "min_send_size",
                            "Minimum send fragment size ...",
                            false, false, 32*1024 - 40, &tmp);
    mca_btl_mx_module.super.btl_min_send_size = tmp;
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "max_send_size",
                            "Maximum send fragment size withour RDMA ...",
                            false, false, 128*1024, &tmp);
    mca_btl_mx_module.super.btl_max_send_size = tmp;
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "min_rdma_size",
                            "Minimum size of fragment for the RDMA protocol",
                            false, false, 1024*1024, &tmp);
    mca_btl_mx_module.super.btl_min_rdma_size = tmp;
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "max_rdma_size",
                            "Maximum size of fragment for the RDMA protocol",
                            false, false, 1024*1024, &tmp);
    mca_btl_mx_module.super.btl_max_rdma_size = tmp;
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "flags",
                            "Flags to activate/deactivate the RDMA",
                            true, false, MCA_BTL_FLAGS_PUT, (int*)&mca_btl_mx_module.super.btl_flags );

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_mx_component_close(void)
{
    if( NULL == mca_btl_mx_component.mx_btls )
        return OMPI_SUCCESS;
    
    if(OMPI_SUCCESS != ompi_common_mx_finalize()) { 
        return OMPI_ERROR;
    }

    /* release resources */
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_send_eager_frags);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_send_user_frags);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_recv_frags);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_procs);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_pending_acks);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_lock);
    return OMPI_SUCCESS;
}

/*
 * Create and intialize an MX PTL module, where each module
 * represents a specific NIC.
 */

static mca_btl_mx_module_t* mca_btl_mx_create(uint64_t addr)
{
    mca_btl_mx_module_t* mx_btl;
    mx_return_t status;
    uint32_t nic_id;

    status = mx_nic_id_to_board_number( addr, &nic_id );
    if( MX_SUCCESS != status ) {
        return NULL;
    }

    mx_btl = malloc(sizeof(mca_btl_mx_module_t));
    if( NULL == mx_btl ) return NULL;

    /* copy over default settings */
    memcpy( mx_btl, &mca_btl_mx_module, sizeof(mca_btl_mx_module_t) );
    mx_btl->super.btl_flags = MCA_BTL_FLAGS_SEND_INPLACE; /* | MCA_BTL_FLAGS_PUT;*/
    OBJ_CONSTRUCT( &mx_btl->mx_peers, opal_list_t );
    OBJ_CONSTRUCT( &mx_btl->mx_lock, opal_mutex_t );
    /* open local endpoint */
    status = mx_open_endpoint( nic_id, MX_ANY_ENDPOINT,
                               mca_btl_mx_component.mx_filter,
                               NULL, 0, &mx_btl->mx_endpoint);
    if(status != MX_SUCCESS) {
        opal_output(0, "mca_btl_mx_init: mx_open_endpoint() failed with status=%d\n", status);
        mx_btl->mx_endpoint = NULL;
        mca_btl_mx_finalize( &mx_btl->super );
        return NULL;
    }

    /* query the endpoint address */
    if((status = mx_get_endpoint_addr( mx_btl->mx_endpoint,
                                       &mx_btl->mx_endpoint_addr)) != MX_SUCCESS) {
        opal_output(0, "mca_btl_mx_init: mx_get_endpoint_addr() failed with status=%d\n", status);
        mca_btl_mx_finalize( &mx_btl->super );
        return NULL;
    }

    return mx_btl;
}

/*
 *  MX component initialization:
 *  - check if MX can be initialized.
 *  - and construct all static objects.
 */

mca_btl_base_module_t** mca_btl_mx_component_init(int *num_btl_modules, 
                                                  bool enable_progress_threads,
                                                  bool enable_mpi_threads)
{
    mca_btl_base_module_t** btls;
    mx_return_t status;
    uint32_t size, count;
    int32_t i;
    uint64_t *nic_addrs;
    mca_btl_mx_addr_t *mx_addrs;

    *num_btl_modules = 0;

    if (enable_progress_threads) { 
        opal_output( 0, "mca_btl_mx_component_init: progress threads requested but not supported");
        mca_pml_base_modex_send(&mca_btl_mx_component.super.btl_version, 
                                NULL, 0);
        return NULL;
    }

    /* set the MX error handle to always return. This function is the only MX function
     * allowed to be called before mx_init in order to make sure that if the MX is not
     * up and running the MX library does not exit the application.
     */
    mx_set_error_handler(MX_ERRORS_RETURN);
    /* Until this BTL reach a stable state let MX library generate assert for the errors */
    /*mx_set_error_handler(MX_ERRORS_ARE_FATAL);*/

    /* First check if MX is available ... */
    if(OMPI_SUCCESS!=ompi_common_mx_initialize()) { 
        mca_pml_base_modex_send(&mca_btl_mx_component.super.btl_version, 
                                NULL, 0);
        return NULL;
    }
        
    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_send_eager_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_send_user_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_recv_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_pending_acks, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_lock, opal_mutex_t);

    ompi_free_list_init( &mca_btl_mx_component.mx_send_eager_frags,
                         sizeof(mca_btl_mx_frag_t) + mca_btl_mx_module.super.btl_eager_limit,
                         OBJ_CLASS(mca_btl_mx_frag_t),
                         mca_btl_mx_component.mx_free_list_num,
                         mca_btl_mx_component.mx_free_list_max,
                         mca_btl_mx_component.mx_free_list_inc,
                         NULL ); /* use default allocator */

    ompi_free_list_init( &mca_btl_mx_component.mx_send_user_frags,
                         sizeof(mca_btl_mx_frag_t),
                         OBJ_CLASS(mca_btl_mx_frag_t),
                         mca_btl_mx_component.mx_free_list_num,
                         mca_btl_mx_component.mx_free_list_max,
                         mca_btl_mx_component.mx_free_list_inc,
                         NULL ); /* use default allocator */

    ompi_free_list_init( &mca_btl_mx_component.mx_recv_frags,
                         sizeof(mca_btl_mx_frag_t),
                         OBJ_CLASS(mca_btl_mx_frag_t),
                         mca_btl_mx_component.mx_free_list_num,
                         mca_btl_mx_component.mx_free_list_max,
                         mca_btl_mx_component.mx_free_list_inc,
                         NULL ); /* use default allocator */

    /* intialize process hash table */
    OBJ_CONSTRUCT( &mca_btl_mx_component.mx_procs, opal_list_t );

    /* get the number of card available on the system */
    if( (status = mx_get_info( NULL, MX_NIC_COUNT, NULL, 0,
                               &mca_btl_mx_component.mx_num_btls, sizeof(uint32_t))) != MX_SUCCESS ) {
        opal_output(0, "mca_btl_mx_component_init: mx_get_info(MX_NIC_COUNT) failed with status=%d\n", status);
        mca_pml_base_modex_send(&mca_btl_mx_component.super.btl_version, 
                                NULL, 0);
        return NULL;
    }

    if (0 == mca_btl_mx_component.mx_num_btls) {
        mca_btl_base_error_no_nics("Myrinet/MX", "NIC");
        mca_pml_base_modex_send(&mca_btl_mx_component.super.btl_version, 
                                NULL, 0);
        return NULL;
    }

    /* check for limit on number of btls */
    if(mca_btl_mx_component.mx_num_btls > mca_btl_mx_component.mx_max_btls)
        mca_btl_mx_component.mx_num_btls = mca_btl_mx_component.mx_max_btls;

    /* Now we know how many NIC are available on the system. We will create a BTL for each one
     * and then give a pointer to the BTL to the upper level.
     */
    mca_btl_mx_component.mx_btls = malloc( mca_btl_mx_component.mx_num_btls * sizeof(mca_btl_base_module_t*) );
    if( NULL == mca_btl_mx_component.mx_btls )
        return NULL;

    /* determine the NIC ids */
    size = sizeof(uint64_t) * (mca_btl_mx_component.mx_num_btls + 1);
    if( NULL == (nic_addrs = (uint64_t*)malloc(size)) )
        return NULL;
    if( (status = mx_get_info( NULL, MX_NIC_IDS, NULL, 0,
                               nic_addrs, size)) != MX_SUCCESS) {
        free(nic_addrs);
        return NULL;
    }

    size = sizeof(mca_btl_mx_addr_t) * mca_btl_mx_component.mx_num_btls;
    mx_addrs = (mca_btl_mx_addr_t*)malloc( size );
    if( NULL == mx_addrs ) {
        free( nic_addrs );
        return NULL;
    }

    /* create a btl for each NIC */
    for( i = count = 0; i < mca_btl_mx_component.mx_num_btls; i++ ) {
        mca_btl_mx_module_t* btl = mca_btl_mx_create(nic_addrs[i]);
        if( NULL == btl ) {
            continue;
        }
        status = mx_decompose_endpoint_addr( btl->mx_endpoint_addr, &(mx_addrs[i].nic_id),
                                             &(mx_addrs[i].endpoint_id) );
        if( MX_SUCCESS != status ) {
            OBJ_RELEASE( btl );
            continue;
        }

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        BTL_MX_ADDR_HTON(mx_addrs[i]);
#endif
        mca_btl_mx_component.mx_btls[count++] = btl;
    }
    size = sizeof(mca_btl_mx_addr_t) * count;
    if( 0 == count ) {
        /* No active BTL module */
    }
    mca_btl_mx_component.mx_num_btls = count;

    /* publish the MX addresses via the MCA framework */
    mca_pml_base_modex_send( &mca_btl_mx_component.super.btl_version, mx_addrs, size );

    free( nic_addrs );
    free( mx_addrs );

    btls = malloc( mca_btl_mx_component.mx_num_btls * sizeof(mca_btl_base_module_t*) );

    if( NULL == btls ) {
        free( mca_btl_mx_component.mx_btls );
        mca_btl_mx_component.mx_num_btls = 0;  /* no active BTL modules */
        return NULL;
    }
    memcpy( btls,  mca_btl_mx_component.mx_btls,
            mca_btl_mx_component.mx_num_btls*sizeof(mca_btl_mx_module_t*) );
    *num_btl_modules = mca_btl_mx_component.mx_num_btls;
    return btls;
}

/*
 *  MX component progress.
 */
int mca_btl_mx_component_progress()
{
    int32_t num_progressed = 0, i;
    mx_status_t mx_status;
    mx_return_t mx_return;
    mx_segment_t mx_segment;
    mx_request_t mx_request;
    mca_btl_mx_frag_t* frag;

    for( i = 0; i < mca_btl_mx_component.mx_num_btls; i++ ) {
        mca_btl_mx_module_t* mx_btl = mca_btl_mx_component.mx_btls[i];
        uint32_t mx_result = 0;
        
        /* pre-post receive */
#if 0
        if( mx_btl->mx_recvs_posted == 0 ) {
            OPAL_THREAD_ADD32( &mx_btl->mx_recvs_posted, 1 );
            MCA_BTL_MX_POST( mx_btl, frag );
        }
#endif

        /*if( mx_btl->mx_posted_request ) { */
        mx_return = mx_ipeek( mx_btl->mx_endpoint, &mx_request, &mx_result );
        if( mx_return != MX_SUCCESS ) {
            opal_output(0, "mca_btl_mx_component_progress: mx_ipeek() failed with status %d\n",
                        mx_return);
            continue;
        }
        if( mx_result == 0 ) {
            continue;
        }
        
        mx_return = mx_test( mx_btl->mx_endpoint, &mx_request, &mx_status, &mx_result);
        if( mx_return != MX_SUCCESS ) {
            opal_output(0, "mca_btl_mx_progress: mx_test() failed with status=%dn",
                        mx_return);
            continue;
        }

        frag = mx_status.context;
        if( 0 == frag->base.des_dst_cnt ) {  /* it's a send */
            /* call the completion callback */
            frag->base.des_cbfunc( &(mx_btl->super), frag->endpoint, &(frag->base), OMPI_SUCCESS);
        } else { /* and this one is a receive */
            mca_btl_base_recv_reg_t* reg;

            reg = &(mx_btl->mx_reg[frag->tag]);
            frag->base.des_dst->seg_len = mx_status.msg_length;
            reg->cbfunc( &(mx_btl->super), frag->tag, &(frag->base), reg->cbdata );
            /*
             * The upper level extract the data from the fragment. Now we can register the fragment
             * again with the MX BTL.
             */
            mx_segment.segment_ptr = frag->base.des_dst->seg_addr.pval;
            mx_segment.segment_length = mca_btl_mx_module.super.btl_eager_limit;
            mx_return = mx_irecv( mx_btl->mx_endpoint, &mx_segment, 1, (uint64_t)frag->tag, 
                                  (uint64_t)0xffffffffffffffffULL,
                                  frag, &(frag->mx_request) );
            if( MX_SUCCESS != mx_return ) {
                opal_output( 0, "Fail to re-register a fragment with the MX NIC ...\n" );
            }
        }

        /*MCA_BTL_MX_PROGRESS(mx_btl, mx_status);*/
        /*
         * on the mx_status we have now the pointer attached to the request. This pointer indicate
         * which fragment we are working on. On the status we have the status of the operation, so
         * we know what we are supposed to do next.
         */
        num_progressed++;
    }
    return num_progressed;
}

