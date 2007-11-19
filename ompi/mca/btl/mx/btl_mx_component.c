/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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
#include "opal/prefetch.h"
#include "opal/util/opal_environ.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "ompi/constants.h"

#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/common/mx/common_mx.h"

#include "btl_mx.h"
#include "btl_mx_frag.h"
#include "btl_mx_endpoint.h" 

#if MX_HAVE_MAPPER_STATE
#include "mx_io.h"
#include "mx_internals/mx__fops.h"
#include "mx_internals/mx__driver_interface.h"
#endif  /* MX_HAVE_MAPPER_STATE */

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
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
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
    /* initialize state */
    mca_btl_mx_component.mx_num_btls = 0;
    mca_btl_mx_component.mx_btls = NULL;
    mca_btl_mx_component.mx_use_unexpected = 0;

    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_procs, opal_list_t);
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "max_btls",
                            "Maximum number of accepted Myrinet cards",
                            false, false, 1, &mca_btl_mx_component.mx_max_btls );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "timeout",
                            "Timeout for connections",
                            false, false, MX_INFINITE, &mca_btl_mx_component.mx_timeout );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "retries",
                            "Number of retries for each new connection before considering the peer as unreacheable",
                            false, false, 20, &mca_btl_mx_component.mx_connection_retries );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "filter",
                            "Unique ID for the application (used to connect to the peers)",
                            false, false, 0xdeadbeef, &mca_btl_mx_component.mx_filter );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "self",
                            "Enable the MX support for self communications",
                            false, false, 0, &mca_btl_mx_component.mx_support_self );
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "shared_mem",
                            "Enable the MX support for shared memory",
                            false, false, 0, &mca_btl_mx_component.mx_support_sharedmem );
#ifdef HAVE_MX_REGISTER_UNEXP_HANDLER
    mca_base_param_reg_int( (mca_base_component_t*)&mca_btl_mx_component, "register_unexp",
                            "Enable the MX support for the unexpected request handler (Open MPI matching)",
			    false, false, 0, &mca_btl_mx_component.mx_use_unexpected );
#endif  /* HAVE_MX_REGISTER_UNEXP_HANDLER */
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
                            "Number of received posted in advance. Increasing this number for"
			    " communication bound application can lead to visible improvement"
			    " in performances",
                            false, false, 16, &mca_btl_mx_component.mx_max_posted_recv );

    mca_base_param_reg_string( (mca_base_component_t*)&mca_btl_mx_component, "if_include",
			       "Myrinet card to use (last 6 digits from the mapper MAC)",
			       false, false, NULL, &mca_btl_mx_component.mx_if_include );
    mca_base_param_reg_string( (mca_base_component_t*)&mca_btl_mx_component, "if_exclude",
			       "Myrinet card to avoid (last 6 digits from the mapper MAC)",
			       false, false, NULL, &mca_btl_mx_component.mx_if_exclude );

    mca_btl_mx_module.super.btl_exclusivity = 50;
    mca_btl_mx_module.super.btl_eager_limit = 4096;
    mca_btl_mx_module.super.btl_min_send_size = 4096;
    mca_btl_mx_module.super.btl_max_send_size = 64*1024;
    mca_btl_mx_module.super.btl_rdma_pipeline_send_length = 256*1024;
    mca_btl_mx_module.super.btl_rdma_pipeline_frag_size = 8*1024*1024;
    mca_btl_mx_module.super.btl_min_rdma_pipeline_size = 0;
    mca_btl_mx_module.super.btl_flags = 
        MCA_BTL_FLAGS_SEND_INPLACE | 
        MCA_BTL_FLAGS_PUT | 
        MCA_BTL_FLAGS_SEND |
        MCA_BTL_FLAGS_RDMA_MATCHED;
    mca_btl_mx_module.super.btl_bandwidth = 2000;
    mca_btl_mx_module.super.btl_latency = 5;
    mca_btl_base_param_register(&mca_btl_mx_component.super.btl_version,
            &mca_btl_mx_module.super);
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
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_procs);
    OBJ_DESTRUCT(&mca_btl_mx_component.mx_lock);

    if( NULL != mca_btl_mx_component.mx_if_include ) {
        free( mca_btl_mx_component.mx_if_include );
	mca_btl_mx_component.mx_if_include = NULL;
    }
    if( NULL != mca_btl_mx_component.mx_if_exclude ) {
        free( mca_btl_mx_component.mx_if_exclude );
	mca_btl_mx_component.mx_if_exclude = NULL;
    }
    return OMPI_SUCCESS;
}

#ifdef HAVE_MX_REGISTER_UNEXP_HANDLER
/**
 * In order to avoid useless memcpy, the unexpected handler will be called
 * by the MX library before doing any match in the MX internal queues. Here
 * we have a chance to match the message using our own matching logic from
 * the PML. If the match is realized, we will return MX_RECV_FINISHED (the
 * MX request will vanish in the MX library). If the match do not succeed
 * MX_RECV_CONTINUE have to be returned and the MX library will do the
 * match itself.
 */
static mx_unexp_handler_action_t
mca_btl_mx_unexpected_handler( void *context, mx_endpoint_addr_t source,
                               uint64_t match_value, uint32_t length,
                               void * data_if_available )
{
    mca_btl_mx_module_t* mx_btl = (mca_btl_mx_module_t*)context;
    mca_btl_base_recv_reg_t* reg;
    mca_btl_base_tag_t tag;
    mca_btl_base_descriptor_t descriptor;
    mca_btl_base_segment_t segment;

    /*opal_output( 0, "Get unexpected handler context %p source %lld match_value %lld\n"
      "\tlength %d data %p\n", context, source.stuff[0], match_value, length,
      data_if_available );*/
    if( match_value > MCA_BTL_TAG_MAX )
        return MX_RECV_CONTINUE;

    tag = match_value & 0xff;
    assert( tag < 16 );
    reg = &(mx_btl->mx_reg[tag]);

    segment.seg_addr.pval = data_if_available;
    segment.seg_len = length;
    descriptor.des_dst = &segment;
    descriptor.des_dst_cnt = 1;
    reg->cbfunc( &(mx_btl->super), tag, &descriptor, reg->cbdata );

    return MX_RECV_FINISHED;
}
#endif  /* HAVE_MX_REGISTER_UNEXP_HANDLER */

/*
 * Create and intialize an MX PTL module, where each module
 * represents a specific NIC.
 */
static mca_btl_mx_module_t* mca_btl_mx_create(uint64_t addr)
{
    mca_btl_mx_module_t* mx_btl;
    mx_return_t status;
    uint32_t nic_id, mx_unique_network_id = 0;
    char mapper_mac[7], *where;

    status = mx_nic_id_to_board_number( addr, &nic_id );
    if( MX_SUCCESS != status ) {
        return NULL;
    }

#if MX_HAVE_MAPPER_STATE
    {
        mx_return_t ret;
        mx_endpt_handle_t endp_handle;
        mx_mapper_state_t ms;

        ret = mx_open_board( nic_id, &endp_handle );
        if( MX_SUCCESS != ret ) {
            opal_output( 0, "Unable to open board %d: %s\n", nic_id, mx_strerror(ret) );
            return NULL;
        }

        ms.board_number = nic_id;
        ms.iport = 0;
        ret = mx__get_mapper_state( endp_handle, &ms );
        if( MX_SUCCESS != ret ) {
            opal_output( 0, "get_mapper_state failed for board %d: %s\n",
                         nic_id, mx_strerror(ret) );
            return NULL;
        }
	/* Keep the first 4 bytes for the network speed */
	mx_unique_network_id = ((ms.mapper_mac[3] << 16) +
				(ms.mapper_mac[4] << 8)  +
				(ms.mapper_mac[5]));

    }
#endif  /* MX_HAVE_MAPPER_STATE */

    /* Try to figure out if we are allowed to use this network */
    snprintf( mapper_mac, 7, "%6x", mx_unique_network_id );

    if( (NULL != mca_btl_mx_component.mx_if_exclude) &&
	(NULL != (where = strstr(mca_btl_mx_component.mx_if_exclude, mapper_mac))) ) {
        /*opal_output( 0, "MX network %d connected to the mapper %s has been excluded\n",
		     nic_id, mapper_mac );*/
        return NULL;
    }
    else if( (NULL != mca_btl_mx_component.mx_if_include) &&
	     (NULL == (where = strstr(mca_btl_mx_component.mx_if_include, mapper_mac))) ) {
        /*opal_output( 0, "MX network %d connected to the mapper %s has not been included\n",
		     nic_id, mapper_mac );*/
        return NULL;
    }

    mx_btl = malloc(sizeof(mca_btl_mx_module_t));
    if( NULL == mx_btl ) return NULL;

    /* copy over default settings */
    memcpy( mx_btl, &mca_btl_mx_module, sizeof(mca_btl_mx_module_t) );
    OBJ_CONSTRUCT( &mx_btl->mx_peers, opal_list_t );
    OBJ_CONSTRUCT( &mx_btl->mx_lock, opal_mutex_t );
    /* open local endpoint */
    status = mx_open_endpoint( nic_id, MX_ANY_ENDPOINT,
                               mca_btl_mx_component.mx_filter,
                               NULL, 0, &mx_btl->mx_endpoint);
    if(status != MX_SUCCESS) {
        opal_output( 0, "mca_btl_mx_init: mx_open_endpoint() failed with status %d (%s)\n",
                     status, mx_strerror(status) );
        mx_btl->mx_endpoint = NULL;
        mca_btl_mx_finalize( &mx_btl->super );
        return NULL;
    }
    mx_btl->mx_unique_network_id = mx_unique_network_id;
    mx_btl->super.btl_bandwidth = 1000;  /* whatever */
    mx_btl->super.btl_latency = 10;
#if defined(MX_HAS_NET_TYPE)
    {
        int value;
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_LINE_SPEED,
                                   &nic_id, sizeof(nic_id),
                                   &value, sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_LINE_SPEED) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        } else {
            if( MX_SPEED_2G == value ) {
                mx_btl->mx_unique_network_id |= 0xaa000000;
                mx_btl->super.btl_bandwidth = 2000;
                mx_btl->super.btl_latency = 5;
            } else if( MX_SPEED_10G == value ) {
                mx_btl->mx_unique_network_id |= 0xbb000000;
                mx_btl->super.btl_bandwidth = 10000;
                mx_btl->super.btl_latency = 3;
            } else {
                mx_btl->mx_unique_network_id |= 0xcc000000;
            }
        }
    }
#endif  /* defined(MX_HAS_NET_TYPE) */

#if 0
    {
        int counters, board, i, value, *counters_value;
        char text[MX_MAX_STR_LEN];
        char *counters_name;
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_PIO_SEND_MAX, NULL, 0,
                                   &value, sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_PIO_SEND_MAX) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        printf( "MX_PIO_SEND_MAX = %d\n", value );
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_COPY_SEND_MAX, NULL, 0,
                                   &value, sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_COPY_SEND_MAX) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        printf( "MX_COPY_SEND_MAX = %d\n", value );

        board = 0;
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_PRODUCT_CODE, &board, sizeof(int),
                                   text, MX_MAX_STR_LEN)) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_PRODUCT_CODE) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        printf( "product code %s\n", text );

        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_COUNTERS_COUNT, &board, sizeof(int),
                                   &counters, sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_COUNTERS_COUNT) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        printf( "counters = %d\n", counters );
        counters_name = (char*)malloc( counters * MX_MAX_STR_LEN );
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_COUNTERS_LABELS, &board, sizeof(int),
                                   counters_name, counters * MX_MAX_STR_LEN)) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_COUNTERS_LABELS) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        counters_value = (int*)malloc( counters * sizeof(int) );
        if( (status = mx_get_info( mx_btl->mx_endpoint, MX_COUNTERS_VALUES, &board, sizeof(int),
                                   counters_value, counters * sizeof(int))) != MX_SUCCESS ) {
            opal_output( 0, "mx_get_info(MX_COUNTERS_VALUES) failed with status %d (%s)\n",
                         status, mx_strerror(status) );
        }
        for( i = 0; i < counters; i++ )
            printf( "%d -> %s = %d\n", i, counters_name + i * MX_MAX_STR_LEN,
                    counters_value[i] );
        free( counters_name );
        free( counters_value );
    }
#endif
    /* query the endpoint address */
    if((status = mx_get_endpoint_addr( mx_btl->mx_endpoint,
                                       &mx_btl->mx_endpoint_addr)) != MX_SUCCESS) {
        opal_output( 0, "mca_btl_mx_init: mx_get_endpoint_addr() failed with status %d (%s)\n",
                     status, mx_strerror(status) );
        mca_btl_mx_finalize( &mx_btl->super );
        return NULL;
    }
#ifdef HAVE_MX_REGISTER_UNEXP_HANDLER
    if( mca_btl_mx_component.mx_use_unexpected ) {
        status = mx_register_unexp_handler( mx_btl->mx_endpoint, mca_btl_mx_unexpected_handler,
                                            (void*)mx_btl );
        if( MX_SUCCESS != status ) {
            opal_output( 0, "mca_btl_mx_init: mx_register_unexp_handler() failed with status %d (%s)\n",
                         status, mx_strerror(status) );
            mca_btl_mx_finalize( &mx_btl->super );
            return NULL;
        }
    }
#endif  /* HAVE_MX_REGISTER_UNEXP_HANDLER */
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
        ompi_modex_send(&mca_btl_mx_component.super.btl_version, 
                                NULL, 0);
        return NULL;
    }

    /* set the MX error handle to always return. This function is the only MX function
     * allowed to be called before mx_init in order to make sure that if the MX is not
     * up and running the MX library does not exit the application.
     */
    mx_set_error_handler(MX_ERRORS_RETURN);
    if( 0 == mca_btl_mx_component.mx_support_sharedmem )
        opal_setenv( "MX_DISABLE_SHMEM", "1", true, &environ );
    if( 0 == mca_btl_mx_component.mx_support_self )
        opal_setenv( "MX_DISABLE_SELF", "1", true, &environ );
    /* Force the long pipeline (up to 4Kb fragments) */
    opal_setenv( "MX_PIPELINE_LOG", "0", true, &environ );

    /* First check if MX is available ... */
    if( OMPI_SUCCESS != ompi_common_mx_initialize() ) { 
        ompi_modex_send(&mca_btl_mx_component.super.btl_version, 
                                NULL, 0);
        return NULL;
    }
        
    /* initialize objects */
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_send_eager_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_send_user_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_mx_component.mx_lock, opal_mutex_t);

    ompi_free_list_init_new( &mca_btl_mx_component.mx_send_eager_frags,
                         sizeof(mca_btl_mx_frag_t) + mca_btl_mx_module.super.btl_eager_limit,
                         CACHE_LINE_SIZE,
                         OBJ_CLASS(mca_btl_mx_frag_t),
                         0,CACHE_LINE_SIZE,
                         mca_btl_mx_component.mx_free_list_num,
                         mca_btl_mx_component.mx_free_list_max,
                         mca_btl_mx_component.mx_free_list_inc,
                         NULL ); /* use default allocator */

    ompi_free_list_init_new( &mca_btl_mx_component.mx_send_user_frags,
                         sizeof(mca_btl_mx_frag_t),
                         CACHE_LINE_SIZE,
                         OBJ_CLASS(mca_btl_mx_frag_t),
                         0,CACHE_LINE_SIZE,
                         mca_btl_mx_component.mx_free_list_num,
                         mca_btl_mx_component.mx_free_list_max,
                         mca_btl_mx_component.mx_free_list_inc,
                         NULL ); /* use default allocator */

    /* intialize process hash table */
    OBJ_CONSTRUCT( &mca_btl_mx_component.mx_procs, opal_list_t );

    /* get the number of card available on the system */
    if( (status = mx_get_info( NULL, MX_NIC_COUNT, NULL, 0,
                               &mca_btl_mx_component.mx_num_btls, sizeof(uint32_t))) != MX_SUCCESS ) {
        opal_output( 0, "mca_btl_mx_component_init: mx_get_info(MX_NIC_COUNT) failed with status %d(%s)\n",
                     status, mx_strerror(status) );
        ompi_modex_send(&mca_btl_mx_component.super.btl_version, 
                                NULL, 0);
        return NULL;
    }

    if (0 == mca_btl_mx_component.mx_num_btls) {
        mca_btl_base_error_no_nics("Myrinet/MX", "NIC");
        ompi_modex_send(&mca_btl_mx_component.super.btl_version, 
                                NULL, 0);
        return NULL;
    }

#if 0
    /* check for limit on number of btls */
    if(mca_btl_mx_component.mx_num_btls > mca_btl_mx_component.mx_max_btls)
        mca_btl_mx_component.mx_num_btls = mca_btl_mx_component.mx_max_btls;
#endif
    /* Now we know how many NIC are available on the system. We will create a BTL for each one
     * and then give a pointer to the BTL to the upper level.
     */
    mca_btl_mx_component.mx_btls = malloc( mca_btl_mx_component.mx_num_btls * sizeof(mca_btl_base_module_t*) );
    if( NULL == mca_btl_mx_component.mx_btls ) {
        opal_output( 0, "MX BTL no memory\n" );
        return NULL;
    }

    /* determine the NIC ids */
    size = sizeof(uint64_t) * (mca_btl_mx_component.mx_num_btls + 1);
    if( NULL == (nic_addrs = (uint64_t*)malloc(size)) )
        return NULL;
    if( (status = mx_get_info( NULL, MX_NIC_IDS, NULL, 0,
                               nic_addrs, size)) != MX_SUCCESS) {
        opal_output(0, "MX BTL error (mx_get_info failed) size = %ld [%s] #cards %d\n",
                    (unsigned long)size, mx_strerror(status), mca_btl_mx_component.mx_num_btls );
        free(nic_addrs);
        return NULL;
    }

    mx_addrs = (mca_btl_mx_addr_t*)calloc( mca_btl_mx_component.mx_num_btls, sizeof(mca_btl_mx_addr_t) );
    if( NULL == mx_addrs ) {
        free( nic_addrs );
        return NULL;
    }

    /* create a btl for each NIC */
    for( i = count = 0; i < mca_btl_mx_component.mx_num_btls; i++ ) {
        mca_btl_mx_module_t* mx_btl = mca_btl_mx_create(nic_addrs[i]);
        if( NULL == mx_btl ) {
            continue;
        }
        status = mx_decompose_endpoint_addr( mx_btl->mx_endpoint_addr, &(mx_addrs[count].nic_id),
                                             &(mx_addrs[count].endpoint_id) );
        if( MX_SUCCESS != status ) {
            mca_btl_mx_finalize( &mx_btl->super );
            continue;
        }
        mx_addrs[count].unique_network_id = mx_btl->mx_unique_network_id;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
        BTL_MX_ADDR_HTON(mx_addrs[count]);
#endif
        mca_btl_mx_component.mx_btls[count] = mx_btl;
	count++;  /* one more succesfully initialized MX interface */
    }
    mca_btl_mx_component.mx_num_btls = count;
    *num_btl_modules = count;
    if( 0 == count ) {
        /* No active BTL module */
        return NULL;
    }

    /* publish the MX addresses via the MCA framework */
    ompi_modex_send(&mca_btl_mx_component.super.btl_version, mx_addrs,
			     sizeof(mca_btl_mx_addr_t) * mca_btl_mx_component.mx_num_btls);

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
    return btls;
}

/*
 *  MX component progress.
 */
int mca_btl_mx_component_progress(void)
{
    int32_t num_progressed = 0, i;
    mx_status_t mx_status;
    mx_return_t mx_return;
    mx_request_t mx_request;
    mca_btl_mx_frag_t* frag;

    for( i = 0; i < mca_btl_mx_component.mx_num_btls; i++ ) {
        mca_btl_mx_module_t* mx_btl = mca_btl_mx_component.mx_btls[i];
        uint32_t mx_result = 0;

        mx_return = mx_ipeek( mx_btl->mx_endpoint, &mx_request, &mx_result );
        if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) {
            opal_output( 0, "mca_btl_mx_component_progress: mx_ipeek() failed with status %d (%s)\n",
                         mx_return, mx_strerror(mx_return) );
            continue;
        }
        if( OPAL_LIKELY(mx_result == 0) ) {
            continue;
        }
        
        mx_return = mx_test( mx_btl->mx_endpoint, &mx_request, &mx_status, &mx_result);
        if( OPAL_UNLIKELY(mx_return != MX_SUCCESS) ) {
            opal_output(0, "mca_btl_mx_progress: mx_test() failed with status %d (%s)\n",
                        mx_return, mx_strerror(mx_return));
            continue;
        }
        /* on the mx_status we have now the pointer attached to the request.
	 * This pointer indicate which fragment we are working on. On the
	 * status we have the status of the operation, so we know what we
	 * are supposed to do next.
         */
        frag = mx_status.context;
        if( NULL != frag ) {
            if( 0xff == frag->tag ) {  /* it's a send */
                /* call the completion callback */
                frag->base.des_cbfunc( &(mx_btl->super), frag->endpoint,
                                       &(frag->base), OMPI_SUCCESS );
            } else if( !mca_btl_mx_component.mx_use_unexpected ) { /* and this one is a receive */
                mca_btl_base_recv_reg_t* reg;
                mx_segment_t mx_segment;

                reg = &(mx_btl->mx_reg[frag->tag]);
                frag->base.des_dst->seg_len = mx_status.msg_length;
                reg->cbfunc( &(mx_btl->super), frag->tag, &(frag->base),
                             reg->cbdata );
                /**
                 * The upper level extract the data from the fragment.
                 * Now we can register the fragment
                 * again with the MX BTL.
                 */
                mx_segment.segment_ptr = frag->base.des_dst->seg_addr.pval;
                mx_segment.segment_length = mca_btl_mx_module.super.btl_eager_limit;
                mx_return = mx_irecv( mx_btl->mx_endpoint, &mx_segment, 1,
                                      (uint64_t)frag->tag, BTL_MX_RECV_MASK,
                                      frag, &(frag->mx_request) );
                if( MX_SUCCESS != mx_return ) {
                    opal_output( 0, "Fail to re-register a fragment with the MX NIC ... (%s)\n",
                                 mx_strerror(mx_return) );
                }
            }
        }
        num_progressed++;
    }
    return num_progressed;
}

