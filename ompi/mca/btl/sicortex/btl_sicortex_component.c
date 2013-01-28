/*
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
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
#include "orte/util/show_help.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h" 
#include "btl_sicortex.h"
#include "btl_sicortex_frag.h"
#include "btl_sicortex_endpoint.h" 
#include "ompi/mca/btl/base/base.h" 
#include "ompi/datatype/convertor.h" 

mca_btl_sicortex_component_t mca_btl_sicortex_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_2_0_0,

            "sicortex", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_sicortex_component_open,  /* component open */
            mca_btl_sicortex_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

	mca_btl_sicortex_component_init,
        mca_btl_sicortex_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char*
mca_btl_sicortex_param_register_string( const char* param_name, 
                                        const char* default_value )
{
    char *param_value;
    int id = mca_base_param_register_string( "btl", "sicortex", param_name,
                                             NULL, default_value );
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int
mca_btl_sicortex_param_register_int( const char* param_name, 
                                     int default_value )
{
    int id = mca_base_param_register_int( "btl", "sicortex", param_name,
                                          NULL, default_value );
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_sicortex_component_open(void)
{
    /* initialize state */
    mca_btl_sicortex_component.sicortex_num_btls = 0;
    mca_btl_sicortex_component.sicortex_btls = NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_sicortex_component.sicortex_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_sicortex_component.sicortex_lock, opal_mutex_t);

    /* register SICORTEX component parameters */
    mca_btl_sicortex_component.sicortex_max_btls =
            mca_btl_sicortex_param_register_int("max_modules", 4);
    mca_btl_sicortex_component.sicortex_free_list_num =
        mca_btl_sicortex_param_register_int ("free_list_num", 8);
    mca_btl_sicortex_component.sicortex_free_list_max =
        mca_btl_sicortex_param_register_int ("free_list_max", 1024);
    mca_btl_sicortex_component.sicortex_free_list_inc =
        mca_btl_sicortex_param_register_int ("free_list_inc", 32);
    mca_btl_sicortex_component.sicortex_mpool_name = 
        mca_btl_sicortex_param_register_string("mpool", "sicortex"); 

    mca_btl_sicortex_module.super.btl_exclusivity = 0;
    mca_btl_sicortex_module.super.btl_eager_limit =  2400;/* 4*1024 - sizeof(mca_btl_base_header_t);*/
    mca_btl_sicortex_module.super.btl_rndv_eager_limit = 4*1024;/*4*1024 - sizeof(mca_btl_base_header_t);*/
    mca_btl_sicortex_module.super.btl_max_send_size = 4*1024;/*4*1024 - sizeof(mca_btl_base_header_t);*/
    mca_btl_sicortex_module.super.btl_min_rdma_pipeline_size = 1024*1024;
    mca_btl_sicortex_module.super.btl_rdma_pipeline_frag_size = 1024*1024;
    mca_btl_sicortex_module.super.btl_rdma_pipeline_send_length = 1024*1024;
    mca_btl_sicortex_module.super.btl_flags = MCA_BTL_FLAGS_SEND | MCA_BTL_FLAGS_PUT;
    mca_btl_sicortex_module.super.btl_latency = 3;  /* Microsec */
    mca_btl_sicortex_module.super.btl_bandwidth = 14000;  /* Mbs */

    mca_btl_base_param_register(&mca_btl_sicortex_component.super.btl_version,
                                &mca_btl_sicortex_module.super);

    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_sicortex_component_close(void)
{
    if( 0 != mca_btl_sicortex_component.sicortex_num_btls ) {
        OBJ_DESTRUCT(&mca_btl_sicortex_component.sicortex_frag_eager);
        OBJ_DESTRUCT(&mca_btl_sicortex_component.sicortex_frag_max);
        OBJ_DESTRUCT(&mca_btl_sicortex_component.sicortex_frag_user);
        mca_btl_sicortex_component.sicortex_num_btls = 0;
        free(mca_btl_sicortex_component.sicortex_btls);
        mca_btl_sicortex_component.sicortex_btls = NULL;
    }

    return OMPI_SUCCESS;
}

static int mca_btl_sicortex_modex_send(void)
{
    int		rc;
    size_t 	i;
    size_t	size;
    uint64_t	*addrs = NULL;

    size = mca_btl_sicortex_component.sicortex_num_btls * sizeof( uint64_t);
    if(0 != size){
	addrs = (uint64_t*) malloc(size);
	for( i = 0; i < mca_btl_sicortex_component.sicortex_num_btls; i++ ) {
            mca_btl_sicortex_module_t *btl = mca_btl_sicortex_component.sicortex_btls[i];
            addrs[i]= btl->localContext;
	}
    }
    rc = ompi_modex_send(&mca_btl_sicortex_component.super.btl_version, addrs, size);
    if( NULL != addrs ) {
	free(addrs);
    }
    return rc;

}

mca_btl_base_module_t**
mca_btl_sicortex_component_init( int *num_btl_modules, 
				 bool enable_progress_threads,
				 bool enable_mpi_threads )
{
    int rc;
    mca_btl_base_module_t **btls;
    uint64_t localContext;
    mca_btl_sicortex_module_t *btl;
    scdma_context_t *ctx = (scdma_context_t*) malloc(sizeof(scdma_context_t));

    *num_btl_modules = 0;

    /*init scdma*/
    rc = scdma_open( -1,      /* Process (-1 for ANY) */
                     ctx,     /* the context... */
                     131072,  /* event queue size */
                     16384,   /* cmd queue size */
                     65536,   /* heap size */
                     4096,    /* num RDT entries */
                     4096);   /* num BDT entries */
    if( rc ) {
    	opal_output( 0, "[%s:%d] error in initializing the sicortex library\n", __FILE__, __LINE__ );
	return NULL;
    }

    /* get context of each card */
    localContext = scdma_get_my_context_id(ctx);
    mca_btl_sicortex_component.sicortex_btls = malloc( mca_btl_sicortex_component.sicortex_max_btls * sizeof (mca_btl_sicortex_module_t*));
    if( NULL == mca_btl_sicortex_component.sicortex_btls ) {
	return NULL;
    }
    
    mca_btl_sicortex_component.sicortex_num_btls = 0;
    btl = (mca_btl_sicortex_module_t *)malloc( sizeof(mca_btl_sicortex_module_t) );
    memcpy (btl, &mca_btl_sicortex_module, sizeof(mca_btl_sicortex_module_t));
    btl->localContext = localContext;
    btl->ctx = ctx;
    btl->handle_count = 0;

    OBJ_CONSTRUCT(&btl->sicortex_lock, opal_mutex_t);

    mca_btl_sicortex_component.sicortex_btls[mca_btl_sicortex_component.sicortex_num_btls] = btl;
    mca_btl_sicortex_component.sicortex_num_btls++;

    if (OMPI_SUCCESS != mca_btl_sicortex_modex_send()) {
	return NULL;
    }

    /* initialize free lists */
    OBJ_CONSTRUCT(&mca_btl_sicortex_component.sicortex_frag_eager, ompi_free_list_t);
    ompi_free_list_init_new( &mca_btl_sicortex_component.sicortex_frag_eager,
			     sizeof(mca_btl_sicortex_frag_eager_t) + mca_btl_sicortex_module.super.btl_eager_limit,
			     CACHE_LINE_SIZE,
			     OBJ_CLASS (mca_btl_sicortex_frag_eager_t),
			     0,
			     CACHE_LINE_SIZE,
			     mca_btl_sicortex_component.sicortex_free_list_num,
			     mca_btl_sicortex_component.sicortex_free_list_max,
			     mca_btl_sicortex_component.sicortex_free_list_inc,
			     btl->super.btl_mpool );
 
    OBJ_CONSTRUCT(&mca_btl_sicortex_component.sicortex_frag_max, ompi_free_list_t);
    ompi_free_list_init_new( &mca_btl_sicortex_component.sicortex_frag_max,
			     sizeof (mca_btl_sicortex_frag_max_t) + mca_btl_sicortex_module.super.btl_max_send_size,
			     CACHE_LINE_SIZE,
			     OBJ_CLASS (mca_btl_sicortex_frag_max_t),
			     0,
			     CACHE_LINE_SIZE,
			     mca_btl_sicortex_component.sicortex_free_list_num,
			     mca_btl_sicortex_component.sicortex_free_list_max,
			     mca_btl_sicortex_component.sicortex_free_list_inc,
			     btl->super.btl_mpool );
    
    OBJ_CONSTRUCT(&mca_btl_sicortex_component.sicortex_frag_user, ompi_free_list_t);
    ompi_free_list_init_new( &mca_btl_sicortex_component.sicortex_frag_user,
			     sizeof (mca_btl_sicortex_frag_user_t),
			     CACHE_LINE_SIZE,
			     OBJ_CLASS (mca_btl_sicortex_frag_user_t),
			     0,
			     CACHE_LINE_SIZE,
			     mca_btl_sicortex_component.sicortex_free_list_num,
			     mca_btl_sicortex_component.sicortex_free_list_max,
			     mca_btl_sicortex_component.sicortex_free_list_inc,
			     NULL ); 

    /* return array of BTLs */
    btls = (mca_btl_base_module_t**) malloc (
    		mca_btl_sicortex_component.sicortex_num_btls * sizeof(mca_btl_base_module_t *));
    if( NULL == btls ){
	return NULL;
    }

    memcpy(btls, mca_btl_sicortex_component.sicortex_btls,
    	   mca_btl_sicortex_component.sicortex_num_btls * sizeof(mca_btl_sicortex_module_t *));
    *num_btl_modules = mca_btl_sicortex_component.sicortex_num_btls;
    return btls;
}

int mca_btl_sicortex_component_progress(void)
{
    int count = 0;
    size_t i;
    uint64_t *evt;

    for( i = 0; i < mca_btl_sicortex_component.sicortex_num_btls; i++ ) {
        mca_btl_sicortex_module_t* btl = mca_btl_sicortex_component.sicortex_btls[i];
        int pooling = 0;

        while( pooling++ < 5000 ) {
            evt = (uint64_t *) scdma_eq_next_event(btl->ctx);
            if( NULL != evt ) break;
        }
        if( NULL == evt ) continue;

	switch ((evt[0] >> 8) & 0xff) {

        case SCDMA_HW_EVENT_TYPE_RDT_FAULT: {
            uint64_t sw_bucket = evt[1];

            opal_output( 0, "Error RDT_FAULT sw_bucket %llx", (unsigned long long)sw_bucket );
            scdma_eq_processed_event(btl->ctx);
            break;
        }
        case SCDMA_HW_EVENT_TYPE_BDT_FAULT: {
            uint64_t sw_bucket = evt[1];

            opal_output( 0, "Error BDT_FAULT sw_bucket %llx", (unsigned long long)sw_bucket );
            scdma_eq_processed_event(btl->ctx);
            break;
        }
        case SCDMA_HW_EVENT_TYPE_SEG_ABORT: {
            uint64_t sw_bucket = evt[1];

            opal_output( 0, "Error SEG_ABORT sw_bucket %llx", (unsigned long long)sw_bucket );
            scdma_eq_processed_event(btl->ctx);
            break;
        }

	case SCDMA_HW_EVENT_TYPE_DEFERRED_CMD: {
	    void *cmd = (void *) scdma_cq_head_spinwait(btl->ctx);

	    memcpy(cmd, &evt[1], 0x78);
	    scdma_cq_post(btl->ctx);
            scdma_eq_processed_event(btl->ctx);
	    break;
	}

	case SCDMA_HW_EVENT_TYPE_RX_END_SEG: {
	    scdma_hw_cmd_t *cmd_2;
	    uint64_t swbucket = evt[1];

            scdma_eq_processed_event(btl->ctx);
	    cmd_2 = (scdma_hw_cmd_t *)scdma_cq_head_spinwait(btl->ctx);
	    cmd_2->header = scdma_cmd_header(SCDMA_HW_CMD_TYPE_SEND_EVENT, 0x18,
                                             btl->peers[0]->route->route_handles[btl->handle_count],
                                             btl->peers[0]->route->ports[btl->handle_count]);
	    btl->handle_count = (btl->handle_count +1) % SICORTEX_PORTS_NUM;
	    cmd_2->payload[0] = (EVENT_PUT << 8) + 0x18;
	    cmd_2->payload[1] = swbucket;
	    cmd_2->payload[2] = 0;
	    scdma_cq_post_fastpath(btl->ctx,cmd_2->header);
	    break;
	}

	/* Receive small message */
	case EVENT_SEND_IM: {
	    mca_btl_active_message_callback_t* reg;
	    mca_btl_sicortex_frag_t frag;

	    frag.base.des_dst = &frag.segment;
	    frag.base.des_dst->seg_addr.pval = (void*) (evt+1);
	    frag.base.des_dst->seg_len = (size_t)((evt[0] & SCDMA_HW_CMD_LEN_MASK)
                                                  - 2 * sizeof(uint64_t));
	    frag.base.des_dst_cnt = 1;
	    frag.tag = (mca_btl_base_tag_t)((evt[0] >> 16) & 0xff);
	    reg = mca_btl_base_active_message_trigger + frag.tag;
	    reg->cbfunc( &(btl->super), frag.tag, &(frag.base), reg->cbdata );
            scdma_eq_processed_event(btl->ctx);
	    count++;
	    break;
	}

        case EVENT_SEND_PUT_ALIGN: {
            /* There are at most 2 PUT_ALIGN per segment, one of the begining and
             * one for the end. As the DMA guarantee that the orders in each route and port
             * are completed in order, we don't have to care about these two PUT_ALIGN
             * as they will always be followed by a real PUT on the same route. When the
             * PUT complete, these two PUT_ALIGN have to be completed.
             */
            uint32_t length = ((evt[0] & SCDMA_HW_CMD_LEN_MASK) - 3 * sizeof(uint64_t));
            void* ptr = (void*)evt[1];
            memcpy( ptr, &(evt[2]), length );
            /*opal_output(0, "PUT_ALIGN ptr %p length %d\n", ptr, length );*/
            scdma_eq_processed_event(btl->ctx);
            break;
        }

	case EVENT_SEND_IM_PUT_POST: {
	    mca_btl_active_message_callback_t* reg;
	    mca_btl_sicortex_frag_t frag;

	    frag.base.des_dst = &frag.segment;
	    frag.base.des_dst->seg_addr.pval = (void*)((char*)scdma_heap_addr(btl->ctx) + SCDMA_USABLE_HEAP_OFFSET);
	    frag.base.des_dst->seg_len = (size_t)((evt[0]>> 32 & 0xffffffff));
	    frag.base.des_dst_cnt = 1;
	    frag.tag = (mca_btl_base_tag_t)((evt[0]>>16)& 0xff);
	    reg = mca_btl_base_active_message_trigger + frag.tag;
	    reg->cbfunc( &(btl->super), frag.tag, &(frag.base), reg->cbdata );
            scdma_eq_processed_event(btl->ctx);
	    count++;
	    break;
	}

        case EVENT_PUT: {
	    mca_btl_sicortex_frag_t *frag = (mca_btl_sicortex_frag_t *)(uintptr_t) evt[1];
            if( --frag->count == 0 ) {
            	int btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP );
            	count++;
            	frag->base.des_cbfunc( &(btl->super), frag->endpoint,&(frag->base), OMPI_SUCCESS );
            	if( btl_ownership ) {
                    MCA_BTL_SICORTEX_FRAG_RETURN(btl, frag);
            	}
            }
            scdma_eq_processed_event(btl->ctx);
	    break;
	}

	default:
	    opal_output(0, "Got a badley formatted event (evt[0] = 0x%llx)\n",
			(long long unsigned int)evt[0]);
            scdma_eq_processed_event(btl->ctx);
            sleep(5);
	    break;
	}
    }
    return count;
}



