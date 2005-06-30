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
#include "gm_config.h"
#include "include/constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"

#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/errmgr/errmgr.h"
#include "mca/mpool/base/base.h" 
#include "mca/mpool/gm/mpool_gm.h"
#include "btl_gm.h"
#include "btl_gm_frag.h"
#include "btl_gm_endpoint.h" 
#include "mca/btl/base/base.h" 
#include "datatype/convertor.h" 
#include "btl_gm_endpoint.h"


mca_btl_gm_component_t mca_btl_gm_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_0,

            "ib", /* MCA component name */
            1,  /* MCA component major version */
            0,  /* MCA component minor version */
            0,  /* MCA component release version */
            mca_btl_gm_component_open,  /* component open */
            mca_btl_gm_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_btl_gm_component_init,  
        mca_btl_gm_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_gm_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","ib",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_gm_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("btl","ib",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_gm_component_open(void)
{
    /* initialize state */
    mca_btl_gm_component.gm_num_btls=0;
    mca_btl_gm_component.gm_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_gm_component.gm_procs, ompi_list_t);
    OBJ_CONSTRUCT(&mca_btl_gm_component.gm_lock, ompi_mutex_t);

    /* register GM component parameters */
    mca_btl_gm_component.gm_free_list_num =
        mca_btl_gm_param_register_int ("free_list_num", 8);
    mca_btl_gm_component.gm_free_list_max =
        mca_btl_gm_param_register_int ("free_list_max", 1024);
    mca_btl_gm_component.gm_free_list_inc =
        mca_btl_gm_param_register_int ("free_list_inc", 32);
    mca_btl_gm_component.gm_mpool_name = 
        mca_btl_gm_param_register_string("mpool", "gm"); 

    /* register gm module parameters */
    mca_btl_gm_module.super.btl_exclusivity =
        mca_btl_gm_param_register_int ("exclusivity", 0);
    mca_btl_gm_module.super.btl_eager_limit = 
        mca_btl_gm_param_register_int ("first_frag_size", 64*1024) - sizeof(mca_btl_base_header_t);
    mca_btl_gm_module.super.btl_min_send_size =
        mca_btl_gm_param_register_int ("min_send_size", 64*1024) - sizeof(mca_btl_base_header_t);
    mca_btl_gm_module.super.btl_max_send_size =
        mca_btl_gm_param_register_int ("max_send_size", 128*1024) - sizeof(mca_btl_base_header_t);
    mca_btl_gm_module.super.btl_min_rdma_size = 
        mca_btl_gm_param_register_int("min_rdma_size", 1024*1024); 
    mca_btl_gm_module.super.btl_max_rdma_size = 
        mca_btl_gm_param_register_int("max_rdma_size", 1024*1024); 
#if OMPI_MCA_BTL_GM_HAVE_RDMA_PUT
    mca_btl_gm_module.super.btl_flags  = 
        mca_btl_gm_param_register_int("flags", MCA_BTL_FLAGS_RDMA); 
#else
    mca_btl_gm_module.super.btl_flags  = 
        mca_btl_gm_param_register_int("flags", MCA_BTL_FLAGS_SEND); 
#endif
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_gm_component_close(void)
{
    return OMPI_SUCCESS;
}


/**
 * Initialize module instance
 */

static int
mca_btl_gm_module_init (mca_btl_gm_module_t * btl)
{
    mca_mpool_base_resources_t resources;
    size_t i;
    int rc;

    /* initialize objects */
    OBJ_CONSTRUCT(&btl->gm_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->gm_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->gm_frag_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->gm_pending, ompi_list_t);
    OBJ_CONSTRUCT(&btl->gm_lock, ompi_mutex_t);
                                                                                                  
    /* query nic tokens */
    btl->gm_num_send_tokens = gm_num_send_tokens (btl->gm_port);
    btl->gm_max_send_tokens = btl->gm_num_send_tokens;
    btl->gm_num_recv_tokens = gm_num_receive_tokens (btl->gm_port);
    btl->gm_max_recv_tokens = btl->gm_num_recv_tokens;
                                                                                                  
    /* initialize memory pool */
    resources.gm_port = btl->gm_port;
    btl->gm_mpool = mca_mpool_base_module_create(
        mca_btl_gm_component.gm_mpool_name,
        &btl->super,
        &resources);
    if(NULL == btl->gm_mpool) {
        ompi_output (0, "[%s:%d] unable to initialize mpool", __FILE__, __LINE__);
        return OMPI_ERROR;
    }
 
    /* initialize free lists */
    ompi_free_list_init( &btl->gm_frag_eager,
                         sizeof (mca_btl_gm_frag_eager_t),
                         OBJ_CLASS (mca_btl_gm_frag_eager_t),
                         mca_btl_gm_component.gm_free_list_num,  
                         mca_btl_gm_component.gm_free_list_max, 
                         mca_btl_gm_component.gm_free_list_inc,
                         btl->gm_mpool ); 

    ompi_free_list_init( &btl->gm_frag_max,
                         sizeof (mca_btl_gm_frag_max_t),
                         OBJ_CLASS (mca_btl_gm_frag_max_t),
                         mca_btl_gm_component.gm_free_list_num,  
                         mca_btl_gm_component.gm_free_list_max, 
                         mca_btl_gm_component.gm_free_list_inc,
                         btl->gm_mpool ); 

    ompi_free_list_init( &btl->gm_frag_user,
                         sizeof (mca_btl_gm_frag_user_t),
                         OBJ_CLASS (mca_btl_gm_frag_user_t),
                         mca_btl_gm_component.gm_free_list_num,  
                         mca_btl_gm_component.gm_free_list_max, 
                         mca_btl_gm_component.gm_free_list_inc,
                         btl->gm_mpool ); 

    /* post receive buffers */
    for(i=0; i<mca_btl_gm_component.gm_num_high_priority; i++) {
        mca_btl_gm_frag_t* frag;
        MCA_BTL_GM_FRAG_ALLOC_EAGER(btl, frag, rc);
        if(NULL == frag) {
            return rc;
        }
        gm_provide_receive_buffer(btl->gm_port, frag->hdr, frag->size, GM_HIGH_PRIORITY);
    }
    for(i=mca_btl_gm_component.gm_num_high_priority; i<btl->gm_max_recv_tokens; i++) {
        mca_btl_gm_frag_t* frag;
        MCA_BTL_GM_FRAG_ALLOC_MAX(btl, frag, rc);
        if(NULL == frag) {
            return rc;
        }
        gm_provide_receive_buffer(btl->gm_port, frag->hdr, frag->size, GM_LOW_PRIORITY);
    }

    /* enable rdma */
    if( GM_SUCCESS != gm_allow_remote_memory_access (btl->gm_port) ) {
        ompi_output (0, "[%s:%d] unable to allow remote memory access", __FILE__, __LINE__);
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}


/* 
 * Scan all ports on the boards. As it's difficult to find the total number of boards
 * so we use a predefined maximum.  
 */
static int mca_btl_gm_discover( void )
{
    uint32_t board_no;
    uint32_t  port_no;
    uint32_t local_id;
    struct gm_port* gm_port;
#if GM_API_VERSION > 0x200
    uint32_t global_id;
#else
    char  global_id[GM_MAX_HOST_NAME_LEN];
#endif  /* GM_API_VERSION > 0x200 */
    int rc;

    for( board_no = 0; board_no < mca_btl_gm_component.gm_max_boards; board_no++ ) {
        mca_btl_gm_module_t *btl;

        /* open the first available gm port for this board  */
        for( port_no = 2; port_no < mca_btl_gm_component.gm_max_ports; port_no++ ) {
            if (3 == port_no) {
                continue;  /* port 0,1,3 reserved  */
            } else if (GM_SUCCESS ==
                       gm_open(&gm_port, board_no, port_no,
                               mca_btl_gm_component.gm_port_name,
                               GM_API_VERSION) ) {
            break;
            }
        }
        if( port_no == mca_btl_gm_component.gm_max_ports ) {
            continue;
        }

        /*  Get node local Id */
        if( GM_SUCCESS != gm_get_node_id( gm_port, &local_id) ) {
            ompi_output (0, " failure to get local_id \n");
            continue;
        }
        /* Gather an unique id for the node */
#if GM_API_VERSION > 0x200
        if (GM_SUCCESS != gm_node_id_to_global_id( gm_port, local_id, &global_id) ) {
            ompi_output (0, "[%s:%d] Unable to get my GM global unique id", __FILE__, __LINE__);
            continue;
        }
#else
        if( GM_SUCCESS != gm_get_host_name( gm_port, global_id ) ) {
            ompi_output( 0, "[%s:%d] Unable to get the GM host name\n", __FILE__, __LINE__);
            continue;
        }
#endif  /* GM_API_VERSION > 0x200 */

        /* create the btl module */
        btl = (mca_btl_gm_module_t *)malloc( sizeof(mca_btl_gm_module_t) );
        if (NULL == btl) {
            ompi_output( 0, "[%s:%d] out of resources", __FILE__, __LINE__);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        /* copy the basic informations in the new PTL */
        memcpy (btl, &mca_btl_gm_module, sizeof(mca_btl_gm_module_t) );

        /* setup local address */
        btl->gm_port           = gm_port;
        btl->gm_addr.port_id   = port_no;
        btl->gm_addr.local_id  = local_id;
#if GM_API_VERSION > 0x200
        btl->gm_addr.global_id = global_id;
#else
        strncpy( btl->gm_addr.global_id, global_id, GM_MAX_HOST_NAME_LEN );
#endif  /* GM_API_VERSION > 0x200 */

        if((rc = mca_btl_gm_module_init(btl)) != OMPI_SUCCESS) {
            ompi_output(0, "[%s:%d] unable to initialze gm port", __FILE__, __LINE__);
            return rc;
        }

        /* everything is OK let's mark it as usable and go to the next one */
        mca_btl_gm_component.gm_btls[mca_btl_gm_component.gm_num_btls++];
        if(mca_btl_gm_component.gm_num_btls >= mca_btl_gm_component.gm_max_btls ) {
            break;
        }
    }
    return OMPI_SUCCESS;
}



/*
 *  Register GM component addressing information. The MCA framework
 *  will make this available to all peers.
 */

static int
mca_btl_gm_modex_send(void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_btl_gm_addr_t *addrs;

    size = mca_btl_gm_component.gm_num_btls * sizeof (mca_btl_gm_addr_t);
    addrs = (mca_btl_gm_addr_t *)malloc (size);
    if (NULL == addrs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < mca_btl_gm_component.gm_num_btls; i++) {
        mca_btl_gm_module_t *btl = mca_btl_gm_component.gm_btls[i];
        addrs[i] = btl->gm_addr;
    }
    rc = mca_base_modex_send (&mca_btl_gm_component.super.btl_version, addrs, size);
    free (addrs);
    return rc;
}
                                                                                                                

/*
 * Initialize the GM component,
 * check how many boards are available and open ports on them.
 */
                                                                                                                
mca_btl_base_module_t **
mca_btl_gm_component_init (int *num_btl_modules,
                           bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    mca_btl_base_module_t **btls;
    *num_btl_modules = 0;

    /* s try to initialize GM */
    if( GM_SUCCESS != gm_init() ) {
        ompi_output( 0, "[%s:%d] error in initializing the gm library\n", __FILE__, __LINE__ );
        return NULL;
    }

    /* First discover all available boards. For each board we create a unique BTL */
    mca_btl_gm_component.gm_btls = malloc( mca_btl_gm_component.gm_max_btls * sizeof (mca_btl_gm_module_t *));
    if (NULL == mca_btl_gm_component.gm_btls) {
        ompi_output( 0, "[%s:%d] out of resources.", __FILE__, __LINE__ );
        return NULL;
    }

    mca_btl_gm_component.gm_num_btls = mca_btl_gm_discover( );

    /* initialize gm */
    if (OMPI_SUCCESS != mca_btl_gm_discover()) {
        return NULL;
    }

    /* publish GM parameters with the MCA framework */
    if (OMPI_SUCCESS != mca_btl_gm_modex_send()) {
        return NULL;
    }

    /* return array of BTLs */
    btls = (mca_btl_base_module_t**) malloc (
                mca_btl_gm_component.gm_num_btls * sizeof(mca_btl_base_module_t *));
    if (NULL == btls) {
        return NULL;
    }

    memcpy(btls, mca_btl_gm_component.gm_btls,
           mca_btl_gm_component.gm_num_btls * sizeof(mca_btl_gm_module_t *));
    *num_btl_modules = mca_btl_gm_component.gm_num_btls;
    return btls;
}
                                                                                                                

/*
 *  GM component progress.
 */


int mca_btl_gm_component_progress()
{
   int count = 0;
   size_t i;

    for( i = 0; i < mca_btl_gm_component.gm_num_btls; ) {
        mca_btl_gm_module_t* btl = mca_btl_gm_component.gm_btls[i];
        gm_recv_event_t* event = gm_receive(btl->gm_port);
        unsigned char* buffer = (unsigned char*)gm_ntohp(event->recv.buffer);
        mca_btl_gm_frag_t* frag = (mca_btl_gm_frag_t*)(buffer - sizeof(mca_btl_gm_frag_t));
        mca_btl_base_header_t* hdr;
        uint32_t priority = GM_HIGH_PRIORITY;

        /* If there are no receive events just skip the function call */
        switch(gm_ntohc(event->recv.type)) {
            case GM_FAST_RECV_EVENT:
            case GM_FAST_PEER_RECV_EVENT:
                priority = GM_LOW_PRIORITY;
            case GM_FAST_HIGH_RECV_EVENT:
            case GM_FAST_HIGH_PEER_RECV_EVENT:
                {
                mca_btl_base_recv_reg_t* reg;
                hdr = (mca_btl_base_header_t *)gm_ntohp(event->recv.message);
                reg = &btl->gm_reg[hdr->tag];
                reg->cbfunc(&btl->super, hdr->tag, &frag->base, reg->cbdata);
                count++;
                break;
                }
            case GM_RECV_EVENT:
            case GM_PEER_RECV_EVENT:
                priority = GM_LOW_PRIORITY;
            case GM_HIGH_RECV_EVENT:
            case GM_HIGH_PEER_RECV_EVENT:
                {
                mca_btl_base_recv_reg_t* reg;
                hdr = (mca_btl_base_header_t*)buffer;
                reg = &btl->gm_reg[hdr->tag];
                reg->cbfunc(&btl->super, hdr->tag, &frag->base, reg->cbdata);
                count++;
                break;
                }
            case GM_NO_RECV_EVENT:
                i++;
                break;
            default:
                gm_unknown(btl->gm_port, event);
                break;
        }
        gm_provide_receive_buffer(btl->gm_port, buffer, frag->size, priority);
    }
    return count;
}

