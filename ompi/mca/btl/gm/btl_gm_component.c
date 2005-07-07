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
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"

#include "mca/base/mca_base_param.h"
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

            "gm", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
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
    int id = mca_base_param_register_string("btl","gm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_gm_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("btl","gm",param_name,NULL,default_value);
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
    OBJ_CONSTRUCT(&mca_btl_gm_component.gm_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_gm_component.gm_lock, opal_mutex_t);

    /* register GM component parameters */
    mca_btl_gm_component.gm_free_list_num =
        mca_btl_gm_param_register_int ("free_list_num", 8);
    mca_btl_gm_component.gm_free_list_max =
        mca_btl_gm_param_register_int ("free_list_max", -1);
    mca_btl_gm_component.gm_free_list_inc =
        mca_btl_gm_param_register_int ("free_list_inc", 8);
    mca_btl_gm_component.gm_debug = 
        mca_btl_gm_param_register_int("debug", 0); 
    mca_btl_gm_component.gm_mpool_name = 
        mca_btl_gm_param_register_string("mpool", "gm"); 
    mca_btl_gm_component.gm_max_ports = 
        mca_btl_gm_param_register_int("max_ports", 16); 
    mca_btl_gm_component.gm_max_boards = 
        mca_btl_gm_param_register_int("max_boards", 4); 
    mca_btl_gm_component.gm_max_btls = 
        mca_btl_gm_param_register_int("max_modules", 4); 
    mca_btl_gm_component.gm_num_high_priority = 
        mca_btl_gm_param_register_int("num_high_priority", 8); 
    mca_btl_gm_component.gm_port_name=
        mca_btl_gm_param_register_string("port_name", "OMPI"); 

    /* register gm module parameters */
    mca_btl_gm_module.super.btl_exclusivity =
        mca_btl_gm_param_register_int ("exclusivity", 0);
    mca_btl_gm_module.super.btl_eager_limit = 
        mca_btl_gm_param_register_int ("eager_limit", 32*1024);
    mca_btl_gm_module.super.btl_min_send_size =
        mca_btl_gm_param_register_int ("min_send_size", 32*1024);
    mca_btl_gm_module.super.btl_max_send_size =
        mca_btl_gm_param_register_int ("max_send_size", 64*1024);
    mca_btl_gm_module.super.btl_min_rdma_size = 
        mca_btl_gm_param_register_int("min_rdma_size", 1024*1024); 
    mca_btl_gm_module.super.btl_max_rdma_size = 
        mca_btl_gm_param_register_int("max_rdma_size", 1024*1024); 
#if OMPI_MCA_BTL_GM_SUPPORT_REGISTERING && OMPI_MCA_BTL_GM_HAVE_RDMA_PUT 
    mca_btl_gm_module.super.btl_flags  = 
        mca_btl_gm_param_register_int("flags", MCA_BTL_FLAGS_RDMA); 
#else
    mca_btl_gm_module.super.btl_flags = MCA_BTL_FLAGS_SEND;
#endif

    /* compute the eager frag size */
    mca_btl_gm_component.gm_eager_frag_size =
        gm_min_size_for_length(mca_btl_gm_module.super.btl_eager_limit) -
        sizeof(mca_btl_base_header_t);
    mca_btl_gm_module.super.btl_eager_limit = 
        gm_max_length_for_size(mca_btl_gm_component.gm_eager_frag_size);
 
    /* compute the max frag size */
    mca_btl_gm_component.gm_max_frag_size = 
        gm_min_size_for_length(mca_btl_gm_module.super.btl_max_send_size);
    mca_btl_gm_module.super.btl_max_send_size = 
        gm_max_length_for_size(mca_btl_gm_component.gm_max_frag_size) -
        sizeof(mca_btl_base_header_t);
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
    size_t num_high_priority;
    size_t i;
    int rc;

    /* initialize objects */
    OBJ_CONSTRUCT(&btl->gm_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->gm_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->gm_frag_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&btl->gm_pending, opal_list_t);
    OBJ_CONSTRUCT(&btl->gm_lock, opal_mutex_t);
                                                                                                  
    /* query nic tokens */
    btl->gm_num_send_tokens = gm_num_send_tokens (btl->port);
    btl->gm_max_send_tokens = btl->gm_num_send_tokens;
    btl->gm_num_recv_tokens = gm_num_receive_tokens (btl->port);
    btl->gm_max_recv_tokens = btl->gm_num_recv_tokens;

    /* dont allow high priority to exceed 1/2 of available recv tokens */
    num_high_priority = mca_btl_gm_component.gm_num_high_priority;
    if(num_high_priority > (btl->gm_num_recv_tokens >> 1)) {
        num_high_priority = btl->gm_num_recv_tokens >> 1;
    }
                                                                                                  
    /* initialize memory pool */
    resources.port = btl->port;
    btl->gm_mpool = mca_mpool_base_module_create(
        mca_btl_gm_component.gm_mpool_name,
        &btl->super,
        &resources);
    if(NULL == btl->gm_mpool) {
        opal_output (0, "[%s:%d] unable to initialize mpool", __FILE__, __LINE__);
        return OMPI_ERROR;
    }
 
    /* initialize free lists */
    ompi_free_list_init( &btl->gm_frag_eager,
                         sizeof (mca_btl_gm_frag_eager_t) + (1 << mca_btl_gm_component.gm_eager_frag_size),
                         OBJ_CLASS (mca_btl_gm_frag_eager_t),
                         btl->gm_max_send_tokens,  
                         mca_btl_gm_component.gm_free_list_max, 
                         mca_btl_gm_component.gm_free_list_inc,
                         btl->gm_mpool ); 

    ompi_free_list_init( &btl->gm_frag_max,
                         sizeof (mca_btl_gm_frag_max_t) + (1 << mca_btl_gm_component.gm_max_frag_size),
                         OBJ_CLASS (mca_btl_gm_frag_max_t),
                         btl->gm_max_recv_tokens,
                         mca_btl_gm_component.gm_free_list_max, 
                         mca_btl_gm_component.gm_free_list_inc,
                         btl->gm_mpool ); 

    ompi_free_list_init( &btl->gm_frag_user,
                         sizeof (mca_btl_gm_frag_user_t),
                         OBJ_CLASS (mca_btl_gm_frag_user_t),
                         mca_btl_gm_component.gm_free_list_num,  
                         mca_btl_gm_component.gm_free_list_max, 
                         mca_btl_gm_component.gm_free_list_inc,
                         NULL );


    /* post receive buffers */
    for(i=0; i<num_high_priority; i++) {
        mca_btl_gm_frag_t* frag;
        MCA_BTL_GM_FRAG_ALLOC_EAGER(btl, frag, rc);
        if(NULL == frag) {
            return rc;
        }
        frag->base.des_src = NULL;
        frag->base.des_src_cnt = 0;
        frag->base.des_dst = &frag->segment;
        frag->base.des_dst_cnt = 1;
        gm_provide_receive_buffer(btl->port, frag->hdr, frag->size, GM_LOW_PRIORITY);
    }

    for(i=mca_btl_gm_component.gm_num_high_priority; i<btl->gm_max_recv_tokens; i++) {
        mca_btl_gm_frag_t* frag;
        MCA_BTL_GM_FRAG_ALLOC_MAX(btl, frag, rc);
        if(NULL == frag) {
            return rc;
        }
        frag->base.des_src = NULL;
        frag->base.des_src_cnt = 0;
        frag->base.des_dst = &frag->segment;
        frag->base.des_dst_cnt = 1;
        gm_provide_receive_buffer(btl->port, frag->hdr, frag->size, GM_LOW_PRIORITY);
    }

    /* enable rdma */
    if( GM_SUCCESS != gm_allow_remote_memory_access (btl->port) ) {
        opal_output (0, "[%s:%d] unable to allow remote memory access", __FILE__, __LINE__);
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
    uint32_t node_id;
    struct gm_port* port;
#if GM_API_VERSION > 0x200
    uint32_t global_id;
#else
    char  global_id[GM_MAX_HOST_NAME_LEN];
#endif  /* GM_API_VERSION > 0x200 */
    int rc;

    for( board_no = 0; board_no < mca_btl_gm_component.gm_max_boards; board_no++ ) {
        mca_btl_gm_module_t *btl;

        /* open the first available gm port for this board  */
        for( port_no = 4; port_no < mca_btl_gm_component.gm_max_ports; port_no++ ) {
            if (3 == port_no) {
                continue;  /* port 0,1,3 reserved  */
            } else if (GM_SUCCESS == gm_open(&port, board_no, port_no,
                mca_btl_gm_component.gm_port_name, GM_API_VERSION) ) {
                break;
            }
        }
        if( port_no == mca_btl_gm_component.gm_max_ports ) {
            continue;
        }

        /*  Get node local Id */
        if( GM_SUCCESS != gm_get_node_id( port, &node_id) ) {
            opal_output (0, " failure to get node_id \n");
            continue;
        }
        /* Gather an unique id for the node */
#if GM_API_VERSION > 0x200
        if (GM_SUCCESS != gm_node_id_to_global_id( port, node_id, &global_id) ) {
            opal_output (0, "[%s:%d] Unable to get my GM global unique id", __FILE__, __LINE__);
            continue;
        }
#else
        if( GM_SUCCESS != gm_get_host_name( port, global_id ) ) {
            opal_output( 0, "[%s:%d] Unable to get the GM host name\n", __FILE__, __LINE__);
            continue;
        }
#endif  /* GM_API_VERSION > 0x200 */

        /* create the btl module */
        btl = (mca_btl_gm_module_t *)malloc( sizeof(mca_btl_gm_module_t) );
        if (NULL == btl) {
            opal_output( 0, "[%s:%d] out of resources", __FILE__, __LINE__);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        /* copy the basic informations into the new BTL */
        memcpy (btl, &mca_btl_gm_module, sizeof(mca_btl_gm_module_t));

        /* setup local address */
        btl->port = port;
        btl->gm_addr.port_id  = port_no;
        btl->gm_addr.node_id = node_id;
#if GM_API_VERSION > 0x200
        btl->gm_addr.global_id = global_id;
#else
        strncpy( btl->gm_addr.global_id, global_id, GM_MAX_HOST_NAME_LEN );
#endif  /* GM_API_VERSION > 0x200 */

        if(mca_btl_gm_component.gm_debug > 0) {
            opal_output(0, "[%d,%d,%d] gm_port %08X, board %lu, global %lu node %lu port %lu\n", 
                ORTE_NAME_ARGS(orte_process_info.my_name), port, board_no, global_id, node_id, port_no);
        }

        if((rc = mca_btl_gm_module_init(btl)) != OMPI_SUCCESS) {
            opal_output(0, "[%s:%d] unable to initialze gm port", __FILE__, __LINE__);
            return rc;
        }

        /* everything is OK let's mark it as usable and go to the next one */
        mca_btl_gm_component.gm_btls[mca_btl_gm_component.gm_num_btls] = btl;
        if(++mca_btl_gm_component.gm_num_btls >= mca_btl_gm_component.gm_max_btls ) {
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

    /* try to initialize GM */
    if( GM_SUCCESS != gm_init() ) {
        opal_output( 0, "[%s:%d] error in initializing the gm library\n", __FILE__, __LINE__ );
        return NULL;
    }

    /* First discover all available boards. For each board we create a unique BTL */
    mca_btl_gm_component.gm_btls = malloc( mca_btl_gm_component.gm_max_btls * sizeof (mca_btl_gm_module_t *));
    if (NULL == mca_btl_gm_component.gm_btls) {
        opal_output( 0, "[%s:%d] out of resources.", __FILE__, __LINE__ );
        return NULL;
    }

    /* initialize gm */
    if (OMPI_SUCCESS != mca_btl_gm_discover()) {
        return NULL;
    }
    if (mca_btl_gm_component.gm_num_btls == 0) {
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
        gm_recv_event_t* event = gm_receive(btl->port);
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
                frag->segment.seg_addr.pval = (hdr+1);
                frag->segment.seg_len = gm_ntohl(event->recv.length) - sizeof(mca_btl_base_header_t);
                reg = &btl->gm_reg[hdr->tag];
                reg->cbfunc(&btl->super, hdr->tag, &frag->base, reg->cbdata);
                gm_provide_receive_buffer(btl->port, buffer, frag->size, priority);
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
                frag->segment.seg_addr.pval = (hdr+1);
                frag->segment.seg_len = gm_ntohl(event->recv.length) - sizeof(mca_btl_base_header_t);
                reg = &btl->gm_reg[hdr->tag];
                reg->cbfunc(&btl->super, hdr->tag, &frag->base, reg->cbdata);
                gm_provide_receive_buffer(btl->port, buffer, frag->size, priority);
                count++;
                break;
                }
            case GM_NO_RECV_EVENT:
                i++;
                break;
            default:
                gm_unknown(btl->port, event);
                break;
        }
    }
    return count;
}

