/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include "include/constants.h"
#include "util/output.h"
#include "mca/ptl/ptl.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_gm.h"
#include "ptl_gm_priv.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_sendfrag.h"

mca_ptl_gm_component_t mca_ptl_gm_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */
        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */
            MCA_PTL_BASE_VERSION_1_0_0,
            "gm",                     /* MCA component name */
            1,                        /* MCA component major version */
            0,                        /* MCA component minor version */
            0,                        /* MCA component release version */
            mca_ptl_gm_component_open,   /* component open */
            mca_ptl_gm_component_close   /* component close */
        }
        ,
        /* Next the MCA v1.0.0 component meta data */
        {
           /* Whether the component is checkpointable or not */
            false
        },
        mca_ptl_gm_component_init,
        mca_ptl_gm_component_control,
        mca_ptl_gm_component_progress
    }
};

/*
 * utility routines for parameter registration
 */

static inline char *
mca_ptl_gm_param_register_string( const char *param_name,
                                  const char *default_value )
{
    char *param_value;
    int  id = mca_base_param_register_string( "ptl", "gm", param_name, NULL,
					      default_value) ;
    mca_base_param_lookup_string (id, &param_value);
    return param_value;
}

static inline int
mca_ptl_gm_param_register_int( const char *param_name, int default_value )
{
    int id =
        mca_base_param_register_int ("ptl", "gm", param_name, NULL,
                                     default_value);
    int param_value = default_value;
    mca_base_param_lookup_int (id, &param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int
mca_ptl_gm_component_open(void)
{
    /* initialize state */
    mca_ptl_gm_component.gm_ptl_modules = NULL;
    mca_ptl_gm_component.gm_num_ptl_modules = 0;

    /* initialize objects */
    OBJ_CONSTRUCT (&mca_ptl_gm_component.gm_lock, ompi_mutex_t);
    OBJ_CONSTRUCT (&mca_ptl_gm_component.gm_procs, ompi_list_t);
    OBJ_CONSTRUCT (&mca_ptl_gm_component.gm_send_req, ompi_list_t);

    /* register GM component parameters */
    mca_ptl_gm_component.gm_port_name =
        mca_ptl_gm_param_register_string( "port_name", "OMPI_GM" );
    mca_ptl_gm_component.gm_max_port_number =
        mca_ptl_gm_param_register_int ("max_ports_number", 16 );
    mca_ptl_gm_component.gm_max_boards_number =
        mca_ptl_gm_param_register_int ("max_boards_number", 4 );
    mca_ptl_gm_component.gm_max_ptl_modules = 
	mca_ptl_gm_param_register_int( "max_ptl_modules", 1 );

    mca_ptl_gm_component.gm_segment_size = 
        mca_ptl_gm_param_register_int( "segment_size", 16 * 1024 ); /* 16K by default */
    mca_ptl_gm_module.super.ptl_first_frag_size =
        mca_ptl_gm_param_register_int ("first_frag_size", mca_ptl_gm_component.gm_segment_size - sizeof(mca_ptl_base_rendezvous_header_t) );
    mca_ptl_gm_module.super.ptl_min_frag_size =
        mca_ptl_gm_param_register_int ("min_frag_size", 1<<16);
    mca_ptl_gm_module.super.ptl_max_frag_size =
        mca_ptl_gm_param_register_int ("max_frag_size", 256 * 1024 * 1024);
    /* Parameters setting the message limits. */
    mca_ptl_gm_component.gm_eager_limit = 
        mca_ptl_gm_param_register_int( "eager_limit", 2 * mca_ptl_gm_component.gm_segment_size );
    mca_ptl_gm_component.gm_rdma_frag_size =
        mca_ptl_gm_param_register_int ("rdma_frag_size", 512 * 1024);
    
    mca_ptl_gm_component.gm_free_list_num =
        mca_ptl_gm_param_register_int ("free_list_num", 256);
    mca_ptl_gm_component.gm_free_list_inc =
        mca_ptl_gm_param_register_int ("free_list_inc", 32);

    return OMPI_SUCCESS;
}

/*
 * component close 
 */
int mca_ptl_gm_component_close (void)
{
    uint32_t index;
    mca_ptl_base_module_t* ptl;

    for( index = 0; index < mca_ptl_gm_component.gm_num_ptl_modules; index++ ) {
        ptl = (mca_ptl_base_module_t*)mca_ptl_gm_component.gm_ptl_modules[index];
        if( NULL != ptl )
            ptl->ptl_finalize( ptl );
    }
    mca_ptl_gm_component.gm_num_ptl_modules = 0;

    if (NULL != mca_ptl_gm_component.gm_ptl_modules)
        free (mca_ptl_gm_component.gm_ptl_modules);

    OBJ_DESTRUCT (&mca_ptl_gm_component.gm_procs);
    OBJ_DESTRUCT (&mca_ptl_gm_component.gm_send_req);
    OBJ_DESTRUCT (&mca_ptl_gm_component.gm_lock);

    return OMPI_SUCCESS;
}

/*
 *  Create a ptl instance and add to components list.
 */

static int
mca_ptl_gm_create( mca_ptl_gm_module_t** pptl )
{
    mca_ptl_gm_module_t *ptl;

    ptl = (mca_ptl_gm_module_t *)malloc( sizeof(mca_ptl_gm_module_t) );
    if (NULL == ptl) {
        ompi_output( 0, " ran out of resource to allocate ptl_instance \n" );
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    /* copy the basic informations in the new PTL */
    memcpy (ptl, &mca_ptl_gm_module, sizeof(mca_ptl_gm_module_t) );
#if OMPI_HAVE_POSIX_THREADS
    ptl->thread.t_handle = (pthread_t)-1;
#endif  /* OMPI_HAVE_POSIX_THREADS */
    *pptl = ptl;
    
    return OMPI_SUCCESS;
}

/*
 *  Register GM component addressing information. The MCA framework
 *  will make this available to all peers. 
 */
static int
mca_ptl_gm_module_store_data_toexchange (void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_ptl_gm_addr_t *addrs;

    size = mca_ptl_gm_component.gm_num_ptl_modules * sizeof (mca_ptl_gm_addr_t);
    addrs = (mca_ptl_gm_addr_t *)malloc (size);

    if (NULL == addrs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < mca_ptl_gm_component.gm_num_ptl_modules; i++) {
        mca_ptl_gm_module_t *ptl = mca_ptl_gm_component.gm_ptl_modules[i];
        addrs[i].local_id = ptl->local_id;
        addrs[i].global_id = ptl->global_id;
        addrs[i].port_id = ptl->port_id;
    }
    rc = mca_base_modex_send (&mca_ptl_gm_component.super.ptlm_version, addrs,
                              size);
    free (addrs);
    return rc;
}

#if OMPI_HAVE_POSIX_THREADS
static void*
mca_ptl_gm_thread_progress( ompi_thread_t* thread )
{
    gm_recv_event_t *event;
    mca_ptl_gm_module_t* ptl = thread->t_arg;

    /* This thread enter in a cancel enabled state */
    pthread_setcancelstate( PTHREAD_CANCEL_ENABLE, NULL );
    pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, NULL );

    while(1) {
	event = gm_blocking_receive(ptl->gm_port);
	if( GM_NO_RECV_EVENT != gm_ntohc(event->recv.type) )
	    mca_ptl_gm_analyze_recv_event( ptl, event );
    }
    return PTHREAD_CANCELED;
}
#endif  /* OMPI_HAVE_POSIX_THREADS */

/* Scan all ports on the boards. As it's difficult to find the total number of boards
 * we use a predefined maximum.
 * Return the number of discovered boards where opening a port was a succesfull operation.
 */
static int32_t
mca_ptl_gm_discover_boards( mca_ptl_gm_module_t** pptl,
			    uint32_t max_ptls, uint32_t max_boards, uint32_t max_port )
{
    uint32_t        board_no, port_no;
    struct gm_port* gm_port;
    uint32_t        index = 0;
    uint32_t        local_id, global_id;

    for( board_no = 0; board_no < max_boards; board_no++ ) {

        /* open the first available gm port for this board  */
        for( port_no = 2; port_no < max_port; port_no++ ) {
            if (port_no == 3) continue;  /* port 0,1,3 reserved  */
            if( GM_SUCCESS == gm_open( &gm_port, board_no, port_no,
				       mca_ptl_gm_component.gm_port_name, GM_API_VERSION_2_0) )
		break;
        }
        if( port_no == max_port ) continue;

        /*  Get node local Id */
        if( GM_SUCCESS != gm_get_node_id( gm_port, &local_id) ) {
            ompi_output (0, " failure to get local_id \n");
            continue;
        }
	
        /* Convert local id to global id */
        if (GM_SUCCESS != gm_node_id_to_global_id( gm_port, local_id, &global_id) ) {
            ompi_output (0, " Error: Unable to get my GM global id \n");
            continue;
        }

	/* Create the ptl. If fail return the number of already created */
	if( OMPI_SUCCESS != mca_ptl_gm_create( &(pptl[index]) ) )
	    return index;

        pptl[index]->port_id   = port_no;
        pptl[index]->gm_port   = gm_port;
	pptl[index]->local_id  = local_id;
	pptl[index]->global_id = global_id;

	/* everything is OK let's mark it as usable and go to the next one */
	if( (++index) >= max_ptls ) break;
    }

    return index;
}

static inline int
mca_ptl_gm_init_sendrecv (mca_ptl_gm_module_t * ptl)
{
    uint32_t i;
    mca_ptl_gm_send_frag_t *sfragment;
    mca_ptl_gm_recv_frag_t *free_rfragment;

    ptl->num_send_tokens = gm_num_send_tokens (ptl->gm_port);
    ptl->max_send_tokens = ptl->num_send_tokens;
    ptl->num_send_tokens -= PTL_GM_ADMIN_SEND_TOKENS;
    ptl->num_recv_tokens = gm_num_receive_tokens (ptl->gm_port);
    ptl->max_recv_tokens = ptl->num_recv_tokens;
    ptl->num_recv_tokens -= PTL_GM_ADMIN_RECV_TOKENS;

    /****************SEND****************************/
    /* construct a list of send fragments */
    OBJ_CONSTRUCT (&(ptl->gm_send_frags), ompi_free_list_t);
    OBJ_CONSTRUCT (&(ptl->gm_send_dma_frags), ompi_free_list_t);
    OBJ_CONSTRUCT (&(ptl->gm_send_frags_queue), ompi_list_t);

    /* We need a free list just to handle the send fragment that we provide.
     * Just to make sure that we dont waste memory, we dont allow this list to
     * grow anymore.
     */
    ompi_free_list_init( &(ptl->gm_send_frags),
                         sizeof (mca_ptl_gm_send_frag_t),
                         OBJ_CLASS (mca_ptl_gm_send_frag_t),
                         0,  /* do not allocate any items I'll provide them */
                         0,  /* maximum number of list allocated elements will be zero */
                         0,
                         NULL ); /* not using mpool */
    /* A free list containing all DMA allocate memory.
     * This free list does not have the right to allocate any new item
     * as they should be allocated with a special GM function.
     */
    ompi_free_list_init( &(ptl->gm_send_dma_frags),
                         mca_ptl_gm_component.gm_segment_size,
                         OBJ_CLASS (ompi_list_item_t),
                         0,  /* do not allocate any items I'll provide them */
                         0,  /* maximum number of list allocated elements will be zero */
                         0,
                         NULL ); /* not using mpool */

    /* allocate the elements */
    sfragment = (mca_ptl_gm_send_frag_t *)calloc( ptl->num_send_tokens, sizeof(mca_ptl_gm_send_frag_t) );
    ptl->gm_send_fragments = sfragment;
    /* allocate the registered memory */
    ptl->gm_send_dma_memory = gm_dma_malloc( ptl->gm_port,
                                             (mca_ptl_gm_component.gm_segment_size * ptl->num_send_tokens) + GM_PAGE_LEN );
    if( NULL == ptl->gm_send_dma_memory ) {
	ompi_output( 0, "unable to allocate registered memory\n" );
	return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i = 0; i < ptl->num_send_tokens; i++) {
	sfragment->send_buf = NULL;
	OMPI_FREE_LIST_RETURN( &(ptl->gm_send_frags), (ompi_list_item_t *)sfragment );
	OMPI_FREE_LIST_RETURN( &(ptl->gm_send_dma_frags),
                               (ompi_list_item_t *)((char*)ptl->gm_send_dma_memory +
						    i * mca_ptl_gm_component.gm_segment_size) );
	sfragment++;
    }

    /*****************RECEIVE*****************************/
    /* allow remote memory access */
    if( GM_SUCCESS != gm_allow_remote_memory_access (ptl->gm_port) ) {
	ompi_output (0, "unable to allow remote memory access\n");
    }

    OBJ_CONSTRUCT (&(ptl->gm_recv_outstanding_queue), ompi_list_t);

    /* construct the list of recv fragments free */
    OBJ_CONSTRUCT (&(ptl->gm_recv_frags_free), ompi_free_list_t);
  
    ompi_free_list_init( &(ptl->gm_recv_frags_free),
			 sizeof (mca_ptl_gm_recv_frag_t),
			 OBJ_CLASS (mca_ptl_gm_recv_frag_t),
			 0,  /* by default I will provide all items */
                         ptl->num_recv_tokens * 10,  /* the maximum number of items in the free list */
                         ptl->num_recv_tokens,  /* if it need to allocate some more */
                         NULL );
 
    /* allocate the elements */
    free_rfragment = (mca_ptl_gm_recv_frag_t *)
	calloc( ptl->num_recv_tokens, sizeof(mca_ptl_gm_recv_frag_t) );
    ptl->gm_recv_fragments = free_rfragment;

    /*allocate the registered memory */
    ptl->gm_recv_dma_memory =
	gm_dma_malloc( ptl->gm_port, (mca_ptl_gm_component.gm_segment_size * ptl->num_recv_tokens) + GM_PAGE_LEN );
    if( NULL == ptl->gm_recv_dma_memory ) {
	ompi_output( 0, "unable to allocate registered memory for receive\n" );
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for( i = 0; i < 2; i++ ) {
	OMPI_FREE_LIST_RETURN( &(ptl->gm_recv_frags_free), (ompi_list_item_t *)free_rfragment );
	free_rfragment++;

	gm_provide_receive_buffer( ptl->gm_port, (char*)ptl->gm_recv_dma_memory + i * mca_ptl_gm_component.gm_segment_size,
				   GM_SIZE, GM_HIGH_PRIORITY );
    }
    for( i = 2; i < ptl->num_recv_tokens; i++ ) {
	OMPI_FREE_LIST_RETURN( &(ptl->gm_recv_frags_free), (ompi_list_item_t *)free_rfragment );
	free_rfragment++;

	gm_provide_receive_buffer( ptl->gm_port, (char*)ptl->gm_recv_dma_memory + i * mca_ptl_gm_component.gm_segment_size,
				   GM_SIZE, GM_LOW_PRIORITY );
    }

    OBJ_CONSTRUCT( &(ptl->gm_pending_acks), ompi_list_t );

    return OMPI_SUCCESS;
}
 
static int
mca_ptl_gm_init( mca_ptl_gm_component_t * gm )
{
    uint32_t index;
    mca_ptl_gm_module_t* ptl;
    uint32_t save_counter;

    /* let's try to find if GM is available */
    if( GM_SUCCESS != gm_init() ) {
        ompi_output( 0, "[%s:%d] error in initializing the gm library\n", __FILE__, __LINE__ );
	return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* First discover all available boards. For each board we will create a unique PTL */
    mca_ptl_gm_component.gm_ptl_modules = calloc( mca_ptl_gm_component.gm_max_ptl_modules,
						  sizeof (mca_ptl_gm_module_t *));
    if (NULL == mca_ptl_gm_component.gm_ptl_modules) {
        ompi_output( 0, "[%s:%d] error in initializing the gm PTL's.\n", __FILE__, __LINE__ );
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    mca_ptl_gm_component.gm_num_ptl_modules = 
	mca_ptl_gm_discover_boards( mca_ptl_gm_component.gm_ptl_modules,
				    mca_ptl_gm_component.gm_max_ptl_modules,
				    mca_ptl_gm_component.gm_max_boards_number,
				    mca_ptl_gm_component.gm_max_port_number );

    /* In the case when we are in a multi-threaded environment each PTL will have it's
     * own thread. At this point all structures are correctly initialized, each thread
     * will grab one and use it.
     */
    for( index = 0; index < mca_ptl_gm_component.gm_num_ptl_modules; index++ ) {
        ptl = mca_ptl_gm_component.gm_ptl_modules[index];
        /* Now prepost some received and allocate some sends. After this step the PTL
         * is fully initialized.
         */
        if( OMPI_SUCCESS != mca_ptl_gm_init_sendrecv( ptl ) )
            break;
        if( ompi_using_threads() ) {
#if OMPI_HAVE_POSIX_THREADS
	    ptl->thread.t_run = (ompi_thread_fn_t)mca_ptl_gm_thread_progress;
	    ptl->thread.t_arg = (void*)ptl;
#endif  /* OMPI_HAVE_POSIX_THREADS */
	    if( OMPI_SUCCESS != ompi_thread_start( &(ptl->thread) ) )
		break;
        }
    }
    save_counter = index;
    /* If we are unable to start all the required threads we update the total
     * number of threads and call finalize for the others PTLs.
     */
    for( ; index < mca_ptl_gm_component.gm_num_ptl_modules; index++ ) {
        mca_ptl_base_module_t* ptl = (mca_ptl_base_module_t*)mca_ptl_gm_component.gm_ptl_modules[index];
        ptl->ptl_finalize( ptl );
    }
    mca_ptl_gm_component.gm_num_ptl_modules = save_counter;

    return (mca_ptl_gm_component.gm_num_ptl_modules > 0 ? OMPI_SUCCESS : OMPI_ERR_OUT_OF_RESOURCE);
}

/*
 * Initialize the GM component, 
 * check how many boards are available and open ports on them.
 */

mca_ptl_base_module_t **
mca_ptl_gm_component_init (int *num_ptl_modules,
                           bool * allow_multi_user_threads,
                           bool * have_hidden_threads)
{
    mca_ptl_base_module_t **ptls;

    *num_ptl_modules = 0;
    *allow_multi_user_threads = true;
#if OMPI_HAVE_POSIX_THREADS
    *have_hidden_threads = true;
#else
    *have_hidden_threads = false;
#endif  /* OMPI_HAVE_POSIX_THREADS */

    if (OMPI_SUCCESS != mca_ptl_gm_init (&mca_ptl_gm_component)) {
        ompi_output( 0, "[%s:%d] error in initializing gm state and PTL's. (%d PTL's)\n",
                     __FILE__, __LINE__, mca_ptl_gm_component.gm_num_ptl_modules );
        return NULL;
    }

    /* publish GM parameters with the MCA framework */
    if (OMPI_SUCCESS != mca_ptl_gm_module_store_data_toexchange ())
        return 0;

    /* return array of PTLs */
    ptls = (mca_ptl_base_module_t**) malloc (
                mca_ptl_gm_component.gm_num_ptl_modules * sizeof(mca_ptl_base_module_t *));
    if (NULL == ptls) {
        return NULL;
    }

    memcpy (ptls, mca_ptl_gm_component.gm_ptl_modules,
            mca_ptl_gm_component.gm_num_ptl_modules * sizeof(mca_ptl_gm_module_t *));
    *num_ptl_modules = mca_ptl_gm_component.gm_num_ptl_modules;
    return ptls;
}

/*
 *  GM module control
 */

int
mca_ptl_gm_component_control (int param, void *value, size_t size)
{
    return OMPI_SUCCESS;
}

char* gm_get_local_buffer( void )
{
    return malloc( sizeof(char) * mca_ptl_gm_component.gm_segment_size );
}

void gm_release_local_buffer( char* ptr )
{
    free(ptr);
}

/*
 *  GM module progress.
 */

int
mca_ptl_gm_component_progress (mca_ptl_tstamp_t tstamp)
{
    uint32_t i;
    gm_recv_event_t *event;
    mca_ptl_gm_module_t *ptl;

    for( i = 0; i < mca_ptl_gm_component.gm_num_ptl_modules; i++) {
        ptl = mca_ptl_gm_component.gm_ptl_modules[i];
        event = gm_receive(ptl->gm_port);
        /* If there are no receive events just skip the function call */
        if( GM_NO_RECV_EVENT != gm_ntohc(event->recv.type) ) {
            mca_ptl_gm_analyze_recv_event( ptl, event );
        }
    }
    return OMPI_SUCCESS;
}
