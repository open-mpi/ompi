/*
 * $HEADER$
 */
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "include/constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_gm.h"
#include "ptl_gm_addr.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_req.h"
#include "ptl_gm_priv.h"

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

static bool mca_ptl_gm_component_initialized = false;

/*
 * utility routines for parameter registration
 */

static inline char *
mca_ptl_gm_param_register_string (const char *param_name,
                                  const char *default_value)
{
    char       *param_value;
    int         id =
        mca_base_param_register_string ("ptl", "gm", param_name, NULL,
                                        default_value);
    mca_base_param_lookup_string (id, &param_value);
    return param_value;
}

static inline int
mca_ptl_gm_param_register_int (const char *param_name, int default_value)
{
    int         id =
        mca_base_param_register_int ("ptl", "gm", param_name, NULL,
                                     default_value);
    int         param_value = default_value;
    mca_base_param_lookup_int (id, &param_value);
    return param_value;
}

/*
 *
 */
static int
ompi_mca_ptl_gm_finalize (mca_ptl_gm_module_t * gm)
{
    /* add code */
    return OMPI_SUCCESS;
}

/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int
mca_ptl_gm_component_open (void)
{
    /* initialize state */
    mca_ptl_gm_component.gm_ptl_modules = NULL;
    mca_ptl_gm_component.gm_num_ptl_modules = 0;

    /* initialize objects */
    OBJ_CONSTRUCT (&mca_ptl_gm_component.gm_lock, ompi_mutex_t);
    OBJ_CONSTRUCT (&mca_ptl_gm_component.gm_procs, ompi_list_t);
    OBJ_CONSTRUCT (&mca_ptl_gm_component.gm_send_req, ompi_list_t);

    /* register GM component parameters */
    mca_ptl_gm_module.super.ptl_first_frag_size =
        mca_ptl_gm_param_register_int ("first_frag_size", 
		((PTL_GM_FIRST_FRAG_SIZE) - 64));
    mca_ptl_gm_module.super.ptl_min_frag_size =
        mca_ptl_gm_param_register_int ("min_frag_size", 1<<16);
    mca_ptl_gm_module.super.ptl_max_frag_size =
        mca_ptl_gm_param_register_int ("max_frag_size", 256 * 1024);
    
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
#ifdef GOPAL_TODO
    if (OMPI_SUCCESS != ompi_mca_ptl_gm_finalize(&mca_ptl_gm_component)) {
       ompi_output(0,
       "[%s:%d] error in finalizing gm state and PTL's.\n",
       __FILE__, __LINE__);
       return NULL;
       }

#endif

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
mca_ptl_gm_create (int i)
{
    mca_ptl_gm_module_t *ptl;

    ptl = (mca_ptl_gm_module_t *)malloc (sizeof (mca_ptl_gm_module_t));
    if (NULL == ptl) {
        ompi_output (0,
                     " ran out of resource to allocate ptl_instance \n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    memcpy (ptl, &mca_ptl_gm_module, sizeof (mca_ptl_gm_module));
    mca_ptl_gm_component.gm_ptl_modules[i] = ptl;

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
    addrs = (mca_ptl_gm_addr_t *)malloc (size);/*XXX: check this out */

    if (NULL == addrs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < mca_ptl_gm_component.gm_num_ptl_modules; i++) {
        mca_ptl_gm_module_t *ptl = mca_ptl_gm_component.gm_ptl_modules[i];
        addrs[i].local_id = ptl->my_local_id;
        addrs[i].global_id = ptl->my_global_id;
        addrs[i].port_id = ptl->my_port_id;
    }
    rc = mca_base_modex_send (&mca_ptl_gm_component.super.ptlm_version, addrs,
                              size);
    free (addrs);
    return rc;
}

static int
ompi_mca_ptl_gm_init (mca_ptl_gm_component_t * gm)
{
    mca_ptl_gm_module_t *ptl;
    unsigned int board_no, port_no;
    gm_status_t status;
    int         i;
    int         maxptls = 1;    /* maxptls set to 1 */

    mca_ptl_gm_component.gm_max_ptl_modules = maxptls;
    mca_ptl_gm_component.gm_ptl_modules = malloc (maxptls * 
                                                  sizeof (mca_ptl_gm_module_t *));
    if (NULL == mca_ptl_gm_component.gm_ptl_modules)
        return OMPI_ERR_OUT_OF_RESOURCE;

    for (i = 0; i < maxptls; i++) {
        mca_ptl_gm_create (i);
    }

    /*Hack : we have set the gm_max_ptl_modules to 1 */
    for (i = 0; i < mca_ptl_gm_component.gm_max_ptl_modules; i++) {
        ptl = mca_ptl_gm_component.gm_ptl_modules[i];

        /* open the first available gm port for this board  */
        board_no = i;
        for (port_no = 2; port_no < MAX_GM_PORTS; port_no++) { 
	    GM_DBG(PTL_GM_DBG_COMM,"about to call open port\n");
	    if (port_no == 3) continue;
	    /* port 0,1,3 reserved  */
	    status = gm_open (&(ptl->my_port), board_no, 
                              port_no, "OMPI-GM", GM_API_VERSION_2_0);  

	    if (GM_SUCCESS == status) {
	       	ptl->my_port_id = port_no;
                mca_ptl_gm_component.gm_num_ptl_modules++;
	       	break;
	    }
       	}

#if 1
        /*  Get node local Id */
        if (GM_SUCCESS != gm_get_node_id (ptl->my_port, &(ptl->my_local_id))) {
            ompi_output (0, " failure to get local_id \n");
            return 0;
        }
#endif

        /* Convert local id to global id */
        if (GM_SUCCESS !=
            gm_node_id_to_global_id (ptl->my_port, ptl->my_local_id,
                                     &(ptl->my_global_id))) {
            ompi_output (0, " Error: Unable to get my GM global id \n");
            return 0;
        }
    }

    return OMPI_SUCCESS;

}




static int
ompi_mca_ptl_gm_init_sendrecv (mca_ptl_gm_component_t * gm)
{
    int i;
    mca_ptl_gm_module_t *ptl;
    gm_status_t status;
    void *gm_send_reg_memory , *gm_recv_reg_memory;
    ompi_free_list_t *fslist, *free_rlist;
    mca_ptl_gm_send_frag_t *sfragment;
    mca_ptl_gm_recv_frag_t *free_rfragment;

    for (i = 0; i < mca_ptl_gm_component.gm_max_ptl_modules; i++) {
        ptl = mca_ptl_gm_component.gm_ptl_modules[i];

        ptl->num_send_tokens = gm_num_send_tokens (ptl->my_port);
        ptl->num_send_tokens -= PTL_GM_ADMIN_SEND_TOKENS;
        ptl->num_recv_tokens = gm_num_receive_tokens (ptl->my_port);
        ptl->num_recv_tokens -= PTL_GM_ADMIN_RECV_TOKENS;

       /****************SEND****************************/
        /* construct a list of send fragments */
        OBJ_CONSTRUCT (&(ptl->gm_send_frags), ompi_free_list_t);
        OBJ_CONSTRUCT (&(ptl->gm_send_frags_queue), ompi_list_t);
        fslist = &(ptl->gm_send_frags);

        ompi_free_list_init (&(ptl->gm_send_frags),
                             sizeof (mca_ptl_gm_send_frag_t),
                             OBJ_CLASS (mca_ptl_gm_send_frag_t),
                             ptl->num_send_tokens,ptl->num_send_tokens, 1, NULL); /* not using mpool */

        /* allocate the elements */
        sfragment = (mca_ptl_gm_send_frag_t *)
            malloc (sizeof(mca_ptl_gm_send_frag_t) *
                    (ptl->num_send_tokens));

        /* allocate the registered memory */
        gm_send_reg_memory = gm_dma_malloc ( ptl->my_port,
                              (GM_SEND_BUF_SIZE * ptl->num_send_tokens) );
        if( NULL == gm_send_reg_memory ) {
            ompi_output( 0, "unable to allocate registered memory\n" );
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i = 0; i < ptl->num_send_tokens; i++) {
            ompi_list_item_t *item;
            sfragment->send_buf = gm_send_reg_memory;
            item = (ompi_list_item_t *) sfragment;
            OMPI_FREE_LIST_RETURN( fslist, item );

            gm_send_reg_memory = ((char *)gm_send_reg_memory) + GM_SEND_BUF_SIZE;
            sfragment++;

        }

        /*****************RECEIVE*****************************/
        /*allow remote memory access */
        status = gm_allow_remote_memory_access (ptl->my_port);
        if (GM_SUCCESS != status) {
            ompi_output (0, "unable to allow remote memory access\n");

        }

        OBJ_CONSTRUCT (&(ptl->gm_recv_outstanding_queue), ompi_list_t);

        /* construct the list of recv fragments free */
        OBJ_CONSTRUCT (&(ptl->gm_recv_frags_free), ompi_free_list_t);
        free_rlist = &(ptl->gm_recv_frags_free);
  
        ompi_free_list_init (&(ptl->gm_recv_frags_free),
                             sizeof (mca_ptl_gm_recv_frag_t),
                             OBJ_CLASS (mca_ptl_gm_recv_frag_t),
                             ptl->num_recv_tokens,ptl->num_recv_tokens, 1, NULL); 
 
        /*allocate the elements */
        free_rfragment = (mca_ptl_gm_recv_frag_t *)
                    malloc(sizeof(mca_ptl_gm_recv_frag_t) * NUM_RECV_FRAGS);


        for (i = 0; i < NUM_RECV_FRAGS; i++) {
            ompi_list_item_t *item;
            item = (ompi_list_item_t *) free_rfragment;
            OMPI_FREE_LIST_RETURN( free_rlist, item );
            free_rfragment++;
        }

        /*allocate the registered memory */
        gm_recv_reg_memory =
            gm_dma_malloc (ptl->my_port,
                           (GM_RECV_BUF_SIZE * ptl->num_recv_tokens ) );
        if( NULL == gm_recv_reg_memory ) {
            ompi_output( 0, "unable to allocate registered memory for receive\n" );
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i = 0; i < ptl->num_recv_tokens ; i++) {
            gm_provide_receive_buffer( ptl->my_port, gm_recv_reg_memory,
                                       GM_SIZE, GM_LOW_PRIORITY );
            gm_recv_reg_memory = ((char *)gm_recv_reg_memory) + GM_RECV_BUF_SIZE;
        }
    }

    return OMPI_SUCCESS;
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
    *allow_multi_user_threads = false;
    *have_hidden_threads = false;

    if (OMPI_SUCCESS != ompi_mca_ptl_gm_init (&mca_ptl_gm_component)) {
        ompi_output (0,
                     "[%s:%d] error in initializing gm state and PTL's.\n",
                     __FILE__, __LINE__);
        return NULL;
    }

    if (OMPI_SUCCESS != ompi_mca_ptl_gm_init_sendrecv (&mca_ptl_gm_component)) {
        ompi_output (0,
                     "[%s:%d] error in initializing buffer resources .\n",
                     __FILE__, __LINE__);
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


/*
 *  GM module progress.
 */
int
mca_ptl_gm_component_progress (mca_ptl_tstamp_t tstamp)
{
    int rc;
    /* XXX: Do all the following inside a dispatcher, either in this routine
     *      or mca_ptl_gm_incoming_recv(), YUW
     * i) check the send queue to see if any pending send can proceed 
     * ii) check for recieve and , call ptl_match to send it to the upper level
     * BTW, ptl_matced is invoked inside ptl_match() via PML.
     */
     rc = mca_ptl_gm_incoming_recv(&mca_ptl_gm_component);
    return OMPI_SUCCESS;
}
