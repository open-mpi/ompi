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


mca_ptl_gm_module_1_0_0_t mca_ptl_gm_module = {
    {
     /* First, the mca_base_module_t struct containing meta information
        about the module itself */
     {
      /* Indicate that we are a pml v1.0.0 module (which also implies a
         specific MCA version) */
      MCA_PTL_BASE_VERSION_1_0_0,
      "gm",                     /* MCA module name */
      1,                        /* MCA module major version */
      0,                        /* MCA module minor version */
      0,                        /* MCA module release version */
      mca_ptl_gm_module_open,   /* module open */
      mca_ptl_gm_module_close   /* module close */
      }
     ,
     /* Next the MCA v1.0.0 module meta data */
     {
      /* Whether the module is checkpointable or not */
      false}
     ,
     mca_ptl_gm_module_init,
     mca_ptl_gm_module_control,
     mca_ptl_gm_module_progress}
};

static int  mca_ptl_gm_module_initialized = 0;



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
ompi_mca_ptl_gm_finalize (mca_ptl_gm_module_1_0_0_t * gm)
{
    /* add code */
    return OMPI_SUCCESS;
}




/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int
mca_ptl_gm_module_open (void)
{
    /* initialize state */
    mca_ptl_gm_module.gm_ptls = NULL;
    mca_ptl_gm_module.gm_num_ptls = 0;

    /* initialize objects */
    OBJ_CONSTRUCT (&mca_ptl_gm_module.gm_lock, ompi_mutex_t);
    OBJ_CONSTRUCT (&mca_ptl_gm_module.gm_procs, ompi_list_t);
    OBJ_CONSTRUCT (&mca_ptl_gm_module.gm_send_req, ompi_list_t);

    /* register GM module parameters */
    mca_ptl_gm.super.ptl_first_frag_size =
        mca_ptl_gm_param_register_int ("first_frag_size", 16 * 1024);
    mca_ptl_gm.super.ptl_min_frag_size =
        mca_ptl_gm_param_register_int ("min_frag_size", 0);
    mca_ptl_gm.super.ptl_min_frag_size =
        mca_ptl_gm_param_register_int ("free_list_num", 32);
    mca_ptl_gm.super.ptl_min_frag_size =
        mca_ptl_gm_param_register_int ("free_list_inc", 32);

    return OMPI_SUCCESS;
}




/*
 * module close 
 */

int
mca_ptl_gm_module_close (void)
{

    /* if (OMPI_SUCCESS != ompi_mca_ptl_gm_finalize(&mca_ptl_gm_module)) {
       ompi_output(0,
       "[%s:%d] error in finalizing gm state and PTL's.\n",
       __FILE__, __LINE__);
       return NULL;
       } */


    if (NULL != mca_ptl_gm_module.gm_ptls)
        free (mca_ptl_gm_module.gm_ptls);

    OBJ_DESTRUCT (&mca_ptl_gm_module.gm_procs);
    OBJ_DESTRUCT (&mca_ptl_gm_module.gm_send_req);

    return ompi_event_fini ();
}




/*
 *  Create a ptl instance and add to modules list.
 */

static int
mca_ptl_gm_create ()
{
    mca_ptl_gm_t *ptl;
    char        param[256];

    ptl = malloc (sizeof (mca_ptl_gm_t));
    if (NULL == ptl) {
        ompi_output (0,
                     " ran out of resource to allocate ptl_instance \n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    memcpy (ptl, &mca_ptl_gm, sizeof (mca_ptl_gm));
    mca_ptl_gm_module.gm_ptls[mca_ptl_gm_module.gm_num_ptls++] = ptl;

    return OMPI_SUCCESS;
}





/*
 * Create a GM PTL instance 
 */

static int
mca_ptl_gm_module_create_instances (void)
{

    int         i;
    int         maxptls = 1;    // maxptls set to 1
    /* allocate memory for ptls */

    mca_ptl_gm_module.gm_max_ptls = maxptls;
    mca_ptl_gm_module.gm_ptls = malloc (maxptls * sizeof (mca_ptl_gm_t *));
    if (NULL == mca_ptl_gm_module.gm_ptls)
        return OMPI_ERR_OUT_OF_RESOURCE;

    for (i = 0; i < maxptls; i++) {
        mca_ptl_gm_create ();
    }
    return OMPI_SUCCESS;
}




/*
 *  Register GM module addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int
mca_ptl_gm_module_store_data_toexchange (void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_ptl_gm_addr_t *addrs;

    size = mca_ptl_gm_module.gm_num_ptls * sizeof (mca_ptl_gm_addr_t);
    addrs = malloc (size);

    if (NULL == addrs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < mca_ptl_gm_module.gm_num_ptls; i++) {
        mca_ptl_gm_t *ptl = mca_ptl_gm_module.gm_ptls[i];
        addrs[i].local_id = ptl->my_lid;
        addrs[i].global_id = ptl->my_gid;
        addrs[i].port_id = ptl->my_port_id;
    }
    rc = mca_base_modex_send (&mca_ptl_gm_module.super.ptlm_version, addrs,
                              size);
    free (addrs);
    return rc;
}




/*
 * initialize a ptl interface
 * 
 */

static int
ompi_mca_ptl_gm_init (mca_ptl_gm_module_1_0_0_t * gm)
{

    mca_ptl_gm_t *ptl;
    unsigned int board_no, port_no;
    char       *buffer_ptr;
    gm_status_t status;
    int         buf_len;
    int         i;

    if (OMPI_SUCCESS != mca_ptl_gm_module_create_instances ())
        return 0;

    /*hack : we have set the gm_max_ptls to 1 */
    for (i = 0; i < mca_ptl_gm_module.gm_max_ptls; i++) {
        ptl = mca_ptl_gm_module.gm_ptls[i];

        // open the first available gm port for this board 
        board_no = i;
        for (port_no = 2; port_no < MAX_GM_PORTS; port_no++) {

            printf ("about to call open port\n");
            if (port_no != 3) {
                status = gm_open (&(ptl->my_port), board_no, port_no, "OMPI-GM", GM_API_VERSION_2_0);   // port 0,1,3 reserved 

                if (GM_SUCCESS == status) {
                    ptl->my_port_id = port_no;
                    break;
                }
            }

        }

        /*  Get node local Id */
        if (GM_SUCCESS != gm_get_node_id (ptl->my_port, &(ptl->my_lid))) {
            ompi_output (0, " failure to get local_id \n");
            return 0;
        }

        /* Convert local id to global id */
        if (GM_SUCCESS !=
            gm_node_id_to_global_id (ptl->my_port, ptl->my_lid,
                                     &(ptl->my_gid))) {
            ompi_output (0, " Error: Unable to get my GM global id \n");
            return 0;
        }

    }

    /* publish GM parameters with the MCA framework */
    if (OMPI_SUCCESS != mca_ptl_gm_module_store_data_toexchange ())
        return 0;

    return OMPI_SUCCESS;

}




static int
ompi_mca_ptl_gm_init_sendrecv (mca_ptl_gm_module_1_0_0_t * gm)
{
    int         i;
    mca_ptl_gm_t *ptl;
    gm_status_t status;
    int         buf_len;
    void       *buffer_ptr;

    for (i = 0; i < mca_ptl_gm_module.gm_max_ptls; i++) {

        ptl = mca_ptl_gm_module.gm_ptls[i];

#if 0
        /* initialise the free lists */
        ompi_free_list_init (&(mca_ptl_gm_module.gm_send_req),
                             sizeof (mca_ptl_gm_send_request_t),
                             OBJ_CLASS (mca_ptl_gm_send_request_t),
                             mca_ptl_gm_module.gm_free_list_num,
                             mca_ptl_gm_module.gm_free_list_max,
                             mca_ptl_gm_module.gm_free_list_inc, NULL);

#endif

        /** Receive part **/
        /*allow remote memory access */
        status = gm_allow_remote_memory_access (ptl->my_port);
        if (GM_SUCCESS != status) {
            ompi_output (0, "unable to allow remote memory access\n");

        }

        ptl->num_send_tokens = gm_num_send_tokens (ptl->my_port);
        ptl->num_recv_tokens = gm_num_receive_tokens (ptl->my_port);

        /* set acceptable sizes */
        /*status = gm_set_acceptable_sizes(ptl->my_port, GM_LOW_PRIORITY,
         * MASK);*/


        /* post receive buffers for each available token */
        buf_len = THRESHOLD;
        /*TODO need to provide buffers with two different sizes to distinguish
         * between header and data */

        for (i = 0; i < ptl->num_recv_tokens; i++) {
            buffer_ptr = gm_dma_malloc (ptl->my_port, buf_len);
            gm_provide_receive_buffer (ptl->my_port, buffer_ptr,
                                       SIZE, GM_LOW_PRIORITY);
        }
#if 0
        /** Send Part **/
        OBJ_CONSTRUCT (&mca_ptl_gm.gm_send_frag, ompi_free_list_t);
        ompi_free_list_init (&(mca_ptl_gm_module.gm_send_frag),
                             sizeof (mca_ptl_gm_send_frag_t),
                             OBJ_CLASS (mca_ptl_gm_send_frag_t));
        /* allocate send buffers */
        total_registered_memory = max_send_buf * SIZE;
        ptl->send_req->req_frag->head =
            (struct send_buf *) gm_dma_malloc (ptl->my_port,
                                     total_registered_memory);

#endif
    }

    return OMPI_SUCCESS;

}


/*
 * Initialize the GM module, 
 * check how many boards are available and open ports on them.
 */

mca_ptl_t **
mca_ptl_gm_module_init (int *num_ptls,
                        bool * allow_multi_user_threads,
                        bool * have_hidden_threads)
{
    mca_ptl_t **ptls;
    int         rc;
    unsigned int board_id, port_id;

    *num_ptls = 0;
    *allow_multi_user_threads = false;
    *have_hidden_threads = false;


/*
    ompi_free_list_init (&(mca_ptl_gm_module.gm_send_req),
                         sizeof (mca_ptl_gm_send_request_t),
                         OBJ_CLASS (mca_ptl_gm_send_request_t),
                         mca_ptl_gm_module.gm_free_list_num,
                         mca_ptl_gm_module.gm_free_list_max,
                         mca_ptl_gm_module.gm_free_list_inc, NULL);
*/
    if (OMPI_SUCCESS != ompi_mca_ptl_gm_init (&mca_ptl_gm_module)) {
        ompi_output (0,
                     "[%s:%d] error in initializing gm state and PTL's.\n",
                     __FILE__, __LINE__);
        return NULL;
    }

    if (OMPI_SUCCESS != ompi_mca_ptl_gm_init_sendrecv (&mca_ptl_gm_module)) {
        ompi_output (0,
                     "[%s:%d] error in initializing buffer resources .\n",
                     __FILE__, __LINE__);
        return NULL;
    }


    /* return array of PTLs */
    ptls = malloc (mca_ptl_gm_module.gm_num_ptls * sizeof (mca_ptl_t *));
    if (NULL == ptls)
        return NULL;


    memcpy (ptls, mca_ptl_gm_module.gm_ptls,
            mca_ptl_gm_module.gm_num_ptls * sizeof (mca_ptl_gm_t *));
    *num_ptls = mca_ptl_gm_module.gm_num_ptls;
    return ptls;
}

/*
 *  GM module control
 */

int
mca_ptl_gm_module_control (int param, void *value, size_t size)
{
    return OMPI_SUCCESS;
}


/*
 *  GM module progress.
 */

int
mca_ptl_gm_module_progress (mca_ptl_tstamp_t tstamp)
{


    /* check the send queue to see if any pending send can proceed */


    /* check for recieve and , call ptl_match to send it to the upper
       level */


    /* in case matched, do the appropriate queuing. */

    return OMPI_SUCCESS;
}
