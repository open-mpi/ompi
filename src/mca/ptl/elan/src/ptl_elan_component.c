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

#include "constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_elan.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"
#include "ptl_elan_priv.h"

extern ompi_proc_t *ompi_proc_local_proc;

mca_ptl_elan_component_t mca_ptl_elan_component = {
    {
        /* Base module information about itself */
        {
             /* Indicate that we are a pml v1.0.0 module 
              * (which also implies a specific MCA version) */
             MCA_PTL_BASE_VERSION_1_0_0,
             "elan",                   /* MCA module name */
             1,                        /* MCA module major version */
             0,                        /* MCA module minor version */
             0,                        /* MCA module release version */
             mca_ptl_elan_component_open, /* module open */
             mca_ptl_elan_component_close /* module close */
        },
            
        /* Next the MCA v1.0.0 module meta data */
        {
            /* Whether the module is checkpointable or not */
            false
        },
        
        /* The management related interfaces */
        mca_ptl_elan_component_init,
        mca_ptl_elan_component_control,
        mca_ptl_elan_component_progress
    }
};

static mca_ptl_elan_component_t *elan_mp = &mca_ptl_elan_component;
static bool mca_ptl_elan_component_initialized = false;

/*
 *  XXX: Leave it as a routine for possible extension
 *       some elan vp information to the the global registery
 */
static int mca_ptl_elan_addr_put (mca_ptl_elan_component_t  *emp)
{
     int rc;
     size_t i;
     size_t size;
         
     mca_ptl_elan_addr_t *addrs;
    
     size  = emp->num_modules * sizeof(mca_ptl_elan_addr_t);
     addrs = (mca_ptl_elan_addr_t *) malloc(size);

     for(i=0; i< emp->num_modules; i++) {
         mca_ptl_elan_module_t * ptl = emp->modules[i];
         addrs[i].elan_vp    = ptl->elan_vp;
         addrs[i].inuse      = 0;
         addrs[i].gid        = ompi_proc_local_proc->proc_name;
     }

     rc = mca_base_modex_send(&emp->super.ptlm_version, addrs, size);
     free(addrs);
     return rc;
}


/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int
mca_ptl_elan_component_open (void)
{
    int length;
    int param1, param2, param3;

    mca_ptl_elan_module.super.ptl_exclusivity =
        mca_ptl_elan_param_register_int ("exclusivity", 0);

    length = OMPI_PTL_ELAN_MAX_QSIZE - sizeof(mca_ptl_base_header_t);
    param1 = mca_ptl_elan_param_register_int ("first_frag_size", length);
    param2 = mca_ptl_elan_param_register_int ("min_frag_size", length);
    param3 = mca_ptl_elan_param_register_int ("max_frag_size", 2<<31);

    /* Correct these if user give violent parameters */
    mca_ptl_elan_module.super.ptl_first_frag_size = 
	OMPI_PTL_ELAN_GET_MIN(param1, length);
    mca_ptl_elan_module.super.ptl_min_frag_size =  
	OMPI_PTL_ELAN_GET_MAX(param2, length);
    mca_ptl_elan_module.super.ptl_max_frag_size =  
	OMPI_PTL_ELAN_GET_MIN(param3, 2<<31);

    /* initialize state */
    elan_mp->elan_local = NULL; 
    elan_mp->num_modules = 0;
    elan_mp->modules = NULL;
    elan_mp->free_list_num = 32;
    elan_mp->free_list_max = 128;
    elan_mp->free_list_inc = 32;

    /* initialize objects*/
    OBJ_CONSTRUCT (&elan_mp->elan_procs, ompi_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_recv_frags_free, ompi_free_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_lock, ompi_mutex_t);

    return OMPI_SUCCESS;
}


int
mca_ptl_elan_component_close (void)
{
    if (mca_ptl_elan_component_initialized) {

	/* cleanup the proc, ptl, and the module */
	mca_ptl_elan_state_finalize(&mca_ptl_elan_component);
	if (elan_mp->elan_local) {
	    free (elan_mp->elan_local);
	}

	if (NULL != elan_mp->modules) {
	    int         i;
	    for (i = elan_mp->num_modules; i > 0; i--) {
		free (elan_mp->modules[i - 1]);
	    }
	    free (elan_mp->modules);
	}
    }

    /* Free the empty list holders */
    OBJ_DESTRUCT (&(elan_mp->elan_procs));

    /* FIXME:
     * We need free all the memory allocated for this list
     * before desctructing this free_list */
    if (elan_mp->elan_recv_frags_free.fl_num_allocated !=
        elan_mp->elan_recv_frags_free.super.ompi_list_length) {
        ompi_output (0, 
		     "[%s:%d] recv_frags : %d allocated %d returned\n",
		     __FILE__, __LINE__,
                     elan_mp->elan_recv_frags_free.fl_num_allocated,
                     elan_mp->elan_recv_frags_free.super.ompi_list_length);
    }
    OBJ_DESTRUCT (&(elan_mp->elan_recv_frags_free));

    /* Destruct other structures */
    OBJ_DESTRUCT (&elan_mp->elan_lock);

    return OMPI_SUCCESS;
}

/*
 *  ELAN module initialization:
 *  (1) elan4_init() to initialize the basic support and mapping etc.
 *  (2) set up STEN, RDMA and QDMA structures.
 *  (3) register the list of PTL parameters with the MCA
 */
mca_ptl_base_module_t **
mca_ptl_elan_component_init (int *num_ptls,
			     bool * allow_multi_user_threads,
			     bool * have_hidden_threads)
{
    mca_ptl_base_module_t **ptls;
 
    START_FUNC(PTL_ELAN_DEBUG_INIT);

    *num_ptls = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    /* XXX: Set the global variable to be true for threading */
    if(OMPI_HAVE_THREADS)
        ompi_set_using_threads(true);

    ompi_free_list_init (&(elan_mp->elan_recv_frags_free),
                         sizeof (mca_ptl_elan_recv_frag_t),
                         OBJ_CLASS (mca_ptl_elan_recv_frag_t),
			 /*32, 128, 32, */
                         elan_mp->free_list_num,
                         elan_mp->free_list_max,
                         elan_mp->free_list_inc, NULL);

    /* open basic elan device */
    if (OMPI_SUCCESS != mca_ptl_elan_state_init(&mca_ptl_elan_component)) {
	ompi_output(0, 
		"[%s:%d] error in initializing elan state and PTL's.\n",
		__FILE__, __LINE__);
        return NULL;
    }

    if (OMPI_SUCCESS != mca_ptl_elan_addr_put(&mca_ptl_elan_component)) {
        ompi_output(0, 
                "[%s:%d] error in registering with Runtime/OOB \n",
                __FILE__, __LINE__);
        return NULL;
    }

    ptls = (mca_ptl_base_module_t **) malloc (elan_mp->num_modules *
                                              sizeof (mca_ptl_elan_module_t *));
    if (NULL == ptls) {
        ompi_output(0, 
		"[%s:%d] error in allocating memory \n",
                __FILE__, __LINE__);
        return NULL;
    }

    memcpy (ptls, elan_mp->modules,
            elan_mp->num_modules * sizeof (mca_ptl_elan_module_t *));
    *num_ptls = elan_mp->num_modules;
    mca_ptl_elan_component_initialized = true;

    /* XXX + TODO: 
     * have threads listening on send/recv completion from this point on.
     * Points to be aware of.
     * a) Main thread trigger the send 
     * b) Asynchronous thread detects the completion of send/recv, put/get
     *    then update fragment descriptors and requests if needed.
     * c) Main thread progresses with the status of requests being detected.
     * d) Asynchronous thread handles the retransmission and recv side
     *    data copying; Main thread does nothing more than initiating
     *    the messaging.
     * e) Asynchronous thread should also detect the completion of the
     *    program and exit accordingly, via a signal/interrupt.
     */

    if ((mca_ptl_elan_thread_init(elan_mp)) != OMPI_SUCCESS) {
        ompi_output(0, 
		"unable to initialize %d asynchronous threads\n",
		elan_mp->num_modules);
    }

    END_FUNC(PTL_ELAN_DEBUG_INIT);
    return ptls;
}

/* support ELAN module control */ 
int
mca_ptl_elan_component_control (int param,
                             void *value,
                             size_t size)
{
    switch (param) {
    case MCA_PTL_ENABLE:
    default:
        break;
    }
    return OMPI_SUCCESS;
}

/* TODO: to support event-based module progress later.  */
int
mca_ptl_elan_component_progress (mca_ptl_tstamp_t tstamp)
{
    int         i, no_ptls;

    START_FUNC (PTL_ELAN_DEBUG_NONE);

    no_ptls = elan_mp->num_modules;

    /* Iterate over all the PTL input Queues */
    for (i = 0; i < no_ptls; i++) {
	mca_ptl_elan_drain_recv(elan_mp->modules[i]);
	mca_ptl_elan_update_desc(elan_mp->modules[i]);
    }

    END_FUNC (PTL_ELAN_DEBUG_NONE);
    return OMPI_SUCCESS;
}

