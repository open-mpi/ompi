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
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_elan.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"
#include "ptl_elan_req.h"
#include "ptl_elan_priv.h"

mca_ptl_elan_module_1_0_0_t mca_ptl_elan_module = {
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
             mca_ptl_elan_module_open, /* module open */
             mca_ptl_elan_module_close /* module close */
        },
            
        /* Next the MCA v1.0.0 module meta data */
        {
            /* Whether the module is checkpointable or not */
            false
        },
        
        /* The management related interfaces */
        mca_ptl_elan_module_init,
        mca_ptl_elan_module_control,
        mca_ptl_elan_module_progress
    }
};

static mca_ptl_elan_module_1_0_0_t *elan_mp = &mca_ptl_elan_module;
static int      mca_ptl_elan_module_initialized = 0;

/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int
mca_ptl_elan_module_open (void)
{
    /* register super module parameters */
    mca_ptl_elan.super.ptl_exclusivity =
        mca_ptl_elan_param_register_int ("exclusivity", 0);
    mca_ptl_elan.super.ptl_first_frag_size =
        mca_ptl_elan_param_register_int ("first_frag_size", 2048/*magic*/);
    mca_ptl_elan.super.ptl_min_frag_size =
        mca_ptl_elan_param_register_int ("min_frag_size", 320);
    mca_ptl_elan.super.ptl_max_frag_size =
        mca_ptl_elan_param_register_int ("max_frag_size", -1);

    /* register ELAN module parameters */
    elan_mp->elan_free_list_num =
        mca_ptl_elan_param_register_int ("free_list_num", 64);
    elan_mp->elan_free_list_max =
        mca_ptl_elan_param_register_int ("free_list_max", -1);
    elan_mp->elan_free_list_inc =
        mca_ptl_elan_param_register_int ("free_list_inc", 64);

    /* initialize state */
    elan_mp->elan_ptls = NULL;
    elan_mp->elan_num_ptls = 0;
    elan_mp->elan_local = NULL; 

    /* initialize list */
    OBJ_CONSTRUCT (&elan_mp->elan_reqs, ompi_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_prog_events, ompi_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_comp_events, ompi_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_procs, ompi_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_pending_acks, ompi_list_t);

    /* initialize free list */
    OBJ_CONSTRUCT (&elan_mp->elan_events_free, ompi_free_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_reqs, ompi_free_list_t);

    /* initialize other objects */
    OBJ_CONSTRUCT (&elan_mp->elan_lock, ompi_mutex_t);

    return OMPI_SUCCESS;
}


int
mca_ptl_elan_module_close (void)
{
    if (mca_ptl_elan_module_initialized) {

	/* cleanup the proc, ptl, and the module */
	ompi_mca_ptl_elan_finalize (&mca_ptl_elan_module);

	/* XXX: Make sure this is not just an alias pointer */
	if (elan_mp->elan_local) {
	    free (elan_mp->elan_local);
	}

	if (NULL != elan_mp->elan_ptls) {
	    int         i;
	    for (i = elan_mp->elan_num_ptls; i > 0; i--) {
		free (elan_mp->elan_ptls[i - 1]);
	    }
	    free (elan_mp->elan_ptls);
	}
    }

    /* Check whether all the entries are return to the free list */
    if (elan_mp->elan_reqs_free.fl_num_allocated !=
        elan_mp->elan_reqs_free.super.ompi_list_length) {
        ompi_output (0, "elan requests: %d allocated %d returned\n",
                     elan_mp->elan_reqs_free.fl_num_allocated,
                     elan_mp->elan_reqs_free.super.ompi_list_length);
    }

    if (elan_mp->elan_events_free.fl_num_allocated !=
        elan_mp->elan_events_free.super.ompi_list_length) {
        ompi_output (0, "elan events: %d allocated %d returned\n",
                     elan_mp->elan_reqs_free.fl_num_allocated,
                     elan_mp->elan_reqs_free.super.ompi_list_length);
    }


    /* Free the empty list holders */
    OBJ_DESTRUCT (&(elan_mp->elan_reqs));
    OBJ_DESTRUCT (&(elan_mp->elan_prog_events));
    OBJ_DESTRUCT (&(elan_mp->elan_comp_events));
    OBJ_DESTRUCT (&(elan_mp->elan_procs));
    OBJ_DESTRUCT (&(elan_mp->elan_pending_acks));

    /* Destruct the free lists */
    OBJ_DESTRUCT (&(elan_mp->elan_events_free));
    OBJ_DESTRUCT (&(elan_mp->elan_reqs_free));

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
mca_ptl_t **
mca_ptl_elan_module_init (int *num_ptls,
                          bool * allow_multi_user_threads,
                          bool * have_hidden_threads)
{
    mca_ptl_t **ptls;
    int         rc;
 
    *num_ptls = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    /* need to set ompi_using_threads() as ompi_event_init() 
     * will spawn a thread if supported */
    if(OMPI_HAVE_THREADS)
        ompi_set_using_threads(true);

    /* duplicated actions are avoid with a static variable, 
     * inited in ompi_event_init() */
    if ((rc = ompi_event_init ()) != OMPI_SUCCESS) {
	ompi_output(0, 
		"[%s:%d] mca_ptl_elan_module_init: " 
		"unable to initialize event dispatch thread: %d\n",
	       	__FILE__, __LINE__, rc);
        return NULL;
    }

    /* initialize free lists */
    ompi_free_list_init (&(elan_mp->elan_reqs_free),
                         sizeof (mca_ptl_elan_send_request_t),
                         OBJ_CLASS (mca_ptl_elan_send_request_t),
                         elan_mp->elan_free_list_num,
                         elan_mp->elan_free_list_max,
                         elan_mp->elan_free_list_inc, NULL);

    ompi_free_list_init (&elan_mp->elan_events_free,
                         sizeof (mca_ptl_elan_send_frag_t),
                         OBJ_CLASS (mca_ptl_elan_send_frag_t),
                         elan_mp->elan_free_list_num,
                         elan_mp->elan_free_list_max,
                         elan_mp->elan_free_list_inc, NULL);

    /* use default allocator */

    /* open basic elan device */
    if (OMPI_SUCCESS != ompi_mca_ptl_elan_init(&mca_ptl_elan_module)) {
	ompi_output(0, 
		"[%s:%d] error in initializing elan state and PTL's.\n",
		__FILE__, __LINE__);
        return NULL;
    }
    /* 
     * (mca_ptl_elan_module_exchange () != OMPI_SUCCESS)
     *
     * No need to publish parameters with the MCA framework
     *
     * This is called only by those processes who have elan.
     * So it does not qualify to be a global call.
     * Since the processes has elan support can already communicate 
     * over elan, there is no need for a oob_based exchange.
     */

    ptls = (mca_ptl_t **) malloc (elan_mp->elan_num_ptls *
                                       sizeof (mca_ptl_elan_t *));

    if (NULL == ptls) {
	ompi_output(0, 
		"[%s:%d] error in malloc for elan PTL references\n",
		__FILE__, __LINE__);
        return NULL;
    }

    /* FIXME: 
     * Why use memcopy to create two instances of the same 
     * structures, do they need to be defined them as constants,
     * will coherency on two replicas be a potential problem? */
    memcpy (ptls, elan_mp->elan_ptls,
            elan_mp->elan_num_ptls * sizeof (mca_ptl_elan_t *));
    *num_ptls = elan_mp->elan_num_ptls;
    mca_ptl_elan_module_initialized = 1; 

    return ptls;
}

/*
 *  FIXME: to support ELAN module control 
 */

int
mca_ptl_elan_module_control (int param,
                             void *value,
                             size_t size)
{
    switch (param) {
    case MCA_PTL_ENABLE:
        if (*(int *) value) {
            ompi_event_add (&elan_mp->elan_recv_event, 0);
        } else {
            ompi_event_del (&elan_mp->elan_recv_event);
        }
        break;
    default:
        break;
    }
    return OMPI_SUCCESS;
}


/*
 *  FIXME: to support event-based module progress.
 */

int
mca_ptl_elan_module_progress (mca_ptl_tstamp_t tstamp)
{
    ompi_event_loop (OMPI_EVLOOP_ONCE);
    return OMPI_SUCCESS;
}

