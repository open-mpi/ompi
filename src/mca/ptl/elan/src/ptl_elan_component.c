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

#ifdef CHECK_ELAN
#undef CHECK_ELAN
#define CHECK_ELAN 0
#endif

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
 *  some elan vp information to the the global registery
 */
static int mca_ptl_elan_component_register (mca_ptl_elan_component_t  *emp)
{
     int rc;
     size_t i;
     size_t size;
         
     mca_ptl_elan_addr_t *addrs;
    
     size  = emp->elan_num_ptl_modules * sizeof(mca_ptl_elan_addr_t);
     addrs = (mca_ptl_elan_addr_t *) malloc(size);

     for(i=0; i< emp->elan_num_ptl_modules; i++) {
         mca_ptl_elan_module_t * ptl = emp->elan_ptl_modules[i];
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
    /* register super module parameters */
    mca_ptl_elan_module.super.ptl_exclusivity =
        mca_ptl_elan_param_register_int ("exclusivity", 0);
    mca_ptl_elan_module.super.ptl_first_frag_size =
        mca_ptl_elan_param_register_int ("first_frag_size", 
                (2048 - sizeof(mca_ptl_base_header_t))/*magic*/);
    mca_ptl_elan_module.super.ptl_min_frag_size =
        mca_ptl_elan_param_register_int ("min_frag_size", 
                (2048 - sizeof(mca_ptl_base_header_t))/*magic*/);
    mca_ptl_elan_module.super.ptl_max_frag_size =
        mca_ptl_elan_param_register_int ("max_frag_size", 2<<30);

    /* register ELAN module parameters */
    elan_mp->elan_free_list_num =
        mca_ptl_elan_param_register_int ("free_list_num", 32);
    elan_mp->elan_free_list_max =
        mca_ptl_elan_param_register_int ("free_list_max", 1024);
    elan_mp->elan_free_list_inc =
        mca_ptl_elan_param_register_int ("free_list_inc", 32);

    /* initialize state */
    elan_mp->elan_ptl_modules = NULL;
    elan_mp->elan_num_ptl_modules = 0;
    elan_mp->elan_local = NULL; 

    /* initialize lists */
    OBJ_CONSTRUCT (&elan_mp->elan_procs, ompi_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_pending_acks, ompi_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_recv_frags, ompi_list_t);
    OBJ_CONSTRUCT (&elan_mp->elan_send_frags, ompi_list_t);

    OBJ_CONSTRUCT (&elan_mp->elan_recv_frags_free, ompi_free_list_t);

    /* initialize other objects */
    OBJ_CONSTRUCT (&elan_mp->elan_lock, ompi_mutex_t);

    return OMPI_SUCCESS;
}


int
mca_ptl_elan_component_close (void)
{
    if (mca_ptl_elan_component_initialized) {

	/* cleanup the proc, ptl, and the module */
	ompi_mca_ptl_elan_finalize (&mca_ptl_elan_component);

	/* XXX: Make sure this is not just an alias pointer */
	if (elan_mp->elan_local) {
	    free (elan_mp->elan_local);
	}

	if (NULL != elan_mp->elan_ptl_modules) {
	    int         i;
	    for (i = elan_mp->elan_num_ptl_modules; i > 0; i--) {
		free (elan_mp->elan_ptl_modules[i - 1]);
	    }
	    free (elan_mp->elan_ptl_modules);
	}
    }

    if (elan_mp->elan_recv_frags_free.fl_num_allocated !=
        elan_mp->elan_recv_frags_free.super.ompi_list_length) {
        ompi_output (0, "[%s:%d] recv_frags : %d allocated %d returned\n",
		     __FILE__, __LINE__,
                     elan_mp->elan_recv_frags_free.fl_num_allocated,
                     elan_mp->elan_recv_frags_free.super.ompi_list_length);
    }

    /* FIXME: free free_list entries before destructing lists */

    /* Free the empty list holders */
    OBJ_DESTRUCT (&(elan_mp->elan_procs));
    OBJ_DESTRUCT (&(elan_mp->elan_pending_acks));
    OBJ_DESTRUCT (&(elan_mp->elan_send_frags));
    OBJ_DESTRUCT (&(elan_mp->elan_recv_frags));

    /* TODO:
     * We need free all the memory allocated for this list
     * before desctructing this free_list */
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
mca_ptl_elan_component_init (int *num_ptl_modules,
                          bool * allow_multi_user_threads,
                          bool * have_hidden_threads)
{
    mca_ptl_base_module_t **ptls;
 
    *num_ptl_modules = 0;

    START_FUNC();

    if (CHECK_ELAN) { 
	char hostname[32]; gethostname(hostname, 32); 
	fprintf(stderr, "[%s:%s:%d] debugging ...\n",
		hostname, __FUNCTION__, __LINE__);
    }

    /* TODO: support multiple threads */

    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    if (CHECK_ELAN) { 
	char hostname[32]; gethostname(hostname, 32); 
	fprintf(stderr, "[%s:%s:%d] before list init...\n",
		hostname, __FUNCTION__, __LINE__);
    }

    if (CHECK_ELAN) { 
	char hostname[32]; gethostname(hostname, 32); 
	fprintf(stderr, "[%s:%s:%d] after list init...\n",
		hostname, __FUNCTION__, __LINE__);
    }


    ompi_free_list_init (&(elan_mp->elan_recv_frags_free),
                         sizeof (mca_ptl_elan_recv_frag_t),
                         OBJ_CLASS (mca_ptl_elan_recv_frag_t),
                         elan_mp->elan_free_list_num,
                         elan_mp->elan_free_list_max,
                         elan_mp->elan_free_list_inc, NULL);

    if (CHECK_ELAN) { 
	char hostname[32]; gethostname(hostname, 32); 
	fprintf(stderr, "[%s:%s:%d] after list init...\n",
		hostname, __FUNCTION__, __LINE__);
    }

    /* open basic elan device */
    if (OMPI_SUCCESS != ompi_mca_ptl_elan_init(&mca_ptl_elan_component)) {
	ompi_output(0, 
		"[%s:%d] error in initializing elan state and PTL's.\n",
		__FILE__, __LINE__);
        return NULL;
    }

    if (OMPI_SUCCESS != mca_ptl_elan_component_register(&mca_ptl_elan_component)) {
        ompi_output(0, 
                "[%s:%d] error in registering with Runtime/OOB \n",
                __FILE__, __LINE__);
        return NULL;
    }

    ptls = (mca_ptl_base_module_t **) malloc (elan_mp->elan_num_ptl_modules *
                                              sizeof (mca_ptl_elan_module_t *));
    if (NULL == ptls) {
	ompi_output(0, 
		"[%s:%d] error in malloc for elan PTL references\n",
		__FILE__, __LINE__);
        return NULL;
    }

    memcpy (ptls, elan_mp->elan_ptl_modules,
            elan_mp->elan_num_ptl_modules * sizeof (mca_ptl_elan_module_t *));
    *num_ptl_modules = elan_mp->elan_num_ptl_modules;
    mca_ptl_elan_component_initialized = true;

    END_FUNC();
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

static int times = 0;
int
mca_ptl_elan_component_progress (mca_ptl_tstamp_t tstamp)
{
    START_FUNC();
    /*if (times <= -1000)*/
    if (times <= -1) 
    {
	char hostname[32]; gethostname(hostname, 32); 
	fprintf(stderr, "[%s:%s:%d] debugging ...\n",
		hostname, __FUNCTION__, __LINE__);
	exit(1); 
    } else {
	times ++;
    }
    mca_ptl_elan_drain_recv(elan_mp);
    mca_ptl_elan_update_desc(elan_mp);
    END_FUNC();
    return OMPI_SUCCESS;
}
