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

static mca_ptl_elan_module_1_0_0_t *mp = &mca_ptl_elan_module;

/**
 * utility routines for parameter registration
 */
static inline char *
mca_ptl_elan_param_register_string (const char *param_name,
                                    const char *default_value)
{
    int         id;
    char       *param_value;

    id = mca_base_param_register_string ("ptl", "elan", param_name, NULL,
                                         default_value);
    mca_base_param_lookup_string (id, &param_value);
    return param_value;
}

static inline int
mca_ptl_elan_param_register_int (const char *param_name,
                                 int default_value)
{
    int         id;
    int         param_value;

    param_value = default_value;
    id = mca_base_param_register_int ("ptl", "elan", param_name, NULL,
                                      default_value);
    mca_base_param_lookup_int (id, &param_value);
    return param_value;
}

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
        mca_ptl_elan_param_register_int ("first_frag_size", 1984/*magic*/);
    mca_ptl_elan.super.ptl_min_frag_size =
        mca_ptl_elan_param_register_int ("min_frag_size", 320);
    mca_ptl_elan.super.ptl_max_frag_size =
        mca_ptl_elan_param_register_int ("max_frag_size", -1);

#if 0 /* These parameters not very useful */
    /* register ELAN module parameters */
    mca_ptl_elan_module.elan_free_list_num =
        mca_ptl_elan_param_register_int ("free_list_num", 64);
    mca_ptl_elan_module.elan_free_list_max =
        mca_ptl_elan_param_register_int ("free_list_max", -1);
    mca_ptl_elan_module.elan_free_list_inc =
        mca_ptl_elan_param_register_int ("free_list_inc", 64);

    /* initialize state */
    mca_ptl_elan_module.elan_state = NULL;
    mca_ptl_elan_module.elan_ptls = NULL;
    mca_ptl_elan_module.elan_num_ptls = 0;

    /* initialize objects */
    OBJ_CONSTRUCT (&mca_ptl_elan_module.elan_lock, ompi_mutex_t);
    OBJ_CONSTRUCT (&mca_ptl_elan_module.elan_procs, ompi_list_t);
#endif

    return OMPI_SUCCESS;
}


int
mca_ptl_elan_module_close (void)
{
    if (mca_ptl_elan_module.elan_reqs_free.fl_num_allocated !=
        mca_ptl_elan_module.elan_reqs_free.super.ompi_list_length) {
        ompi_output (0, "elan requests: %d allocated %d returned\n",
                     mca_ptl_elan_module.elan_reqs_free.fl_num_allocated,
                     mca_ptl_elan_module.elan_reqs_free.super.ompi_list_length);
    }

    if (mca_ptl_elan_module.elan_events_free.fl_num_allocated !=
        mca_ptl_elan_module.elan_events_free.super.ompi_list_length) {
    }

    if (NULL != mca_ptl_elan_module.elan_ptls) {
        int         i;
        for (i = mca_ptl_elan_module.elan_num_ptls; i > 0; i--) {
            free (mca_ptl_elan_module.elan_ptls[i - 1]);
        }
        free (mca_ptl_elan_module.elan_ptls);
    }

    /* Free the empty list holders */
    OBJ_DESTRUCT (&(mca_ptl_elan_module.elan_reqs));
    OBJ_DESTRUCT (&(mca_ptl_elan_module.elan_prog_events));
    OBJ_DESTRUCT (&(mca_ptl_elan_module.elan_comp_events));
    OBJ_DESTRUCT (&(mca_ptl_elan_module.elan_procs));
    OBJ_DESTRUCT (&(mca_ptl_elan_module.elan_lock));

    /* TODO: check if this is needed. 
     *       Free the free lists */
    OBJ_DESTRUCT (&(mca_ptl_elan_module.elan_reqs_free));
    OBJ_DESTRUCT (&(mca_ptl_elan_module.elan_events_free));

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

    if ((rc = ompi_event_init ()) != OMPI_SUCCESS) {
        ompi_output (0,
                     "mca_ptl_elan_module_init: "
                     "unable to initialize event dispatch thread: %d\n",
                     rc);
        return NULL;
    }

#if ELAN_COMP
    /* initialize free lists */
    ompi_free_list_init (&(mca_ptl_elan_module.elan_reqs_free),
                         sizeof (mca_ptl_elan_send_request_t),
                         OBJ_CLASS (mca_ptl_elan_send_request_t),
                         mca_ptl_elan_module.elan_free_list_num,
                         mca_ptl_elan_module.elan_free_list_max,
                         mca_ptl_elan_module.elan_free_list_inc, NULL);

    /* use default allocator */
    ompi_free_list_init (&mca_ptl_elan_module.elan_events_free,
                         sizeof (mca_ptl_elan_send_frag_t),
                         OBJ_CLASS (mca_ptl_elan_send_frag_t),
                         mca_ptl_elan_module.elan_free_list_num,
                         mca_ptl_elan_module.elan_free_list_max,
                         mca_ptl_elan_module.elan_free_list_inc, NULL);
#endif

    /* open basic elan device */
    if (OMPI_SUCCESS != ompi_mca_ptl_elan_init(&mca_ptl_elan_module)) {
        return NULL;
    }

    /* setup communication infrastructure */
    if (OMPI_SUCCESS != ompi_mca_ptl_elan_setup (&mca_ptl_elan_module)) {
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

    ptls = (mca_ptl_elan_t **) malloc (mca_ptl_elan_module.elan_num_ptls *
                                       sizeof (mca_ptl_t *));

    if (NULL == ptls) {
        return NULL;
    }

    memcpy (ptls, mca_ptl_elan_module.elan_ptls,
            mca_ptl_elan_module.elan_num_ptls * sizeof (mca_ptl_elan_t *));
    *num_ptls = mca_ptl_elan_module.elan_num_ptls;

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
            ompi_event_add (&mca_ptl_elan_module.elan_recv_event, 0);
        } else {
            ompi_event_del (&mca_ptl_elan_module.elan_recv_event);
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

