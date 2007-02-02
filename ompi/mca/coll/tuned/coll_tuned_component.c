/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"
#include "coll_tuned.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"
#include "coll_tuned.h"

#include "opal/util/output.h"

/*
 * Public string showing the coll ompi_tuned component version number
 */
const char *ompi_coll_tuned_component_version_string =
  "Open MPI tuned collective MCA component version " OMPI_VERSION;

/*
 * Global variable
 */
int   ompi_coll_tuned_stream = -1;
int   ompi_coll_tuned_priority = 30;
int   ompi_coll_tuned_preallocate_memory_comm_size_limit = (32*1024);
int   ompi_coll_tuned_use_dynamic_rules = 0;
char* ompi_coll_tuned_dynamic_rules_filename = (char*) NULL;
int   ompi_coll_tuned_init_tree_fanout = 4;
int   ompi_coll_tuned_init_chain_fanout = 4;

/* forced alogrithm variables */
/* indices for the MCA parameters */
coll_tuned_force_algorithm_mca_param_indices_t ompi_coll_tuned_forced_params[COLLCOUNT];
/* max algorithm values */
int ompi_coll_tuned_forced_max_algorithms[COLLCOUNT];


/*
 * Local function
 */
static int tuned_open(void);
static int tuned_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_tuned_component_t mca_coll_tuned_component = {

    /* First, fill in the super (mca_coll_base_component_1_0_0_t) */

    {   

        /* First, the mca_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a coll v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_COLL_BASE_VERSION_1_0_0,

            /* Component name and version */

            "tuned",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            tuned_open,
            tuned_close
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            true
        },

        /* Initialization / querying functions */

        ompi_coll_tuned_init_query,
        ompi_coll_tuned_comm_query,
        NULL
    },

    /* priority of the module */
    0,

    /* Tuned component specific information */
    /* Note some of this WAS in the module */
    NULL /* ompi_coll_alg_rule_t ptr */       
};


static int tuned_open(void)
{
#if OMPI_ENABLE_DEBUG
    {
        int param;

        param = mca_base_param_find("coll", NULL, "base_verbose");
        if (param >= 0) {
            int verbose;
            mca_base_param_lookup_int(param, &verbose);
            if (verbose > 0) {
                ompi_coll_tuned_stream = opal_output_open(NULL);
            }
        }
    }
#endif  /* OMPI_ENABLE_DEBUG */

    /* Use a low priority, but allow other components to be lower */    
    mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "priority",
                           "Priority of the tuned coll component",
                           false, false, ompi_coll_tuned_priority,
                           &ompi_coll_tuned_priority);

    /* parameter for pre-allocated memory requests etc */
    mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "pre_allocate_memory_comm_size_limit",
                           "Size of communicator were we stop pre-allocating memory for the fixed internal buffer used for message requests etc that is hung off the communicator data segment. I.e. if you have a 100'000 nodes you might not want to pre-allocate 200'000 request handle slots per communicator instance!",
                           false, false, ompi_coll_tuned_preallocate_memory_comm_size_limit,
                           &ompi_coll_tuned_preallocate_memory_comm_size_limit);
    
    /* some initial guesses at topology parameters */
    mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "init_tree_fanout",
                           "Inital fanout used in the tree topologies for each communicator. This is only an initial guess, if a tuned collective needs a different fanout for an operation, it build it dynamically. This parameter is only for the first guess and might save a little time",
                           false, false, ompi_coll_tuned_init_tree_fanout,
                           &ompi_coll_tuned_init_tree_fanout);

    mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "init_chain_fanout",
                           "Inital fanout used in the chain (fanout followed by pipeline) topologies for each communicator. This is only an initial guess, if a tuned collective needs a different fanout for an operation, it build it dynamically. This parameter is only for the first guess and might save a little time",
                           false, false, ompi_coll_tuned_init_chain_fanout,
                           &ompi_coll_tuned_init_chain_fanout);

    /* now check that the user hasn't overrode any of the decision functions if dynamic rules are enabled */
    /* the user can redo this before every comm dup/create if they like */
    /* this is useful for benchmarking and user knows best tuning */
    /* as this is the component we only lookup the indicies of the mca params */
    /* the actual values are looked up during comm create via module init */
   
    /* intra functions first */
    /* if dynamic rules allowed then look up dynamic rules config filename, else we leave it an empty filename (NULL) */
    /* by default DISABLE dynamic rules and instead use fixed [if based] rules */
    mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                           "use_dynamic_rules",
                           "Switch used to decide if we use static (compiled/if statements) or dynamic (built at runtime) decision function rules",
                           false, false, ompi_coll_tuned_use_dynamic_rules,
                           &ompi_coll_tuned_use_dynamic_rules);

    if (ompi_coll_tuned_use_dynamic_rules) {
        mca_base_param_reg_string(&mca_coll_tuned_component.super.collm_version,
                                  "dynamic_rules_filename",
                                  "Filename of configuration file that contains the dynamic (@runtime) decision function rules",
                                  false, false, ompi_coll_tuned_dynamic_rules_filename,
                                  &ompi_coll_tuned_dynamic_rules_filename);
        ompi_coll_tuned_allreduce_intra_check_forced_init(&ompi_coll_tuned_forced_params[ALLREDUCE]);
        ompi_coll_tuned_alltoall_intra_check_forced_init(&ompi_coll_tuned_forced_params[ALLTOALL]);
        ompi_coll_tuned_allgather_intra_check_forced_init(&ompi_coll_tuned_forced_params[ALLGATHER]);
        /*ompi_coll_tuned_alltoallv_intra_check_forced_init(&ompi_coll_tuned_forced_params[ALLTOALLV]); */
        ompi_coll_tuned_barrier_intra_check_forced_init(&ompi_coll_tuned_forced_params[BARRIER]);
        ompi_coll_tuned_bcast_intra_check_forced_init(&ompi_coll_tuned_forced_params[BCAST]);
        ompi_coll_tuned_reduce_intra_check_forced_init(&ompi_coll_tuned_forced_params[REDUCE]);
    }

    OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:component_open: done!"));

    return OMPI_SUCCESS;
}

/* here we should clean up state stored on the component */
/* i.e. alg table and dynamic changable rules if allocated etc */
static int tuned_close(void)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:component_close: called"));

    /* dealloc alg table if allocated */
    /* dealloc dynamic changable rules if allocated */

    OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:component_close: done!"));

    return OMPI_SUCCESS;
}

