/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
#include "mca/coll/coll.h"
#include "coll_tuned.h"

#include "opal/util/output.h"

/*
 * Public string showing the coll ompi_tuned component version number
 */
const char *mca_coll_tuned_component_version_string =
  "Open MPI tuned collective MCA component version " OMPI_VERSION;

/*
 * Global variable
 */
int   mca_coll_tuned_stream = -1;
int   mca_coll_tuned_priority = 30;
int   mca_coll_tuned_preallocate_memory_comm_size_limit = (32*1024);
int   mca_coll_tuned_use_dynamic_rules = 0;
char* mca_coll_tuned_dynamic_rules_filename = (char*) NULL;
int   mca_coll_tuned_init_tree_fanout = 4;
int   mca_coll_tuned_init_chain_fanout = 4;

/* forced alogrithm variables */
int mca_coll_tuned_allreduce_forced_choice = 0;
int mca_coll_tuned_allreduce_forced_segsize = 0;
int mca_coll_tuned_allreduce_forced_chain_fanout = 0;
int mca_coll_tuned_allreduce_forced_tree_fanout = 0;

int mca_coll_tuned_alltoall_forced_choice = 0;
int mca_coll_tuned_alltoall_forced_segsize = 0;
int mca_coll_tuned_alltoall_forced_chain_fanout = 0;
int mca_coll_tuned_alltoall_forced_tree_fanout = 0;

int mca_coll_tuned_barrier_forced_choice = 0;

int mca_coll_tuned_bcast_forced_choice = 0;
int mca_coll_tuned_bcast_forced_segsize = 0;
int mca_coll_tuned_bcast_forced_chain_fanout = 0;
int mca_coll_tuned_bcast_forced_tree_fanout = 0;

int mca_coll_tuned_reduce_forced_choice = 0;
int mca_coll_tuned_reduce_forced_segsize = 0;
int mca_coll_tuned_reduce_forced_chain_fanout = 0;
int mca_coll_tuned_reduce_forced_tree_fanout = 0;


/*
 * Local function
 */
static int tuned_open(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_coll_base_component_1_0_0_t mca_coll_tuned_component = {

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
    NULL
  },

  /* Next the MCA v1.0.0 component meta data */

  {
   /* Whether the component is checkpointable or not */

   true
  },

  /* Initialization / querying functions */

  mca_coll_tuned_init_query,
  mca_coll_tuned_comm_query,
  NULL
};


static int tuned_open(void)
{
    int param;

/*     mca_coll_tuned_component_t *ct = &mca_coll_tuned_component; */

    /* Use a low priority, but allow other components to be lower */
    
    mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "priority",
                           "Priority of the tuned coll component",
                           false, false, mca_coll_tuned_priority,
                           &mca_coll_tuned_priority);

    /* parameter for pre-allocated memory requests etc */
    mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "pre_allocate_memory_comm_size_limit",
                           "Size of communicator were we stop pre-allocating memory for the fixed internal buffer used for message requests etc that is hung off the communicator data segment. I.e. if you have a 100'000 nodes you might not want to pre-allocate 200'000 request handle slots per communicator instance!",
                           false, false, mca_coll_tuned_preallocate_memory_comm_size_limit,
                           &mca_coll_tuned_preallocate_memory_comm_size_limit);
    
    /* by default DISABLE dynamic rules and instead use fixed [if based] rules */
    mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "use_dynamic_rules",
                           "Switch used to decide if we use static (if statements) or dynamic (built at runtime) decision function rules",
                           false, false, mca_coll_tuned_use_dynamic_rules,
                           &mca_coll_tuned_use_dynamic_rules);

    /* if dynamic rules allowed then look up dynamic rules config filename, else we leave it an empty filename (NULL) */
    if (mca_coll_tuned_use_dynamic_rules) {
/*         char *default_name; */
/*         asprintf(&default_name, "~/.openmpi/openmpi-coll-tuned-params.conf"); */
        mca_base_param_reg_string(&mca_coll_tuned_component.collm_version,
                               "dynamic_rules_filename",
                               "Filename of configuration file that contains the dynamic (@runtime) decision function rules",
                               false, false, mca_coll_tuned_dynamic_rules_filename,
                               &mca_coll_tuned_dynamic_rules_filename);
    }

    /* some initial guesses at topology parameters */
    mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "init_tree_fanout",
                           "Inital fanout used in the tree topologies for each communicator. This is only an initial guess, if a tuned collective needs a different fanout for an operation, it build it dynamically. This parameter is only for the first guess and might save a little time",
                           false, false, mca_coll_tuned_init_tree_fanout,
                           &mca_coll_tuned_init_tree_fanout);

    mca_base_param_reg_int(&mca_coll_tuned_component.collm_version,
                           "init_chain_fanout",
                           "Inital fanout used in the chain (fanout followed by pipeline) topologies for each communicator. This is only an initial guess, if a tuned collective needs a different fanout for an operation, it build it dynamically. This parameter is only for the first guess and might save a little time",
                           false, false, mca_coll_tuned_init_chain_fanout,
                           &mca_coll_tuned_init_chain_fanout);

    param = mca_base_param_find("coll", NULL, "base_verbose");
    if (param >= 0) {
        int verbose;
        mca_base_param_lookup_int(param, &verbose);
        if (verbose > 0) {
           mca_coll_tuned_stream = opal_output_open(NULL);
        }
    }

    /* now check that the user hasn't overrode any of the decision functions */
    /* the user can do this before every comm dup/create if they like */
    /* this is useful for benchmarking and user knows best tuning */
   
    /* intra functions first */
    mca_coll_tuned_allreduce_intra_check_forced();
    mca_coll_tuned_alltoall_intra_check_forced();
    mca_coll_tuned_barrier_intra_check_forced();
    mca_coll_tuned_bcast_intra_check_forced();
    mca_coll_tuned_reduce_intra_check_forced();




    OPAL_OUTPUT((mca_coll_tuned_stream, "coll:tuned:component_open: done!"));

    return OMPI_SUCCESS;
}

