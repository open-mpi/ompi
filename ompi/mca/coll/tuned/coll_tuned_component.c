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

/*
 * Public string showing the coll ompi_tuned component version number
 */
const char *mca_coll_tuned_component_version_string =
  "Open MPI tuned collective MCA component version " OMPI_VERSION;

/*
 * Global variable
 */
int mca_coll_tuned_priority_param = -1;
int mca_coll_tuned_use_dynamic_rules_param = -1;
int mca_coll_tuned_init_tree_fanout_param = -1;
int mca_coll_tuned_init_chain_fanout_param = -1;
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
    printf("Tuned_open called\n");
/*     mca_coll_tuned_component_t *ct = &mca_coll_tuned_component; */

    /* Use a low priority, but allow other components to be lower */
    
    mca_coll_tuned_priority_param = 
        mca_base_param_register_int("coll", "tuned", "priority", NULL, 30);

    /* by default DISABLE dynamic rules and force the use of fixed [if] rules */
    mca_coll_tuned_use_dynamic_rules_param = 
        mca_base_param_register_int("coll", "tuned", "use_dynamic_rules",
                                    NULL, 0);

    /* some initial guesses at topology parameters */
    mca_coll_tuned_init_tree_fanout_param = 
        mca_base_param_register_int("coll", "tuned", "init_tree_fanout", 
                                    NULL, 4);

    mca_coll_tuned_init_chain_fanout_param = 
        mca_base_param_register_int("coll", "tuned", "init_chain_fanout", 
                                    NULL, 4);

/* use the newer interface rsn */
/*     mca_coll_tuned_priority_param = mca_base_param_reg_int(&(ct->super), "priority", "Priority of the tuned coll component", */
/*                            false, false, 30, NULL); */

/*     mca_base_param_reg_int(&(ct->super), "init_tree_fanout", "Fan out used for [balanced] tree topologies in the tuned coll component", */
/*                            false, false, 2, NULL); */

/*     mca_base_param_reg_int(&(ct->super), "init_chain_fanout",  */
/*                             "Fan out used for chain [1 fanout followed by pipelines] topology in the tuned coll component", */
/*                            false, false, 2, NULL); */


    return OMPI_SUCCESS;
}

