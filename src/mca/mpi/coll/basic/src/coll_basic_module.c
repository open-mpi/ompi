/*
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "laminfo" from having to import
 * entire modules just to query their version and parameters.
 */

#include "lam_config.h"
#include "coll_basic_config.h"

#include "mpi.h"
#include "mca/mpi/coll/coll.h"
#include "coll_basic.h"

/*
 * Public string showing the coll lam_basic module version number
 */
const char *mca_coll_basic_module_version_string =
  "LAM/MPI basic collective MCA module version " MCA_COLL_BASIC_VERSION;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_coll_base_module_1_0_0_t mca_coll_basic_module = {

  /* First, the mca_module_t struct containing meta information
     about the module itself */

  {
    /* Indicate that we are a coll v1.0.0 module (which also implies a
       specific MCA version) */

    MCA_COLL_BASE_VERSION_1_0_0,

    /* Module name and version */

    "basic",
    MCA_COLL_BASIC_MAJOR_VERSION,
    MCA_COLL_BASIC_MINOR_VERSION,
    MCA_COLL_BASIC_RELEASE_VERSION,

    /* Module open and close functions */

    NULL,
    NULL
  },

  /* Next the MCA v1.0.0 module meta data */

  {
   /* Whether the module is checkpointable or not */

   true
  },

  /* Initialization / querying functions */

  mca_coll_basic_thread_query,
  mca_coll_basic_query
};
