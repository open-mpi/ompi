/*
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include "mpi.h"
#include "mca/coll/coll.h"
#include "coll_basic.h"

/*
 * Public string showing the coll ompi_basic component version number
 */
const char *mca_coll_basic_component_version_string =
  "OMPI/MPI basic collective MCA component version " MCA_coll_basic_FULL_VERSION;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_coll_base_component_1_0_0_t mca_coll_basic_component = {

  /* First, the mca_component_t struct containing meta information
     about the component itself */

  {
    /* Indicate that we are a coll v1.0.0 component (which also implies a
       specific MCA version) */

    MCA_COLL_BASE_VERSION_1_0_0,

    /* Component name and version */

    "basic",
    MCA_coll_basic_MAJOR_VERSION,
    MCA_coll_basic_MINOR_VERSION,
    MCA_coll_basic_RELEASE_VERSION,

    /* Component open and close functions */

    NULL,
    NULL
  },

  /* Next the MCA v1.0.0 component meta data */

  {
   /* Whether the component is checkpointable or not */

   true
  },

  /* Initialization / querying functions */

  mca_coll_basic_init_query,
  mca_coll_basic_comm_query,
  NULL
};
