/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/ns/base/static-components.h"

/*
 * globals
 */

/*
 * Global variables
 */
int mca_ns_base_output = -1;
mca_ns_base_module_t ompi_name_server;
bool mca_ns_base_selected = false;
ompi_list_t mca_ns_base_components_available;
mca_ns_base_component_t mca_ns_base_selected_component;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_ns_base_open(void)
{
    int id;
    char *replica;

    /* check the environment for replica information */
    id = mca_base_param_register_string("ns", "base", "replica", NULL, NULL);
    mca_base_param_lookup_string(id, &replica);
    if (NULL != replica) {
	mca_oob_set_contact_info(replica);
	ompi_process_info.ns_replica = ns_base_create_process_name(0,0,0);
	mca_oob_parse_contact_info(replica, ompi_process_info.ns_replica, NULL);
    }


  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("ns", 0, mca_ns_base_static_components, 
                               &mca_ns_base_components_available)) {
    return OMPI_ERROR;
  }

  /* setup output for debug messages */
  if (!ompi_output_init) {  /* can't open output */
      return OMPI_ERROR;
  }

  mca_ns_base_output = ompi_output_open(NULL);

  /* All done */

  return OMPI_SUCCESS;
}
