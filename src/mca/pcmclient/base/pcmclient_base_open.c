/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/pcmclient/base/static-components.h"


/*
 * Global variables
 */
int mca_pcmclient_base_output = 0;
mca_pcmclient_base_module_t mca_pcmclient;
ompi_list_t mca_pcmclient_base_components_available;
mca_pcmclient_base_component_t mca_pcmclient_base_selected_component;
mca_pcmclient_base_module_t mca_pcmclient;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_pcmclient_base_open(void)
{
  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("pcmclient", 0, 
                               mca_pcmclient_base_static_components, 
                               &mca_pcmclient_base_components_available)) {
    return OMPI_ERROR;
  }

  /* All done */

  return OMPI_SUCCESS;
}
