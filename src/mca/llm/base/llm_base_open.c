/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "runtime/runtime_types.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/llm/base/static-components.h"

/*
 * Global variables
 */
int mca_llm_base_output = -1;
ompi_list_t mca_llm_base_components_available;
mca_llm_base_component_t mca_llm_base_selected_component;

ompi_mutex_t mca_llm_base_parse_mutex;

/* give us a way to hook in for base unit tests */
void
mca_llm_base_setup(void)
{
    /* initialize the internal mutex */
    OBJ_CONSTRUCT(&mca_llm_base_parse_mutex, ompi_mutex_t);
}

/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_llm_base_open(void)
{
    mca_llm_base_setup();

  /* Open up all available components */
  if (OMPI_SUCCESS != 
      mca_base_components_open("llm", 0, mca_llm_base_static_components, 
                               &mca_llm_base_components_available)) {
    return OMPI_ERROR;
  }

  /* All done */
  return OMPI_SUCCESS;
}
