/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "util/output.h"
#include "mca/gpr/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_module_t struct.
 */

#include "mca/gpr/base/static-components.h"

/*
 * globals
 */

/* constructor - used to initialize state of registry value instance */
static void ompi_registry_value_construct(ompi_registry_value_t* reg_val)
{
    reg_val->object = NULL;
    reg_val->object_size = -1;
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_value_destructor(ompi_registry_value_t* reg_val)
{
    if (NULL != reg_val->object) {
	free(reg_val->object);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_value_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_registry_value_construct, /* constructor */
		   ompi_registry_value_destructor); /* destructor */



/*
 * Global variables
 */
int mca_gpr_base_output = -1;
mca_gpr_base_module_t ompi_registry;
bool mca_gpr_base_selected = false;
ompi_list_t mca_gpr_base_components_available;
mca_gpr_base_component_t mca_gpr_base_selected_component;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_gpr_base_open(void)
{
    int checksize;

  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("gpr", 0, mca_gpr_base_static_components, 
                            &mca_gpr_base_components_available)) {
    return OMPI_ERROR;
  }

  /* setup output for debug messages */
  if (!ompi_output_init) {  /* can't open output */
      return OMPI_ERROR;
  }

  mca_gpr_base_output = ompi_output_open(NULL);
  checksize = ompi_list_get_size(&mca_gpr_base_components_available);
  ompi_output(mca_gpr_base_output, "number of components: %d", checksize);

  /* All done */

  return OMPI_SUCCESS;
}
