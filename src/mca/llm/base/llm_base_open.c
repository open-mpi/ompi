/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"


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
mca_llm_base_module_t mca_llm;

/*
 * LLM interface type support
 */
static
void
mca_llm_base_node_construct(ompi_object_t *obj)
{
    mca_llm_base_node_t *node = (mca_llm_base_node_t*) obj;
    OBJ_CONSTRUCT(&(node->info), ompi_list_t);
}

static
void
mca_llm_base_node_destruct(ompi_object_t *obj)
{
    mca_llm_base_node_t *node = (mca_llm_base_node_t*) obj;
    OBJ_DESTRUCT(&(node->info));
}

static
void
mca_llm_base_valuepair_construct(ompi_object_t *obj)
{
    mca_llm_base_valuepair_t *valpair = (mca_llm_base_valuepair_t*) obj;
    valpair->key = NULL;
    valpair->value = NULL;
}

static
void
mca_llm_base_valuepair_destruct(ompi_object_t *obj)
{
    mca_llm_base_valuepair_t *valpair = (mca_llm_base_valuepair_t*) obj;
    if (NULL != valpair->key) free(valpair->key);
    if (NULL != valpair->value) free(valpair->value);
}

OBJ_CLASS_INSTANCE(mca_llm_base_node_t, ompi_list_item_t, 
                   mca_llm_base_node_construct, mca_llm_base_node_destruct);
OBJ_CLASS_INSTANCE(mca_llm_base_valuepair_t, ompi_list_item_t, 
                   mca_llm_base_valuepair_construct,
                   mca_llm_base_valuepair_destruct);

ompi_mutex_t mca_llm_base_parse_mutex;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_llm_base_open(void)
{
    /* initialize the internal mutex */
    OBJ_CONSTRUCT(&mca_llm_base_parse_mutex, ompi_mutex_t);

  /* Open up all available components */
  if (OMPI_SUCCESS != 
      mca_base_components_open("llm", 0, mca_llm_base_static_components, 
                               &mca_llm_base_components_available)) {
    return OMPI_ERROR;
  }

  /* All done */
  return OMPI_SUCCESS;
}
