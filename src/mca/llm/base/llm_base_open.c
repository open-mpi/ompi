/*
 * $HEADER$
 */


#include "ompi_config.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"
#include "runtime/runtime_types.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#ifdef WIN32
    const mca_base_component_t *mca_llm_base_static_components[] = {NULL};
#else 
#include "mca/llm/base/static-components.h"
#endif

/*
 * Global variables
 */
int mca_llm_base_output = 0;
ompi_list_t mca_llm_base_components_available;

/* mutex for the host file parsing code in the base */
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
    int ret;

    mca_llm_base_setup();

  /* Open up all available components */
  if (OMPI_SUCCESS != 
      (ret = mca_base_components_open("llm", 0, mca_llm_base_static_components, 
                                      &mca_llm_base_components_available))) {
    return ret;
  }

  /* All done */
  return OMPI_SUCCESS;
}


/*
 * Object maintenance code
 */

/** constructor for \c mca_llm_base_hostfile_data_t */
static
void
llm_base_int_hostfile_data_construct(ompi_object_t *obj)
{
    mca_llm_base_hostfile_data_t *data = (mca_llm_base_hostfile_data_t*) obj;
    data->hostlist = OBJ_NEW(ompi_list_t);
}


/** destructor for \c mca_llm_base_hostfile_data_t */
static
void
llm_base_int_hostfile_data_destruct(ompi_object_t *obj)
{
    mca_llm_base_hostfile_data_t *data = (mca_llm_base_hostfile_data_t*) obj;
    mca_llm_base_deallocate(data->hostlist);
}


/** constructor for \c mca_llm_base_hostfile_node_t */
static
void
llm_base_int_hostfile_node_construct(ompi_object_t *obj)
{
    mca_llm_base_hostfile_node_t *node = (mca_llm_base_hostfile_node_t*) obj;
    (node->hostname)[0] = '\0';
    node->count = 0;
    node->given_count = 0;
    node->info = OBJ_NEW(ompi_list_t);
}


/** destructor for \c mca_llm_base_hostfile_node_t */
static
void
llm_base_int_hostfile_node_destruct(ompi_object_t *obj)
{
    mca_llm_base_hostfile_node_t *node = (mca_llm_base_hostfile_node_t*) obj;
    ompi_list_item_t *item;

    if (NULL == node->info) return;

    while (NULL != (item = ompi_list_remove_first(node->info))) {
        OBJ_RELEASE(item);
    }

    OBJ_RELEASE(node->info);
}


/** create instance information for \c mca_llm_base_hostfile_data_t */
OBJ_CLASS_INSTANCE(mca_llm_base_hostfile_data_t, 
                   ompi_rte_node_allocation_data_t,
                   llm_base_int_hostfile_data_construct,
                   llm_base_int_hostfile_data_destruct);
/** create instance information for \c mca_llm_base_hostfile_node_t */
OBJ_CLASS_INSTANCE(mca_llm_base_hostfile_node_t,
                   ompi_list_item_t,
                   llm_base_int_hostfile_node_construct,
                   llm_base_int_hostfile_node_destruct);
