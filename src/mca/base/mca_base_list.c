/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "mca/base/base.h"


/* 
 * Local functions
 */
static void cl_constructor(ompi_object_t *obj);
static void cpl_constructor(ompi_object_t *obj);


/*
 * Class instance of the mca_base_component_list_item_t class
 */
OBJ_CLASS_INSTANCE(mca_base_component_list_item_t, 
                   ompi_list_item_t, cl_constructor, NULL);


/*
 * Class instance of the mca_base_component_priority_list_item_t class
 */
OBJ_CLASS_INSTANCE(mca_base_component_priority_list_item_t, 
                   ompi_list_item_t, cpl_constructor, NULL);


/*
 * Just do basic sentinel intialization
 */
static void cl_constructor(ompi_object_t *obj)
{
  mca_base_component_list_item_t *cli = (mca_base_component_list_item_t *) obj;
  cli->cli_component = NULL;
}


/*
 * Just do basic sentinel intialization
 */
static void cpl_constructor(ompi_object_t *obj)
{
  mca_base_component_priority_list_item_t *cpli = 
    (mca_base_component_priority_list_item_t *) obj;
  cpli->cpli_priority = -1;
  cpli->cpli_allow_multi_user_threads = false;
  cpli->cpli_have_hidden_threads = false;
  cpli->cpli_component = NULL;
}
