/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "include/constants.h"
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
mca_ns_base_module_t ompi_name_server = {
    mca_ns_base_create_cellid_not_available,
    mca_ns_base_assign_cellid_to_process,
    mca_ns_base_create_jobid_not_available,
    mca_ns_base_create_process_name,
    mca_ns_base_copy_process_name,
    mca_ns_base_convert_string_to_process_name,
    mca_ns_base_get_vpid_range_not_available,
    mca_ns_base_free_name,
    mca_ns_base_get_proc_name_string,
    mca_ns_base_get_vpid_string,
    mca_ns_base_convert_vpid_to_string,
    mca_ns_base_convert_string_to_vpid,
    mca_ns_base_get_jobid_string,
    mca_ns_base_convert_jobid_to_string,
    mca_ns_base_convert_string_to_jobid,
    mca_ns_base_get_cellid_string,
    mca_ns_base_convert_cellid_to_string,
    mca_ns_base_convert_string_to_cellid,
    mca_ns_base_get_vpid,
    mca_ns_base_get_jobid,
    mca_ns_base_get_cellid,
    mca_ns_base_compare,
    mca_ns_base_pack_name,
    mca_ns_base_unpack_name,
    mca_ns_base_pack_cellid,
    mca_ns_base_unpack_cellid,
    mca_ns_base_pack_jobid,
    mca_ns_base_unpack_jobid
};
bool mca_ns_base_selected = false;
ompi_list_t mca_ns_base_components_available;
mca_ns_base_component_t mca_ns_base_selected_component;


/* constructor - used to initialize namelist instance */
static void ompi_name_server_namelist_construct(ompi_name_server_namelist_t* list)
{
    list->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void ompi_name_server_namelist_destructor(ompi_name_server_namelist_t* list)
{
    if (NULL != list->name) {
	free(list->name);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_name_server_namelist_t,              /* type name */
		   ompi_list_item_t,                        /* parent "class" name */
		   ompi_name_server_namelist_construct,    /* constructor */
		   ompi_name_server_namelist_destructor);  /* destructor */



/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_ns_base_open(void)
{
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
