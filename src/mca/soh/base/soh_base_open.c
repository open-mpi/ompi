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

#include "mca/soh/base/base.h"

#include "stdio.h" /* just for gef debug */


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/soh/base/static-components.h"

/*
 * globals
 */

/*
 * Global variables
 */
orte_soh_base_t orte_soh_base;

orte_soh_base_module_t orte_soh = {

	orte_soh_base_get_proc_soh,
    orte_soh_base_set_proc_soh,
	orte_soh_base_get_node_soh_not_available,
    orte_soh_base_set_node_soh_not_available,
    orte_soh_base_module_finalize_not_available
};

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_soh_base_open(void)
{

    int param, value;

/* fprintf(stderr,"orte_soh_base_open:enter\n"); */

  /* setup output for debug messages */

    orte_soh_base.soh_output = ompi_output_open(NULL);
    param = mca_base_param_register_int("soh", "base", "verbose", NULL, 0);
    mca_base_param_lookup_int(param, &value);
    if (value != 0) {
        orte_soh_base.soh_output = ompi_output_open(NULL);
    } else {
        orte_soh_base.soh_output = -1;
    }


  /* Open up all available components */

  if (OMPI_SUCCESS != 
		mca_base_components_open("soh", 0, mca_soh_base_static_components,
                                 &orte_soh_base.soh_components)) {

/* fprintf(stderr,"orte_soh_base_open:failed\n"); */
    return OMPI_ERROR;
  }

  /* All done */
/* fprintf(stderr,"orte_soh_base_open:success\n"); */

  return OMPI_SUCCESS;
}
