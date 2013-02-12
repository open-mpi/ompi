/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include <stdio.h>

#include "oshmem_config.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "oshmem/mca/atomic/base/static-components.h"


/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
int mca_atomic_base_output = -1;

bool mca_atomic_base_components_opened_valid = false;
opal_list_t mca_atomic_base_components_opened;

OBJ_CLASS_INSTANCE(mca_atomic_base_module_t, opal_object_t, NULL, NULL);

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_atomic_base_open(void)
{
    /* Open an output stream for this framework */
    int value = -1;

    mca_atomic_base_output = opal_output_open(NULL);
    mca_base_param_reg_int_name("atomic_base","verbose",
			"Verbose level of the shmem atomic component",false,false,0,&value);
	opal_output_set_verbosity(mca_atomic_base_output, value);

    /* Open up all available components */
    if (OSHMEM_SUCCESS !=
        mca_base_components_open("atomic", mca_atomic_base_output,
                                 mca_atomic_base_static_components,
                                 &mca_atomic_base_components_opened, true)) {
        return OSHMEM_ERROR;
    }
    mca_atomic_base_components_opened_valid = true;

    /* All done */

    return OSHMEM_SUCCESS;
}
