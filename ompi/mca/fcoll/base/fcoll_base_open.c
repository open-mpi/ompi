/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include "ompi/class/ompi_free_list.h"
#include "orte/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"

#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#ifdef __WINDOWS__
    const mca_base_component_t *mca_fcoll_base_static_components[] = {NULL};
#else 
#include "ompi/mca/fcoll/base/static-components.h"
#endif

/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
int mca_fcoll_base_param = -1;
int mca_fcoll_base_output = -1;

opal_list_t mca_fcoll_base_components_opened;
opal_list_t mca_fcoll_base_components_available;

bool mca_fcoll_base_components_available_valid = false;
bool mca_fcoll_base_components_opened_valid = false;

mca_fcoll_base_component_t mca_fcoll_base_selected_component;
mca_fcoll_base_module_t mca_fcoll;

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_fcoll_base_open(void)
{
    /* Open an output stream for this framework */

    mca_fcoll_base_output = opal_output_open(NULL);

     /* Open up all available components */

    if (OMPI_SUCCESS != 
        mca_base_components_open("fcoll", mca_fcoll_base_output,
                                 mca_fcoll_base_static_components, 
                                 &mca_fcoll_base_components_opened, true)) {
        return OMPI_ERROR;
    }
    mca_fcoll_base_components_opened_valid = true;

    /* Find the index of the MCA "fcoll" param for selection */
    
    mca_fcoll_base_param = mca_base_param_find("fcoll", "base", NULL);

    return OMPI_SUCCESS;
}
