/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#include "opal/include/constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/timer/timer.h"
#include "opal/mca/timer/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/timer/base/static-components.h"


/*
 * Globals
 */
opal_list_t opal_timer_base_components_opened;


/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int opal_timer_base_open(void)
{
    /* Open up all available components */
    if (OMPI_SUCCESS !=
        mca_base_components_open("timer", 0,
                                 mca_timer_base_static_components,
                                 &opal_timer_base_components_opened, 
                                 true)) {
        return OPAL_ERROR;
    }

    /* All done */
    return OPAL_SUCCESS;
}
