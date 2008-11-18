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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"

#include "orte/mca/notifier/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/notifier/base/static-components.h"

static void orte_base_log(int priority, const char *msg, ...);
static void orte_log_show_help(int priority, const char *file, const char *topic, ...);
/*
 * Global variables
 */
int orte_notifier_base_output = -1;
orte_notifier_base_module_t orte_notifier = {
    NULL,
    NULL,
    orte_base_log,
    orte_log_show_help
};
opal_list_t mca_notifier_base_components_available;
orte_notifier_base_component_t mca_notifier_base_selected_component;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_notifier_base_open(void)
{
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_notifier_base_output = opal_output_open(NULL);
    
    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("notifier", orte_notifier_base_output,
                                 mca_notifier_base_static_components,
                                 &mca_notifier_base_components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}

static void orte_base_log(int priority, const char *msg, ...)
{
    /* just do nothing - it is here just so
     * someone calling it won't segv
     */
}

static void orte_log_show_help(int priority, const char *file, const char *topic, ...)
{
    /* just do nothing - it is here just so someone calling it won't
     * segv.  Put in va_start/va_end just so that compilers won't
     * complain.
     */
    va_list ap;
    va_start(ap, topic);
    va_end(ap);
}
