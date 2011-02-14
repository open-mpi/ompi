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
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/mca/notifier/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/notifier/base/static-components.h"

#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * Global variables
 */
int orte_notifier_base_output = -1;
orte_notifier_base_severity_t orte_notifier_threshold_severity = 
    ORTE_NOTIFIER_ERROR;
opal_list_t orte_notifier_base_components_available;
opal_list_t orte_notifier_base_selected_modules;
opal_list_t orte_notifier_log_selected_modules;
opal_list_t orte_notifier_help_selected_modules;
opal_list_t orte_notifier_log_peer_selected_modules;
opal_list_t orte_notifier_log_event_selected_modules;

orte_notifier_API_module_t orte_notifier = {
    orte_notifier_log,
    orte_notifier_show_help,
    orte_notifier_log_peer,
};
#endif

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_notifier_base_open(void)
{

#if !ORTE_DISABLE_FULL_SUPPORT
    char *level;
    
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_notifier_base_output = opal_output_open(NULL);
    
    /* let the user define a base level of severity to report */
    mca_base_param_reg_string_name("notifier", "threshold_severity",
                                   "Report all events at or above this severity [default: error]",
                                   false, false, "error", &level);
    if (0 == strncasecmp(level, "emerg", strlen("emerg"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_EMERG;
    } else if (0 == strncasecmp(level, "alert", strlen("alert"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_ALERT;
    } else if (0 == strncasecmp(level, "crit", strlen("crit"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_CRIT;
    } else if (0 == strncasecmp(level, "warn", strlen("warn"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_WARN;
    } else if (0 == strncasecmp(level, "notice", strlen("notice"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_NOTICE;
    } else if (0 == strncasecmp(level, "info", strlen("info"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_INFO;
    } else if (0 == strncasecmp(level, "debug", strlen("debug"))) {
        orte_notifier_threshold_severity = ORTE_NOTIFIER_DEBUG;
    } else if (0 != strncasecmp(level, "error", strlen("error"))) {
        opal_output(0, "Unknown notifier level");
        return ORTE_ERROR;
    }
    free(level);
    
    OBJ_CONSTRUCT(&orte_notifier_base_selected_modules, opal_list_t);
    OBJ_CONSTRUCT(&orte_notifier_log_selected_modules, opal_list_t);
    OBJ_CONSTRUCT(&orte_notifier_help_selected_modules, opal_list_t);
    OBJ_CONSTRUCT(&orte_notifier_log_peer_selected_modules, opal_list_t);
    OBJ_CONSTRUCT(&orte_notifier_log_event_selected_modules, opal_list_t);

    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("notifier", orte_notifier_base_output,
                                 mca_notifier_base_static_components,
                                 &orte_notifier_base_components_available, 
                                 true)) {
        return ORTE_ERROR;
    }

    /* All done */
#endif
    
    return ORTE_SUCCESS;
}
