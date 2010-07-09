/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
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

#include "orte/mca/debugger/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/debugger/base/static-components.h"

/*
 * Global variables
 */
int orte_debugger_base_output = -1;
opal_list_t orte_debugger_base_components_available;

orte_debugger_base_module_t orte_debugger;

/* instance the standard MPIR interfaces */
struct MPIR_PROCDESC *MPIR_proctable = NULL;
int MPIR_proctable_size = 0;
volatile int MPIR_being_debugged = 0;
volatile int MPIR_debug_state = 0;
volatile int MPIR_i_am_starter = 0;
volatile int MPIR_partial_attach_ok = 1;
volatile char MPIR_executable_path[MPIR_MAX_PATH_LENGTH];
volatile char MPIR_server_arguments[MPIR_MAX_ARG_LENGTH];
volatile int MPIR_forward_output = 0;
volatile int MPIR_forward_comm = 0;
char MPIR_attach_fifo[MPIR_MAX_PATH_LENGTH];


#if ORTE_DISABLE_FULL_SUPPORT
int orte_debugger_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_debugger_base_open(void)
{
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_debugger_base_output = opal_output_open(NULL);

    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("debugger", orte_debugger_base_output,
                                 mca_debugger_base_static_components,
                                 &orte_debugger_base_components_available, 
                                 true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}
#endif
