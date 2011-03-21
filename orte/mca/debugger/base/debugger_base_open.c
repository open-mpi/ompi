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
orte_debugger_base_t orte_debugger_base;
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
int MPIR_force_to_main = 0;

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
    int value;

    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_debugger_base.output = opal_output_open(NULL);

    mca_base_param_reg_int_name("orte",
                                "output_debugger_proctable",
                                "Whether or not to output the debugger proctable after launch (default: false)",
                                true, false, 0, &value);
    orte_debugger_base.dump_proctable = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_string_name("orte", "debugger_test_daemon",
                                   "Name of the executable to be used to simulate a debugger colaunch (relative or absolute path)",
                                   false, false, NULL, &orte_debugger_base.test_daemon);

    mca_base_param_reg_int_name("orte",
                                "debugger_test_attach",
                                "Test debugger colaunch after debugger attachment",
                                false, false, 0, &value);
    orte_debugger_base.test_attach = OPAL_INT_TO_BOOL(value);
    
    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("debugger", orte_debugger_base.output,
                                 mca_debugger_base_static_components,
                                 &orte_debugger_base_components_available, 
                                 true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}
#endif
