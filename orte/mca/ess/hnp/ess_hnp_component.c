/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "orte/util/proc_info.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/hnp/ess_hnp.h"
#include "orte/runtime/orte_globals.h"

extern orte_ess_base_module_t orte_ess_hnp_module;
static int orte_ess_hnp_component_register (void);

int orte_ess_hnp_forward_signals[ORTE_ESS_HNP_MAX_FORWARD_SIGNALS] = {SIGUSR1, SIGUSR2, SIGTSTP, SIGCONT};
unsigned int orte_ess_hnp_forward_signals_count = 4;
static char *orte_ess_hnp_forward_additional_signals;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_base_component_t mca_ess_hnp_component = {
    .base_version = {
        ORTE_ESS_BASE_VERSION_3_0_0,

        /* Component name and version */
        .mca_component_name = "hnp",
        MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                              ORTE_RELEASE_VERSION),

        /* Component open and close functions */
        .mca_open_component = orte_ess_hnp_component_open,
        .mca_close_component = orte_ess_hnp_component_close,
        .mca_query_component = orte_ess_hnp_component_query,
        .mca_register_component_params = orte_ess_hnp_component_register,
    },
    .base_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

static int orte_ess_hnp_component_register (void)
{
    orte_ess_hnp_forward_additional_signals = NULL;
    (void) mca_base_component_var_register (&mca_ess_hnp_component.base_version,
                                            "forward_signals", "Comma-delimited list "
                                            "of additional signals (integers) to forward to "
                                            "application processes", MCA_BASE_VAR_TYPE_STRING,
                                            NULL, 0, 0, OPAL_INFO_LVL_4, MCA_BASE_VAR_SCOPE_READONLY,
                                            &orte_ess_hnp_forward_additional_signals);

    return ORTE_SUCCESS;
}

int
orte_ess_hnp_component_open(void)
{
    /* reset the signal count to the original value */
    orte_ess_hnp_forward_signals_count = 4;

    if (NULL != orte_ess_hnp_forward_additional_signals && 0 != strlen (orte_ess_hnp_forward_additional_signals)) {
        char **signals = opal_argv_split (orte_ess_hnp_forward_additional_signals, ',');
        int forward_signal;
        char *tmp = NULL;

        for (int i = 0 ; signals[i] ; ++i) {
            if (orte_ess_hnp_forward_signals_count == ORTE_ESS_HNP_MAX_FORWARD_SIGNALS) {
                /* print out an error here */
                break;
            }

            errno = 0;
            forward_signal = (int) strtol (signals[i], &tmp, 0);
            if (0 == errno && '\0' == *tmp) {
                bool duplicate_signal = false;
                for (int j = 0 ; j < orte_ess_hnp_forward_signals_count ; ++j) {
                    if (orte_ess_hnp_forward_signals[j] == forward_signal) {
                        /* duplicate signal */
                        duplicate_signal = true;
                        break;
                    }
                }

                if (!duplicate_signal) {
                    orte_ess_hnp_forward_signals[orte_ess_hnp_forward_signals_count++] = forward_signal;
                } else {
                    opal_show_help ("help-ess-hnp.txt", "duplicate_signal", true, signals[i]);
                }
            } else {
                opal_show_help ("help-ess-hnp.txt", "invalid_signal", true, signals[i]);
            }
        }

        opal_argv_free (signals);
    }

    return ORTE_SUCCESS;
}


int orte_ess_hnp_component_query(mca_base_module_t **module, int *priority)
{

    /* we are the hnp module - we need to be selected
     * IFF we are designated as the hnp
     */
    if (ORTE_PROC_IS_HNP) {
        *priority = 100;
        *module = (mca_base_module_t *)&orte_ess_hnp_module;
        return ORTE_SUCCESS;
    }

    /* else, we are not */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}


int
orte_ess_hnp_component_close(void)
{
    return ORTE_SUCCESS;
}

