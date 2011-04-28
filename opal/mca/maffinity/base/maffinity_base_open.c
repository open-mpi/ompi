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


#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/maffinity/base/static-components.h"


/*
 * Globals
 */
int opal_maffinity_base_output = -1;
bool opal_maffinity_base_components_opened_valid = false;
opal_list_t opal_maffinity_base_components_opened;
bool opal_maffinity_setup = false;
opal_maffinity_base_map_t opal_maffinity_base_map = 
    OPAL_MAFFINITY_BASE_MAP_NONE;
opal_maffinity_base_bfa_t opal_maffinity_base_bfa =
    OPAL_MAFFINITY_BASE_BFA_ERROR;

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int opal_maffinity_base_open(void)
{
    int int_value;
    char *str_value;

    /* Debugging / verbose output */

    mca_base_param_reg_int_name("maffinity", "base_verbose", 
                                "Verbosity level of the maffinity framework",
                                false, false,
                                0, &int_value);
    if (0 != int_value) {
        opal_maffinity_base_output = opal_output_open(NULL);
    } else {
        opal_maffinity_base_output = -1;
    }

    /* maffinity_base_mbind_policy */
    switch (opal_maffinity_base_map) {
    case OPAL_MAFFINITY_BASE_MAP_NONE:
        str_value = "none";
        break;
    case OPAL_MAFFINITY_BASE_MAP_LOCAL_ONLY:
        str_value = "local_only";
        break;
    }
    mca_base_param_reg_string_name("maffinity", "base_alloc_policy",
                                   "Policy that determines how general memory allocations are bound after MPI_INIT.  A value of \"none\" means that no memory policy is applied.  A value of \"local_only\" means that all memory allocations will be restricted to the local NUMA node where each process is placed.  Note that operating system paging policies are unaffected by this setting.  For example, if \"local_only\" is used and local NUMA node memory is exhausted, a new memory allocation may cause paging.",
                                   false, false, str_value, &str_value);
    if (strcasecmp(str_value, "none") == 0) {
        opal_maffinity_base_map = OPAL_MAFFINITY_BASE_MAP_NONE;
    } else if (strcasecmp(str_value, "local_only") == 0 ||
               strcasecmp(str_value, "local-only") == 0) {
        opal_maffinity_base_map = OPAL_MAFFINITY_BASE_MAP_LOCAL_ONLY;
    } else {
        char hostname[32];
        gethostname(hostname, sizeof(hostname));
        opal_show_help("help-opal-maffinity-base.txt", "invalid policy",
                       true, hostname, getpid(), str_value);
        return OPAL_ERR_BAD_PARAM;
    }

    /* maffinity_base_bind_failure_action */
    switch (opal_maffinity_base_bfa) {
    case OPAL_MAFFINITY_BASE_BFA_WARN:
        str_value = "warn";
        break;
    case OPAL_MAFFINITY_BASE_BFA_ERROR:
        str_value = "error";
        break;
    }
    mca_base_param_reg_string_name("maffinity", "base_bind_failure_action",
                                   "What Open MPI will do if it explicitly tries to bind memory to a specific NUMA location, and fails.  Note that this is a different case than the general allocation policy described by maffinity_base_alloc_policy.  A value of \"warn\" means that Open MPI will warn the first time this happens, but allow the job to continue (possibly with degraded performance).  A value of \"error\" means that Open MPI will abort the job if this happens.",
                                   false, false, str_value, &str_value);
    if (strcasecmp(str_value, "warn") == 0) {
        opal_maffinity_base_bfa = OPAL_MAFFINITY_BASE_BFA_WARN;
    } else if (strcasecmp(str_value, "error") == 0) {
        opal_maffinity_base_bfa = OPAL_MAFFINITY_BASE_BFA_ERROR;
    } else {
        char hostname[32];
        gethostname(hostname, sizeof(hostname));
        opal_show_help("help-opal-maffinity-base.txt", "invalid error action",
                       true, hostname, getpid(), str_value);
        return OPAL_ERR_BAD_PARAM;
    }

    opal_maffinity_base_components_opened_valid = false;

    /* Open up all available components */

    if (OPAL_SUCCESS !=
        mca_base_components_open("maffinity", opal_maffinity_base_output,
                                 mca_maffinity_base_static_components,
                                 &opal_maffinity_base_components_opened, 
                                 true)) {
        return OPAL_ERROR;
    }
    opal_maffinity_base_components_opened_valid = true;

    /* All done */

    return OPAL_SUCCESS;
}
