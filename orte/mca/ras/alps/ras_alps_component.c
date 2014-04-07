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
 * Copyright (c) 2008      UT-Battelle, LLC
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "orte/constants.h"
#include "orte/util/proc_info.h"
#include "ras_alps.h"

#include <ctype.h>

/* Local variables */
static int param_priority;
static int ras_alps_read_attempts;

/* Local functions */
static int ras_alps_register(void);
static int ras_alps_open(void);
static int orte_ras_alps_component_query(mca_base_module_t **module,
                                         int *priority);
unsigned long int orte_ras_alps_res_id;
char *ras_alps_apstat_cmd;

orte_ras_base_component_t mca_ras_alps_component = {
    /* First, the mca_base_component_t struct containing meta information about
     * the component itself
     * */
    {
        ORTE_RAS_BASE_VERSION_2_0_0,

        /* Component name and version */
        "alps",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        ras_alps_open,
        NULL,
        orte_ras_alps_component_query,
        ras_alps_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

/* simple function used to strip off characters on and after a period. NULL
 * will be returned upon failure.  Otherwise, a "prepped" string will be
 * returned.  The caller is responsible for freeing returned resources.
 * for example: if jid is 138295.sdb, then 138295 will be returned.
 */
static char *
prep_job_id(const char *jid)
{
    char *tmp = strdup(jid);
    char *tmp2 = NULL;

    if (NULL == tmp) {
        /* out of resources */
        return NULL;
    }
    if (NULL != (tmp2 = strchr(tmp, '.'))) {
        *tmp2 = '\0';
    }
    return tmp;
}

/* this function replicates some of the id setting functionality found in
 * ras-alps-command.sh. we wanted the ability to just "mpirun" the application
 * without having to set an environment variable
 */
static unsigned long int
get_res_id(void)
{
    char *apstat_cmd;
    char *id = NULL;
    char read_buf[512];
    FILE *apstat_fp = NULL;
    /* zero is considered to be an invalid res id */
    unsigned long jid = 0;
    int ret;

    if (NULL != (id = getenv("BATCH_PARTITION_ID"))) {
        return strtoul(id, NULL, 10);
    }
    if (NULL != (id = getenv("PBS_JOBID"))) {
        char *prepped_jid = prep_job_id(id);
        if (NULL == prepped_jid) {
            /* out of resources */
            return 0;
        }

        ret = asprintf (&apstat_cmd, "%s -r", ras_alps_apstat_cmd);
        if (0 > ret) {
            return 0;
        }

        apstat_fp = popen(apstat_cmd, "r");
        free (apstat_cmd);
        if (NULL == apstat_fp) {
            /* popen failure */
            free(prepped_jid);
            return 0;
        }
        while (NULL != fgets(read_buf, 512, apstat_fp)) {
            /* does this line have the id that we care about? */
            if (NULL != strstr(read_buf, prepped_jid)) {
        /* the line is going to be in the form of something like:
        A 1450   571783 batch:138309     XT    80 - -   2000 conf,claim
         */
                char *t = read_buf;
                for (t = read_buf; !isdigit(*t) && *t; ++t) {
                    jid = strtoul(t, NULL, 10);
                }
                /* if we are here, then jid should be, given the example above,
                 * 1450 */
                break;
            }
        }
        fclose(apstat_fp);
        free(prepped_jid);
    }
    return jid;
}

static int
ras_alps_register(void)
{
    param_priority = 75;
    (void) mca_base_component_var_register (&mca_ras_alps_component.base_version,
                                            "priority", "Priority of the alps ras component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &param_priority);

    ras_alps_read_attempts = 10;
    (void) mca_base_component_var_register (&mca_ras_alps_component.base_version,
                                            "appinfo_read_attempts",
                                            "Maximum number of attempts to read ALPS "
                                            "appinfo file", MCA_BASE_VAR_TYPE_INT,
                                            NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, &ras_alps_read_attempts);

    ras_alps_apstat_cmd = "/usr/bin/apstat";
    (void) mca_base_component_var_register (&mca_ras_alps_component.base_version,
                                            "apstat_cmd", "Location of the apstat command",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_6,
                                            MCA_BASE_VAR_SCOPE_READONLY, &ras_alps_apstat_cmd);

    return ORTE_SUCCESS;
}

static int
ras_alps_open(void)
{
    return ORTE_SUCCESS;
}

static int
orte_ras_alps_component_query(mca_base_module_t **module,
                              int *priority)
{
    char *jid_str = NULL;
    /* default to an invalid value */
    orte_ras_alps_res_id = 0;

    /* if we are not an HNP, then we must not be selected */
    if (!ORTE_PROC_IS_HNP) {
        *module = NULL;
        return ORTE_ERROR;
    }

    /* Are we running under a ALPS job? */
    /* BASIL_RESERVATION_ID is the equivalent of OMPI_ALPS_RESID
     * on some systems
     */
    if ((NULL == (jid_str = getenv("OMPI_ALPS_RESID"))) &&
        (NULL == (jid_str = getenv("BASIL_RESERVATION_ID")))) {
            orte_ras_alps_res_id = get_res_id();
    }
    else {
        orte_ras_alps_res_id = strtoul(jid_str, NULL, 10);
    }
    if (0 != orte_ras_alps_res_id) {
        *priority = param_priority;
        opal_output_verbose(2, orte_ras_base_framework.framework_output,
                             "ras:alps: available for selection");
        *module = (mca_base_module_t *) &orte_ras_alps_module;
        return ORTE_SUCCESS;
    }

    /* Sadly, no */

    opal_output(orte_ras_base_framework.framework_output,
                "ras:alps: NOT available for selection -- "
                "OMPI_ALPS_RESID or BASIL_RESERVATION_ID not set?");
    *module = NULL;
    return ORTE_ERROR;
}

int
orte_ras_alps_get_appinfo_attempts(int *attempts)
{
    *attempts = ras_alps_read_attempts;
    opal_output_verbose(2, orte_ras_base_framework.framework_output,
                         "ras:alps:orte_ras_alps_get_appinfo_attempts: %d",
                         *attempts);
    return ORTE_SUCCESS;
}
