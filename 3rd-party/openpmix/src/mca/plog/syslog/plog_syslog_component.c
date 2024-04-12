/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * includes
 */
#include "pmix_config.h"
#include "pmix_common.h"

#ifdef HAVE_SYSLOG_H
#    include <syslog.h>
#endif

#include "plog_syslog.h"
#include "src/util/pmix_show_help.h"

static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority);
static pmix_status_t syslog_register(void);

/*
 * Struct of function pointers that need to be initialized
 */
pmix_plog_syslog_component_t pmix_mca_plog_syslog_component = {
    .super = {
        PMIX_PLOG_BASE_VERSION_1_0_0,

        .pmix_mca_component_name = "syslog",
        PMIX_MCA_BASE_MAKE_VERSION(component, PMIX_MAJOR_VERSION, PMIX_MINOR_VERSION,
                              PMIX_RELEASE_VERSION),
        .pmix_mca_query_component = component_query,
        .pmix_mca_register_component_params = syslog_register,
    },
    .console = false,
    .level = LOG_ERR,
    .facility = LOG_USER
};

static char *level = "info";
static char *facility = "user";

static pmix_status_t syslog_register(void)
{
    pmix_status_t rc = PMIX_SUCCESS;

    (void) pmix_mca_base_component_var_register(
        &pmix_mca_plog_syslog_component.super, "console",
        "Write directly to system console if there is an error while sending to system logger",
        PMIX_MCA_BASE_VAR_TYPE_BOOL,
        &pmix_mca_plog_syslog_component.console);

    level = "info";
    (void) pmix_mca_base_component_var_register(&pmix_mca_plog_syslog_component.super, "level",
                                                "Default syslog logging level (err, alert, crit, "
                                                "emerg, warning, notice, info[default], or debug)",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING, &level);
    if (0 == strncasecmp(level, "err", 3)) {
        pmix_mca_plog_syslog_component.level = LOG_ERR;
    } else if (0 == strcasecmp(level, "alert")) {
        pmix_mca_plog_syslog_component.level = LOG_ALERT;
    } else if (0 == strncasecmp(level, "crit", 4)) {
        pmix_mca_plog_syslog_component.level = LOG_CRIT;
    } else if (0 == strncasecmp(level, "emerg", 5)) {
        pmix_mca_plog_syslog_component.level = LOG_EMERG;
    } else if (0 == strncasecmp(level, "warn", 4)) {
        pmix_mca_plog_syslog_component.level = LOG_WARNING;
    } else if (0 == strncasecmp(level, "not", 3)) {
        pmix_mca_plog_syslog_component.level = LOG_NOTICE;
    } else if (0 == strcasecmp(level, "info")) {
        pmix_mca_plog_syslog_component.level = LOG_INFO;
    } else if (0 == strcasecmp(level, "debug") || 0 == strcasecmp(level, "dbg")) {
        pmix_mca_plog_syslog_component.level = LOG_DEBUG;
    } else {
        pmix_show_help("help-pmix-plog.txt", "syslog:unrec-level", true, level);
        rc = PMIX_ERR_NOT_SUPPORTED;
    }

    facility = "user";
    (void) pmix_mca_base_component_var_register(
        &pmix_mca_plog_syslog_component.super, "facility",
        "Specify what type of program is logging the message "
        "(only \"auth\", \"priv\", \"daemon\", and \"user\" are supported)",
        PMIX_MCA_BASE_VAR_TYPE_STRING, &facility);
    if (0 == strncasecmp(facility, "auth", 4)) {
        pmix_mca_plog_syslog_component.facility = LOG_AUTH;
    } else if (0 == strncasecmp(facility, "priv", 4)) {
        pmix_mca_plog_syslog_component.facility = LOG_AUTHPRIV;
    } else if (0 == strcasecmp(facility, "daemon")) {
        pmix_mca_plog_syslog_component.facility = LOG_DAEMON;
    } else if (0 == strcasecmp(facility, "user")) {
        pmix_mca_plog_syslog_component.facility = LOG_USER;
    } else {
        pmix_show_help("help-pmix-plog.txt", "syslog:unrec-facility", true, facility);
        rc = PMIX_ERR_NOT_SUPPORTED;
    }

    return rc;
}

static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority)
{
    *priority = 10;
    *module = (pmix_mca_base_module_t *) &pmix_plog_syslog_module;
    return PMIX_SUCCESS;
}
