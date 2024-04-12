/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "prte_config.h"
#include "constants.h"

#if HAVE_UNISTD_H
#    include <unistd.h>
#endif
#if HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#include <pmix.h>
#include <pmix_server.h>

#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/grpcomm/grpcomm.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/plm/plm.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/rml/rml.h"
#include "src/prted/pmix/pmix_server_internal.h"
#include "src/runtime/prte_data_server.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_wait.h"
#include "src/threads/pmix_threads.h"
#include "src/util/prte_cmd_line.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/state/base/base.h"

int prte_state_base_set_default_rto(prte_job_t *jdata,
                                    prte_rmaps_options_t *options)
{
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(options);

    rc = prte_state_base_set_runtime_options(jdata, NULL);
    return rc;
}

/* this function is called if pmix_server_dyn receives a
 * PMIX_RUNTIME_OPTIONS info struct */
int prte_state_base_set_runtime_options(prte_job_t *jdata, char *spec)
{
    char **options, **tmp, *ptr;
    int n;
    bool flag, *fptr = &flag;
    int32_t i32;
    prte_job_t *djob;
    prte_app_context_t *app;
    pmix_rank_t rank;
    pmix_info_t info;
    pmix_value_t value;

    if (NULL == spec) {
        /* set everything to the defaults if not already set. We don't want to
         * have to check the value of BOOL settings everywhere we use them, so
         * we translate them here by removing the attribute if it is set to false,
         * and leaving it if it is set to true. If it isn't present, then it wasn't
         * provided via PMIx_Spawn and we instead set it based on the defaults
         */
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_ERROR_NONZERO_EXIT, (void**)&fptr, PMIX_BOOL)) {
            /* it is present - check the value */
            if (!flag) {
                /* remove the attribute */
                prte_remove_attribute(&jdata->attributes, PRTE_JOB_ERROR_NONZERO_EXIT);
            }
        } else {
            /* set it based on default value */
            if (prte_state_base.error_non_zero_exit) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_ERROR_NONZERO_EXIT,
                                   PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
            }
        }

        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_SHOW_PROGRESS, (void**)&fptr, PMIX_BOOL)) {
            /* it is present - check the value */
            if (!flag) {
                /* remove the attribute */
                prte_remove_attribute(&jdata->attributes, PRTE_JOB_SHOW_PROGRESS);
            }
        } else {
            /* set it based on default value */
            if (prte_state_base.show_launch_progress) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_SHOW_PROGRESS,
                                   PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
            }
        }

        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_RECOVERABLE, NULL, PMIX_BOOL)) {
            /* it is present - check the value */
            if (!flag) {
                /* remove the attribute */
                prte_remove_attribute(&jdata->attributes, PRTE_JOB_RECOVERABLE);
            }
        } else {
            /* set it based on default value */
            if (prte_state_base.recoverable) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_RECOVERABLE,
                                   PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
            }
        }

        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_CONTINUOUS, (void**)&fptr, PMIX_BOOL)) {
            /* it is present - check the value */
            if (!flag) {
                /* remove the attribute */
                prte_remove_attribute(&jdata->attributes, PRTE_JOB_CONTINUOUS);
            }
        } else {
            /* set it based on default value */
            if (prte_state_base.continuous) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_CONTINUOUS,
                                   PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
            }
        }

        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_NOTIFY_ERRORS, NULL, PMIX_BOOL)) {
            /* it is present - check the value */
            if (!flag) {
                /* remove the attribute */
                prte_remove_attribute(&jdata->attributes, PRTE_JOB_NOTIFY_ERRORS);
            }
        } else {
            /* set it based on default value */
            if (prte_state_base.notifyerrors) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_NOTIFY_ERRORS,
                                   PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
            }
        }

        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_AUTORESTART, NULL, PMIX_BOOL)) {
            /* it is present - check the value */
            if (!flag) {
                /* remove the attribute */
                prte_remove_attribute(&jdata->attributes, PRTE_JOB_AUTORESTART);
            }
        } else {
            /* set it based on default value */
            if (prte_state_base.autorestart) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_AUTORESTART,
                                   PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
            }
        }

        if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_EXEC_AGENT, NULL, PMIX_STRING)) {
            if (NULL != prte_odls_globals.exec_agent) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_EXEC_AGENT,
                                   PRTE_ATTR_GLOBAL,
                                   prte_odls_globals.exec_agent, PMIX_STRING);
            }
        }

        /* check the apps for max restarts */
        if (0 < prte_state_base.max_restarts) {
            for (n = 0; n < jdata->apps->size; n++) {
                app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, n);
                if (NULL == app) {
                    continue;
                }
                if (!prte_get_attribute(&app->attributes, PRTE_APP_MAX_RESTARTS, NULL, PMIX_INT32)) {
                    prte_set_attribute(&app->attributes, PRTE_APP_MAX_RESTARTS, PRTE_ATTR_GLOBAL,
                                       &prte_state_base.max_restarts, PMIX_INT32);
                }
            }
        }

    } else {
        options = PMIX_ARGV_SPLIT_COMPAT(spec, ',');
        for (n=0; NULL != options[n]; n++) {
            /* see if there is an '=' */
            ptr = strchr(options[n], '=');
            if (NULL != ptr) {
                *ptr = '\0';
                ++ptr;
                if ('\0' == *ptr) {
                    /* missing the value */
                    pmix_show_help("help-prte-rmaps-base.txt", "missing-value", true,
                                   "runtime options", options[n], "empty");
                    PMIX_ARGV_FREE_COMPAT(options);
                    return PRTE_ERR_BAD_PARAM;
                }
            }
            PMIX_VALUE_LOAD(&value, ptr, PMIX_STRING); // just in case we need to evaluate a bool
            /* check the options */
            if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_ERROR_NZ)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_ERROR_NONZERO_EXIT,
                                   PRTE_ATTR_GLOBAL, &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_NOLAUNCH)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);
                /* if we are not in a persistent DVM, then make sure we also
                 * apply this to the daemons */
                if (!prte_persistent) {
                    djob = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
                    prte_set_attribute(&djob->attributes, PRTE_JOB_DO_NOT_LAUNCH, PRTE_ATTR_GLOBAL,
                                       &flag, PMIX_BOOL);
                }

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_SHOW_PROGRESS)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_SHOW_PROGRESS, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_NOTIFY_ERRORS)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_NOTIFY_ERRORS, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_RECOVERABLE)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_RECOVERABLE, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_AUTORESTART)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_AUTORESTART, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_CONTINUOUS)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_CONTINUOUS, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_MAX_RESTARTS)) {
                if ('\0' == *ptr) {
                    /* missing the value */
                    pmix_show_help("help-prte-rmaps-base.txt", "missing-value", true,
                                   "runtime options", options[n], "empty");
                    PMIX_ARGV_FREE_COMPAT(options);
                    return PRTE_ERR_BAD_PARAM;
                }
                i32 = strtol(ptr, NULL, 10);
                for (n = 0; n < jdata->apps->size; n++) {
                    app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, n);
                    if (NULL == app) {
                        continue;
                    }
                    prte_set_attribute(&app->attributes, PRTE_APP_MAX_RESTARTS, PRTE_ATTR_GLOBAL,
                                       &i32, PMIX_INT32);
                }

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_EXEC_AGENT)) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_EXEC_AGENT, PRTE_ATTR_GLOBAL,
                                   ptr, PMIX_STRING);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_DEFAULT_EXEC_AGENT)) {
                prte_remove_attribute(&jdata->attributes, PRTE_JOB_EXEC_AGENT);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_STOP_ON_EXEC)) {
                flag = PMIX_CHECK_TRUE(&value);
                if (flag) {
                    prte_set_attribute(&jdata->attributes, PRTE_JOB_STOP_ON_EXEC,
                                       PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
                } else {
                    prte_remove_attribute(&jdata->attributes, PRTE_JOB_STOP_ON_EXEC);
                }

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_STOP_IN_INIT)) {
                flag = PMIX_CHECK_TRUE(&value);
                if (flag) {
                    prte_set_attribute(&jdata->attributes, PRTE_JOB_STOP_IN_INIT,
                                       PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
                    /* also must add to job-level cache */
                    PMIX_INFO_LOAD(&info, PMIX_DEBUG_STOP_IN_INIT, NULL, PMIX_BOOL);
                    pmix_server_cache_job_info(jdata, &info);
                } else {
                    prte_remove_attribute(&jdata->attributes, PRTE_JOB_STOP_IN_INIT);
                }

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_STOP_IN_APP)) {
                flag = PMIX_CHECK_TRUE(&value);
                if (flag) {
                    prte_set_attribute(&jdata->attributes, PRTE_JOB_STOP_IN_APP,
                                       PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
                    /* also must add to job-level cache */
                    PMIX_INFO_LOAD(&info, PMIX_DEBUG_STOP_IN_APP, NULL, PMIX_BOOL);
                    pmix_server_cache_job_info(jdata, &info);
                } else {
                    prte_remove_attribute(&jdata->attributes, PRTE_JOB_STOP_IN_APP);
                }

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_TIMEOUT)) {
                n = PMIX_CONVERT_TIME(ptr);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_TIMEOUT, PRTE_ATTR_GLOBAL,
                                   &n, PMIX_INT);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_SPAWN_TIMEOUT)) {
                n = PMIX_CONVERT_TIME(ptr);
                prte_set_attribute(&jdata->attributes, PRTE_SPAWN_TIMEOUT, PRTE_ATTR_GLOBAL,
                                   &n, PMIX_INT);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_STACK_TRACES)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_STACKTRACES, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_REPORT_STATE)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_REPORT_STATE, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_AGG_HELP)) {
                flag = PMIX_CHECK_TRUE(&value);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_NOAGG_HELP, PRTE_ATTR_GLOBAL,
                                   &flag, PMIX_BOOL);

            } else if (PMIX_CHECK_CLI_OPTION(options[n], PRTE_CLI_OUTPUT_PROCTABLE)) {
                if (NULL == ptr || '\0' == *ptr) {
                    /* no value provided, so assume stdout */
                    ptr = "-";
                }
                prte_set_attribute(&jdata->attributes, PRTE_JOB_OUTPUT_PROCTABLE,
                                    PRTE_ATTR_GLOBAL, ptr, PMIX_STRING);

            } else {
                pmix_show_help("help-prte-rmaps-base.txt", "unrecognized-policy", true,
                               "runtime options", spec);
                return PRTE_ERR_SILENT;
            }
        }
        PMIX_ARGV_FREE_COMPAT(options);
    }
    /* if notify-error is set but neither recovery nor continuous were specified,
     * then notifications will not be given as we will terminate the job upon
     * error. Detect that situation, provide a show-help explaining the problem,
     * and then error out */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_NOTIFY_ERRORS, NULL, PMIX_BOOL) &&
        !prte_get_attribute(&jdata->attributes, PRTE_JOB_RECOVERABLE, NULL, PMIX_BOOL) &&
        !prte_get_attribute(&jdata->attributes, PRTE_JOB_CONTINUOUS, NULL, PMIX_BOOL)) {
        pmix_show_help("help-state-base.txt", "bad-combination", true);
        return PRTE_ERR_SILENT;
    }
    return PRTE_SUCCESS;
}
