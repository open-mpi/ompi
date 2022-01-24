/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2022      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"
#include "opal/mca/threads/thread_usage.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"

#include "opal/mca/pmix/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"

/*
 * The following file was created by configure.  It contains extern
 * components and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "opal/mca/pmix/base/static-components.h"

bool opal_pmix_collect_all_data = true;
int opal_pmix_verbose_output = -1;
bool opal_pmix_base_async_modex = false;
opal_pmix_base_t opal_pmix_base = {.evbase = NULL,
                                   .timeout = 0,
                                   .initialized = 0,
                                   .lock = {.mutex = OPAL_MUTEX_STATIC_INIT,
                                            .cond = OPAL_PMIX_CONDITION_STATIC_INIT,
                                            .active = false}};

static int opal_pmix_base_frame_register(mca_base_register_flag_t flags)
{
    opal_pmix_base_async_modex = false;
    (void) mca_base_var_register("opal", "pmix", "base", "async_modex",
                                 "Use asynchronous modex mode", MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                 OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_pmix_base_async_modex);
    opal_pmix_collect_all_data = true;
    (void) mca_base_var_register("opal", "pmix", "base", "collect_data",
                                 "Collect all data during modex", MCA_BASE_VAR_TYPE_BOOL, NULL, 0,
                                 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_pmix_collect_all_data);

    opal_pmix_base.timeout = -1;
    (void) mca_base_var_register("opal", "pmix", "base", "exchange_timeout",
                                 "Time (in seconds) to wait for a data exchange to complete",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_3,
                                 MCA_BASE_VAR_SCOPE_READONLY, &opal_pmix_base.timeout);
    return OPAL_SUCCESS;
}

static char*
opal_get_proc_hostname_using_pmix(const opal_proc_t *proc)
{
    int ret;
    char *hostname;

    /* if the proc is NULL, then we can't know */
    if (NULL == proc) {
        return strdup("unknown");
    }

    /* if it is my own hostname we are after, then just hand back
     * the value in opal_process_info */
    if (proc == opal_proc_local_get()) {
        return strdup(opal_process_info.nodename);
    }
    /* if we don't already have it, then try to get it */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, PMIX_HOSTNAME, &proc->proc_name, (char **) &hostname,
                                   PMIX_STRING);
    if (OPAL_SUCCESS != ret) {
        return strdup("unknown"); // return something so the caller doesn't segfault
    }
    /* user is not allowed to release the data */
    return hostname;
}

static int opal_pmix_base_frame_close(void)
{
    int rc;

    rc = mca_base_framework_components_close(&opal_pmix_base_framework, NULL);
    return rc;
}

static int opal_pmix_base_frame_open(mca_base_open_flag_t flags)
{
    int rc;

    /* Open up all available components */
    rc = mca_base_framework_components_open(&opal_pmix_base_framework, flags);
    /* default to the OPAL event base */
    opal_pmix_base.evbase = opal_sync_event_base;
    /* pass across the verbosity */
    opal_pmix_verbose_output = opal_pmix_base_framework.framework_output;
    /* Set the distributed name service via PMIx */
    opal_get_proc_hostname = opal_get_proc_hostname_using_pmix;
    return rc;
}

MCA_BASE_FRAMEWORK_DECLARE(opal, pmix, "OPAL PMI Client Framework", opal_pmix_base_frame_register,
                           opal_pmix_base_frame_open, opal_pmix_base_frame_close,
                           mca_pmix_base_static_components, 0);
