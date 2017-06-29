/*
 * Copyright (c) 2016-2017 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>
#include <sys/syscall.h>

#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/mca/schizo/base/base.h"

#include "schizo_alps.h"

static orte_schizo_launch_environ_t check_launch_environment(void);
static void finalize(void);

orte_schizo_base_module_t orte_schizo_alps_module = {
    .check_launch_environment = check_launch_environment,
    .finalize = finalize
};

static char **pushed_envs = NULL;
static char **pushed_vals = NULL;
static orte_schizo_launch_environ_t myenv;
static bool myenvdefined = false;

static orte_schizo_launch_environ_t check_launch_environment(void)
{
    int i;
    const char proc_job_file[]="/proc/job";
    FILE *fd = NULL, *fd_task_is_app = NULL;
    char task_is_app_fname[PATH_MAX];

    if (myenvdefined) {
        return myenv;
    }
    myenvdefined = true;

    /* we were only selected because we are an app,
     * so no need to further check that here. Instead,
     * see if we were direct launched vs launched via mpirun */
    if (NULL != orte_process_info.my_daemon_uri) {
        /* nope */
        myenv = ORTE_SCHIZO_NATIVE_LAUNCHED;
        opal_argv_append_nosize(&pushed_envs, OPAL_MCA_PREFIX"ess");
        opal_argv_append_nosize(&pushed_vals, "pmi");
        /* do not try to bind when launched with aprun. there is a significant
         * launch performance penalty for hwloc at high ppn on knl */
        opal_argv_append_nosize(&pushed_envs, OPAL_MCA_PREFIX "orte_bound_at_launch");
        opal_argv_append_nosize(&pushed_vals, "true");
        /* mark that we are native */
        opal_argv_append_nosize(&pushed_envs, "ORTE_SCHIZO_DETECTION");
        opal_argv_append_nosize(&pushed_vals, "NATIVE");
        goto setup;
    }

    /* mark that we are on ALPS */
    opal_argv_append_nosize(&pushed_envs, "ORTE_SCHIZO_DETECTION");
    opal_argv_append_nosize(&pushed_vals, "ALPS");

    /* see if we are running in a Cray PAGG container */
    fd = fopen(proc_job_file, "r");
    if (NULL == fd) {
        /* we are a singleton */
        myenv = ORTE_SCHIZO_MANAGED_SINGLETON;
        opal_argv_append_nosize(&pushed_envs, OPAL_MCA_PREFIX"ess");
        opal_argv_append_nosize(&pushed_vals, "singleton");
    } else {
        if (NULL != orte_process_info.my_daemon_uri) {
            myenv = ORTE_SCHIZO_NATIVE_LAUNCHED;
        } else {
            myenv = ORTE_SCHIZO_DIRECT_LAUNCHED;
        }
        opal_argv_append_nosize(&pushed_envs, OPAL_MCA_PREFIX"ess");
        opal_argv_append_nosize(&pushed_vals, "pmi");
        snprintf(task_is_app_fname,sizeof(task_is_app_fname),
                 "/proc/self/task/%ld/task_is_app",syscall(SYS_gettid));
        fd_task_is_app = fopen(task_is_app_fname, "r");
        if (fd_task_is_app != NULL) {   /* okay we're in a PAGG container,
                                           and we are an app task (not just a process
                                           running on a mom node, for example) */
            opal_argv_append_nosize(&pushed_envs, OPAL_MCA_PREFIX"pmix");
            opal_argv_append_nosize(&pushed_vals, "cray");
        }
        fclose(fd);
    }

  setup:
    opal_output_verbose(1, orte_schizo_base_framework.framework_output,
                        "schizo:alps DECLARED AS %s", orte_schizo_base_print_env(myenv));
    if (NULL != pushed_envs) {
        for (i=0; NULL != pushed_envs[i]; i++) {
            opal_setenv(pushed_envs[i], pushed_vals[i], true, &environ);
        }
    }

    return myenv;
}

static void finalize(void)
{
    int i;

    if (NULL != pushed_envs) {
        for (i=0; NULL != pushed_envs[i]; i++) {
            opal_unsetenv(pushed_envs[i], &environ);
        }
        opal_argv_free(pushed_envs);
        opal_argv_free(pushed_vals);
    }
}
