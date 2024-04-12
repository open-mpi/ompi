/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2018 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
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

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif

#include "src/util/error.h"
#include "src/util/error_strings.h"
#include "src/util/pmix_keyval_parse.h"
#include "src/util/malloc.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_os_path.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_show_help.h"
#include "src/util/stacktrace.h"
#include "src/util/sys_limits.h"

#include "src/hwloc/hwloc-internal.h"
#include "src/prted/pmix/pmix_server.h"
#include "src/threads/pmix_threads.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_mca_base_vari.h"
#include "src/mca/errmgr/base/base.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/ess/ess.h"
#include "src/mca/filem/base/base.h"
#include "src/mca/grpcomm/base/base.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/oob/base/base.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/pmdl/base/base.h"
#include "src/mca/prtebacktrace/base/base.h"
#include "src/mca/prteinstalldirs/base/base.h"
#include "src/mca/ras/base/base.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rtc/base/base.h"
#include "src/mca/schizo/base/base.h"
#include "src/mca/state/base/base.h"

#include "src/runtime/pmix_init_util.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_locks.h"
#include "src/runtime/runtime.h"
#include "src/runtime/runtime_internals.h"

/*
 * Whether we have completed prte_init or we are in prte_finalize
 */
bool prte_initialized = false;
bool prte_finalizing = false;
bool prte_debug_flag = false;
int prte_debug_verbosity = -1;
char *prte_prohibited_session_dirs = NULL;
bool prte_create_session_dirs = true;
prte_event_base_t *prte_event_base = {0};
bool prte_event_base_active = true;
bool prte_proc_is_bound = false;
int prte_progress_thread_debug = -1;
hwloc_cpuset_t prte_proc_applied_binding = NULL;
int prte_cache_line_size = 128;

pmix_proc_t prte_name_wildcard = {{0}, PMIX_RANK_WILDCARD};

pmix_proc_t prte_name_invalid = {{0}, PMIX_RANK_INVALID};

pmix_nspace_t prte_nspace_wildcard = {0};

static bool util_initialized = false;
static bool min_initialized = false;

#if PRTE_CC_USE_PRAGMA_IDENT
#    pragma ident PRTE_IDENT_STRING
#elif PRTE_CC_USE_IDENT
#    ident PRTE_IDENT_STRING
#endif
const char prte_version_string[] = PRTE_IDENT_STRING;

static bool check_exist(char *path)
{
    struct stat buf;
    /* coverity[TOCTOU] */
    if (0 == stat(path, &buf)) { /* exists */
        return true;
    }
    return false;
}

int prte_init_minimum(void)
{
    int ret;
    char *path = NULL;

    if (min_initialized) {
        return PRTE_SUCCESS;
    }
    min_initialized = true;

    /* carry across the toolname */
    pmix_tool_basename = prte_tool_basename;

    /* initialize install dirs code */
    ret = pmix_mca_base_framework_open(&prte_prteinstalldirs_base_framework,
                                       PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PRTE_SUCCESS != ret) {
        fprintf(stderr,
                "prte_prteinstalldirs_base_open() failed -- process will likely abort (%s:%d, "
                "returned %d instead of PRTE_SUCCESS)\n",
                __FILE__, __LINE__, ret);
        return ret;
    }

    /* initialize the MCA infrastructure */
    if (check_exist(prte_install_dirs.prtelibdir)) {
        pmix_asprintf(&path, "prte@%s", prte_install_dirs.prtelibdir);
    }
    ret = pmix_init_util(NULL, 0, path);
    if (NULL != path) {
        free(path);
    }
    if (PMIX_SUCCESS != ret) {
        return prte_pmix_convert_status(ret);
    }
    ret = pmix_show_help_add_dir(prte_install_dirs.prtedatadir);
    if (PMIX_SUCCESS != ret) {
        return prte_pmix_convert_status(ret);
    }

    /* keyval lex-based parser */
    /* Setup the parameter system */
    if (PRTE_SUCCESS != (ret = pmix_mca_base_var_init())) {
        return ret;
    }

    /* pre-load any default mca param files */
    prte_preload_default_mca_params();

    return PRTE_SUCCESS;
}

int prte_init_util(prte_proc_type_t flags)
{
    int ret;
    char *error = NULL;
    char *path = NULL;

    if (util_initialized) {
        return PRTE_SUCCESS;
    }
    util_initialized = true;

    ret = prte_init_minimum();
    if (PRTE_SUCCESS != ret) {
        return ret;
    }

    /* ensure we know the type of proc for when we finalize */
    prte_process_info.proc_type = flags;

    /* initialize the memory allocator */
    prte_malloc_init();

    /* initialize the output system */
    pmix_output_init();

    /* set the nodename so anyone who needs it has it - this
     * must come AFTER we initialize the installdirs */
    prte_setup_hostname();

    /* pretty-print stack handlers */
    if (PRTE_SUCCESS != (ret = prte_util_register_stackhandlers())) {
        error = "prte_util_register_stackhandlers";
        goto error;
    }

    /* set system resource limits - internally protected against
     * doing so twice in cases where the launch agent did it for us
     */
    if (PRTE_SUCCESS != (ret = prte_util_init_sys_limits(&error))) {
        pmix_show_help("help-prte-runtime.txt", "prte_init:syslimit", false, error);
        return PRTE_ERR_SILENT;
    }

    ret = pmix_mca_base_framework_open(&prte_prtebacktrace_base_framework,
                                       PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PRTE_SUCCESS != ret) {
        error = "prte_backtrace_base_open";
        goto error;
    }

    return PRTE_SUCCESS;

error:
    if (PRTE_ERR_SILENT != ret) {
        pmix_show_help("help-prte-runtime", "prte_init:startup:internal-failure", true, error,
                       PRTE_ERROR_NAME(ret), ret);
    }

    return ret;
}

int prte_init(int *pargc, char ***pargv, prte_proc_type_t flags)
{
    int ret;
    char *error = NULL;

    PMIX_ACQUIRE_THREAD(&prte_init_lock);
    if (prte_initialized) {
        PMIX_RELEASE_THREAD(&prte_init_lock);
        return PRTE_SUCCESS;
    }
    PMIX_RELEASE_THREAD(&prte_init_lock);

    ret = prte_init_util(flags);
    if (PRTE_SUCCESS != ret) {
        return ret;
    }

    /*
     * Initialize the event library
     */
    if (PRTE_SUCCESS != (ret = prte_event_base_open())) {
        error = "prte_event_base_open";
        goto error;
    }

    /* setup the locks */
    if (PRTE_SUCCESS != (ret = prte_locks_init())) {
        error = "prte_locks_init";
        goto error;
    }

    /* Ensure the rest of the process info structure is initialized */
    if (PRTE_SUCCESS != (ret = prte_proc_info())) {
        error = "prte_proc_info";
        goto error;
    }

    if (PRTE_SUCCESS != (ret = prte_hwloc_base_register())) {
        error = "prte_hwloc_base_register";
        goto error;
    }

    /* let the pmix server register params */
    pmix_server_register_params();

    /* open hwloc */
    prte_hwloc_base_open();

    /* setup the global job and node arrays */
    prte_job_data = PMIX_NEW(pmix_pointer_array_t);
    ret = pmix_pointer_array_init(prte_job_data,
                                  PRTE_GLOBAL_ARRAY_BLOCK_SIZE,
                                  PRTE_GLOBAL_ARRAY_MAX_SIZE,
                                  PRTE_GLOBAL_ARRAY_BLOCK_SIZE);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        error = "setup job array";
        goto error;
    }
    prte_node_pool = PMIX_NEW(pmix_pointer_array_t);
    ret = pmix_pointer_array_init(prte_node_pool, PRTE_GLOBAL_ARRAY_BLOCK_SIZE,
                                  PRTE_GLOBAL_ARRAY_MAX_SIZE,
                                  PRTE_GLOBAL_ARRAY_BLOCK_SIZE);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        error = "setup node array";
        goto error;
    }
    prte_node_topologies = PMIX_NEW(pmix_pointer_array_t);
    ret = pmix_pointer_array_init(prte_node_topologies, PRTE_GLOBAL_ARRAY_BLOCK_SIZE,
                                  PRTE_GLOBAL_ARRAY_MAX_SIZE,
                                  PRTE_GLOBAL_ARRAY_BLOCK_SIZE);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        error = "setup node topologies array";
        goto error;
    }

    /* open the SCHIZO framework as everyone needs it, and the
     * ess will use it to help select its component */
    ret = pmix_mca_base_framework_open(&prte_schizo_base_framework,
                                       PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        error = "prte_schizo_base_open";
        goto error;
    }

    if (PRTE_SUCCESS != (ret = prte_schizo_base_select())) {
        error = "prte_schizo_base_select";
        goto error;
    }

    /* open the ESS and select the correct module for this environment */
    ret = pmix_mca_base_framework_open(&prte_ess_base_framework,
                                       PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        error = "prte_ess_base_open";
        goto error;
    }

    if (PRTE_SUCCESS != (ret = prte_ess_base_select())) {
        error = "prte_ess_base_select";
        goto error;
    }

    /* initialize the RTE for this environment */
    if (PRTE_SUCCESS != (ret = prte_ess.init(*pargc, *pargv))) {
        error = "prte_ess_init";
        goto error;
    }
    /* add network aliases to our list of alias hostnames */
    pmix_ifgetaliases(&prte_process_info.aliases);

    /* initialize the cache */
    prte_cache = PMIX_NEW(pmix_pointer_array_t);
    pmix_pointer_array_init(prte_cache, 1, INT_MAX, 1);

    /* All done */
    PMIX_ACQUIRE_THREAD(&prte_init_lock);
    prte_initialized = true;
    PMIX_RELEASE_THREAD(&prte_init_lock);
    return PRTE_SUCCESS;

error:
    if (PRTE_ERR_SILENT != ret) {
        pmix_show_help("help-prte-runtime", "prte_init:startup:internal-failure", true, error,
                       PRTE_ERROR_NAME(ret), ret);
    }

    return ret;
}

static bool check_pmix_overlap(char *var, char *value)
{
    char *tmp;

    if (0 == strncmp(var, "dl_", 3)) {
        pmix_asprintf(&tmp, "PMIX_MCA_pdl_%s", &var[3]);
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "oob_", 4) &&
               NULL == strstr(var, "verbose")) {
        pmix_asprintf(&tmp, "PMIX_MCA_ptl_%s", &var[4]);
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "hwloc_", 6)) {
        pmix_asprintf(&tmp, "PMIX_MCA_%s", var);
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "if_", 3)) {
        // need to convert if to pif
        pmix_asprintf(&tmp, "PMIX_MCA_pif_%s", &var[3]);
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "mca_", 4)) {
        pmix_asprintf(&tmp, "PMIX_MCA_%s", var);
        setenv(tmp, value, false);
        free(tmp);
        return true;
    }
    return false;
}

void prte_preload_default_mca_params(void)
{
    char *file, *home, *tmp;
    pmix_list_t params, params2, pfinal;
    pmix_mca_base_var_file_value_t *fv, *fv2, *fvnext, *fvnext2;
    bool match;

    home = (char*)pmix_home_directory(-1);
    PMIX_CONSTRUCT(&params, pmix_list_t);
    PMIX_CONSTRUCT(&params2, pmix_list_t);
    PMIX_CONSTRUCT(&pfinal, pmix_list_t);

    /* start with the system-level defaults */
    file = pmix_os_path(false, prte_install_dirs.sysconfdir, "prte-mca-params.conf", NULL);
    pmix_mca_base_parse_paramfile(file, &params);
    free(file);

    /* now get the user-level defaults */
    file = pmix_os_path(false, home, ".prte", "mca-params.conf", NULL);
    pmix_mca_base_parse_paramfile(file, &params2);
    free(file);

    /* cross-check the lists, keeping the params2 entries over any
     * matching params entries as they overwrite the system ones */
    PMIX_LIST_FOREACH_SAFE(fv, fvnext, &params, pmix_mca_base_var_file_value_t) {
        match = false;
        PMIX_LIST_FOREACH_SAFE(fv2, fvnext2, &params2, pmix_mca_base_var_file_value_t) {
            /* do we have a match? */
            if (0 == strcmp(fv->mbvfv_var, fv2->mbvfv_var)) {
                /* transfer the user-level default to the final list */
                pmix_list_remove_item(&params2, &fv2->super);
                pmix_list_append(&pfinal, &fv2->super);
                /* remove and release the system-level duplicate */
                pmix_list_remove_item(&params, &fv->super);
                PMIX_RELEASE(fv);
                match = true;
                break;
            }
        }
        if (!match) {
            /* transfer the system-level default to the final list */
            pmix_list_remove_item(&params, &fv->super);
            pmix_list_append(&pfinal, &fv->super);
        }
    }
    /* transfer any remaining use-level defaults to the final list
     * as they had no matches */
    while (NULL != (fv2 = (pmix_mca_base_var_file_value_t*)pmix_list_remove_first(&params2))) {
        pmix_list_append(&pfinal, &fv2->super);
    }

    /* now process the final list - but do not overwrite if the
     * user already has the param in our environment as their
     * environment settings override all defaults */
    PMIX_LIST_FOREACH(fv, &pfinal, pmix_mca_base_var_file_value_t) {
        if (pmix_pmdl_base_check_prte_param(fv->mbvfv_var)) {
            pmix_asprintf(&tmp, "PRTE_MCA_%s", fv->mbvfv_var);
            // set it, but don't overwrite if they already
            // have a value in our environment
            setenv(tmp, fv->mbvfv_value, false);
            free(tmp);
            // if this relates to the DL, OOB, HWLOC, or IF,
            // or mca frameworks, then we also need to set
            // the equivalent PMIx value
            check_pmix_overlap(fv->mbvfv_var, fv->mbvfv_value);
        } else if (pmix_pmdl_base_check_pmix_param(fv->mbvfv_var)) {
            pmix_asprintf(&tmp, "PMIX_MCA_%s", fv->mbvfv_var);
            // set it, but don't overwrite if they already
            // have a value in our environment
            setenv(tmp, fv->mbvfv_value, false);
            free(tmp);
        }
    }

    PMIX_LIST_DESTRUCT(&params);
    PMIX_LIST_DESTRUCT(&params2);
    PMIX_LIST_DESTRUCT(&pfinal);

}
