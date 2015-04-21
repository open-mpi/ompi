/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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

#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/show_help.h"
#include "opal/mca/shmem/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/schizo/schizo.h"

static int parse_cli(char *personality,
                     int argc, int start, char **argv);
static int parse_env(char *personality,
                     char *path,
                     opal_cmd_line_t *cmd_line,
                     char *server,
                     char **srcenv,
                     char ***dstenv);
static int setup_fork(orte_job_t *jdata,
                      orte_app_context_t *context);
static int setup_child(orte_job_t *jobdat,
                       orte_proc_t *child,
                       orte_app_context_t *app);

orte_schizo_base_module_t orte_schizo_ompi_module = {
    parse_cli,
    parse_env,
    setup_fork,
    setup_child
};

static int parse_cli(char *personality,
                     int argc, int start, char **argv)
{
    int i, j, k;
    bool ignore;
    char *no_dups[] = {
        "grpcomm",
        "odls",
        "rml",
        "routed",
        NULL
    };
    
    for (i = 0; i < (argc-start); ++i) {
        if (0 == strcmp("-mca",  argv[i]) ||
            0 == strcmp("--mca", argv[i]) ) {
            /* ignore this one */
            if (0 == strcmp(argv[i+1], "mca_base_env_list")) {
                i += 2;
                continue;
            }
            /* It would be nice to avoid increasing the length
             * of the orted cmd line by removing any non-ORTE
             * params. However, this raises a problem since
             * there could be OPAL directives that we really
             * -do- want the orted to see - it's only the OMPI
             * related directives we could ignore. This becomes
             * a very complicated procedure, however, since
             * the OMPI mca params are not cleanly separated - so
             * filtering them out is nearly impossible.
             *
             * see if this is already present so we at least can
             * avoid growing the cmd line with duplicates
             */
            ignore = false;
            if (NULL != orted_cmd_line) {
                for (j=0; NULL != orted_cmd_line[j]; j++) {
                    if (0 == strcmp(argv[i+1], orted_cmd_line[j])) {
                        /* already here - if the value is the same,
                         * we can quitely ignore the fact that they
                         * provide it more than once. However, some
                         * frameworks are known to have problems if the
                         * value is different. We don't have a good way
                         * to know this, but we at least make a crude
                         * attempt here to protect ourselves.
                         */
                        if (0 == strcmp(argv[i+2], orted_cmd_line[j+1])) {
                            /* values are the same */
                            ignore = true;
                            break;
                        } else {
                            /* values are different - see if this is a problem */
                            for (k=0; NULL != no_dups[k]; k++) {
                                if (0 == strcmp(no_dups[k], argv[i+1])) {
                                    /* print help message
                                     * and abort as we cannot know which one is correct
                                     */
                                    orte_show_help("help-orterun.txt", "orterun:conflicting-params",
                                                   true, orte_basename, argv[i+1],
                                                   argv[i+2], orted_cmd_line[j+1]);
                                    return ORTE_ERR_BAD_PARAM;
                                }
                            }
                            /* this passed muster - just ignore it */
                            ignore = true;
                            break;
                        }
                    }
                }
            }
            if (!ignore) {
                opal_argv_append_nosize(&orted_cmd_line, argv[i]);
                opal_argv_append_nosize(&orted_cmd_line, argv[i+1]);
                opal_argv_append_nosize(&orted_cmd_line, argv[i+2]);
            }
            i += 2;
        }
    }
    return ORTE_SUCCESS;
}

static int parse_env(char *personality,
                     char *path,
                     opal_cmd_line_t *cmd_line,
                     char *ompi_server,
                     char **srcenv,
                     char ***dstenv)
{
    int i, j;
    char *param;
    char *value;
    char *env_set_flag;
    char **vars;
    
    for (i = 0; NULL != srcenv[i]; ++i) {
        if (0 == strncmp("OMPI_", srcenv[i], 5)) {
            /* check for duplicate in app->env - this
             * would have been placed there by the
             * cmd line processor. By convention, we
             * always let the cmd line override the
             * environment
             */
            param = strdup(srcenv[i]);
            value = strchr(param, '=');
            *value = '\0';
            value++;
            opal_setenv(param, value, false, dstenv);
            free(param);
        }
    }
    
    /* add the ompi-server, if provided */
    if (NULL != ompi_server) {
        opal_setenv("OMPI_MCA_pubsub_orte_server", ompi_server, true, dstenv);
    }

    /* set necessary env variables for external usage from tune conf file*/
    int set_from_file = 0;
    vars = NULL;
    if (OPAL_SUCCESS == mca_base_var_process_env_list_from_file(&vars) &&
            NULL != vars) {
        for (i=0; NULL != vars[i]; i++) {
            value = strchr(vars[i], '=');
            /* terminate the name of the param */
            *value = '\0';
            /* step over the equals */
            value++;
            /* overwrite any prior entry */
            opal_setenv(vars[i], value, true, dstenv);
            /* save it for any comm_spawn'd apps */
            opal_setenv(vars[i], value, true, &orte_forwarded_envars);
        }
        set_from_file = 1;
        opal_argv_free(vars);
    }
    /* Did the user request to export any environment variables on the cmd line? */
    env_set_flag = getenv("OMPI_MCA_mca_base_env_list");
    if (opal_cmd_line_is_taken(cmd_line, "x")) {
        if (NULL != env_set_flag) {
            orte_show_help("help-orterun.txt", "orterun:conflict-env-set", false);
            return ORTE_ERR_FATAL;
        }
        j = opal_cmd_line_get_ninsts(cmd_line, "x");
        for (i = 0; i < j; ++i) {
            param = opal_cmd_line_get_param(cmd_line, "x", i, 0);

            if (NULL != (value = strchr(param, '='))) {
                /* terminate the name of the param */
                *value = '\0';
                /* step over the equals */
                value++;
                /* overwrite any prior entry */
                opal_setenv(param, value, true, dstenv);
                /* save it for any comm_spawn'd apps */
                opal_setenv(param, value, true, &orte_forwarded_envars);
            } else {
                value = getenv(param);
                if (NULL != value) {
                    /* overwrite any prior entry */
                    opal_setenv(param, value, true, dstenv);
                    /* save it for any comm_spawn'd apps */
                    opal_setenv(param, value, true, &orte_forwarded_envars);
                } else {
                    opal_output(0, "Warning: could not find environment variable \"%s\"\n", param);
                }
            }
        }
    } else if (NULL != env_set_flag) {
        /* if mca_base_env_list was set, check if some of env vars were set via -x from a conf file.
         * If this is the case, error out.
         */
        if (!set_from_file) {
            /* set necessary env variables for external usage */
            vars = NULL;
            if (OPAL_SUCCESS == mca_base_var_process_env_list(&vars) &&
                    NULL != vars) {
                for (i=0; NULL != vars[i]; i++) {
                    value = strchr(vars[i], '=');
                    /* terminate the name of the param */
                    *value = '\0';
                    /* step over the equals */
                    value++;
                    /* overwrite any prior entry */
                    opal_setenv(vars[i], value, true, dstenv);
                    /* save it for any comm_spawn'd apps */
                    opal_setenv(vars[i], value, true, &orte_forwarded_envars);
                }
                opal_argv_free(vars);
            }
        } else {
            orte_show_help("help-orterun.txt", "orterun:conflict-env-set", false);
            return ORTE_ERR_FATAL;
        }
    }

    /* If the user specified --path, store it in the user's app
       environment via the OMPI_exec_path variable. */
    if (NULL != path) {
        asprintf(&value, "OMPI_exec_path=%s", path);
        opal_argv_append_nosize(dstenv, value);
        /* save it for any comm_spawn'd apps */
        opal_argv_append_nosize(&orte_forwarded_envars, value);
        free(value);
    }

    return ORTE_SUCCESS;
}

static int setup_fork(orte_job_t *jdata,
                      orte_app_context_t *app)
{
    int i;
    char *param;
    bool oversubscribed;
    orte_node_t *node;
    char **envcpy, **nps, **firstranks;
    char *npstring, *firstrankstring;
    char *num_app_ctx;
    
    /* see if the mapper thinks we are oversubscribed */
    oversubscribed = false;
    if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, ORTE_PROC_MY_NAME->vpid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_FLAG_TEST(node, ORTE_NODE_FLAG_OVERSUBSCRIBED)) {
        oversubscribed = true;
    }

    /* setup base environment: copy the current environ and merge
       in the app context environ */
    if (NULL != app->env) {
        /* manually free original context->env to avoid a memory leak */
        char **tmp = app->env;
        envcpy = opal_environ_merge(orte_launch_environ, app->env);
        if (NULL != tmp) {
            opal_argv_free(tmp);
        }
    } else {
        envcpy = opal_argv_copy(orte_launch_environ);
    }
    app->env = envcpy;
    
    /* special case handling for --prefix: this is somewhat icky,
       but at least some users do this.  :-\ It is possible that
       when using --prefix, the user will also "-x PATH" and/or
       "-x LD_LIBRARY_PATH", which would therefore clobber the
       work that was done in the prior pls to ensure that we have
       the prefix at the beginning of the PATH and
       LD_LIBRARY_PATH.  So examine the context->env and see if we
       find PATH or LD_LIBRARY_PATH.  If found, that means the
       prior work was clobbered, and we need to re-prefix those
       variables. */
    param = NULL;
    orte_get_attribute(&app->attributes, ORTE_APP_PREFIX_DIR, (void**)&param, OPAL_STRING);
    for (i = 0; NULL != param && NULL != app->env && NULL != app->env[i]; ++i) {
        char *newenv;
        
        /* Reset PATH */
        if (0 == strncmp("PATH=", app->env[i], 5)) {
            asprintf(&newenv, "%s/bin:%s", param, app->env[i] + 5);
            opal_setenv("PATH", newenv, true, &app->env);
            free(newenv);
        }
        
        /* Reset LD_LIBRARY_PATH */
        else if (0 == strncmp("LD_LIBRARY_PATH=", app->env[i], 16)) {
            asprintf(&newenv, "%s/lib:%s", param, app->env[i] + 16);
            opal_setenv("LD_LIBRARY_PATH", newenv, true, &app->env);
            free(newenv);
        }
    }
    if (NULL != param) {
        free(param);
    }

    /* pass my contact info to the local proc so we can talk */
    opal_setenv("OMPI_MCA_orte_local_daemon_uri", orte_process_info.my_daemon_uri, true, &app->env);
    
    /* pass the hnp's contact info to the local proc in case it
     * needs it
     */
    if (NULL != orte_process_info.my_hnp_uri) {
        opal_setenv("OMPI_MCA_orte_hnp_uri", orte_process_info.my_hnp_uri, true, &app->env);
    }
    
    /* setup yield schedule - do not override any user-supplied directive! */
    if (oversubscribed) {
        opal_setenv("OMPI_MCA_mpi_yield_when_idle", "1", false, &app->env);
    } else {
        opal_setenv("OMPI_MCA_mpi_yield_when_idle", "0", false, &app->env);
    }
    
    /* set the app_context number into the environment */
    asprintf(&param, "%ld", (long)app->idx);
    opal_setenv("OMPI_MCA_orte_app_num", param, true, &app->env);
    free(param);
    
    /* although the total_slots_alloc is the universe size, users
     * would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here. Also required by the ompi_attributes code!
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    asprintf(&param, "%ld", (long)jdata->total_slots_alloc);
    opal_setenv("OMPI_UNIVERSE_SIZE", param, true, &app->env);
    free(param);
    
    /* pass the number of nodes involved in this job */
    asprintf(&param, "%ld", (long)(jdata->map->num_nodes));
    opal_setenv("OMPI_MCA_orte_num_nodes", param, true, &app->env);
    free(param);

#if OPAL_HAVE_HWLOC
    {
        /* pass a param telling the child what type and model of cpu we are on,
         * if we know it. If hwloc has the value, use what it knows. Otherwise,
         * see if we were explicitly given it and use that value.
         */
        hwloc_obj_t obj;
        char *htmp;
        if (NULL != opal_hwloc_topology) {
            obj = hwloc_get_root_obj(opal_hwloc_topology);
            if (NULL != (htmp = (char*)hwloc_obj_get_info_by_name(obj, "CPUType")) ||
                NULL != (htmp = orte_local_cpu_type)) {
                opal_setenv("OMPI_MCA_orte_cpu_type", htmp, true, &app->env);
            }
            if (NULL != (htmp = (char*)hwloc_obj_get_info_by_name(obj, "CPUModel")) ||
                NULL != (htmp = orte_local_cpu_model)) {
                opal_setenv("OMPI_MCA_orte_cpu_model", htmp, true, &app->env);
            }
        } else {
            if (NULL != orte_local_cpu_type) {
                opal_setenv("OMPI_MCA_orte_cpu_type", orte_local_cpu_type, true, &app->env);
            }
            if (NULL != orte_local_cpu_model) {
                opal_setenv("OMPI_MCA_orte_cpu_model", orte_local_cpu_model, true, &app->env);
            }
        }
    }
#endif

    /* get shmem's best component name so we can provide a hint to the shmem
     * framework. the idea here is to have someone figure out what component to
     * select (via the shmem framework) and then have the rest of the
     * components in shmem obey that decision. for more details take a look at
     * the shmem framework in opal.
     */
    if (NULL != (param = opal_shmem_base_best_runnable_component_name())) {
        opal_setenv("OMPI_MCA_shmem_RUNTIME_QUERY_hint", param, true, &app->env);
        free(param);
    }
    
    /* Set an info MCA param that tells the launched processes that
     * any binding policy was applied by us (e.g., so that
     * MPI_INIT doesn't try to bind itself)
     */
    opal_setenv("OMPI_MCA_orte_bound_at_launch", "1", true, &app->env);

    /* tell the ESS to select the pmi component - but don't override
     * anything that may have been provided elsewhere
     */
    opal_setenv("OMPI_MCA_ess", "pmi", false, &app->env);

    /* since we want to pass the name as separate components, make sure
     * that the "name" environmental variable is cleared!
     */
    opal_unsetenv("OMPI_MCA_orte_ess_name", &app->env);

    asprintf(&param, "%ld", (long)jdata->num_procs);
    opal_setenv("OMPI_MCA_orte_ess_num_procs", param, true, &app->env);

    /* although the num_procs is the comm_world size, users
     * would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    opal_setenv("OMPI_COMM_WORLD_SIZE", param, true, &app->env);
    free(param);

    /* users would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    asprintf(&param, "%ld", (long)jdata->num_local_procs);
    opal_setenv("OMPI_COMM_WORLD_LOCAL_SIZE", param, true, &app->env);
    free(param);
        
    /* forcibly set the local tmpdir base to match ours */
    opal_setenv("OMPI_MCA_orte_tmpdir_base", orte_process_info.tmpdir_base, true, &app->env);

    /* MPI-3 requires we provide some further info to the procs,
     * so we pass them as envars to avoid introducing further
     * ORTE calls in the MPI layer
     */
    asprintf(&num_app_ctx, "%lu", (unsigned long)jdata->num_apps);

    /* build some common envars we need to pass for MPI-3 compatibility */
    nps = NULL;
    firstranks = NULL;
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        opal_argv_append_nosize(&nps, ORTE_VPID_PRINT(app->num_procs));
        opal_argv_append_nosize(&firstranks, ORTE_VPID_PRINT(app->first_rank));
    }
    npstring = opal_argv_join(nps, ' ');
    firstrankstring = opal_argv_join(firstranks, ' ');
    opal_argv_free(nps);
    opal_argv_free(firstranks);

    /* add the MPI-3 envars */
    opal_setenv("OMPI_NUM_APP_CTX", num_app_ctx, true, &app->env);
    opal_setenv("OMPI_FIRST_RANKS", firstrankstring, true, &app->env);
    opal_setenv("OMPI_APP_CTX_NUM_PROCS", npstring, true, &app->env);
    free(num_app_ctx);
    free(firstrankstring);
    free(npstring);
    return ORTE_SUCCESS;
}


static int setup_child(orte_job_t *jdata,
                       orte_proc_t *child,
                       orte_app_context_t *app)
{
    char *param, *value;
    int rc;
    int32_t nrestarts=0, *nrptr;

    /* setup the jobid */
    if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&value, child->name.jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    opal_setenv("OMPI_MCA_ess_base_jobid", value, true, &app->env);
    free(value);

    /* setup the vpid */
    if (ORTE_SUCCESS != (rc = orte_util_convert_vpid_to_string(&value, child->name.vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    opal_setenv("OMPI_MCA_ess_base_vpid", value, true, &app->env);

    /* although the vpid IS the process' rank within the job, users
     * would appreciate being given a public environmental variable
     * that also represents this value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    opal_setenv("OMPI_COMM_WORLD_RANK", value, true, &app->env);
    free(value);  /* done with this now */
    
    /* users would appreciate being given a public environmental variable
     * that also represents the local rank value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    if (ORTE_LOCAL_RANK_INVALID == child->local_rank) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
        return rc;
    }
    asprintf(&value, "%lu", (unsigned long) child->local_rank);
    opal_setenv("OMPI_COMM_WORLD_LOCAL_RANK", value, true, &app->env);
    free(value);
    
    /* users would appreciate being given a public environmental variable
     * that also represents the node rank value - something MPI specific - so
     * do that here.
     *
     * AND YES - THIS BREAKS THE ABSTRACTION BARRIER TO SOME EXTENT.
     * We know - just live with it
     */
    if (ORTE_NODE_RANK_INVALID == child->node_rank) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
        return rc;
    }
    asprintf(&value, "%lu", (unsigned long) child->node_rank);
    opal_setenv("OMPI_COMM_WORLD_NODE_RANK", value, true, &app->env);
    /* set an mca param for it too */
    opal_setenv("OMPI_MCA_orte_ess_node_rank", value, true, &app->env);
    free(value);

    /* provide the identifier for the PMIx connection - the
     * PMIx connection is made prior to setting the process
     * name itself. Although in most cases the ID and the
     * process name are the same, it isn't necessarily
     * required */
    orte_util_convert_process_name_to_string(&value, &child->name);
    opal_setenv("PMIX_ID", value, true, &app->env);
    free(value);

    nrptr = &nrestarts;
    if (orte_get_attribute(&child->attributes, ORTE_PROC_NRESTARTS, (void**)&nrptr, OPAL_INT32)) {
        /* pass the number of restarts for this proc - will be zero for
         * an initial start, but procs would like to know if they are being
         * restarted so they can take appropriate action
         */
        asprintf(&value, "%d", nrestarts);
        opal_setenv("OMPI_MCA_orte_num_restarts", value, true, &app->env);
        free(value);
    }
    
    /* if the proc should not barrier in orte_init, tell it */
    if (orte_get_attribute(&child->attributes, ORTE_PROC_NOBARRIER, NULL, OPAL_BOOL)
        || 0 < nrestarts) {
        opal_setenv("OMPI_MCA_orte_do_not_barrier", "1", true, &app->env);
    }
    
    /* if we are using staged execution, tell it */
    if (orte_staged_execution) {
        opal_setenv("OMPI_MCA_orte_staged_execution", "1", true, &app->env);
    }

    /* if the proc isn't going to forward IO, then we need to flag that
     * it has "completed" iof termination as otherwise it will never fire
     */
    if (!ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_FORWARD_OUTPUT)) {
        ORTE_FLAG_SET(child, ORTE_PROC_FLAG_IOF_COMPLETE);
    }

    /* construct the proc's session dir name */
    if (NULL != orte_process_info.tmpdir_base) {
        value = strdup(orte_process_info.tmpdir_base);
    } else {
        value = NULL;
    }
    param = NULL;
    if (ORTE_SUCCESS != (rc = orte_session_dir_get_name(&param, &value, NULL,
                                                        orte_process_info.nodename,
                                                        NULL, &child->name))) {
        ORTE_ERROR_LOG(rc);
        if (NULL != value) {
            free(value);
        }
        return rc;
    }
    free(value);
    /* pass an envar so the proc can find any files it had prepositioned */
    opal_setenv("OMPI_FILE_LOCATION", param, true, &app->env);

    /* if the user wanted the cwd to be the proc's session dir, then
     * switch to that location now
     */
    if (orte_get_attribute(&app->attributes, ORTE_APP_SSNDIR_CWD, NULL, OPAL_BOOL)) {
        /* create the session dir - may not exist */
        if (OPAL_SUCCESS != (rc = opal_os_dirpath_create(param, S_IRWXU))) {
            ORTE_ERROR_LOG(rc);
            /* doesn't exist with correct permissions, and/or we can't
             * create it - either way, we are done
             */
            free(param);
            return rc;
        }
        /* change to it */
        if (0 != chdir(param)) {
            free(param);
            return ORTE_ERROR;
        }
        /* It seems that chdir doesn't
         * adjust the $PWD enviro variable when it changes the directory. This
         * can cause a user to get a different response when doing getcwd vs
         * looking at the enviro variable. To keep this consistent, we explicitly
         * ensure that the PWD enviro variable matches the CWD we moved to.
         *
         * NOTE: if a user's program does a chdir(), then $PWD will once
         * again not match getcwd! This is beyond our control - we are only
         * ensuring they start out matching.
         */
        opal_setenv("PWD", param, true, &app->env);
        /* update the initial wdir value too */
        opal_setenv("OMPI_MCA_initial_wdir", param, true, &app->env);
    }
    free(param);
    return ORTE_SUCCESS;
}
