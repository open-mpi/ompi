/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2018 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif  /* HAVE_STRINGS_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */
#include <fcntl.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "opal/mca/event/event.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/cmd_line.h"
#include "opal/util/opal_environ.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/show_help.h"
#include "opal/util/fd.h"
#include "opal/sys/atomic.h"

#include "opal/version.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_info_support.h"
#include "opal/runtime/opal_progress_threads.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/schizo/base/base.h"
#include "orte/mca/state/state.h"
#include "orte/orted/orted_submit.h"

/* ensure I can behave like a daemon */
#include "prun.h"

typedef struct {
    opal_object_t super;
    opal_pmix_lock_t lock;
    opal_list_t info;
} myinfo_t;
static void mcon(myinfo_t *p)
{
    OPAL_PMIX_CONSTRUCT_LOCK(&p->lock);
    OBJ_CONSTRUCT(&p->info, opal_list_t);
}
static void mdes(myinfo_t *p)
{
    OPAL_PMIX_DESTRUCT_LOCK(&p->lock);
    OPAL_LIST_DESTRUCT(&p->info);
}
static OBJ_CLASS_INSTANCE(myinfo_t, opal_object_t,
                          mcon, mdes);

static struct {
    bool terminate_dvm;
    bool system_server_first;
    bool system_server_only;
    int pid;
} myoptions;

static opal_list_t job_info;
static volatile bool active = false;
static orte_jobid_t myjobid = ORTE_JOBID_INVALID;
static myinfo_t myinfo;

static int create_app(int argc, char* argv[],
                      opal_list_t *jdata,
                      opal_pmix_app_t **app,
                      bool *made_app, char ***app_env);
static int parse_locals(opal_list_t *jdata, int argc, char* argv[]);
static void set_classpath_jar_file(opal_pmix_app_t *app, int index, char *jarfile);
static size_t evid = INT_MAX;


static opal_cmd_line_init_t cmd_line_init[] = {
    /* tell the dvm to terminate */
    { NULL, '\0', "terminate", "terminate", 0,
      &myoptions.terminate_dvm, OPAL_CMD_LINE_TYPE_BOOL,
      "Terminate the DVM", OPAL_CMD_LINE_OTYPE_DVM },

    /* look first for a system server */
    { NULL, '\0', "system-server-first", "system-server-first", 0,
      &myoptions.system_server_first, OPAL_CMD_LINE_TYPE_BOOL,
      "First look for a system server and connect to it if found", OPAL_CMD_LINE_OTYPE_DVM },

    /* connect only to a system server */
    { NULL, '\0', "system-server-only", "system-server-only", 0,
      &myoptions.system_server_only, OPAL_CMD_LINE_TYPE_BOOL,
      "Connect only to a system-level server", OPAL_CMD_LINE_OTYPE_DVM },

    /* provide a connection PID */
    { NULL, '\0', "pid", "pid", 1,
      &myoptions.pid, OPAL_CMD_LINE_TYPE_INT,
      "PID of the session-level daemon to which we should connect",
      OPAL_CMD_LINE_OTYPE_DVM },

    /* End of list */
    { NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};


static void infocb(int status,
                   opal_list_t *info,
                   void *cbdata,
                   opal_pmix_release_cbfunc_t release_fn,
                   void *release_cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;
    OPAL_ACQUIRE_OBJECT(lock);

    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    OPAL_PMIX_WAKEUP_THREAD(lock);
}

static void regcbfunc(int status, size_t ref, void *cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;
    OPAL_ACQUIRE_OBJECT(lock);
    evid = ref;
    OPAL_PMIX_WAKEUP_THREAD(lock);
}

static void opcbfunc(int status, void *cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;
    OPAL_ACQUIRE_OBJECT(lock);
    OPAL_PMIX_WAKEUP_THREAD(lock);
}

static bool fired = false;
static void evhandler(int status,
                      const opal_process_name_t *source,
                      opal_list_t *info, opal_list_t *results,
                      opal_pmix_notification_complete_fn_t cbfunc,
                      void *cbdata)
{
    opal_value_t *val;
    int jobstatus=0;
    orte_jobid_t jobid = ORTE_JOBID_INVALID;

    /* we should always have info returned to us - if not, there is
     * nothing we can do */
    if (NULL != info) {
        OPAL_LIST_FOREACH(val, info, opal_value_t) {
            if (0 == strcmp(val->key, OPAL_PMIX_JOB_TERM_STATUS)) {
                jobstatus = val->data.integer;
            } else if (0 == strcmp(val->key, OPAL_PMIX_PROCID)) {
                jobid = val->data.name.jobid;
            }
        }
        if (orte_cmd_options.verbose && (myjobid != ORTE_JOBID_INVALID && jobid == myjobid)) {
            opal_output(0, "JOB %s COMPLETED WITH STATUS %d",
                        ORTE_JOBID_PRINT(jobid), jobstatus);
        }
    }

    /* only terminate if this was our job - keep in mind that we
     * can get notifications of job termination prior to our spawn
     * having completed! */
    if (!fired && (myjobid != ORTE_JOBID_INVALID && jobid == myjobid)) {
        fired = true;
        active = false;
    }

    /* we _always_ have to execute the evhandler callback or
     * else the event progress engine will hang */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, NULL, NULL, NULL, cbdata);
    }
}

typedef struct {
    opal_pmix_lock_t lock;
    opal_list_t list;
} mylock_t;


static void setupcbfunc(int status,
                        opal_list_t *info,
                        void *provided_cbdata,
                        opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    mylock_t *mylock = (mylock_t*)provided_cbdata;
    opal_value_t *kv;

    if (NULL != info) {
        /* cycle across the provided info */
        while (NULL != (kv = (opal_value_t*)opal_list_remove_first(info))) {
            opal_list_append(&mylock->list, &kv->super);
        }
    }

    /* release the caller */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }

    OPAL_PMIX_WAKEUP_THREAD(&mylock->lock);
}

static void launchhandler(int status,
                          const opal_process_name_t *source,
                          opal_list_t *info, opal_list_t *results,
                          opal_pmix_notification_complete_fn_t cbfunc,
                          void *cbdata)
{
    opal_value_t *p;

    /* the info list will include the launch directives, so
     * transfer those to the myinfo_t for return to the main thread */
    while (NULL != (p = (opal_value_t*)opal_list_remove_first(info))) {
        opal_list_append(&myinfo.info, &p->super);
    }

    /* we _always_ have to execute the evhandler callback or
     * else the event progress engine will hang */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, NULL, NULL, NULL, cbdata);
    }

    /* now release the thread */
    OPAL_PMIX_WAKEUP_THREAD(&myinfo.lock);
}

int prun(int argc, char *argv[])
{
    int rc, i;
    char *param;
    opal_pmix_lock_t lock;
    opal_list_t apps, *lt;
    opal_pmix_app_t *app;
    opal_value_t *val, *kv, *kv2;
    opal_list_t info, codes;
    struct timespec tp = {0, 100000};
    mylock_t mylock;

    /* init the globals */
    memset(&orte_cmd_options, 0, sizeof(orte_cmd_options));
    memset(&myoptions, 0, sizeof(myoptions));
    OBJ_CONSTRUCT(&job_info, opal_list_t);
    OBJ_CONSTRUCT(&apps, opal_list_t);

    /* search the argv for MCA params */
    for (i=0; NULL != argv[i]; i++) {
        if (':' == argv[i][0] ||
            NULL == argv[i+1] || NULL == argv[i+2]) {
            break;
        }
        if (0 == strncmp(argv[i], "-"OPAL_MCA_CMD_LINE_ID, strlen("-"OPAL_MCA_CMD_LINE_ID)) ||
            0 == strncmp(argv[i], "--"OPAL_MCA_CMD_LINE_ID, strlen("--"OPAL_MCA_CMD_LINE_ID)) ||
            0 == strncmp(argv[i], "-g"OPAL_MCA_CMD_LINE_ID, strlen("-g"OPAL_MCA_CMD_LINE_ID)) ||
            0 == strncmp(argv[i], "--g"OPAL_MCA_CMD_LINE_ID, strlen("--g"OPAL_MCA_CMD_LINE_ID))) {
            (void) mca_base_var_env_name (argv[i+1], &param);
            opal_setenv(param, argv[i+2], true, &environ);
            free(param);
        } else if (0 == strcmp(argv[i], "-am") ||
                   0 == strcmp(argv[i], "--am")) {
            (void)mca_base_var_env_name("mca_base_param_file_prefix", &param);
            opal_setenv(param, argv[i+1], true, &environ);
            free(param);
        } else if (0 == strcmp(argv[i], "-tune") ||
                   0 == strcmp(argv[i], "--tune")) {
            (void)mca_base_var_env_name("mca_base_envar_file_prefix", &param);
            opal_setenv(param, argv[i+1], true, &environ);
            free(param);
        }
    }

    /* init only the util portion of OPAL */
    if (OPAL_SUCCESS != (rc = opal_init_util(&argc, &argv))) {
        return rc;
    }

    /* set our proc type for schizo selection */
    orte_process_info.proc_type = ORTE_PROC_TOOL;

    /* open the SCHIZO framework so we can setup the command line */
    if (ORTE_SUCCESS != (rc = mca_base_framework_open(&orte_schizo_base_framework, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_schizo_base_select())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* setup our cmd line */
    orte_cmd_line = OBJ_NEW(opal_cmd_line_t);
    if (OPAL_SUCCESS != (rc = opal_cmd_line_add(orte_cmd_line, cmd_line_init))) {
        return rc;
    }

    /* setup the rest of the cmd line only once */
    if (OPAL_SUCCESS != (rc = orte_schizo.define_cli(orte_cmd_line))) {
        return rc;
    }

    /* now that options have been defined, finish setup */
    mca_base_cmd_line_setup(orte_cmd_line);

    /* parse the result to get values */
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(orte_cmd_line,
                                                  true, false, argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(rc));
        }
        return rc;
    }

    /* see if print version is requested. Do this before
     * check for help so that --version --help works as
     * one might expect. */
     if (orte_cmd_options.version) {
        char *str;
        str = opal_info_make_version_str("all",
                                         OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                         OPAL_RELEASE_VERSION,
                                         OPAL_GREEK_VERSION,
                                         OPAL_REPO_REV);
        if (NULL != str) {
            fprintf(stdout, "%s (%s) %s\n\nReport bugs to %s\n",
                    "prun", "PMIx Reference Server", str, PACKAGE_BUGREPORT);
            free(str);
        }
        exit(0);
    }

    /* check if we are running as root - if we are, then only allow
     * us to proceed if the allow-run-as-root flag was given. Otherwise,
     * exit with a giant warning flag
     */
    if (0 == geteuid() && !orte_cmd_options.run_as_root) {
        /* show_help is not yet available, so print an error manually */
        fprintf(stderr, "--------------------------------------------------------------------------\n");
        if (orte_cmd_options.help) {
            fprintf(stderr, "prun cannot provide the help message when run as root.\n\n");
        } else {
            fprintf(stderr, "prun has detected an attempt to run as root.\n\n");
        }

        fprintf(stderr, "Running as root is *strongly* discouraged as any mistake (e.g., in\n");
        fprintf(stderr, "defining TMPDIR) or bug can result in catastrophic damage to the OS\n");
        fprintf(stderr, "file system, leaving your system in an unusable state.\n\n");

        fprintf(stderr, "We strongly suggest that you run prun as a non-root user.\n\n");

        fprintf(stderr, "You can override this protection by adding the --allow-run-as-root\n");
        fprintf(stderr, "option to your command line.  However, we reiterate our strong advice\n");
        fprintf(stderr, "against doing so - please do so at your own risk.\n");
        fprintf(stderr, "--------------------------------------------------------------------------\n");
        exit(1);
    }

    /* process any mca params */
    rc = mca_base_cmd_line_process_args(orte_cmd_line, &environ, &environ);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    /* Check for help request */
    if (orte_cmd_options.help) {
        char *str, *args = NULL;
        args = opal_cmd_line_get_usage_msg(orte_cmd_line);
        str = opal_show_help_string("help-orterun.txt", "orterun:usage", false,
                                    "prun", "PSVR", OPAL_VERSION,
                                    "prun", args,
                                    PACKAGE_BUGREPORT);
        if (NULL != str) {
            printf("%s", str);
            free(str);
        }
        free(args);

        /* If someone asks for help, that should be all we do */
        exit(0);
    }

    /* ensure we ONLY take the ess/tool component */
    opal_setenv(OPAL_MCA_PREFIX"ess", "tool", true, &environ);
    /* tell the ess/tool component how we want to connect */
    if (myoptions.system_server_only) {
        opal_setenv(OPAL_MCA_PREFIX"ess_tool_system_server_only", "1", true, &environ);
    }
    if (myoptions.system_server_first) {
        opal_setenv(OPAL_MCA_PREFIX"ess_tool_system_server_first", "1", true, &environ);
    }
    /* if they specified the DVM's pid, then pass it along */
    if (0 != myoptions.pid) {
        opal_asprintf(&param, "%d", myoptions.pid);
        opal_setenv(OPAL_MCA_PREFIX"ess_tool_server_pid", param, true, &environ);
        free(param);
    }
    /* if they specified the URI, then pass it along */
    if (NULL != orte_cmd_options.hnp) {
        opal_setenv("PMIX_MCA_ptl_tcp_server_uri", orte_cmd_options.hnp, true, &environ);
    }

    /* now initialize ORTE */
    if (OPAL_SUCCESS != (rc = orte_init(&argc, &argv, ORTE_PROC_TOOL))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }

    /* if the user just wants us to terminate a DVM, then do so */
    if (myoptions.terminate_dvm) {
        OBJ_CONSTRUCT(&info, opal_list_t);
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_JOB_CTRL_TERMINATE);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&info, &val->super);
        fprintf(stderr, "TERMINATING DVM...");
        OPAL_PMIX_CONSTRUCT_LOCK(&lock);
        rc = opal_pmix.job_control(NULL, &info, infocb, (void*)&lock);
        OPAL_PMIX_WAIT_THREAD(&lock);
        OPAL_PMIX_DESTRUCT_LOCK(&lock);
        OPAL_LIST_DESTRUCT(&info);
        fprintf(stderr, "DONE\n");
        goto DONE;
    }

    /* get here if they want to run an application, so let's parse
     * the cmd line to get it */

    if (OPAL_SUCCESS != (rc = parse_locals(&apps, argc, argv))) {
        OPAL_ERROR_LOG(rc);
        OPAL_LIST_DESTRUCT(&apps);
        goto DONE;
    }

    /* bozo check */
    if (0 == opal_list_get_size(&apps)) {
        opal_output(0, "No application specified!");
        goto DONE;
    }

    /* init flag */
    active = true;

    /* register for job terminations so we get notified when
     * our job completes */
    OPAL_PMIX_CONSTRUCT_LOCK(&lock);
    OBJ_CONSTRUCT(&info, opal_list_t);
    val = OBJ_NEW(opal_value_t);
    val->key = strdup("foo");
    val->type = OPAL_INT;
    val->data.integer = OPAL_ERR_JOB_TERMINATED;
    opal_list_append(&info, &val->super);
    opal_pmix.register_evhandler(&info, NULL, evhandler, regcbfunc, &lock);
    OPAL_PMIX_WAIT_THREAD(&lock);
    OPAL_PMIX_DESTRUCT_LOCK(&lock);
    OPAL_LIST_DESTRUCT(&info);

    /* we want to be notified upon job completion */
    val = OBJ_NEW(opal_value_t);
    val->key = strdup(OPAL_PMIX_NOTIFY_COMPLETION);
    val->type = OPAL_BOOL;
    val->data.flag = true;
    opal_list_append(&job_info, &val->super);

    /* see if they specified the personality */
    if (NULL != orte_cmd_options.personality) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_PERSONALITY);
        val->type = OPAL_STRING;
        val->data.string = strdup(orte_cmd_options.personality);
        opal_list_append(&job_info, &val->super);
    }

    /* check for stdout/err directives */
    /* if we were asked to tag output, mark it so */
    if (orte_cmd_options.tag_output) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_TAG_OUTPUT);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }
    /* if we were asked to timestamp output, mark it so */
    if (orte_cmd_options.timestamp_output) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_TIMESTAMP_OUTPUT);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }
    /* if we were asked to output to files, pass it along */
    if (NULL != orte_cmd_options.output_filename) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_OUTPUT_TO_FILE);
        val->type = OPAL_STRING;
        /* if the given filename isn't an absolute path, then
         * convert it to one so the name will be relative to
         * the directory where prun was given as that is what
         * the user will have seen */
        if (!opal_path_is_absolute(orte_cmd_options.output_filename)) {
            char cwd[OPAL_PATH_MAX];
            getcwd(cwd, sizeof(cwd));
            val->data.string = opal_os_path(false, cwd, orte_cmd_options.output_filename, NULL);
        } else {
            val->data.string = strdup(orte_cmd_options.output_filename);
        }
        opal_list_append(&job_info, &val->super);
    }
    /* if we were asked to merge stderr to stdout, mark it so */
    if (orte_cmd_options.merge) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_MERGE_STDERR_STDOUT);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }

    /* check what user wants us to do with stdin */
    if (NULL != orte_cmd_options.stdin_target) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_STDIN_TGT);
        val->type = OPAL_UINT32;
        opal_list_append(&job_info, &val->super);
        if (0 == strcmp(orte_cmd_options.stdin_target, "all")) {
            val->data.uint32 = ORTE_VPID_WILDCARD;
        } else if (0 == strcmp(orte_cmd_options.stdin_target, "none")) {
            val->data.uint32 = ORTE_VPID_INVALID;
        } else {
            val->data.uint32 = strtoul(orte_cmd_options.stdin_target, NULL, 10);
        }
    }

    /* if we want the argv's indexed, indicate that */
    if (orte_cmd_options.index_argv) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_INDEX_ARGV);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }

    if (NULL != orte_cmd_options.mapping_policy) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_MAPBY);
        val->type = OPAL_STRING;
        val->data.string = strdup(orte_cmd_options.mapping_policy);
        opal_list_append(&job_info, &val->super);
    } else if (orte_cmd_options.pernode) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_PPR);
        val->type = OPAL_STRING;
        val->data.string = strdup("1:node");
        opal_list_append(&job_info, &val->super);
    } else if (0 < orte_cmd_options.npernode) {
        /* define the ppr */
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_PPR);
        val->type = OPAL_STRING;
        opal_asprintf(&val->data.string, "%d:node", orte_cmd_options.npernode);
        opal_list_append(&job_info, &val->super);
    } else if (0 < orte_cmd_options.npersocket) {
        /* define the ppr */
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_PPR);
        val->type = OPAL_STRING;
        opal_asprintf(&val->data.string, "%d:socket", orte_cmd_options.npernode);
        opal_list_append(&job_info, &val->super);
    }

    /* if the user specified cpus/rank, set it */
    if (0 < orte_cmd_options.cpus_per_proc) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_CPUS_PER_PROC);
        val->type = OPAL_UINT32;
        val->data.uint32 = orte_cmd_options.cpus_per_proc;
        opal_list_append(&job_info, &val->super);
    }

    /* if the user specified a ranking policy, then set it */
    if (NULL != orte_cmd_options.ranking_policy) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_RANKBY);
        val->type = OPAL_STRING;
        val->data.string = strdup(orte_cmd_options.ranking_policy);
        opal_list_append(&job_info, &val->super);
    }

    /* if the user specified a binding policy, then set it */
    if (NULL != orte_cmd_options.binding_policy) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_BINDTO);
        val->type = OPAL_STRING;
        val->data.string = strdup(orte_cmd_options.binding_policy);
        opal_list_append(&job_info, &val->super);
    }

    /* if they asked for nolocal, mark it so */
    if (orte_cmd_options.nolocal) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_NO_PROCS_ON_HEAD);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }
    if (orte_cmd_options.no_oversubscribe) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_NO_OVERSUBSCRIBE);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }
    if (orte_cmd_options.oversubscribe) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_NO_OVERSUBSCRIBE);
        val->type = OPAL_BOOL;
        val->data.flag = false;
        opal_list_append(&job_info, &val->super);
    }
    if (orte_cmd_options.report_bindings) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_REPORT_BINDINGS);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }
    if (NULL != orte_cmd_options.cpu_list) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_CPU_LIST);
        val->type = OPAL_STRING;
        val->data.string = strdup(orte_cmd_options.cpu_list);
        opal_list_append(&job_info, &val->super);
    }

    /* mark if recovery was enabled on the cmd line */
    if (orte_enable_recovery) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_JOB_RECOVERABLE);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }
    /* record the max restarts */
    if (0 < orte_max_restarts) {
        OPAL_LIST_FOREACH(app, &apps, opal_pmix_app_t) {
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_MAX_RESTARTS);
            val->type = OPAL_UINT32;
            val->data.uint32 = orte_max_restarts;
            opal_list_append(&app->info, &val->super);
        }
    }
    /* if continuous operation was specified */
    if (orte_cmd_options.continuous) {
        /* mark this job as continuously operating */
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_JOB_CONTINUOUS);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&job_info, &val->super);
    }

    /* pickup any relevant envars */
    if (NULL != opal_pmix.server_setup_application) {
        OBJ_CONSTRUCT(&info, opal_list_t);
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_SETUP_APP_ENVARS);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&info, &val->super);

        OPAL_PMIX_CONSTRUCT_LOCK(&mylock.lock);
        OBJ_CONSTRUCT(&mylock.list, opal_list_t);
        rc = opal_pmix.server_setup_application(ORTE_PROC_MY_NAME->jobid,
                                                &info, setupcbfunc, &mylock);
        if (OPAL_SUCCESS != rc) {
            OPAL_LIST_DESTRUCT(&info);
            OPAL_PMIX_DESTRUCT_LOCK(&mylock.lock);
            OBJ_DESTRUCT(&mylock.list);
            goto DONE;
        }
        OPAL_PMIX_WAIT_THREAD(&mylock.lock);
        OPAL_PMIX_DESTRUCT_LOCK(&mylock.lock);
        /* transfer any returned ENVARS to the job_info */
        while (NULL != (val = (opal_value_t*)opal_list_remove_first(&mylock.list))) {
            if (0 == strcmp(val->key, OPAL_PMIX_SET_ENVAR) ||
                0 == strcmp(val->key, OPAL_PMIX_ADD_ENVAR) ||
                0 == strcmp(val->key, OPAL_PMIX_UNSET_ENVAR) ||
                0 == strcmp(val->key, OPAL_PMIX_PREPEND_ENVAR) ||
                0 == strcmp(val->key, OPAL_PMIX_APPEND_ENVAR)) {
                opal_list_append(&job_info, &val->super);
            } else {
                OBJ_RELEASE(val);
            }
        }
        OPAL_LIST_DESTRUCT(&mylock.list);
    }

    /* if we were launched by a tool wanting to direct our
     * operation, then we need to pause here and give it
     * a chance to tell us what we need to do */
    if (NULL != (param = getenv("PMIX_LAUNCHER_PAUSE_FOR_TOOL")) &&
        0 == strcmp(param, "1")) {
        /* register for the PMIX_LAUNCH_DIRECTIVE event */
        OPAL_PMIX_CONSTRUCT_LOCK(&lock);
        OBJ_CONSTRUCT(&codes, opal_list_t);
        val = OBJ_NEW(opal_value_t);
        val->key = strdup("foo");
        val->type = OPAL_INT;
        val->data.integer = OPAL_PMIX_LAUNCH_DIRECTIVE;
        opal_list_append(&codes, &val->super);
        /* setup the myinfo object to capture the returned
         * values - must do so prior to registering in case
         * the event has already arrived */
        OBJ_CONSTRUCT(&myinfo, myinfo_t);
        /* go ahead and register */
        opal_pmix.register_evhandler(&codes, NULL, launchhandler, regcbfunc, &lock);
        OPAL_PMIX_WAIT_THREAD(&lock);
        OPAL_PMIX_DESTRUCT_LOCK(&lock);
        OPAL_LIST_DESTRUCT(&codes);
        /* now wait for the launch directives to arrive */
        OPAL_PMIX_WAIT_THREAD(&myinfo.lock);
        /* process the returned directives */
        OPAL_LIST_FOREACH(val, &myinfo.info, opal_value_t) {
            if (0 == strcmp(val->key, OPAL_PMIX_DEBUG_JOB_DIRECTIVES)) {
                /* there will be a pointer to a list containing the directives */
                lt = (opal_list_t*)val->data.ptr;
                while (NULL != (kv = (opal_value_t*)opal_list_remove_first(lt))) {
                    opal_output(0, "JOB DIRECTIVE: %s", kv->key);
                    opal_list_append(&job_info, &kv->super);
                }
            } else if (0 == strcmp(val->key, OPAL_PMIX_DEBUG_APP_DIRECTIVES)) {
                /* there will be a pointer to a list containing the directives */
                lt = (opal_list_t*)val->data.ptr;
                OPAL_LIST_FOREACH(kv, lt, opal_value_t) {
                    opal_output(0, "APP DIRECTIVE: %s", kv->key);
                    OPAL_LIST_FOREACH(app, &apps, opal_pmix_app_t) {
                        /* the value can only be on one list at a time, so replicate it */
                        kv2 = OBJ_NEW(opal_value_t);
                        opal_value_xfer(kv2, kv);
                        opal_list_append(&app->info, &kv2->super);
                    }
                }
            }
        }
    }

    if (OPAL_SUCCESS != (rc = opal_pmix.spawn(&job_info, &apps, &myjobid))) {
        opal_output(0, "Job failed to spawn: %s", opal_strerror(rc));
        goto DONE;
    }
    OPAL_LIST_DESTRUCT(&job_info);
    OPAL_LIST_DESTRUCT(&apps);

    if (orte_cmd_options.verbose) {
        opal_output(0, "JOB %s EXECUTING", OPAL_JOBID_PRINT(myjobid));
    }

    while (active) {
        nanosleep(&tp, NULL);
    }
    OPAL_PMIX_CONSTRUCT_LOCK(&lock);
    opal_pmix.deregister_evhandler(evid, opcbfunc, &lock);
    OPAL_PMIX_WAIT_THREAD(&lock);
    OPAL_PMIX_DESTRUCT_LOCK(&lock);

  DONE:
    /* cleanup and leave */
    orte_finalize();
    return 0;
}

static int parse_locals(opal_list_t *jdata, int argc, char* argv[])
{
    int i, rc;
    int temp_argc;
    char **temp_argv, **env;
    opal_pmix_app_t *app;
    bool made_app;

    /* Make the apps */
    temp_argc = 0;
    temp_argv = NULL;
    opal_argv_append(&temp_argc, &temp_argv, argv[0]);

    /* NOTE: This bogus env variable is necessary in the calls to
       create_app(), below.  See comment immediately before the
       create_app() function for an explanation. */

    env = NULL;
    for (i = 1; i < argc; ++i) {
        if (0 == strcmp(argv[i], ":")) {
            /* Make an app with this argv */
            if (opal_argv_count(temp_argv) > 1) {
                if (NULL != env) {
                    opal_argv_free(env);
                    env = NULL;
                }
                app = NULL;
                rc = create_app(temp_argc, temp_argv, jdata, &app, &made_app, &env);
                if (OPAL_SUCCESS != rc) {
                    /* Assume that the error message has already been
                       printed; no need to cleanup -- we can just
                       exit */
                    exit(1);
                }
                if (made_app) {
                    opal_list_append(jdata, &app->super);
                }

                /* Reset the temps */

                temp_argc = 0;
                temp_argv = NULL;
                opal_argv_append(&temp_argc, &temp_argv, argv[0]);
            }
        } else {
            opal_argv_append(&temp_argc, &temp_argv, argv[i]);
        }
    }

    if (opal_argv_count(temp_argv) > 1) {
        app = NULL;
        rc = create_app(temp_argc, temp_argv, jdata, &app, &made_app, &env);
        if (ORTE_SUCCESS != rc) {
            /* Assume that the error message has already been printed;
               no need to cleanup -- we can just exit */
            exit(1);
        }
        if (made_app) {
            opal_list_append(jdata, &app->super);
        }
    }
    if (NULL != env) {
        opal_argv_free(env);
    }
    opal_argv_free(temp_argv);

    /* All done */

    return ORTE_SUCCESS;
}


/*
 * This function takes a "char ***app_env" parameter to handle the
 * specific case:
 *
 *   orterun --mca foo bar -app appfile
 *
 * That is, we'll need to keep foo=bar, but the presence of the app
 * file will cause an invocation of parse_appfile(), which will cause
 * one or more recursive calls back to create_app().  Since the
 * foo=bar value applies globally to all apps in the appfile, we need
 * to pass in the "base" environment (that contains the foo=bar value)
 * when we parse each line in the appfile.
 *
 * This is really just a special case -- when we have a simple case like:
 *
 *   orterun --mca foo bar -np 4 hostname
 *
 * Then the upper-level function (parse_locals()) calls create_app()
 * with a NULL value for app_env, meaning that there is no "base"
 * environment that the app needs to be created from.
 */
static int create_app(int argc, char* argv[],
                      opal_list_t *jdata,
                      opal_pmix_app_t **app_ptr,
                      bool *made_app, char ***app_env)
{
    char cwd[OPAL_PATH_MAX];
    int i, j, count, rc;
    char *param, *value;
    opal_pmix_app_t *app = NULL;
    bool found = false;
    char *appname = NULL;
    opal_value_t *val;

    *made_app = false;

    /* parse the cmd line - do this every time thru so we can
     * repopulate the globals */
    if (OPAL_SUCCESS != (rc = opal_cmd_line_parse(orte_cmd_line, true, false,
                                                  argc, argv)) ) {
        if (OPAL_ERR_SILENT != rc) {
            fprintf(stderr, "%s: command line error (%s)\n", argv[0],
                    opal_strerror(rc));
        }
        return rc;
    }

    /* Setup application context */
    app = OBJ_NEW(opal_pmix_app_t);
    opal_cmd_line_get_tail(orte_cmd_line, &count, &app->argv);

    /* See if we have anything left */
    if (0 == count) {
        opal_show_help("help-orterun.txt", "orterun:executable-not-specified",
                       true, "prun", "prun");
        rc = OPAL_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Grab all MCA environment variables */
    app->env = opal_argv_copy(*app_env);
    for (i=0; NULL != environ[i]; i++) {
        if (0 == strncmp("PMIX_", environ[i], 5) ||
            0 == strncmp("OMPI_", environ[i], 5)) {
            /* check for duplicate in app->env - this
             * would have been placed there by the
             * cmd line processor. By convention, we
             * always let the cmd line override the
             * environment
             */
            param = strdup(environ[i]);
            value = strchr(param, '=');
            *value = '\0';
            value++;
            opal_setenv(param, value, false, &app->env);
            free(param);
        }
    }

    /* set necessary env variables for external usage from tune conf file*/
    int set_from_file = 0;
    char **vars = NULL;
    if (OPAL_SUCCESS == mca_base_var_process_env_list_from_file(&vars) &&
            NULL != vars) {
        for (i=0; NULL != vars[i]; i++) {
            value = strchr(vars[i], '=');
            /* terminate the name of the param */
            *value = '\0';
            /* step over the equals */
            value++;
            /* overwrite any prior entry */
            opal_setenv(vars[i], value, true, &app->env);
            /* save it for any comm_spawn'd apps */
            opal_setenv(vars[i], value, true, &orte_forwarded_envars);
        }
        set_from_file = 1;
        opal_argv_free(vars);
    }
    /* Did the user request to export any environment variables on the cmd line? */
    char *env_set_flag;
    env_set_flag = getenv("OMPI_MCA_mca_base_env_list");
    if (opal_cmd_line_is_taken(orte_cmd_line, "x")) {
        if (NULL != env_set_flag) {
            opal_show_help("help-orterun.txt", "orterun:conflict-env-set", false);
            return ORTE_ERR_FATAL;
        }
        j = opal_cmd_line_get_ninsts(orte_cmd_line, "x");
        for (i = 0; i < j; ++i) {
            param = opal_cmd_line_get_param(orte_cmd_line, "x", i, 0);

            if (NULL != (value = strchr(param, '='))) {
                /* terminate the name of the param */
                *value = '\0';
                /* step over the equals */
                value++;
                /* overwrite any prior entry */
                opal_setenv(param, value, true, &app->env);
                /* save it for any comm_spawn'd apps */
                opal_setenv(param, value, true, &orte_forwarded_envars);
            } else {
                value = getenv(param);
                if (NULL != value) {
                    /* overwrite any prior entry */
                    opal_setenv(param, value, true, &app->env);
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
            if (OPAL_SUCCESS == mca_base_var_process_env_list(env_set_flag, &vars) &&
                    NULL != vars) {
                for (i=0; NULL != vars[i]; i++) {
                    value = strchr(vars[i], '=');
                    /* terminate the name of the param */
                    *value = '\0';
                    /* step over the equals */
                    value++;
                    /* overwrite any prior entry */
                    opal_setenv(vars[i], value, true, &app->env);
                    /* save it for any comm_spawn'd apps */
                    opal_setenv(vars[i], value, true, &orte_forwarded_envars);
                }
                opal_argv_free(vars);
            }
        } else {
            opal_show_help("help-orterun.txt", "orterun:conflict-env-set", false);
            return ORTE_ERR_FATAL;
        }
    }

    /* Did the user request a specific wdir? */

    if (NULL != orte_cmd_options.wdir) {
        /* if this is a relative path, convert it to an absolute path */
        if (opal_path_is_absolute(orte_cmd_options.wdir)) {
            app->cwd = strdup(orte_cmd_options.wdir);
        } else {
            /* get the cwd */
            if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
                opal_show_help("help-orterun.txt", "orterun:init-failure",
                               true, "get the cwd", rc);
                goto cleanup;
            }
            /* construct the absolute path */
            app->cwd = opal_os_path(false, cwd, orte_cmd_options.wdir, NULL);
        }
    } else if (orte_cmd_options.set_cwd_to_session_dir) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_SET_SESSION_CWD);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&app->info, &val->super);
    } else {
        if (OPAL_SUCCESS != (rc = opal_getcwd(cwd, sizeof(cwd)))) {
            opal_show_help("help-orterun.txt", "orterun:init-failure",
                           true, "get the cwd", rc);
            goto cleanup;
        }
        app->cwd = strdup(cwd);
    }

    /* Did the user specify a hostfile. Need to check for both
     * hostfile and machine file.
     * We can only deal with one hostfile per app context, otherwise give an error.
     */
    found = false;
    if (0 < (j = opal_cmd_line_get_ninsts(orte_cmd_line, "hostfile"))) {
        if (1 < j) {
            opal_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, "prun", NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(orte_cmd_line, "hostfile", 0, 0);
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_HOSTFILE);
            val->type = OPAL_STRING;
            val->data.string = value;
            opal_list_append(&app->info, &val->super);
            found = true;
        }
    }
    if (0 < (j = opal_cmd_line_get_ninsts(orte_cmd_line, "machinefile"))) {
        if (1 < j || found) {
            opal_show_help("help-orterun.txt", "orterun:multiple-hostfiles",
                           true, "prun", NULL);
            return ORTE_ERR_FATAL;
        } else {
            value = opal_cmd_line_get_param(orte_cmd_line, "machinefile", 0, 0);
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_HOSTFILE);
            val->type = OPAL_STRING;
            val->data.string = value;
            opal_list_append(&app->info, &val->super);
        }
    }

    /* Did the user specify any hosts? */
    if (0 < (j = opal_cmd_line_get_ninsts(orte_cmd_line, "host"))) {
        char **targ=NULL, *tval;
        for (i = 0; i < j; ++i) {
            value = opal_cmd_line_get_param(orte_cmd_line, "host", i, 0);
            opal_argv_append_nosize(&targ, value);
        }
        tval = opal_argv_join(targ, ',');
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_HOST);
        val->type = OPAL_STRING;
        val->data.string = tval;
        opal_list_append(&app->info, &val->super);
    }

    /* check for bozo error */
    if (0 > orte_cmd_options.num_procs) {
        opal_show_help("help-orterun.txt", "orterun:negative-nprocs",
                       true, "prun", app->argv[0],
                       orte_cmd_options.num_procs, NULL);
        return ORTE_ERR_FATAL;
    }

    app->maxprocs = orte_cmd_options.num_procs;

    /* see if we need to preload the binary to
     * find the app - don't do this for java apps, however, as we
     * can't easily find the class on the cmd line. Java apps have to
     * preload their binary via the preload_files option
     */
    if (NULL == strstr(app->argv[0], "java")) {
        if (orte_cmd_options.preload_binaries) {
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_SET_SESSION_CWD);
            val->type = OPAL_BOOL;
            val->data.flag = true;
            opal_list_append(&app->info, &val->super);
            val = OBJ_NEW(opal_value_t);
            val->key = strdup(OPAL_PMIX_PRELOAD_BIN);
            val->type = OPAL_BOOL;
            val->data.flag = true;
            opal_list_append(&app->info, &val->super);
        }
    }
    if (NULL != orte_cmd_options.preload_files) {
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_PRELOAD_FILES);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(&app->info, &val->super);
    }

    /* Do not try to find argv[0] here -- the starter is responsible
       for that because it may not be relevant to try to find it on
       the node where orterun is executing.  So just strdup() argv[0]
       into app. */

    app->cmd = strdup(app->argv[0]);
    if (NULL == app->cmd) {
        opal_show_help("help-orterun.txt", "orterun:call-failed",
                       true, "prun", "library", "strdup returned NULL", errno);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* if this is a Java application, we have a bit more work to do. Such
     * applications actually need to be run under the Java virtual machine
     * and the "java" command will start the "executable". So we need to ensure
     * that all the proper java-specific paths are provided
     */
    appname = opal_basename(app->cmd);
    if (0 == strcmp(appname, "java")) {
        /* see if we were given a library path */
        found = false;
        for (i=1; NULL != app->argv[i]; i++) {
            if (NULL != strstr(app->argv[i], "java.library.path")) {
                char *dptr;
                /* find the '=' that delineates the option from the path */
                if (NULL == (dptr = strchr(app->argv[i], '='))) {
                    /* that's just wrong */
                    rc = ORTE_ERR_BAD_PARAM;
                    goto cleanup;
                }
                /* step over the '=' */
                ++dptr;
                /* yep - but does it include the path to the mpi libs? */
                found = true;
                if (NULL == strstr(app->argv[i], opal_install_dirs.libdir)) {
                    /* doesn't appear to - add it to be safe */
                    if (':' == app->argv[i][strlen(app->argv[i]-1)]) {
                        opal_asprintf(&value, "-Djava.library.path=%s%s", dptr, opal_install_dirs.libdir);
                    } else {
                        opal_asprintf(&value, "-Djava.library.path=%s:%s", dptr, opal_install_dirs.libdir);
                    }
                    free(app->argv[i]);
                    app->argv[i] = value;
                }
                break;
            }
        }
        if (!found) {
            /* need to add it right after the java command */
            opal_asprintf(&value, "-Djava.library.path=%s", opal_install_dirs.libdir);
            opal_argv_insert_element(&app->argv, 1, value);
            free(value);
        }

        /* see if we were given a class path */
        found = false;
        for (i=1; NULL != app->argv[i]; i++) {
            if (NULL != strstr(app->argv[i], "cp") ||
                NULL != strstr(app->argv[i], "classpath")) {
                /* yep - but does it include the path to the mpi libs? */
                found = true;
                /* check if mpi.jar exists - if so, add it */
                value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    set_classpath_jar_file(app, i+1, "mpi.jar");
                }
                free(value);
                /* check for oshmem support */
                value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    set_classpath_jar_file(app, i+1, "shmem.jar");
                }
                free(value);
                /* always add the local directory */
                opal_asprintf(&value, "%s:%s", app->cwd, app->argv[i+1]);
                free(app->argv[i+1]);
                app->argv[i+1] = value;
                break;
            }
        }
        if (!found) {
            /* check to see if CLASSPATH is in the environment */
            found = false;  // just to be pedantic
            for (i=0; NULL != environ[i]; i++) {
                if (0 == strncmp(environ[i], "CLASSPATH", strlen("CLASSPATH"))) {
                    value = strchr(environ[i], '=');
                    ++value; /* step over the = */
                    opal_argv_insert_element(&app->argv, 1, value);
                    /* check for mpi.jar */
                    value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                    if (access(value, F_OK ) != -1) {
                        set_classpath_jar_file(app, 1, "mpi.jar");
                    }
                    free(value);
                    /* check for shmem.jar */
                    value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                    if (access(value, F_OK ) != -1) {
                        set_classpath_jar_file(app, 1, "shmem.jar");
                    }
                    free(value);
                    /* always add the local directory */
                    opal_asprintf(&value, "%s:%s", app->cwd, app->argv[1]);
                    free(app->argv[1]);
                    app->argv[1] = value;
                    opal_argv_insert_element(&app->argv, 1, "-cp");
                    found = true;
                    break;
                }
            }
            if (!found) {
                /* need to add it right after the java command - have
                 * to include the working directory and trust that
                 * the user set cwd if necessary
                 */
                char *str, *str2;
                /* always start with the working directory */
                str = strdup(app->cwd);
                /* check for mpi.jar */
                value = opal_os_path(false, opal_install_dirs.libdir, "mpi.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    opal_asprintf(&str2, "%s:%s", str, value);
                    free(str);
                    str = str2;
                }
                free(value);
                /* check for shmem.jar */
                value = opal_os_path(false, opal_install_dirs.libdir, "shmem.jar", NULL);
                if (access(value, F_OK ) != -1) {
                    opal_asprintf(&str2, "%s:%s", str, value);
                    free(str);
                    str = str2;
                }
                free(value);
                opal_argv_insert_element(&app->argv, 1, str);
                free(str);
                opal_argv_insert_element(&app->argv, 1, "-cp");
            }
        }
        /* try to find the actual command - may not be perfect */
        for (i=1; i < opal_argv_count(app->argv); i++) {
            if (NULL != strstr(app->argv[i], "java.library.path")) {
                continue;
            } else if (NULL != strstr(app->argv[i], "cp") ||
                       NULL != strstr(app->argv[i], "classpath")) {
                /* skip the next field */
                i++;
                continue;
            }
            /* declare this the winner */
            opal_setenv("OMPI_COMMAND", app->argv[i], true, &app->env);
            /* collect everything else as the cmd line */
            if ((i+1) < opal_argv_count(app->argv)) {
                value = opal_argv_join(&app->argv[i+1], ' ');
                opal_setenv("OMPI_ARGV", value, true, &app->env);
                free(value);
            }
            break;
        }
    } else {
        /* add the cmd to the environment for MPI_Info to pickup */
        opal_setenv("OMPI_COMMAND", appname, true, &app->env);
        if (1 < opal_argv_count(app->argv)) {
            value = opal_argv_join(&app->argv[1], ' ');
            opal_setenv("OMPI_ARGV", value, true, &app->env);
            free(value);
        }
    }

    *app_ptr = app;
    app = NULL;
    *made_app = true;

    /* All done */

 cleanup:
    if (NULL != app) {
        OBJ_RELEASE(app);
    }
    if (NULL != appname) {
        free(appname);
    }
    return rc;
}

static void set_classpath_jar_file(opal_pmix_app_t *app, int index, char *jarfile)
{
    if (NULL == strstr(app->argv[index], jarfile)) {
        /* nope - need to add it */
        char *fmt = ':' == app->argv[index][strlen(app->argv[index]-1)]
                    ? "%s%s/%s" : "%s:%s/%s";
        char *str;
        opal_asprintf(&str, fmt, app->argv[index], opal_install_dirs.libdir, jarfile);
        free(app->argv[index]);
        app->argv[index] = str;
    }
}
