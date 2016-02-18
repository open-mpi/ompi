/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#include <errno.h>

#include "opal/hash_string.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/mca/pmix/pmix.h"

#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/plm/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/session_dir.h"
#include "orte/util/pre_condition_transports.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/singleton/ess_singleton.h"


static int rte_init(void);
static int rte_finalize(void);

orte_ess_base_module_t orte_ess_singleton_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    NULL /* ft_event */
};

extern char *orte_ess_singleton_server_uri;
static bool added_transport_keys=false;
static bool added_num_procs = false;
static bool added_app_ctx = false;
static bool added_pmix_envs = false;
static char *pmixenvars[4];

static int fork_hnp(void);

static int rte_init(void)
{
    int rc, ret;
    char *error = NULL;
    char *envar, *ev1, *ev2;
    uint64_t unique_key[2];
    char *string_key;
    opal_value_t *kv;
    char *val;
    int u32, *u32ptr;
    uint16_t u16, *u16ptr;

    /* run the prolog */
    if (ORTE_SUCCESS != (rc = orte_ess_base_std_prolog())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    u32ptr = &u32;
    u16ptr = &u16;

    if (NULL != orte_ess_singleton_server_uri) {
        /* we are going to connect to a server HNP */
        if (0 == strncmp(orte_ess_singleton_server_uri, "file", strlen("file")) ||
            0 == strncmp(orte_ess_singleton_server_uri, "FILE", strlen("FILE"))) {
            char input[1024], *filename;
            FILE *fp;

            /* it is a file - get the filename */
            filename = strchr(orte_ess_singleton_server_uri, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-bad", true,
                               "singleton", orte_ess_singleton_server_uri);
                return ORTE_ERROR;
            }
            ++filename; /* space past the : */

            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-missing", true,
                               "singleton", orte_ess_singleton_server_uri);
                return ORTE_ERROR;
            }

            /* open the file and extract the uri */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-access", true,
                               "singleton", orte_ess_singleton_server_uri);
                return ORTE_ERROR;
            }
            memset(input, 0, 1024);  // initialize the array to ensure a NULL termination
            if (NULL == fgets(input, 1023, fp)) {
                /* something malformed about file */
                fclose(fp);
                orte_show_help("help-orterun.txt", "orterun:ompi-server-file-bad", true,
                               "singleton", orte_ess_singleton_server_uri, "singleton");
                return ORTE_ERROR;
            }
            fclose(fp);
            input[strlen(input)-1] = '\0';  /* remove newline */
            orte_process_info.my_hnp_uri = strdup(input);
        } else {
            orte_process_info.my_hnp_uri = strdup(orte_ess_singleton_server_uri);
        }
        /* save the daemon uri - we will process it later */
        orte_process_info.my_daemon_uri = strdup(orte_process_info.my_hnp_uri);
        /* construct our name - we are in their job family, so we know that
         * much. However, we cannot know how many other singletons and jobs
         * this HNP is running. Oh well - if someone really wants to use this
         * option, they can try to figure it out. For now, we'll just assume
         * we are the only ones */
        ORTE_PROC_MY_NAME->jobid = ORTE_CONSTRUCT_LOCAL_JOBID(ORTE_PROC_MY_HNP->jobid, 1);
        /* obviously, we are vpid=0 for this job */
        ORTE_PROC_MY_NAME->vpid = 0;

        /* for convenience, push the pubsub version of this param into the environ */
        opal_setenv (OPAL_MCA_PREFIX"pubsub_orte_server", orte_process_info.my_hnp_uri, 1, &environ);
    } else if (NULL != getenv("SINGULARITY_CONTAINER")) {
         /* mark that we are in a container */
        opal_setenv("OPAL_PROC_CONTAINER", "1", true, &environ);
    } else {
        /* spawn our very own HNP to support us */
        if (ORTE_SUCCESS != (rc = fork_hnp())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* our name was given to us by the HNP */
    }

    /* open and setup pmix */
    if (NULL == opal_pmix.initialized) {
        if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_pmix_base_framework, 0))) {
            error = "opening pmix";
            goto error;
        }
        if (OPAL_SUCCESS != (ret = opal_pmix_base_select())) {
            error = "select pmix";
            goto error;
        }
    }
    /* initialize the selected module */
    if (!opal_pmix.initialized() && (OPAL_SUCCESS != (ret = opal_pmix.init()))) {
        error = "init pmix";
        goto error;
    }

    /* pmix.init set our process name down in the OPAL layer,
     * so carry it forward here */
    ORTE_PROC_MY_NAME->jobid = OPAL_PROC_MY_NAME.jobid;
    ORTE_PROC_MY_NAME->vpid = OPAL_PROC_MY_NAME.vpid;

    /* get our local rank from PMI */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_RANK,
                          ORTE_PROC_MY_NAME, &u16ptr, OPAL_UINT16);
    if (OPAL_SUCCESS != ret) {
        error = "getting local rank";
        goto error;
    }
    orte_process_info.my_local_rank = u16;

    /* get our node rank from PMI */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_NODE_RANK,
                          ORTE_PROC_MY_NAME, &u16ptr, OPAL_UINT16);
    if (OPAL_SUCCESS != ret) {
        error = "getting node rank";
        goto error;
    }
    orte_process_info.my_node_rank = u16;

    /* get universe size */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_UNIV_SIZE,
                          ORTE_PROC_MY_NAME, &u32ptr, OPAL_UINT32);
    if (OPAL_SUCCESS != ret) {
        error = "getting univ size";
        goto error;
    }
    orte_process_info.num_procs = u32;
    /* push into the environ for pickup in MPI layer for
     * MPI-3 required info key
     */
    if (NULL == getenv(OPAL_MCA_PREFIX"orte_ess_num_procs")) {
        asprintf(&ev1, OPAL_MCA_PREFIX"orte_ess_num_procs=%d", orte_process_info.num_procs);
        putenv(ev1);
        added_num_procs = true;
    }
    if (NULL == getenv("OMPI_APP_CTX_NUM_PROCS")) {
        asprintf(&ev2, "OMPI_APP_CTX_NUM_PROCS=%d", orte_process_info.num_procs);
        putenv(ev2);
        added_app_ctx = true;
    }


    /* get our app number from PMI - ok if not found */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_APPNUM,
                          ORTE_PROC_MY_NAME, &u32ptr, OPAL_UINT32);
    if (OPAL_SUCCESS == ret) {
        orte_process_info.app_num = u32;
    } else {
        orte_process_info.app_num = 0;
    }
    /* set some other standard values */
    orte_process_info.num_local_peers = 0;

    /* setup transport keys in case the MPI layer needs them -
     * we can use the jobfam and stepid as unique keys
     * because they are unique values assigned by the RM
     */
    if (NULL == getenv(OPAL_MCA_PREFIX"orte_precondition_transports")) {
        unique_key[0] = ORTE_JOB_FAMILY(ORTE_PROC_MY_NAME->jobid);
        unique_key[1] = ORTE_LOCAL_JOBID(ORTE_PROC_MY_NAME->jobid);
        if (NULL == (string_key = orte_pre_condition_transports_print(unique_key))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        asprintf(&envar, OPAL_MCA_PREFIX"orte_precondition_transports=%s", string_key);
        putenv(envar);
        added_transport_keys = true;
        /* cannot free the envar as that messes up our environ */
        free(string_key);
    }

    /* retrieve our topology */
    OPAL_MODEX_RECV_VALUE(ret, OPAL_PMIX_LOCAL_TOPO,
                          ORTE_PROC_MY_NAME, &val, OPAL_STRING);
    if (OPAL_SUCCESS == ret && NULL != val) {
        /* load the topology */
        if (0 != hwloc_topology_init(&opal_hwloc_topology)) {
            ret = OPAL_ERROR;
            free(val);
            error = "setting topology";
            goto error;
        }
        if (0 != hwloc_topology_set_xmlbuffer(opal_hwloc_topology, val, strlen(val))) {
            ret = OPAL_ERROR;
            free(val);
            hwloc_topology_destroy(opal_hwloc_topology);
            error = "setting topology";
            goto error;
        }
        /* since we are loading this from an external source, we have to
         * explicitly set a flag so hwloc sets things up correctly
         */
        if (0 != hwloc_topology_set_flags(opal_hwloc_topology,
                                         (HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM |
                                          HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM |
                                          HWLOC_TOPOLOGY_FLAG_IO_DEVICES))) {
            ret = OPAL_ERROR;
            hwloc_topology_destroy(opal_hwloc_topology);
            free(val);
            error = "setting topology";
            goto error;
        }
        /* now load the topology */
        if (0 != hwloc_topology_load(opal_hwloc_topology)) {
            ret = OPAL_ERROR;
            hwloc_topology_destroy(opal_hwloc_topology);
            free(val);
            error = "setting topology";
            goto error;
        }
        free(val);
    } else {
        /* it wasn't passed down to us, so go get it */
        if (OPAL_SUCCESS != (ret = opal_hwloc_base_get_topology())) {
            error = "topology discovery";
            goto error;
        }
        /* push it into the PMIx database in case someone
         * tries to retrieve it so we avoid an attempt to
         * get it again */
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_LOCAL_TOPO);
        kv->type = OPAL_STRING;
        if (0 != (ret = hwloc_topology_export_xmlbuffer(opal_hwloc_topology, &kv->data.string, &u32))) {
            error = "topology export";
            goto error;
        }
        if (OPAL_SUCCESS != (ret = opal_pmix.store_local(ORTE_PROC_MY_NAME, kv))) {
            error = "topology store";
            goto error;
        }
        OBJ_RELEASE(kv);
    }

    /* use the std app init to complete the procedure */
    if (ORTE_SUCCESS != (rc = orte_ess_base_app_setup(true))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* push our hostname so others can find us, if they need to */
    OPAL_MODEX_SEND_VALUE(ret, OPAL_PMIX_GLOBAL, OPAL_PMIX_HOSTNAME, orte_process_info.nodename, OPAL_STRING);
    if (ORTE_SUCCESS != ret) {
        error = "db store hostname";
        goto error;
    }

    return ORTE_SUCCESS;

 error:
    if (ORTE_ERR_SILENT != ret && !orte_report_silent_errors) {
        orte_show_help("help-orte-runtime.txt",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }
    return ret;
}

static int rte_finalize(void)
{
    int ret;

    /* remove the envars that we pushed into environ
     * so we leave that structure intact
     */
    if (added_transport_keys) {
        unsetenv(OPAL_MCA_PREFIX"orte_precondition_transports");
    }
    if (added_num_procs) {
        unsetenv(OPAL_MCA_PREFIX"orte_ess_num_procs");
    }
    if (added_app_ctx) {
        unsetenv("OMPI_APP_CTX_NUM_PROCS");
    }
    if (added_pmix_envs) {
        unsetenv("PMIX_NAMESPACE");
        unsetenv("PMIX_RANK");
        unsetenv("PMIX_SERVER_URI");
        unsetenv("PMIX_SECURITY_MODE");
    }
    /* mark us as finalized */
    if (NULL != opal_pmix.finalize) {
        opal_pmix.finalize();
        (void) mca_base_framework_close(&opal_pmix_base_framework);
    }

    /* use the default procedure to finish */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
        ORTE_ERROR_LOG(ret);
    }

    return ret;
}

#define ORTE_URI_MSG_LGTH   256

static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}

static int fork_hnp(void)
{
    int p[2], death_pipe[2];
    char *cmd;
    char **argv = NULL;
    int argc;
    char *param, *cptr;
    sigset_t sigs;
    int buffer_length, num_chars_read, chunk;
    char *orted_uri;
    int rc, i;

    /* A pipe is used to communicate between the parent and child to
       indicate whether the exec ultimately succeeded or failed.  The
       child sets the pipe to be close-on-exec; the child only ever
       writes anything to the pipe if there is an error (e.g.,
       executable not found, exec() fails, etc.).  The parent does a
       blocking read on the pipe; if the pipe closed with no data,
       then the exec() succeeded.  If the parent reads something from
       the pipe, then the child was letting us know that it failed.
    */
    if (pipe(p) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
        return ORTE_ERR_SYS_LIMITS_PIPES;
    }

    /* we also have to give the HNP a pipe it can watch to know when
     * we terminated. Since the HNP is going to be a child of us, it
     * can't just use waitpid to see when we leave - so it will watch
     * the pipe instead
     */
    if (pipe(death_pipe) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
        return ORTE_ERR_SYS_LIMITS_PIPES;
    }

    /* find the orted binary using the install_dirs support - this also
     * checks to ensure that we can see this executable and it *is* executable by us
     */
    cmd = opal_path_access("orted", opal_install_dirs.bindir, X_OK);
    if (NULL == cmd) {
        /* guess we couldn't do it - best to abort */
        ORTE_ERROR_LOG(ORTE_ERR_FILE_NOT_EXECUTABLE);
        close(p[0]);
        close(p[1]);
        return ORTE_ERR_FILE_NOT_EXECUTABLE;
    }

    /* okay, setup an appropriate argv */
    opal_argv_append(&argc, &argv, "orted");

    /* tell the daemon it is to be the HNP */
    opal_argv_append(&argc, &argv, "--hnp");

    /* tell the daemon to get out of our process group */
    opal_argv_append(&argc, &argv, "--set-sid");

    /* tell the daemon to report back its uri so we can connect to it */
    opal_argv_append(&argc, &argv, "--report-uri");
    asprintf(&param, "%d", p[1]);
    opal_argv_append(&argc, &argv, param);
    free(param);

    /* give the daemon a pipe it can watch to tell when we have died */
    opal_argv_append(&argc, &argv, "--singleton-died-pipe");
    asprintf(&param, "%d", death_pipe[0]);
    opal_argv_append(&argc, &argv, param);
    free(param);

    /* add any debug flags */
    if (orte_debug_flag) {
        opal_argv_append(&argc, &argv, "--debug");
    }

    if (orte_debug_daemons_flag) {
        opal_argv_append(&argc, &argv, "--debug-daemons");
    }

    if (orte_debug_daemons_file_flag) {
        if (!orte_debug_daemons_flag) {
            opal_argv_append(&argc, &argv, "--debug-daemons");
        }
        opal_argv_append(&argc, &argv, "--debug-daemons-file");
    }

    /* indicate that it must use the novm state machine */
    opal_argv_append(&argc, &argv, "-"OPAL_MCA_CMD_LINE_ID);
    opal_argv_append(&argc, &argv, "state_novm_select");
    opal_argv_append(&argc, &argv, "1");

    /* Fork off the child */
    orte_process_info.hnp_pid = fork();
    if(orte_process_info.hnp_pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        close(p[0]);
        close(p[1]);
        close(death_pipe[0]);
        close(death_pipe[1]);
        free(cmd);
        opal_argv_free(argv);
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }

    if (orte_process_info.hnp_pid == 0) {
        close(p[0]);
        close(death_pipe[1]);
        /* I am the child - exec me */

        /* Set signal handlers back to the default.  Do this close
           to the execve() because the event library may (and likely
           will) reset them.  If we don't do this, the event
           library may have left some set that, at least on some
           OS's, don't get reset via fork() or exec().  Hence, the
           orted could be unkillable (for example). */
        set_handler_default(SIGTERM);
        set_handler_default(SIGINT);
        set_handler_default(SIGHUP);
        set_handler_default(SIGPIPE);
        set_handler_default(SIGCHLD);

        /* Unblock all signals, for many of the same reasons that
           we set the default handlers, above.  This is noticable
           on Linux where the event library blocks SIGTERM, but we
           don't want that blocked by the orted (or, more
           specifically, we don't want it to be blocked by the
           orted and then inherited by the ORTE processes that it
           forks, making them unkillable by SIGTERM). */
        sigprocmask(0, 0, &sigs);
        sigprocmask(SIG_UNBLOCK, &sigs, 0);

        execv(cmd, argv);

        /* if I get here, the execv failed! */
        orte_show_help("help-ess-base.txt", "ess-base:execv-error",
                       true, cmd, strerror(errno));
        exit(1);

    } else {
        free(cmd);
        /* I am the parent - wait to hear something back and
         * report results
         */
        close(p[1]);  /* parent closes the write - orted will write its contact info to it*/
        close(death_pipe[0]);  /* parent closes the death_pipe's read */
        opal_argv_free(argv);

        /* setup the buffer to read the HNP's uri */
        buffer_length = ORTE_URI_MSG_LGTH;
        chunk = ORTE_URI_MSG_LGTH-1;
        num_chars_read = 0;
        orted_uri = (char*)malloc(buffer_length);
        memset(orted_uri, 0, buffer_length);

        while (chunk == (rc = read(p[0], &orted_uri[num_chars_read], chunk))) {
            /* we read an entire buffer - better get more */
            num_chars_read += chunk;
            orted_uri = realloc((void*)orted_uri, buffer_length+ORTE_URI_MSG_LGTH);
            memset(&orted_uri[buffer_length], 0, ORTE_URI_MSG_LGTH);
            buffer_length += ORTE_URI_MSG_LGTH;
        }
        num_chars_read += rc;

        if (num_chars_read <= 0) {
            /* we didn't get anything back - this is bad */
            ORTE_ERROR_LOG(ORTE_ERR_HNP_COULD_NOT_START);
            free(orted_uri);
            return ORTE_ERR_HNP_COULD_NOT_START;
        }

        /* parse the sysinfo from the returned info - must
         * start from the end of the string as the uri itself
         * can contain brackets */
        if (NULL == (param = strrchr(orted_uri, '['))) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            free(orted_uri);
            return ORTE_ERR_COMM_FAILURE;
        }
        *param = '\0'; /* terminate the uri string */
        ++param;  /* point to the start of the sysinfo */

        /* find the end of the sysinfo */
        if (NULL == (cptr = strchr(param, ']'))) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            free(orted_uri);
            return ORTE_ERR_COMM_FAILURE;
        }
        *cptr = '\0';  /* terminate the sysinfo string */
        ++cptr;  /* point to the start of the pmix uri */

        /* convert the sysinfo string */
        if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_sysinfo(&orte_local_cpu_type,
                                      &orte_local_cpu_model, param))) {
            ORTE_ERROR_LOG(rc);
            free(orted_uri);
            return rc;
        }

        /* save the daemon uri - we will process it later */
        orte_process_info.my_daemon_uri = strdup(orted_uri);
        /* likewise, since this is also the HNP, set that uri too */
        orte_process_info.my_hnp_uri = orted_uri;

        /* split the pmix_uri into its parts */
        argv = opal_argv_split(cptr, ',');
        if (4 != opal_argv_count(argv)) {
            opal_argv_free(argv);
            return ORTE_ERR_BAD_PARAM;
        }
        /* push each piece into the environment */
        for (i=0; i < 4; i++) {
            pmixenvars[i] = strdup(argv[i]);
            putenv(pmixenvars[i]);
        }
        opal_argv_free(argv);
        added_pmix_envs = true;

        /* all done - report success */
        return ORTE_SUCCESS;
    }
}
