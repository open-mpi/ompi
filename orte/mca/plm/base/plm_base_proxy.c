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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
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
#include "orte/constants.h"

#include "opal/dss/dss.h"
#include "opal/util/path.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/util/argv.h"

#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/orted/pmix/pmix_server.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"

int orte_plm_proxy_init(void)
{
    return ORTE_SUCCESS;
}

typedef struct {
    opal_object_t super;
    orte_jobid_t jobid;
    int32_t rc;
    bool active;
} orte_proxy_spawn_t;
static void proxy_const(orte_proxy_spawn_t *p)
{
    p->jobid = ORTE_JOBID_INVALID;
    p->rc = ORTE_ERROR;
    p->active = false;
}
OBJ_CLASS_INSTANCE(orte_proxy_spawn_t, opal_object_t, proxy_const, NULL);

static void proxy_spawn_response(int status, orte_process_name_t* sender,
                                 opal_buffer_t* buffer, orte_rml_tag_t tag,
                                 void* cbdata)
{
    int rc;
    orte_std_cntr_t count;
    orte_proxy_spawn_t *ps = (orte_proxy_spawn_t*)cbdata;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:proxy recvd spawn response",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get the returned status code for the launch request */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ps->rc, &count, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
        ps->rc = rc;
        goto done;
    }

    /* get the new jobid back in case the caller wants it */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ps->jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        ps->rc = rc;
    }

 done:
    /* release the waiting call */
    ps->active = false;
}

int orte_plm_proxy_spawn(orte_job_t *jdata)
{
    opal_buffer_t *buf;
    orte_plm_cmd_flag_t command;
    int rc, i;
    orte_proxy_spawn_t *ps;
    orte_app_context_t *app;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:proxy spawn child job",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* if we are a singleton and the supporting HNP hasn't
     * been spawned, then do so now
     */
    if ((orte_process_info.proc_type & ORTE_PROC_SINGLETON) &&
        !orte_routing_is_enabled) {
        OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                             "%s plm:base:proxy spawn HNP for support",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        if (ORTE_SUCCESS != orte_plm_base_fork_hnp()) {
            ORTE_ERROR_LOG(ORTE_ERR_FATAL);
            return ORTE_ERR_FATAL;
        }
        orte_routing_is_enabled = true;
        /* need to init_routes again to redirect messages
         * thru the HNP
         */
        orte_routed.init_routes(ORTE_PROC_MY_NAME->jobid, NULL);
    }

    /* setup the buffer */
    buf = OBJ_NEW(opal_buffer_t);
    
    /* tell the recipient we are sending a launch request */
    command = ORTE_PLM_LAUNCH_JOB_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_PLM_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }

    /* ensure that our prefix is passed along so that any launching daemons
     * use the correct path */
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL != (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            if (!orte_get_attribute(&app->attributes, ORTE_APP_PREFIX_DIR, NULL, OPAL_STRING)) {
                orte_set_attribute(&app->attributes, ORTE_APP_PREFIX_DIR, ORTE_ATTR_GLOBAL,
                                   opal_install_dirs.prefix, OPAL_STRING);
            }
        }
    }

    /* pack the jdata object */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &jdata, 1, ORTE_JOB))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
        
    }
    
    /* create the proxy spawn object */
    ps = OBJ_NEW(orte_proxy_spawn_t);
    /* post the recv the HNP's response */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_PLM_PROXY,
                            ORTE_RML_NON_PERSISTENT,
                            proxy_spawn_response,
                            ps);
    
    /* tell the HNP to launch the job */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf,
                                          ORTE_RML_TAG_PLM,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        OBJ_RELEASE(ps);
        goto CLEANUP;
    }
    
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_base_framework.framework_output,
                         "%s plm:base:proxy waiting for response",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    ps->active = true;
    ORTE_WAIT_FOR_COMPLETION(ps->active);

    /* return the values */
    jdata->jobid = ps->jobid;
    rc = ps->rc;
    /* cleanup the memory */
    OBJ_RELEASE(ps);

 CLEANUP:    
    return rc;
}

int orte_plm_proxy_finalize(void)
{
    return ORTE_SUCCESS;
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

int orte_plm_base_fork_hnp(void)
{
    int p[2], death_pipe[2];
    char *cmd;
    char **argv = NULL;
    int argc;
    char *param, *cptr, *pmix_uri;
    sigset_t sigs;
    int buffer_length, num_chars_read, chunk;
    char *orted_uri;
    int rc;
    orte_jobid_t jobid;

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

    /* pass it a jobid to match my job family */
    opal_argv_append(&argc, &argv, "-"OPAL_MCA_CMD_LINE_ID);
    opal_argv_append(&argc, &argv, "ess_base_jobid");
    jobid = ORTE_DAEMON_JOBID(ORTE_PROC_MY_NAME->jobid);
    if (ORTE_SUCCESS != (rc = orte_util_convert_jobid_to_string(&param, jobid))) {
        ORTE_ERROR_LOG(rc);
        free(cmd);
        return rc;
    }
    opal_argv_append(&argc, &argv, param);
    free(param);

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

        while (chunk == (rc = read(p[0], &orted_uri[num_chars_read], chunk))) {
            /* we read an entire buffer - better get more */
            num_chars_read += chunk;
            buffer_length += ORTE_URI_MSG_LGTH;
            orted_uri = realloc((void*)orted_uri, buffer_length);
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
        /* Set the contact info in the RML - this won't actually establish
         * the connection, but just tells the RML how to reach the daemon
         * if/when we attempt to send to it
         */
        orte_rml.set_contact_info(orte_process_info.my_daemon_uri);
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_daemon_uri,
                                                           ORTE_PROC_MY_DAEMON, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(orted_uri);
            return rc;
        }

        /* likewise, since this is also the HNP, set that uri too */
        orte_process_info.my_hnp_uri = orted_uri;
        orte_rml.set_contact_info(orte_process_info.my_hnp_uri);
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orte_process_info.my_hnp_uri,
                                                           ORTE_PROC_MY_HNP, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* push the pmix_uri into our environment - need to protect it */
        (void)asprintf(&pmix_uri, "PMIX_SERVER_URI=%s", cptr);
        putenv(pmix_uri);
        /* now re-init the pmix framework so we can connect when required */
        if (OPAL_SUCCESS != (rc = opal_pmix.init())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* now call fence to push our own modex data into the
         * newly-launched HNP in case someone else needs it */
        if (OPAL_SUCCESS != (rc = opal_pmix.fence(NULL, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* all done - report success */
        return ORTE_SUCCESS;
    }
}
