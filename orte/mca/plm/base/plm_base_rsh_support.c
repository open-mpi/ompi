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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <signal.h>
#include <ctype.h>

#include "opal/dss/dss.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"
#include "orte/util/dash_host/dash_host.h"

#include "orte/mca/plm/base/plm_private.h"

static char **search(const char* agent_list);

int orte_plm_base_rsh_launch_agent_setup(void)
{
    char *bname;
    int i;
    
    /* if no agent was provided, then report not found */
    if (NULL == orte_rsh_agent) {
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* Take the orte_rsh_agent MCA param and search for the argv */
    orte_plm_globals.rsh_agent_argv = search(orte_rsh_agent);
    
    if (0 == opal_argv_count(orte_plm_globals.rsh_agent_argv)) {
        /* nothing was found */
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* see if we can find the agent in the path */
    orte_plm_globals.rsh_agent_path = 
    opal_path_findv(orte_plm_globals.rsh_agent_argv[0], X_OK,
                    environ, NULL);

    if (NULL == orte_plm_globals.rsh_agent_path) {
        /* not an error - just report not found */
        opal_argv_free(orte_plm_globals.rsh_agent_argv);
        return ORTE_ERR_NOT_FOUND;
    }
    
    bname = opal_basename(orte_plm_globals.rsh_agent_argv[0]);
    if (NULL != bname && 0 == strcmp(bname, "ssh")) {
        /* if xterm option was given, add '-X', ensuring we don't do it twice */
        if (NULL != orte_xterm) {
            opal_argv_append_unique_nosize(&orte_plm_globals.rsh_agent_argv, "-X");
        } else if (0 >= opal_output_get_verbosity(orte_plm_globals.output)) {
            /* if debug was not specified, and the user didn't explicitly
             * specify X11 forwarding/non-forwarding, add "-x" if it
             * isn't already there (check either case)
             */
            for (i = 1; NULL != orte_plm_globals.rsh_agent_argv[i]; ++i) {
                if (0 == strcasecmp("-x", 
                                    orte_plm_globals.rsh_agent_argv[i])) {
                    break;
                }
            }
            if (NULL == orte_plm_globals.rsh_agent_argv[i]) {
                opal_argv_append_nosize(&orte_plm_globals.rsh_agent_argv, "-x");
            }
        }
    }
    
    return ORTE_SUCCESS;
}


static bool ack_recvd;

static void release_ack(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    ack_recvd = true;
    OBJ_RELEASE(mev);
}

static void recv_ack(int status, orte_process_name_t* sender,
                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                     void* cbdata)
{
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, release_ack);    
}

static void set_handler_default(int sig)
{
    struct sigaction act;
    
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    
    sigaction(sig, &act, (struct sigaction *)0);
}

int orte_plm_base_local_slave_launch(orte_job_t *jdata)
{
    char **argv;
    opal_list_t hosts;
    orte_node_t *node;
    char *nodename, *bootproxy, *cmd, *scp=NULL;
    char *exefile=NULL, *basename, *param, *path=NULL, *bppath=NULL;
    char *exec_path=NULL;
    char *tmp;
    bool flag;
    orte_app_context_t **apps, *app;
    int i;
    int rc;
    pid_t pid;
    long fd, fdmax = sysconf(_SC_OPEN_MAX);
    sigset_t sigs;
    bool local_op = false;
    
    /* increment the local slave jobid */
    orte_plm_globals.local_slaves++;
    
    /* point to the apps array */
    apps = (orte_app_context_t**)jdata->apps->addr;
    app = apps[0];
    
    /* identify the target host - can only be one! */
    OBJ_CONSTRUCT(&hosts, opal_list_t);
    if (ORTE_SUCCESS != (rc = orte_util_add_dash_host_nodes(&hosts, &flag, app->dash_host))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&hosts);
        return rc;
    }
    if (1 < opal_list_get_size(&hosts)) {
        opal_output(0, "too many hosts: %d", (int)opal_list_get_size(&hosts));
        return ORTE_ERROR;
    }
    node = (orte_node_t*)opal_list_remove_first(&hosts);
    nodename = strdup(node->name);
    OBJ_RELEASE(node);
    OBJ_DESTRUCT(&hosts);
    
    /* is this a local operation? */
    opal_output(0, "local: %s node: %s", orte_process_info.nodename, nodename);
    if (0 == strcmp(orte_process_info.nodename, nodename)) {
        local_op = true;
    }
    
    /* find the bootproxy */
    bootproxy = opal_find_absolute_path("orte-bootproxy.sh");
    
    /* do we need to preload the binary? */
    if(app->preload_binary) {
        /* the target location -must- be an absolute path */
        if (NULL == app->preload_files_dest_dir ||
            !opal_path_is_absolute(app->preload_files_dest_dir)) {
            opal_output(0, "target location must be given and an absolute path: %s",
                        (NULL == app->preload_files_dest_dir) ? "NULL" : app->preload_files_dest_dir);
            return ORTE_ERROR;
        }
        /* if the binary is not given in absolute path form,
         * then convert it to one
         */
        if (!opal_path_is_absolute(app->app)) {
            exefile = opal_find_absolute_path(app->app);
            if (NULL == exefile) {
                opal_output(0, "could not find executable %s", app->app);
                return ORTE_ERROR;
            }
        } else {
            exefile = strdup(app->app);
        }
        /* construct the target path */
        basename = opal_basename(exefile);
        path = opal_os_path(false, app->preload_files_dest_dir, basename, NULL);
        free(basename);
        /* we are going to use the "bootproxy" script to launch
         * this job - so move it over to the target host as well
         */
        bppath = opal_os_path(false, app->preload_files_dest_dir, "orte-bootproxy.sh", NULL);
        /* if this is a local node, then we just use the cp command */
        if (local_op) {
            scp = opal_find_absolute_path("cp");
            if (NULL == scp) {
                opal_output(0, "could not find cp");
                return ORTE_ERROR;
            }
            /* form and execute the cp commands */
            asprintf(&cmd, "%s %s %s", scp, exefile, path);
            system(cmd);
            free(cmd);
            asprintf(&cmd, "%s %s %s", scp, bootproxy, bppath);
            system(cmd);
            free(cmd);
            /* start the argv with the bootproxy cmd */
            argv = NULL;
            opal_argv_append_nosize(&argv, "orte-bootproxy.sh");
            /* set the exec path to bppath */
            exec_path = strdup(bppath);
        } else {
            /* find the scp command */
            scp = opal_find_absolute_path("scp");
            if (NULL == scp) {
                opal_output(0, "could not find scp");
                return ORTE_ERROR;
            }
            /* form and execute the scp commands */
            asprintf(&cmd, "%s %s %s:%s", scp, exefile, nodename, path);
            system(cmd);
            free(cmd);
            asprintf(&cmd, "%s %s %s:%s", scp, bootproxy, nodename, bppath);
            system(cmd);
            free(cmd);
            /* set the exec path to the agent path */
            exec_path = strdup(orte_plm_globals.rsh_agent_path);
            /* Start the argv with the rsh/ssh command */
            argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
            /* add the hostname */
            opal_argv_append_nosize(&argv, nodename);
            /* add the bootproxy cmd */
            opal_argv_append_nosize(&argv, bootproxy);
        }
    }
    if (NULL != exefile) {
        free(exefile);
    }
    if (NULL != path) {
        free(path);
    }
    if (NULL != bppath) {
        free(bppath);
    }
    /* release the scp command */
    if (NULL != scp) {
        free(scp);
    }
    
    /* done with bootproxy */
    free(bootproxy);
    
    /* if there is a prefix, add it in a special way so the bootproxy
     * can deal with it
     */
    if (NULL != app->prefix_dir) {
        asprintf(&param, "OMPI_PREFIX=%s", app->prefix_dir);
        opal_argv_append_nosize(&argv, param);
        free(param);
    }
    
    /* add all OMPI params from the app */
    if (NULL != app->env) {
        for (i=0; NULL != app->env[i]; i++) {
            if (0 == strncmp(app->env[i], "OMPI_", 5)) {
                if (NULL == strchr(app->env[i], ';') &&
                    NULL == strchr(app->env[i], ':')) {
                    opal_argv_append_nosize(&argv, app->env[i]);
                } else {
                    tmp = strchr(app->env[i], '=');
                    *tmp = '\0';
                    tmp++;
                    asprintf(&param, "%s=\"%s\"", app->env[i], tmp);
                    opal_argv_append_nosize(&argv, param);
                    free(param);
                }
            }
        }
    }
    
    /* add MCA params required for launch */
    
    /* tell ESS to select the "slave" component */
    param = mca_base_param_environ_variable("ess",NULL,NULL);
    opal_setenv(param, "slave", true, &argv);
    free(param);
    
    /* tell ROUTED to select the "slave" component */
    param = mca_base_param_environ_variable("routed",NULL,NULL);
    opal_setenv(param, "slave", true, &argv);
    free(param);
    
    /* tell GRPCOMM to select the "hier" component */
    param = mca_base_param_environ_variable("grpcomm",NULL,NULL);
    opal_setenv(param, "hier", true, &argv);
    free(param);
    
    /* must tell "hier" two pieces of info */
    param = mca_base_param_environ_variable("grpcomm","hier","num_nodes");
    opal_setenv(param, "1", true, &argv);
    free(param);
    param = mca_base_param_environ_variable("grpcomm","hier","step");
    opal_setenv(param, "1", true, &argv);
    free(param);
    
    /* set the daemon uri to point to me */
    param = mca_base_param_environ_variable("orte","local_daemon","uri");
    asprintf(&path, "\"%s\"", orte_rml.get_contact_info());
    opal_setenv(param, path, true, &argv);
    free(param);
    free(path);
    
    /* set a value for the HNP uri - it won't be needed, but is
     * required to pass existence tests
     */
    param = mca_base_param_environ_variable("orte","hnp","uri");
    asprintf(&path, "\"%s\"", orte_process_info.my_hnp_uri);
    opal_setenv(param, path, true, &argv);
    free(param);
    free(path);
    
    /* setup yield schedule to be aggressive */
    param = mca_base_param_environ_variable("mpi", NULL, "yield_when_idle");
    opal_setenv(param, "0", true, &argv);
    free(param);
    
    /* set the app_context number */
    param = mca_base_param_environ_variable("orte","app","num");
    opal_setenv(param, "1", true, &argv);
    free(param);
    
    /* ensure that any "name" envar is cleared */
    param = mca_base_param_environ_variable("orte","ess","name");
    opal_unsetenv(param, &argv);
    free(param);
    
    /* set the jobid */
    orte_util_convert_jobid_to_string(&cmd, orte_plm_globals.local_slaves);
    param = mca_base_param_environ_variable("orte","ess","jobid");
    opal_setenv(param, cmd, true, &argv);
    free(param);
    free(cmd);
    /* set the jobid in jdata so the caller knows what it is */
    jdata->jobid = orte_plm_globals.local_slaves;
    
    /* set the vpid to 0 */
    param = mca_base_param_environ_variable("orte","ess","vpid");
    opal_setenv(param, "0", true, &argv);
    free(param);
    
    /* set the number of procs */
    param = mca_base_param_environ_variable("orte","ess","num_procs");
    opal_setenv(param, "1", true, &argv);
    free(param);
    
    /* some user-requested public environmental variables */
    opal_setenv("OMPI_COMM_WORLD_RANK", "0", true, &argv);
    opal_setenv("OMPI_COMM_WORLD_LOCAL_RANK", "0", true, &argv);
    opal_setenv("OMPI_UNIVERSE_SIZE", "1", true, &argv);
    opal_setenv("OMPI_COMM_WORLD_SIZE", "1", true, &argv);
    opal_setenv("OMPI_COMM_WORLD_LOCAL_SIZE", "1", true, &argv);
    
    /* add the provided argv*/
    for (i=0; NULL != app->argv[i]; i++) {
        opal_argv_append_nosize(&argv, app->argv[i]);
    }
    
    param = opal_argv_join(argv, ' ');
    opal_output(0, "%s plm:rsh: final bootproxy cmd:\n\t%s",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                (NULL == param) ? "NULL" : param);
    if (NULL != param) free(param);
    
    /* fork a child to exec the rsh/ssh session */
    pid = fork();
    if (pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }
    
    /* child */
    if (pid == 0) {
        /* close all file descriptors w/ exception of stdin/stdout/stderr */
        for(fd=3; fd<fdmax; fd++)
            close(fd);
        
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
        
        /* exec the slave */
        execv(exec_path, argv);
        opal_output(0, "plm:rsh: execv of %s failed with errno=%s(%d)\n",
                    exec_path, strerror(errno), errno);
        exit(-1);    
    } else {
        /* parent waits to hear that slave is running */
        ack_recvd = false;
        rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_REPORT_REMOTE_LAUNCH,
                                     ORTE_RML_NON_PERSISTENT, recv_ack, NULL);
        
        ORTE_PROGRESSED_WAIT(ack_recvd, 0, 1);
        /* cleanup */
        free(exec_path);
        opal_argv_free(argv);
    }
    
    return ORTE_SUCCESS;
}

/*
 * Take a colon-delimited list of agents and locate the first one that
 * we are able to find in the PATH.  Split that one into argv and
 * return it.  If nothing found, then return NULL.
 */
static char **search(const char* agent_list)
{
    int i, j;
    char *line, **lines = opal_argv_split(agent_list, ':');
    char **tokens, *tmp;
    char cwd[OMPI_PATH_MAX];
    
    getcwd(cwd, OMPI_PATH_MAX);
    for (i = 0; NULL != lines[i]; ++i) {
        line = lines[i];
        
        /* Trim whitespace at the beginning and end of the line */
        for (j = 0; '\0' != line[j] && isspace(line[j]); ++line) {
            continue;
        }
        for (j = strlen(line) - 2; j > 0 && isspace(line[j]); ++j) {
            line[j] = '\0';
        }
        if (strlen(line) <= 0) {
            continue;
        }
        
        /* Split it */
        tokens = opal_argv_split(line, ' ');
        
        /* Look for the first token in the PATH */
        tmp = opal_path_findv(tokens[0], X_OK, environ, cwd);
        if (NULL != tmp) {
            free(tokens[0]);
            tokens[0] = tmp;
            opal_argv_free(lines);
            return tokens;
        }
        
        /* Didn't find it */
        opal_argv_free(tokens);
    }
    
    /* Doh -- didn't find anything */
    opal_argv_free(lines);
    return NULL;
}
