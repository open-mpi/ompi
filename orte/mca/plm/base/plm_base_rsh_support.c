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
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#if HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <errno.h>
#include <signal.h>
#include <ctype.h>

#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/path.h"
#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
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
            opal_argv_append_unique_nosize(&orte_plm_globals.rsh_agent_argv, "-X", false);
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

/****    SLAVE LAUNCH SUPPORT    ****/

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
    char *exefile=NULL, *basename, *param, *path=NULL;
    char *exec_path=NULL;
    char *tmp, *src, *dest, *dest_dir, *filenm;
    char **files;
    bool flag;
    orte_app_context_t **apps, *app;
    int i, j;
    int rc;
    pid_t pid;
    long fd, fdmax = sysconf(_SC_OPEN_MAX);
    sigset_t sigs;
    char cwd[OPAL_PATH_MAX];
    opal_list_item_t *item;
    orte_slave_files_t *slave_node, *tst_node;
    
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
        orte_show_help("help-plm-base.txt", "too-many-hosts", true, (int)opal_list_get_size(&hosts));
        return ORTE_ERROR;
    }
    node = (orte_node_t*)opal_list_remove_first(&hosts);
    nodename = strdup(node->name);
    OBJ_RELEASE(node);
    OBJ_DESTRUCT(&hosts);
    
    /* have we launched anything on this node before? */
    slave_node = NULL;
    for (item = opal_list_get_first(&orte_plm_globals.slave_files);
         item != opal_list_get_end(&orte_plm_globals.slave_files);
         item = opal_list_get_next(item)) {
        tst_node = (orte_slave_files_t*)item;
        if (0 == strcmp(tst_node->node, nodename)) {
            slave_node = tst_node;
            break;
        }
    }
    if (NULL == slave_node) {
        slave_node = OBJ_NEW(orte_slave_files_t);
        slave_node->node = strdup(nodename);
        opal_list_append(&orte_plm_globals.slave_files, &slave_node->super);
    }
    
    /* is this a local operation? */
    if (0 == strcmp(orte_process_info.nodename, nodename)) {
        slave_node->local = true;
    }
    
    /* if we are going to position the binary or files, did they give us a dest? */
    if (NULL != app->preload_files_dest_dir) {
        /* the target location -must- be an absolute path */
        if (!opal_path_is_absolute(app->preload_files_dest_dir)) {
            orte_show_help("help-plm-base.txt", "abs-path-reqd", true, app->preload_files_dest_dir);
            return ORTE_ERROR;
        }
        dest_dir = app->preload_files_dest_dir;
        /* if this is a local op, make sure this location exists. we can't
         * do this for remote ops as there is no way to create a remote
         * directory
         */
        if (slave_node->local) {
            if (ORTE_SUCCESS != (rc = opal_os_dirpath_create(dest_dir, S_IRWXU))) {
                orte_show_help("help-plm-base.txt", "path-not-created", true, dest_dir);
                return rc;
            }
        }
    } else {
        /* put everything in /tmp */
        dest_dir = "/tmp";
    }
    
    /* have we preloaded the bootproxy on this node? */
    if (NULL == slave_node->bootproxy) {
        /* find the local bootproxy */
        bootproxy = opal_find_absolute_path("orte-bootproxy.sh");
        if (NULL == bootproxy) {
            orte_show_help("help-plm-base.txt", "bootproxy-not-found", true);
            return ORTE_ERR_NOT_FOUND;
        }
        if (slave_node->local) {
            /* if this is a local operation, then just set
             * the exec_path to be the bootproxy
             */
            argv = NULL;
            opal_argv_append_nosize(&argv, bootproxy);
            exec_path = strdup(argv[0]);
            slave_node->bootproxy = strdup(exec_path);
            /* don't remove upon completion */
            slave_node->positioned = false;
        } else {
            /* set the exec path to the rsh agent path */
            exec_path = strdup(orte_plm_globals.rsh_agent_path);
            /* Start the argv with the rsh/ssh command */
            argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
            /* add the hostname */
            opal_argv_append_nosize(&argv, nodename);
            /* add the bootproxy cmd */
            if (NULL != app->prefix_dir) {
                /* the caller gave us a prefix directory, indicating
                 * that OMPI has been installed on the remote node, so
                 * just use that prefix
                 */
                asprintf(&cmd, "%s/bin/%s", app->prefix_dir, "orte-bootproxy.sh");
                opal_argv_append_nosize(&argv, cmd);
                slave_node->bootproxy = strdup(cmd);
                free(cmd);
                /* don't remove upon completion */
                slave_node->positioned = false;
            } else {
                /* since there is no prefix installation, we need to
                 * position the bootproxy on the remote node.
                 * even though we may not be preloading binaries, the
                 * caller can use this to specify where to put the
                 * bootproxy itself
                 */
                path = opal_os_path(false, dest_dir, "orte-bootproxy.sh", NULL);
                /* find the scp command */
                scp = opal_find_absolute_path("scp");
                if (NULL == scp) {
                    orte_show_help("help-plm-base.txt", "cp-not-found", true, "scp", "scp");
                    return ORTE_ERROR;
                }
                /* form and execute the scp command */
                asprintf(&cmd, "%s %s %s:%s", scp, bootproxy, nodename, path);
                system(cmd);
                free(cmd);
                /* setup the remote executable cmd */
                opal_argv_append_nosize(&argv, path);
                slave_node->bootproxy = strdup(path);
                free(path);
                /* remove upon completion */
                slave_node->positioned = true;
            }
        }
        free(bootproxy);
    } else {
        /* the bootproxy has been positioned - setup to use it */
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:local:slave: bootproxy %s already positioned",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), slave_node->bootproxy));
        if (slave_node->local) {
            /* if this is a local operation, then just set
             * the exec_path to be the bootproxy
             */
            argv = NULL;
            opal_argv_append_nosize(&argv, slave_node->bootproxy);
            exec_path = strdup(argv[0]);
        } else {
            /* set the exec path to the rsh agent path */
            exec_path = strdup(orte_plm_globals.rsh_agent_path);
            /* Start the argv with the rsh/ssh command */
            argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
            /* add the hostname */
            opal_argv_append_nosize(&argv, nodename);
            /* add the bootproxy cmd */
            opal_argv_append_nosize(&argv, slave_node->bootproxy);
        }
    }
        
    
    /* do we need to preload the binary? */
    if (app->preload_binary) {
       /* if the binary is not given in absolute path form,
         * then convert it to one
         */
        if (!opal_path_is_absolute(app->app)) {
            /* see if a source directory was given */
            if (NULL!= app->preload_files_src_dir) {
                /* prepend the src dir to the executable name */
                path = opal_os_path(false, app->preload_files_src_dir, app->app, NULL);
                /* now check for the existence of the app */
                src = opal_find_absolute_path(path);
                if (NULL == src) {
                    orte_show_help("help-plm-base.txt", "exec-not-found", true, path);
                    return ORTE_ERROR;
                }
            } else {
                /* look for it in the cwd */
                getcwd(cwd, OPAL_PATH_MAX);
                src = opal_path_access(app->app, cwd, X_OK);
                if (NULL == src) {
                    orte_show_help("help-plm-base.txt", "exec-not-found", true, cwd);
                    return ORTE_ERROR;
                }
            }
        } else {
            src = opal_path_access(app->app, NULL, X_OK);
            if (NULL == src) {
                orte_show_help("help-plm-base.txt", "exec-not-found", true, app->app);
                return ORTE_ERROR;
            }
        }
        /* get the basename */
        basename = opal_basename(app->app);
        
        /* define the destination */
        dest = opal_os_path(false, dest_dir, basename, NULL);

        /* has this binary already been positioned? */
        for (i=0; i < slave_node->apps.size; i++) {
            if (NULL != (filenm = opal_pointer_array_get_item(&slave_node->apps, i)) &&
                0 == strcmp(filenm, dest)) {
                /* this app already has been positioned on the node - skip it */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:local:slave: app %s already positioned",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), filenm));
                goto PRELOAD_FILES;
            }
        }
        /* add the app to the slave_node list */
        opal_pointer_array_add(&slave_node->apps, strdup(dest));
        /* since we are positioning the binary, add it to the list
         * of files to be cleaned up when done
         */
        opal_pointer_array_add(&slave_node->files, strdup(dest));

        /* if this is a local node, then we just use the cp command */
        if (slave_node->local) {
            scp = opal_find_absolute_path("cp");
            if (NULL == scp) {
                orte_show_help("help-plm-base.txt", "cp-not-found", true, "cp", "cp");
                return ORTE_ERROR;
            }
            /* form and execute the cp commands */
            asprintf(&cmd, "%s %s %s", scp, src, dest);
            system(cmd);
            free(cmd);
        } else {
            /* find the scp command */
            scp = opal_find_absolute_path("scp");
            if (NULL == scp) {
                orte_show_help("help-plm-base.txt", "cp-not-found", true, "scp", "scp");
                return ORTE_ERROR;
            }
            /* form and execute the scp commands */
            asprintf(&cmd, "%s %s %s:%s", scp, src, nodename, dest);
            system(cmd);
            free(cmd);
        }
        free(src);
        free(dest);
        free(scp);
    } else {
        /* we don't need to pre-position the binary, but we do need
         * to check if we should record it
         */
        for (i=0; i < slave_node->apps.size; i++) {
            if (NULL != (filenm = opal_pointer_array_get_item(&slave_node->apps, i)) &&
                0 == strcmp(filenm, app->app)) {
                /* this app already has been positioned on the node - skip it */
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:local:slave: app %s already positioned",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), filenm));
                goto PRELOAD_FILES;
            }
        }
        /* add the app to the slave_node list */
        opal_pointer_array_add(&slave_node->apps, strdup(app->app));
        /* do not add it to the files to be cleaned up when done as
         * we are not positioning it!
         */
    }
    
PRELOAD_FILES:
    /* do we need to pre-position supporting files? */
    if (NULL != app->preload_files) {
        if (slave_node->local) {
            scp = opal_find_absolute_path("cp");
            if (NULL == scp) {
                orte_show_help("help-plm-base.txt", "cp-not-found", true, "cp", "cp");
                return ORTE_ERROR;
            }
        } else {
            /* find the scp command */
            scp = opal_find_absolute_path("scp");
            if (NULL == scp) {
                orte_show_help("help-plm-base.txt", "cp-not-found", true, "scp", "scp");
                return ORTE_ERROR;
            }
        }
        /* break apart the comma-separated list of files */
        files = opal_argv_split(app->preload_files, ',');
        /* copy each file across */
        for (i=0; i < opal_argv_count(files); i++) {
            /* if the file is not given in absolute path form,
             * then convert it to one
             */
            if (!opal_path_is_absolute(files[i])) {
                /* see if a source directory was given */
                if (NULL!= app->preload_files_src_dir) {
                    /* look for the file there */
                    exefile = opal_path_access(files[i], app->preload_files_src_dir, R_OK);
                } else {
                    /* look for it in the cwd */
                    getcwd(cwd, OPAL_PATH_MAX);
                    exefile = opal_path_access(files[i], cwd, R_OK);
                }
            } else {
                exefile = opal_path_access(files[i], NULL, R_OK);
            }
            if (NULL == exefile) {
                getcwd(cwd, OPAL_PATH_MAX);
                orte_show_help("help-plm-base.txt", "file-not-found", true, files[i],
                               (NULL == app->preload_files_src_dir) ? cwd : app->preload_files_src_dir);
                return ORTE_ERROR;
            }
            /* define the destination */
            dest = opal_os_path(false, dest_dir, files[i], NULL);
            /* has this file already been positioned? */
            for (j=0; j < slave_node->files.size; j++) {
                if (NULL != (filenm = opal_pointer_array_get_item(&slave_node->files, j)) &&
                    0 == strcmp(filenm, dest)) {
                    /* this app already has been positioned on the node - skip it */
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s plm:base:local:slave: file %s already positioned",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), filenm));
                    goto SKIP;
                }
            }
            /* add the file to the slave_node list */
            opal_pointer_array_add(&slave_node->files, strdup(dest));
            if (slave_node->local) {
                /* form and execute the cp command */
                asprintf(&cmd, "%s %s %s", scp, exefile, dest);
                system(cmd);
                free(cmd);
            } else {
                /* form and execute the scp commands */
                asprintf(&cmd, "%s -q %s %s:%s", scp, exefile, nodename, dest);
                system(cmd);
                free(cmd);
            }
        SKIP:
            free(exefile);
            free(dest);
        }
        opal_argv_free(files);
        free(scp);
    }
    
    /* done with nodename */
    free(nodename);

    /* if there is a prefix, add it in a special way so the bootproxy
     * can deal with it
     */
    if (NULL != app->prefix_dir) {
        asprintf(&param, "OMPI_PREFIX=%s", app->prefix_dir);
        opal_argv_append_nosize(&argv, param);
        free(param);
    }
    
    /* if there is a working directory specified, add it in a special
     * way so the bootproxy can deal with it
     */
    if (NULL != app->cwd) {
        asprintf(&param, "OMPI_WDIR=%s", app->cwd);
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
    
    /* must pass the number of nodes */
    param = mca_base_param_environ_variable("orte","num","nodes");
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
    
    if (app->preload_binary) {
        /* construct the target path */
        basename = opal_basename(app->app);
        path = opal_os_path(false, dest_dir, basename, NULL);
        free(basename);
        /* add this to the cmd */
        opal_argv_append_nosize(&argv, path);
        free(path);
    } else {
        /* it must already have been put there, so use the given path */
        opal_argv_append_nosize(&argv, app->app);
    }

    /* add any provided argv */
    for (i=1; NULL != app->argv[i]; i++) {
        opal_argv_append_nosize(&argv, app->argv[i]);
    }
    
    if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
        param = opal_argv_join(argv, ' ');
        opal_output(0, "%s plm:rsh: final bootproxy cmd:\n\t%s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    (NULL == param) ? "NULL" : param);
        if (NULL != param) free(param);
    }
    
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
    char cwd[OPAL_PATH_MAX];
    
    getcwd(cwd, OPAL_PATH_MAX);
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

void orte_plm_base_local_slave_finalize(void)
{
    opal_list_item_t *item;
    orte_slave_files_t *slave_node;
    char *cmd, *filenm, **argv;
    int i;
    bool first;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:local:slave:finalize",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    while (NULL != (item = opal_list_remove_first(&orte_plm_globals.slave_files))) {
        slave_node = (orte_slave_files_t*)item;
        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                             "%s plm:base:local:slave:finalize - entry for node %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), slave_node->node));
        
        /* we will use the bootproxy cmd script to clean up for us. All we
         * have to do is tell it to run in CLEANUP mode, and then tell it
         * the APPS and FILES it needs to cleanup
         */
        
        if (slave_node->local) {
            /* setup the bootproxy cmd */
            argv = NULL;
            opal_argv_append_nosize(&argv, slave_node->bootproxy);
        } else {
            /* Start the argv with the rsh/ssh command */
            argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
            /* add the hostname */
            opal_argv_append_nosize(&argv, slave_node->node);
            /* add the bootproxy cmd */
            opal_argv_append_nosize(&argv, slave_node->bootproxy);
        }
        /* pass the CLEANUP mode */
        opal_argv_append_nosize(&argv, "CLEANUP");
        /* pass the name of the apps running on the node - the bootproxy will
         * send a TERM signal to each of them
         */
        first = true;
        for (i=0; i < slave_node->apps.size; i++) {
            if (NULL == (filenm = opal_pointer_array_get_item(&slave_node->apps, i))) {
                continue;
            }
            if (first) {
                opal_argv_append_nosize(&argv, "APPS");
                first = false;
            }
            opal_argv_append_nosize(&argv, filenm);
        }
        /* remove any files we positioned */
        first = true;
        for (i=0; i < slave_node->files.size; i++) {
            if (NULL == (filenm = opal_pointer_array_get_item(&slave_node->files, i))) {
                continue;
            }
            if (first) {
                opal_argv_append_nosize(&argv, "FILES");
                first = false;
            }
            opal_argv_append_nosize(&argv, filenm);
        }
        /* execute the cmd */
        cmd = opal_argv_join(argv, ' ');
        opal_argv_free(argv);
        argv = NULL;
        system(cmd);
        free(cmd);
        /* now remove the bootproxy itself, if needed */
        if (slave_node->positioned) {
            if (slave_node->local) {
                asprintf(&cmd, "rm -f %s", slave_node->bootproxy);
            } else {
                /* Start the argv with the rsh/ssh command */
                argv = opal_argv_copy(orte_plm_globals.rsh_agent_argv);
                /* add the hostname */
                opal_argv_append_nosize(&argv, slave_node->node);
                /* add the rm cmd */
                opal_argv_append_nosize(&argv, "rm -f");
                /* add the bootproxy file */
                opal_argv_append_nosize(&argv, slave_node->bootproxy);
                /* form the cmd */
                cmd = opal_argv_join(argv, ' ');
                opal_argv_free(argv);
                argv = NULL;
            }
            /* execute it */
            system(cmd);
            free(cmd);
        }
        OBJ_RELEASE(item);
    }
}
