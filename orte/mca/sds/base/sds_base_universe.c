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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#include <errno.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/installdirs/installdirs.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/params.h"
#include "orte/runtime/runtime.h"

#include "orte/mca/sds/base/base.h"

static int fork_hnp(void);

static void set_handler_default(int sig)
{
#if !defined(__WINDOWS__)
    struct sigaction act;
    
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    
    sigaction(sig, &act, (struct sigaction *)0);
#endif /* !defined(__WINDOWS__) */
}

int
orte_sds_base_basic_contact_universe(void)
{
    int rc=ORTE_SUCCESS, value;
    bool exit_if_not_exist;
    orte_universe_t univ;
    
    OBJ_CONSTRUCT(&univ, orte_universe_t);

    /* if we were NOT given registry and name service replicas (i.e., we
     * weren't told a universe contact point), check for some
     * existing universe to join */
    if (NULL == orte_process_info.ns_replica_uri || NULL == orte_process_info.gpr_replica_uri) {
        if (ORTE_SUCCESS == (rc = orte_universe_exists(&univ))) {
            /* copy universe info into our universe structure */
            orte_universe_info.name = strdup(univ.name);
            orte_universe_info.host = strdup(univ.host);
            orte_universe_info.uid  = strdup(univ.uid);
            orte_universe_info.persistence = univ.persistence;
            orte_universe_info.scope = strdup(univ.scope);
            /* JJH XXX This will inadvertently overwrite the console MCA param */
            /* orte_universe_info.console = univ.console; */
            orte_universe_info.seed_uri = strdup(univ.seed_uri);
            orte_universe_info.console_connected = univ.console_connected;
            if( NULL != univ.scriptfile)
                orte_universe_info.scriptfile = strdup(univ.scriptfile);
            else 
                orte_universe_info.scriptfile = NULL;
            /* define the replica contact points */
            orte_process_info.ns_replica_uri = strdup(univ.seed_uri);
            orte_process_info.gpr_replica_uri = strdup(univ.seed_uri);
        } else {
            /* if an existing universe is not detected, check the
             * relevant MCA parameter to see if the caller wants
             * us to abort in this situation
             */
            mca_base_param_reg_int_name("orte", "univ_exist",
                                        "Exit if an existing universe cannot be detected or does not allow connection",
                                        false, false, (int)false, &value);
            exit_if_not_exist = OPAL_INT_TO_BOOL(value);
            
            if (exit_if_not_exist) {
                /* tell orte_init to exit */
                rc = ORTE_ERR_UNREACH;
                goto CLEANUP;
            }
            
            /* so the user didn't tell us to abort if it wasn't found - see
             * if the user specified a universe. if so, and we couldn't find
             * it, then we don't really want to continue
             */
            if (ORTE_ERR_NOT_FOUND != rc) {
                /* user-specified name - abort */
                opal_output(0, "orte_init: could not contact the specified universe name %s",
                            orte_universe_info.name);
                goto CLEANUP;
            }
            
            /* so we could not find a universe to which we could connect, and we don't
             * want to just abort - let's see what we can do about starting one
             * of our very own! First, let's check if we are a singleton - if
             * we are infrastructure, then we are not a singleton and we will
             * simply declare ourselves to be an HNP to do the various good things
             */
            if (orte_infrastructure) {
                orte_process_info.seed = true;
                /* since we are an HNP, ensure that all replica info is NULL'd */
                if (NULL != orte_process_info.ns_replica_uri) {
                    free(orte_process_info.ns_replica_uri);
                    orte_process_info.ns_replica_uri = NULL;
                }
                if (NULL != orte_process_info.ns_replica) {
                    free(orte_process_info.ns_replica);
                    orte_process_info.ns_replica = NULL;
                }
                
                if (NULL != orte_process_info.gpr_replica_uri) {
                    free(orte_process_info.gpr_replica_uri);
                    orte_process_info.gpr_replica_uri = NULL;
                }
                if (NULL != orte_process_info.gpr_replica) {
                    free(orte_process_info.gpr_replica);
                    orte_process_info.gpr_replica = NULL;
                }
                rc = ORTE_SUCCESS;
                goto CLEANUP;
            }
            
            /* so we are not infrastructure, which means we *are* a singleton. In
             * this case, we need to start a daemon that can support our operation.
             * We must do this for two reasons:
             * (1) if we try to play the role of the HNP, then any child processes
             * we might start via comm_spawn will rely on us for all ORTE-level
             * support. However, we can only progress those requests when the
             * the application calls into the OMPI/ORTE library! Thus, if this
             * singleton just does computation, the other processes will "hang"
             * in any calls into the ORTE layer that communicate with the HNP -
             * and most calls on application procs *do*.
             *
             * (2) daemons are used to communicate messages for administrative
             * purposes in a broadcast-like manner. Thus, daemons are expected
             * to be able to interpret specific commands. Our application process
             * doesn't have any idea how to handle those commands, thus causing
             * the entire ORTE administrative system to break down.
             *
             * For those reasons, we choose to fork/exec a daemon at this time
             * and then reconnect ourselves to it. We could just "fork" and declare
             * the child to be a daemon, but that would require we place *all* of the
             * daemon command processing code in the ORTE library, do some strange
             * mojo in a few places, etc. This doesn't seem worth it, so we'll just
             * do the old fork/exec here
             *
             * Note that Windows-based systems have to do their own special trick as
             * they don't support fork/exec. So we have to use a giant "if" here to
             * protect the Windows world. To make the results more readable, we put
             * the whole mess in a separate function below
             */
            if (ORTE_SUCCESS != (rc= fork_hnp())) {
                /* if this didn't work, then we cannot support operation any further.
                 * Abort the system and tell orte_init to exit
                 */
                ORTE_ERROR_LOG(rc);
                goto CLEANUP;
            }
        }
    }

CLEANUP:
    OBJ_DESTRUCT(&univ);

    return rc;
}


int
orte_sds_base_seed_set_name(void)
{
    int rc;
    
    /* if we are a seed, then there can be only one proc */
    orte_process_info.num_procs = 1;
    orte_process_info.local_rank = 0;
    orte_process_info.num_local_procs = 1;

    /* if we're a seed and we're not infrastructure, we're also a
       singleton.  So set the singleton flag in that case */
    if (!orte_infrastructure) {
        orte_process_info.singleton = true;
    }
    /* now need to create our name in a manner that puts our job info on the name service
     * tracker. This is necessary so that
     * functions like get_job_peers will work. Since we are the seed, these
     * functions will always return the proper jobid=0, vpid=0 values
     */
    if (ORTE_SUCCESS != (rc = orte_ns.create_my_name())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}


#define ORTE_URI_MSG_LGTH   256

static int fork_hnp(void)
{
#if !defined(__WINDOWS__) && !defined(__LIBCATAMOUNT__)
    int p[2], death_pipe[2];
    char *cmd;
    char **argv = NULL;
    int argc;
    char *param;
    sigset_t sigs;
    pid_t pid;
    int buffer_length, num_chars_read, chunk;
    char *orted_uri;
    int rc;
    
    /* A pipe is used to communicate between the parent and child to
        indicate whether the exec ultiimately succeeded or failed.  The
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
    
    /* tell the daemon it is to be the seed */
    opal_argv_append(&argc, &argv, "--seed");

    /* tell the daemon to get out of our process group */
    opal_argv_append(&argc, &argv, "--set-sid");
    
    /* tell the daemon to not daemonize so we can see any output */
    opal_argv_append(&argc, &argv, "--no-daemonize");
    
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
    
    /* pass along the universe name so we match */
    opal_argv_append(&argc, &argv, "--universe");
    opal_argv_append(&argc, &argv, orte_universe_info.name);
    
    /* Fork off the child */
    pid = fork();
    if(pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        close(p[0]);
        close(p[1]);
        close(death_pipe[0]);
        close(death_pipe[1]);
        free(cmd);
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }
    
    if (pid == 0) {
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
        opal_show_help("help-sds-base.txt", "sds-base:execv-error",
                       true, cmd, strerror(errno));
        exit(1);
        
    } else {
       /* I am the parent - wait to hear something back and
         * report results
         */
        close(p[1]);  /* parent closes the write - orted will write its contact info to it*/
        close(death_pipe[0]);  /* parent closes the death_pipe's read */
        
        /* setup the buffer to read the name + uri */
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
        
        /* parse the name from the returned info */
        if (']' != orted_uri[strlen(orted_uri)-1]) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            free(orted_uri);
            return ORTE_ERR_COMM_FAILURE;
        }
        orted_uri[strlen(orted_uri)-1] = '\0';
        if (NULL == (param = strrchr(orted_uri, '['))) {
            ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
            free(orted_uri);
            return ORTE_ERR_COMM_FAILURE;
        }
        *param = '\0';  /* terminate the string */
        param++;
        if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_process_name(&orte_process_info.my_name, param))) {
            ORTE_ERROR_LOG(rc);
            free(orted_uri);
            return rc;
        }
        /* we got something back - let's hope it was the uri.
         * Set the contact info into our RML - it will bark
         * if the returned info isn't a uri
         */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(orted_uri))) {
            ORTE_ERROR_LOG(rc);
            free(orted_uri);
            return rc;
        }
        /* extract the name, noting that the HNP is also my local daemon,
         * and define the route as direct
         */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(orted_uri, &orte_process_info.my_daemon, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(orted_uri);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_routed.update_route(&orte_process_info.my_daemon,
                                                           &orte_process_info.my_daemon))) {
            ORTE_ERROR_LOG(rc);
            free(orted_uri);
            return rc;
        }
        /* okay, the HNP is now setup. We actually don't need to
         * restart ourselves as we haven't really done anything yet.
         * So set the HNP info in our globals, and tell
         * orte_init that those things are done
         */
        orte_universe_info.seed_uri = strdup(orted_uri);
        orte_process_info.ns_replica_uri = strdup(orted_uri);
        orte_process_info.gpr_replica_uri = strdup(orted_uri);
       /* indicate we are a singleton so orte_init knows what to do */
        orte_process_info.singleton = true;
        /* all done - report success */
        free(orted_uri);
        return ORTE_SUCCESS;
    }
#else
    /* someone will have to devise a Windows equivalent */
#endif    
    
    return ORTE_SUCCESS;
}
