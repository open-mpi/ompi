/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <fcntl.h>
#include <signal.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/if.h"
#include "opal/util/os_path.h"
#include "opal/util/path.h"
#include "opal/event/event.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "opal/util/basename.h"

#include "orte/util/sys_info.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"

#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_wakeup.h"
#include "orte/runtime/params.h"
#include "orte/dss/dss.h"

#include "orte/mca/ns/ns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ras/ras_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/smr/smr.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"
#include "orte/mca/pls/process/pls_process.h"


#define rindex(a,b) strrchr((a),(b)) //daniel

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
static int orte_pls_process_launch_threaded(orte_jobid_t jobid);
#endif

/*
 * Interface
 */
static int orte_pls_process_launch(orte_jobid_t);
static int orte_pls_process_terminate_job(orte_jobid_t, struct timeval *timeout, opal_list_t*);
static int orte_pls_process_terminate_orteds(struct timeval *timeout, opal_list_t*);
static int orte_pls_process_terminate_proc(const orte_process_name_t* proc_name);
static int orte_pls_process_signal_job(orte_jobid_t, int32_t, opal_list_t*);
static int orte_pls_process_signal_proc(const orte_process_name_t* proc_name, int32_t);

orte_pls_base_module_t orte_pls_process_module = {
#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS
    orte_pls_process_launch_threaded,
#else
    orte_pls_process_launch,
#endif
    orte_pls_process_terminate_job,
    orte_pls_process_terminate_orteds,
    orte_pls_process_terminate_proc,
    orte_pls_process_signal_job,
    orte_pls_process_signal_proc,
    orte_pls_process_finalize
};

static void set_handler_default(int sig);

enum {
    ORTE_PLS_RSH_SHELL_BASH = 0,
    ORTE_PLS_RSH_SHELL_ZSH,
    ORTE_PLS_RSH_SHELL_TCSH,
    ORTE_PLS_RSH_SHELL_CSH,
    ORTE_PLS_RSH_SHELL_KSH,
    ORTE_PLS_RSH_SHELL_SH,
    ORTE_PLS_RSH_SHELL_UNKNOWN
};

typedef int orte_pls_process_shell;

static const char * orte_pls_process_shell_name[] = {
    "bash",
    "zsh",
    "tcsh",       /* tcsh has to be first otherwise strstr finds csh */
    "csh",
    "ksh",
    "sh",
    "unknown"
}; 


/* local global storage of timing variables */
static struct timeval joblaunchstart, joblaunchstop;

/* global storage of active jobid being launched */
static orte_jobid_t active_job = ORTE_JOBID_INVALID;

/**
 * Check the Shell variable on the specified node
 */

static int orte_pls_process_probe(orte_mapped_node_t * node, orte_pls_process_shell * shell)
{
    char ** argv;
    int rc, nfds;
    int fd[2];
    pid_t pid;

/*    HANDLE myPipeFd[2]; 
    SECURITY_ATTRIBUTES securityAttr;
    STARTUPINFO startupInfo;
    PROCESS_INFORMATION processInfo;
*/    
    fd_set readset;
    fd_set errset;
    char outbuf[4096];

    if (mca_pls_process_component.debug) {
        opal_output(0, "pls:process: going to check SHELL variable on node %s\n",
                    node->nodename);
    }
    *shell = ORTE_PLS_RSH_SHELL_UNKNOWN;
    /*
     * Build argv array
     */
     
    pid = _spawnve( _P_DETACH, argv[0], argv, NULL); //daniel     
     
#if 0
    securityAttr.nLength = sizeof(SECURITY_ATTRIBUTES);     // Size of struct
    securityAttr.lpSecurityDescriptor = NULL;               // Default descriptor
    securityAttr.bInheritHandle = TRUE;                     // Inheritable

    // Create the pipe
    if (CreatePipe(&myPipeFd[0], &myPipeFd[1], &securityAttr, 0)) {

        // Create duplicate of write end so that original 
        if (!DuplicateHandle(
            GetCurrentProcess(),
            myPipeFd[0],           // Original handle
            GetCurrentProcess(),
            NULL,                    // don't create new handle
            0,
            FALSE,                   // Not inheritable
            DUPLICATE_SAME_ACCESS)
        ) {
            CloseHandle(myPipeFd[0]);
            CloseHandle(myPipeFd[1]);
            opal_output(0, "pls:process: DuplicateHandle failed with errno=%d\n", errno);
            return ORTE_ERR_IN_ERRNO;
        }  
        
        ZeroMemory( &startupInfo, sizeof(startupInfo) );
        startupInfo.cb = sizeof(startupInfo);
        ZeroMemory( &processInfo, sizeof(processInfo) );

        // Now populate startup info for CreateProcess
        GetStartupInfo(&startupInfo);
        startupInfo.dwFlags    = STARTF_USESTDHANDLES;
        startupInfo.hStdInput  = myPipeFd[0];
        startupInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
        startupInfo.hStdError  = GetStdHandle(STD_ERROR_HANDLE);
        

        // Start the child process. 
        if( !CreateProcess( argv[0],    //module name   NULL,
            NULL, //(LPSTR)(const char *) argv,
            NULL,           // Process handle not inheritable
            NULL,           // Thread handle not inheritable
            TRUE,           // Set handle inheritance to TRUE; 
            // each inheritable handle in the calling process is inherited by the new process
            0,              // No creation flags
            NULL,           // Use parent's environment block
            NULL,           // Use parent's starting directory 
            &startupInfo,   // Pointer to STARTUPINFO structure
            &processInfo )  // Pointer to PROCESS_INFORMATION structure
        ) 
        {
            CloseHandle(myPipeFd[1]);
            opal_output(0, "pls:process: CreateProcess failed with errno=%d\n", errno); //, GetLastError() ); 
            return ORTE_ERR_IN_ERRNO;
        }
    }
#endif    
    /*
    if ((pid = fork()) < 0) {
        opal_output(0, "pls:process: fork failed with errno=%d\n", errno);
        return ORTE_ERR_IN_ERRNO;
    }
    else if (pid == 0) {          // child      //processInfo.hProcess

        if (dup2(fd[1], 1) < 0) {
            opal_output(0, "pls:process: dup2 failed with errno=%d\n", errno);
            return ORTE_ERR_IN_ERRNO;
        }
        execvp(argv[0], argv);
        exit(errno);
    }
    if (close(fd[1])) {
        opal_output(0, "pls:process: close failed with errno=%d\n", errno);
        return ORTE_ERR_IN_ERRNO;
    }
    */
    
    
    /* Monitor stdout */
    FD_ZERO(&readset);
    nfds = fd[0]+1;

    memset (outbuf, 0, sizeof (outbuf));
    rc = ORTE_SUCCESS;;
    while (ORTE_SUCCESS == rc) {
        int err;
        FD_SET (fd[0], &readset);
        errset = readset;
        err = select(nfds, &readset, NULL, &errset, NULL);
        if (err == -1) {
            if (errno == EINTR)
                continue;
            else {
                rc = ORTE_ERR_IN_ERRNO;
                break;
            }
        }
        if (FD_ISSET(fd[0], &errset) != 0)
            rc = ORTE_ERR_FATAL;
        /* In case we have something valid to read on stdin */
        if (FD_ISSET(fd[0], &readset) != 0) {
            ssize_t ret = 1;
            char temp[4096];
            char * ptr = outbuf;
            ssize_t outbufsize = sizeof(outbuf);

            memset (temp, 0, sizeof(temp));

            while (ret != 0) {
                ret = read (fd[0], temp, 256);
                if (ret < 0) {
                    if (errno == EINTR)
                        continue;
                    else {
                        rc = ORTE_ERR_IN_ERRNO;
                        break;
                    }
                }
                else {
                    if (outbufsize > 0) {
                        memcpy (ptr, temp, (ret > outbufsize) ? outbufsize : ret);
                        outbufsize -= ret;
                        ptr += ret;
                        if (outbufsize > 0)
                            *ptr = '\0';
                    }
                }
            }
            /* After reading complete string (aka read returns 0), we just break */
            break;
        }
    }

    /* Search for the substring of known shell-names */
/*    for (i = 0; i < (int)(sizeof (orte_pls_process_shell_name)/
                          sizeof(orte_pls_process_shell_name[0])); i++) {
        char *sh_name = NULL;

        sh_name = rindex(outbuf, '/');
        if ( sh_name != NULL ) {
            sh_name++; /* skip '/' */
            
            /* We cannot use "echo -n $SHELL" because -n is not portable. Therefore
             * we have to remove the "\n" */
/*            if ( sh_name[strlen(sh_name)-1] == '\n' ) {
                sh_name[strlen(sh_name)-1] = '\0';
            }
            if ( 0 == strcmp(sh_name, orte_pls_process_shell_name[i]) ) {
                *shell = i;
                break;
            }
        }
    }
*/
    if (mca_pls_process_component.debug) {
        opal_output(0, "pls:process: node:%s has SHELL: %s\n",
                    node->nodename, orte_pls_process_shell_name[*shell]);
    }
    return rc;
}

/**
 * Fill the exec_path variable with the directory to the orted
 */

static int orte_pls_process_fill_exec_path( char ** exec_path )
{
    struct stat buf;

    asprintf(exec_path, "%s/orted", opal_install_dirs.bindir);
    if (0 != stat(*exec_path, &buf)) {
        char *path = getenv("PATH");
        if (NULL == path) {
            path = "PATH is empty!";
        }
        opal_show_help("help-pls-process.txt", "no-local-orted",
                        true, path, opal_install_dirs.bindir);
        return ORTE_ERR_NOT_FOUND;
    }
   return ORTE_SUCCESS;
}

/**
 * Callback on daemon exit.
 */

static void orte_pls_process_wait_daemon(pid_t pid, int status, void* cbdata)
{
    orte_pls_daemon_info_t *info = (orte_pls_daemon_info_t*) cbdata;
    int rc;
    unsigned long deltat;
    orte_buffer_t ack;
    int src[3] = {-1, -1};

    if (! WIFEXITED(status) || ! WEXITSTATUS(status) == 0) {
        /* tell the user something went wrong */
        opal_output(0, "ERROR: A daemon failed to start as expected.");
        opal_output(0, "ERROR: There may be more information available from");
        opal_output(0, "ERROR: the remote shell (see above).");

        if (WIFEXITED(status)) {
            opal_output(0, "ERROR: The daemon exited unexpectedly with status %d.",
                   WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
#ifdef WCOREDUMP
            if (WCOREDUMP(status)) {
                opal_output(0, "The daemon received a signal %d (with core).",
                            WTERMSIG(status));
            } else {
                opal_output(0, "The daemon received a signal %d.", WTERMSIG(status));
            }
#else
            opal_output(0, "The daemon received a signal %d.", WTERMSIG(status));
#endif /* WCOREDUMP */
        } else {
            opal_output(0, "No extra status information is available: %d.", status);
        }
        /* need to fake a message to the daemon callback system so it can break out
            * of its receive loop
            */
        src[2] = pid;
        if(WIFSIGNALED(status)) {
            src[1] = WTERMSIG(status);
        }
        OBJ_CONSTRUCT(&ack, orte_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&ack, &src, 3, ORTE_INT))) {
            ORTE_ERROR_LOG(rc);
        }
        rc = orte_rml.send_buffer(ORTE_PROC_MY_NAME, &ack, ORTE_RML_TAG_ORTED_CALLBACK, 0);
        if (0 > rc) {
            ORTE_ERROR_LOG(rc);
        }
        OBJ_DESTRUCT(&ack);
        
        /*  The usual reasons for ssh to exit abnormally all are a pretty good
            indication that the child processes aren't going to start up properly.
            Set the job state to indicate we failed to launch so orterun's exit status
            will be non-zero and forcibly terminate the job so orterun can exit
        */
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(active_job, ORTE_JOB_STATE_FAILED_TO_START))) {
            ORTE_ERROR_LOG(rc);
        }
        
        if (ORTE_SUCCESS != (rc = orte_wakeup(active_job))) {
            ORTE_ERROR_LOG(rc);
        }

    } /* if abnormal exit */

    /* release any waiting threads */
    OPAL_THREAD_LOCK(&mca_pls_process_component.lock);

    if (mca_pls_process_component.num_children-- >=
        mca_pls_process_component.num_concurrent ||
        mca_pls_process_component.num_children == 0) {
        opal_condition_signal(&mca_pls_process_component.cond);
    }

    if (mca_pls_process_component.timing && mca_pls_process_component.num_children == 0) {
        if (0 != gettimeofday(&joblaunchstop, NULL)) {
            opal_output(0, "pls_process: could not obtain job launch stop time");
        } else {
            deltat = (joblaunchstop.tv_sec - joblaunchstart.tv_sec)*1000000 +
                     (joblaunchstop.tv_usec - joblaunchstart.tv_usec);
            opal_output(0, "pls_process: total time to launch job is %lu usec", deltat);
        }
    }
    
    OPAL_THREAD_UNLOCK(&mca_pls_process_component.lock);

}

/**
 * Launch a daemon (bootproxy) on each node. The daemon will be responsible
 * for launching the application.
 */

int orte_pls_process_launch(orte_jobid_t jobid)
{
    orte_job_map_t *map = NULL;
    opal_list_item_t *n_item;
    orte_mapped_node_t *rmaps_node;
    orte_std_cntr_t num_nodes;
    int node_name_index2;
    int proc_name_index;
    int local_exec_index;
    char *jobid_string = NULL;
    char *param;
    char **argv = NULL;
    char *prefix_dir;
    int argc = 0;
    int rc;
    char *lib_base = NULL, *bin_base = NULL;
    bool failed_launch = true;

    if (mca_pls_process_component.timing) {
        if (0 != gettimeofday(&joblaunchstart, NULL)) {
            opal_output(0, "pls_process: could not obtain start time");
            joblaunchstart.tv_sec = 0;
            joblaunchstart.tv_usec = 0;
        }        
    }
    
    /* set the active jobid */
    active_job = jobid;

    /* Get the map for this job
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know the nodes we are launching on
     * All other mapping responsibilities fall to orted in the fork PLS
     */
    rc = orte_rmaps.get_job_map(&map, jobid);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    num_nodes = map->num_new_daemons;
 	if (0 == num_nodes) {
        /* have all the daemons we need - launch app */
        goto launch_apps;
    }

    if (mca_pls_process_component.debug_daemons &&
        mca_pls_process_component.num_concurrent < num_nodes) {
        /**
         * If we are in '--debug-daemons' we keep the ssh connection 
         * alive for the span of the run. If we use this option 
         * AND we launch on more than "num_concurrent" machines
         * then we will deadlock. No connections are terminated 
         * until the job is complete, no job is started
         * since all the orteds are waiting for all the others
         * to come online, and the others ore not launched because
         * we are waiting on those that have started to terminate
         * their ssh tunnels. :(
         * As we cannot run in this situation, pretty print the error
         * and return an error code.
         */
        opal_show_help("help-pls-process.txt", "deadlock-params",
                       true, mca_pls_process_component.num_concurrent, num_nodes);
        rc = ORTE_ERR_FATAL;
        goto cleanup;
    }

    /*
     * After a discussion between Ralph & Jeff, we concluded that we
     * really are handling the prefix dir option incorrectly. It currently
     * is associated with an app_context, yet it really refers to the
     * location where OpenRTE/Open MPI is installed on a NODE. Fixing
     * this right now would involve significant change to orterun as well
     * as elsewhere, so we will intentionally leave this incorrect at this
     * point. The error, however, is identical to that seen in all prior
     * releases of OpenRTE/Open MPI, so our behavior is no worse than before.
     *
     * A note to fix this, along with ideas on how to do so, has been filed
     * on the project's Trac system under "feature enhancement".
     *
     * For now, default to the prefix_dir provided in the first app_context.
     * Since there always MUST be at least one app_context, we are safe in
     * doing this.
     */
    prefix_dir = map->apps[0]->prefix_dir;
    
    /*
     * Build argv array
     */
    opal_argv_append(&argc, &argv, "<template>");

    /* add the daemon command (as specified by user) */
    local_exec_index = argc;
    opal_argv_append(&argc, &argv, mca_pls_process_component.orted);

    /* Add basic orted command line options */
    orte_pls_base_orted_append_basic_args(&argc, &argv,
                                          &proc_name_index,
                                          &node_name_index2,
                                          map->num_nodes);

    if (mca_pls_process_component.debug) {
        param = opal_argv_join(argv, ' ');
        if (NULL != param) {
            opal_output(0, "pls:process: final template argv:");
            opal_output(0, "pls:process:     %s", param);
            free(param);
        }
    }

    /* Figure out the basenames for the libdir and bindir.  This
       requires some explanation:

       - Use OPAL_LIBDIR and OPAL_BINDIR instead of -D'ing some macros
       in this directory's Makefile.am because it makes all the
       dependencies work out correctly.  These are defined in
       opal/install_dirs.h.

       - After a discussion on the devel-core mailing list, the
       developers decided that we should use the local directory
       basenames as the basis for the prefix on the remote note.
       This does not handle a few notable cases (e.g., f the
       libdir/bindir is not simply a subdir under the prefix, if the
       libdir/bindir basename is not the same on the remote node as
       it is here in the local node, etc.), but we decided that
       --prefix was meant to handle "the common case".  If you need
       something more complex than this, a) edit your shell startup
       files to set PATH/LD_LIBRARY_PATH properly on the remove
       node, or b) use some new/to-be-defined options that
       explicitly allow setting the bindir/libdir on the remote
       node.  We decided to implement these options (e.g.,
       --remote-bindir and --remote-libdir) to orterun when it
       actually becomes a problem for someone (vs. a hypothetical
       situation).

       Hence, for now, we simply take the basename of this install's
       libdir and bindir and use it to append this install's prefix
       and use that on the remote node.
    */

    lib_base = opal_basename(opal_install_dirs.libdir);
    bin_base = opal_basename(opal_install_dirs.bindir);

    /*
     * Iterate through each of the nodes
     */
    
    for(n_item =  opal_list_get_first(&map->nodes);
        n_item != opal_list_get_end(&map->nodes);
        n_item =  opal_list_get_next(n_item)) {
        pid_t pid;
        char *exec_path = NULL;
        char **exec_argv;
        
        rmaps_node = (orte_mapped_node_t*)n_item;
        
        /* if this daemon already exists, don't launch it! */ 
        if (rmaps_node->daemon_preexists) { 
            continue; 
        }

        /* setup node name */
        free(argv[node_name_index2]);
        argv[node_name_index2] = strdup(rmaps_node->nodename);

        if (mca_pls_process_component.debug) {
            param = opal_argv_join(argv, ' ');
            if (NULL != param) {
                opal_output(0, "pls:process: start daemon as:");
                opal_output(0, "pls:process:     %s", param);
                free(param);
            }
        }
        {
            char* name_string;
            char** env;
            char* var;

            if (mca_pls_process_component.debug) {
                opal_output(0, "pls:process: launching on node %s\n",
                            rmaps_node->nodename);
            }

            /* We don't need to sense an oversubscribed condition and set the sched_yield
             * for the node as we are only launching the daemons at this time. The daemons
             * are now smart enough to set the oversubscribed condition themselves when
             * they launch the local procs.
             */

            /* We're always starting local jobs ... */
            if (mca_pls_process_component.debug) {
                opal_output(0, "pls:process: %s is a LOCAL node\n",
                            rmaps_node->nodename);
            }

            exec_argv = &argv[local_exec_index];
            /* If the user provide a prefix then first try to find the application there */
            if( NULL != prefix_dir ) {
                char* full_path[3];
                
                full_path[0] = opal_os_path( false, prefix_dir, NULL );
                full_path[1] = opal_os_path( false, prefix_dir, bin_base, NULL );
                full_path[2] = NULL;
                exec_path = opal_path_find(exec_argv[0], full_path, F_OK, NULL);
                free(full_path[0]); free(full_path[1]);
            }
            if( NULL == exec_path ) {
                /* find the application in the default PATH */
                exec_path = opal_path_findv(exec_argv[0], F_OK, environ, NULL);
                if( NULL == exec_path ) {
                    char* full_path[2];
                    
                    full_path[0] = opal_os_path( false, opal_install_dirs.bindir, NULL );
                    full_path[1] = NULL;
                    exec_path = opal_path_find(exec_argv[0], full_path, F_OK, NULL);
                    free(full_path[0]);
                    
                    if( NULL == exec_path ) {
                        rc = orte_pls_process_fill_exec_path (&exec_path);
                        if (ORTE_SUCCESS != rc) {
                            return rc;
                        }
                    }
                }
            }
            
            /* If we have a prefix, then modify the PATH and
               LD_LIBRARY_PATH environment variables.  We're
               already in the child process, so it's ok to modify
               environ. */
            if (NULL != prefix_dir) {
                char *oldenv, *newenv;
                
                /* Reset PATH */
                newenv = opal_dirname(exec_path);
                /*newenv = opal_os_path( false, prefix_dir, bin_base, NULL );*/
                oldenv = getenv("PATH");
                if (NULL != oldenv) {
                    char *temp;
                    asprintf(&temp, "%s;%s", newenv, oldenv );
                    free( newenv );
                    newenv = temp;
                }
                opal_setenv("PATH", newenv, true, &environ);
                if (mca_pls_process_component.debug) {
                    opal_output(0, "pls:process: reset PATH: %s", newenv);
                }
                free(newenv);
                
                /* Reset LD_LIBRARY_PATH */
                newenv = opal_os_path( false, prefix_dir, lib_base, NULL );
                oldenv = getenv("LD_LIBRARY_PATH");
                if (NULL != oldenv) {
                    char* temp;
                    asprintf(&temp, "%s;%s", newenv, oldenv);
                    free(newenv);
                    newenv = temp;
                }
                opal_setenv("LD_LIBRARY_PATH", newenv, true, &environ);
                if (mca_pls_process_component.debug) {
                    opal_output(0, "pls:process: reset LD_LIBRARY_PATH: %s",
                                newenv);
                }
                free(newenv);
            }
            
            /* tell the daemon to setup its own process session/group */
            opal_argv_append(&argc, &argv, "--set-sid");
            exec_argv = &argv[local_exec_index];

#if 0
            /* Finally, chdir($HOME) because we're making the
               assumption that this is what will happen on
               remote nodes (via submit).  This allows a user
               to specify a path that is relative to $HOME for
               both the cwd and argv[0] and it will work on
               all nodes -- including the local host.
               Otherwise, it would work on remote nodes and
               not the local node.  If the user does not start
               in $HOME on the remote nodes... well... let's
               hope they start in $HOME.  :-) */
            var = getenv("HOME");
            if (NULL != var) {
                if (mca_pls_process_component.debug) {
                    opal_output(0, "pls:process: changing to directory %s",
                                var);
                }
                /* Ignore errors -- what are we going to do?
                   (and we ignore errors on the remote nodes
                   in the fork pls, so this is consistent) */
                chdir(var);
            }
#endif                
        
            /* setup process name */
            rc = orte_ns.get_proc_name_string(&name_string, rmaps_node->daemon);
            if (ORTE_SUCCESS != rc) {
                opal_output(0, "orte_pls_process: unable to get daemon name as string");
                exit(-1);
            }
            free(argv[proc_name_index]);
            argv[proc_name_index] = strdup(name_string);
            
            /* Set signal handlers back to the default.  Do this close
               to the execve() because the event library may (and likely
               will) reset them.  If we don't do this, the event
               library may have left some set that, at least on some
               OS's, don't get reset via fork() or exec().  Hence, the
               orted could be unkillable (for example). */
            
            set_handler_default(SIGTERM);
            set_handler_default(SIGINT);
            //set_handler_default(SIGHUP);
            //set_handler_default(SIGPIPE);
            set_handler_default(SIGCHLD);
            
            /* setup environment */
            env = opal_argv_copy(environ);
            var = mca_base_param_environ_variable("seed",NULL,NULL);
            opal_setenv(var, "0", true, &env);
            
            /* exec the daemon */
            if (mca_pls_process_component.debug) {
                param = opal_argv_join(exec_argv, ' ');
                if (NULL != param) {
                    opal_output(0, "pls:process: executing: %s", param);
                    free(param);
                }
            }
            pid = _spawnve( _P_NOWAIT, exec_path, exec_argv, env);
            if (pid == -1) {
                /* indicate this daemon has been launched in case anyone is sitting on that trigger */ 
                if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(rmaps_node->daemon, ORTE_PROC_STATE_LAUNCHED, 0))) { 
                    ORTE_ERROR_LOG(rc); 
                    goto cleanup; 
                }
                failed_launch = true;
                rc = ORTE_ERROR;
                goto cleanup;
            }
            /* indicate this daemon has been launched in case anyone is sitting on that trigger */
            if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(rmaps_node->daemon, ORTE_PROC_STATE_LAUNCHED, 0))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            opal_output(orte_pls_base.pls_output, "pls:process: execv %s hopefully started (pid %d)\n", exec_path, pid);
        
            OPAL_THREAD_LOCK(&mca_pls_process_component.lock);
            /* This situation can lead to a deadlock if '--debug-daemons' is set.
             * However, the deadlock condition is tested at the begining of this
             * function, so we're quite confident it should not happens here.
             */
            if (mca_pls_process_component.num_children++ >=
                mca_pls_process_component.num_concurrent) {
                opal_condition_wait(&mca_pls_process_component.cond, &mca_pls_process_component.lock);
            }
            OPAL_THREAD_UNLOCK(&mca_pls_process_component.lock);

            /* setup callback on sigchild - wait until setup above is complete
             * as the callback can occur in the call to orte_wait_cb
             */
            orte_wait_cb(pid, orte_pls_process_wait_daemon, NULL);
            
            /* if required - add delay to avoid problems w/ X11 authentication */
            if (mca_pls_process_component.debug && mca_pls_process_component.delay) {
                sleep(mca_pls_process_component.delay);
            }
        }
    }

    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = orte_pls_base_daemon_callback(map->num_new_daemons))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
launch_apps:
    if (ORTE_SUCCESS != (rc = orte_pls_base_launch_apps(map))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* get here if launch went okay */
    failed_launch = false;
    
 cleanup:
    if (NULL != map) {
        OBJ_RELEASE(map);
    }

    if (NULL != lib_base) {
        free(lib_base);
    }
    if (NULL != bin_base) {
        free(bin_base);
    }

    if (NULL != jobid_string) {
        free(jobid_string);  /* done with this variable */
    }
    if (NULL != argv) {
        opal_argv_free(argv);
    }

    /* check for failed launch - if so, force terminate */
    if( failed_launch ) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(jobid, ORTE_JOB_STATE_FAILED_TO_START))) {
            ORTE_ERROR_LOG(rc);
        }
        
        if (ORTE_SUCCESS != (rc = orte_wakeup(jobid))) {
            ORTE_ERROR_LOG(rc);
        }        
    }

    return rc;
}


/**
 * Terminate all processes for a given job
 */
int orte_pls_process_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* order them to kill their local procs for this job */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_kill_local_procs(jobid, timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/**
* Terminate the orteds for a given job
 */
int orte_pls_process_terminate_orteds(struct timeval *timeout, opal_list_t *attrs)
{
    int rc;
    
    /* now tell them to die! */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_exit(timeout, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

/*
 * Terminate a specific process
 */
int orte_pls_process_terminate_proc(const orte_process_name_t* proc)
{
    OPAL_TRACE(1);
    
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_pls_process_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    int rc;
    
    /* order them to pass this signal to their local procs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_orted_signal_local_procs(jobid, signal, attrs))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}

int orte_pls_process_signal_proc(const orte_process_name_t* proc, int32_t signal)
{
    OPAL_TRACE(1);
    
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_pls_process_finalize(void)
{
    int rc;
    
    /* cleanup any pending recvs */
    if (ORTE_SUCCESS != (rc = orte_pls_base_comm_stop())) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/**
 * Handle threading issues.
 */

#if OMPI_HAVE_POSIX_THREADS && OMPI_THREADS_HAVE_DIFFERENT_PIDS && OMPI_ENABLE_PROGRESS_THREADS

struct orte_pls_process_stack_t {
    opal_condition_t cond;
    opal_mutex_t mutex;
    bool complete;
    orte_jobid_t jobid;
    int rc;
};
typedef struct orte_pls_process_stack_t orte_pls_process_stack_t;

static void orte_pls_process_stack_construct(orte_pls_process_stack_t* stack)
{
    OBJ_CONSTRUCT(&stack->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&stack->cond, opal_condition_t);
    stack->rc = 0;
    stack->complete = false;
}

static void orte_pls_process_stack_destruct(orte_pls_process_stack_t* stack)
{
    OBJ_DESTRUCT(&stack->mutex);
    OBJ_DESTRUCT(&stack->cond);
}

static OBJ_CLASS_INSTANCE(
    orte_pls_process_stack_t,
    opal_object_t,
    orte_pls_process_stack_construct,
    orte_pls_process_stack_destruct);

static void orte_pls_process_launch_cb(int fd, short event, void* args)
{
    orte_pls_process_stack_t *stack = (orte_pls_process_stack_t*)args;
    OPAL_THREAD_LOCK(&stack->mutex);
    stack->rc = orte_pls_process_launch(stack->jobid);
    stack->complete = true;
    opal_condition_signal(&stack->cond);
    OPAL_THREAD_UNLOCK(&stack->mutex);
}

static int orte_pls_process_launch_threaded(orte_jobid_t jobid)
{
    struct timeval tv = { 0, 0 };
    struct opal_event event;
    struct orte_pls_process_stack_t stack;

    OBJ_CONSTRUCT(&stack, orte_pls_process_stack_t);

    stack.jobid = jobid;
    if( opal_event_progress_thread() ) {
        stack.rc = orte_pls_process_launch( jobid );
    } else {
        opal_evtimer_set(&event, orte_pls_process_launch_cb, &stack);
        opal_evtimer_add(&event, &tv);

        OPAL_THREAD_LOCK(&stack.mutex);
        while (stack.complete == false) {
            opal_condition_wait(&stack.cond, &stack.mutex);
        }
        OPAL_THREAD_UNLOCK(&stack.mutex);
    }
    OBJ_DESTRUCT(&stack);
    return stack.rc;
}

#endif


static void set_handler_default(int sig)
{
    OPAL_TRACE(1);
/*
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
*/
}
