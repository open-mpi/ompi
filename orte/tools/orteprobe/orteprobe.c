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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <fcntl.h>
#include <errno.h>

#include "orte/orte_constants.h"

#include "opal/runtime/opal.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/event/event.h"
#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/os_path.h"
#include "opal/util/cmd_line.h"
#include "opal/util/printf.h"
#include "opal/util/daemon_init.h"
#include "opal/util/malloc.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/timer/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/schema/base/base.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"

#include "orte/tools/orteprobe/orteprobe.h"

orteprobe_globals_t orteprobe_globals;

/*
 * define the orteprobe context table for obtaining parameters
 */
opal_cmd_line_init_t orte_cmd_line_opts[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0, 
      &orteprobe_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, '\0', NULL, "verbose", 0, 
      &orteprobe_globals.verbose, OPAL_CMD_LINE_TYPE_BOOL,
      "Toggle Verbosity" },

    { NULL, NULL, NULL, 'd', NULL, "debug", 0,
      &orteprobe_globals.debug, OPAL_CMD_LINE_TYPE_BOOL,
      "Run in debug mode (not generally intended for users)" },

    { NULL, NULL, NULL, '\0', NULL, "name", 1,
      &orteprobe_globals.name_string, OPAL_CMD_LINE_TYPE_STRING,
      "Set the orte process name"},

    { NULL, NULL, NULL, '\0', NULL, "nsreplica", 1,
      &orte_process_info.ns_replica_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Name service contact information."},

    { NULL, NULL, NULL, '\0', NULL, "gprreplica", 1,
      &orte_process_info.gpr_replica_uri, OPAL_CMD_LINE_TYPE_STRING,
      "Registry contact information."},

    { NULL, NULL, NULL, '\0', NULL, "nodename", 1,
      &orte_system_info.nodename, OPAL_CMD_LINE_TYPE_STRING,
      "Node name as specified by host/resource description." },

    { NULL, NULL, NULL, '\0', NULL, "requestor", 1,
      &orteprobe_globals.requestor_string, OPAL_CMD_LINE_TYPE_STRING,
      "Set the orte process name"},

    { "seed", NULL, NULL, '\0', NULL, "seed", 0,
      &orte_process_info.seed, OPAL_CMD_LINE_TYPE_BOOL,
      "seed"},

    { "universe", "persistence", NULL, '\0', NULL, "persistent", 0,
      &orte_universe_info.persistence, OPAL_CMD_LINE_TYPE_BOOL,
      "persistent"},

    { "universe", "scope", NULL, '\0', NULL, "scope", 1,
      &orte_universe_info.scope, OPAL_CMD_LINE_TYPE_STRING,
      "scope"},

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

#if !defined(__WINDOWS__)
extern char **environ;
#endif  /* !defined(__WINDOWS__) */

int main(int argc, char *argv[])
{
    int ret = 0, ortedargc;
    opal_cmd_line_t *cmd_line = NULL;
    char *contact_path = NULL, *orted=NULL;
    char *log_path = NULL, **ortedargv;
    char *universe, orted_uri[256], **orted_uri_ptr, *path, *param;
    orte_universe_t univ;
    orte_buffer_t buffer;
    orte_process_name_t requestor;
    int id, orted_pipe[2];
    pid_t pid;

#if defined(HAVE_FORK) && defined(HAVE_PIPE)

    if (ORTE_SUCCESS != (ret = opal_init())) {
        return ret;
    }
    
    /* setup to check common command line options that just report and die */
    memset(&orteprobe_globals, 0, sizeof(orteprobe_globals));
    cmd_line = OBJ_NEW(opal_cmd_line_t);
    opal_cmd_line_create(cmd_line, orte_cmd_line_opts);

    ret = opal_cmd_line_parse(cmd_line, true, argc, argv);
    if (ORTE_SUCCESS != ret) {
        return ret;
    }
    
    /* check for help request */
    if (orteprobe_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        opal_show_help("help-orteprobe.txt", "orteprobe:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    /*
     * Attempt to parse the probe's name and save in proc_info
     */
    if (orteprobe_globals.name_string) {
        ret = orte_ns.convert_string_to_process_name(
            &orte_process_info.my_name, orteprobe_globals.name_string);
        if(ORTE_SUCCESS != ret) {
            fprintf(stderr, "orteprobe: Couldn't convert environmental string to probe's process name\n");
            return 1;
        }
    }

    /* 
     * If threads are supported - assume that we are using threads - and reset otherwise. 
     */
    opal_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        return ret;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        return ret;
    }

    /* Ensure the universe_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_univ_info())) {
        return ret;
    }
 
    /*
     * Initialize the data storage service.
     */
    if (ORTE_SUCCESS != (ret = orte_dss_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Open the name services to ensure access to local functions 
     */
    if (ORTE_SUCCESS != (ret = orte_ns_base_open())) {
        return ret;
    }

    /* Open the error manager to activate error logging - needs local name services */
    if (ORTE_SUCCESS != (ret = orte_errmgr_base_open())) {
        return ret;
    }
    
    /*****   ERROR LOGGING NOW AVAILABLE *****/

    /*
     * Initialize the event library 
    */
    if (ORTE_SUCCESS != (ret = opal_event_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Intialize the general progress engine
     */
    if (ORTE_SUCCESS != (ret = opal_progress_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Internal startup
     */
    if (ORTE_SUCCESS != (ret = orte_wait_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Runtime Messaging Layer
     */
    if (ORTE_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Runtime Messaging Layer
     */
    if (ORTE_SUCCESS != (ret = orte_rml_base_select())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Registry
     */
    if (ORTE_SUCCESS != (ret = orte_gpr_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Initialize schema utilities
     */

    if (ORTE_SUCCESS != (ret = orte_schema_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Attempt to parse the requestor's name and contact info
     */
    if (orteprobe_globals.requestor_string) {
        if(ORTE_SUCCESS != (ret = orte_rml.parse_uris(
                    orteprobe_globals.requestor_string, &requestor, NULL))) {
            fprintf(stderr, "orteprobe: Couldn't parse environmental string for requestor's contact info\n");
            return 1;
        }
        /* set the contact info */
        if (ORTE_SUCCESS != (ret = orte_rml.set_uri(orteprobe_globals.requestor_string))) {
            fprintf(stderr, "orteprobe: Couldn't set contact info for requestor\n");
            return ret;
        }
    } else {
        fprintf(stderr, "orteprobe: No contact info received for requestor\n");
        return 1;
    }

    /* see if a universe already exists on this machine and
     * will allow contact with us
     */
    if (ORTE_SUCCESS == (ret = orte_universe_exists(&univ))) {
        /* universe is here! send info back and die */
        if(orteprobe_globals.verbose)
            fprintf(stderr, "orteprobe: Contacted existing universe - sending contact info back\n");

        OBJ_CONSTRUCT(&buffer, orte_buffer_t);
        orted_uri_ptr = &(univ.seed_uri);

        if (ORTE_SUCCESS != (ret = orte_dss.pack(&buffer, &orted_uri_ptr, 1, ORTE_STRING))) {
            fprintf(stderr, "orteprobe: failed to pack contact info for existing universe\n");
            orte_errmgr.error_detected(1, NULL);
        }

        if (0 > orte_rml.send_buffer(&requestor, &buffer, ORTE_RML_TAG_PROBE, 0)) {
            fprintf(stderr, "orteprobe: comm failure when sending contact info for existing univ back to requestor\n");
            OBJ_DESTRUCT(&buffer);
            orte_errmgr.error_detected(1, NULL);
        }

        OBJ_DESTRUCT(&buffer);
    } else {
        /* existing universe is not here or does not allow contact.
         * ensure we have a unique universe name, fork/exec an appropriate
         * daemon, and then tell whomever spawned us how to talk to the new
         * daemon
         */
        if(orteprobe_globals.verbose)
            fprintf(stderr, "orteprobe: Could not connect to existing universe\n");

        if (ORTE_ERR_NOT_FOUND != ret) {
            if(orteprobe_globals.verbose)
                fprintf(stderr, "orteprobe: Existing universe did not respond\n");

            /* if it exists but no contact could be established,
             * define unique name based on current one.
             */
            universe = strdup(orte_universe_info.name);
            free(orte_universe_info.name);
            orte_universe_info.name = NULL;
            pid = getpid();

            if (0 > asprintf(&orte_universe_info.name, "%s-%d", universe, pid)) {
                fprintf(stderr, "orteprobe: failed to create unique universe name");
                orte_errmgr.error_detected(1, NULL);
            }
        }

        /* setup to fork/exec the new universe */
        /* setup the pipe to get the contact info back */
        if (pipe(orted_pipe)) {
            fprintf (stderr, "orteprobe: Pipe failed\n");
            orte_errmgr.error_detected(1, NULL);
        }
        
        /* get name of orted application - just in case user specified something different */
        id = mca_base_param_register_string("orted",NULL,NULL,NULL,"orted");
        mca_base_param_lookup_string(id, &orted);
        
        if(orteprobe_globals.verbose)
            fprintf(stderr, "orteprobe: Using \"%s\" for orted command\n", orted);

        /* Initialize the argv array */
        ortedargv = opal_argv_split(orted, ' ');
        ortedargc = opal_argv_count(ortedargv);
        if (ortedargc <= 0) {
            fprintf(stderr, "orteprobe: could not initialize argv array for daemon\n");
            orte_errmgr.error_detected(1, NULL);
        }
        
        /* setup the path */
        path = opal_path_findv(ortedargv[0], 0, environ, NULL);
    
        if(orteprobe_globals.verbose)
            fprintf(stderr, "orteprobe: Path setup as \"%s\"\n", path);

        /* tell the daemon it's the seed */
        opal_argv_append(&ortedargc, &ortedargv, "--seed");
    
        /* tell the daemon it's scope */
        opal_argv_append(&ortedargc, &ortedargv, "--scope");
        opal_argv_append(&ortedargc, &ortedargv, orte_universe_info.scope);
        
        /* tell the daemon if it's to be persistent */
        if (orte_universe_info.persistence) {
            opal_argv_append(&ortedargc, &ortedargv, "--persistent");
        }
        
        /* tell the daemon to report its uri to us */
        asprintf(&param, "%d", orted_pipe[1]);
        opal_argv_append(&ortedargc, &ortedargv, "--report-uri");
        opal_argv_append(&ortedargc, &ortedargv, param);
        free(param);
 
        if(orteprobe_globals.verbose)
            fprintf(stderr, "orteprobe: Forking now\n");

        /* Create the child process. */
        pid = fork ();
        if (pid == (pid_t) 0) {
            /* This is the child process.
                Close read end first. */
            execv(path, ortedargv);
            fprintf(stderr, "orteprobe: execv failed with errno=%d\n", errno);
            orte_errmgr.error_detected(1, NULL);
        } else if (pid < (pid_t) 0) {
            /* The fork failed. */
            fprintf (stderr, "orteprobe: Fork failed\n");
            orte_errmgr.error_detected(1, NULL);
        } else {
            /* This is the parent process.
                Close write end first. */

            if(orteprobe_globals.verbose)
                fprintf(stderr, "orteprobe: Attempting to read from daemon\n");

            read(orted_pipe[0], orted_uri, 255);
            close(orted_pipe[0]);
            
            /* send back the info */
            OBJ_CONSTRUCT(&buffer, orte_buffer_t);
            param = orted_uri;
            orted_uri_ptr = &param;

            if (ORTE_SUCCESS != (ret = orte_dss.pack(&buffer, &orted_uri_ptr[0], 1, ORTE_STRING))) {
                fprintf(stderr, "orteprobe: failed to pack daemon uri\n");
                orte_errmgr.error_detected(1, NULL);
            }

            if (0 > orte_rml.send_buffer(&requestor, &buffer, ORTE_RML_TAG_PROBE, 0)) {
                fprintf(stderr, "orteprobe: could not send daemon uri info back to probe\n");
                OBJ_DESTRUCT(&buffer);
                orte_errmgr.error_detected(1, NULL);
            }

            OBJ_DESTRUCT(&buffer);
        }
    }

    if(orteprobe_globals.verbose)
        fprintf(stderr, "orteprobe: All finished!\n");
    
    /* cleanup */
    if (NULL != contact_path) {
	    unlink(contact_path);
    }

    if (NULL != log_path) {
        unlink(log_path);
    }

    /* finalize the system */
    orte_finalize();

    opal_finalize();

    exit(0);
#else /* HAVE_FORK && HAVE_PIPE */
    fprintf(stderr, "orteprobe: system appears to not support remote probes\n");
    exit(1);
#endif
}
