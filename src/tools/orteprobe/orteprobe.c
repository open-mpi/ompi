/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/orte_constants.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "dps/dps.h"
#include "event/event.h"
#include "util/output.h"
#include "util/show_help.h"
#include "util/sys_info.h"
#include "util/os_path.h"
#include "util/cmd_line.h"
#include "util/proc_info.h"
#include "util/univ_info.h"
#include "util/session_dir.h"
#include "util/printf.h"
#include "util/daemon_init.h"
#include "util/universe_setup_file_io.h"
#include "util/malloc.h"

#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/rml/base/base.h"
#include "mca/rml/rml.h"
#include "mca/errmgr/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "mca/schema/base/base.h"
#include "mca/soh/base/base.h"

#include "runtime/runtime.h"
#include "runtime/orte_wait.h"

#include "tools/orteprobe/orteprobe.h"

orteprobe_globals_t orteprobe_globals;

/*
 * define the orteprobe context table for obtaining parameters
 */
ompi_cmd_line_init_t orte_cmd_line_opts[] = {
    /* Various "obvious" options */
    { NULL, NULL, NULL, 'h', NULL, "help", 0, 
      &orteprobe_globals.help, OMPI_CMD_LINE_TYPE_BOOL,
      "This help message" },

    { NULL, NULL, NULL, '\0', NULL, "version", 0,
      &orteprobe_globals.version, OMPI_CMD_LINE_TYPE_BOOL,
      "Show the orteprobe version" },

    { NULL, NULL, NULL, 'd', NULL, "debug", 0,
      &orteprobe_globals.debug, OMPI_CMD_LINE_TYPE_BOOL,
      "Run in debug mode (not generally intended for users)" },

    { NULL, NULL, NULL, '\0', NULL, "name", 1,
      &orteprobe_globals.name_string, OMPI_CMD_LINE_TYPE_STRING,
      "Set the orte process name"},

    { NULL, NULL, NULL, '\0', NULL, "nsreplica", 1,
      &orte_process_info.ns_replica_uri, OMPI_CMD_LINE_TYPE_STRING,
      "Name service contact information."},

    { NULL, NULL, NULL, '\0', NULL, "gprreplica", 1,
      &orte_process_info.gpr_replica_uri, OMPI_CMD_LINE_TYPE_STRING,
      "Registry contact information."},

    { NULL, NULL, NULL, '\0', NULL, "nodename", 1,
      &orte_system_info.nodename, OMPI_CMD_LINE_TYPE_STRING,
      "Node name as specified by host/resource description." },

    { NULL, NULL, NULL, '\0', NULL, "requestor", 1,
      &orteprobe_globals.requestor_string, OMPI_CMD_LINE_TYPE_STRING,
      "Set the orte process name"},

    { "seed", NULL, NULL, '\0', NULL, "seed", 0,
      &orte_process_info.seed, OMPI_CMD_LINE_TYPE_BOOL,
      "seed"},

    { "universe", "persistence", NULL, '\0', NULL, "persistent", 0,
      &orte_universe_info.persistence, OMPI_CMD_LINE_TYPE_BOOL,
      "persistent"},

    { "universe", "scope", NULL, '\0', NULL, "scope", 1,
      &orte_universe_info.scope, OMPI_CMD_LINE_TYPE_STRING,
      "scope"},

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OMPI_CMD_LINE_TYPE_NULL, NULL }
};


int main(int argc, char *argv[])
{
    int ret = 0;
    ompi_cmd_line_t *cmd_line = NULL;
    char *contact_path = NULL;
    char *log_path = NULL;
    orte_universe_t univ;
    orte_buffer_t buffer;
    orte_process_name_t requestor;

    /* setup to check common command line options that just report and die */
    memset(&orteprobe_globals, 0, sizeof(orteprobe_globals));
    cmd_line = OBJ_NEW(ompi_cmd_line_t);
    ompi_cmd_line_create(cmd_line, orte_cmd_line_opts);
    if (OMPI_SUCCESS != (ret = ompi_cmd_line_parse(cmd_line, true, 
                                                   argc, argv))) {
        return ret;
    }
    
    /* check for help and version requests */
    if (orteprobe_globals.help) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(cmd_line);
        ompi_show_help("help-orteprobe.txt", "orteprobe:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    if (orteprobe_globals.version) {
        /* show version message */
        printf("...showing off my version!\n");
        exit(1);
    }

    /*
     * Attempt to parse the probe's name and save in proc_info
     */
    if (orteprobe_globals.name_string) {
        ret = orte_ns_base_convert_string_to_process_name(
            &orte_process_info.my_name, orteprobe_globals.name_string);
        if(ORTE_SUCCESS != ret) {
            fprintf(stderr, "Couldn't convert environmental string to probe's process name\n");
            return 1;
        }
    }
    
    /* Open up the output streams */
    if (!ompi_output_init()) {
        return OMPI_ERROR;
    }

    /* 
     * If threads are supported - assume that we are using threads - and reset otherwise. 
     */
    ompi_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);

    /* For malloc debugging */
    ompi_malloc_init();

    /*
     * Initialize the MCA framework 
     */
    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        return ret;
    }

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
     * Initialize the data packing service.
     */
    if (ORTE_SUCCESS != (ret = orte_dps_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Open the name services to ensure access to local functions 
     */
    if (OMPI_SUCCESS != (ret = orte_ns_base_open())) {
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
    if (OMPI_SUCCESS != (ret = ompi_event_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Intialize the general progress engine
     */
    if (OMPI_SUCCESS != (ret = ompi_progress_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Internal startup
     */
    if (OMPI_SUCCESS != (ret = orte_wait_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Runtime Messaging Layer
     */
    if (OMPI_SUCCESS != (ret = orte_rml_base_open())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Runtime Messaging Layer
     */
    if (OMPI_SUCCESS != (ret = orte_rml_base_select())) {
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

    /* see if a universe already exists on this machine */
    if (ORTE_SUCCESS == (ret = orte_universe_exists(&univ))) {
        /* universe is here! send info back and die */
    } else {
        /* existing universe is not here or does not allow contact.
         * ensure we have a unique universe name, fork/exec an appropriate
         * daemon, and then tell whomever spawned us how to talk to the new
         * daemon
         */
    }
     
    /* cleanup */
    if (NULL != contact_path) {
	    unlink(contact_path);
    }
    if (NULL != log_path) {
        unlink(log_path);
    }
    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
    if (ORTE_SUCCESS != (ret = orte_dps.pack(&buffer, &ret, 1, ORTE_INT))) {
        ORTE_ERROR_LOG(ret);
        exit(1);
    }

    /*
     * Attempt to parse the requestor's name and contact info
     */
    if (orteprobe_globals.requestor_string) {
        if(ORTE_SUCCESS != (ret = orte_rml.parse_uris(
                    orteprobe_globals.requestor_string, &requestor, NULL))) {
            fprintf(stderr, "Couldn't parse environmental string for requestor's contact info\n");
            return 1;
        }
        /* set the contact info */
        if (ORTE_SUCCESS != (ret = orte_rml.set_uri(orteprobe_globals.requestor_string))) {
            fprintf(stderr, "Couldn't set contact info for requestor\n");
            return ret;
        }
    } else {
        fprintf(stderr, "No contact info received for requestor\n");
        return 1;
    }

    if (0 > orte_rml.send_buffer(&requestor, &buffer, ORTE_RML_TAG_PROBE, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_DESTRUCT(&buffer);
        return ORTE_ERR_COMM_FAILURE;
    }
    OBJ_DESTRUCT(&buffer);

    /* finalize the system */
    orte_finalize();

    exit(0);
}
