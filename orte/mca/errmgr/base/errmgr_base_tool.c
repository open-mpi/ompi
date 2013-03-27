/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif /* HAVE_DIRENT_H */
#include <time.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/basename.h"
#include "opal/util/argv.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

/**
 * This file contains function for the HNP to communicate with the
 * orte-migrate command.
 */
#if OPAL_ENABLE_FT_CR

/******************
 * Local Functions
 ******************/
static int  errmgr_base_tool_start_cmdline_listener(void);
static int  errmgr_base_tool_stop_cmdline_listener(void);

static void errmgr_base_tool_cmdline_recv(int status,
                                         orte_process_name_t* sender,
                                         opal_buffer_t* buffer,
                                         orte_rml_tag_t tag,
                                         void* cbdata);

/******************
 * Object stuff
 ******************/
static orte_process_name_t errmgr_cmdline_sender = {ORTE_JOBID_INVALID, ORTE_VPID_INVALID};
static bool errmgr_cmdline_recv_issued = false;
static int errmgr_tool_initialized = false;

/********************
 * Module Functions
 ********************/
int orte_errmgr_base_tool_init(void)
{
    int ret;

    if( (++errmgr_tool_initialized) != 1 ) {
        if( errmgr_tool_initialized < 1 ) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }

    /* Only HNP communicates with tools */
    if (! ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    /*
     * Setup command line migrate tool request listener
     */
    if( ORTE_SUCCESS != (ret = errmgr_base_tool_start_cmdline_listener()) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_base_tool_finalize(void)
{
    int ret;

    if( (--errmgr_tool_initialized) != 0 ) {
        if( errmgr_tool_initialized < 0 ) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }

    /* Only HNP communicates with tools */
    if (! ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    /*
     * Clean up listeners
     */
    if( ORTE_SUCCESS != (ret = errmgr_base_tool_stop_cmdline_listener()) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_base_migrate_update(int status)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t *loc_buffer = NULL;
    orte_errmgr_tool_cmd_flag_t command = ORTE_ERRMGR_MIGRATE_TOOL_UPDATE_CMD;

    /* Only HNP communicates with tools */
    if (! ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    /*
     * If this is an invalid state, then return an error
     */
    if( ORTE_ERRMGR_MIGRATE_MAX < status ) {
        opal_output(orte_errmgr_base_framework.framework_output,
                    "errmgr:base:tool:update() Error: Invalid state %d < (Max %d)",
                    status, ORTE_ERRMGR_MIGRATE_MAX);
        return ORTE_ERR_BAD_PARAM;
    }

    /*
     * Report the status over the notifier interface
     */
    orte_errmgr_base_migrate_state_notify(status);

    /*
     * If the caller is indicating that they are finished and ready for another
     * command, then repost the RML listener.
     */
    if( ORTE_ERRMGR_MIGRATE_STATE_NONE == status ) {
        if( ORTE_SUCCESS != (ret = errmgr_base_tool_start_cmdline_listener()) ) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        return ORTE_SUCCESS;
    }

    /*
     * Noop if invalid peer, or peer not specified
     */
    if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_NAME_INVALID, &errmgr_cmdline_sender) ) {
        return ORTE_SUCCESS;
    }

    /*
     * Do not send to self, as that is silly.
     */
    if( OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_HNP, &errmgr_cmdline_sender) ) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_framework.framework_output,
                             "errmgr:base:tool:update() Warning: Do not send to self!\n"));
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_framework.framework_output,
                         "errmgr:base:tool:update() Sending update command <status %d>\n",
                         status));

    /********************
     * Send over the status of the checkpoint
     * - migration state
     ********************/
    if (NULL == (loc_buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(loc_buffer, &command, 1, ORTE_ERRMGR_MIGRATE_TOOL_CMD)) ) {
        opal_output(orte_errmgr_base_framework.framework_output,
                    "errmgr:base:tool:update() Error: DSS Pack (cmd) Failure (ret = %d)\n",
                    ret);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(loc_buffer, &status, 1, OPAL_INT))) {
        opal_output(orte_errmgr_base_framework.framework_output,
                    "errmgr:base:tool:update() Error: DSS Pack (status) Failure (ret = %d)\n",
                    ret);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(&errmgr_cmdline_sender, loc_buffer, ORTE_RML_TAG_MIGRATE, 0))) {
        opal_output(orte_errmgr_base_framework.framework_output,
                    "errmgr:base:tool:update() Error: Send (status) Failure (ret = %d)\n",
                    ret);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if(NULL != loc_buffer) {
        OBJ_RELEASE(loc_buffer);
        loc_buffer = NULL;
    }

    return exit_status;
}

/********************
 * Utility functions
 ********************/

/********************
 * Local Functions
 ********************/
static int errmgr_base_tool_start_cmdline_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    if (errmgr_cmdline_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                         "errmgr:base:tool: Startup Command Line Channel"));

    /*
     * Coordinator command listener
     */
    errmgr_cmdline_sender.jobid = ORTE_JOBID_INVALID;
    errmgr_cmdline_sender.vpid  = ORTE_VPID_INVALID;
    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                       ORTE_RML_TAG_MIGRATE,
                                                       0,
                                                       errmgr_base_tool_cmdline_recv,
                                                       NULL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    errmgr_cmdline_recv_issued = true;

 cleanup:
    return exit_status;
}


static int errmgr_base_tool_stop_cmdline_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    
    if (!errmgr_cmdline_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                         "errmgr:base:tool: Shutdown Command Line Channel"));
    
    if (ORTE_SUCCESS != (ret = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                    ORTE_RML_TAG_MIGRATE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    errmgr_cmdline_recv_issued = false;
    
 cleanup:
    return exit_status;
}

/*****************
 * Listener Callbacks
 *****************/
static void errmgr_base_tool_cmdline_recv(int status,
                                         orte_process_name_t* sender,
                                         opal_buffer_t* buffer,
                                         orte_rml_tag_t tag,
                                         void* cbdata)
{
    int ret;
    orte_process_name_t swap_dest;
    orte_errmgr_tool_cmd_flag_t command;
    orte_std_cntr_t count = 1;
    char *off_nodes  = NULL;
    char *off_procs  = NULL;
    char *onto_nodes = NULL;
    char **split_off_nodes  = NULL;
    char **split_off_procs  = NULL;
    char **split_onto_nodes = NULL;
    opal_list_t *proc_list = NULL;
    opal_list_t *node_list = NULL;
    opal_list_t *suggested_map_list = NULL;
    orte_errmgr_predicted_proc_t *off_proc = NULL;
    orte_errmgr_predicted_node_t *off_node = NULL;
    orte_errmgr_predicted_map_t  *onto_map = NULL;
    int cnt = 0, i;


    if( ORTE_RML_TAG_MIGRATE != tag ) {
        opal_output(orte_errmgr_base_framework.framework_output,
                    "errmgr:base:tool:recv() Error: Unknown tag: Received a command message from %s (tag = %d).",
                    ORTE_NAME_PRINT(sender), tag);
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return;
    }

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_framework.framework_output,
                         "errmgr:base:tool:recv() Command Line: Start a migration operation [Sender = %s]",
                         ORTE_NAME_PRINT(sender)));

    errmgr_cmdline_recv_issued = false; /* Not a persistent RML message */

    /*
     * If we are already interacting with a command line tool then reject this
     * request. Since we only allow the processing of one tool command at a
     * time.
     */
    if( OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_NAME_INVALID, &errmgr_cmdline_sender) ) {
        swap_dest.jobid = errmgr_cmdline_sender.jobid;
        swap_dest.vpid  = errmgr_cmdline_sender.vpid;

        errmgr_cmdline_sender = *sender;
        orte_errmgr_base_migrate_update(ORTE_ERRMGR_MIGRATE_STATE_ERR_INPROGRESS);

        errmgr_cmdline_sender.jobid = swap_dest.jobid;
        errmgr_cmdline_sender.vpid  = swap_dest.vpid;

        return;
    }

    errmgr_cmdline_sender = *sender;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &count, ORTE_ERRMGR_MIGRATE_TOOL_CMD))) {
        ORTE_ERROR_LOG(ret);
        return;
    }

    /*
     * orte-migrate has requested that a checkpoint be taken
     */
    if (ORTE_ERRMGR_MIGRATE_TOOL_INIT_CMD == command) {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_framework.framework_output,
                             "errmgr:base:tool:recv() Command line requested process migration [command %d]\n",
                             command));

        /*
         * Unpack the buffer from the orte-migrate command
         */
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(off_procs), &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
	    return;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(off_nodes), &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            return;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(onto_nodes), &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            return;
        }

        /*
         * Parse the comma separated list
         */
        proc_list = OBJ_NEW(opal_list_t);
        node_list = OBJ_NEW(opal_list_t);
        suggested_map_list = OBJ_NEW(opal_list_t);

        split_off_procs     = opal_argv_split(off_procs,  ',');
        cnt = opal_argv_count(split_off_procs);
        if( cnt > 0 ) {
            for(i = 0; i < cnt; ++i) {
                off_proc = OBJ_NEW(orte_errmgr_predicted_proc_t);
                off_proc->proc_name.vpid = atoi(split_off_procs[i]);
                opal_list_append(proc_list, &(off_proc->super));
            }
        }

        split_off_nodes     = opal_argv_split(off_nodes,  ',');
        cnt = opal_argv_count(split_off_nodes);
        if( cnt > 0 ) {
            for(i = 0; i < cnt; ++i) {
                off_node = OBJ_NEW(orte_errmgr_predicted_node_t);
                off_node->node_name = strdup(split_off_nodes[i]);
                opal_list_append(node_list, &(off_node->super));
            }
        }

        split_onto_nodes    = opal_argv_split(onto_nodes, ',');
        cnt = opal_argv_count(split_onto_nodes);
        if( cnt > 0 ) {
            for(i = 0; i < cnt; ++i) {
                onto_map = OBJ_NEW(orte_errmgr_predicted_map_t);
                onto_map->map_node_name = strdup(split_onto_nodes[i]);
                opal_list_append(suggested_map_list, &(onto_map->super));
            }
        }

        /*
         * Pass to the predicted fault function to see how they would like to progress
         */
        orte_errmgr.predicted_fault(proc_list, node_list, suggested_map_list);
    }
    /*
     * Unknown command
     */
    else {
        OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base_framework.framework_output,
                             "errmgr:base:tool:recv() Command line sent an unknown command (command %d)\n",
                             command));
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
    }

    return;
}
#endif
