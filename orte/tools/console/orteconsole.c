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
/** @file **/

#include "orte_config.h"

#include <stdio.h>

#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/orte_constants.h"

#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/base.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/universe_setup_file_io.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/rds/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/pls/base/base.h"
#include "orte/runtime/orte_setup_hnp.h"

#include "orte/tools/console/orteconsole.h"

/*
 * Global Variables
 */
static bool exit_cmd;
static bool daemon_is_active;

/*
 * Globals for catching command line options
 */
orte_console_globals_t orte_console_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 'h', NULL, "help", 0,
      &orte_console_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    /* A Hostfile */
    { "rds", "hostfile", "path", '\0', "hostfile", "hostfile", 1,
      &orte_console_globals.hostfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },

    { "rds", "hostfile", "path", '\0', "machinefile", "machinefile", 1,
      &orte_console_globals.hostfile, OPAL_CMD_LINE_TYPE_STRING,
      "Provide a hostfile" },

    { "orte", "debug", NULL, 'd', NULL, "debug-devel", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of OpenRTE" },
    { "orte", "debug", "daemons", '\0', NULL, "debug-daemons", 0,
      NULL, OPAL_CMD_LINE_TYPE_INT,
      "Enable debugging of any OpenRTE daemons used by this application" },
    { "orte", "debug", "daemons_file", '\0', NULL, "debug-daemons-file", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Enable debugging of any OpenRTE daemons used by this application, storing output in files" },
    { "orte", "no_daemonize", NULL, '\0', NULL, "no-daemonize", 0,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Do not detach OpenRTE daemons used by this application" },
    { "universe", NULL, NULL, '\0', NULL, "universe", 1,
      NULL, OPAL_CMD_LINE_TYPE_STRING,
      "Set the universe name as username@hostname:universe_name for this application" },
    { NULL, NULL, NULL, '\0', NULL, "tmpdir", 1,
      &orte_process_info.tmpdir_base, OPAL_CMD_LINE_TYPE_STRING,
      "Set the root for the session directory tree for orterun ONLY" },
    { "orte", "universe", "exist", '\0', NULL, NULL, (int)false,
      NULL, OPAL_CMD_LINE_TYPE_BOOL,
      "Report error if universe does not already exist" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL,
      NULL }
};

/*
 * Global structure describing valid internal commands
 */
orte_console_command_t console_commands[] = {
    { "add", NULL, 1, ORTE_CONSOLE_TYPE_STD,
      orte_console_add_host,
      "add <hostname> [<hostname> <hostname> ...]",
      "Add a host to the current universe" },

    { "alias", NULL, 0, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "alias [<name> <cmd> <arg>]",
      "Alias command" },

    { "boot", "b", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_boot_daemons,
      "boot [hostname] [username]",
      "Launch Persistant Daemons. This will use the specifiec host or the first host added." },

    { "clean", "cl", 0, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "clean",
      "Kill all jobs in the universe, preserving all daemons" },

    { "conf", NULL, 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_display_configuration,
      "conf [-a]",
      "Diplay a list of the machines in the current universe" },

    { "contactinfo", "ci", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_contactinfo,
      "contactinfo",
      "Query Contact Information from Daemons" },

    { "cwd", NULL, 0, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "cwd [name]",
      "Set or print the current working directory" },

    { "delete", "del", 1, ORTE_CONSOLE_TYPE_STD,
      orte_console_remove_host,
      "delete <hostid> [<hostid> <hostid> ...]",
      "Delete a host from the current universe" },

    { "exit", "e", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_exit,
      "exit",
      "Exit the console" },

    { "expire", NULL, 1, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "expire <age (seconds)>",
      "Expire process information" },

    { "halt", NULL, 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_halt,
      "halt",
      "Halt virtual machine" },

    { "haltall", NULL, 0, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "haltall",
      "Halt virtual machine and stop all services" },

    { "help", "h", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_help,
      "help [command]",
      "Print this display" },

#ifdef HAVE_READLINE
    { "history", NULL, 0, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "history",
      "Display list of command history" },
#endif

    { "kill", NULL, 1, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "kill <GID> [<GID> <GID> ...]",
      "Terminate process(es)" },

    { "killall", NULL, 1, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "killall <RunID> [<RunID> <RunID> ...]",
      "Terminate all process(es) in runID" },

    { "mpispawn", NULL, 3, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "mpispawn -np <number of process> [ <-option name> [option argument] ] <process name>",
      "Spawn MPI process(es)" },

    { "ps", NULL, 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_ps,
      "ps",
      "Display process(es) status" },

    { "quit", "q", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_exit,
      "quit",
      "Quit from console" },

    { "reset", NULL, 0, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "reset",
      "Kill all tasks" },

    { "service", NULL, 2, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "service [service_name] [operation]",
      "Service management" },

    { "sig", NULL, 2, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "sig <signum> <GID> [<GID> <GID> ...]",
      "Send signal to process(es)" },

    { "sigall", NULL, 2, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "sigall <signum> <RunID> [<RunID> <RunID> ...]",
      "Send signal to all process(es) in runID" },

    { "spawn", NULL, 3, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "spawn -np <number of process> [ <-option name> [option argument] ] <process name>",
      "Spawn process(es)" },

    { "unalias", NULL, 1, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "unalias <name>",
      "unalias command" },

    { "vmname", NULL, 0, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_not_imp,
      "vmname [name]",
      "Set or print the current virtual machine name" },

    { "dump", NULL, 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_dump,
      "dump [arg1 arg2]",
      "Dump registry data - [all, segment, triggers, subscriptions] [segment_name]" },

    /* End of list */
    { NULL, NULL, 0, ORTE_CONSOLE_TYPE_NULL,
      NULL,
      NULL,
      NULL }
};

/* This should be added to opal_list.c ??? JJH */
static int opal_list_clear(opal_list_t *list) {
    opal_list_item_t *item;

    while ( NULL != (item = opal_list_remove_first(list) ) ) {
        OBJ_RELEASE(item);
    }

    return ORTE_SUCCESS;
}

int main(int argc, char *argv[])
{
    int ret=0;
    opal_cmd_line_t *cmd_line;
    char *usercmd;
    orte_console_input_command_t input_command;

    /*
     * Setup to check common command line options
     */
    memset(&orte_console_globals, 0, sizeof(orte_console_globals_t));
    cmd_line = OBJ_NEW(opal_cmd_line_t);
    opal_cmd_line_create(cmd_line, cmd_line_opts);
    if (ORTE_SUCCESS != (ret = opal_cmd_line_parse(cmd_line, false,
                                                   argc, argv))) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        opal_show_help("help-orteconsole.txt", "orteconsole:usage", false,
                       argv[0], args);
        free(args);
        return ret;
    }

    /* Check for help request */
    if ( orte_console_globals.help ) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        opal_show_help("help-orteconsole.txt", "orteconsole:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    opal_show_help("help-orteconsole.txt", "orteconsole:splash-screen", false);

    /*
     * Intialize the ORTE environment
     */
    /* Set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */
    daemon_is_active = true;
    if (ORTE_SUCCESS != (ret = orte_init(ORTE_INFRASTRUCTURE, ORTE_NON_BARRIER)) ) {
        if (ORTE_ERR_UNREACH == ret) {
            opal_output(0, "Specified universe could not be reached - please ensure it has been started\n");
            return ret;
        } else {
            opal_show_help("help-orteconsole.txt", "orteconsole:init-failure", false,
                           "orte_init()", ret);
            return ret;
        }
    }
    if (orte_process_info.seed) {
        daemon_is_active = false;
    }
    
    /*
     * Work Loop
     */
    OBJ_CONSTRUCT(&orte_console_hosts, opal_list_t);
    if (NULL != orte_ras.node_query) orte_ras.node_query(&orte_console_hosts);

    exit_cmd = false;
    memset(&input_command, 0, sizeof(orte_console_input_command_t));
    while ( !exit_cmd ) {
        usercmd = orte_console_get_input_line();
        if (NULL == usercmd || 0 >= strlen(usercmd) ) {
            continue;
        }

        orte_console_parse_command(usercmd, &input_command);

        orte_console_execute_command(input_command);
    }

    OBJ_DESTRUCT(&orte_console_hosts);

    /*
     * Finialize ORTE Environment
     */
    if ( ORTE_SUCCESS != (ret = orte_finalize()) ) {
        opal_show_help("help-orteconsole.txt", "orteconsole:finalize-failure", false,
                       "orte_finalize()", ret);
        return ret;
    }

    return ORTE_SUCCESS;
}

static int command_cmp(char* user_command, orte_console_command_t system_command) {

    /*
     * Check for Full Name Match
     */
    if ( 0 == strncmp(user_command, system_command.cmd_full_name,
                     strlen(system_command.cmd_full_name)) ) {
        return 0;
    }
    /*
     * Check for Short Name Match
     */
    else if ( ( NULL != system_command.cmd_short_name )                         &&
              ( strlen(user_command) == strlen(system_command.cmd_short_name) ) &&
              ( 0 == strncmp(user_command, system_command.cmd_short_name,
                             strlen(system_command.cmd_short_name))           ) ) {
        return 0;
    }

    return -1;
}

static int orte_console_execute_command(orte_console_input_command_t input_command) {
    orte_console_command_t *cur_cmd;
    int i, ret;

    for (i = 0; console_commands[i].cmd_type != ORTE_CONSOLE_TYPE_NULL; ++i) {
        cur_cmd = &console_commands[i];

        /* Check for matching command */
        if ( 0 == command_cmp(input_command.cmd_name, *cur_cmd) ){
            /* Check number of arguments */
            if (input_command.argc < (cur_cmd->cmd_args+1)) {
                opal_show_help("help-orteconsole.txt", "orteconsole:invalid-num-arguments", false,
                               input_command.cmd_name, cur_cmd->cmd_args,
                               input_command.argc,     cur_cmd->cmd_full_name);
                return ORTE_ERROR;
            }

            ret = cur_cmd->cmd_function(input_command);

            /* Check Return Codes */
            if ( ORTE_ERR_NOT_IMPLEMENTED == ret ) {
                opal_show_help("help-orteconsole.txt", "orteconsole:unimplemented-command", false,
                               cur_cmd->cmd_full_name);
                return ret;
            }
            else if ( ORTE_SUCCESS != ret ) {
                opal_show_help("help-orteconsole.txt", "orteconsole:failed-command", false,
                               cur_cmd->cmd_full_name, ret);
                return ret;
            }

            break;
        }
    }

    /*
     * If command was not found :(
     */
    if ( ORTE_CONSOLE_TYPE_NULL == console_commands[i].cmd_type ) {
        opal_show_help("help-orteconsole.txt", "orteconsole:unknown-command", false,
                       input_command.cmd_name);
        return ORTE_ERR_NOT_IMPLEMENTED;
    }

    return ORTE_SUCCESS;
}

static int   orte_console_parse_command(char * usercmd, orte_console_input_command_t *input_command){

    input_command->argv     = opal_argv_split(usercmd, ' ');
    input_command->argc     = opal_argv_count(input_command->argv);
    input_command->cmd_name = strdup(input_command->argv[0]);

    return ORTE_SUCCESS;
}

/* ===========================
 * Actual Functionality below
 * =========================== */

static int orte_console_not_imp(orte_console_input_command_t input_command) {
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int orte_console_dump(orte_console_input_command_t input_command) {
    int i, j;
    
    if(daemon_is_active) {
        if (NULL == input_command.argv[1]) { /** default to dump_all */
            orte_gpr.dump_all();
        } else if (strcmp(input_command.argv[1], "segment") == 0) {
            if (2 < input_command.argc) {  /** specific segment was requested */
                for (i=2; i < input_command.argc; i++) orte_gpr.dump_segment(input_command.argv[i]);
            } else { /** nothing specific - dump them all */
                orte_gpr.dump_segment(NULL);
            }
        } else if (strcmp(input_command.argv[1], "trigger") == 0) {
            if (2 < input_command.argc) {  /** specific trigger was requested */
                j = strtol(input_command.argv[2], NULL, 10);
                orte_gpr.dump_triggers(j);
            } else { /** nothing specific - dump them all */
                orte_gpr.dump_triggers(0);
            }
        } else if (strcmp(input_command.argv[1], "subs") == 0) {
            if (2 < input_command.argc) {  /** specific subscription was requested */
                j = strtol(input_command.argv[2], NULL, 10);
                orte_gpr.dump_subscriptions(j);
            } else { /** nothing specific - dump them all */
                orte_gpr.dump_subscriptions(0);
           }
        } else if (strcmp(input_command.argv[1], "callbacks") == 0) {
                orte_gpr.dump_callbacks();
        } else if (strcmp(input_command.argv[1], "cells") == 0) {
                orte_ns.dump_cells();
        } else if (strcmp(input_command.argv[1], "jobs") == 0) {
                orte_ns.dump_jobs();
        } else if (strcmp(input_command.argv[1], "tags") == 0) {
                orte_ns.dump_tags();
        } else if (strcmp(input_command.argv[1], "datatypes") == 0) {
                orte_ns.dump_datatypes();
        } else {
            /** let user know that this isn't recognized */
            opal_output(0, "orteconsole: specified dump option not recognized\n");
        }    
    } else {
        /** let user know that this isn't available */
       opal_output(0, "orteconsole: no daemon is active - dump cannot be executed\n");
    }

    return ORTE_SUCCESS;
}

static int orte_console_ps(orte_console_input_command_t input_command) {
    if(daemon_is_active) {
        /** find the jobids in the system */
        /** for each jobid, get its status and output the info */
        /** no real way to do this right now - need the 2.0 interface
         * so let's just punt for the moment 
         */
        orte_ns.dump_jobs();
    } else {
        /** let user know that this isn't available */
       opal_output(0, "orteconsole: no daemon is active - ps cannot be executed\n");
    }

    return ORTE_SUCCESS;
}

static int add_hosts_to_registry(opal_list_t *updates) {
    orte_rds_cell_desc_t *rds_item;
    orte_rds_cell_attr_t *new_attr;
    orte_ras_node_t *ras_item;
    opal_list_item_t *item;
    opal_list_t rds_updates;
    int ret;
    orte_cellid_t local_cellid;
    bool need_cellid = true;

    OBJ_CONSTRUCT(&rds_updates, opal_list_t);

    /* Convert RAS list to RDS list */
    for ( item  = opal_list_get_first(updates);
          item != opal_list_get_end(  updates);
          item  = opal_list_get_next( item)) {
        ras_item = (orte_ras_node_t *) item;

        rds_item = OBJ_NEW(orte_rds_cell_desc_t);
        if (NULL == rds_item) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        rds_item->site  = strdup("Console");
        rds_item->name  = strdup(ras_item->node_name);

        if(need_cellid) {
#if 0 /* JJH Repair when cellid's are fixed */
            /* Create a new cellid */
            ret = orte_ns.create_cellid(&local_cellid, rds_item->site, rds_item->name);
            if (ORTE_SUCCESS != ret) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
#else
            local_cellid = 0;
#endif
        }
        rds_item->cellid      = local_cellid;
        ras_item->node_cellid = local_cellid;

        new_attr = OBJ_NEW(orte_rds_cell_attr_t);
        if (NULL == new_attr) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        new_attr->keyval.value = OBJ_NEW(orte_data_value_t);
        if (NULL == new_attr->keyval.value) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        new_attr->keyval.key          = strdup(ORTE_RDS_NAME);
        new_attr->keyval.value->type         = ORTE_STRING;
        new_attr->keyval.value->data = strdup(ras_item->node_name);
        opal_list_append(&(rds_item->attributes), &new_attr->super);

        new_attr = OBJ_NEW(orte_rds_cell_attr_t);
        if (NULL == new_attr) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        new_attr->keyval.value = OBJ_NEW(orte_data_value_t);
        if (NULL == new_attr->keyval.value) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        new_attr->keyval.key          = strdup(ORTE_CELLID_KEY);
        new_attr->keyval.value->type         = ORTE_CELLID;
        if (ORTE_SUCCESS != (ret = orte_dss.copy((void**)&(new_attr->keyval.value->data), &(rds_item->cellid), ORTE_CELLID))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        opal_list_append(&(rds_item->attributes), &new_attr->super);

        opal_list_append(&rds_updates,  &rds_item->super);
    }

    /* Add the hosts to the registry */
    ret = orte_rds.store_resource(&rds_updates);
    if (ORTE_SUCCESS != ret) {
        return ret;
    }

    ret = orte_ras.node_insert(updates);
    if (ORTE_SUCCESS != ret ) {
        return ret;
    }

    opal_list_clear(&rds_updates);
    OBJ_DESTRUCT(&rds_updates);

    return ORTE_SUCCESS;
}

static int remove_hosts_from_registry(opal_list_t *updates) {
    opal_list_t rds_updates;
    /* int ret; */

    OBJ_CONSTRUCT(&rds_updates, opal_list_t);

    /* Add the hosts to the registry */
#if 0 /* This functionality needs to be written */
    orte_rds_base_convert_ras_to_rds(updates, &rds_updates);

    ret = orte_rds_base_node_delete(&rds_updates);
    if (ORTE_SUCCESS != ret) {
        return ret;
    }

    ret = orte_ras.node_delete(updates);
    if (ORTE_SUCCESS != ret ) {
        return ret;
    }
#endif

    opal_list_clear(&rds_updates);
    OBJ_DESTRUCT(&rds_updates);

    return ORTE_SUCCESS;
}


static int orte_console_add_host(orte_console_input_command_t input_command) {
    int i, ret;
    orte_ras_node_t *tmp_host;
    opal_list_t hosts_to_add;

    OBJ_CONSTRUCT(&hosts_to_add, opal_list_t);

    for(i = 1; i < input_command.argc; ++i) {
        tmp_host = OBJ_NEW(orte_ras_node_t);
        if (NULL == tmp_host) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        tmp_host->node_name        = strdup(input_command.argv[i]);
        tmp_host->node_arch        = strdup("unknown");
        tmp_host->node_cellid      = 0; /* JJH Repair when cellid's are fixed */
        tmp_host->node_slots_inuse = 0;
        tmp_host->node_slots_max   = 1;
        tmp_host->node_slots       = 1;

        if (daemon_is_active) {
            opal_list_append(&hosts_to_add,       &tmp_host->super);
        }
        else {
            opal_list_append(&orte_console_hosts, &tmp_host->super);
        }

        printf("Added Host: <%s>\n", input_command.argv[i]);
    }

    if ( !opal_list_is_empty(&hosts_to_add) && daemon_is_active) {
        /*
         * If there is an active daemon, then add to the registry
         */
        ret = add_hosts_to_registry(&hosts_to_add);
        if (ORTE_SUCCESS != ret) {
            return ret;
        }

        /* Get a new list of registered hosts */
        opal_list_clear(&orte_console_hosts);
        ret = orte_ras.node_query(&orte_console_hosts);
        if (ORTE_SUCCESS != ret) {
            return ret;
        }
    }

    opal_list_clear(&hosts_to_add);
    OBJ_DESTRUCT(&hosts_to_add);

    return ORTE_SUCCESS;
}

static int orte_console_remove_host(orte_console_input_command_t input_command) {
    int i, ret;
    orte_ras_node_t *tmp_host;
    opal_list_item_t *item;
    opal_list_t hosts_to_remove;
    bool done;

    OBJ_CONSTRUCT(&hosts_to_remove, opal_list_t);

    for(i = 1; i < input_command.argc; ++i) {
        done = false;
        for (item  = opal_list_get_first(&orte_console_hosts);
             item != opal_list_get_end(  &orte_console_hosts);
             item  = opal_list_get_next( item)) {
            tmp_host = (orte_ras_node_t *)item;

            if (0 == strcmp(tmp_host->node_name, input_command.argv[i])) {
                opal_list_remove_item(&orte_console_hosts, item);
                opal_list_append(&hosts_to_remove, item);
                done = true;
                break;
            }
        }
        if(!done) {
            printf("Could not find host <%s>\n", input_command.argv[i]);
        }
    }

    if ( !opal_list_is_empty(&hosts_to_remove) && daemon_is_active) {
        /* Delete hosts from registry */
        ret = remove_hosts_from_registry(&hosts_to_remove);
        if (ORTE_SUCCESS != ret) {
            return ret;
        }

        /* Get a new list of registered hosts */
        opal_list_clear(&orte_console_hosts);
        ret = orte_ras.node_query(&orte_console_hosts);
        if (ORTE_SUCCESS != ret) {
            return ret;
        }
    }

    opal_list_clear(&hosts_to_remove);
    OBJ_DESTRUCT(&hosts_to_remove);

    return ORTE_SUCCESS;
}

static int orte_console_display_configuration(orte_console_input_command_t input_command) {
    orte_ras_node_t *tmp_host;
    opal_list_item_t *item;
    int i;

    if ( opal_list_is_empty(&orte_console_hosts) ) {
        opal_show_help("help-orteconsole.txt", "orteconsole:no-hosts", false);
        return ORTE_SUCCESS;
    }

    printf("%6s %15s %10s %13s %15s\n", "Index",
           "Hostname", "CPU(s)",
           "CPU(s) Used", "Arch");
    for (item  = opal_list_get_first(&orte_console_hosts), i = 0;
         item != opal_list_get_end(  &orte_console_hosts);
         item  = opal_list_get_next( item), ++i) {
        tmp_host = (orte_ras_node_t *)item;
        printf("%6d %15s %10lu %13lu %15s\n", i,
               tmp_host->node_name, (unsigned long)tmp_host->node_slots,
               (unsigned long)tmp_host->node_slots_inuse,
               tmp_host->node_arch);
    }

    return ORTE_SUCCESS;
}

static int orte_console_boot_daemons(orte_console_input_command_t input_command) {
    int rc, id;
    orte_ras_node_t *item;
    char *remote_daemon;
    char *username = NULL;

    if ( opal_list_is_empty(&orte_console_hosts) && 1 >= input_command.argc ) {
        opal_show_help("help-orteconsole.txt", "orteconsole:no-hosts", false);
        return ORTE_ERROR;
    }

    /* If hostname supplied on command line use it */
    if ( 1 < input_command.argc) {
        remote_daemon = strdup(input_command.argv[1]);
    }
    /* Otherwise get first node in list to serve as the primary daemon */
    else {
        item = (orte_ras_node_t *)opal_list_get_first(&orte_console_hosts);
        remote_daemon = strdup(item->node_name);
    }

    printf("Launching Remote Daemon on \"%s\"", remote_daemon);

    /* If they supplied a username then use that,
       otherwise assume same username as on the console system */
    if ( 2 < input_command.argc) {
        username = strdup(input_command.argv[2]);
        printf(" Username \"%s\"\n", username);
    }
    else {
        username = NULL;
        printf("\n");
    }

    /* Create the persistent daemon */
    id = mca_base_param_register_int("persistent",NULL,NULL,NULL,(int)false);
    mca_base_param_set_int(id, (int)true);

    rc = orte_setup_hnp(NULL, remote_daemon, username);
    if ( ORTE_SUCCESS != rc) {
        printf("Open RTE Boot: Failed!\n");
        return rc;
    }

    printf("Open RTE Boot: Successful!\n");
    daemon_is_active = true;

    return ORTE_SUCCESS;
}

static int orte_console_halt(orte_console_input_command_t input_command) {
    int ret;

    if(!daemon_is_active) {
        opal_show_help("help-orteconsole.txt", "orteconsole:no-daemon-started", false);
        return ORTE_SUCCESS;
    }

    ret = orte_console_send_command(ORTE_DAEMON_EXIT_CMD);
    if (ORTE_SUCCESS != ret) {
        return ret;
    }

    return ORTE_SUCCESS;
}

static int orte_console_exit(orte_console_input_command_t input_command) {
    exit_cmd = true;

    return ORTE_SUCCESS;
}

static int orte_console_help(orte_console_input_command_t input_command) {
    orte_console_command_t *cur_cmd;
    int i;

    /*
     * Generic Help
     */
    if ( input_command.argc <= 1 ) {
        printf("Open RTE Console Commands:\n\n");

        for (i = 0; console_commands[i].cmd_type != ORTE_CONSOLE_TYPE_NULL; ++i) {
            cur_cmd = &console_commands[i];
            if ( ORTE_CONSOLE_TYPE_HIDDEN != cur_cmd->cmd_type ) {
                printf("%15s ", cur_cmd->cmd_full_name);
                if ( NULL == cur_cmd->cmd_short_name ) {
                    printf("         ");
                }
                else {
                    printf(" | %5s ", cur_cmd->cmd_short_name);
                }
                printf("\t%s\n", cur_cmd->cmd_description);
            }
        }

        printf("\n");
    }
    /*
     * Specific Help Message for a Command
     */
    else {
        for(i = 0; console_commands[i].cmd_type != ORTE_CONSOLE_TYPE_NULL; ++i) {
            cur_cmd = &console_commands[i];

            if ( 0 == command_cmp(input_command.argv[1], *cur_cmd) ){
                printf("Command:\n");
                printf("\t%s ", cur_cmd->cmd_full_name);
                if ( NULL != cur_cmd->cmd_short_name ) {
                    printf(" | %5s", cur_cmd->cmd_short_name);
                }
                printf("\n");

                if ( NULL != cur_cmd->cmd_usage ) {
                    printf("Usage:\n");
                    printf("\t%s\n", cur_cmd->cmd_usage);
                }

                printf("Description:\n");
                printf("\t%s\n", cur_cmd->cmd_description);

                break;
            }
        }

        /*
         * Command Not Found
         */
        if( ORTE_CONSOLE_TYPE_NULL == console_commands[i].cmd_type ) {
            opal_show_help("help-orteconsole.txt", "orteconsole:unknown-command", false,
                           input_command.argv[1]);
            return ORTE_SUCCESS;
        }

        printf("\n");
    }

    return ORTE_SUCCESS;
}

/*
 * Get the contact information for the remote daemon
 */
static int orte_console_contactinfo(orte_console_input_command_t input_command) {
    char * str_response;
    orte_buffer_t *buffer = NULL;
    orte_process_name_t seed={0,0,0};
    int ret;
    orte_std_cntr_t n;

    if(!daemon_is_active) {
        opal_show_help("help-orteconsole.txt", "orteconsole:no-daemon-started", false);
        return ORTE_SUCCESS;
    }

    /** initialize the buffer */
    buffer = OBJ_NEW(orte_buffer_t);
    
    /* Start the exchange */
    ret = orte_console_send_command(ORTE_DAEMON_CONTACT_QUERY_CMD);
    if (ORTE_SUCCESS != ret ){
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    ret = orte_rml.recv_buffer(&seed, buffer, ORTE_RML_TAG_DAEMON);
    if ( 0 > ret) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    n = 1;
    ret = orte_dss.unpack(buffer, &str_response, &n, ORTE_STRING);
    if ( ORTE_SUCCESS != ret ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    printf(str_response);
    printf("\n");
    
    /** cleanup the buffer */
    OBJ_RELEASE(buffer);

    return ORTE_SUCCESS;
}

/*
 * Send a command to the remote daemon
 */
static int orte_console_send_command(orte_daemon_cmd_flag_t usercmd)
{
    orte_buffer_t *cmd;
    orte_daemon_cmd_flag_t command;
    orte_process_name_t    seed = {0,0,0};
    int rc;

    if(!daemon_is_active) {
        opal_show_help("help-orteconsole.txt", "orteconsole:no-daemon-started", false);
        return ORTE_SUCCESS;
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    command = usercmd;

    rc = orte_dss.pack(cmd, &command, 1, ORTE_DAEMON_CMD);
    if ( ORTE_SUCCESS != rc ) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
        return rc;
    }

    rc = orte_rml.send_buffer(&seed, cmd, ORTE_RML_TAG_DAEMON, 0);
    if ( 0 > rc ) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(cmd);
        return ORTE_ERR_COMM_FAILURE;
    }

    OBJ_RELEASE(cmd);

    return ORTE_SUCCESS;
}

static char *orte_console_get_input_line(void)
{
    char *ret, *buff;
    char input[ORTE_CONSOLE_MAX_LINE_LENGTH];

    printf("orteconsole> ");

    ret = fgets(input, ORTE_CONSOLE_MAX_LINE_LENGTH, stdin);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        buff = strdup(input);
        return buff;
    }

    return NULL;
}
