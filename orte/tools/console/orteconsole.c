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
/** @file **/

#include "orte_config.h"

#include <stdio.h>

#include "include/orte_constants.h"
#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "dps/dps.h"

#include "util/sys_info.h"
#include "opal/util/cmd_line.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "opal/util/output.h"
#include "opal/util/os_path.h"
#include "opal/util/show_help.h"
#include "util/universe_setup_file_io.h"
#include "runtime/runtime.h"

#include "mca/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/pls/base/base.h"
#include "tools/orted/orted.h"

#include "tools/console/orteconsole.h"

/*
 * Global Variables
 */
static bool exit_cmd;

/*
 * Globals for catching command line options
 */
orteconsole_globals_t orteconsole_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 'h', NULL, "help", 0, 
      &orteconsole_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
      "This help message" },

    /* End of list */
    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
      NULL, OPAL_CMD_LINE_TYPE_NULL, NULL }
};

/*
 * Global structure describing valid internal commands
 */
orte_console_command_t console_commands[] = {
    { "quit", "q", 0, ORTE_CONSOLE_TYPE_STD, 
      orte_console_exit,
      "Exit the console" },

    { "help", "h", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_help,
      "Print this display" },

    { "contactinfo", "ci", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_contactinfo,
      "Query Contact Information from Daemons" },

    { "dumpvm", "vm", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_dumpvm,
      "Get VM List from daemons" },

    /* End of list */
    { NULL, NULL, 0, ORTE_CONSOLE_TYPE_NULL,
      NULL,
      NULL }
};

int main(int argc, char *argv[])
{
    int ret=0;
    opal_cmd_line_t *cmd_line;
    char *usercmd;

    /* 
     * Setup to check common command line options
     */
    memset(&orteconsole_globals, 0, sizeof(orteconsole_globals_t));
    cmd_line = OBJ_NEW(opal_cmd_line_t);
    opal_cmd_line_create(cmd_line, cmd_line_opts);
    if (OMPI_SUCCESS != (ret = opal_cmd_line_parse(cmd_line, false,
                                                   argc, argv))) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        opal_show_help("help-orteconsole.txt", "orteconsole:usage", false,
                       argv[0], args);
        free(args);
        return ret;
    }

    /* Check for help request */
    if(orteconsole_globals.help) {
        char *args = NULL;
        args = opal_cmd_line_get_usage_msg(cmd_line);
        opal_show_help("help-orteconsole.txt", "orteconsole:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    /*
     * Intialize the ORTE environment
     */
    if (OMPI_SUCCESS != (ret = orte_init()) ) {
        opal_show_help("help-orteconsole.txt", "orteconsole:init-failure", false,
                       "orte_init()", ret);
        return ret;
    }

    /*
     * Work Loop
     */
    exit_cmd = false;
    while (!exit_cmd) {
        printf("ompiconsole> ");
        
        usercmd = orte_getinputline();
        
        execute_command(usercmd);
    }

    /*
     * Finialize ORTE Environment
     */
    if(ORTE_SUCCESS != (ret = orte_finalize()) ) {
        opal_show_help("help-orteconsole.txt", "orteconsole:finalize-failure", false,
                       "orte_finalize()", ret);
        return ret;
    }
    
    return ORTE_SUCCESS;
}

static int execute_command(char *command) {
    orte_console_command_t *cur_cmd;
    int i, ret;

    for(i = 0; console_commands[i].cmd_type != ORTE_CONSOLE_TYPE_NULL; ++i) {
        cur_cmd = &console_commands[i];
        
        /*
         * Check Full Name then check short name
         */
        if ( ( 0 == strncmp(command, cur_cmd->cmd_full_name, 
                            strlen(cur_cmd->cmd_full_name))           )  ||
             ( ( strlen(command) == strlen(cur_cmd->cmd_short_name) ) &&
               ( 0 == strncmp(command, cur_cmd->cmd_short_name,
                              strlen(cur_cmd->cmd_short_name))      ) ) ) {
            ret = cur_cmd->cmd_function();

            if(ret == ORTE_ERR_NOT_IMPLEMENTED) {
                opal_show_help("help-orteconsole.txt", "orteconsole:unimplemented-command", false,
                               cur_cmd->cmd_full_name);
                return ret;
            }
            else if(ret != ORTE_SUCCESS) {
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
    if( console_commands[i].cmd_type == ORTE_CONSOLE_TYPE_NULL ) {
        opal_show_help("help-orteconsole.txt", "orteconsole:unknown-command", false,
                       command);
        return ORTE_ERR_NOT_IMPLEMENTED;
    }

    return ORTE_SUCCESS;
}

static int orte_console_exit() {
    exit_cmd = true;

    orte_console_sendcmd(ORTE_DAEMON_EXIT_CMD);

    return ORTE_SUCCESS;
}

static int orte_console_help() {
    orte_console_command_t *cur_cmd;
    int i;

    printf("Open RTE Console Commands:\n\n");

    for(i = 0; console_commands[i].cmd_type != ORTE_CONSOLE_TYPE_NULL; ++i) {
        cur_cmd = &console_commands[i];
        printf("%15s | %5s \t%s\n", 
               cur_cmd->cmd_full_name,
               cur_cmd->cmd_short_name, 
               cur_cmd->cmd_description);
    }

    printf("\n");

    return ORTE_SUCCESS;
}

static int orte_console_dumpvm() {

    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int orte_console_contactinfo() {
    char * str_response;
    orte_buffer_t *buffer = NULL;
    orte_process_name_t seed={0,0,0};
    size_t n;

    orte_console_sendcmd(ORTE_DAEMON_CONTACT_QUERY_CMD);
    if (0 > orte_rml.recv_buffer(&seed, buffer, ORTE_RML_TAG_DAEMON)) {
        printf("****got a bad response\n");
        return ORTE_ERROR;
    } else {
        n = 1;
        if (ORTE_SUCCESS != orte_dps.unpack(buffer, &str_response, &n, ORTE_STRING)) {
            printf("****couldn't decode answer\n");
            return ORTE_ERROR;
        } else {
            printf(str_response);
            printf("\n");
        }
    }

    return ORTE_SUCCESS;
}

static void orte_console_sendcmd(orte_daemon_cmd_flag_t usercmd)
{
    orte_buffer_t *cmd;
    orte_daemon_cmd_flag_t command;
    int rc;
    orte_process_name_t seed={0,0,0};

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        fprintf(stderr, "console: comm failure\n");
        return;
    }
    command = usercmd;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    if (0 > orte_rml.send_buffer(&seed, cmd, ORTE_RML_TAG_DAEMON, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return;
    }
    OBJ_RELEASE(cmd);
}

char *orte_getinputline()
{
    char *ret, *buff;
    char input[ORTE_CONSOLE_MAX_LINE_LENGTH];

    ret = fgets(input, ORTE_CONSOLE_MAX_LINE_LENGTH, stdin);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        buff = strdup(input);
        return buff;
    }
    return NULL;
}
