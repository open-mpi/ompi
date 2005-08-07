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

#ifdef HAVE_READLINE_H
#include <readline/readline.h>
#include <readline/history.h>
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
orte_console_globals_t orte_console_globals;

opal_cmd_line_init_t cmd_line_opts[] = {
    { NULL, NULL, NULL, 'h', NULL, "help", 0, 
      &orte_console_globals.help, OPAL_CMD_LINE_TYPE_BOOL,
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
      "quit",
      "Exit the console" },

    { "help", "h", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_help,
      "help [command]",
      "Print this display" },

    { "contactinfo", "ci", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_contactinfo,
      "contactinfo",
      "Query Contact Information from Daemons" },

    { "dumpvm", "vm", 0, ORTE_CONSOLE_TYPE_STD,
      orte_console_dumpvm,
      "dumpvm",
      "Get VM List from daemons" },

    { "devel", NULL, 2, ORTE_CONSOLE_TYPE_HIDDEN,
      orte_console_devel,
      "devel arg1 arg2",
      "Development Debugging function" },

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
    orte_console_input_command_t input_command;

    /* 
     * Setup to check common command line options
     */
    memset(&orte_console_globals, 0, sizeof(orte_console_globals_t));
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
    if(orte_console_globals.help) {
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
    /* first, set the flag telling orte_init that I am NOT a
     * singleton, but am "infrastructure" - prevents setting
     * up incorrect infrastructure that only a singleton would
     * require
     */

    ret = mca_base_param_reg_int_name("orte_base", "infrastructure",
                                "Whether we are ORTE infrastructure or an ORTE application",
                                false, false, (int)false, NULL);
    mca_base_param_set_int(ret, (int)true);
    
    if (OMPI_SUCCESS != (ret = orte_init()) ) {
        opal_show_help("help-orteconsole.txt", "orteconsole:init-failure", false,
                       "orte_init()", ret);
        return ret;
    }

    /*
     * Work Loop
     */
    exit_cmd = false;
    memset(&input_command, 0, sizeof(orte_console_input_command_t));
    while (!exit_cmd) {
        usercmd = orte_console_get_input_line();
        
        orte_console_parse_command(usercmd, &input_command);

        orte_console_execute_command(input_command);
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

static int command_cmp(char* user_command, orte_console_command_t system_command) {
    
    /*
     * Check for Full Name Match
     */
    if( 0 == strncmp(user_command, system_command.cmd_full_name, 
                     strlen(system_command.cmd_full_name)) ) {
        return 0;
    }
    /*
     * Check for Short Name Match
     */
    else if( ( system_command.cmd_short_name != NULL )                                  && 
             ( strlen(user_command) == strlen(system_command.cmd_short_name) ) &&
             ( 0 == strncmp(user_command, system_command.cmd_short_name,
                            strlen(system_command.cmd_short_name))                  ) ) {
        return 0;
    }
    
    return -1;
}

static int orte_console_execute_command(orte_console_input_command_t input_command) {
    orte_console_command_t *cur_cmd;
    int i, ret;

    for(i = 0; console_commands[i].cmd_type != ORTE_CONSOLE_TYPE_NULL; ++i) {
        cur_cmd = &console_commands[i];

        /*
         * Check the requested command
         */
        if ( command_cmp(input_command.cmd_name, *cur_cmd) == 0 ){
            ret = cur_cmd->cmd_function(input_command);

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
                       input_command.cmd_name);
        return ORTE_ERR_NOT_IMPLEMENTED;
    }

    return ORTE_SUCCESS;
}

static int   orte_console_parse_command(char * usercmd, orte_console_input_command_t *input_command){
    char *tok;
    
    input_command->argc = 0;
    tok = strtok(usercmd, " ");
    while(tok != NULL) {
        if(input_command->argc == 0) {
            input_command->cmd_name = strdup(tok);
        }
        input_command->argv[input_command->argc] = strdup(tok);
        ++(input_command->argc);
        
        tok = strtok(NULL, " ");
    }
    
    return ORTE_SUCCESS;
}

static int orte_console_devel(orte_console_input_command_t input_command) {
    
    system("orted --seed --persistent --scope public");
    
    return ORTE_SUCCESS;
}

static int orte_console_exit(orte_console_input_command_t input_command) {
    exit_cmd = true;

    orte_console_send_command(ORTE_DAEMON_EXIT_CMD);

    return ORTE_SUCCESS;
}

static int orte_console_help(orte_console_input_command_t input_command) {
    orte_console_command_t *cur_cmd;
    int i;

    /*
     * Generic Help
     */
    if(input_command.argc <= 1) {
        printf("Open RTE Console Commands:\n\n");
        
        for(i = 0; console_commands[i].cmd_type != ORTE_CONSOLE_TYPE_NULL; ++i) {
            cur_cmd = &console_commands[i];
            if(cur_cmd->cmd_type != ORTE_CONSOLE_TYPE_HIDDEN) {
                printf("%15s ", cur_cmd->cmd_full_name);
                if(cur_cmd->cmd_short_name == NULL) {
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

            if ( command_cmp(input_command.argv[1], *cur_cmd) == 0 ){
                printf("Command:\n");
                printf("\t%s ", cur_cmd->cmd_full_name);
                if(cur_cmd->cmd_short_name != NULL) {
                    printf(" | %5s", cur_cmd->cmd_short_name);
                }
                printf("\n");
                
                printf("Description:\n");
                printf("\t%s\n", cur_cmd->cmd_description);
                
                if(cur_cmd->cmd_usage != NULL) {
                    printf("Usage:\n");
                    printf("\t%s\n", cur_cmd->cmd_usage);
                }
                break;
            }
        }
        
        /*
         * Command Not Found
         */
        if( console_commands[i].cmd_type == ORTE_CONSOLE_TYPE_NULL ) {
            opal_show_help("help-orteconsole.txt", "orteconsole:unknown-command", false,
                           input_command.argv[1]);
            return ORTE_SUCCESS;
        }

        printf("\n");
    }

    return ORTE_SUCCESS;
}

static int orte_console_dumpvm(orte_console_input_command_t input_command) {

    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int orte_console_contactinfo(orte_console_input_command_t input_command) {
    char * str_response;
    orte_buffer_t *buffer = NULL;
    orte_process_name_t seed={0,0,0};
    size_t n;

    orte_console_send_command(ORTE_DAEMON_CONTACT_QUERY_CMD);
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

static int orte_console_send_command(orte_daemon_cmd_flag_t usercmd)
{
    orte_buffer_t *cmd;
    orte_daemon_cmd_flag_t command;
    int rc;
    orte_process_name_t seed={0,0,0};

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) {
        fprintf(stderr, "console: comm failure\n");
        return ORTE_ERROR;
    }

    command = usercmd;

    if (ORTE_SUCCESS != (rc = orte_dps.pack(cmd, &command, 1, ORTE_DAEMON_CMD))) {
        fprintf(stderr, "console: comm failure 1\n");
        ORTE_ERROR_LOG(rc);
        return ORTE_ERROR;
    }

    if (0 > orte_rml.send_buffer(&seed, cmd, ORTE_RML_TAG_DAEMON, 0)) {
        fprintf(stderr, "console: comm failure 2\n");
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        return ORTE_ERR_COMM_FAILURE;
    }

    OBJ_RELEASE(cmd);
    
    return ORTE_SUCCESS;
}

char *orte_console_get_input_line()
{
#ifdef HAVE_READLINE_H
    return readline("orteconsole>");
#else
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
#endif
}
