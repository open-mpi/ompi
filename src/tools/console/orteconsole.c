/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include <unistd.h>

#include "dps/dps.h"

#include "util/sys_info.h"
#include "util/cmd_line.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/output.h"
#include "util/os_path.h"
#include "util/show_help.h"
#include "util/universe_setup_file_io.h"
#include "runtime/runtime.h"

#include "mca/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/pls/base/base.h"
#include "tools/orted/orted.h"

#define ORTE_CONSOLE_MAX_LINE_LENGTH 1024

static char *orte_getinputline(void);

static void orte_console_sendcmd(orte_daemon_cmd_flag_t usercmd);


int main(int argc, char *argv[])
{
    int ret=0;
    ompi_cmd_line_t *cmd_line;
    bool exit_cmd;
    char *usercmd, *str_response;
    orte_buffer_t *buffer;
    orte_process_name_t seed={0,0,0};
    size_t n;

    /* setup to check common command line options that just report and die */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);

    ompi_cmd_line_make_opt(cmd_line, 'v', "version", 0,
            "Show version of this program");

    ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0,
            "Show help for this function");


    /* parse the local commands */
    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, true, argc, argv)) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(cmd_line);
        ompi_show_help("help-console.txt", "console:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    /* check for help and version requests */
    if (ompi_cmd_line_is_taken(cmd_line, "help") || 
        ompi_cmd_line_is_taken(cmd_line, "h")) {
        char *args = NULL;
        args = ompi_cmd_line_get_usage_msg(cmd_line);
        ompi_show_help("help-console.txt", "console:usage", false,
                       argv[0], args);
        free(args);
        return 1;
    }

    if (ompi_cmd_line_is_taken(cmd_line, "version") ||
        ompi_cmd_line_is_taken(cmd_line, "v")) {
        printf("...showing off my version!\n");
        exit(1);
    }

    /*
     * Intialize the ORTE environment
     */
    if (OMPI_SUCCESS != orte_init()) {
        /* BWB show_help */
        printf("show_help: ompi_init failed\n");
        return ret;
    }

    exit_cmd = false;
    while (!exit_cmd) {
        	printf("ompiconsole> ");
        	usercmd = orte_getinputline();
        	if (0 == strncmp(usercmd, "exit", strlen("exit"))) {
        	    exit_cmd = true;
        	    orte_console_sendcmd(ORTE_DAEMON_EXIT_CMD);
        	} else if (0 == strncmp(usercmd, "contactinfo", strlen("contactinfo"))) {
        	    orte_console_sendcmd(ORTE_DAEMON_CONTACT_QUERY_CMD);
        	    if (0 > orte_rml.recv_buffer(&seed, buffer, ORTE_RML_TAG_DAEMON)) {
        		    printf("****got a bad response\n");
        	    } else {
                 n = 1;
            		if (ORTE_SUCCESS != orte_dps.unpack(buffer, &str_response, &n, ORTE_STRING)) {
            		    printf("****couldn't decode answer\n");
            		} else {
            		    printf(str_response);
            		    printf("\n");
            		}
        	    }
        	} else if (0 == strncmp(usercmd, "dumpvm", strlen("dumpvm"))) {
        	    fprintf(stderr, "getting vm list\n");
        	} else {
        	    printf("huh???\n");
        	}
    }

    fprintf(stderr, "finalize rte\n");
    orte_finalize();
    exit(0);
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
