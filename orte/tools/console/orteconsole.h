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

#ifndef ORTECONSOLE_H
#define ORTECONSOLE_H

#define ORTE_CONSOLE_MAX_LINE_LENGTH 1024
#define ORTE_CONSOLE_MAX_ARGC 10

/*
 * Local Structures
 */

/* Command line Structure */
typedef struct {
    bool help;

    opal_mutex_t     lock;
    opal_condition_t cond;
} orte_console_globals_t;

enum orte_console_type_t {
    ORTE_CONSOLE_TYPE_NULL,

    ORTE_CONSOLE_TYPE_STD,
    ORTE_CONSOLE_TYPE_HIDDEN
};
typedef enum orte_console_type_t orte_console_type_t;

typedef struct {
    /* Command Name */
    char *    cmd_name;

    char *    argv[ORTE_CONSOLE_MAX_ARGC];
    int       argc;
} orte_console_input_command_t;

/* Structure detailing each command allowed by the console */
typedef struct {
    /* Full Name for the command */
    const char *        cmd_full_name;
    /* Common abbreviation for this command */
    const char *        cmd_short_name;
    /* Number of expected additional arguments */
    int                 cmd_args;
    /* Type of command */
    orte_console_type_t cmd_type;
    /* Pointer to the function to execute */
    int               (*cmd_function) (orte_console_input_command_t);
    /* Short illustration of how the command should be used */
    const char *       cmd_usage;
    /* Short description of what this command does */
    const char *       cmd_description;
} orte_console_command_t;

/*
 * Function for each command
 */
static int orte_console_exit(orte_console_input_command_t);
static int orte_console_help(orte_console_input_command_t);
static int orte_console_contactinfo(orte_console_input_command_t);
static int orte_console_dumpvm(orte_console_input_command_t);
static int orte_console_devel(orte_console_input_command_t);

/*
 * Support Functions
 */
static char *orte_console_get_input_line(void);
static int   orte_console_send_command(orte_daemon_cmd_flag_t usercmd);
static int   orte_console_parse_command(char * usercmd, orte_console_input_command_t *input_command); 
static int   orte_console_execute_command(orte_console_input_command_t command);

#endif /* ORTECONSOLE_H */
