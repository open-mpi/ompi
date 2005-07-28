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

/*
 * Local Structures
 */

/* Command line Structure */
typedef struct {
    bool help;

    opal_mutex_t lock;
    opal_condition_t cond;
} orteconsole_globals_t;

enum orte_console_type_t {
    ORTE_CONSOLE_TYPE_NULL,
    ORTE_CONSOLE_TYPE_STD
};
typedef enum orte_console_type_t orte_console_type_t;

/* Structure detailing each command allowed by the console */
typedef struct {
    /* Full Name for the command */
    const char *cmd_full_name;
    /* Common abbreviation for this command */
    const char *cmd_short_name;
    /* Number of expected arguments */
    int cmd_args;
    /* Type of command */
    orte_console_type_t cmd_type;
    /* Pointer to the function to execute */
    int (*cmd_function) (void);
    /* Short description of what this command does */
    const char *cmd_description;
} orte_console_command_t;

/*
 * Function for each command
 */
static int orte_console_exit(void);
static int orte_console_help(void);
static int orte_console_contactinfo(void);
static int orte_console_dumpvm(void);

/*
 * Support Functions
 */
static char *orte_getinputline(void);
static void  orte_console_sendcmd(orte_daemon_cmd_flag_t usercmd);
static int   execute_command(char *command);

#endif /* ORTECONSOLE_H */
