/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_DEBUGGER_BASE_H
#define MCA_DEBUGGER_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "opal/class/opal_list.h"

#include "orte/mca/debugger/debugger.h"

BEGIN_C_DECLS

typedef struct {
    int output;
    bool dump_proctable;
    char *test_daemon;
    bool test_attach;
} orte_debugger_base_t;

ORTE_DECLSPEC extern orte_debugger_base_t orte_debugger_base;

/*
 * function definitions
 */
ORTE_DECLSPEC int orte_debugger_base_open(void);
ORTE_DECLSPEC int orte_debugger_base_close(void);

ORTE_DECLSPEC int orte_debugger_base_select(void);
ORTE_DECLSPEC void orte_debugger_base_run_debugger(char *basename, opal_cmd_line_t *cmd_line,
                                                   int argc, char *argv[], int num_procs);
ORTE_DECLSPEC void orte_debugger_base_init_after_spawn(orte_job_t *jdata);
ORTE_DECLSPEC void orte_debugger_base_dump(void);

ORTE_DECLSPEC extern opal_list_t orte_debugger_base_components_available;

/* +++ begin MPICH/TotalView std debugger interface definitions */

#define MPIR_MAX_PATH_LENGTH 512
#define MPIR_MAX_ARG_LENGTH 1024

struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};

ORTE_DECLSPEC extern struct MPIR_PROCDESC *MPIR_proctable;
ORTE_DECLSPEC extern int MPIR_proctable_size;
ORTE_DECLSPEC extern volatile int MPIR_being_debugged;
ORTE_DECLSPEC extern volatile int MPIR_debug_state;
ORTE_DECLSPEC extern volatile int MPIR_i_am_starter;
ORTE_DECLSPEC extern volatile int MPIR_partial_attach_ok;
ORTE_DECLSPEC extern volatile char MPIR_executable_path[MPIR_MAX_PATH_LENGTH];
ORTE_DECLSPEC extern volatile char MPIR_server_arguments[MPIR_MAX_ARG_LENGTH];
ORTE_DECLSPEC extern volatile int MPIR_forward_output;
ORTE_DECLSPEC extern volatile int MPIR_forward_comm;
ORTE_DECLSPEC extern char MPIR_attach_fifo[MPIR_MAX_PATH_LENGTH];
ORTE_DECLSPEC extern int MPIR_force_to_main;

typedef void* (*orte_debugger_breakpoint_fn_t)(void);

ORTE_DECLSPEC void* MPIR_Breakpoint(void);

/* --- end MPICH/TotalView std debugger interface definitions */

END_C_DECLS
#endif
