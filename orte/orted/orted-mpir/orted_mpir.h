/* Copyright (c) 2021      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTED_MPIR_H
#define ORTED_MPIR_H

#include "orte_config.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

#define MPIR_MAX_PATH_LENGTH 512
#define MPIR_MAX_ARG_LENGTH 1024


/* Note to future MPIR maintainers:
 *
 * This struct MUST (along with all other MPIR_* symbols) be declared
 * and defined in this file. Otherwise they may be compiled *without* -g
 * and *with* optimizations in production. In the case where they are
 * not here, the debugger won't have the complete definition of the proctable.
 * This will prevent the debugger from reading it properly.
 *
 * It also needs to be seen by multiple files (orted_submih.c and
 * orted_mpir_breakpoint.c), so this is a better place for it anyway.
 *
 * For more info/discussion on this, see the following github issue:
 * https://github.com/open-mpi/ompi/issues/8563
 */
struct MPIR_PROCDESC {
    char *host_name;        /* something that can be passed to inet_addr */
    char *executable_name;  /* name of binary */
    int pid;                /* process pid */
};

extern struct MPIR_PROCDESC *MPIR_proctable;
extern int MPIR_proctable_size;
extern volatile int MPIR_being_debugged;
extern volatile int MPIR_debug_state;
extern int MPIR_i_am_starter;
extern int MPIR_partial_attach_ok;
extern char MPIR_executable_path[MPIR_MAX_PATH_LENGTH];
extern char MPIR_server_arguments[MPIR_MAX_ARG_LENGTH];
extern volatile int MPIR_forward_output;
extern volatile int MPIR_forward_comm;
extern char MPIR_attach_fifo[MPIR_MAX_PATH_LENGTH];
extern int MPIR_force_to_main;

ORTE_DECLSPEC void __opal_attribute_optnone__ MPIR_Breakpoint(void);

#endif
