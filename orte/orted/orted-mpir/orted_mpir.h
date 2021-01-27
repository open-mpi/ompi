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
