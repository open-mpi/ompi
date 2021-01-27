/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orted_mpir.h"

/* instance the standard MPIR interfaces */
struct MPIR_PROCDESC *MPIR_proctable = NULL;
int MPIR_proctable_size = 0;
volatile int MPIR_being_debugged = 0;
volatile int MPIR_debug_state = 0;
int MPIR_i_am_starter = 0;
int MPIR_partial_attach_ok = 1;
char MPIR_executable_path[MPIR_MAX_PATH_LENGTH] = {0};
char MPIR_server_arguments[MPIR_MAX_ARG_LENGTH] = {0};
volatile int MPIR_forward_output = 0;
volatile int MPIR_forward_comm = 0;
char MPIR_attach_fifo[MPIR_MAX_PATH_LENGTH] = {0};
int MPIR_force_to_main = 0;

/* 
 * Attempt to prevent the compiler from optimizing out
 * MPIR_Breakpoint().
 *
 * Some older versions of automake can add -O3 to every
 * file via CFLAGS (which was demonstrated in automake v1.13.4),
 * so there is a possibility that the compiler will see
 * this function as a NOOP and optimize it out on older versions.
 * While using the current/recommended version of automake
 * does not do this, the following will help those
 * stuck with an older version, as well as guard against
 * future regressions.
 *
 * See the following git issue for more discussion:
 * https://github.com/open-mpi/ompi/issues/5501
 */
volatile void* volatile orte_noop_mpir_breakpoint_ptr = NULL;

/*
 * Breakpoint function for parallel debuggers
 */
void MPIR_Breakpoint(void)
{
    /* 
     * Actually do something with this pointer to make
     * sure the compiler does not optimize out this function.
     * The compiler should be forced to keep this
     * function around due to the volatile void* type.
     *
     * This pointer doesn't actually do anything other than
     * prevent unwanted optimization, and
     * *should not* be used anywhere else in the code.
     * So pointing this to the weeds should be OK.
     */
    orte_noop_mpir_breakpoint_ptr = (volatile void *) 0x42;
    return;
}
