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

#ifndef OMPI_STACKTRACE_H
#define OMPI_STACKTRACE_H

#include "ompi_config.h"

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
	
/**
 * This function is being called as a signal-handler in response
 * to a user-specified signal (e.g. SIGFPE or SIGSEGV).
 * For Linux/Glibc, it then uses backtrace and backtrace_symbols
 * to figure the current stack and then prints that out to stdout.
 * Yes, printf and malloc are not signal-safe per se, but should be 
 * on Linux?
 *
 *  @param signo with the signal number raised
 *  @param act with information regarding the reason/send of the signal
 *  @param oldact
 *
 */
#ifndef WIN32
static void ompi_show_stackframe (int signo, siginfo_t * info, void * p);
#endif

/**
 * Here we register the ompi_show_stackframe function for signals
 * passed to OpenMPI by the mpi_signal-parameter passed to mpirun
 * by the user.
 *
 *  @returnvalue OMPI_SUCCESS
 *  @returnvalue OMPI_ERR_BAD_PARAM if the value in the signal-list
 *    is not a valid signal-number
 *               
 */
OMPI_DECLSPEC int ompi_util_register_stackhandlers (void);

#endif /* OMPI_STACKTRACE_H */
