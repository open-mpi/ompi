/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef PRTE_STACKTRACE_H
#define PRTE_STACKTRACE_H

#include "prte_config.h"

/*
 * File descriptor to be used by the backtrace framework if prte_backtrace_print
 * is passed NULL for it's FILE file pointer.
 */
extern int prte_stacktrace_output_fileno;

/**
 * Output the current stack trace (not including the call to this
 * function) to the stream indicated.
 */
PRTE_EXPORT void prte_stackframe_output(int stream);

/**
 * Return the current stack trace (not including the call to this
 * function) as a string (which must be freed by the caller).
 */
PRTE_EXPORT char *prte_stackframe_output_string(void);

/**
 * Here we register the prte_show_stackframe function for signals
 * passed to OpenMPI by the mpi_signal-parameter passed to mpirun
 * by the user.
 *
 *  @returnvalue PRTE_SUCCESS
 *  @returnvalue PRTE_ERR_BAD_PARAM if the value in the signal-list
 *    is not a valid signal-number
 *
 */
PRTE_EXPORT int prte_util_register_stackhandlers(void);

#endif /* PRTE_STACKTRACE_H */
