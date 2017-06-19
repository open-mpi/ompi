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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef OPAL_STACKTRACE_H
#define OPAL_STACKTRACE_H

#include "opal_config.h"

/*
 * File descriptor to be used by the backtrace framework if opal_backtrace_print
 * is passed NULL for it's FILE file pointer.
 */
extern int opal_stacktrace_output_fileno;

/**
 * Set the opal_stacktrace_output_filename variable.
 *
 * We append VPID and PID as a suffix to the filename specified by the MCA
 * parameter opal_stacktrace_output. But since the MCA parameter registration
 * and its parsing may be processed before setting the VPID of this process,
 * they may not be able to determine the suffix. This function should be
 * called before using the opal_stacktrace_output_filename variable so that
 * the filename suffix will be set appropriately with the VPID.
 */
OPAL_DECLSPEC void opal_stacktrace_set_output_filename(void);

/**
 * Output the current stack trace (not including the call to this
 * function) to the stream indicated.
 */
OPAL_DECLSPEC void opal_stackframe_output(int stream);

/**
 * Return the current stack trace (not including the call to this
 * function) as a string (which must be freed by the caller).
 */
OPAL_DECLSPEC char *opal_stackframe_output_string(void);

/**
 * Here we register the opal_show_stackframe function for signals
 * passed to OpenMPI by the mpi_signal-parameter passed to mpirun
 * by the user.
 *
 *  @returnvalue OPAL_SUCCESS
 *  @returnvalue OPAL_ERR_BAD_PARAM if the value in the signal-list
 *    is not a valid signal-number
 *
 */
OPAL_DECLSPEC int opal_util_register_stackhandlers (void);

#endif /* OPAL_STACKTRACE_H */
