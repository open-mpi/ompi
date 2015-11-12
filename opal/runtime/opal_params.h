/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#if !defined(OPAL_PARAMS_H)
#define OPAL_PARAMS_H

extern char *opal_signal_string;
extern char *opal_net_private_ipv4;
extern char *opal_set_max_sys_limits;

#if OPAL_ENABLE_DEBUG
extern bool opal_progress_debug;
#endif

/**
 * Whether an abort operation should print out a stack trace or not.
 */
OPAL_DECLSPEC extern bool opal_abort_print_stack;
OPAL_DECLSPEC extern int opal_abort_print_stack_var_index;

/**
 * Whether  abort operation  should  print  out an  identifying  message
 * (e.g., hostname  and PID)  and loop waiting  for a  debugger to
 * attach.  The value of the integer is how many seconds to wait:
 *
 * 0 = do not print the message and do not loop
 * negative value = print the message and loop forever
 * positive value = print the message and delay for that many seconds
 */
OPAL_DECLSPEC extern int opal_abort_delay;
OPAL_DECLSPEC extern int opal_abort_delay_var_index;

#endif
