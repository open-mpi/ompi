/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Global params for OpenRTE
 */
#ifndef ORTE_RUNTIME_PARAM_H
#define ORTE_RUNTIME_PARAM_H

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* globals used by RTE - instanced in orte_params.c */

ORTE_DECLSPEC extern int orte_debug_flag;

ORTE_DECLSPEC extern struct timeval orte_abort_timeout;

/**
 * Whether ORTE is initialized or not
 */
ORTE_DECLSPEC extern bool orte_initialized;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* ORTE_RUNTIME_PARAM_H */
