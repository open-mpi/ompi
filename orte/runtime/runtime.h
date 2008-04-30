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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Interface into the Open MPI Run Time Environment
 */
#ifndef ORTE_RUNTIME_H
#define ORTE_RUNTIME_H

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/util/cmd_line.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

/* some convenience definitions for code clarity */
#define ORTE_NON_TOOL           0x00
#define ORTE_TOOL               0x01
#define ORTE_TOOL_WITH_NAME     0x02

    /**
     * Initialize the Open Run Time Environment
     *
     * Initlize the Open Run Time Environment, including process
     * control, malloc debugging and threads, and out of band messaging.
     * This function should be called exactly once.  This function should
     * be called by every application using the RTE interface, including
     * MPI applications and mpirun.
     *
     * @param tool Whether we are ORTE tool or not
     */
ORTE_DECLSPEC    int orte_init(char flags);

    /**
     * Initialize parameters for ORTE.
     *
     * @retval ORTE_SUCCESS Upon success.
     * @retval ORTE_ERROR Upon failure.
     */
ORTE_DECLSPEC    int orte_register_params(void);

    /**
     * Initialize global storage for HNPs
     */
ORTE_DECLSPEC   int orte_hnp_globals_init(void);

    /**
     * Init the ORTE datatype support
     */
ORTE_DECLSPEC   int orte_dt_init(void);

    /**
     * Finalize the Open run time environment. Any function calling \code
     * orte_init should call \code orte_finalize. 
     *
     */
ORTE_DECLSPEC    int orte_finalize(void);

END_C_DECLS

#endif /* RUNTIME_H */
