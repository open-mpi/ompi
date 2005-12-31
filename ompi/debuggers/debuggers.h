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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * MPI portion of debugger support
 */

#ifndef OMPI_DEBUGGERS_H
#define OMPI_DEBUGGERS_H

#include "ompi_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * Wait for a TotalView-like debugger if asked.
     */
    OMPI_DECLSPEC void ompi_wait_for_totalview(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_DEBUGGERS_H */
