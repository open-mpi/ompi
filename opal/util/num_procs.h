/*
 * Copyright (c) 2007 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * This function is a simple wrapper to have an OS-independent
 * mechanism to get the number of processors on a local host.
 */

#ifndef OPAL_NUM_PROCS_H
#define OPAL_NUM_PROCS_H

#include "opal_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Provide a portable method for getting the number of processors on
     * the local machine.  In POSIX environments, this is a simple wrapper
     * around sysconf().
     *
     * @retval OPAL_SUCCESS If successful, indicating that num_procs
     * has a meaningful value.
     * @retval OPAL_ERR_NOT_IMPLEMENTED on platforms that are not yet
     * supported.
     */
    int opal_get_num_processors(int *num_procs);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
