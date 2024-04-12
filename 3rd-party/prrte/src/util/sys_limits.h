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
 * Copyright (c) 2013      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_SYS_LIMITS_H
#define PRTE_SYS_LIMITS_H

#include "prte_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

BEGIN_C_DECLS

/* define a structure to hold the various limits we find
 * so that users can neatly access them
 */
typedef struct prte_sys_limits_t {
    bool initialized;
    int num_files;
    int num_procs;
    size_t file_size;
} prte_sys_limits_t;

/* since we only want to do this once, we will store the
 * values in the following locations - provide access here
 */
PRTE_EXPORT extern prte_sys_limits_t prte_sys_limits;

/* Get the system resource limits and, if requested, set
 * them to the specified limit
 */
PRTE_EXPORT int prte_util_init_sys_limits(char **errmsg);

/**
 * Get pagesize
 */
PRTE_EXPORT int prte_getpagesize(void);

END_C_DECLS

#endif /* PRTE_STRNCPY_H */
