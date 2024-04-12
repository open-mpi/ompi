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
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Locks to prevent loops inside PRTE
 */
#ifndef PRTE_LOCKS_H
#define PRTE_LOCKS_H

#include "prte_config.h"

#include "src/threads/pmix_mutex.h"
#include "src/threads/pmix_threads.h"

BEGIN_C_DECLS

/* for everyone */
PRTE_EXPORT extern pmix_mutex_t prte_finalize_lock;

/* for HNPs */
PRTE_EXPORT extern pmix_mutex_t prte_abort_inprogress_lock;
PRTE_EXPORT extern pmix_mutex_t prte_jobs_complete_lock;
PRTE_EXPORT extern pmix_mutex_t prte_quit_lock;
PRTE_EXPORT extern pmix_lock_t prte_init_lock;

/**
 * Initialize the locks
 */
PRTE_EXPORT int prte_locks_init(void);

END_C_DECLS

#endif /* #ifndef PRTE_LOCKS_H */
