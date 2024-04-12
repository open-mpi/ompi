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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "constants.h"

#include "src/runtime/prte_locks.h"

/* for everyone */
pmix_mutex_t prte_finalize_lock = PMIX_MUTEX_STATIC_INIT;

/* for HNPs */
pmix_mutex_t prte_abort_inprogress_lock = PMIX_MUTEX_STATIC_INIT;
pmix_mutex_t prte_jobs_complete_lock = PMIX_MUTEX_STATIC_INIT;
pmix_mutex_t prte_quit_lock = PMIX_MUTEX_STATIC_INIT;
pmix_lock_t prte_init_lock = PMIX_LOCK_STATIC_INIT;

int prte_locks_init(void)
{
    return PRTE_SUCCESS;
}
