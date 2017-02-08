/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_JOBDATA_H
#define PMIX_JOBDATA_H

#include <src/include/pmix_config.h>

#include "src/buffer_ops/buffer_ops.h"
#include "src/class/pmix_hash_table.h"

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
pmix_status_t pmix_job_data_dstore_store(const char *nspace, pmix_buffer_t *bptr);
#endif
pmix_status_t pmix_job_data_htable_store(const char *nspace, pmix_buffer_t *bptr);

#endif // PMIX_JOBDATA_H
