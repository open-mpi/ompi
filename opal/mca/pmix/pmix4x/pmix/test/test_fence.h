/*
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <src/include/pmix_config.h>
#include <pmix.h>

#include <time.h>
#include "test_common.h"

int test_fence(test_params params, char *my_nspace, pmix_rank_t my_rank);
int test_job_fence(test_params params, char *my_nspace, pmix_rank_t my_rank);
