/*
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "src/include/pmix_config.h"
#include "include/pmix.h"

#include "test_common.h"
#include <time.h>

int test_fence(test_params params, char *my_nspace, pmix_rank_t my_rank);
int test_job_fence(test_params params, char *my_nspace, pmix_rank_t my_rank);
