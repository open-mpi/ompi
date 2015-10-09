/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <private/autogen/config.h>
#include <pmix.h>

#include <time.h>
#include "test_common.h"

int test_fence(test_params params, char *my_nspace, int my_rank);
int test_job_fence(test_params params, char *my_nspace, int my_rank);
