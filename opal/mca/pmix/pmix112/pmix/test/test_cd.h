/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <private/autogen/config.h>
#include <pmix.h>

#include "test_common.h"

int test_connect_disconnect(char *my_nspace, int my_rank);
int test_cd_common(pmix_proc_t *procs, size_t nprocs, int blocking, int disconnect);
