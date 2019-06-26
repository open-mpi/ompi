/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include "src/util/argv.h"
#include "test_common.h"

void set_client_argv(test_params *params, char ***argv);
int launch_clients(int nprocs, char *binary, char *** client_env, char ***base_argv);
