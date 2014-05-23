/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 * $HEADER$
 */
#ifndef __TOPO_HELPERS_H__
#define __TOPO_HELPERS_H__
#include "ompi_config.h"

#include "mpi.h"

#include "ompi/include/ompi/constants.h"
#include "ompi/communicator/communicator.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <math.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

int ompi_comm_neighbors_count(MPI_Comm comm, int *indegree, int *outdegree, int *weighted);
int ompi_comm_neighbors(MPI_Comm comm, int maxindegree, int sources[], int sourceweights[], int maxoutdegree, int destinations[], int destweights[]);

#ifdef __cplusplus
}
#endif
 
#endif
