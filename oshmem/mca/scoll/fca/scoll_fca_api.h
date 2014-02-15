/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"

#include <fca_api.h>
#include <fca_version.h>
#include <config/fca_parse_specfile.h>

#ifndef FCA_API
#define OSHMEM_FCA_VERSION 12
#else
#define OSHMEM_FCA_VERSION FCA_API
#endif

/*
 *  * FCA API compatibility layer.
 *   * MPI build must define an FCA version macro.
 *    */

#define OSHMEM_FCA_BARRIER            1
#define OSHMEM_FCA_BCAST              1
#define OSHMEM_FCA_ALLREDUCE          1

#if OSHMEM_FCA_VERSION == 12

#define OSHMEM_FCA_ALLGATHER          0
#define FCA_MAJOR_BIT               24ul
#define FCA_MINOR_BIT               16ul
#define EUSESHMEM                     287

static inline int mca_scoll_fca_comm_init(fca_t *fca_context,
                                          int rank,
                                          int comm_size,
                                          int local_proc_idx,
                                          int num_local_procs,
                                          fca_comm_desc_t *comm_desc,
                                          fca_comm_t **fca_comm)
{
    return fca_comm_init(fca_context,
                         local_proc_idx,
                         num_local_procs,
                         comm_size,
                         comm_desc,
                         fca_comm);
}
#elif OSHMEM_FCA_VERSION >= 20

#define OSHMEM_FCA_ALLGATHER          1
#define OSHMEM_FCA_ALLGATHERV         1

#define OSHMEM_FCA_PROGRESS           1
#define EUSESHMEM                     287

static inline int mca_scoll_fca_comm_init(fca_t *fca_context, int rank, int comm_size,
        int local_proc_idx, int num_local_procs,
        fca_comm_desc_t *comm_desc,
        fca_comm_t **fca_comm)
{
    fca_comm_init_spec_t spec;

    spec.rank = rank;
    spec.size = comm_size;
    spec.desc = *comm_desc;
    spec.proc_idx = local_proc_idx;
    spec.num_procs = num_local_procs;
    return fca_comm_init(fca_context, &spec, fca_comm);
}
#else

#error "FCA API version is unsupported"

#endif
