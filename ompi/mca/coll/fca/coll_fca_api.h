/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#include "ompi_config.h"

#include <fca_api.h>

#ifndef FCA_API
#define OMPI_FCA_VERSION 12
#else
#define OMPI_FCA_VERSION FCA_API
#endif

/*
 * FCA API compatibility layer.
 * MPI build must define an FCA version macro.
 */

#define OMPI_FCA_BARRIER            1
#define OMPI_FCA_BCAST              1
#define OMPI_FCA_REDUCE             1
#define OMPI_FCA_ALLREDUCE          1

#define OMPI_FCA_REDUCE_SCATTER     0
#define OMPI_FCA_GATHER             0
#define OMPI_FCA_GATHERV            0
#define OMPI_FCA_ALLTOALL           0
#define OMPI_FCA_ALLTOALLV          0
#define OMPI_FCA_ALLTOALLW          0


#if OMPI_FCA_VERSION == 12

#define OMPI_FCA_ALLGATHER          0

#define FCA_API_ABI_MAJOR           1
#define FCA_API_ABI_MINOR           2
#define FCA_MAJOR_BIT               24ul
#define FCA_MINOR_BIT               16ul
#define EUSEMPI                     287


typedef struct mca_coll_fca_ops_t {

    /* FCA Context operations */
    int (*init)(fca_init_spec_t *spec, fca_t **context);
    void (*cleanup)(fca_t *context);

    /* Fabric communicator creation */
    int (*comm_new)(fca_t *context, fca_comm_new_spec_t *spec, fca_comm_desc_t *comm_desc);
    int (*comm_end)(fca_t *context, int comm_id);
    void* (*get_rank_info)(fca_t *context, int *size);
    void (*free_rank_info)(void *rank_info);

    /* Local communicator creation */
    int (*comm_init)(fca_t *context, int proc_idx, int num_procs, int comm_size,
                     fca_comm_desc_t *comm_desc, fca_comm_t** fca_comm);
    void (*comm_destroy)(fca_comm_t *comm);
    int (*comm_get_caps)(fca_comm_t *comm, fca_comm_caps_t *caps);

    /* Collectives supported by FCA */
    int (*do_reduce)(fca_comm_t *comm, fca_reduce_spec_t *spec);
    int (*do_all_reduce)(fca_comm_t *comm, fca_reduce_spec_t *spec);
    int (*do_bcast)(fca_comm_t *comm, fca_bcast_spec_t *spec);
    int (*do_barrier)(fca_comm_t *comm);

    /* Helper functions */
    unsigned long (*get_version)(void);
    char * (*get_version_string)(void);
    fca_init_spec_t *(*parse_spec_file)(char* spec_ini_file);
    void (*free_init_spec)(fca_init_spec_t *fca_init_spec);
    int (*translate_mpi_op)(char *mpi_op);
    int (*translate_mpi_dtype)(char *mpi_dtype);
    int (*get_dtype_size)(int dtype);
    const char* (*strerror)(int code);
} mca_coll_fca_ops_t;


static inline int mca_coll_fca_comm_init(mca_coll_fca_ops_t *fca_ops,
                                         fca_t *fca_context, int rank, int comm_size,
                                         int local_proc_idx, int num_local_procs,
                                         fca_comm_desc_t *comm_desc,
                                         fca_comm_t **fca_comm)
{
    return fca_ops->comm_init(fca_context, local_proc_idx, num_local_procs,
                              comm_size, comm_desc, fca_comm);
}

static inline void mca_coll_fca_get_bcast_root(int root_rank, int *local_ranks,
                                               int num_local_ranks,
                                               fca_bcast_spec_t *spec)
{
    int i;

    for (i = 0; i < num_local_ranks; ++i) {
        if (local_ranks[i] == root_rank) {
            spec->root_indx = i;
            return;
        }
    }
    spec->root_indx = -1;
}

static inline void mca_coll_fca_get_reduce_root(int root_rank, int my_rank,
                                                fca_reduce_spec_t *spec)
{
    spec->is_root = root_rank == my_rank;
}

#elif OMPI_FCA_VERSION == 20 || OMPI_FCA_VERSION == 21

#define OMPI_FCA_ALLGATHER          1
#define OMPI_FCA_PROGRESS           1

#define FCA_API_ABI_MAJOR           2
#if OMPI_FCA_VERSION == 20
#  define FCA_API_ABI_MINOR         0
#else
#  define FCA_API_ABI_MINOR         1
#endif

typedef struct mca_coll_fca_ops_t {

    /* FCA Context operations */
    int (*init)(fca_init_spec_t *spec, fca_t **context);
    void (*cleanup)(fca_t *context);
    void (*progress)(fca_t *context);

    /* Fabric communicator creation */
    int (*comm_new)(fca_t *context, fca_comm_new_spec_t *spec, fca_comm_desc_t *comm_desc);
    int (*comm_end)(fca_t *context, int comm_id);
    void* (*get_rank_info)(fca_t *context, int *size);
    void (*free_rank_info)(void *rank_info);

    /* Local communicator creation */
    int (*comm_init)(fca_t *context, fca_comm_init_spec_t *spec, fca_comm_t** fca_comm);
    void (*comm_destroy)(fca_comm_t *comm);
    int (*comm_get_caps)(fca_comm_t *comm, fca_comm_caps_t *caps);

    /* Collectives supported by FCA */
    int (*do_reduce)(fca_comm_t *comm, fca_reduce_spec_t *spec);
    int (*do_all_reduce)(fca_comm_t *comm, fca_reduce_spec_t *spec);
    int (*do_bcast)(fca_comm_t *comm, fca_bcast_spec_t *spec);
    int (*do_barrier)(fca_comm_t *comm);
    int (*do_allgather)(fca_comm_t *comm, fca_gather_spec_t *spec);
    int (*do_allgatherv)(fca_comm_t *comm, fca_gatherv_spec_t *spec);

    /* Helper functions */
    unsigned long (*get_version)(void);
    char * (*get_version_string)(void);
    fca_init_spec_t *(*parse_spec_file)(char* spec_ini_file);
    void (*free_init_spec)(fca_init_spec_t *fca_init_spec);
    int (*translate_mpi_op)(char *mpi_op);
    int (*translate_mpi_dtype)(char *mpi_dtype);
    int (*get_dtype_size)(int dtype);
    const char* (*strerror)(int code);
} mca_coll_fca_ops_t;


static inline int mca_coll_fca_comm_init(mca_coll_fca_ops_t *fca_ops,
                                         fca_t *fca_context, int rank, int comm_size,
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
    return fca_ops->comm_init(fca_context, &spec, fca_comm);
}

static inline void mca_coll_fca_get_bcast_root(int root_rank, int *local_ranks,
                                               int num_local_ranks,
                                               fca_bcast_spec_t *spec)
{
    spec->root = root_rank;
}

static inline void mca_coll_fca_get_reduce_root(int root_rank, int my_rank,
                                                fca_reduce_spec_t *spec)
{
    spec->root = root_rank;
}

#else

#error "FCA API version is unsupported"

#endif
