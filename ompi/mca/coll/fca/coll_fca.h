/*
 * Copyright (c) 2011      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_FCA_H
#define MCA_COLL_FCA_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"
#include "orte/runtime/orte_globals.h"
#include "coll_fca_api.h"
#include "coll_fca_debug.h"


#ifdef OMPI_DATATYPE_MAX_PREDEFINED

#include "opal/datatype/opal_convertor.h"
#define FCA_CONVERTOR_T opal_convertor_t
#define FCA_CONVERTOR_COPY_AND_PREPARE_FOR_SEND opal_convertor_copy_and_prepare_for_send
#define FCA_CONVERTOR_COPY_AND_PREPARE_FOR_RECV opal_convertor_copy_and_prepare_for_recv
#define FCA_CONVERTOR_CONVERTOR_GET_PACKED_SIZE opal_convertor_get_packed_size
#define FCA_CONVERTOR_CONVERTOR_UNPACK opal_convertor_unpack
#define FCA_CONVERTOR_CONVERTOR_PACK opal_convertor_pack

#define FCA_DT_MAX_PREDEFINED     OMPI_DATATYPE_MAX_PREDEFINED
#define FCA_DT_EXTENT             ompi_datatype_type_extent
#define FCA_DT_GET_TRUE_EXTENT    ompi_datatype_get_true_extent
#define FCA_DT_IS_CONTIGUOUS_MEMORY_LAYOUT(__dtype, __count) \
                                  ompi_datatype_is_contiguous_memory_layout(__dtype, __count)
#else

#include "ompi/datatype/convertor.h"
#define FCA_CONVERTOR_T ompi_convertor_t
#define FCA_CONVERTOR_COPY_AND_PREPARE_FOR_SEND ompi_convertor_copy_and_prepare_for_send
#define FCA_CONVERTOR_COPY_AND_PREPARE_FOR_RECV ompi_convertor_copy_and_prepare_for_recv
#define FCA_CONVERTOR_CONVERTOR_GET_PACKED_SIZE ompi_convertor_get_packed_size
#define FCA_CONVERTOR_CONVERTOR_UNPACK ompi_convertor_unpack
#define FCA_CONVERTOR_CONVERTOR_PACK ompi_convertor_pack

#define FCA_DT_MAX_PREDEFINED     DT_MAX_PREDEFINED
#define FCA_DT_EXTENT             ompi_ddt_type_extent
#define FCA_DT_GET_TRUE_EXTENT    ompi_ddt_get_true_extent
#define FCA_DT_IS_CONTIGUOUS_MEMORY_LAYOUT(__dtype, __count) \
                                  ompi_ddt_is_contiguous_memory_layout(__dtype, __count)
#endif

BEGIN_C_DECLS

/*
 * FCA library functions.
 * Used to load the library dynamically.
 */

int modular_pow(uint64_t base, uint64_t exponent, uint64_t modulus);

/**
 * FCA data type information
 */
struct mca_coll_fca_dtype_info_t {
    ompi_datatype_t *mpi_dtype;
    size_t mpi_dtype_extent;
    int fca_dtype; /* -1 if invalid */
    size_t fca_dtype_extent;

};
typedef struct mca_coll_fca_dtype_info_t mca_coll_fca_dtype_info_t;

/**
 * FCA operator information
 */
struct mca_coll_fca_op_info_t {
    ompi_op_t *mpi_op;
    int fca_op; /* -1 if invalid */
};
typedef struct mca_coll_fca_op_info_t mca_coll_fca_op_info_t;

#define FCA_MAX_OPS 32 /* Must be large enough to hold all FCA-supported operators */

/**
 * Globally exported structure
 */
struct mca_coll_fca_component_t {
    /** Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /** MCA parameter: Priority of this component */
    int fca_priority;

    /** MCA parameter: Verbose level of this component */
    int fca_verbose;

    /** MCA parameter: Comm_mLid */
    char *fca_comm_mlid;

    /** MCA parameter: Comm_mGid */
    char *fca_comm_mgid;

    /** MCA parameter: FCA_Mlid */
    char *fca_fmm_mlid;

    /** MCA parameter: Path to fca spec file */
    char* fca_spec_file;

    /** MCA parameter: FCA device */
    char* fca_dev;

    /** MCA parameter: Enable FCA */
    int   fca_enable;

    /** MCA parameter: Enable FCA Barrier */
    int   fca_enable_barrier;

    /** MCA parameter: Enable FCA Bcast */
    int   fca_enable_bcast;

    /** MCA parameter: Enable FCA Reduce */
    int   fca_enable_reduce;

    /** MCA parameter: Enable FCA Reduce_Scatter */
    int   fca_enable_reduce_scatter;

    /** MCA parameter: Enable FCA Allreduce */
    int   fca_enable_allreduce;

    /** MCA parameter: Enable FCA Allgather */
    int   fca_enable_allgather;

    /** MCA parameter: Enable FCA Allgatherv */
    int   fca_enable_allgatherv;

    /** MCA parameter: Enable FCA Gather */
    int   fca_enable_gather;

    /** MCA parameter: Enable FCA Gatherv */
    int   fca_enable_gatherv;

    /** MCA parameter: Enable FCA AlltoAll */
    int   fca_enable_alltoall;

    /** MCA parameter: Enable FCA AlltoAllv */
    int   fca_enable_alltoallv;

    /** MCA parameter: Enable FCA AlltoAllw */
    int   fca_enable_alltoallw;

    /** MCA parameter: FCA NP */
    int   fca_np;

    /** MCA parameter: Enable FCA comm cache */
    int   fca_enable_cache;

    /** MCA parameter: Enable parallel hash calc */
    int fca_parallel_hash_calc;

    /* r/o MCA parameter: compiletime libfca version */
    char* compiletime_version;

    /* r/o MCA parameter: runtime libfca version */
    char* runtime_version;

    /** Some statistics counters */
    double fca_total_work_time;
    double fca_work_time_parallel;
    double fca_work_time_sequency;
    int fca_cache_hit;
    int fca_cache_miss;
    int fca_hash_hit;
    int fca_hash_miss;
    int fca_max_deep_in_cache;

    /** MCA parameter: Enable hash for cache */
    int fca_enable_hash;

    /* FCA global stuff */
    fca_t *fca_context;                                 /* FCA context handle */
    mca_coll_fca_dtype_info_t fca_dtypes[FCA_DT_MAX_PREDEFINED]; /* FCA dtype translation */
    mca_coll_fca_op_info_t fca_reduce_ops[FCA_MAX_OPS]; /* FCA op translation */

    /* communicator cache */
    opal_list_t c_cache;

    /* pointer to primes */
    int *fca_primes;

    /** MCA parameter hash table size*/
    int fca_hash_size;

    /** MCA parameter hash table size*/
    int fca_number_of_primes;

    /* Pointer to hash  */
    opal_list_t **fca_hash;

};
typedef struct mca_coll_fca_component_t mca_coll_fca_component_t;

OMPI_MODULE_DECLSPEC extern mca_coll_fca_component_t mca_coll_fca_component;

struct mca_coll_fca_comm_wrap_t {
    opal_object_t super;
    fca_comm_t *fca_comm;
    int comm_id, rank;
};
typedef struct mca_coll_fca_comm_wrap_t mca_coll_fca_comm_wrap_t;
OBJ_CLASS_DECLARATION(mca_coll_fca_comm_wrap_t);

struct mca_coll_fca_c_cache_item_t {
    opal_list_item_t super;
    int size;
    ompi_communicator_t *comm;
    mca_coll_fca_comm_wrap_t *fca_comm_wrap;
};
typedef struct mca_coll_fca_c_cache_item_t mca_coll_fca_c_cache_item_t;
OBJ_CLASS_DECLARATION(mca_coll_fca_c_cache_item_t);


/**
 * FCA enabled communicator
 */
struct mca_coll_fca_module_t {
    mca_coll_base_module_t super;

    MPI_Comm            comm;
    int                 rank;
    int                 local_proc_idx;
    int                 num_local_procs;
    int                 *local_ranks;
    fca_comm_t          *fca_comm;
    fca_comm_desc_t     fca_comm_desc;
    fca_comm_caps_t     fca_comm_caps;

    /* Saved handlers - for fallback */
    mca_coll_base_module_reduce_fn_t previous_reduce;
    mca_coll_base_module_t *previous_reduce_module;
    mca_coll_base_module_allreduce_fn_t previous_allreduce;
    mca_coll_base_module_t *previous_allreduce_module;
    mca_coll_base_module_bcast_fn_t previous_bcast;
    mca_coll_base_module_t *previous_bcast_module;
    mca_coll_base_module_barrier_fn_t previous_barrier;
    mca_coll_base_module_t *previous_barrier_module;
    mca_coll_base_module_allgather_fn_t previous_allgather;
    mca_coll_base_module_t *previous_allgather_module;
    mca_coll_base_module_allgatherv_fn_t previous_allgatherv;
    mca_coll_base_module_t *previous_allgatherv_module;
    mca_coll_base_module_alltoall_fn_t previous_alltoall;
    mca_coll_base_module_t *previous_alltoall_module;
    mca_coll_base_module_alltoallv_fn_t previous_alltoallv;
    mca_coll_base_module_t *previous_alltoallv_module;
    mca_coll_base_module_alltoallw_fn_t previous_alltoallw;
    mca_coll_base_module_t *previous_alltoallw_module;
    mca_coll_base_module_gather_fn_t previous_gather;
    mca_coll_base_module_t *previous_gather_module;
    mca_coll_base_module_gatherv_fn_t previous_gatherv;
    mca_coll_base_module_t *previous_gatherv_module;
    mca_coll_base_module_reduce_scatter_fn_t previous_reduce_scatter;
    mca_coll_base_module_t *previous_reduce_scatter_module;
#if OMPI_FCA_VERSION >= 30
    mca_coll_base_module_scatter_fn_t previous_scatter;
    mca_coll_base_module_t *previous_scatter_module;
#endif
};
typedef struct mca_coll_fca_module_t mca_coll_fca_module_t;

OBJ_CLASS_DECLARATION(mca_coll_fca_module_t);


/* API functions */
int mca_coll_fca_init_query(bool enable_progress_threads, bool enable_mpi_threads);
mca_coll_base_module_t *mca_coll_fca_comm_query(struct ompi_communicator_t *comm, int *priority);
int mca_coll_fca_get_fca_lib(struct ompi_communicator_t *comm);


/* Collective functions */
int mca_coll_fca_allreduce(const void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);
int mca_coll_fca_bcast(void *buff, int count, struct ompi_datatype_t *datatype,
                       int root, struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module);

int mca_coll_fca_reduce(const void *sbuf, void* rbuf, int count,
                        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_fca_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_fca_allgather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                           void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_fca_allgatherv(const void *sbuf, int scount,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, const int *rcounts, const int *disps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_fca_alltoall(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_fca_alltoallv(const void *sbuf, const int *scounts, const int *sdisps,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, const int *rcounts, const int *rdisps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_fca_alltoallw(const void *sbuf, const int *scounts, const int *sdisps,
                           struct ompi_datatype_t * const *sdtypes,
                           void *rbuf, const int *rcounts, const int *rdisps,
                           struct ompi_datatype_t * const *rdtypes,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_fca_gather(const void *sbuf, int scount,
                        struct ompi_datatype_t *sdtype,
                        void *rbuf, int rcount,
                        struct ompi_datatype_t *rdtype,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_fca_gatherv(const void *sbuf, int scount,
                         struct ompi_datatype_t *sdtype,
                         void *rbuf, const int *rcounts, const int *disps,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_fca_reduce_scatter(const void *sbuf, void *rbuf, const int *rcounts,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module);

END_C_DECLS

#endif
