/**
  Copyright (c) 2010 Voltaire, Inc. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#ifndef MCA_COLL_FCA_H
#define MCA_COLL_FCA_H

#include <fca_api.h>

#include "ompi_config.h"
#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"
#include "coll_fca_debug.h"

#ifdef OMPI_DATATYPE_MAX_PREDEFINED
#define FCA_DT_MAX_PREDEFINED OMPI_DATATYPE_MAX_PREDEFINED
#define FCA_DT_GET_TRUE_EXTENT	ompi_datatype_get_true_extent
#define FCA_DT_IS_CONTIGUOUS_MEMORY_LAYOUT ompi_datatype_is_contiguous_memory_layout
#else
#define FCA_DT_MAX_PREDEFINED DT_MAX_PREDEFINED
#define FCA_DT_GET_TRUE_EXTENT	ompi_ddt_get_true_extent
#define FCA_DT_IS_CONTIGUOUS_MEMORY_LAYOUT ompi_ddt_is_contiguous_memory_layout
#endif

#ifdef OMPI_PROC_FLAG_LOCAL
#define FCA_IS_LOCAL_PROCESS(n) ((n) & OMPI_PROC_FLAG_LOCAL)
#else
#define FCA_IS_LOCAL_PROCESS(n) OPAL_PROC_ON_LOCAL_NODE(n)
#endif


BEGIN_C_DECLS

/*
 * FCA library functions.
 * Used to load the library dynamically.
 */

struct mca_coll_fca_fca_ops_t {

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
    int (*do_allgather)(fca_comm_t *comm, fca_gather_spec_t *spec);
    int (*do_allgatherv)(fca_comm_t *comm, fca_gatherv_spec_t *spec);
    int (*do_barrier)(fca_comm_t *comm);

    /* Helper functions */
    unsigned long (*get_version)(void);
    char * (*get_version_string)(void);
    int (*maddr_ib_pton)(const char *mlid_str, const char *mgid_str, fca_mcast_addr_t *dst);
    int (*maddr_inet_pton)(int af, const char *src, fca_mcast_addr_t *dst);
    fca_init_spec_t *(*parse_spec_file)(char* spec_ini_file);
    void (*free_init_spec)(fca_init_spec_t *fca_init_spec);
    int (*translate_mpi_op)(char *mpi_op);
    int (*translate_mpi_dtype)(char *mpi_dtype);
    int (*get_dtype_size)(int dtype);
    const char* (*strerror)(int code);
};
typedef struct mca_coll_fca_fca_ops_t mca_coll_fca_fca_ops_t;

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

    /** MCA parameter: Path to libfca.so */
    char* fca_lib_path;

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

    /* FCA global stuff */
    void *fca_lib_handle;                              /* FCA dynamic library */
    mca_coll_fca_fca_ops_t fca_ops;                         /* FCA operations */
    fca_t *fca_context;                                 /* FCA context handle */
    mca_coll_fca_dtype_info_t fca_dtypes[FCA_DT_MAX_PREDEFINED]; /* FCA dtype translation */
    mca_coll_fca_op_info_t fca_reduce_ops[FCA_MAX_OPS]; /* FCA op translation */
};
typedef struct mca_coll_fca_component_t mca_coll_fca_component_t;

OMPI_MODULE_DECLSPEC extern mca_coll_fca_component_t mca_coll_fca_component;


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
};
typedef struct mca_coll_fca_module_t mca_coll_fca_module_t;

OBJ_CLASS_DECLARATION(mca_coll_fca_module_t);


/* API functions */
int mca_coll_fca_init_query(bool enable_progress_threads, bool enable_mpi_threads);
mca_coll_base_module_t *mca_coll_fca_comm_query(struct ompi_communicator_t *comm, int *priority);
int mca_coll_fca_get_fca_lib(struct ompi_communicator_t *comm);


/* Collective functions */
int mca_coll_fca_allreduce(void *sbuf, void *rbuf, int count,
                           struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);
int mca_coll_fca_bcast(void *buff, int count, struct ompi_datatype_t *datatype,
                       int root, struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module);

int mca_coll_fca_reduce(void *sbuf, void* rbuf, int count,
                        struct ompi_datatype_t *dtype, struct ompi_op_t *op,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_fca_barrier(struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_fca_allgather(void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                           void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_fca_allgatherv(void *sbuf, int scount,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, int *rcounts, int *disps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_fca_alltoall(void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module);

int mca_coll_fca_alltoallv(void *sbuf, int *scounts, int *sdisps,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, int *rcounts, int *rdisps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_fca_alltoallw(void *sbuf, int *scounts, int *sdisps,
                           struct ompi_datatype_t **sdtypes,
                           void *rbuf, int *rcounts, int *rdisps,
                           struct ompi_datatype_t **rdtypes,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module);

int mca_coll_fca_gather(void *sbuf, int scount,
                        struct ompi_datatype_t *sdtype,
                        void *rbuf, int rcount,
                        struct ompi_datatype_t *rdtype,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_fca_gatherv(void *sbuf, int scount,
                         struct ompi_datatype_t *sdtype,
                         void *rbuf, int *rcounts, int *disps,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_fca_reduce_scatter(void *sbuf, void *rbuf, int *rcounts,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module);

END_C_DECLS

#endif
