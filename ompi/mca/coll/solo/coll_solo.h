/**
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_SOLO_EXPORT_H
#define MCA_COLL_SOLO_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"
#include "ompi/include/mpi.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "opal/util/info.h"
#include "ompi/op/op.h"
#include "opal/runtime/opal_progress.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_solo_mpool.h"

BEGIN_C_DECLS
/**
 * Structure to hold the solo coll component.  First it holds the base coll component, and then 
 * holds a bunch of solo-coll-component-specific stuff (e.g., current MCA param values).
 */
    typedef struct mca_coll_solo_component_t {
    /* Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /* MCA parameters */
    /* Priority of the solo module */
    int solo_priority;
    /* The size of data_bufs in the static_win */
    uint32_t static_block_size;
    uint32_t mpool_small_block_size;
    uint32_t mpool_small_block_num;
    uint32_t mpool_large_block_size;
    uint32_t mpool_large_block_num;

    /* Shared memory pool */
    mca_coll_solo_mpool_t *solo_mpool;
} mca_coll_solo_component_t;

/* Coll solo module */
typedef struct mca_coll_solo_module_t {
    /* Base module */
    mca_coll_base_module_t super;

    /* Whether this module has been lazily initialized or not yet */
    bool enabled;

    /**
     * osc alrogithms attach memory blocks to this bynamic window and use it to perform one-sided 
     * communications. 
     */
    MPI_Win dynamic_win;

    /**
     * This window is created by ompi_win_allocate_shared such that each process contains a shared 
     * memory data buffer, and this data buffer is divided into two parts - ctrl_bufs and data_bufs.
     */
    MPI_Win static_win;
    /** 
     * The first 4 * opal_cache_line_size bytes in the shared memory data buffer in static_win, used
     * to store control messages.
     */
    char **ctrl_bufs;
    /** 
     * The rest of the shared memory data buffer in static_win, which is intent to be used to 
     * tranfer very small messages. Its size is set by static_block_size.
     */
    char **data_bufs;

    /* Identify which ctrl_buf is currently used in mac_coll_solo_barrier_intra. */
    int barrier_tag;
} mca_coll_solo_module_t;
OBJ_CLASS_DECLARATION(mca_coll_solo_module_t);

/**
 * Global component instance
 */
OMPI_MODULE_DECLSPEC extern mca_coll_solo_component_t mca_coll_solo_component;

/**
 * coll module functions
 */
int mca_coll_solo_init_query(bool enable_progress_threads, bool enable_mpi_threads);

mca_coll_base_module_t *mca_coll_solo_comm_query(struct ompi_communicator_t *comm, int *priority);

/* Lazily enable a module (since it involves expensive memory allocation, etc.) */
int mca_coll_solo_lazy_enable(mca_coll_base_module_t * module, struct ompi_communicator_t *comm);

/* Attach a memory block to the dynamic_win of a communicator */
char **mca_coll_solo_attach_buf(mca_coll_solo_module_t * solo_module,
                                struct ompi_communicator_t *comm,
                                char *local_buf, 
                                size_t local_buf_size);

/* Detach a memory block from the dynamic_win of a communicator */
void mca_coll_solo_detach_buf(mca_coll_solo_module_t * solo_module,
                              struct ompi_communicator_t *comm,
                              char *local_buf, 
                              char ***attached_bufs);

/* Setup and initialize the static_win of a communicator */
void mca_coll_solo_setup_static_win(mca_coll_solo_module_t *solo_module,
                                    struct ompi_communicator_t *comm, 
                                    size_t data_buf_size);

/* MPI_Barrier algorithms */
int mac_coll_solo_barrier_intra(struct ompi_communicator_t *comm, 
                                mca_coll_base_module_t * module);

/* MPI_Bcast algorithms */
int mca_coll_solo_bcast_intra(void *buff, int count,
                              struct ompi_datatype_t *dtype, 
                              int root,
                              struct ompi_communicator_t *comm, 
                              mca_coll_base_module_t * module);

int mca_coll_solo_bcast_linear_intra_memcpy(void *buff, int count, 
                                            struct ompi_datatype_t *dtype, 
                                            int root, 
                                            struct ompi_communicator_t *comm, 
                                            mca_coll_base_module_t * module);

int mca_coll_solo_bcast_linear_intra_osc(void *buff, int count,
                                         struct ompi_datatype_t *dtype,
                                         int root, 
                                         struct ompi_communicator_t *comm, 
                                         mca_coll_base_module_t * module);

/* MPI_Reduce algorithms */
int mca_coll_solo_reduce_intra(const void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               int root,
                               struct ompi_communicator_t *comm, 
                               mca_coll_base_module_t * module);

int mca_coll_solo_reduce_ring_intra(const void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op, int root,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t * module);

int mca_coll_solo_reduce_ring_intra_memcpy(const void *sbuf, void *rbuf, int count,
                                           struct ompi_datatype_t *dtype,
                                           struct ompi_op_t *op,
                                           int root, 
                                           struct ompi_communicator_t
                                           *comm, mca_coll_base_module_t * module);

int mca_coll_solo_reduce_ring_intra_osc(const void *sbuf, void *rbuf, int count,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op, int root,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t * module);

/* MPI_Allreduce algorithms */
int mca_coll_solo_allreduce_intra(const void *sbuf, void *rbuf, int count,
                                  struct ompi_datatype_t *dtype,
                                  struct ompi_op_t *op,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t * module);

int mca_coll_solo_allreduce_ring_intra_memcpy(const void *sbuf, void *rbuf, int count, 
                                              struct ompi_datatype_t *dtype, 
                                              struct ompi_op_t *op, 
                                              struct ompi_communicator_t *comm, 
                                              mca_coll_base_module_t * module);

int mca_coll_solo_allreduce_ring_intra_osc(const void *sbuf, void *rbuf, int count, 
                                           struct ompi_datatype_t *dtype, 
                                           struct ompi_op_t *op, 
                                           struct ompi_communicator_t *comm, 
                                           mca_coll_base_module_t * module);
END_C_DECLS
#endif                          /* MCA_COLL_SOLO_EXPORT_H */
