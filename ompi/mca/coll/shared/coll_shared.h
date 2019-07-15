/*
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_SM_EXPORT_H
#define MCA_COLL_SM_EXPORT_H

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

BEGIN_C_DECLS

/**
 * Structure to hold the shared coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * shared-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
typedef struct mca_coll_shared_component_t {
    /** Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /** MCA parameter: Priority of this component */
    int shared_priority;
} mca_coll_shared_component_t;

/** Coll shared module */
typedef struct mca_coll_shared_module_t {
    /** Base module */
    mca_coll_base_module_t super;

    /* Whether this module has been lazily initialized or not yet */
    bool enabled;

    /* Shared memory window of data buf */
    MPI_Win sm_data_win;
    /* Address array of data buf */
    char **data_buf;
    size_t data_buf_size;

    /* Shared memory control buf */
    int *sm_ctrl_ptr;
    MPI_Win sm_ctrl_win;
    /* Address array of control buf */
    int **ctrl_buf;

    /* Identify which ctrl_buf is used in the MPI_Barrier */
    int barrier_tag;

} mca_coll_shared_module_t;
OBJ_CLASS_DECLARATION(mca_coll_shared_module_t);

/**
 * Global component instance
 */
OMPI_MODULE_DECLSPEC extern mca_coll_shared_component_t
    mca_coll_shared_component;

/*
 * coll module functions
 */
int mca_coll_shared_init_query(bool enable_progress_threads,
                               bool enable_mpi_threads);

mca_coll_base_module_t *mca_coll_shared_comm_query(struct
                                                   ompi_communicator_t
                                                   *comm, int *priority);

/* Lazily enable a module (since it involves expensive/slow mmap
   allocation, etc.) */
int mca_coll_shared_lazy_enable(mca_coll_base_module_t * module,
                                struct ompi_communicator_t *comm,
                                size_t data_buf_size);

void mca_coll_shared_attach_data_buf(mca_coll_shared_module_t *
                                     shared_module,
                                     struct ompi_communicator_t *comm,
                                     size_t data_buf_size);

void mca_coll_shared_setup_ctrl_buf(mca_coll_shared_module_t *
                                    shared_module,
                                    struct ompi_communicator_t *comm);

int mac_coll_shared_barrier_intra(struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t * module);

int mca_coll_shared_reduce_intra(const void *sbuf, void *rbuf, int count,
                                 struct ompi_datatype_t *dtype,
                                 struct ompi_op_t *op,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t * module);

int mca_coll_shared_reduce_ring_intra(const void *sbuf, void *rbuf,
                                      int count,
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op, int root,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t * module);

int mca_coll_shared_allreduce_intra(const void *sbuf, void *rbuf,
                                    int count,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t * module);

int mca_coll_shared_allreduce_ring_intra(const void *sbuf,
                                         void *rbuf, int count,
                                         struct ompi_datatype_t
                                         *dtype, struct ompi_op_t *op, struct ompi_communicator_t
                                         *comm,
                                         mca_coll_base_module_t * module);
                                         
int mca_coll_shared_bcast_intra(void *buff, int count,
                                struct ompi_datatype_t *dtype, int root,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t * module);

int mca_coll_shared_bcast_linear_intra(void *buff, int count,
                                       struct ompi_datatype_t *dtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t * module);


END_C_DECLS
#endif                          /* MCA_COLL_SM_EXPORT_H */
