/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2024      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_DEMO_EXPORT_H
#define MCA_COLL_DEMO_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/coll/coll.h"

BEGIN_C_DECLS

    /* Globally exported variables */

    OMPI_DECLSPEC extern const mca_coll_base_component_3_0_0_t mca_coll_demo_component;
    extern int mca_coll_demo_priority;
    extern int mca_coll_demo_verbose;


    /* Component functions */

    int mca_coll_demo_init_query(bool enable_progress_threads,
                                 bool enable_mpi_threads);
    mca_coll_base_module_t *
    mca_coll_demo_comm_query(struct ompi_communicator_t *comm, int *priority);

    /* Module functions */

    int mca_coll_demo_allgather_intra(const void *sbuf, size_t scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, size_t rcount,
                                      struct ompi_datatype_t *rdtype,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);
    int mca_coll_demo_allgather_inter(const void *sbuf, size_t scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, size_t rcount,
                                      struct ompi_datatype_t *rdtype,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

    int mca_coll_demo_allgatherv_intra(const void *sbuf, size_t scount,
                                       struct ompi_datatype_t *sdtype,
                                       void * rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                                       struct ompi_datatype_t *rdtype,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);
    int mca_coll_demo_allgatherv_inter(const void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype,
                                       void * rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                                       struct ompi_datatype_t *rdtype,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

    int mca_coll_demo_allreduce_intra(const void *sbuf, void *rbuf, size_t count,
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);
    int mca_coll_demo_allreduce_inter(const void *sbuf, void *rbuf, size_t count,
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

    int mca_coll_demo_alltoall_intra(const void *sbuf, size_t scount,
                                     struct ompi_datatype_t *sdtype,
                                     void* rbuf, size_t rcount,
                                     struct ompi_datatype_t *rdtype,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);
    int mca_coll_demo_alltoall_inter(const void *sbuf, size_t scount,
                                     struct ompi_datatype_t *sdtype,
                                     void* rbuf, size_t rcount,
                                     struct ompi_datatype_t *rdtype,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);

    int mca_coll_demo_alltoallv_intra(const void *sbuf, ompi_count_array_t scounts, ompi_disp_array_t sdisps,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                                      struct ompi_datatype_t *rdtype,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);
    int mca_coll_demo_alltoallv_inter(const void *sbuf, ompi_count_array_t scounts, ompi_disp_array_t sdisps,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                                      struct ompi_datatype_t *rdtype,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

    int mca_coll_demo_alltoallw_intra(const void *sbuf, ompi_count_array_t scounts, ompi_disp_array_t sdisps,
                                      struct ompi_datatype_t **sdtypes,
                                      void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                                      struct ompi_datatype_t **rdtypes,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);
    int mca_coll_demo_alltoallw_inter(void *sbuf, ompi_count_array_t scounts, ompi_disp_array_t sdisps,
                                      struct ompi_datatype_t **sdtypes,
                                      void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                                      struct ompi_datatype_t **rdtypes,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

    int mca_coll_demo_barrier_intra(struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);
    int mca_coll_demo_barrier_inter(struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);

    int mca_coll_demo_bcast_intra(void *buff, size_t count,
                                  struct ompi_datatype_t *datatype,
                                  int root,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module);
    int mca_coll_demo_bcast_inter(void *buff, size_t count,
                                  struct ompi_datatype_t *datatype,
                                  int root,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module);

    int mca_coll_demo_exscan_intra(const void *sbuf, void *rbuf, size_t count,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);
    int mca_coll_demo_exscan_inter(const void *sbuf, void *rbuf, size_t count,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);

    int mca_coll_demo_gather_intra(const void *sbuf, size_t scount,
                                   struct ompi_datatype_t *sdtype, void *rbuf,
                                   size_t rcount, struct ompi_datatype_t *rdtype,
                                   int root, struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);
    int mca_coll_demo_gather_inter(const void *sbuf, size_t scount,
                                   struct ompi_datatype_t *sdtype, void *rbuf,
                                   size_t rcount, struct ompi_datatype_t *rdtype,
                                   int root, struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);

    int mca_coll_demo_gatherv_intra(const void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype, void *rbuf,
                                    ompi_count_array_t rcounts, ompi_disp_array_t disps,
                                    struct ompi_datatype_t *rdtype, int root,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);
    int mca_coll_demo_gatherv_inter(void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype, void *rbuf,
                                    ompi_count_array_t rcounts, ompi_disp_array_t disps,
                                    struct ompi_datatype_t *rdtype, int root,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);

    int mca_coll_demo_reduce_intra(const void *sbuf, void* rbuf, size_t count,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   int root,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);
    int mca_coll_demo_reduce_inter(const void *sbuf, void* rbuf, size_t count,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   int root,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);

    int mca_coll_demo_reduce_scatter_intra(const void *sbuf, void *rbuf,
                                           ompi_count_array_t rcounts,
                                           struct ompi_datatype_t *dtype,
                                           struct ompi_op_t *op,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module);
    int mca_coll_demo_reduce_scatter_inter(const void *sbuf, void *rbuf,
                                           ompi_count_array_t rcounts,
                                           struct ompi_datatype_t *dtype,
                                           struct ompi_op_t *op,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module);

    int mca_coll_demo_scan_intra(const void *sbuf, void *rbuf, size_t count,
                                 struct ompi_datatype_t *dtype,
                                 struct ompi_op_t *op,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module);
    int mca_coll_demo_scan_inter(const void *sbuf, void *rbuf, size_t count,
                                 struct ompi_datatype_t *dtype,
                                 struct ompi_op_t *op,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module);

    int mca_coll_demo_scatter_intra(const void *sbuf, size_t scount,
                                    struct ompi_datatype_t *sdtype, void *rbuf,
                                    size_t rcount, struct ompi_datatype_t *rdtype,
                                    int root, struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);
    int mca_coll_demo_scatter_inter(const void *sbuf, size_t scount,
                                    struct ompi_datatype_t *sdtype, void *rbuf,
                                    size_t rcount, struct ompi_datatype_t *rdtype,
                                    int root, struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);

    int mca_coll_demo_scatterv_intra(const void *sbuf, ompi_count_array_t scounts, ompi_disp_array_t disps,
                                     struct ompi_datatype_t *sdtype,
                                     void* rbuf, size_t rcount,
                                     struct ompi_datatype_t *rdtype, int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);
    int mca_coll_demo_scatterv_inter(const void *sbuf, ompi_count_array_t scounts, ompi_disp_array_t disps,
                                     struct ompi_datatype_t *sdtype,
                                     void* rbuf, size_t rcount,
                                     struct ompi_datatype_t *rdtype, int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);

    struct mca_coll_demo_module_t {
        mca_coll_base_module_t super;

        mca_coll_base_comm_coll_t c_coll;
    };
    typedef struct mca_coll_demo_module_t mca_coll_demo_module_t;
    OBJ_CLASS_DECLARATION(mca_coll_demo_module_t);


END_C_DECLS

#endif /* MCA_COLL_DEMO_EXPORT_H */
