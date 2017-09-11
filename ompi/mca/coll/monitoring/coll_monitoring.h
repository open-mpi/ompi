/*
 * Copyright (c) 2016      Inria.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_MONITORING_H
#define MCA_COLL_MONITORING_H

BEGIN_C_DECLS

#include <ompi_config.h>
#include <ompi/mca/coll/coll.h>
#include <ompi/op/op.h>
#include <ompi/request/request.h>
#include <ompi/datatype/ompi_datatype.h>
#include <ompi/communicator/communicator.h>
#include <ompi/mca/common/monitoring/common_monitoring.h>

struct mca_coll_monitoring_component_t {
    mca_coll_base_component_t super;
    int priority;
};
typedef struct mca_coll_monitoring_component_t mca_coll_monitoring_component_t;

OMPI_DECLSPEC extern mca_coll_monitoring_component_t mca_coll_monitoring_component;

struct mca_coll_monitoring_module_t {
    mca_coll_base_module_t super;
    mca_coll_base_comm_coll_t real;
    mca_monitoring_coll_data_t*data;
    int32_t is_initialized;
};
typedef struct mca_coll_monitoring_module_t mca_coll_monitoring_module_t;
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_coll_monitoring_module_t);

/* 
 * Coll interface functions
 */

/* Blocking */
extern int mca_coll_monitoring_allgather(const void *sbuf, int scount,
                                         struct ompi_datatype_t *sdtype,
                                         void *rbuf, int rcount,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module);

extern int mca_coll_monitoring_allgatherv(const void *sbuf, int scount,
                                          struct ompi_datatype_t *sdtype,
                                          void *rbuf, const int *rcounts,
                                          const int *disps,
                                          struct ompi_datatype_t *rdtype,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module);

extern int mca_coll_monitoring_allreduce(const void *sbuf, void *rbuf, int count,
                                         struct ompi_datatype_t *dtype,
                                         struct ompi_op_t *op,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module);

extern int mca_coll_monitoring_alltoall(const void *sbuf, int scount,
                                        struct ompi_datatype_t *sdtype,
                                        void *rbuf, int rcount,
                                        struct ompi_datatype_t *rdtype,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);

extern int mca_coll_monitoring_alltoallv(const void *sbuf, const int *scounts,
                                         const int *sdisps,
                                         struct ompi_datatype_t *sdtype,
                                         void *rbuf, const int *rcounts,
                                         const int *rdisps,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module);

extern int mca_coll_monitoring_alltoallw(const void *sbuf, const int *scounts,
                                         const int *sdisps,
                                         struct ompi_datatype_t * const *sdtypes,
                                         void *rbuf, const int *rcounts,
                                         const int *rdisps,
                                         struct ompi_datatype_t * const *rdtypes,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module);

extern int mca_coll_monitoring_barrier(struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

extern int mca_coll_monitoring_bcast(void *buff, int count,
                                     struct ompi_datatype_t *datatype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);

extern int mca_coll_monitoring_exscan(const void *sbuf, void *rbuf, int count,
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

extern int mca_coll_monitoring_gather(const void *sbuf, int scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                      int root, struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

extern int mca_coll_monitoring_gatherv(const void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, const int *rcounts, const int *disps,
                                       struct ompi_datatype_t *rdtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

extern int mca_coll_monitoring_reduce(const void *sbuf, void *rbuf, int count,
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op,
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);

extern int mca_coll_monitoring_reduce_scatter(const void *sbuf, void *rbuf,
                                              const int *rcounts,
                                              struct ompi_datatype_t *dtype,
                                              struct ompi_op_t *op,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module);

extern int mca_coll_monitoring_reduce_scatter_block(const void *sbuf, void *rbuf,
                                                    int rcount,
                                                    struct ompi_datatype_t *dtype,
                                                    struct ompi_op_t *op,
                                                    struct ompi_communicator_t *comm,
                                                    mca_coll_base_module_t *module);

extern int mca_coll_monitoring_scan(const void *sbuf, void *rbuf, int count,
                                    struct ompi_datatype_t *dtype,
                                    struct ompi_op_t *op,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);

extern int mca_coll_monitoring_scatter(const void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, int rcount,
                                       struct ompi_datatype_t *rdtype,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);

extern int mca_coll_monitoring_scatterv(const void *sbuf, const int *scounts, const int *disps,
                                        struct ompi_datatype_t *sdtype,
                                        void *rbuf, int rcount,
                                        struct ompi_datatype_t *rdtype,
                                        int root,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);

/* Nonblocking */
extern int mca_coll_monitoring_iallgather(const void *sbuf, int scount,
                                          struct ompi_datatype_t *sdtype,
                                          void *rbuf, int rcount,
                                          struct ompi_datatype_t *rdtype,
                                          struct ompi_communicator_t *comm,
                                          ompi_request_t ** request,
                                          mca_coll_base_module_t *module);

extern int mca_coll_monitoring_iallgatherv(const void *sbuf, int scount,
                                           struct ompi_datatype_t *sdtype,
                                           void *rbuf, const int *rcounts,
                                           const int *disps,
                                           struct ompi_datatype_t *rdtype,
                                           struct ompi_communicator_t *comm,
                                           ompi_request_t ** request,
                                           mca_coll_base_module_t *module);

extern int mca_coll_monitoring_iallreduce(const void *sbuf, void *rbuf, int count,
                                          struct ompi_datatype_t *dtype,
                                          struct ompi_op_t *op,
                                          struct ompi_communicator_t *comm,
                                          ompi_request_t ** request,
                                          mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ialltoall(const void *sbuf, int scount,
                                         struct ompi_datatype_t *sdtype,
                                         void *rbuf, int rcount,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
                                         ompi_request_t ** request,
                                         mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ialltoallv(const void *sbuf, const int *scounts,
                                          const int *sdisps,
                                          struct ompi_datatype_t *sdtype,
                                          void *rbuf, const int *rcounts,
                                          const int *rdisps,
                                          struct ompi_datatype_t *rdtype,
                                          struct ompi_communicator_t *comm,
                                          ompi_request_t ** request,
                                          mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ialltoallw(const void *sbuf, const int *scounts,
                                          const int *sdisps,
                                          struct ompi_datatype_t * const *sdtypes,
                                          void *rbuf, const int *rcounts,
                                          const int *rdisps,
                                          struct ompi_datatype_t * const *rdtypes,
                                          struct ompi_communicator_t *comm,
                                          ompi_request_t ** request,
                                          mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ibarrier(struct ompi_communicator_t *comm,
                                        ompi_request_t ** request,
                                        mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ibcast(void *buff, int count,
                                      struct ompi_datatype_t *datatype,
                                      int root,
                                      struct ompi_communicator_t *comm,
                                      ompi_request_t ** request,
                                      mca_coll_base_module_t *module);

extern int mca_coll_monitoring_iexscan(const void *sbuf, void *rbuf, int count,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm,
                                       ompi_request_t ** request,
                                       mca_coll_base_module_t *module);

extern int mca_coll_monitoring_igather(const void *sbuf, int scount,
                                       struct ompi_datatype_t *sdtype,
                                       void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                       int root, struct ompi_communicator_t *comm,
                                       ompi_request_t ** request,
                                       mca_coll_base_module_t *module);

extern int mca_coll_monitoring_igatherv(const void *sbuf, int scount,
                                        struct ompi_datatype_t *sdtype,
                                        void *rbuf, const int *rcounts, const int *disps,
                                        struct ompi_datatype_t *rdtype,
                                        int root,
                                        struct ompi_communicator_t *comm,
                                        ompi_request_t ** request,
                                        mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ireduce(const void *sbuf, void *rbuf, int count,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       int root,
                                       struct ompi_communicator_t *comm,
                                       ompi_request_t ** request,
                                       mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ireduce_scatter(const void *sbuf, void *rbuf,
                                               const int *rcounts,
                                               struct ompi_datatype_t *dtype,
                                               struct ompi_op_t *op,
                                               struct ompi_communicator_t *comm,
                                               ompi_request_t ** request,
                                               mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ireduce_scatter_block(const void *sbuf, void *rbuf,
                                                     int rcount,
                                                     struct ompi_datatype_t *dtype,
                                                     struct ompi_op_t *op,
                                                     struct ompi_communicator_t *comm,
                                                     ompi_request_t ** request,
                                                     mca_coll_base_module_t *module);

extern int mca_coll_monitoring_iscan(const void *sbuf, void *rbuf, int count,
                                     struct ompi_datatype_t *dtype,
                                     struct ompi_op_t *op,
                                     struct ompi_communicator_t *comm,
                                     ompi_request_t ** request,
                                     mca_coll_base_module_t *module);

extern int mca_coll_monitoring_iscatter(const void *sbuf, int scount,
                                        struct ompi_datatype_t *sdtype,
                                        void *rbuf, int rcount,
                                        struct ompi_datatype_t *rdtype,
                                        int root,
                                        struct ompi_communicator_t *comm,
                                        ompi_request_t ** request,
                                        mca_coll_base_module_t *module);

extern int mca_coll_monitoring_iscatterv(const void *sbuf, const int *scounts, const int *disps,
                                         struct ompi_datatype_t *sdtype,
                                         void *rbuf, int rcount,
                                         struct ompi_datatype_t *rdtype,
                                         int root,
                                         struct ompi_communicator_t *comm,
                                         ompi_request_t ** request,
                                         mca_coll_base_module_t *module);

/* Neighbor */
extern int mca_coll_monitoring_neighbor_allgather(const void *sbuf, int scount,
                                                  struct ompi_datatype_t *sdtype, void *rbuf,
                                                  int rcount, struct ompi_datatype_t *rdtype,
                                                  struct ompi_communicator_t *comm,
                                                  mca_coll_base_module_t *module);

extern int mca_coll_monitoring_neighbor_allgatherv(const void *sbuf, int scount,
                                                   struct ompi_datatype_t *sdtype, void * rbuf,
                                                   const int *rcounts, const int *disps,
                                                   struct ompi_datatype_t *rdtype,
                                                   struct ompi_communicator_t *comm,
                                                   mca_coll_base_module_t *module);

extern int mca_coll_monitoring_neighbor_alltoall(const void *sbuf, int scount,
                                                 struct ompi_datatype_t *sdtype,
                                                 void *rbuf, int rcount,
                                                 struct ompi_datatype_t *rdtype,
                                                 struct ompi_communicator_t *comm,
                                                 mca_coll_base_module_t *module);

extern int mca_coll_monitoring_neighbor_alltoallv(const void *sbuf, const int *scounts,
                                                  const int *sdisps,
                                                  struct ompi_datatype_t *sdtype,
                                                  void *rbuf, const int *rcounts,
                                                  const int *rdisps,
                                                  struct ompi_datatype_t *rdtype,
                                                  struct ompi_communicator_t *comm,
                                                  mca_coll_base_module_t *module);

extern int mca_coll_monitoring_neighbor_alltoallw(const void *sbuf, const int *scounts,
                                                  const MPI_Aint *sdisps,
                                                  struct ompi_datatype_t * const *sdtypes,
                                                  void *rbuf, const int *rcounts,
                                                  const MPI_Aint *rdisps,
                                                  struct ompi_datatype_t * const *rdtypes,
                                                  struct ompi_communicator_t *comm,
                                                  mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ineighbor_allgather(const void *sbuf, int scount,
                                                   struct ompi_datatype_t *sdtype, void *rbuf,
                                                   int rcount, struct ompi_datatype_t *rdtype,
                                                   struct ompi_communicator_t *comm,
                                                   ompi_request_t ** request,
                                                   mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ineighbor_allgatherv(const void *sbuf, int scount,
                                                    struct ompi_datatype_t *sdtype,
                                                    void * rbuf, const int *rcounts,
                                                    const int *disps,
                                                    struct ompi_datatype_t *rdtype,
                                                    struct ompi_communicator_t *comm,
                                                    ompi_request_t ** request,
                                                    mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ineighbor_alltoall(const void *sbuf, int scount,
                                                  struct ompi_datatype_t *sdtype, void *rbuf,
                                                  int rcount, struct ompi_datatype_t *rdtype,
                                                  struct ompi_communicator_t *comm,
                                                  ompi_request_t ** request,
                                                  mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ineighbor_alltoallv(const void *sbuf, const int *scounts,
                                                   const int *sdisps,
                                                   struct ompi_datatype_t *sdtype,
                                                   void *rbuf, const int *rcounts,
                                                   const int *rdisps,
                                                   struct ompi_datatype_t *rdtype,
                                                   struct ompi_communicator_t *comm,
                                                   ompi_request_t ** request,
                                                   mca_coll_base_module_t *module);

extern int mca_coll_monitoring_ineighbor_alltoallw(const void *sbuf, const int *scounts,
                                                   const MPI_Aint *sdisps,
                                                   struct ompi_datatype_t * const *sdtypes,
                                                   void *rbuf, const int *rcounts,
                                                   const MPI_Aint *rdisps,
                                                   struct ompi_datatype_t * const *rdtypes,
                                                   struct ompi_communicator_t *comm,
                                                   ompi_request_t ** request,
                                                   mca_coll_base_module_t *module);

END_C_DECLS

#endif  /* MCA_COLL_MONITORING_H */
