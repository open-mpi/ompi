/*
 * $HEADER$
 */

#ifndef MCA_COLL_BASIC_EXPORT_H
#define MCA_COLL_BASIC_EXPORT_H

#include "lam_config.h"

#include "mca/mca.h"
#include "mca/mpi/coll/coll.h"


/*
 * Globally exported variable
 */

extern const mca_coll_base_module_1_0_0_t mca_coll_basic_module;


/*
 * coll API functions
 */

#ifdef __cplusplus
extern "C" {
#endif

  /* basic's query function is prototyped in <mca/mpi/coll/coll.h>
     because other modules may invoke it. */

  int mca_coll_basic_thread_query(int *thread_min, int *thread_max);

  int mca_coll_basic_init(MPI_Comm comm, const mca_coll_1_0_0_t **new_coll);
  int mca_coll_basic_finalize(MPI_Comm comm);

  int mca_coll_basic_allgather(void *sbuf, int scount, 
                               MPI_Datatype sdtype, void *rbuf, 
                               int rcount, MPI_Datatype rdtype, 
                               MPI_Comm comm);
  int mca_coll_basic_allgatherv(void *sbuf, int scount, 
                                MPI_Datatype sdtype, void * rbuf, 
                                int *rcounts, int *disps, 
                                MPI_Datatype rdtype, 
                                MPI_Comm comm);
  int mca_coll_basic_allreduce(void *sbuf, void *rbuf, int count, 
                               MPI_Datatype dtype, MPI_Op op, 
                               MPI_Comm comm);
  int mca_coll_basic_alltoall(void *sbuf, int scount, 
                              MPI_Datatype sdtype, void* rbuf, 
                              int rcount, MPI_Datatype rdtype, 
                              MPI_Comm comm);
  int mca_coll_basic_alltoallv(void *sbuf, int *scounts, 
                               int *sdisps, MPI_Datatype sdtype, 
                               void *rbuf, int *rcounts, 
                               int *rdisps, MPI_Datatype rdtype, 
                               MPI_Comm comm);
  int mca_coll_basic_alltoallw(void *sbuf, int *scounts, 
                               int *sdisps, MPI_Datatype *sdtypes, 
                               void *rbuf, int *rcounts, 
                               int *rdisps, MPI_Datatype *rdtypes, 
                               MPI_Comm comm);
  int mca_coll_basic_barrier_lin(MPI_Comm comm);
  int mca_coll_basic_barrier_log(MPI_Comm comm);
  int mca_coll_basic_bcast_lin(void *buff, int count, 
                               MPI_Datatype datatype, int root, 
                               MPI_Comm comm);
  int mca_coll_basic_bcast_log(void *buff, int count, 
                               MPI_Datatype datatype, int root, 
                               MPI_Comm comm);
  int mca_coll_basic_exscan(void *sbuf, void *rbuf, int count, 
                            MPI_Datatype dtype, MPI_Op op, 
                            MPI_Comm comm);
  int mca_coll_basic_gather(void *sbuf, int scount, 
                            MPI_Datatype sdtype, void *rbuf, 
                            int rcount, MPI_Datatype rdtype, 
                            int root, MPI_Comm comm);
  int mca_coll_basic_gatherv(void *sbuf, int scount, 
                             MPI_Datatype sdtype, void *rbuf, 
                             int *rcounts, int *disps, 
                             MPI_Datatype rdtype, int root, 
                             MPI_Comm comm);
  int mca_coll_basic_reduce_lin(void *sbuf, void* rbuf, int count, 
                                MPI_Datatype dtype, MPI_Op op, 
                                int root, MPI_Comm comm);
  int mca_coll_basic_reduce_log(void *sbuf, void* rbuf, int count, 
                                MPI_Datatype dtype, MPI_Op op, 
                                int root, MPI_Comm comm);
  int mca_coll_basic_reduce_scatter(void *sbuf, void *rbuf, 
                                    int *rcounts, 
                                    MPI_Datatype dtype, 
                                    MPI_Op op, MPI_Comm comm);
  int mca_coll_basic_scan(void *sbuf, void *rbuf, int count, 
                          MPI_Datatype dtype, MPI_Op op, 
                          MPI_Comm comm);
  int mca_coll_basic_scatter(void *sbuf, int scount, 
                             MPI_Datatype sdtype, void *rbuf, 
                             int rcount, MPI_Datatype rdtype, 
                             int root, MPI_Comm comm);
  int mca_coll_basic_scatterv(void *sbuf, int *scounts, 
                              int *disps, MPI_Datatype sdtype, 
                              void* rbuf, int rcount, 
                              MPI_Datatype rdtype, int root, 
                              MPI_Comm comm);
#ifdef __cplusplus
}
#endif

#endif /* MCA_COLL_BASIC_EXPORT_H */
