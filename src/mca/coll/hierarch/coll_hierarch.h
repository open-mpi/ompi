/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_HIERARCH_EXPORT_H
#define MCA_COLL_HIERARCH_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/coll/coll.h"
#include "request/request.h"
#include "mca/pml/pml.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Globally exported variable
 */

extern const mca_coll_base_component_1_0_0_t mca_coll_hierarch_component;
extern int mca_coll_hierarch_priority_param;
extern int mca_coll_hierarch_verbose_param;
extern int mca_coll_hierarch_verbose;

/*
 * Data structure for attaching data to the communicator 
 */

    struct mca_coll_hierarch_topo {
	int topo_root;
	int topo_prev;
	int topo_nextsize;
	int topo_maxsize;
	int *topo_next;
    };

    struct mca_coll_base_comm_t {
	struct ompi_communicator_t   *hier_comm; /* link back to the attached comm */ 
	struct ompi_communicator_t *hier_llcomm; /* low level communicator */
	int                   hier_num_lleaders; /* number of local leaders */
	int                      *hier_lleaders; /* list of local leaders */
	int                     hier_my_lleader; /* pos. of my lleader in hier_lleaders */
	int                     hier_am_lleader; /* am I an lleader? */
	int                       hier_num_reqs; /* num. of requests */
	ompi_request_t              **hier_reqs; /* list of requests */
	struct mca_coll_hierarch_topo hier_topo; /* topology used in the coll ops */
    };



#define MCA_COLL_HIERARCH_IS_ROOT_LLEADER(_root, _lls, _llsize, _found, _pos) { \
        int _i;                                                                  \
        for (_found=0, _pos=_llsize, _i=0; _i<_llsize; _i++) {                  \
           if ( _lls[_i] == _root )      {                                       \
               _found = 1;                                                       \
               _pos = _i;                                                        \
               break;                                                            \
           }                                                                     \
        }                                                                        \
     }

/*
 * coll API functions
 */


  /* API functions */

    int mca_coll_hierarch_init_query(bool *allow_hierarch_user_threads,
				     bool *have_hidden_threads);
    const struct mca_coll_base_module_1_0_0_t *
    mca_coll_hierarch_comm_query(struct ompi_communicator_t *comm, 
				 int *priority, struct mca_coll_base_comm_t **data);
    int mca_coll_hierarch_comm_unquery(struct ompi_communicator_t *comm, 
				       struct mca_coll_base_comm_t *data);
    
    const struct mca_coll_base_module_1_0_0_t *
            mca_coll_hierarch_module_init(struct ompi_communicator_t *comm);
    int mca_coll_hierarch_module_finalize(struct ompi_communicator_t *comm);
    
    int mca_coll_hierarch_allgather_intra(void *sbuf, int scount, 
					  struct ompi_datatype_t *sdtype, 
					  void *rbuf, int rcount, 
					  struct ompi_datatype_t *rdtype, 
					  struct ompi_communicator_t *comm);
    int mca_coll_hierarch_allgatherv_intra(void *sbuf, int scount, 
					   struct ompi_datatype_t *sdtype, 
					   void * rbuf, int *rcounts, 
					   int *disps, 
					   struct ompi_datatype_t *rdtype, 
					   struct ompi_communicator_t *comm);
    int mca_coll_hierarch_allreduce_intra(void *sbuf, void *rbuf, int count, 
					  struct ompi_datatype_t *dtype, 
					  struct ompi_op_t *op, 
					  struct ompi_communicator_t *comm);
    int mca_coll_hierarch_alltoall_intra(void *sbuf, int scount, 
					 struct ompi_datatype_t *sdtype, 
					 void* rbuf, int rcount, 
					 struct ompi_datatype_t *rdtype, 
					 struct ompi_communicator_t *comm);
    int mca_coll_hierarch_alltoallv_intra(void *sbuf, int *scounts, 
					  int *sdisps, 
					  struct ompi_datatype_t *sdtype, 
					  void *rbuf, int *rcounts, 
					  int *rdisps, 
					  struct ompi_datatype_t *rdtype, 
					  struct ompi_communicator_t *comm);
    int mca_coll_hierarch_alltoallw_intra(void *sbuf, int *scounts, 
					  int *sdisps, 
					  struct ompi_datatype_t **sdtypes, 
					  void *rbuf, int *rcounts, 
					  int *rdisps, 
					  struct ompi_datatype_t **rdtypes, 
					  struct ompi_communicator_t *comm);
    int mca_coll_hierarch_barrier_intra(struct ompi_communicator_t *comm);
    int mca_coll_hierarch_bcast_intra(void *buff, int count, 
				      struct ompi_datatype_t *datatype,
				      int root, 
				      struct ompi_communicator_t *comm);
    int mca_coll_hierarch_exscan_intra(void *sbuf, void *rbuf, int count, 
				       struct ompi_datatype_t *dtype, 
				       struct ompi_op_t *op, 
				       struct ompi_communicator_t *comm);
    int mca_coll_hierarch_gather_intra(void *sbuf, int scount, 
				       struct ompi_datatype_t *sdtype, 
				       void *rbuf, int rcount, 
				       struct ompi_datatype_t *rdtype, 
				       int root, 
				       struct ompi_communicator_t *comm);
    int mca_coll_hierarch_gatherv_intra(void *sbuf, int scount, 
					struct ompi_datatype_t *sdtype, 
					void *rbuf, int *rcounts, int *disps, 
					struct ompi_datatype_t *rdtype, 
					int root, 
					struct ompi_communicator_t *comm);
    int mca_coll_hierarch_reduce_intra(void *sbuf, void* rbuf, int count, 
				       struct ompi_datatype_t *dtype, 
				       struct ompi_op_t *op, 
				       int root,
				       struct ompi_communicator_t *comm);
    int mca_coll_hierarch_reduce_scatter_intra(void *sbuf, void *rbuf, 
					       int *rcounts, 
					       struct ompi_datatype_t *dtype, 
					       struct ompi_op_t *op, 
					       struct ompi_communicator_t *comm);
    int mca_coll_hierarch_scan_intra(void *sbuf, void *rbuf, int count, 
				     struct ompi_datatype_t *dtype, 
				     struct ompi_op_t *op, 
				     struct ompi_communicator_t *comm);
    int mca_coll_hierarch_scatter_intra(void *sbuf, int scount, 
					struct ompi_datatype_t *sdtype, void *rbuf, 
					int rcount, struct ompi_datatype_t *rdtype, 
					int root, struct ompi_communicator_t *comm);
    int mca_coll_hierarch_scatterv_intra(void *sbuf, int *scounts, int *disps, 
					 struct ompi_datatype_t *sdtype, 
					 void* rbuf, int rcount, 
					 struct ompi_datatype_t *rdtype, int root, 
					 struct ompi_communicator_t *comm);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_COLL_HIERARCH_EXPORT_H */
