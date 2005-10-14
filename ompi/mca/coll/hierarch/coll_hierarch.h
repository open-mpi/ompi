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
extern int mca_coll_hierarch_use_rdma_param;
extern int mca_coll_hierarch_ignore_sm_param;


#define HIER_DEFAULT_NUM_LLEAD 5
/*
 * Data structure for attaching data to the communicator 
 */

/* Clarifying some terminology:
 *  comm:    the input communicator, consisting of several lower level communicators.
 *  lcomm:   low level communicator, often refered to as subcommunicator
 *  lleader: local leader, a dedicated process of each low level communicator
 *  llcomm:  local leader communicator, grouping all local leaders of a comm.
*/

    struct mca_coll_base_comm_t {
	struct ompi_communicator_t        *hier_comm; /* link back to the attached comm */ 
	struct ompi_communicator_t       *hier_lcomm; /* low level communicator */
	ompi_pointer_array_t              hier_llead; /* local leader communicator structure */
	int                        hier_num_lleaders; /* number of local leaders */
	int                               hier_level; /* level in the hierarchy. just debugging */
 	int                            hier_num_reqs; /* num. of requests */
	ompi_request_t                   **hier_reqs; /* list of requests */
	int                        hier_num_colorarr; /* size of the colorarr array */
	int                                *hier_llr; /* color array compacted (1 entry per color)*/
	int                           *hier_colorarr; /* array containing the color of all procs */
    };

    struct mca_coll_hierarch_llead_t {
	struct ompi_communicator_t    *llcomm; /* local leader communicator */
	int                         *lleaders; /* list of local leaders, ranks in comm */
	int                        my_lleader; /* rank of my lleader in lcomm */
	int                        am_lleader; /* am I an lleader? */
    };
    
    typedef struct mca_coll_hierarch_llead_t mca_coll_hierarch_llead_t;


static inline int mca_coll_hierarch_count_lleaders ( int size, int *carr)
{
    int cnt, i, j, found;
    int *llr=NULL;

    llr = (int *) malloc ( size * sizeof(int));
    if (NULL == llr ){
	return -1;
    }

    for ( i=0; i<size; i++ ) {
	if ( carr[i] != MPI_UNDEFINED ) {
	    llr[0] = carr[i];
	    break;
	}
    }


    for (cnt=1, i=0; i<size; i++ ) {
	for ( found=0, j=0; j<cnt; j++ ) {
	    if ( carr[i] == llr[j] ) {
		found = 1;
		break;
	    }
	}
	if ( !found && (MPI_UNDEFINED != carr[i]) ) {
	    llr[cnt++] = carr[i];
	}
    }

    free (llr);

    return cnt;
}

static inline void mca_coll_hierarch_get_llr ( int size, int *carr, int *llr )
{
    int i,j,cnt, found;

    for ( i=0; i<size; i++ ) {
	if ( carr[i] != MPI_UNDEFINED ) {
	    llr[0] = carr[i];
	    break;
	}
    }


    for (cnt=1, i=0; i<size; i++ ) {
	for ( found=0, j=0; j<cnt; j++ ) {
	    if ( carr[i] == llr[j] ) {
		found = 1;
		break;
	    }
	}
	if ( !found && (MPI_UNDEFINED != carr[i]) ) {
	    llr[cnt++] = carr[i];
	}
    }

    return;
}
    
static inline void mca_coll_hierarch_get_all_lleaders ( int size, int *carr, int lsize,
							int *llr, int *larr, int offset)
{
    int i, j, k;
    int *cntarr=NULL;

    cntarr = (int *)calloc (1, sizeof (int)*lsize );
    if ( NULL == cntarr ) {
	return;
    }

    for ( k=0, i=0;i<size;i++) {
	for ( j=0; j<lsize; j++) {
	    if ( carr[i] == llr[j] && cntarr[j] < offset) {
		cntarr[j]++;
		larr[j] = i;
		break;
	    }
	}
    }


    return;
}

static inline int mca_coll_hierarch_get_offset ( int rank, int size, int *carr) 
{
    int offset=1;
    return offset;
}


static inline void mca_coll_hierarch_get_lleader (int rank, struct mca_coll_base_comm_t *data,
						  int* lleader, int *am_lleader, int offset ) 
{
    int color, i;

    /* initialize it to be undefined */
    *lleader = MPI_UNDEFINED;

    /* sanity check */
    if ( rank > data->hier_num_colorarr-1 ) {
	return;
    }
    
    /* Get the color of this rank */
    color = data->hier_colorarr[rank];
    
    /* get the first rank having this color. this is
       currently by definition the local leader */
    for ( i=0; i< data->hier_num_colorarr-1; i++ ) {
	if ( data->hier_colorarr[i] == color ) {
	    *lleader = i;
	    break;
	}
    }
	    
    return;
}

/*
 * coll API functions
 */
struct ompi_communicator_t*  mca_coll_hierarch_get_llcomm (int rroot, struct mca_coll_base_comm_t *data,
							   int* llroot, int* lleader); 


int mca_coll_hierarch_init_query(bool allow_hierarch_user_threads,
				 bool have_hidden_threads);
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
