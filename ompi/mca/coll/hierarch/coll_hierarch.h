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
extern int mca_coll_hierarch_walk_through_list_param;
extern int mca_coll_hierarch_use_next_param;

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
	int                          hier_level; /* level in the hierarchy. just debugging */
	int                   hier_num_lleaders; /* number of local leaders */
	int                      *hier_lleaders; /* list of local leaders, ranks in comm */
	int                     hier_my_lleader; /* pos. of my lleader in hier_lleaders */
	int           hier_my_lleader_on_llcomm; /* rank of my lleader in llcomm */
	int                     hier_am_lleader; /* am I an lleader? */
	int                       hier_num_reqs; /* num. of requests */
	ompi_request_t              **hier_reqs; /* list of requests */
	int                  hier_type_colorarr; /* format in which the colorarr is stored */
	int                   hier_num_colorarr; /* size of the colorarr array */
	int*                      hier_colorarr; /* array containing the color of all procs */
	struct mca_coll_hierarch_topo hier_topo; /* topology used in the coll ops */
    };

/* These are various modes how the colorarr is stored. The reason
   for the various versions is to minimize the memory requirement
   for this task, since in most real-world scenarios, the information
   can be stored significantly more compact that storing the whole array
   
   MCA_COLL_HIERARCH_COLORARR_LINEAR: 
          contains an array of size hier_num_colorarr. Each element 
	  contains the color of the according process
   MCA_COLL_HIERARCH_COLORARR_RANGE:
          the ranks beeing in the same subcommunicator are consecutive
	  ranks  (e.g. ranks 0-8 are in subgroup1, 9-16 in subgroup2 etc)

          hier_colorarr[0] : number of blocks
	  hier_colorarr[2*i+1] : first rank of block i, i=0,(hier_colorarr[0]-1)
	  hier_colorarr[2*i+2] : last rank of block i

	  hier_num_coloarr = hier_coloarr[0] + 1;

   MCA_COLL_HIERARCH_COLORARR_STRIDE2:
          the processes are in two subgroups with a stride of two,
	  e.g. (0,2,4,6,...) are in subgroup 1, (1,3,5,7,...) in subgroup2
	  This scenario might happen on dual-processor nodes if the scheduler
	  has distributed the processes in a round-robin fashion.

	  hier_colorarr[0] = first rank of first subgroup
	  hier_colorarr[1] = first rank of second subgroup
	  hier_num_colorarr = 2

   MCA_COLL_HIERARCH_COLORARR_STRIDE4:
          the processes are in four subgroups with a stride of four,
	  e.g. (0,4,8,12,...) are in subgroup 1, (1,5,9,13,...) in subgroup2 etc.
	  This scenario might happen on quad-processor nodes if the scheduler
	  has distributed the processes in a round-robin fashion.

	  hier_colorarr[0] = first rank of first subgroup
	  hier_colorarr[1] = first rank of second subgroup
	  hier_colorarr[2] = first rank of third subgroup
	  hier_colorarr[3] = first rank of forth subgroup
	  hier_num_colorarr = 4

*/

#define MCA_COLL_HIERARCH_COLORARR_INVALID -1
#define MCA_COLL_HIERARCH_COLORARR_LINEAR   0
#define MCA_COLL_HIERARCH_COLORARR_RANGE    1
#define MCA_COLL_HIERARCH_COLORARR_STRIDE2  2
#define MCA_COLL_HIERARCH_COLORARR_STRIDE4  3

static inline int mca_coll_hierarch_count_lleaders ( int size, int *carr ) 
{
    int cnt, i, j, found;
    int *llr=NULL;

    llr  = (int *) calloc (1, sizeof(int) * size);
    if (NULL == llr ) {
	return -1;
    }

    for (cnt=0, i=0; i<size; i++ ) {
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
    
static inline void mca_coll_hierarch_get_all_lleaders ( int size, int *carr, int lsize,
							int *larr)
{
    int i, j, cnt, found;

    for (cnt=0, i=0; i<size; i++ ) {
	for ( found=0, j=0; j<cnt; j++ ) {
	    if ( carr[i] == larr[j] ) {
		found = 1;
		break;
	    }
	}
	if ( !found && (MPI_UNDEFINED != carr[i]) ) {
	    larr[cnt++] = carr[i];
	}
    }
    
    return;
}

static inline void mca_coll_hierarch_get_lleader (int rank, struct mca_coll_base_comm_t *data,
						  int* lleader ) 
{
    int color, i;

    /* initialize it to be undefined */
    *lleader = MPI_UNDEFINED;

    switch ( data->hier_type_colorarr ) 
    {
	case MCA_COLL_HIERARCH_COLORARR_LINEAR:
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
	    
	    break;
	case MCA_COLL_HIERARCH_COLORARR_RANGE:
	case MCA_COLL_HIERARCH_COLORARR_STRIDE2:
	case MCA_COLL_HIERARCH_COLORARR_STRIDE4:
	case MCA_COLL_HIERARCH_COLORARR_INVALID:
	default:
	    break;
    }
    
    return;
}

static inline void mca_coll_hierarch_map_rank (int rank, struct mca_coll_base_comm_t *data,
					       int* lrank ) 
{
    int i, color, tmprank=-1;

    /* initialize it to MPI_UNDEFINED */
    *lrank = MPI_UNDEFINED;
    
    switch ( data->hier_type_colorarr ) 
    {
	case MCA_COLL_HIERARCH_COLORARR_LINEAR:
	    /* sanity check */
	    if ( rank > data->hier_num_colorarr-1 ) {
		return;
	    }

	    /* Get the color of this process */
	    color = data->hier_colorarr[rank];

	    /* walk through the array until we reach 'rank' and calculate
	       how many processes had the same color */
	    for ( i=0; i< rank+1; i++ ) {
		if ( data->hier_colorarr[i] == color ) {
		    tmprank++;
		    break;
		}
	    }

	    *lrank = tmprank;
	    break;
	case MCA_COLL_HIERARCH_COLORARR_RANGE:
	case MCA_COLL_HIERARCH_COLORARR_STRIDE2:
	case MCA_COLL_HIERARCH_COLORARR_STRIDE4:
	case MCA_COLL_HIERARCH_COLORARR_INVALID:
	default:
	    break;
    }
    
    return;
}

/*
 * coll API functions
 */

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
