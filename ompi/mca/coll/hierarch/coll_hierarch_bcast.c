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

#include "ompi_config.h"
#include "coll_hierarch.h"

#include "mpi.h"
#include "include/constants.h"
#include "opal/util/output.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"
#include "mca/coll/base/coll_tags.h"
#include "coll_hierarch.h"

/*
 *	bcast_intra
 *
 *	Function:	- broadcast using O(N) algorithm
 *	Accepts:	- same arguments as MPI_Bcast()
 *	Returns:	- MPI_SUCCESS or error code
 */


static int mca_coll_hierarch_intra_segmented_bcast ( void* buffer, 
						     int count, 
						     ompi_datatype_t * datatype, 
						     int root, 
						     ompi_communicator_t * comm, 
						     int segsize,
						     struct mca_coll_hierarch_topo *topo);

static int mca_coll_hierarch_intra_bcast_setup_topo (int count, 
						     ompi_datatype_t *datatype, 
						     int root, 
						     struct mca_coll_base_comm_t *data,
						     int *segsize);
static void setup_topo_bmtree ( int root, struct mca_coll_base_comm_t *data );




int mca_coll_hierarch_bcast_intra(void *buff, 
				  int count,
				  struct ompi_datatype_t *datatype, 
				  int root,
				  struct ompi_communicator_t *comm)
{
    struct mca_coll_base_comm_t *data=NULL;
    struct ompi_communicator_t *llcomm=NULL;
    int lleader_of_root, lleader_replaced_by_root=0;
    int rank, ret, lroot;
    int segsize;

    rank   = ompi_comm_rank ( comm );
    data   = comm->c_coll_selected_data;
    llcomm = data->hier_llcomm;

    /* Determine whether
       a) we have the same local leader like the root of this operation
       b) the root and the local leader are the identical

       If a) is true and b) not, we will replace the local leader for this
       subgroup by the root 
    */

    mca_coll_hierarch_get_lleader (root, data, &lleader_of_root);
    if ( (lleader_of_root == data->hier_my_lleader) && (lleader_of_root != root )) {
	lleader_replaced_by_root = 1;
    }

    /* Bcast on the upper level among the local leaders */
    if ( rank == root || ( data->hier_am_lleader && !lleader_replaced_by_root) ) {
	/* this functions sets up the topology used in the segmented
	   bcast afterwards and determines the segment size. */
	ret = mca_coll_hierarch_intra_bcast_setup_topo (count, datatype, root, 
							data, &segsize);
	if ( OMPI_SUCCESS != ret ) {
	    return ret;
	}
	/* ok, do now the actual bcast. Hopefully, this routine will come
	   out of Jelena's collective module in the end. For the moment,
	   I've implemented it myself 
	*/
	ret = mca_coll_hierarch_intra_segmented_bcast (buff, count,
						       datatype, root,
						       comm, segsize, 
						       &(data->hier_topo));
	if ( OMPI_SUCCESS != ret ) {
	    return ret;
	}
    }

    /* once the local leaders got the data from the root, they can distribute
       it to the processes in their local, low-leve communicator.
    */
    if ( MPI_COMM_NULL != llcomm ) {
	if ( lleader_replaced_by_root ) {
	    mca_coll_hierarch_map_rank(root, data, &lroot);
	    ret = llcomm->c_coll.coll_bcast(buff, count, datatype, lroot,
					    llcomm);
	}
	else {
	    /* Assumption: the rank of the local leader on llcomm is always 0 */	       
	    ret = llcomm->c_coll.coll_bcast(buff, count, datatype, 0, llcomm );
	}
    }

    return  ret;
}



/* 
 *  This is the mother of all segmented bcast algorithms of any type.
 *  Due to the general structure of the topo argument, you can use this function
 *  for any type of algorith - it just depends on the settings of topo.
 *  
 *  The implementation is strongly leaning on the implementation in FT-MPI.
 */

static int mca_coll_hierarch_intra_segmented_bcast ( void* buffer, 
						     int count, 
						     ompi_datatype_t * datatype, 
						     int root, 
						     ompi_communicator_t * comm, 
						     int segsize,
						     struct mca_coll_hierarch_topo *topo)
{
  int err=0, i, j;
  int size, rank;
  int segcount;       /* Number of elements sent with each segment */
  int num_segments;   /* Number of segmenets */
  int recvcount;      /* the same like segcount, except for the last segment */ 
  int typelng, realsegsize;
  char *tmpbuf;
  long rlb, ext;
  ompi_request_t ** recv_request= NULL;

  size = ompi_comm_size ( comm );
  rank = ompi_comm_rank ( comm );

  /* ------------------------------------------- */
  /* special case for size == 1 and 2 */
  if (size == 1) {
    return OMPI_SUCCESS;
  }
  if (size == 2) {
    if (rank == root) {
      err = mca_pml.pml_send(buffer, count, datatype, (rank+1)%2, 
			     MCA_COLL_BASE_TAG_BCAST, 
			     MCA_PML_BASE_SEND_STANDARD, comm );
      if ( OMPI_SUCCESS != err ) { 
	  return err;
      }
    } else {
	err = mca_pml.pml_recv(buffer, count, datatype, root,
			       MCA_COLL_BASE_TAG_BCAST, comm, 
			       MPI_STATUS_IGNORE);
      if ( OMPI_SUCCESS != err) {
	  return err;      
      }
    }
    return OMPI_SUCCESS;
  }
  /* end special case for size == 1 and 2 */


  tmpbuf = (char *) buffer;
  /* -------------------------------------------------- */
  /* Determine number of segments and number of elements
     sent per operation  */
  err = ompi_ddt_type_size( datatype, &typelng);
  if (  OMPI_SUCCESS != err) {
      return ( err );
  }

  if ( segsize > 0 ) {
      segcount     = segsize/typelng; 
      num_segments = count/segcount;
      if (0 != (count % segcount)) {
	  num_segments++;
      }
  }
  else  {
      segcount     = count;
      num_segments = 1;
  }
  
  /* Determine real segment size = segcount * extent */
  err = ompi_ddt_get_extent( datatype, &rlb, &ext );
  if ( OMPI_SUCCESS != err) {
      return ( err );
  }
  realsegsize = segcount*ext;

  /* ----------------------------------------------------- */
  /* Post Irecv if not root-node */
  if (rank != root)  {
      /* has a parent. need to receive before sending */
      if ( num_segments > 2 * size ) {
	  recv_request = (MPI_Request*)malloc ( sizeof(ompi_request_t *)*num_segments );
      }
      else {
	  recv_request = comm->c_coll_selected_data->hier_reqs;
      }

      for( i = 0; i < num_segments; i++) {
	  if ( i == (num_segments -1) ) {
	    recvcount = count - (segcount * i);
	  }
	  else {
	    recvcount = segcount;
	  }
	  err = mca_pml.pml_irecv(tmpbuf+i*realsegsize, recvcount, datatype,
				  topo->topo_prev, MCA_COLL_BASE_TAG_BCAST, 
				  comm, &recv_request[i]);
	  if ( OMPI_SUCCESS != err ) {
	      return ( err );
	  }
	}
    }

  /* ---------------------------------------------- */
  /* If leaf node, just finish the receive */
  if (topo->topo_nextsize == 0)  {
      if(recv_request != NULL) {
	  err = ompi_request_wait_all (num_segments, recv_request, MPI_STATUSES_IGNORE);
	  if ( OMPI_SUCCESS != err ) {
	      return ( err );
	  }
      }
  }
  else  {
      /* ------------------------------------------ */
      /* root or intermediate node */      
      for( i = 0; i < num_segments; i++) {
	  if (rank != root)  {
	      /* intermediate nodes have to wait for the completion of
		 the corresponding receive */
	      err = ompi_request_wait_all(1, &recv_request[i], MPI_STATUS_IGNORE);
	      if ( OMPI_SUCCESS != err ) {
		  return ( err );
	      }
	    }
	  for ( j = 0; j < topo->topo_nextsize; j++)   {
	      if ( i == ( num_segments - 1 )) {
		  recvcount = count - ( segcount * i);
	      }
	      else {
		  recvcount = segcount;
	      }

	      err = mca_pml.pml_send(tmpbuf+i*realsegsize, recvcount, 
				     datatype, topo->topo_next[j], 
				     MCA_COLL_BASE_TAG_BCAST, 
				     MCA_PML_BASE_SEND_STANDARD, comm );
	      if( OMPI_SUCCESS != err )  {
		  return ( err );
	      }
	  } /* for ( j = 0; j < topo_nextsize; j++) */
      } /* for ( i = 0; i < num_segments; i++) */
  }
  
  if ( num_segments > 2 * size ) {
      if(recv_request != NULL) {
	  free(recv_request);
      }
  }

  return OMPI_SUCCESS;
} 


/* 
 * This routine does the magic to determine, which topology (bmtree, linear, chain etc)
 *  would perform best in this scenario. At the moment, we just do bmtree.
 *
 * The implementation is once again strongly related to the version in FT-MPI.
 */
static int mca_coll_hierarch_intra_bcast_setup_topo (int count, 
						     ompi_datatype_t *datatype, 
						     int root, 
						     struct mca_coll_base_comm_t *data,
						     int *segsize)
{
    /* without spending time on that issues, I set for the moment segsize to 32k. */
    *segsize = 32768;
    
    /* without spending time on that issue, I set the topology to a binomial tree */
    setup_topo_bmtree ( root, data );

    return OMPI_SUCCESS;
}


static void setup_topo_bmtree ( int root, struct mca_coll_base_comm_t *data ) 
{
    /* This implementation is based on the closest first bmtree algorithms 
       in FT-MPI implemnented by George/Jelena, has however a couple of 
       significant modifications:
       - we are not having a contiguous list of participating processes,
         but a list containing the ranks of the participating processes.
    */

    int childs = 0;
    int rank, size, mask=1;
    int index, remote, found;
    int rootpos;
    struct mca_coll_hierarch_topo *topo=&(data->hier_topo);

    

    if (found) {
	size = data->hier_num_lleaders;
    }
    else {
	size = data->hier_num_lleaders + 1;	
	data->hier_lleaders[rootpos] = root;
    }
    rank = data->hier_my_lleader;

    /* allocate the array of childprocesses, if not yet done */
    if ( NULL == topo->topo_next && 0 == topo->topo_maxsize ) {
	topo->topo_next = (int *) malloc (data->hier_num_lleaders+1 * sizeof(int));
	if ( NULL != topo->topo_next ) {
	    return;
	}
	topo->topo_maxsize=data->hier_num_lleaders+1;
    }

    index = rank - rootpos;

    if( index < 0 ) index += size;
    while( mask <= index ) mask <<= 1;
    
    /* Determine the rank of my father */
    if( rootpos == rank ) {
	topo->topo_prev = root;
    }
    else {
	remote = (index ^ (mask >> 1)) + rootpos;
	if( remote >= size ) {
	    remote -= size;
	}
	topo->topo_prev = data->hier_lleaders[remote];
    }

    /* And now let's fill my childs */
    while( mask < size ) {
	remote = (index ^ mask);
	if( remote >= size ) break;
	remote += rootpos;
	if( remote >= size ) remote -= size;
	topo->topo_next[childs] = data->hier_lleaders[remote];
	mask <<= 1;
	childs++;
    }

    topo->topo_nextsize = childs;
    return;
}

#endif
