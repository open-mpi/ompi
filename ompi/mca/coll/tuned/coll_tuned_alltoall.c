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

#include "mpi.h"
#include "ompi/include/constants.h"
#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/coll_tags.h"
#include "mca/pml/pml.h"
#include "op/op.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"
#include "coll_tuned_util.h"

#include <sys/types.h>
#include <unistd.h>

/* temp debug routines */
static int dump_buf_int (char* ptr, int count, char *comment, int rank);

static int dump_buf_int (char* ptr, int count, char *comment, int rank) {
int i=0;
int *tptr;
int c=0;
tptr=(int*)ptr;
printf("%1d ", rank);
if (comment) printf("%s ", comment);
if (count <0) {
    printf("cnt %d?\n", count);
    return (0);
}

if (count>5) c = 5;
else c = count;
printf("Cnt %1d  ", count);
for(i=0;i<c;i++) {
    printf("%1d [%1d] ", i, *tptr++);
    }
if (c!=count) {
    tptr=(int*)ptr;
    printf(" ... %1d [%1d]", count-1, tptr[count-1]);
}
printf("\n");
return (0);
}


int mca_coll_tuned_alltoall_intra_pairwise(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, int rcount,
                                    struct ompi_datatype_t *rdtype,
                                    struct ompi_communicator_t *comm)
{
    int line = -1, err = 0;
    int rank, size, step;
    int sendto, recvfrom;
    void * tmpsend, *tmprecv;
    MPI_Aint sext, rext;
    long lb;


    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    printf("mca_coll_tuned_alltoall_intra_pairwise rank %d\n", rank);


    err = ompi_ddt_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_ddt_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }


   /* Perform pairwise exchange - starting from 1 so the local copy is last */
   for (step = 1; step < size+1; step++) {

      /* who do we talk to in this step? */
      sendto  = (rank+step)%size;
      recvfrom = (rank+size-step)%size;

      /* where from are we sending and where from are we receiving actual data ? */
      tmpsend = (char*)sbuf+sendto*sext*scount;
      tmprecv = (char*)rbuf+recvfrom*rext*rcount;

      /* send and receive */
      err = coll_tuned_sendrecv( tmpsend, scount, sdtype, sendto, MCA_COLL_BASE_TAG_ALLTOALL,
                            tmprecv, rcount, rdtype, recvfrom, MCA_COLL_BASE_TAG_ALLTOALL,
                            comm, MPI_STATUS_IGNORE, rank);
      if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
   }

   return MPI_SUCCESS;
 
 err_hndl:
   fprintf(stderr,"%s:%4d\tError occurred %d, rank %2d\n",
           __FILE__,line,err,rank);
   return err;
}


int mca_coll_tuned_alltoall_intra_bruck(void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, int rcount,
                                    struct ompi_datatype_t *rdtype,
                                    struct ompi_communicator_t *comm)
{
    int i, k, line = -1;
    int rank, size;
    MPI_Aint sext, rext;
    int sendto, recvfrom, distance, *displs=NULL, *blen=NULL;
    int maxpacksize, packsize, position;
    char * tmpbuf=NULL, *packbuf=NULL;
    long lb;
    int err = 0;
    int weallocated = 0;
    MPI_Datatype iddt;



    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    printf("mca_coll_tuned_alltoall_intra_bruck rank %d\n", rank);


    err = ompi_ddt_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_ddt_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }


#ifdef blahblah
    /* try and SAVE memory by using the data segment hung off the communicator if possible */
    if (comm->c_coll_selected_data->mcct_num_reqs >= size) { 
        /* we have enought preallocated for displments and lengths */
        displs = (int*) comm->c_coll_basic_data->mcct_reqs;
        blen = (int *) (displs + size);
        weallocated = 0;
    } 
    else { /* allocate the buffers ourself */
#endif
        displs = (int *) malloc(size*sizeof(int));
        if (displs == NULL) { line = __LINE__; err = -1; goto err_hndl; }
        blen = (int *) malloc(size*sizeof(int));
        if (blen == NULL) { line = __LINE__; err = -1; goto err_hndl; }
        weallocated = 1;
#ifdef blahblah
    }
#endif


    /* Prepare for packing data */
    err = MPI_Pack_size( scount*size, sdtype, comm, &maxpacksize );
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* pack buffer allocation */
    packbuf = (char*) malloc((unsigned) maxpacksize);
    if (packbuf == NULL) { line = __LINE__; err = -1; goto err_hndl; }

    /* tmp buffer allocation for message data */
    tmpbuf = (char *) malloc(scount*size*sext);
    if (tmpbuf == NULL) { line = __LINE__; err = -1; goto err_hndl; }


    /* Step 1 - local rotation - shift up by rank */
    err = ompi_ddt_copy_content_same_ddt (sdtype, (int32_t) ((size-rank)*scount),
                                          tmpbuf, ((char*)sbuf)+rank*scount*sext);
    if (err<0) {
       line = __LINE__; err = -1; goto err_hndl;
    }

    if (rank != 0) {
        err = ompi_ddt_copy_content_same_ddt (sdtype, (int32_t) (rank*scount),
                                           tmpbuf+(size-rank)*scount*sext, (char*)sbuf);
        if (err<0) {
           line = __LINE__; err = -1; goto err_hndl;
        }
    }

   /* perform communication step */
   for (distance = 1; distance < size; distance<<=1) {

      /* send data to "sendto" */
      sendto = (rank+distance)%size;
      recvfrom = (rank-distance+size)%size;
      packsize = 0;
      k = 0;

      /* create indexed datatype */
      for (i = 1; i < size; i++) {
         if ((i&distance) == distance) {
            displs[k] = i*scount; blen[k] = scount;
            k++;
         }
      }
      /* Set indexes and displacements */
      err = MPI_Type_indexed(k, blen, displs, sdtype, &iddt);
      if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
      /* Commit the new datatype */
      err = MPI_Type_commit(&iddt);
      if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

      /* have the new distribution ddt, pack and exchange data */
      err = MPI_Pack(tmpbuf, 1, iddt, packbuf, maxpacksize, &packsize, comm);
      if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

      /* Sendreceive */
      err = coll_tuned_sendrecv ( packbuf, packsize, MPI_PACKED, sendto, 
                            MCA_COLL_BASE_TAG_ALLTOALL,
                            rbuf, packsize, MPI_PACKED, recvfrom, 
                            MCA_COLL_BASE_TAG_ALLTOALL,
                            comm, MPI_STATUS_IGNORE, rank);
      if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

      /* Unpack data from rbuf to tmpbuf */
      position = 0;
      err = MPI_Unpack(rbuf, packsize, &position,
                         tmpbuf, 1, iddt, comm);
      if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

      /* free ddt */
      err = MPI_Type_free(&iddt);
      if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
   } /* end of for (distance = 1... */

   /* Step 3 - local rotation - */
   for (i = 0; i < size; i++) {

        err = ompi_ddt_copy_content_same_ddt (rdtype, (int32_t) rcount,
                                           ((char*)rbuf)+(((rank-i+size)%size)*rcount*rext), 
                                           tmpbuf+i*rcount*rext);
        if (err<0) {
            line = __LINE__; err = -1; goto err_hndl;
        }
   }

 
    if (err<0) {
       line = __LINE__; err = -1; goto err_hndl;
    }

   /* Step 4 - clean up */
   if (tmpbuf != NULL) free(tmpbuf);
   if (packbuf != NULL) free(packbuf);
   if (weallocated) {
      if (displs != NULL) free(displs);
      if (blen != NULL) free(blen);
   }

   return OMPI_SUCCESS;

 err_hndl:
   fprintf(stderr,"%s:%4d\tError occurred %d, rank %2d\n",
           __FILE__,line,err,rank);
   if (tmpbuf != NULL) free(tmpbuf);
   if (packbuf != NULL) free(packbuf);
   if (weallocated) {
      if (displs != NULL) free(displs);
      if (blen != NULL) free(blen);
   }
 return err;

}


int mca_coll_tuned_alltoall_intra_two_procs(void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, int rcount,
                                    struct ompi_datatype_t *rdtype,
                                    struct ompi_communicator_t *comm)
{
    int line = -1, err = 0;
    int rank;
    int sendto, recvfrom;
    void * tmpsend, *tmprecv;
    MPI_Aint sext, rext;
    long lb;

    rank = ompi_comm_rank(comm);

    printf("mca_coll_tuned_alltoall_intra_two_procs rank %d\n", rank);

    err = ompi_ddt_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_ddt_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    /* exchange data */
    sendto  = (rank+1)%2;
    recvfrom = sendto;

    /* where from are we sending and where to are we receiving ? */
    tmpsend = (char*)sbuf+sendto*sext*scount;
    tmprecv = (char*)rbuf+recvfrom*rext*rcount;

    /* send and receive */
    err = coll_tuned_sendrecv ( tmpsend, scount, sdtype, sendto, MCA_COLL_BASE_TAG_ALLTOALL,
                            tmprecv, rcount, rdtype, recvfrom, MCA_COLL_BASE_TAG_ALLTOALL,
                            comm, MPI_STATUS_IGNORE, rank );
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* ddt sendrecv your own data */
    err = ompi_ddt_sndrcv((char*) sbuf+rank*sext*scount, (int32_t) scount, sdtype, 
                          (char*) rbuf+rank*rext*rcount, (int32_t) rcount, rdtype);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* done */
    return MPI_SUCCESS;

 err_hndl:
   fprintf(stderr,"%s:%4d\tError occurred %d, rank %2d\n",
           __FILE__,line,err,rank);
   return err;
}




int mca_coll_tuned_alltoall_intra_linear(void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, int rcount,
                                    struct ompi_datatype_t *rdtype,
                                    struct ompi_communicator_t *comm)
{
    int line = -1, err = 0;
    int rank, size;
    MPI_Aint sext, rext;
    long lb;


    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    printf("mca_coll_tuned_alltoall_intra_linear rank %d\n", rank);


    err = ompi_ddt_get_extent (sdtype, &lb, &sext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

    err = ompi_ddt_get_extent (rdtype, &lb, &rext);
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }

 err_hndl:
   fprintf(stderr,"%s:%4d\tError occurred %d, rank %2d\n",
           __FILE__,line,err,rank);
   return err;
}




