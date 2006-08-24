#include "nbc.h"
static __inline__ int allred_sched_diss(int rank, int p, int count, MPI_Datatype datatype, void *sendbuf, void *recvbuf, MPI_Op op, NBC_Schedule *schedule, NBC_Handle *handle);
static __inline__ int allred_sched_chain(int rank, int p, int count, MPI_Datatype datatype, void *sendbuf, void *recvbuf, MPI_Op op, int size, int ext, NBC_Schedule *schedule, NBC_Handle *handle, int fragsize);

#ifdef NBC_CACHE_SCHEDULE
/* tree comparison function for schedule cache */
int NBC_Allreduce_args_compare(NBC_Allreduce_args *a, NBC_Allreduce_args *b, void *param) {

	if( (a->sendbuf == b->sendbuf) && 
      (a->recvbuf == b->recvbuf) &&
      (a->count == b->count) && 
      (a->datatype == b->datatype) &&
      (a->op == b->op) ) {
    return  0;
  }
	if( a->sendbuf < b->sendbuf ) {	
    return -1;
	}
	return +1;
}
#endif

int NBC_Iallreduce(void* sendbuf, void* recvbuf, int count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm, NBC_Handle* handle) {
  int rank, p, res, size, segsize;
  MPI_Aint ext;
  NBC_Schedule *schedule;
#ifdef NBC_CACHE_SCHEDULE
  NBC_Allreduce_args *args, *found, search;
#endif
  enum { NBC_ARED_BINOMIAL, NBC_ARED_CHAIN } alg;
  char inplace;
  
  NBC_IN_PLACE(sendbuf, recvbuf, inplace);
  
  res = NBC_Init_handle(handle, comm);
  if(res != NBC_OK) { printf("Error in NBC_Init_handle(%i)\n", res); return res; }
  res = MPI_Comm_rank(comm, &rank);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_rank() (%i)\n", res); return res; }
  res = MPI_Comm_size(comm, &p);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Comm_size() (%i)\n", res); return res; }
  res = MPI_Type_extent(datatype, &ext);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
  res = MPI_Type_size(datatype, &size);
  if (MPI_SUCCESS != res) { printf("MPI Error in MPI_Type_extent() (%i)\n", res); return res; }
  
  handle->tmpbuf = malloc(ext*count);
  if(handle->tmpbuf == NULL) { printf("Error in malloc() (%i)\n", res); return NBC_OOR; }

  if((p == 1) && !inplace) {
    /* for a single node - copy data to receivebuf */
    res = NBC_Copy(sendbuf, count, datatype, recvbuf, count, datatype, comm);
    if (NBC_OK != res) { printf("Error in NBC_Copy() (%i)\n", res); return res; }
  }
  
  /* algorithm selection */
  if(p < 4 || size*count < 65536) {
    alg = NBC_ARED_BINOMIAL;
  } else if(size*count < 262144){
    alg = NBC_ARED_CHAIN;
    segsize = 16384/2;
  } else {
    alg = NBC_ARED_CHAIN;
    segsize = 65536/2;
  }
      
#ifdef NBC_CACHE_SCHEDULE
  /* search schedule in communicator specific tree */
  search.sendbuf=sendbuf;
  search.recvbuf=recvbuf;
  search.count=count;
  search.datatype=datatype;
  search.op=op;
  found = hb_tree_search(handle->comminfo->NBC_Dict[NBC_ALLREDUCE], &search);
  if(found == NULL) {
#endif
    schedule = malloc(sizeof(NBC_Schedule));
    if (NULL == schedule) { printf("Error in malloc()\n"); return res; }

    res = NBC_Sched_create(schedule);
    if(res != NBC_OK) { printf("Error in NBC_Sched_create (%i)\n", res); return res; }

    switch(alg) {
      case NBC_ARED_BINOMIAL:
        res = allred_sched_diss(rank, p, count, datatype, sendbuf, recvbuf, op, schedule, handle);
        break;
      case NBC_ARED_CHAIN:
        res = allred_sched_chain(rank, p, count, datatype, sendbuf, recvbuf, op, size, ext, schedule, handle, segsize);
        break;
    }
    if (NBC_OK != res) { printf("Error in Schedule creation() (%i)\n", res); return res; }
    
    res = NBC_Sched_commit(schedule);
    if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_commit() (%i)\n", res); return res; }
    
#ifdef NBC_CACHE_SCHEDULE
    /* save schedule to tree */
    args = malloc(sizeof(NBC_Allreduce_args));
    args->sendbuf=sendbuf;
    args->recvbuf=recvbuf;
    args->count=count;
    args->datatype=datatype;
    args->op=op;
    args->schedule=schedule;
	  res = hb_tree_insert (handle->comminfo->NBC_Dict[NBC_ALLREDUCE], args, args, 0);
    if(res != 0) printf("error in dict_insert() (%i)\n", res);
    /* increase number of elements for A2A */
    if(++handle->comminfo->NBC_Dict_size[NBC_ALLREDUCE] > SCHED_DICT_UPPER) {
      NBC_SchedCache_dictwipe(handle->comminfo->NBC_Dict[NBC_ALLREDUCE], &handle->comminfo->NBC_Dict_size[NBC_ALLREDUCE]);
    }
  } else {
    /* found schedule */
    schedule=found->schedule;
  }
#endif
  
  res = NBC_Start(handle, schedule);
  if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Start() (%i)\n", res); return res; }
  
  /* tmpbuf is freed with the handle */
  return NBC_OK;
}


/* binomial allreduce (binomial tree up and binomial bcast down)
 * working principle:
 * - each node gets a virtual rank vrank
 * - the 'root' node get vrank 0 
 * - node 0 gets the vrank of the 'root'
 * - all other ranks stay identical (they do not matter)
 *
 * Algorithm:
 * pairwise exchange
 * round r: 
 *  grp = rank % 2^r
 *  if grp == 0: receive from rank + 2^(r-1) if it exists and reduce value
 *  if grp == 1: send to rank - 2^(r-1) and exit function
 *  
 * do this for R=log_2(p) rounds
 * followed by a Bcast:
 * Algorithm:
 * - each node with vrank > 2^r and vrank < 2^r+1 receives from node
 *   vrank - 2^r (vrank=1 receives from 0, vrank 0 receives never)
 * - each node sends each round r to node vrank + 2^r
 * - a node stops to send if 2^r > commsize  
 *    
 */
#define RANK2VRANK(rank, vrank, root) \
{ \
  vrank = rank; \
  if (rank == 0) vrank = root; \
  if (rank == root) vrank = 0; \
}
#define VRANK2RANK(rank, vrank, root) \
{ \
  rank = vrank; \
  if (vrank == 0) rank = root; \
  if (vrank == root) rank = 0; \
}
static __inline__ int allred_sched_diss(int rank, int p, int count, MPI_Datatype datatype, void *sendbuf, void *recvbuf, MPI_Op op, NBC_Schedule *schedule, NBC_Handle *handle) {
  int root, vrank, r, maxr, firstred, vpeer, peer, res;
  
  root = 0; /* this makes the code for ireduce and iallreduce nearly identical - could be changed to improve performance */
  RANK2VRANK(rank, vrank, root);
  maxr = (int)ceil((log(p)/LOG2));

  firstred = 1;
  for(r=1; r<=maxr; r++) {
    if((vrank % (1<<r)) == 0) {
      /* we have to receive this round */
      vpeer = vrank + (1<<(r-1));
      VRANK2RANK(peer, vpeer, root)
      if(peer<p) {
        res = NBC_Sched_recv(0, true, count, datatype, peer, schedule);
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        /* we have to wait until we have the data */
        res = NBC_Sched_barrier(schedule);
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
        if(firstred) {
          /* perform the reduce with the senbuf */
          res = NBC_Sched_op(recvbuf, false, sendbuf, false, 0, true, count, datatype, op, schedule);
          firstred = 0;
        } else {
          /* perform the reduce in my local buffer */
          res = NBC_Sched_op(recvbuf, false, recvbuf, false, 0, true, count, datatype, op, schedule);
        }
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_op() (%i)\n", res); return res; }
        /* this cannot be done until handle->tmpbuf is unused :-( */
        res = NBC_Sched_barrier(schedule);
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
      }
    } else {
      /* we have to send this round */
      vpeer = vrank - (1<<(r-1));
      VRANK2RANK(peer, vpeer, root)
      if(firstred) {
        /* we have to use the sendbuf in the first round .. */
        res = NBC_Sched_send(sendbuf, false, count, datatype, peer, schedule);
      } else {
        /* and the recvbuf in all remeining rounds */
        res = NBC_Sched_send(recvbuf, false, count, datatype, peer, schedule);
      }
      if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
      /* leave the game */
      break;
    }
  }
  
  /* this is the Bcast part - copied with minor changes from nbc_ibcast.c 
   * changed: buffer -> recvbuf  */
  RANK2VRANK(rank, vrank, root);

  /* receive from the right hosts  */
  if(vrank != 0) {
    for(r=0; r<maxr; r++) {
      if((vrank >= (1<<r)) && (vrank < (1<<(r+1)))) {
        VRANK2RANK(peer, vrank-(1<<r), root);
        res = NBC_Sched_recv(recvbuf, false, count, datatype, peer, schedule);
        if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
      }
    }
    res = NBC_Sched_barrier(schedule);
    if(NBC_OK != res) { free(handle->tmpbuf); printf("Error in NBC_Sched_barrier() (%i)\n", res); return res; }
  }

  /* now send to the right hosts */
  for(r=0; r<maxr; r++) {
    if(((vrank + (1<<r) < p) && (vrank < (1<<r))) || (vrank == 0)) {
      VRANK2RANK(peer, vrank+(1<<r), root);
      res = NBC_Sched_send(recvbuf, false, count, datatype, peer, schedule);
      if(res != NBC_OK) { free(handle->tmpbuf); printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
    }
  }
  /* end of the bcast */
  
  return NBC_OK;
}

static __inline__ int allred_sched_chain(int rank, int p, int count, MPI_Datatype datatype, void *sendbuf, void *recvbuf, MPI_Op op, int size, int ext, NBC_Schedule *schedule, NBC_Handle *handle, int fragsize) {
  int res, rrpeer, rbpeer, srpeer, sbpeer, numfrag, fragnum, fragcount, thiscount, bstart, bend;
  long roffset, boffset;
  
  /* reduce peers */
  rrpeer = rank+1; 
  srpeer = rank-1;
  /* bcast peers */
  rbpeer = rank-1;
  sbpeer = rank+1;
  
  if(count == 0) return NBC_OK;
  
  numfrag = count*size/fragsize;
  if((count*size)%fragsize != 0) numfrag++;
  fragcount = count/numfrag;

  /* determine the starting round of bcast ... the first reduced packet
   * is after p-1 rounds at rank 0 and will be sent back ... */
  bstart = p-1+rank;
  /* determine the ending round of bcast ... after arrival of the first
   * packet, each rank has to forward numfrag packets */
  bend = bstart+numfrag;
  //printf("[%i] numfrag: %i, count: %i, size: %i, fragcount: %i, bstart: %i, bend: %i\n", rank, numfrag, count, size, fragcount, bstart, bend);

  /* this are two loops in one - this is a little nasty :-( */
  for(fragnum = 0; fragnum < bend; fragnum++) {
    roffset = fragnum*fragcount*ext;
    boffset = (fragnum-bstart)*fragcount*ext;
    thiscount = fragcount;

    /* first numfrag rounds ... REDUCE to rank 0 */
    if(fragnum < numfrag) {
      if(fragnum == numfrag-1) {
        /* last fragment may not be full */
        thiscount = count-fragcount*fragnum;
      }
      //printf("[%i] reduce %i elements from %lu\n", rank, thiscount, roffset); 

      /* REDUCE - PART last node does not recv */
      if(rank != p-1) {
        res = NBC_Sched_recv((char*)roffset, true, thiscount, datatype, rrpeer, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        res = NBC_Sched_barrier(schedule);
        /* root reduces into receivebuf */
        if(rank == 0) {
          res = NBC_Sched_op((char*)recvbuf+roffset, false, (char*)sendbuf+roffset, false, (char*)roffset, true, thiscount, datatype, op, schedule);
        } else {
          res = NBC_Sched_op((char*)roffset, true, (char*)sendbuf+roffset, false, (char*)roffset, true, thiscount, datatype, op, schedule);
        }
        res = NBC_Sched_barrier(schedule);
      }

      /* REDUCE PART root does not send */
      if(rank != 0) {
        /* rank p-1 has to send out of sendbuffer :) */
        if(rank == p-1) {
          res = NBC_Sched_send((char*)sendbuf+roffset, false, thiscount, datatype, srpeer, schedule);
        } else {
          res = NBC_Sched_send((char*)roffset, true, thiscount, datatype, srpeer, schedule);
        }
        if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
        /* this barrier here seems awkward but isn't!!!! */
        //res = NBC_Sched_barrier(schedule);
      }
    }

    /* BCAST from rank 0 */
    if(fragnum >= bstart) {
      //printf("[%i] bcast %i elements from %lu\n", rank, thiscount, boffset); 
      if(fragnum == bend-1) {
        /* last fragment may not be full */
        thiscount = count-fragcount*(fragnum-bstart);
      }
      
      /* BCAST PART root does not receive */
      if(rank != 0) {
        res = NBC_Sched_recv((char*)recvbuf+boffset, false, thiscount, datatype, rbpeer, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_recv() (%i)\n", res); return res; }
        res = NBC_Sched_barrier(schedule);
      }
      
      /* BCAST PART last rank does not send */
      if(rank != p-1) {
        res = NBC_Sched_send((char*)recvbuf+boffset, false, thiscount, datatype, sbpeer, schedule);
        if (NBC_OK != res) { printf("Error in NBC_Sched_send() (%i)\n", res); return res; }
        res = NBC_Sched_barrier(schedule);
      }
    }
  }

  //NBC_PRINT_SCHED(*schedule);

  return NBC_OK;
}
