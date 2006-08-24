#define IB
#include <mpi.h>
#include "ib.h"

int main(int argc, char **argv) {
  int rank, res, size, i, loops, j, tag;
  IB_Request req;
  double t1=0, t2=0, t3=0, t4=0, t5, t6, t7;
  MPI_Request mpireq;
  void *buf2;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  size = 1;
  loops= 50;
  
  buf2 = malloc(size);
  if(buf2 == NULL) printf("malloc() error\n");
  
  if(rank == 0) {
    res = IB_Isend(buf2, size, MPI_BYTE, 1, 1, MPI_COMM_WORLD, &req);
    if(res) printf("Error in IB_Send (%i) \n", res);
    res = IB_Wait(&req);
    res = IB_Irecv(buf2, size, MPI_BYTE, 1, 1, MPI_COMM_WORLD, &req);
    if(res) printf("Error in IB_Recv (%i) \n", res);
    res = IB_Wait(&req);
  } else {
    res = IB_Irecv(buf2, size, MPI_BYTE, 0, 1, MPI_COMM_WORLD, &req);
    if(res) printf("Error in IB_Recv (%i)\n", res);
    res = IB_Wait(&req);
    res = IB_Isend(buf2, size, MPI_BYTE, 0, 1, MPI_COMM_WORLD, &req);
    if(res) printf("Error in IB_Send (%i) \n", res);
    res = IB_Wait(&req);
  }
  
  printf("[%i] MEASUREMENT\n", rank);
  t1=t2=t3=t4=t5=t6=0;



  for(j=1; j<loops+1;j++) {
    i = 0;
    tag = j;

t1 -= MPI_Wtime();
    if(!rank) {
t2 -= MPI_Wtime();
      res = IB_Isend(buf2+i, size-i, MPI_BYTE, 1, tag, MPI_COMM_WORLD, &req);
t2 += MPI_Wtime();
      if(res) printf("Error in IB_Send (%i) \n", res);
      res = IB_Wait(&req);
      
t3 -= MPI_Wtime();
      res = IB_Irecv(buf2+i, size-i, MPI_BYTE, 1, tag, MPI_COMM_WORLD, &req);
t3 += MPI_Wtime();
      if(res) printf("Error in IB_Recv (%i) \n", res);
      res = IB_Wait(&req);
    } else {
t3 -= MPI_Wtime();
      res = IB_Irecv(buf2+i, size-i, MPI_BYTE, 0, tag, MPI_COMM_WORLD, &req);
      if(res) printf("Error in IB_Recv (%i)\n", res);
      res = IB_Wait(&req);
t3 += MPI_Wtime();
      
t2 -= MPI_Wtime();
      res = IB_Isend(buf2+i, size-i, MPI_BYTE, 0, tag, MPI_COMM_WORLD, &req);
t2 += MPI_Wtime();
      if(res) printf("Error in IB_Send (%i) \n", res);
      res = IB_Wait(&req);
    }
t1 += MPI_Wtime();

t4 -= MPI_Wtime();
    if(!rank) {
t5 -= MPI_Wtime();
      MPI_Isend(buf2, size, MPI_BYTE, 1, 1, MPI_COMM_WORLD, &mpireq);
t5 += MPI_Wtime();
      res = MPI_Wait(&mpireq, MPI_STATUS_IGNORE);
      
t6 -= MPI_Wtime();
      MPI_Irecv(buf2, size, MPI_BYTE, 1, 1, MPI_COMM_WORLD, &mpireq);
t6 += MPI_Wtime();
      res = MPI_Wait(&mpireq, MPI_STATUS_IGNORE);
    } else {
t6 -= MPI_Wtime();
      MPI_Irecv(buf2, size, MPI_BYTE, 0, 1, MPI_COMM_WORLD, &mpireq);
t6 += MPI_Wtime();
      res = MPI_Wait(&mpireq, MPI_STATUS_IGNORE);
      
t5 -= MPI_Wtime();
      MPI_Isend(buf2, size, MPI_BYTE, 0, 1, MPI_COMM_WORLD, &mpireq);
t5 += MPI_Wtime();
      res = MPI_Wait(&mpireq, MPI_STATUS_IGNORE);
    }
t4 += MPI_Wtime();
    printf("[%i] %lf (%lf %lf) | %lf (%lf %lf) \n", rank, t1*1e6/j, t2*1e6/j, t3*1e6/j, t4*1e6/j, t5*1e6/j,t6*1e6/j);
  }
  free(buf2);

  MPI_Finalize(); 

  return 0;
}
