#include "nbc.h"
#include <unistd.h>

int main(int argc, char **argv) {
  NBC_Handle handle1, handle2;
  int rank, i,j, res, p;
  int *buf1, *buf2, *ptr;
  
  MPI_Init(&argc, &argv);

  /* shut up compiler */
  handle1 = handle2;
  handle2 = handle1;
  
  res = MPI_Comm_size(MPI_COMM_WORLD, &p);
  res = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  if(res != MPI_SUCCESS) printf("Error in MPI_Comm_rank!\n");

  buf1=malloc(2*p*sizeof(int));
  buf2=malloc(2*p*sizeof(int));

  for (i=0; i<1; i++) {
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = rank; 
      ptr = buf2+j;
      *ptr = 0;
    }

#if 0
    /**************************** BCAST ********************************/
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = rank; 
      ptr = buf2+j;
      *ptr = 0;
    }

    printf("#[%i] Bcast before: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    

    NBC_Ibcast(buf1, p, MPI_INT, 0, MPI_COMM_WORLD, &handle1);
    NBC_Wait(&handle1);
    
    printf("#[%i] Bcast after: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      printf("%i ", *ptr);
    }
    printf("\n");
#endif
#if 0
    /**************************** GATHER ********************************/
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = rank; 
      ptr = buf2+j;
      *ptr = 0;
    }

    if(rank ==0)  {
      printf("#[%i] Gather before: ", rank);
      for(j=0;j<p;j++) {
        ptr = buf2+j;
        printf("%i ", *ptr);
      }
      printf("\n");
    }
    
    NBC_Igather(buf1, 1, MPI_INT, buf2, 1, MPI_INT, 0, MPI_COMM_WORLD, &handle1);
    NBC_Wait(&handle1);
    
    if(rank ==0) {
      printf("#[%i] Gather after: ", rank);
      for(j=0;j<p;j++) {
        ptr = buf2+j;
        printf("%i ", *ptr);
      }
      printf("\n");
    }

    /**************************** SCATTER ********************************/
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = j; 
      ptr = buf2+j;
      *ptr = 0;
    }

    printf("#[%i] Scatter before: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    
    NBC_Iscatter(buf1, 1, MPI_INT, buf2, 1, MPI_INT, 0, MPI_COMM_WORLD, &handle1);
    NBC_Wait(&handle1);
    
    printf("#[%i] Scatter after: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    /**************************** ALLGATHER ********************************/
    printf("#[%i] sbuf: %lu\n", rank, (unsigned long)buf1);
    printf("#[%i] rbuf: %lu\n", rank, (unsigned long)buf2);
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = rank; 
      ptr = buf2+j;
      *ptr = 0;
    }
    *buf1 = rank; 

    printf("#[%i] Allgather before: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    
    NBC_Iallgather(buf1, 1, MPI_INT, buf2, 1, MPI_INT, MPI_COMM_WORLD, &handle1);
    NBC_Wait(&handle1);
    
    printf("#[%i] Allgather after: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
#endif
    /**************************** ALLTOALL ********************************/
#if 0
    printf("#[%i] sbuf: %lu\n", rank, (unsigned long)buf1);
    printf("#[%i] rbuf: %lu\n", rank, (unsigned long)buf2);
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = j; 
      ptr = buf2+j;
      *ptr = 0;
    }

    printf("#[%i] Alltoall before: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    
    NBC_Ialltoall(buf1, 1, MPI_INT, buf2, 1, MPI_INT, MPI_COMM_WORLD, &handle1);
    NBC_Wait(&handle1);
    
    printf("#[%i] Alltoall after: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    NBC_Ialltoall(buf1, 1, MPI_INT, buf2, 1, MPI_INT, MPI_COMM_WORLD, &handle1);
    NBC_Wait(&handle1);
#endif
#if 0
    /**************************** REDUCE ********************************/
    printf("#[%i] sbuf: %lu\n", rank, (unsigned long)buf1);
    printf("#[%i] rbuf: %lu\n", rank, (unsigned long)buf2);
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = 1; 
      ptr = buf2+j;
      *ptr = 0;
    }

    printf("#[%i] Reduce before: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    
    res = NBC_Ireduce(buf1, buf2, p, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD, &handle1);
    if(res != NBC_OK) { printf("error in NBC_Ireduce(): %i\n", res); }
    NBC_Wait(&handle1);
    
    printf("#[%i] Reduce after: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
#endif
    /**************************** ALLREDUCE ********************************/
    printf("#[%i] sbuf: %lu\n", rank, (unsigned long)buf1);
    printf("#[%i] rbuf: %lu\n", rank, (unsigned long)buf2);
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = 1; 
      ptr = buf2+j;
      *ptr = 0;
    }

    printf("#[%i] Reduce before: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    
    res = NBC_Iallreduce(buf1, buf2, 2*p, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &handle1);
    if(res != NBC_OK) { printf("error in NBC_Iallreduce(): %i\n", res); }
    NBC_Wait(&handle1);
    
    printf("#[%i] Reduce after: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
#if 0
    /**************************** SCAN ********************************/
    printf("#[%i] sbuf: %lu\n", rank, (unsigned long)buf1);
    printf("#[%i] rbuf: %lu\n", rank, (unsigned long)buf2);
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = 1; 
      ptr = buf2+j;
      *ptr = 0;
    }

    printf("#[%i] Scan before: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    
    res = NBC_Iscan(buf1, buf2, p, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &handle1);
    if(res != NBC_OK) { printf("error in NBC_Iscan(): %i\n", res); }
    NBC_Wait(&handle1);
    
    printf("#[%i] Scan after: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    /**************************** REDUCE_SCATTER ********************************/
    printf("#[%i] sbuf: %lu\n", rank, (unsigned long)buf1);
    printf("#[%i] rbuf: %lu\n", rank, (unsigned long)buf2);
    for(j=0;j<p;j++) {
      ptr = buf1+j;
      *ptr = j; 
      ptr = buf2+j;
      *ptr = 0;
    }

    printf("#[%i] reduce_scatter before: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    
    recvcounts=malloc(p*sizeof(int));
    for(j=0;j<p;j++) recvcounts[j] = 1;
    res = NBC_Ireduce_scatter(buf1, buf2, recvcounts, MPI_INT, MPI_SUM, MPI_COMM_WORLD, &handle1);
    if(res != NBC_OK) { printf("error in NBC_Ireduce_scatter(): %i\n", res); }
    NBC_Wait(&handle1);
    free(recvcounts);
    
    printf("#[%i] reduce_scatter after: ", rank);
    for(j=0;j<p;j++) {
      ptr = buf2+j;
      printf("%i ", *ptr);
    }
    printf("\n");
    /**************************** END ********************************/
#endif
  }

  free(buf1);
  free(buf2);

  MPI_Finalize();
  
  return NBC_OK;
}

