#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

int main(int argc, char **argv) {
   int debugme = 1;

   MPI_Init(&argc, &argv);
   printf("init...\n");
   fflush(0);
   MPI_Finalize();
   exit(77);
}
