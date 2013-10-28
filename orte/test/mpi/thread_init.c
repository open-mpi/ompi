#include <mpi.h>
#include <stdio.h>
int main(int argc, const char* argv[]) {
 int provided = -1;
 printf("Calling MPI_Init_thread...\n");
 MPI_Init_thread(NULL, NULL, MPI_THREAD_MULTIPLE, &provided);
 printf("MPI_Init_thread returned, provided = %d\n", provided);
 MPI_Finalize();
 return 0;
}

