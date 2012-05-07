#include <stdio.h>
#include <mpi.h>
#include "mpi-ext.h"

int main(int argc, char* argv[])
{
    int rank;
    char ompi_bound[OMPI_AFFINITY_STRING_MAX];
    char current_binding[OMPI_AFFINITY_STRING_MAX];
    char exists[OMPI_AFFINITY_STRING_MAX];
    
    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    
    OMPI_Affinity_str(OMPI_AFFINITY_RSRC_STRING_FMT,
                      ompi_bound, current_binding, exists);
    printf("rank %d (resource string): \n"
           "       ompi_bound: %s\n"
           "  current_binding: %s\n"
           "           exists: %s\n",
           rank, ompi_bound, current_binding, exists);

    OMPI_Affinity_str(OMPI_AFFINITY_LAYOUT_FMT,
                      ompi_bound, current_binding, exists);
    printf("rank %d (layout): \n"
           "       ompi_bound: %s\n"
           "  current_binding: %s\n"
           "           exists: %s\n",
           rank, ompi_bound, current_binding, exists);
    MPI_Finalize();
    return 0;
}
