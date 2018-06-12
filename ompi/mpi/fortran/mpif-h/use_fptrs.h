// The way this is used in the Fortran wrapper C files is
// code that used to say
//     int mpi_send(....) {
//         ...
//         MPI_Send(....);
//         ...
//     }
// will become
//     int mpi_send(....) {
//         ...
//         ompi_fptr_MPI_Send(....);
//         ...
//     }
// and in the small number of functions that can happen before MPI_Init
// those functions also get an ompi_fptr_init(0) call added.

#include "constructed_fptr_declarations.h"

void ompi_fptr_init(int mca_system_is_ready);
