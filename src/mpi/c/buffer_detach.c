/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/pml_base_bsend.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Buffer_detach = PMPI_Buffer_detach
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Buffer_detach(void *buffer, int *size) 
{
    return mca_pml_base_bsend_detach(buffer, size);
}
