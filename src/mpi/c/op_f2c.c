/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "op/op.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Op_f2c = PMPI_Op_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Op_f2c";


MPI_Op MPI_Op_f2c(MPI_Fint op_f)
{
    size_t op_index = (size_t) op_f;

    /* Error checking */

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }
    
    /* Per MPI-2:4.12.4, do not invoke an error handler if we get an
       invalid fortran handle */
    
    if (op_index < 0 || 
        op_index >= 
        ompi_pointer_array_get_size(ompi_op_f_to_c_table)) {
        return MPI_OP_NULL;
    }

    return ompi_pointer_array_get_item(ompi_op_f_to_c_table, op_index);
}
