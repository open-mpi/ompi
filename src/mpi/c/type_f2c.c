/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_f2c = PMPI_Type_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_f2c";


MPI_Datatype MPI_Type_f2c(MPI_Fint datatype)
{
    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (0 > datatype || datatype >= ompi_pointer_array_get_size(ompi_datatype_f_to_c_table)) {
            return MPI_DATATYPE_NULL;
        }
    }
    /* return the index */
    return ompi_datatype_f_to_c_table->addr[datatype];
}

