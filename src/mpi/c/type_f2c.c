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


OMPI_EXPORT
MPI_Datatype MPI_Type_f2c(MPI_Fint datatype)
{
    size_t datatype_index = (size_t) datatype;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /* Per MPI-2:4.12.4, do not invoke an error handler if we get an
       invalid fortran handle */
    
    if (datatype_index < 0 || 
        datatype_index >= 
        ompi_pointer_array_get_size(ompi_datatype_f_to_c_table)) {
        return MPI_DATATYPE_NULL;
    }

    return ompi_pointer_array_get_item(ompi_datatype_f_to_c_table, datatype);
}

