/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "include/constants.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "mpi/f77/bindings.h"
#include "mpi/f77/strings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_SET_NAME = mpi_comm_set_name_f
#pragma weak pmpi_comm_set_name = mpi_comm_set_name_f
#pragma weak pmpi_comm_set_name_ = mpi_comm_set_name_f
#pragma weak pmpi_comm_set_name__ = mpi_comm_set_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_SET_NAME,
                            pmpi_comm_set_name,
                            pmpi_comm_set_name_,
                            pmpi_comm_set_name__,
                            pmpi_comm_set_name_f,
                            (MPI_Fint *comm, char *comm_name, MPI_Fint *ierr, int name_len),
                            (comm, comm_name, ierr, name_len) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SET_NAME = mpi_comm_set_name_f
#pragma weak mpi_comm_set_name = mpi_comm_set_name_f
#pragma weak mpi_comm_set_name_ = mpi_comm_set_name_f
#pragma weak mpi_comm_set_name__ = mpi_comm_set_name_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_SET_NAME,
                            mpi_comm_set_name,
                            mpi_comm_set_name_,
                            mpi_comm_set_name__,
                            mpi_comm_set_name_f,
                            (MPI_Fint *comm, char *comm_name, MPI_Fint *ierr, int name_len),
                            (comm, comm_name, ierr, name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_set_name_f(MPI_Fint *comm, char *comm_name, MPI_Fint *ierr, 
                         int name_len)
{
    int ret;
    char *c_name;
    MPI_Comm c_comm = MPI_Comm_f2c(*comm);

    /* Convert the fortran string */

    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(comm_name, name_len,
                                                       &c_name))) {
        *ierr = OMPI_INT_2_FINT(OMPI_ERRHANDLER_INVOKE(c_comm, ret,
                                                       "MPI_COMM_SET_NAME"));
        return;
    }

    /* Call the C function */

    *ierr = OMPI_INT_2_FINT(MPI_Comm_set_name(c_comm, c_name));

    /* Free the C name */

    free(c_name);
}
