/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_COMM_GET_ATTR = mpi_comm_get_attr_f
#pragma weak pmpi_comm_get_attr = mpi_comm_get_attr_f
#pragma weak pmpi_comm_get_attr_ = mpi_comm_get_attr_f
#pragma weak pmpi_comm_get_attr__ = mpi_comm_get_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_GET_ATTR,
                           pmpi_comm_get_attr,
                           pmpi_comm_get_attr_,
                           pmpi_comm_get_attr__,
                           pmpi_comm_get_attr_f,
                           (MPI_Fint *comm, MPI_Fint *comm_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (comm, comm_keyval, attribute_val, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_ATTR = mpi_comm_get_attr_f
#pragma weak mpi_comm_get_attr = mpi_comm_get_attr_f
#pragma weak mpi_comm_get_attr_ = mpi_comm_get_attr_f
#pragma weak mpi_comm_get_attr__ = mpi_comm_get_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_GET_ATTR,
                           mpi_comm_get_attr,
                           mpi_comm_get_attr_,
                           mpi_comm_get_attr__,
                           mpi_comm_get_attr_f,
                           (MPI_Fint *comm, MPI_Fint *comm_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (comm, comm_keyval, attribute_val, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_comm_get_attr_f(MPI_Fint *comm, MPI_Fint *comm_keyval,
			 char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(flag);

    c_comm = MPI_Comm_f2c(*comm);

    *ierr = OMPI_INT_2_FINT(MPI_Comm_get_attr(c_comm, 
					      OMPI_FINT_2_INT(*comm_keyval),
					      attribute_val,
					      OMPI_SINGLE_NAME_CONVERT(flag)));

    OMPI_SINGLE_INT_2_FINT(flag);
}
