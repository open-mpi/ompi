/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ATTR_PUT = mpi_attr_put_f
#pragma weak pmpi_attr_put = mpi_attr_put_f
#pragma weak pmpi_attr_put_ = mpi_attr_put_f
#pragma weak pmpi_attr_put__ = mpi_attr_put_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ATTR_PUT,
                           pmpi_attr_put,
                           pmpi_attr_put_,
                           pmpi_attr_put__,
                           pmpi_attr_put_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ATTR_PUT = mpi_attr_put_f
#pragma weak mpi_attr_put = mpi_attr_put_f
#pragma weak mpi_attr_put_ = mpi_attr_put_f
#pragma weak mpi_attr_put__ = mpi_attr_put_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ATTR_PUT,
                           mpi_attr_put,
                           mpi_attr_put_,
                           mpi_attr_put__,
                           mpi_attr_put_f,
                           (MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, MPI_Fint *ierr),
                           (comm, keyval, attribute_val, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_attr_put_f(MPI_Fint *comm, MPI_Fint *keyval, MPI_Fint *attribute_val, 
		    MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    int *c_value;

    c_comm = MPI_Comm_f2c(*comm);

    /* This stuff is very confusing.  Be sure to see MPI-2 4.12.7. */

    /* Note that this function deals with attribute values that are
       the size of Fortran INTEGERS; MPI_ATTR_PUT deals with attribute
       values that are the size of address integers.  Hence, it is
       possible that the C value is larger than the Fortran value.
       MPI says that we sign-extend in this case. */

    c_value = (int *) *attribute_val;
    *ierr = OMPI_INT_2_FINT(MPI_Attr_put(c_comm, 
					 OMPI_FINT_2_INT(*keyval), 
					 attribute_val));
    if (MPI_SUCCESS == *ierr) {
        
    }
}
