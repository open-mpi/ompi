/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_SET_ATTR = mpi_type_set_attr_f
#pragma weak pmpi_type_set_attr = mpi_type_set_attr_f
#pragma weak pmpi_type_set_attr_ = mpi_type_set_attr_f
#pragma weak pmpi_type_set_attr__ = mpi_type_set_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_SET_ATTR,
                           pmpi_type_set_attr,
                           pmpi_type_set_attr_,
                           pmpi_type_set_attr__,
                           pmpi_type_set_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, char *attr_val, MPI_Fint *ierr),
                           (type, type_keyval, attr_val, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_SET_ATTR = mpi_type_set_attr_f
#pragma weak mpi_type_set_attr = mpi_type_set_attr_f
#pragma weak mpi_type_set_attr_ = mpi_type_set_attr_f
#pragma weak mpi_type_set_attr__ = mpi_type_set_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_SET_ATTR,
                           mpi_type_set_attr,
                           mpi_type_set_attr_,
                           mpi_type_set_attr__,
                           mpi_type_set_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, char *attr_val, MPI_Fint *ierr),
                           (type, type_keyval, attr_val, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_set_attr_f(MPI_Fint *type, MPI_Fint *type_keyval, char *attr_val, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c( *type );

    *ierr = MPI_Type_set_attr( c_type, *type_keyval, attr_val );

    if (*ierr == MPI_SUCCESS)
        *type = MPI_Type_c2f( c_type );

}
