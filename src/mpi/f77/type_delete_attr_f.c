/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_DELETE_ATTR = mpi_type_delete_attr_f
#pragma weak pmpi_type_delete_attr = mpi_type_delete_attr_f
#pragma weak pmpi_type_delete_attr_ = mpi_type_delete_attr_f
#pragma weak pmpi_type_delete_attr__ = mpi_type_delete_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_DELETE_ATTR,
                           pmpi_type_delete_attr,
                           pmpi_type_delete_attr_,
                           pmpi_type_delete_attr__,
                           pmpi_type_delete_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Fint *ierr),
                           (type, type_keyval, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_DELETE_ATTR = mpi_type_delete_attr_f
#pragma weak mpi_type_delete_attr = mpi_type_delete_attr_f
#pragma weak mpi_type_delete_attr_ = mpi_type_delete_attr_f
#pragma weak mpi_type_delete_attr__ = mpi_type_delete_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_DELETE_ATTR,
                           mpi_type_delete_attr,
                           mpi_type_delete_attr_,
                           mpi_type_delete_attr__,
                           mpi_type_delete_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, MPI_Fint *ierr),
                           (type, type_keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_delete_attr_f(MPI_Fint *type, MPI_Fint *type_keyval, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*type);

    *ierr = MPI_Type_delete_attr( c_type, *type_keyval ); 

    if (*ierr == MPI_SUCCESS)
      *type = MPI_Type_c2f( c_type );
}


