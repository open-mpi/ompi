/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_ATTR = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr_ = mpi_type_get_attr_f
#pragma weak pmpi_type_get_attr__ = mpi_type_get_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_ATTR,
                           pmpi_type_get_attr,
                           pmpi_type_get_attr_,
                           pmpi_type_get_attr__,
                           pmpi_type_get_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (type, type_keyval, attribute_val, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_ATTR = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr_ = mpi_type_get_attr_f
#pragma weak mpi_type_get_attr__ = mpi_type_get_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_ATTR,
                           mpi_type_get_attr,
                           mpi_type_get_attr_,
                           mpi_type_get_attr__,
                           mpi_type_get_attr_f,
                           (MPI_Fint *type, MPI_Fint *type_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (type, type_keyval, attribute_val, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_get_attr_f(MPI_Fint *type, MPI_Fint *type_keyval,
			 char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c( *type );
    OMPI_SINGLE_NAME_DECL(flag);

    *ierr = OMPI_INT_2_FINT(MPI_Type_get_attr(c_type, 
					      OMPI_FINT_2_INT(*type_keyval),
					      attribute_val, 
					      OMPI_SINGLE_NAME_CONVERT(flag)));

    OMPI_SINGLE_INT_2_FINT(flag);
}
