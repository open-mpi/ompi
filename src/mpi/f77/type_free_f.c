/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_FREE = mpi_type_free_f
#pragma weak pmpi_type_free = mpi_type_free_f
#pragma weak pmpi_type_free_ = mpi_type_free_f
#pragma weak pmpi_type_free__ = mpi_type_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_FREE,
                           pmpi_type_free,
                           pmpi_type_free_,
                           pmpi_type_free__,
                           pmpi_type_free_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_FREE = mpi_type_free_f
#pragma weak mpi_type_free = mpi_type_free_f
#pragma weak mpi_type_free_ = mpi_type_free_f
#pragma weak mpi_type_free__ = mpi_type_free_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_FREE,
                           mpi_type_free,
                           mpi_type_free_,
                           mpi_type_free__,
                           mpi_type_free_f,
                           (MPI_Fint *type, MPI_Fint *ierr),
                           (type, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_free_f(MPI_Fint *type, MPI_Fint *ierr)
{
    MPI_Datatype c_type;
    
    c_type = MPI_Type_f2c(*type);
    
    *ierr = OMPI_INT_2_FINT(MPI_Type_free(&c_type));

    *type = MPI_Type_c2f(c_type);
}
