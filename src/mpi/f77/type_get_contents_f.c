/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_CONTENTS = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents_ = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents__ = mpi_type_get_contents_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_CONTENTS,
                           pmpi_type_get_contents,
                           pmpi_type_get_contents_,
                           pmpi_type_get_contents__,
                           pmpi_type_get_contents_f,
                           (MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Fint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr),
                           (mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_CONTENTS = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents_ = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents__ = mpi_type_get_contents_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_GET_CONTENTS,
                           mpi_type_get_contents,
                           mpi_type_get_contents_,
                           mpi_type_get_contents__,
                           mpi_type_get_contents_f,
                           (MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Fint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr),
                           (mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_get_contents_f(MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Fint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr)
{

}
