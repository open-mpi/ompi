/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_ENVELOPE = mpi_type_get_envelope_f
#pragma weak pmpi_type_get_envelope = mpi_type_get_envelope_f
#pragma weak pmpi_type_get_envelope_ = mpi_type_get_envelope_f
#pragma weak pmpi_type_get_envelope__ = mpi_type_get_envelope_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_ENVELOPE,
                           pmpi_type_get_envelope,
                           pmpi_type_get_envelope_,
                           pmpi_type_get_envelope__,
                           pmpi_type_get_envelope_f,
                           (MPI_Fint *type, MPI_Fint *num_integers, MPI_Fint *num_addresses, MPI_Fint *num_datatypes, MPI_Fint *combiner, MPI_Fint *ierr),
                           (type, num_integers, num_addresses, num_datatypes, combiner, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_ENVELOPE = mpi_type_get_envelope_f
#pragma weak mpi_type_get_envelope = mpi_type_get_envelope_f
#pragma weak mpi_type_get_envelope_ = mpi_type_get_envelope_f
#pragma weak mpi_type_get_envelope__ = mpi_type_get_envelope_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_GET_ENVELOPE,
                           mpi_type_get_envelope,
                           mpi_type_get_envelope_,
                           mpi_type_get_envelope__,
                           mpi_type_get_envelope_f,
                           (MPI_Fint *type, MPI_Fint *num_integers, MPI_Fint *num_addresses, MPI_Fint *num_datatypes, MPI_Fint *combiner, MPI_Fint *ierr),
                           (type, num_integers, num_addresses, num_datatypes, combiner, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_get_envelope_f(MPI_Fint *type, MPI_Fint *num_integers, MPI_Fint *num_addresses, MPI_Fint *num_datatypes, MPI_Fint *combiner, MPI_Fint *ierr)
{

}
