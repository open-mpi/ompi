/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_INDEXED = mpi_type_indexed_f
#pragma weak pmpi_type_indexed = mpi_type_indexed_f
#pragma weak pmpi_type_indexed_ = mpi_type_indexed_f
#pragma weak pmpi_type_indexed__ = mpi_type_indexed_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_INDEXED,
                           pmpi_type_indexed,
                           pmpi_type_indexed_,
                           pmpi_type_indexed__,
                           pmpi_type_indexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_INDEXED = mpi_type_indexed_f
#pragma weak mpi_type_indexed = mpi_type_indexed_f
#pragma weak mpi_type_indexed_ = mpi_type_indexed_f
#pragma weak mpi_type_indexed__ = mpi_type_indexed_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_INDEXED,
                           mpi_type_indexed,
                           mpi_type_indexed_,
                           mpi_type_indexed__,
                           mpi_type_indexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_indexed_f(MPI_Fint *count, MPI_Fint *array_of_blocklengths,
			MPI_Fint *array_of_displacements, MPI_Fint *oldtype,
			MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;
    OMPI_ARRAY_NAME_DECL(array_of_blocklengths);

    OMPI_ARRAY_FINT_2_INT(array_of_blocklengths, *count);

    *ierr = OMPI_INT_2_FINT(MPI_Type_indexed(OMPI_FINT_2_INT(*count),
                                             OMPI_ARRAY_NAME_CONVERT(array_of_blocklengths), 
                                             array_of_displacements, c_old, &c_new));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_blocklengths);

    if( MPI_SUCCESS != (*ierr) ) {
       c_new = MPI_DATATYPE_NULL;
    }
    *newtype = MPI_Type_c2f(c_new);
}
