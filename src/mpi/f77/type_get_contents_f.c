/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_CONTENTS = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents_ = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents__ = mpi_type_get_contents_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_CONTENTS,
                           pmpi_type_get_contents,
                           pmpi_type_get_contents_,
                           pmpi_type_get_contents__,
                           pmpi_type_get_contents_f,
                           (MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Fint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr),
                           (mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_CONTENTS = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents_ = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents__ = mpi_type_get_contents_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_CONTENTS,
                           mpi_type_get_contents,
                           mpi_type_get_contents_,
                           mpi_type_get_contents__,
                           mpi_type_get_contents_f,
                           (MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Fint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr),
                           (mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_get_contents_f(MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Fint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr)
{
    
    MPI_Aint *c_address_array = NULL;
    MPI_Datatype *c_datatype_array = NULL;
    MPI_Datatype c_mtype = MPI_Type_f2c(*mtype);
    int i;

    if (*max_datatypes) {
        c_datatype_array = malloc(*max_datatypes * sizeof(MPI_Datatype));
        if (c_datatype_array == (MPI_Datatype*) NULL) {
            *ierr = MPI_ERR_INTERN;
            return;
        }
    }

    if (*max_addresses) {
        c_address_array = malloc(*max_addresses * sizeof(MPI_Aint));
        if (c_address_array == (MPI_Aint *) NULL) {
            /*  prevent memory leaks */
            if (c_datatype_array != (MPI_Datatype*) NULL)
              free(c_datatype_array);

            *ierr = MPI_ERR_INTERN;
            return;
        }
    }

    *ierr = MPI_Type_get_contents(c_mtype, *max_integers, *max_addresses,
                                  *max_datatypes, array_of_integers, 
                                  c_address_array, c_datatype_array);

    if (*ierr == MPI_SUCCESS) {
        for (i = 0; i < *max_addresses; i++) {
            array_of_addresses[i] = (MPI_Fint)c_address_array[i];
        }
        for (i = 0; i < *max_datatypes; i++) {
          array_of_datatypes[i] = MPI_Type_c2f(c_datatype_array[i]);
        }
    }
    free(c_address_array);
    free(c_datatype_array);
}
