/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "mpi/f77/constants.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"
#include "mpi/runtime/params.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_IPROBE = mpi_iprobe_f
#pragma weak pmpi_iprobe = mpi_iprobe_f
#pragma weak pmpi_iprobe_ = mpi_iprobe_f
#pragma weak pmpi_iprobe__ = mpi_iprobe_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IPROBE,
                           pmpi_iprobe,
                           pmpi_iprobe_,
                           pmpi_iprobe__,
                           pmpi_iprobe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, flag, status, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IPROBE = mpi_iprobe_f
#pragma weak mpi_iprobe = mpi_iprobe_f
#pragma weak mpi_iprobe_ = mpi_iprobe_f
#pragma weak mpi_iprobe__ = mpi_iprobe_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IPROBE,
                           mpi_iprobe,
                           mpi_iprobe_,
                           mpi_iprobe__,
                           mpi_iprobe_f,
                           (MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm, MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (source, tag, comm, flag, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_iprobe_f(MPI_Fint *source, MPI_Fint *tag, MPI_Fint *comm,
		  MPI_Fint *flag, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Status *c_status;
    int c_err;
    MPI_Comm c_comm;
#if OMPI_SIZEOF_FORTRAN_INT != SIZEOF_INT
    MPI_Status c_status2;
#endif
    OMPI_SINGLE_NAME_DECL(flag);

    c_comm = MPI_Comm_f2c (*comm);

    /* Only check for the bad value if we're checking MPI parameters */

    if (MPI_PARAM_CHECK) {
        if (OMPI_IS_FORTRAN_STATUSES_IGNORE(status)) {
            c_err = OMPI_ERRHANDLER_INVOKE(c_comm, MPI_ERR_ARG,
					   "MPI_RECV");
	    *ierr = OMPI_INT_2_FINT(c_err);
            return;
        }
    }

    /* See if we got MPI_STATUS_IGNORE */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        c_status = MPI_STATUS_IGNORE;
    } else {

        /* If sizeof(int) == sizeof(INTEGER), then there's no
           translation necessary -- let the underlying functions write
           directly into the Fortran status */

#if OMPI_SIZEOF_FORTRAN_INT == SIZEOF_INT
        c_status = (MPI_Status *) status;
#else
        c_status = &c_status2;
#endif
    }

    *ierr = OMPI_INT_2_FINT(MPI_Iprobe(OMPI_FINT_2_INT(*source),
				       OMPI_FINT_2_INT(*tag),
				       c_comm, OMPI_SINGLE_NAME_CONVERT(flag), 
				       c_status));

    OMPI_SINGLE_INT_2_FINT(flag);

#if OMPI_SIZEOF_FORTRAN_INT != SIZEOF_INT
    if (MPI_STATUS_IGNORE != c_status) {
        MPI_Status_c2f(c_status, status);
    }
#endif

}
