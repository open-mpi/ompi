/*
 * $HEADER$
 */

/* VPS: gotto change this someday, now including mpi.h, replace this
   with internal calls. Will gotto code some back end calls for
   this  */
#include "mpi.h"
#include "datatype/datatype.h"

/*
 *	lam_dtsndrcv
 *
 *	Function:	- copy MPI message from buffer into another
 *			- send/recv done if cannot optimize
 *	Accepts:	- send buffer
 *			- send count
 *			- send datatype
 *			- receive buffer
 *			- receive count
 *			- receive datatype
 *			- tag
 *			- communicator
 *	Returns:	- MPI_SUCCESS or error code
 */
int
lam_ddt_sndrcv(void *sbuf, int scount, MPI_Datatype sdtype, void *rbuf,
	     int rcount, MPI_Datatype rdtype, int tag, MPI_Comm comm)
{
    int		err;			/* error code */
    int		size;			/* packed size */
    int		rank;			/* caller's rank */
    MPI_Status	stat;			/* status info */
    int position = 0;

    /*
     * If same datatypes used, just copy.
     */
    if (sdtype == rdtype) {
	if (scount <= rcount) {
	    lam_ddt_copy_content_same_ddt(rdtype, scount, (char *) rbuf,
					  (char *) sbuf);
	    err = MPI_SUCCESS;
	}
    }
    /*
     * If receive packed.
     */
    else if (rdtype == MPI_PACKED) {
	MPI_Pack_size(scount, sdtype, MPI_COMM_WORLD, &size);
	if (size <= rcount) {
	    if (MPI_SUCCESS == MPI_Pack(sbuf, scount, sdtype,
			 rbuf, rcount, &position, MPI_COMM_WORLD)) {
		err = MPI_SUCCESS;
	    }
	}
    }
    /*
     * If send packed.
     */
    else if (sdtype == MPI_PACKED) {
	MPI_Pack_size(rcount, rdtype, MPI_COMM_WORLD, &size);
	if (size >= scount) {
	    if (MPI_SUCCESS == MPI_Unpack(sbuf, scount, &position,
					  rbuf, rcount, rdtype, 
					  MPI_COMM_WORLD)) {
		err = MPI_SUCCESS;
	    }
	}
    }
    /*
     * Let send/recv handle it.
     */
    else {
	MPI_Comm_rank(comm, &rank);
	err = MPI_Sendrecv(sbuf, scount, sdtype, rank, tag,
			   rbuf, rcount, rdtype, rank, tag, comm, &stat);
    }

    return(err);
}

