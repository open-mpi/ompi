/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "datatype/datatype.h"
#include "communicator/communicator.h"
#include "request/request.h"
#include "mca/pml/pml.h"


/*
 *	ompi_dtsndrcv
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
int ompi_ddt_sndrcv(void *sbuf, int scount, MPI_Datatype sdtype, void *rbuf,
                    int rcount, MPI_Datatype rdtype, int tag, MPI_Comm comm)
{
    int err;
    unsigned int size;
    int rank;
    int position = 0;
    ompi_convertor_t *local_convertor;
    ompi_request_t *req;

    /* If same datatypes used, just copy. */

    if (sdtype == rdtype) {
	if (scount <= rcount) {
	    ompi_ddt_copy_content_same_ddt(rdtype, scount, (char *) rbuf,
					  (char *) sbuf);
	    err = MPI_SUCCESS;
	} else {
            err = MPI_ERR_TRUNCATE;
        }
    }

    /* If receive packed. */

    else if (rdtype == MPI_PACKED) {
        local_convertor = OBJ_NEW(ompi_convertor_t);
        ompi_convertor_init_for_send( local_convertor, 0, sdtype,
				      scount, NULL, 0, NULL );
        err = ompi_convertor_get_packed_size(local_convertor, &size);
        OBJ_RELEASE(local_convertor);
        if (OMPI_SUCCESS != err) {
            return err;
        }

	if (size <= rcount) {
	    err = MPI_Pack(sbuf, scount, sdtype,
                           rbuf, rcount, &position, MPI_COMM_WORLD);
	} else {
            err = MPI_ERR_TRUNCATE;
        }
    }

    /* If send packed. */

    else if (sdtype == MPI_PACKED) {
        local_convertor = OBJ_NEW(ompi_convertor_t);
        ompi_convertor_init_for_send(local_convertor, 0, rdtype,
                                     rcount, NULL, 0, NULL );
        err = ompi_convertor_get_packed_size(local_convertor, &size);
        OBJ_RELEASE(local_convertor);
        if (OMPI_SUCCESS != err) {
            return err;
        }

	if (scount <= size) {
	    err = MPI_Unpack(sbuf, scount, &position,
                             rbuf, rcount, rdtype, 
                             MPI_COMM_WORLD);
	} else {
            err = MPI_ERR_TRUNCATE;
        }
    }

    /* Let the PML handle it (i.e., do a normal send/recv) */

    else {
        rank = ompi_comm_rank(comm);
        err = mca_pml.pml_irecv(rbuf, rcount, rdtype, rank, tag, comm, &req);
        if (MPI_SUCCESS != err) {
            return err;
        }
        err = mca_pml.pml_send(sbuf, scount, sdtype, rank, tag,
                               MCA_PML_BASE_SEND_STANDARD, comm);
        if (MPI_SUCCESS != err) {
            return err;
        }
        err = mca_pml.pml_wait(1, &req, NULL, MPI_STATUS_IGNORE);
    }

    return err;
}

