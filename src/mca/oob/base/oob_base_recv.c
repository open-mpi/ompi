#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include <netinet/in.h>
#include <string.h>
#include "util/pack.h"


/*
* Similiar to unix recv(2)
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
*                     iovec array without removing the message from the queue.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/
int mca_oob_recv(ompi_process_name_t* peer, struct iovec *msg, int count, int* tag, int flags)
{
    return(mca_oob.oob_recv(peer, msg, count, tag, flags));
}

/*
* Similiar to unix recv(2)
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
* @param buffer (OUT) Buffer that the OOB creates to recv this message...
* @param tag (IN)     User defined tag for matching send/recv.
*                     iovec array without removing the message from the queue.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/
int mca_oob_recv_packed (ompi_process_name_t* peer, ompi_buffer_t *buf, int* tag)
{
	/* ok, this routine is a bit of a cow */
        /* the oob_recv actually needs the real target buffers in advance */
	
	/* this forces a three stage method */
	/* first we need to peek the size we will need */
	/* then we allocate a buffer of the correct size and then */
	/* we post a recv with the matching iov :) */
	/* and we hope that someone inbtween has not posted a recv */
	/* that matches. */
	/* To avoid any RACE we NEED to change the OOB lowlevel to */
	/* alloc the buffer for us.. as per original design.  */
	/* Or do locking on all recv posting between the peek and recv! GEF */

	uint32_t insize;
    int rc;
    struct iovec msg[1];
	ompi_buffer_t tmpbuf;
	void *targetptr;

	insize = mca_oob.oob_recv(peer, NULL, 0, tag, MCA_OOB_PEEK|MCA_OOB_TRUNC);

	if (OMPI_ERROR==insize) { return (rc); }

	targetptr = (void*) malloc (insize);
	if (!targetptr) { return (OMPI_ERROR); }

	rc = ompi_buffer_init_preallocated (&tmpbuf, targetptr, insize);
	if (OMPI_ERROR==rc) { return (rc); }

	/* now update the IOV */
	msg[0].iov_base = (char*) targetptr;
	msg[0].iov_len  = insize;

    rc = mca_oob.oob_recv(peer, msg, 1, tag, 0);

	if (OMPI_ERROR!=rc) *buf = tmpbuf;

	return (rc);
}
