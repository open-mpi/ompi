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
int mca_oob_recv(ompi_process_name_t* peer, const struct iovec *msg, int count, int tag, int flags)
{
    return(mca_oob.oob_recv(peer, msg, count, tag, flags));
}

/*
 * Receive data and convert (if required) to host byte order.
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
int mca_oob_recv_ntoh(
    ompi_process_name_t* peer, 
    const struct iovec *msg,
    const mca_oob_base_type_t *types, 
    int count, 
    int tag,
    int flags)
{
    int rc, num, i = 0;
    struct iovec * orig;
    bool convert = false;
    /* see if we actually have to convert anything */
    /* first check to see if we are already in network byte order */
    if(1 != htons(1)) {
        /* if we aren't, see if there is any data types that need to be converted */
        while(!convert && (i < count)) {
            if((types[i] == MCA_OOB_BASE_INT16) || (types[i] == MCA_OOB_BASE_INT32)) {
                convert = true;
            }
            i++;
        }
    }
    
    if(convert) {
        /* now if we need to convert anything we neeg to create a new iovec
         * to recieve into */
        orig = malloc(sizeof(struct iovec) * count);
        /* copy their array into ours */
        memcpy(orig, msg, sizeof(struct iovec) * count);
        /* now we need to go through the iovects, and any ints we need to
         * allocate our own space to recieve into, so we can convert into
         * their space later */
        for(i = 0; i < count; i++) {
            if((types[i] == MCA_OOB_BASE_INT16) || (types[i] == MCA_OOB_BASE_INT32)) {
                orig[i].iov_base = malloc(orig[i].iov_len);
            }
        }
        /* now the new buffers are ready. do the recieve */
        rc = mca_oob.oob_recv(peer, orig, count, tag, flags);
        /* now we have to do the conversions */
        for(i = 0; i < count; i++) {
            if(types[i] == MCA_OOB_BASE_INT16) {
                /* figure out how many integers we have */
                num = orig[i].iov_len / 2;
                /* unpack the data */
                mca_oob_base_unpack(msg[i].iov_base, orig[i].iov_base, num, MCA_OOB_BASE_INT16);
                /* free the old buffer */
                free(orig[i].iov_base);
            } else if(types[i] == MCA_OOB_BASE_INT32) {
                /* figure out how many integers we have */
                num = orig[i].iov_len / 4;
                /* unpack the data */
                mca_oob_base_unpack(msg[i].iov_base, orig[i].iov_base, num, MCA_OOB_BASE_INT32);
                /* free the old buffer */
                free(orig[i].iov_base);
            }
        }
        /* free the iovecs we allocated */
        free(orig);
    } else {
        rc = mca_oob.oob_recv(peer, msg, count, tag, flags);
    }
    return rc;
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
int mca_oob_recv_packed (ompi_process_name_t* peer, ompi_buffer_t *buf, int tag)
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

	rc = ompi_buffer_init (&tmpbuf, insize);
	if (OMPI_ERROR==rc) { return (rc); }
	rc = ompi_buffer_get_ptrs (tmpbuf, &targetptr, NULL, NULL);
	if (OMPI_ERROR==rc) { return (rc); }

	/* now update the second IOV */
	msg[0].iov_base = (char*) targetptr;
	msg[0].iov_len  = insize;

        rc = mca_oob.oob_recv(peer, msg, 1, tag, 0);

	if (OMPI_ERROR!=rc) *buf = tmpbuf;

	return (rc);
}
