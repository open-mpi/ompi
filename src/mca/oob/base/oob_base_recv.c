#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include <netinet/in.h>
#include <string.h>

/*
* Similiar to unix recv(2)
*
* @param peer (IN)    Opaque name of peer process or OOB_NAME_ANY for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
* @param count (IN)   Number of elements in iovec array.
* @param flags (IN)   May be OOB_PEEK to return up to the number of bytes provided in the
*                     iovec array without removing the message from the queue.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/
                                                                                                         
int mca_oob_recv(ompi_process_name_t* peer, const struct iovec *msg, int count, int flags)
{
    return(mca_oob.oob_recv(peer, msg, count, flags));
}

/*
 * Receive data and convert (if required) to host byte order.
 *
 * @param peer (IN)    Opaque name of peer process or OOB_NAME_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be OOB_PEEK to return up to the number of bytes provided in the
 *                     iovec array without removing the message from the queue.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_recv_ntoh(ompi_process_name_t* peer, const struct iovec *msg,
                      const mca_oob_base_type_t *types, int count, int flags)
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
        rc = mca_oob.oob_recv(peer, orig, count, flags);
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
        rc = mca_oob.oob_recv(peer, msg, count, flags);
    }
    return rc;
}

