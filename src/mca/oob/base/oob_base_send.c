#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include <string.h>
#include <netinet/in.h>
/*
*  Similiar to unix send(2).
*
* @param peer (IN)   Opaque name of peer process.
* @param msg (IN)    Array of iovecs describing user buffers and lengths.
* @param count (IN)  Number of elements in iovec array.
* @param flags (IN)  Currently unused.
* @return            OMPI error code (<0) on error or number of bytes actually sent.
*/
                                                                                                         
int mca_oob_send(const ompi_process_name_t* peer, const struct iovec *msg, int count, int tag, int flags)
{
    return(mca_oob.oob_send(peer, msg, count, tag, flags));
}

/*
*  Similiar to unix send(2) and mca_oob_send.
*
* @param peer (IN)   Opaque name of peer process.
* @param buffer (IN) Prepacked OMPI_BUFFER containing data to send
* @param flags (IN)  Currently unused.
* @return            OMPI error code (<0) on error or number of bytes actually sent.
*/
                                                                                                         
int mca_oob_send_packed (const ompi_process_name_t* peer, const ompi_buffer_t buffer, int tag, int flags)
{
void *dataptr;
size_t datalen;
struct iovec msg[1];
int rc;

    /* first build iovec from buffer information */
    rc = ompi_buffer_size (buffer, &datalen);
    if (OMPI_ERROR==rc) { return (rc); }

    rc = ompi_buffer_get_ptrs (buffer, NULL, NULL, &dataptr);
    if (OMPI_ERROR==rc) { return (rc); }

    msg[0].iov_base = dataptr;
    msg[0].iov_len  = datalen;

    return(mca_oob.oob_send(peer, msg, 1, tag, flags));
}
 
/*
 * Convert data (if required) to network byte order prior to sending to peer.
 *
 * @param peer (IN)   Opaque name of peer process.
 * @param msg (IN)    Array of iovecs describing user buffers and lengths.
 * @param types (IN)  Parallel array to iovecs describing data type of each iovec element.
 * @param count (IN)  Number of elements in iovec array.
 * @param flags (IN)  Currently unused.
 * @return            OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_send_hton(const ompi_process_name_t* peer, const struct iovec *msg,
                      const mca_oob_base_type_t *types, int count, int tag, int flags)
{
    int rc, i = 0;
    struct iovec * converted;
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
        converted = malloc(sizeof(struct iovec) * count);
        memcpy(converted, msg, sizeof(struct iovec) * count);
        for(i = 0; i < count; i++) {
            if(types[i] == MCA_OOB_BASE_INT16) {
                /* figure out how many integers we have */
                rc = msg[i].iov_len / 2;
                /* allocate a buffer for the converted data */
                converted[i].iov_base = malloc(msg[i].iov_len);
                /* pack the data */
                mca_oob_base_pack(converted[i].iov_base, msg[i].iov_base, rc, MCA_OOB_BASE_INT16);
            } else if(types[i] == MCA_OOB_BASE_INT32) {
                /* figure out how many integers we have */
                rc = msg[i].iov_len / 4;
                /* allocate a buffer for the converted data */
                converted[i].iov_base = malloc(msg[i].iov_len);
                /* pack the data */
                mca_oob_base_pack(converted[i].iov_base, msg[i].iov_base, rc, MCA_OOB_BASE_INT32);
            }
        }
        rc = mca_oob.oob_send(peer, converted, count, tag, flags);
        /* clean up any space we allocated */
        for(i = 0; i < count; i++) {
            if((types[i] == MCA_OOB_BASE_INT16) || (types[i] == MCA_OOB_BASE_INT32)) {
                free(converted[i].iov_base);
            }
        }
        free(converted);
    } else {
        rc = mca_oob.oob_send(peer, msg, count, tag, flags);
    }
    return rc;
        
}

