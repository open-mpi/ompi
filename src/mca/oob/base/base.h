/*
 * $HEADER$
 */
/** @file:
 *
 * the oob framework
 */

#ifndef _MCA_OOB_BASE_H_
#define _MCA_OOB_BASE_H_
#include "mca/mca.h"
#include "mca/ns/ns.h"
#include <sys/uio.h>
#include "util/pack.h"


/*
 * Well known address
 */

extern ompi_process_name_t mca_oob_name_any;
extern ompi_process_name_t mca_oob_name_seed;
extern ompi_process_name_t mca_oob_name_self;

/**
 * The wildcard for recieves from any peer.
 */
#define MCA_OOB_NAME_ANY  &mca_oob_name_any
/**
 * The process name of the seed deamon
 */
#define MCA_OOB_NAME_SEED &mca_oob_name_seed
/**
 * Process name of self
 */
#define MCA_OOB_NAME_SELF &mca_oob_name_self

/*
 * Other constants
 */
/**
 * Recieve from any tag.
 */
#define MCA_OOB_TAG_ANY 0

/*
 * OOB API 
 */

/**
 * Supported datatypes for conversion operations.
 */
typedef enum {
    MCA_OOB_BASE_BYTE, /**< a byte of data */
    MCA_OOB_BASE_INT16, /**< a 16 bit integer */
    MCA_OOB_BASE_INT32, /**< a 32 bit integer */
    MCA_OOB_BASE_STRING, /**< a NULL terminated string */
    MCA_OOB_BASE_PACKED /**< already packed data. */
} mca_oob_base_type_t;

/**
*   General flags for send/recv
* 
*   An example of usage - to determine the size of the next available message w/out receiving it:
*
*   int size = mca_oob_recv(name, 0, 0, MCA_OOB_TRUNC|MCA_OOB_PEEK);
*/

#define MCA_OOB_PEEK  0x01   /**< flag to oob_recv to allow caller to peek a portion of the next available
                              * message w/out removing the message from the queue.  */
#define MCA_OOB_TRUNC 0x02   /**< flag to oob_recv to return the actual size of the message even if 
                              * the receive buffer is smaller than the number of bytes available */
#if 0 /* NOT YET IMPLEMENTED */
#define MCA_OOB_ALLOC 0x04   /**< flag to oob_recv to request the oob to allocate a buffer of the appropriate
                              * size for the receive and return the allocated buffer and size in the first
                              * element of the iovec array. */
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
        
/**
*  Returns a null terminated character string returning contact info
*  for all supported OOB channels.
*
*  @return  A null terminated string.
*
*  The caller is responsible for freeing the returned string.
*/

char* mca_oob_get_contact_info(void);

/**
*  Similiar to unix writev(2).
*
*  @param peer (IN)    Opaque name of peer process.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param flags (IN)   Currently unused.
*  @return             OMPI error code (<0) on error number of bytes actually sent.
*
*  This routine provides semantics similar to unix send/writev with the addition of
*  a tag parameter that can be used by the application to match the send w/ a specific
*  receive. In other words - a recv call by the specified peer will only succeed when
*  the corresponding (or wildcard) tag is used.
*
*  The <i>peer</i> parameter represents an opaque handle to the peer process that
*  is resolved by the oob layer (using the registry) to an actual physical network
*  address.
*/

int mca_oob_send(
    ompi_process_name_t* peer, 
    struct iovec *msg, 
    int count, 
    int tag,
    int flags);

/*
*  Similiar to unix send(2) and mca_oob_send.
*
* @param peer (IN)   Opaque name of peer process.
* @param buffer (IN) Prepacked OMPI_BUFFER containing data to send
* @param flags (IN)  Currently unused.
* @return            OMPI error code (<0) on error or number of bytes actually sent.
*/

int mca_oob_send_packed(
    ompi_process_name_t* peer, 
    ompi_buffer_t buffer, 
    int tag, 
    int flags);


/**
* Similiar to unix readv(2)
*
* @param peer (IN/OUT)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive. In the
*                         case of a wildcard receive, will be modified to return the matched peer name.
* @param msg (IN)         Array of iovecs describing user buffers and lengths.
* @param count (IN)       Number of elements in iovec array.
* @param tag (IN/OUT)     User defined tag for matching send/recv. In the case of a wildcard receive, will
*                         be modified to return the matched tag. May be optionally by NULL to specify a 
*                         wildcard receive with no return value.
* @param flags (IN)       May be MCA_OOB_PEEK to return up to the number of bytes provided in the
*                         iovec array without removing the message from the queue.
* @return                 OMPI error code (<0) on error or number of bytes actually received.
*
* The OOB recv call is similar to unix recv/readv in that it requires the caller to manage
* memory associated w/ the message. The routine accepts an array of iovecs (<i>msg</i>); however,
* the caller must determine the appropriate number of elements (<i>count</i>) and allocate the
* buffer space for each entry. 
*
* The <i>tag</i> parameter is provided to facilitate this. The user may define tags based on message 
* type to determine the message layout and size, as the mca_oob_recv call will block until a message
* with the matching tag is received.
*
* Alternately, the <i>flags</i> parameter may be used to peek (MCA_OOB_PEEK) a portion of the message 
* (e.g. a standard message header) or determine the overall message size (MCA_OOB_TRUNC|MCA_OOB_PEEK)
* without removing the message from the queue. 
*
*/

int mca_oob_recv(
    ompi_process_name_t* peer, 
    struct iovec *msg, 
    int count, 
    int* tag, 
    int flags);

/**
* Similiar to unix read(2)
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
* @param buf (OUT)    Array of iovecs describing user buffers and lengths.
* @param tag (IN/OUT) User defined tag for matching send/recv.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*
*
* This version of oob_recv is as above except it does NOT take a iovec list
* but instead hands back a ompi_buffer_t buffer with the message in it. 
* The user is responsible for freeing this buffer with ompi_buffer_free()
* when finished.
*
*
*/

int mca_oob_recv_packed (
	ompi_process_name_t* peer, 
	ompi_buffer_t *buf, 
	int* tag);

/*
 * Non-blocking versions of send/recv.
*/


/**
*  Callback function on send/recv completion.
*
*  @param status (IN)  Completion status - equivalent to the return value from blocking send/recv.
*  @param peer (IN)    Opaque name of peer process.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param cbdata (IN)  User data.
*/

typedef void (*mca_oob_callback_fn_t)(
    int status,
    ompi_process_name_t* peer, 
    struct iovec* msg, 
    int count,
    int tag,
    void* cbdata);

/**
*  Callback function on send/recv completion for buffer PACKED message only.
*  i.e. only mca_oob_send_packed_nb and mca_oob_recv_packed_nb USE this.
*
*  @param status (IN)  Completion status - equivalent to the return value from blocking send/recv.
*  @param peer (IN)    Opaque name of peer process.
*  @param buffer (IN)  For sends, this is a pointer to a prepacked buffer
                       For recvs, OOB creates and returns a buffer 
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param cbdata (IN)  User data.
*/

typedef void (*mca_oob_callback_packed_fn_t)(
    int status,
    ompi_process_name_t* peer, 
    ompi_buffer_t* buffer,
    int count,
    int tag,
    void* cbdata);

/**
*  Non-blocking version of mca_oob_send().
*
*  @param peer (IN)    Opaque name of peer process.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param flags (IN)   Currently unused.
*  @param cbfunc (IN)  Callback function on send completion.
*  @param cbdata (IN)  User data that is passed to callback function.
*  @return             OMPI error code (<0) on error number of bytes actually sent.
*
*  The user supplied callback function is called when the send completes. Note that
*  the callback may occur before the call to mca_oob_send returns to the caller, 
*  if the send completes during the call.
*
*/

int mca_oob_send_nb(
    ompi_process_name_t* peer, 
    struct iovec* msg, 
    int count, 
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/**
* Non-blocking version of mca_oob_recv().
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)  Callback function on recv completion.
* @param cbdata (IN)  User data that is passed to callback function.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*
* The user supplied callback function is called asynchronously when a message is received 
* that matches the call parameters.
*/

int mca_oob_recv_nb(
    ompi_process_name_t* peer, 
    struct iovec* msg,  
    int count, 
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/*
 * functions for pack and unpack routines
 */
/**
 * This function packs the passed data according to the type enum.
 *
 * @param dest the destination for the packed data
 * @param src the source of the data
 * @param n the number of elements in the src
 * @param type the type of data 
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 */

int mca_oob_base_pack(void * dest, void * src, size_t n, mca_oob_base_type_t type);

/**
 * This function unpacks the passed data according to the type enum.
 *
 * @param dest the destination for the unpacked data
 * @param src the source of the packed data
 * @param n the number of elements in the src
 * @param type the type of the data to unpack
 * 
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 */

int mca_oob_base_unpack(void * dest, void * src, size_t n, mca_oob_base_type_t type);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

