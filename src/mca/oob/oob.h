/*
 * $HEADER$
 */
/** @file:
 *
 * the oob framework
 */

#ifndef _MCA_OOB_H_
#define _MCA_OOB_H_
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/ns.h"
#include <sys/uio.h>


/*
 * Well known address
 */

extern ompi_process_name_t mca_oob_name_any;
extern ompi_process_name_t mca_oob_name_seed;
extern ompi_process_name_t mca_oob_name_self;

#define MCA_OOB_NAME_ANY  &mca_oob_name_any
#define MCA_OOB_NAME_SEED &mca_oob_name_seed
#define MCA_OOB_NAME_SELF &mca_oob_name_self

/*
 * Other constants
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
*   int size = mca_oob_recv(name, 0, 0, MSG_OOB_PEEK|MSG_OOB_TRUNC);
*/

#define MCA_OOB_PEEK  0x01   /* flag to oob_recv to allow caller to peek a portion of the next available
                              * message w/out removing the message from the queue.  */
#define MCA_OOB_TRUNC 0x02   /* flag to oob_recv to return the actual size of the message even if the receive
                                buffer is smaller than the number of bytes available */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
        
/**
*  Similiar to unix writev(2).
*
* @param peer (IN)    Opaque name of peer process.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   Currently unused.
* @return             OMPI error code (<0) on error number of bytes actually sent.
*/

int mca_oob_send(
    const ompi_process_name_t* peer, 
    const struct iovec *msg, 
    int count, 
    int tag,
    int flags);

/**
* Convert data (if required) to network byte order prior to sending to peer.
*
* @param peer (IN)    Opaque name of peer process.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   Currently unused.
* @return             OMPI error code (<0) on error number of bytes actually sent.
*/

int mca_oob_send_hton(
    const ompi_process_name_t* peer, 
    const struct iovec *msg, 
    const mca_oob_base_type_t *types, 
    int count, 
    int tag,
    int flags);


/**
* Similiar to unix readv(2)
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
*                     iovec array without removing the message from the queue.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

int mca_oob_recv(
    ompi_process_name_t* peer, 
    const struct iovec *msg, 
    int count, 
    int tag, 
    int flags);

/**
* Receive data and convert (if required) to host byte order.
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
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
   int flags);

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
    const ompi_process_name_t* peer, 
    const struct iovec* msg, 
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
*/

int mca_oob_send_nb(
    const ompi_process_name_t* peer, 
    const struct iovec* msg, 
    int count, 
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/**
*  Non-blocking version of mca_oob_send_hton().
*
*  @param peer (IN)    Opaque name of peer process.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param flags (IN)   Currently unused.
*  @param cbfunc (IN)  Callback function on send completion.
*  @param cbdata (IN)  User data that is passed to callback function.
*  @return             OMPI error code (<0) on error number of bytes actually sent.
*
*/

int mca_oob_send_hton_nb(
    const ompi_process_name_t* peer, 
    const struct iovec* msg, 
    const mca_oob_base_type_t* types,
    int count, 
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/**
* Non-blocking version of mca_oob_recv().
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)  Callback function on recv completion.
* @param cbdata (IN)  User data that is passed to callback function.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

int mca_oob_recv_nb(
    ompi_process_name_t* peer, 
    const struct iovec* msg,  
    int count, 
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/**
* Non-blocking version of mca_oob_recv_ntoh().
*
* @param peer (IN/OUT) Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
* @param msg (IN)      Array of iovecs describing user buffers and lengths.
* @param types (IN)    Parallel array to iovecs describing data type of each iovec element.
* @param count (IN)    Number of elements in iovec array.
* @param tag (IN)      User defined tag for matching send/recv.
* @param flags (IN)    May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)   Callback function on recv completion.
* @param cbdata (IN)   User data that is passed to callback function.
* @return              OMPI error code (<0) on error or number of bytes actually received.
*/

int mca_oob_recv_ntoh_nb(
    ompi_process_name_t* peer, 
    const struct iovec* msg,  
    const mca_oob_base_type_t* types,
    int count, 
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
/*
 * OOB Component/Module function prototypes.
 */

/**
*  Implementation of mca_oob_send().
*
*  @param peer (IN)   Opaque name of peer process.
*  @param msg (IN)    Array of iovecs describing user buffers and lengths.
*  @param count (IN)  Number of elements in iovec array.
*  @param tag (IN)    User defined tag for matching send/recv.
*  @param flags (IN)  Currently unused.
*  @return            OMPI error code (<0) on error number of bytes actually sent.
*/

typedef int (*mca_oob_base_module_send_fn_t)(
    const ompi_process_name_t* peer, 
    const struct iovec *msg, 
    int count, 
    int tag,
    int flags);


/**
*  Implementation of mca_oob_recv().
*
*  @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
*                      iovec array without removing the message from the queue.
*  @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*mca_oob_base_module_recv_fn_t)(
    ompi_process_name_t* peer, 
    const struct iovec *msg, 
    int count, 
    int tag,
    int flags);

/**
*  Implementation of mca_oob_send_nb().
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
*/

typedef int (*mca_oob_base_module_send_nb_fn_t)(
    const ompi_process_name_t* peer, 
    const struct iovec* msg, 
    int count, 
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/**
* Implementation of mca_oob_recv_nb().
*
* @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)  Callback function on recv completion.
* @param cbdata (IN)  User data that is passed to callback function.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*mca_oob_base_module_recv_nb_fn_t)(
    ompi_process_name_t* peer, 
    const struct iovec* msg,  
    int count, 
    int tag,
    int flags, 
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/*
 * OOB Module
 */

struct mca_oob_1_0_0_t {
    mca_oob_base_module_send_fn_t     oob_send; 
    mca_oob_base_module_recv_fn_t     oob_recv;
    mca_oob_base_module_send_nb_fn_t  oob_send_nb;
    mca_oob_base_module_recv_nb_fn_t  oob_recv_nb;
};
typedef struct mca_oob_1_0_0_t mca_oob_1_0_0_t;
typedef struct mca_oob_1_0_0_t mca_oob_t;

/**
 * OOB Component
 */

typedef mca_oob_t* (*mca_oob_base_component_init_fn_t)(
    bool *allow_multi_user_threads,
    bool *have_hidden_threads);

typedef int (*mca_oob_base_component_finalize_fn_t)(void);

/**
 * the standard component data structure
 */
struct mca_oob_base_component_1_0_0_t {
   mca_base_component_t oob_version;
   mca_base_component_data_1_0_0_t oob_data;

   mca_oob_base_component_init_fn_t oob_init;
   mca_oob_base_component_finalize_fn_t oob_finalize;
};
typedef struct mca_oob_base_component_1_0_0_t mca_oob_base_component_1_0_0_t;
typedef mca_oob_base_component_1_0_0_t mca_oob_base_component_t;


/*
 * Macro for use in components that are of type oob v1.0.0
 */
#define MCA_OOB_BASE_VERSION_1_0_0 \
  /* pml v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* pml v1.0 */ \
  "oob", 1, 0, 0

#endif

