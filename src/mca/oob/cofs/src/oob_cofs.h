/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#ifndef OOB_COFS_H
#define OOB_COFS_H
#include "ompi_config.h"

#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "include/types.h"

/*
 * Module open / close
 */
int mca_oob_cofs_open(void);
int mca_oob_cofs_close(void);


/*
 * Startup / Shutdown
 */
mca_oob_t* mca_oob_cofs_init(bool *allow_multi_user_threads, bool *have_hidden_threads);
int mca_oob_cofs_finalize(mca_oob_t*);


                                                                                                          
/**
*  Implementation of mca_oob_send().
*
*  @param peer (IN)   Opaque name of peer process.
*  @param msg (IN)    Array of iovecs describing user buffers and lengths.
*  @param count (IN)  Number of elements in iovec array.
*  @param flags (IN)  Currently unused.
*  @return            OMPI error code (<0) on error number of bytes actually sent.
*/

int mca_oob_cofs_send(
    const ompi_process_name_t*,
    const struct iovec* msg,
    int count,
    int tag,
    int flags);


/**
*  Implementation of mca_oob_recv().
*
*  @param peer (IN)    Opaque name of peer process or OOB_NAME_ANY for wildcard receive.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
*  @param count (IN)   Number of elements in iovec array.
*  @param flags (IN)   May be OOB_PEEK to return up to the number of bytes provided in the
*                      iovec array without removing the message from the queue.
*  @return             OMPI error code (<0) on error or number of bytes actually received.
*/
                                                                                                                                   
int mca_oob_cofs_recv(
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
*  @param flags (IN)   Currently unused.
*  @param cbfunc (IN)  Callback function on send completion.
*  @param cbdata (IN)  User data that is passed to callback function.
*  @return             OMPI error code (<0) on error number of bytes actually sent.
*
*/
                                                                                                                                   
int mca_oob_cofs_send_nb(
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
* @param peer (IN)    Opaque name of peer process or OOB_NAME_ANY for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param flags (IN)   May be OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)  Callback function on recv completion.
* @param cbdata (IN)  User data that is passed to callback function.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/
                                                                                                                                   
int mca_oob_cofs_recv_nb(
    ompi_process_name_t* peer,
    const struct iovec* msg,
    int count,
    int tag,
    int flags,
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);
                                                                                                                                   
extern char mca_oob_cofs_comm_loc[OMPI_PATH_MAX]; /* location for file drop-off */
extern uint64_t mca_oob_cofs_serial;

#endif

