/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * Contains the internal functions and typedefs for the use of the rml
 */

#ifndef ORTE_RML_H_
#define ORTE_RML_H_

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rml/rml_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * RML Module function prototypes.
 */

/**
*  ompi_rml.rml_get_uri()
*
*  Note that orte_rml_base_init() must be called to load and select
*  an RML module prior to calling this routine.
*/

typedef char* (*orte_rml_module_get_uri_fn_t)(void);

/**
*  ompi_rml.rml_set_uri()
*
*  Pre-populate the cache of contact information required by the RTL
*  to reach a given destination. This is required to setup a pointer
*  to initial registry/name server/etc.
*
*  @param  uri   The contact information of the peer process obtained
*  via a call to orte_ml.rml_get_uri().
*
*/

typedef int (*orte_rml_module_set_uri_fn_t)(const char*);

/**
*  orte_rml.rml_parse_uris()
*
*  Extract from the contact info the peer process name and uri.
*
*  @param  uri (IN)   The contact information of the peer process.
*  @param  name (OUT)   The peer process identifier - MUST BE PREVIOUSLY ALLOCATED.
*  @param  uris (OUT)   Will return an array of uri strings corresponding
*                       to the peers exported protocols.
*
*  Note the caller may pass NULL for the uris if they only wish to extact
*  the process name.
*/

typedef int (*orte_rml_module_parse_uris_fn_t)(const char* uri,
                                    orte_process_name_t* peer, char*** uris);

/**
*  Implementation of orte_rml_ping().
*
*  @param peer (IN)   Opaque name of peer process.
*  @param tv (IN)     Timeout to wait in connection response.
*  @return            OMPI error code (<0) or ORTE_SUCCESS
*/

typedef int (*orte_rml_module_ping_fn_t)(const char* uri, const struct timeval* tv);

/**
*  orte_rml.rml_send()
*
*  @param peer (IN)   Opaque name of peer process.
*  @param msg (IN)    Array of iovecs describing user buffers and lengths.
*  @param count (IN)  Number of elements in iovec array.
*  @param tag (IN)    User defined tag for matching send/recv.
*  @param flags (IN)  Currently unused.
*  @return            OMPI error code (<0) on error number of bytes actually sent.
*/

typedef int (*orte_rml_module_send_fn_t)(
    orte_process_name_t* peer,
    struct iovec *msg,
    int count,
    int tag,
    int flags);

/*
* orte_rml.rml_send_buffer()
*
* @param peer (IN)   Opaque name of peer process.
* @param buffer (IN) Prepacked orte_buffer_t containing data to send
* @param flags (IN)  Currently unused.
* @return            OMPI error code (<0) on error or number of bytes actually sent.
*/

typedef int (*orte_rml_module_send_buffer_fn_t)(
    orte_process_name_t* peer,
    orte_buffer_t* buffer,
    orte_rml_tag_t tag,
    int flags);

/**
*  orte_rml.rml_recv().
*
*  @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param flags (IN)   May be ORTE_RML_PEEK to return up to the number of bytes provided in the
*                      iovec array without removing the message from the queue.
*  @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*orte_rml_module_recv_fn_t)(
    orte_process_name_t* peer,
    struct iovec *msg,
    int count,
    orte_rml_tag_t tag,
    int flags);

/**
* orte_rml.rml_recv_buffer()
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param buf (OUT)    Buffer to receive into.
* @param tag (IN      User defined tag for matching send/recv.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*
* This version of oob_recv is as above except it does NOT take a iovec list
* but instead hands back a orte_buffer_t buffer with the message in it.
* The user is responsible for freeing this buffer with orte_buffer_free()
* when finished.
*
*/

typedef int (*orte_rml_module_recv_buffer_fn_t) (
    orte_process_name_t* peer,
    orte_buffer_t *buf,
    orte_rml_tag_t tag);

/**
*  Callback function on send/recv completion for iovec messages only,
*  orte_rml.rml_send_nb() and orte_rml.rml_recv_nb() use this.
*
*  @param status (IN)  Completion status - equivalent to the return value from blocking send/recv.
*  @param peer (IN)    Opaque name of peer process.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param cbdata (IN)  User data.
*/

typedef void (*orte_rml_callback_fn_t)(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    void* cbdata);

/**
*  Callback function on send/recv completion for buffer PACKED message only,
*  orte_rml.rml_send_buffer_nb() and orte_rml.rml_recv_buffer_nb() use this.
*
*  @param status (IN)  Completion status - equivalent to the return value from blocking send/recv.
*  @param peer (IN)    Opaque name of peer process.
*  @param buffer (IN)  For sends, this is a pointer to a prepacked buffer
                       For recvs, OOB creates and returns a buffer
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param cbdata (IN)  User data.
*/

typedef void (*orte_rml_buffer_callback_fn_t)(
    int status,
    orte_process_name_t* peer,
    orte_buffer_t* buffer,
    orte_rml_tag_t tag,
    void* cbdata);

/**
*  orte_rml.rml_send_nb()
*
*  Non-blocking version or orte_rml.rml_send().
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

typedef int (*orte_rml_module_send_nb_fn_t)(
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    int flags,
    orte_rml_callback_fn_t cbfunc,
    void* cbdata);

/**
*  orte_rml.rml_send_buffer_nb()
*
*  Non-blocking version of orte_rml.rml_send_buffer().
*
*  @param peer (IN)    Opaque name of peer process.
*  @param buffer (IN)  Opaque buffer handle.
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

typedef int (*orte_rml_module_send_buffer_nb_fn_t)(
    orte_process_name_t* peer,
    orte_buffer_t* buffer,
    orte_rml_tag_t tag,
    int flags,
    orte_rml_buffer_callback_fn_t cbfunc,
    void* cbdata);


/**
* orte_rml.rml_recv_nb()
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be ORTE_RML_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)  Callback function on recv completion.
* @param cbdata (IN)  User data that is passed to callback function.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*orte_rml_module_recv_nb_fn_t)(
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    int flags,
    orte_rml_callback_fn_t cbfunc,
    void* cbdata);

/**
* orte_rml.rml_recv_buffer_nb()
*
* Non-blocking version of orte_rml.rml_recv_buffer().
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param buffer (IN)  Array of iovecs describing user buffers and lengths.
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

typedef int (*orte_rml_module_recv_buffer_nb_fn_t)(
    orte_process_name_t* peer,
    orte_rml_tag_t tag,
    int flags,
    orte_rml_buffer_callback_fn_t cbfunc,
    void* cbdata);


/**
* orte_rml.rml_recv_cancel()
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param tag (IN)     User defined tag for matching send/recv.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*orte_rml_module_recv_cancel_fn_t)(orte_process_name_t* peer, orte_rml_tag_t tag);

/**
 * Collective Operations.
 */

/**
 * xcast function for sending common messages to all processes
 */

typedef int (*orte_rml_module_xcast_fn_t)(orte_jobid_t job,
                                          bool process_first,
                                          orte_buffer_t* buffer,
                                          orte_gpr_trigger_cb_fn_t cbfunc);

/*
 * Callback on exception condition.
 */

typedef enum {
    ORTE_RML_PEER_UNREACH,
    ORTE_RML_PEER_DISCONNECTED
} orte_rml_exception_t;

typedef void (*orte_rml_exception_callback_t)(
    const orte_process_name_t* peer,
    orte_rml_exception_t exception);

/**
 *  Register a callback function on loss of a connection.
 */

typedef int (*orte_rml_module_exception_fn_t)(
    orte_rml_exception_callback_t cbfunc);

/*
 * Initialization/Cleanup
 */

/**
 * Called after the module is selected and the
 * rest of the rte is initialized.
 */
typedef int (*orte_rml_module_init_fn_t)(void);

/**
 * Called to cleanup the selected modules state.
 */
typedef int (*orte_rml_module_fini_fn_t)(void);

/**
 * RML Module
 */
struct orte_rml_module_t {
    orte_rml_module_init_fn_t            init;
    orte_rml_module_fini_fn_t            fini;
    orte_rml_module_get_uri_fn_t         get_uri;
    orte_rml_module_set_uri_fn_t         set_uri;
    orte_rml_module_parse_uris_fn_t      parse_uris;
    orte_rml_module_ping_fn_t            ping;
    orte_rml_module_send_fn_t            send;
    orte_rml_module_send_nb_fn_t         send_nb;
    orte_rml_module_send_buffer_fn_t     send_buffer;
    orte_rml_module_send_buffer_nb_fn_t  send_buffer_nb;
    orte_rml_module_recv_fn_t            recv;
    orte_rml_module_recv_nb_fn_t         recv_nb;
    orte_rml_module_recv_buffer_fn_t     recv_buffer;
    orte_rml_module_recv_buffer_nb_fn_t  recv_buffer_nb;
    orte_rml_module_recv_cancel_fn_t     recv_cancel;
    orte_rml_module_xcast_fn_t           xcast;
    orte_rml_module_exception_fn_t       add_exception_handler;
    orte_rml_module_exception_fn_t       del_exception_handler;
};
typedef struct orte_rml_module_t orte_rml_module_t;


/**
 * RML Component
 */
typedef orte_rml_module_t* (*orte_rml_component_init_fn_t)(int  *priority);

/**
 * the standard component data structure
 */
struct orte_rml_component_1_0_0_t {
   mca_base_component_t rml_version;
   mca_base_component_data_1_0_0_t rml_data;
   orte_rml_component_init_fn_t rml_init;
};
typedef struct orte_rml_component_1_0_0_t orte_rml_component_t;


/**
 * Macro for use in components that are of type rml v1.0.0
 */
#define ORTE_RML_BASE_VERSION_1_0_0 \
  /* rml v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* rml v1.0 */ \
  "rml", 1, 0, 0


/*
 * This is the RML instance that all functions are called through.
 */

ORTE_DECLSPEC extern orte_rml_module_t orte_rml;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
