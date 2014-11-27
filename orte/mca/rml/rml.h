/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** 
 * @file
 *
 * Runtime Messaging Layer (RML) Communication Interface
 *
 * The Runtime Messaging Layer (RML) provices basic point-to-point
 * communication between ORTE processes.  The system is available for
 * most architectures, with some exceptions (the Cray XT3/XT4, for example).
 */


#ifndef ORTE_MCA_RML_RML_H_
#define ORTE_MCA_RML_RML_H_

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/mca/rml/rml_types.h"

BEGIN_C_DECLS


/* ******************************************************************** */


struct opal_buffer_t;
struct orte_rml_module_t;
typedef struct {
    opal_object_t super;
    orte_process_name_t name;
    opal_buffer_t data;
    bool active;
} orte_rml_recv_cb_t;
OBJ_CLASS_DECLARATION(orte_rml_recv_cb_t);

/* Provide a generic callback function to release buffers
 * following a non-blocking send as this happens all over
 * the code base
 */
ORTE_DECLSPEC void orte_rml_send_callback(int status, orte_process_name_t* sender,
                                          opal_buffer_t* buffer, orte_rml_tag_t tag,
                                          void* cbdata);

ORTE_DECLSPEC void orte_rml_recv_callback(int status, orte_process_name_t* sender,
                                          opal_buffer_t *buffer,
                                          orte_rml_tag_t tag, void *cbdata);

/* ******************************************************************** */


/**
 * RML component initialization 
 *
 * Create an instance (module) of the given RML component.  Upon
 * returning, the module data structure should be fully populated and
 * all functions should be usable.  Non-blocking receive calls may be
 * posted upon return from this function, although communication need
 * not be enabled until enable_comm() call is called on the module.
 *
 * @return Exactly one module created by the call to the component's
 * initialization function should be returned.  The module structure
 * should be fully populated, and the priority should be set to a
 * reasonable value.
 *
 * @param[out] priority Selection priority for the given component
 *
 * @retval NULL An error occurred and initialization did not occur
 * @retval non-NULL The module was successfully initialized
 */
typedef struct orte_rml_module_t* (*orte_rml_component_init_fn_t)(int  *priority);


/**
 * RML component interface
 *
 * Component interface for the RML framework.  A public instance of
 * this structure, called mca_rml_[component name]_component, must
 * exist in any RML component.
 */
struct orte_rml_component_2_0_0_t {
    /* Base component description */
    mca_base_component_t rml_version;
    /* Base component data block */
    mca_base_component_data_t rml_data;
    /* Component intialization function */
    orte_rml_component_init_fn_t rml_init;
};
/** Convienence typedef */
typedef struct orte_rml_component_2_0_0_t orte_rml_component_t;


/* ******************************************************************** */


/**
 * Funtion prototype for callback from non-blocking iovec send and recv
 *
 * Funtion prototype for callback from non-blocking iovec send and recv.
 * On send, the iovec pointer will be the same pointer passed to
 * send_nb and count will equal the count given to send.
 *
 * On recv, the iovec pointer will be the address of a single iovec
 * allocated and owned by the RML, not the process receiving the
 * callback. Ownership of the data block can be transferred by setting
 * a user variable to point to the data block, and setting the
 * iovec->iov_base pointer to NULL.
 *
 * @note The parameter in/out parameters are relative to the user's callback
 * function.
 *
 * @param[in] status  Completion status
 * @param[in] peer    Opaque name of peer process
 * @param[in] msg     Pointer to the array of iovec that was sent
 *                    or to a single iovec that has been recvd
 * @param[in] count   Number of iovecs in the array
 * @param[in] tag     User defined tag for matching send/recv
 * @param[in] cbdata  User data passed to send_nb()
 */
typedef void (*orte_rml_callback_fn_t)(int status,
                                       orte_process_name_t* peer,
                                       struct iovec* msg,
                                       int count,
                                       orte_rml_tag_t tag,
                                       void* cbdata);


/**
 * Funtion prototype for callback from non-blocking buffer send and receive
 *
 * Function prototype for callback from non-blocking buffer send and
 * receive. On send, the buffer will be the same pointer passed to
 * send_buffer_nb. On receive, the buffer will be allocated and owned
 * by the RML, not the process receiving the callback.
 *
 * @note The parameter in/out parameters are relative to the user's callback
 * function.
 *
 * @param[in] status  Completion status
 * @param[in] peer    Name of peer process
 * @param[in] buffer  Message buffer
 * @param[in] tag     User defined tag for matching send/recv
 * @param[in] cbdata  User data passed to send_buffer_nb() or recv_buffer_nb()
 */
typedef void (*orte_rml_buffer_callback_fn_t)(int status,
                                              orte_process_name_t* peer,
                                              struct opal_buffer_t* buffer,
                                              orte_rml_tag_t tag,
                                              void* cbdata);


/**
 * Function prototype for exception callback
 *
 * Function prototype for callback triggered when a communication error is detected.
 *
 * @note The parameter in/out parameters are relative to the user's callback
 * function.
 *
 * @param[in] peer      Name of peer process
 * @param[in] exception Description of the error causing the exception
 */
typedef void (*orte_rml_exception_callback_t)(orte_process_name_t* peer,
                                              orte_rml_exception_t exception);


/* ******************************************************************** */


/**
 * Enable communication using the RML module
 *
 * Enable communication using the RML module.  Before this call, only
 * the non-blocking receive and ping interfaces may be used.  After
 * this call returns, the module must be fully functional, capable of
 * sending and receiving data.  This function will be called after the
 * process has been assigned a proces identifier.
 *
 * @note While the ping interface may be used between the call to the
 * component's initialization function and this call, care must be
 * taken when doing so.  The remote process must have already called
 * enable_comm() or the remote process will not reply to the ping.
 * As the ping interface is generally used by MPI processes to find a
 * daemon to contact, this should not be a major limitation.
 *
 * @retval ORTE_SUCCESS Communications successfully enabled
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*orte_rml_module_enable_comm_fn_t)(void);

/**
 * Finalize the RML module
 *
 * Finalize the RML module, ending all communication and cleaning up
 * all resources associated with the module.  After the finalize
 * function is called, all interface functions (and the module
 * structure itself) are not available for use.
 *
 * @note Whether or not the finalize function returns successfully,
 * the module should not be used once this function is called.
 *
 * @retval ORTE_SUCCESS Success
 * @retval ORTE_ERROR   An unspecified error occurred
 */
typedef int (*orte_rml_module_finalize_fn_t)(void);


/**
 * Get a "contact info" string for the local process
 *
 * Get a "contact info" string that can be used by other processes to
 * share the contact information for the given process.  The "contact
 * info" string includes the process identifier for the given process
 * and uses only basic ascii characters.  It should be quoted when
 * evaluated by a shell, although no special escaping is necessary.
 *
 * @note The function may return a contact info string which contains
 * multiple addresses.
 *
 * @retval non-NULL The contact information for this process
 * @retval NULL     An error occurred when trying to get the current
 *                  process contact info
 */
typedef char* (*orte_rml_module_get_contact_info_fn_t)(void);


/**
 * Update the RML with a remote process's contact info
 *
 * Update the RML with a remote process's contact information, as
 * returned from the get_contact_info() function on the remote
 * process.  Before a send can be initiated to a remote process,
 * either this function must be called for that process or that
 * process must have already established a connection to the local
 * process.
 *
 * @note The user may not always explicitly call this function
 * directly, but may instead cause it to be called through one of the
 * contact setup functions available in
 * orte/mca/rml/base/rml_contact.h.
 *
 * @param[in] contact_info The contact information string of a peer
 */
typedef void (*orte_rml_module_set_contact_info_fn_t)(const char *contact_info);


/**
 * "Ping" another process to determine availability
 *
 * Ping another process to determine if it is available.  This
 * function only verifies that the process is alive and will allow a
 * connection to the local process.  It does *not* qualify as
 * establishing communication with the remote process, as required by
 * the note for set_contact_info().
 *
 * @param[in] contact_info The contact info string for the remote process
 * @param[in] tv           Timeout after which the ping should be failed
 *
 * @retval ORTE_SUCESS The process is available and will allow connections 
 *                     from the local process
 * @retval ORTE_ERROR  An unspecified error occurred during the update
 */
typedef int (*orte_rml_module_ping_fn_t)(const char* contact_info,
                                         const struct timeval* tv);


/**
 * Send an iovec non-blocking message
 *
 * Send an array of iovecs to the specified peer.  The call
 * will return immediately, although the iovecs may not be modified
 * until the completion callback is triggered.  The iovecs *may* be
 * passed to another call to send_nb before the completion callback is
 * triggered.  The callback being triggered does not give any
 * indication of remote completion.
 *
 * @param[in] peer   Name of receiving process
 * @param[in] msg    Pointer to an array of iovecs to be sent
 * @param[in] count  Number of iovecs in array
 * @param[in] tag    User defined tag for matching send/recv
 * @param[in] cbfunc Callback function on message comlpetion
 * @param[in] cbdata User data to provide during completion callback
 *
 * @retval ORTE_SUCCESS The message was successfully started
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_send_nb_fn_t)(orte_process_name_t* peer,
                                            struct iovec* msg,
                                            int count,
                                            orte_rml_tag_t tag,
                                            orte_rml_callback_fn_t cbfunc,
                                            void* cbdata);


/**
 * Send a buffer non-blocking message
 *
 * Send a buffer to the specified peer.  The call
 * will return immediately, although the buffer may not be modified
 * until the completion callback is triggered.  The buffer *may* be
 * passed to another call to send_nb before the completion callback is
 * triggered.  The callback being triggered does not give any
 * indication of remote completion.
 *
 * @param[in] peer   Name of receiving process
 * @param[in] buffer Pointer to buffer to be sent
 * @param[in] tag    User defined tag for matching send/recv
 * @param[in] cbfunc Callback function on message comlpetion
 * @param[in] cbdata User data to provide during completion callback
 *
 * @retval ORTE_SUCCESS The message was successfully started
 * @retval ORTE_ERR_BAD_PARAM One of the parameters was invalid
 * @retval ORTE_ERR_ADDRESSEE_UNKNOWN Contact information for the
 *                    receiving process is not available
 * @retval ORTE_ERROR  An unspecified error occurred
 */
typedef int (*orte_rml_module_send_buffer_nb_fn_t)(orte_process_name_t* peer,
                                                   struct opal_buffer_t* buffer,
                                                   orte_rml_tag_t tag,
                                                   orte_rml_buffer_callback_fn_t cbfunc,
                                                   void* cbdata);

/**
 * Receive an iovec non-blocking message
 *
 * @param[in]  peer    Peer process or ORTE_NAME_WILDCARD for wildcard receive
 * @param[in]  tag     User defined tag for matching send/recv
 * @param[in] persistent Boolean flag indicating whether or not this is a one-time recv
 * @param[in] cbfunc   Callback function on message comlpetion
 * @param[in] cbdata   User data to provide during completion callback
 */
typedef void (*orte_rml_module_recv_nb_fn_t)(orte_process_name_t* peer,
                                             orte_rml_tag_t tag,
                                             bool persistent,
                                             orte_rml_callback_fn_t cbfunc,
                                             void* cbdata);


/**
 * Receive a buffer non-blocking message
 *
 * @param[in]  peer    Peer process or ORTE_NAME_WILDCARD for wildcard receive
 * @param[in]  tag     User defined tag for matching send/recv
 * @param[in] persistent Boolean flag indicating whether or not this is a one-time recv
 * @param[in] cbfunc   Callback function on message comlpetion
 * @param[in] cbdata   User data to provide during completion callback
 */
typedef void (*orte_rml_module_recv_buffer_nb_fn_t)(orte_process_name_t* peer,
                                                    orte_rml_tag_t tag,
                                                    bool persistent,
                                                    orte_rml_buffer_callback_fn_t cbfunc,
                                                    void* cbdata);


/**
 * Cancel a posted non-blocking receive
 *
 * Attempt to cancel a posted non-blocking receive.
 *
 * @param[in] peer    Peer process or ORTE_NAME_WILDCARD, exactly as passed 
 *                    to the non-blocking receive call
 * @param[in] tag     Posted receive tag
 */
typedef void (*orte_rml_module_recv_cancel_fn_t)(orte_process_name_t* peer,
                                                 orte_rml_tag_t tag);


/**
 * Register or deregister an exception callback function
 *
 * Register or deregister a callback when an asynchronous
 * communication exception occurs.
 *
 * @param[in] cbfunc  User callback
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int (*orte_rml_module_exception_fn_t)(orte_rml_exception_callback_t cbfunc);


/**
 * Handle fault tolerance updates
 *
 * Handle fault tolerance updates
 *
 * @param[in] state Fault tolerance state update
 *
 * @retval ORTE_SUCCESS The operation completed successfully
 * @retval ORTE_ERROR   An unspecifed error occurred
 */
typedef int  (*orte_rml_module_ft_event_fn_t)(int state);

/**
 * Purge the RML/OOB of contact info and pending messages
 * to/from a specified process. Used when a process aborts
 * and is to be restarted
 */
typedef void (*orte_rml_module_purge_fn_t)(orte_process_name_t *peer);

/* ******************************************************************** */


/**
 * RML module interface
 *
 * Module interface to the RML communication system.  A global
 * instance of this module, orte_rml, provices an interface into the
 * active RML interface.
 */
struct orte_rml_module_t {
    /** Enable communication once a process name has been assigned */
    orte_rml_module_enable_comm_fn_t             enable_comm;
    /** Shutdown the communication system and clean up resources */
    orte_rml_module_finalize_fn_t                finalize;

    /** Get contact information for local process */
    orte_rml_module_get_contact_info_fn_t        get_contact_info;
    /** Set contact information for remote process */
    orte_rml_module_set_contact_info_fn_t        set_contact_info;

    /** Ping process for connectivity check */
    orte_rml_module_ping_fn_t                    ping;

    /** Send non-blocking iovec message */
    orte_rml_module_send_nb_fn_t                 send_nb;
    /** Send non-blocking buffer message */
    orte_rml_module_send_buffer_nb_fn_t          send_buffer_nb;

    /** Receive non-blocking iovec message */
    orte_rml_module_recv_nb_fn_t                 recv_nb;

    /** Receive non-blocking buffer message */
    orte_rml_module_recv_buffer_nb_fn_t          recv_buffer_nb;

    /** Cancel posted non-blocking receive */
    orte_rml_module_recv_cancel_fn_t             recv_cancel;

    /** Add callback for communication exception */
    orte_rml_module_exception_fn_t               add_exception_handler;
    /** Delete callback for communication exception */
    orte_rml_module_exception_fn_t               del_exception_handler;

    /** Fault tolerance handler */
    orte_rml_module_ft_event_fn_t                ft_event;
    
    /** Purge information */
    orte_rml_module_purge_fn_t                   purge;
};
/** Convienence typedef */
typedef struct orte_rml_module_t orte_rml_module_t;

/** Interface for RML communication */
ORTE_DECLSPEC extern orte_rml_module_t orte_rml;


/* ******************************************************************** */


/** Macro for use in components that are of type rml */
#define ORTE_RML_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "rml", 2, 0, 0


/* ******************************************************************** */


END_C_DECLS

#endif
