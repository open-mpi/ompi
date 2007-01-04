/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 * Contains the internal functions and typedefs for the use of the oob
 */

#ifndef MCA_OOB_H_
#define MCA_OOB_H_

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/types.h"
#include "opal/mca/mca.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"

#include "orte/mca/oob/oob_types.h"
#include "orte/mca/oob/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
struct mca_oob_1_0_0_t;

/**
 * Convenience Typedef
 */
typedef struct mca_oob_1_0_0_t mca_oob_1_0_0_t;
/**
 * Convenience typedef
 */
typedef struct mca_oob_1_0_0_t mca_oob_t;


/********
 * NOTE: these are functions and prototypes for the use of the modules
 *       and components. 
 *       THESE ARE NOT USER INTERFACE FUNCTIONS.
 *       the user interface is in mca/oob/base/base.h
 */

/*
 * OOB Component/Module function prototypes.
 */

/**
*  Implementation of mca_oob_base_module_get_addr().
*/

typedef char* (*mca_oob_base_module_get_addr_fn_t)(void);

/**
*  Implementation of mca_oob_base_module_set_addr().
*
*  @param addr    Address of seed in component specific uri format.
*/

typedef int (*mca_oob_base_module_set_addr_fn_t)(const orte_process_name_t*, const char* uri);


/**
*  Implementation of mca_oob_ping().
*   
*  @param peer (IN)   Opaque name of peer process.
*  @param tv (IN)     Timeout to wait in connection response.
*  @return            OMPI error code (<0) or ORTE_SUCCESS
*/

typedef int (*mca_oob_base_module_ping_fn_t)(const orte_process_name_t*, const char* uri, const struct timeval* tv);

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
    orte_process_name_t* peer,
    struct iovec *msg,
    int count,
    int tag,
    int flags);

/**
*  Implementation of mca_oob_recv().
*
*  @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
*  @param msg (IN)     Array of iovecs describing user buffers and lengths.
*  @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
*  @param count (IN)   Number of elements in iovec array.
*  @param tag (IN)     User defined tag for matching send/recv.
*  @param flags (IN)   May be MCA_OOB_PEEK to return up to the number of bytes provided in the
*                      iovec array without removing the message from the queue.
*  @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*mca_oob_base_module_recv_fn_t)(
    orte_process_name_t* peer,
    struct iovec *msg,
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
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    int flags,
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/**
* Implementation of mca_oob_recv_nb().
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param msg (IN)     Array of iovecs describing user buffers and lengths.
* @param count (IN)   Number of elements in iovec array.
* @param tag (IN)     User defined tag for matching send/recv.
* @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
* @param cbfunc (IN)  Callback function on recv completion.
* @param cbdata (IN)  User data that is passed to callback function.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*mca_oob_base_module_recv_nb_fn_t)(
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    int tag,
    int flags,
    mca_oob_callback_fn_t cbfunc,
    void* cbdata);

/**
* Implementation of mca_oob_recv_cancel().
*
* @param peer (IN)    Opaque name of peer process or ORTE_NAME_WILDCARD for wildcard receive.
* @param tag (IN)     User defined tag for matching send/recv.
* @return             OMPI error code (<0) on error or number of bytes actually received.
*/

typedef int (*mca_oob_base_module_recv_cancel_fn_t)(orte_process_name_t* peer, int tag);

/**
 * Hook function called by mca_oob_base_register to allow
 * the oob component a chance to register contact information
 */
typedef int (*mca_oob_base_module_init_fn_t)(void);

/**
 * Cleanup during finalize.
 */
typedef int (*mca_oob_base_module_fini_fn_t)(void);

/**
 * xcast function for sending common messages to all processes
 */
typedef int (*mca_oob_base_module_xcast_fn_t)(orte_jobid_t job,
                                              bool process_first,
                                              orte_buffer_t* buffer,
                                              orte_gpr_trigger_cb_fn_t cbfunc);

/**
 * OOB Module
 */
struct mca_oob_1_0_0_t {
    mca_oob_base_module_get_addr_fn_t    oob_get_addr;
    mca_oob_base_module_set_addr_fn_t    oob_set_addr;
    mca_oob_base_module_ping_fn_t        oob_ping;
    mca_oob_base_module_send_fn_t        oob_send;
    mca_oob_base_module_recv_fn_t        oob_recv;
    mca_oob_base_module_send_nb_fn_t     oob_send_nb;
    mca_oob_base_module_recv_nb_fn_t     oob_recv_nb;
    mca_oob_base_module_recv_cancel_fn_t oob_recv_cancel;
    mca_oob_base_module_init_fn_t        oob_init;
    mca_oob_base_module_fini_fn_t        oob_fini;
    mca_oob_base_module_xcast_fn_t       oob_xcast;
};

/**
 * OOB Component
 */
typedef mca_oob_t* (*mca_oob_base_component_init_fn_t)(int  *priority);

/**
 * the standard component data structure
 */
struct mca_oob_base_component_1_0_0_t {
   mca_base_component_t oob_base;
   mca_base_component_data_1_0_0_t oob_data;
   mca_oob_base_component_init_fn_t oob_init;
};

/**
 * Convenience Typedef
 */
typedef struct mca_oob_base_component_1_0_0_t mca_oob_base_component_1_0_0_t;
/**
 * Convenience Typedef
 */
typedef mca_oob_base_component_1_0_0_t mca_oob_base_component_t;


/**
 * Macro for use in components that are of type oob v1.0.0
 */
#define MCA_OOB_BASE_VERSION_1_0_0 \
  /* oob v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* oob v1.0 */ \
  "oob", 1, 0, 0

/*
 * This is the first module on the list. This is here temporarily
 * to make things work
 */

ORTE_DECLSPEC extern mca_oob_t mca_oob;

/**
 * associate a component and a module that belongs to it
 */
struct mca_oob_base_info_t {
  opal_list_item_t super;
  mca_oob_base_component_t *oob_component;
  mca_oob_t *oob_module;
};
/**
 * Convenience Typedef
 */
typedef struct mca_oob_base_info_t mca_oob_base_info_t;

/**
 * declare the association structure as a class
 */
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(mca_oob_base_info_t);

/**
 * List of registrations of exception callbacks
 */
struct mca_oob_base_exception_handler_t {
  opal_list_item_t super;
  mca_oob_base_exception_fn_t cbfunc;
};

/**
 * Convenience Typedef
 */
typedef struct mca_oob_base_exception_handler_t mca_oob_base_exception_handler_t;

/**
 * declare the association structure as a class
 */
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(mca_oob_base_exception_handler_t);


/*
 * Global functions for MCA overall collective open and close
 */
ORTE_DECLSPEC int mca_oob_base_open(void);
ORTE_DECLSPEC int mca_oob_base_init(void);
ORTE_DECLSPEC int mca_oob_base_module_init(void);
ORTE_DECLSPEC int mca_oob_base_close(void);


/*
 * Global struct holding the selected module's function pointers
 */
ORTE_DECLSPEC extern int mca_oob_base_output;
ORTE_DECLSPEC extern char* mca_oob_base_include;
ORTE_DECLSPEC extern char* mca_oob_base_exclude;
ORTE_DECLSPEC extern opal_list_t mca_oob_base_components;
ORTE_DECLSPEC extern opal_list_t mca_oob_base_modules;
ORTE_DECLSPEC extern opal_list_t mca_oob_base_exception_handlers;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
