/*
 * $HEADER$
 */
/** @file:
 *
 * Contains the internal functions and typedefs for the use of the oob
 */

#ifndef MCA_OOB_H_
#define MCA_OOB_H_

#include "ompi_config.h"

#include "include/types.h"
#include "mca/mca.h"
#include "mca/oob/base/base.h"
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
*  @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
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
* @param peer (IN)    Opaque name of peer process or MCA_OOB_NAME_ANY for wildcard receive.
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
  /* oob v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* oob v1.0 */ \
  "oob", 1, 0, 0

/*
 * This is the first module on the list. This is here temporarily
 * to make things work
 */
extern mca_oob_t mca_oob;

/**
 * associate a component and a module that belongs to it
 */
struct mca_oob_base_info_t {
  ompi_list_item_t super;
  mca_oob_base_component_t *oob_component;
  mca_oob_t *oob_module;
};
typedef struct mca_oob_base_info_t mca_oob_base_info_t;

/**
 * declare the association structure as a class
 */
OBJ_CLASS_DECLARATION(mca_oob_base_info_t);


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_oob_base_open(void);
    int mca_oob_base_init(bool *allow_multi_user_threads, bool *have_hidden_threads);
    int mca_oob_base_close(void);

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


/*
 * Global struct holding the selected module's function pointers
 */
extern int mca_oob_base_output;
extern ompi_list_t mca_oob_base_components;
extern ompi_list_t mca_oob_base_modules;

#endif
