/*
 * Copyright (c) 2007      High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007-2013 The University of Tennessee and The University of
 *                         Tennessee Research Foundation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * Some text copied from and references made to mpi_interface.h.
 * 
 * Copyright (C) 2000-2004 by Etnus, LLC
 * Copyright (C) 1999 by Etnus, Inc.
 * Copyright (C) 1997-1998 Dolphin Interconnect Solutions Inc.
 *
 * $HEADER$
 */

#ifndef __MPIDBG_INTERFACE_H__
#define __MPIDBG_INTERFACE_H__ 1

#include "ompi_config.h"

/*
 * This file provides interface functions for a debugger to gather
 * additional information about MPI handles.
 */
#include <sys/types.h>

/* Include the Etnus debugger message queue interface so that we can
   use much of its infrastructure (e.g., the mqs_basic_callbacks,
   mqs_image_callbacks, and mqs_process_callbacks). */
#define FOR_MPI2 0
#include "msgq_interface.h"

/**************************************************************************
 * Types and macros
 **************************************************************************/

enum {
    MPIDBG_MAX_OBJECT_NAME = MPI_MAX_OBJECT_NAME
};
enum {
    MPIDBG_MAX_FILENAME = 1024
};
enum {
    MPIDBG_INTERFACE_VERSION = 1
};


/*-----------------------------------------------------------------------
 * Global initialization information for the DLL
 *-----------------------------------------------------------------------*/

/* Structure containing types for C and C++ MPI handles */
struct mpidbg_handle_info_t {
    /* C handle types.  They are typically pointers to something or
       integers. */
    /* Back-end type for MPI_Aint */
    mqs_type *hi_c_aint;
    /* Back-end type for MPI_Comm */
    mqs_type *hi_c_comm;
    /* Back-end type for MPI_Datatype */
    mqs_type *hi_c_datatype;
    /* Back-end type for MPI_Errhandler */
    mqs_type *hi_c_errhandler;
    /* Back-end type for MPI_File */
    mqs_type *hi_c_file;
    /* Back-end type for MPI_Group */
    mqs_type *hi_c_group;
    /* Back-end type for MPI_Info */
    mqs_type *hi_c_info;
    /* Back-end type for MPI_Offset */
    mqs_type *hi_c_offset;
    /* Back-end type for MPI_Op */
    mqs_type *hi_c_op;
    /* Back-end type for MPI_Request */
    mqs_type *hi_c_request;
    /* Back-end type for MPI_Status */
    mqs_type *hi_c_status;
    /* Back-end type for MPI_Win */
    mqs_type *hi_c_win;

    /* C++ handle types.  Note that these will always be *objects*,
       never pointers. */
    /* Back-end type for MPI::Aint */
    mqs_type *hi_cxx_aint;
    /* Back-end type for MPI::Comm */
    mqs_type *hi_cxx_comm;
    /* Back-end type for MPI::Intracomm */
    mqs_type *hi_cxx_intracomm;
    /* Back-end type for MPI::Intercomm */
    mqs_type *hi_cxx_intercomm;
    /* Back-end type for MPI::Graphcomm */
    mqs_type *hi_cxx_graphcomm;
    /* Back-end type for MPI::Cartcomm */
    mqs_type *hi_cxx_cartcomm;
    /* Back-end type for MPI::Datatype */
    mqs_type *hi_cxx_datatype;
    /* Back-end type for MPI::Errhandler */
    mqs_type *hi_cxx_errhandler;
    /* Back-end type for MPI::File */
    mqs_type *hi_cxx_file;
    /* Back-end type for MPI::Group */
    mqs_type *hi_cxx_group;
    /* Back-end type for MPI::Info */
    mqs_type *hi_cxx_info;
    /* Back-end type for MPI::Offset */
    mqs_type *hi_cxx_offset;
    /* Back-end type for MPI::Op */
    mqs_type *hi_cxx_op;
    /* Back-end type for MPI::Request */
    mqs_type *hi_cxx_request;
    /* Back-end type for MPI::Prequest */
    mqs_type *hi_cxx_prequest;
    /* Back-end type for MPI::Grequest */
    mqs_type *hi_cxx_grequest;
    /* Back-end type for MPI::Status */
    mqs_type *hi_cxx_status;
    /* Back-end type for MPI::Win */
    mqs_type *hi_cxx_win;
};

enum mpidbg_return_codes_t {
    /* Success */
    MPIDBG_SUCCESS,
    /* Something was not found */
    MPIDBG_ERR_NOT_FOUND,
    /* Something is not supported */
    MPIDBG_ERR_NOT_SUPPORTED,
    /* Something is out of range */
    MPIDBG_ERR_OUT_OF_RANGE,
    /* Something is not available */
    MPIDBG_ERR_UNAVAILABLE,
    /* Ran out of memory */
    MPIDBG_ERR_NO_MEM,
    /* Sentinel max value */
    MPIDBG_MAX_RETURN_CODE
};

/*-----------------------------------------------------------------------
 * General data structures
 *-----------------------------------------------------------------------*/

/* Information about MPI processes */
struct mpidbg_process_t {
    /* JMS: need something to uniquely ID MPI processes in the
       presence of MPI_COMM_SPAWN */

    /* Global rank in MPI_COMM_WORLD */
    int mpi_comm_world_rank;
};
/* ==> JMS Should we just use mqs_process_location instead?  George
   thinks that this is unncessary -- perhaps due to the fact that we
   could use mqs_process_location...?  Need to get some feedback from
   others on this one.  Need to check Euro PVM/MPI '06 paper... */

/* General name -> handle address mappings.  This is an optional type
   that is used to describe MPI's predefined handles if the
   pre-defined names do not appear as symbols in the MPI process.
   E.g., if MPI_COMM_WORLD is a #define that maps to some other value,
   this data structure can be used to map the string "MPI_COMM_WORLD"
   to the actual value of the handle that it corresponds to (e.g., 0
   or a pointer value). */
struct mpidbg_name_map_t {
    /* Name of the handle */
    char *map_name;

    /* Handle that the name corresponds to.  Will be 0/NULL if there
       is no corresponding back-end object. */
    mqs_taddr_t map_handle;
};

/* MPI attribute / value pairs.  Include both a numeric and string
   key; pre-defined MPI keyvals (e.g., MPI_TAG_MAX) have a
   human-readable string name.  The string will be NULL for
   non-predefined keyvals. */
struct mpidbg_attribute_pair_t {
    /* Keyval */
    int keyval;
    /* Keyval name; will be non-NULL for attributes that have a
       human-readable name (e.g., MPI predefined keyvals) */
    char *keyval_name;
    /* Value */
    char *value;
};

/*-----------------------------------------------------------------------
 * Communicators
 *-----------------------------------------------------------------------*/

/* Using an enum instead of #define because debuggers can show the
   *names* of enum values, not just the values. */
enum mpidbg_comm_capabilities_t {
    /* Whether this MPI DLL supports returning basic information about
       communicators */
    MPIDBG_COMM_CAP_BASIC =                  0x01,
    /* Whether this MPI DLL supports returning names of
       communicators */
    MPIDBG_COMM_CAP_STRING_NAMES =           0x02,
    /* Whether this MPI DLL supports indicating whether a communicator
       has been freed by the user application */
    MPIDBG_COMM_CAP_FREED_HANDLE =           0x04,
    /* Whether this MPI DLL supports indicating whether a communicator
       object has been freed by the MPI implementation or not */
    MPIDBG_COMM_CAP_FREED_OBJECT =           0x08,
    /* Whether this MPI DLL supports returning the list of MPI request
       handles that are pending on a communicator */
    MPIDBG_COMM_CAP_REQUEST_LIST =           0x10,
    /* Whether this MPI DLL supports returning the list of MPI window
       handles that were derived from a given communicator */
    MPIDBG_COMM_CAP_WINDOW_LIST =            0x20,
    /* Whether this MPI DLL supports returning the list of MPI file
       handles that were derived from a given communicator */
    MPIDBG_COMM_CAP_FILE_LIST =              0x40,
    /* Sentinel max value */
    MPIDBG_COMM_CAP_MAX
};

enum mpidbg_comm_info_bitmap_t {
    /* Predefined communicator if set (user-defined if not set) */
    MPIDBG_COMM_INFO_PREDEFINED =      0x01,
    /* Whether this communicator is a cartesian communicator or not
       (mutually exclusive with _GRAPH and _INTERCOMM) */
    MPIDBG_COMM_INFO_CARTESIAN =       0x02,
    /* Whether this communicator is a graph communicator or not
       (mutually exclusive with _CARTESIAN and _INTERCOMM) */
    MPIDBG_COMM_INFO_GRAPH =           0x04,
    /* If a cartesian or graph communicator, whether the processes in
       this communicator were re-ordered when the topology was
       assigned. */
    MPIDBG_COMM_INFO_TOPO_REORDERED =  0x08,
    /* Whether this is an intercommunicator or not (this communicator
       is an intracommunicator if this flag is not yet). */
    MPIDBG_COMM_INFO_INTERCOMM =       0x10,
    /* This communicator has been marked for freeing by the user
       application if set */
    MPIDBG_COMM_INFO_FREED_HANDLE =    0x20,
    /* This communicator has actually been freed by the MPI
       implementation if set */
    MPIDBG_COMM_INFO_FREED_OBJECT =    0x40,
    /* The queried communicator is MPI_COMM_NULL */
    MPIDBG_COMM_INFO_COMM_NULL =       0x80,
    /* The queried communicator has a distributed graph topology attached to it */
    MPIDBG_COMM_INFO_DIST_GRAPH =      0x00000400,
    /* Sentinel max value */
    MPIDBG_COMM_INFO_MAX
};

struct mpidbg_comm_info_t {
    /* Name of the MPI_COMM */
    char comm_name[MPIDBG_MAX_OBJECT_NAME];

    /* Bit flags describing the communicator */
    enum mpidbg_comm_info_bitmap_t comm_bitflags;

    /* This process' rank within this communicator */
    int comm_rank;
    /* The communicator's size  */
    int comm_size;

    /* Number of processes in the local group */
    int comm_num_local_procs;
    /* Information about each process in the local group (in
       communicator rank order, length: comm_num_local_procs) */
    struct mpidbg_process_t *comm_local_procs;

    /* For intercommunicators, the number of processes in the remote
       group */
    int comm_num_remote_procs;
    /* For intercommunicators, information about each process in the
       remote group (in communicator rank order, length:
       comm_num_remote_procs) */
    struct mpidbg_process_t *comm_remote_procs;

    /* For cartesian communicators, the number of dimensions */
    int comm_cart_num_dims;
    /* For cartesian communicators, an array of dimension lengths
       (length: cart_comm_num_dims) */
    int *comm_cart_dims;
    /* For cartesian communicators, an array of boolean values
       indicating whether each dimension is periodic or not (length:
       cart_comm_num_dims) */
    int8_t *comm_cart_periods;

    /* For graph communicators, the number of nodes */
    int comm_graph_num_nodes;
    /* For graph communicators, an array of the node degrees (length:
       comm_graph_num_nodes) */
    int *comm_graph_index;
    /* For graph communicators, an array of the edges (length:
       comm_graph_num_nodes) */
    int *comm_graph_edges;

    /* C handle */
    mqs_taddr_t comm_c_handle;
    /* Fortran handle; will be MPIDBG_ERR_UNAVAILABLE if currently
       unavailable or MPIDBG_ERR_NOT_SUPPORTED if not supported */
    int comm_fortran_handle;

    /* Number of attributes defined on this communicator */
    int comm_num_attrs;
    /* Array of attribute keyval/value pairs defined on this
       communicator (length: comm_num_attrs) */
    struct mpidbg_attribute_pair_t *comm_attrs;

    /* Number of ongoing requests within this communicator, or
       MPIDBG_ERR_NOT_SUPPORTED */
    int comm_num_pending_requests;
    /* If comm_num_pending_requests != MPIDBG_ERR_NOT_SUPPORTED, an
       array of ongoing request handles attached on this
       communicator (length: comm_num_pending_requests) */
    mqs_taddr_t *comm_pending_requests;

    /* Number of MPI windows derived from this communicator, or
       MPIDBG_ERR_NOT_SUPPORTED  */
    int comm_num_derived_windows;
    /* If comm_num_derived_windows != MPIDBG_ERR_NOT_SUPPORTED, an
       array of window handles derived from this communicator (length:
       com_num_derived_windows) */
    mqs_taddr_t *comm_derived_windows;

    /* Number of MPI files derived from this communicator, or
       MPIDBG_ERR_NOT_SUPPORTED  */
    int comm_num_derived_files;
    /* If comm_num_derived_files != MPIDBG_ERR_NOT_SUPPORTED, an array
       of file handles derived from this communicator (length:
       comm_num_derived_files) */
    mqs_taddr_t *comm_derived_files;
};


/*-----------------------------------------------------------------------
 * Requests
 *-----------------------------------------------------------------------*/

/* Using an enum instead of #define because debuggers can show the
   *names* of enum values, not just the values. */
enum mpidbg_request_capabilities_t {
    /* Whether this MPI DLL supports returning basic information about
       requests */
    MPIDBG_REQUEST_CAP_BASIC =           0x01,
    /* Sentinel max value */
    MPIDBG_REQUEST_CAP_MAX
};

enum mpidbg_request_info_bitmap_t {
    /* Predefined request if set (user-defined if not set) */
    MPIDBG_REQUEST_INFO_PREDEFINED =      0x01,
    /* Sentinel max value */
    MPIDBG_REQUEST_INFO_MAX
};

struct mpidbg_request_info_t {
    /* Bit flags describing the error handler */
    enum mpidbg_request_info_bitmap_t req_bitflags;

    /* C handle */
    mqs_taddr_t req_c_handle;
    /* Fortran handle; will be MPIDBG_ERR_UNAVAILABLE if currently
       unavailable or MPIDBG_ERR_NOT_SUPPORTED if not supported */
    int req_fortran_handle;
};

/*-----------------------------------------------------------------------
 * Statuses
 *-----------------------------------------------------------------------*/

enum mpidbg_status_capabilities_t {
    /* Whether this MPI DLL supports returning basic information about
       statuses */
    MPIDBG_STATUS_CAP_BASIC =           0x01,
    /* Sentinel max value */
    MPIDBG_STATUS_CAP_MAX
};

enum mpidbg_status_info_bitmap_t {
    /* Predefined status if set (user-defined if not set) */
    MPIDBG_STATUS_INFO_PREDEFINED =      0x01,
    /* Sentinel max value */
    MPIDBG_STATUS_INFO_MAX
};

struct mpidbg_status_info_t {
    /* Bit flags describing the error handler */
    enum mpidbg_status_info_bitmap_t status_bitflags;
};

/*-----------------------------------------------------------------------
 * Error handlers
 *-----------------------------------------------------------------------*/

/* Using an enum instead of #define because debuggers can show the
   *names* of enum values, not just the values. */
enum mpidbg_errhandler_capabilities_t {
    /* Whether this MPI DLL supports returning basic information about
       error handlers */
    MPIDBG_ERRH_CAP_BASIC =           0x01,
    /* Whether this MPI DLL supports returning names of the predefined
       error handlers */
    MPIDBG_ERRH_CAP_STRING_NAMES =    0x02,
    /* Whether this MPI DLL supports indicating whether an error
       handler has been freed by the user application */
    MPIDBG_ERRH_CAP_FREED_HANDLE =    0x04,
    /* Whether this MPI DLL supports indicating whether an error
       handler object has been freed by the MPI implementation or
       not */
    MPIDBG_ERRH_CAP_FREED_OBJECT =    0x08,
    /* Whether this MPI DLL supports returning the list of MPI handles
       that an MPI error handler is attached to */
    MPIDBG_ERRH_CAP_HANDLE_LIST =     0x10,
    /* Sentinel max value */
    MPIDBG_ERRH_CAP_MAX
};

enum mpidbg_errhandler_info_bitmap_t {
    /* Predefined error handler if set (user-defined if not set) */
    MPIDBG_ERRH_INFO_PREDEFINED =      0x01,
    /* Communicator error handler if set */
    MPIDBG_ERRH_INFO_COMMUNICATOR =    0x02,
    /* File error handler if set */
    MPIDBG_ERRH_INFO_FILE =            0x04,
    /* Window error handler if set */
    MPIDBG_ERRH_INFO_WINDOW =          0x08,
    /* Callback is in C if set (Fortran if not set) */
    MPIDBG_ERRH_INFO_C_CALLBACK =      0x10,
    /* This errorhandler has been marked for freeing by the user
       application if set */
    MPIDBG_ERRH_INFO_FREED_HANDLE =    0x20,
    /* This errorhandler has actually been freed by the MPI
       implementation if set */
    MPIDBG_ERRH_INFO_FREED_OBJECT =    0x40,
    /* Sentinel max value */
    MPIDBG_ERRH_INFO_MAX
};

struct mpidbg_errhandler_info_t {
    /* String name; only relevant for predefined errorhandlers.  If
       not a predefined errorhandler, eh_name[0] will be '\0'; */
    char eh_name[MPIDBG_MAX_OBJECT_NAME];

    /* Bit flags describing the error handler */
    enum mpidbg_errhandler_info_bitmap_t eh_bitflags;

    /* C handle */
    mqs_taddr_t eh_c_handle;
    /* Fortran handle; will be MPIDBG_ERR_UNAVAILABLE if currently
       unavailable or MPIDBG_ERR_NOT_SUPPORTED if not supported */
    int eh_fortran_handle;

    /* Number of MPI handles that this error handler is attached to.
       MPIDBG_ERR_NOT_SUPPORTED means that this information is not
       supported by the DLL. */
    int16_t eh_refcount;
    /* If eh_refcount != MPIDBG_ERR_NOT_SUPPORTED, list of handles
       that are using this error handler (length: eh_refcount). */
    mqs_taddr_t *eh_handles;

    /* Address of the user-defined error handler (will be 0 for
       predefined error handlers).  Note that each of the 3 C
       callbacks contain an MPI handle; the debugger will need to
       figure out the appropriate size for these types depending on
       the platform and MPI implementation.  This value will be NULL
       if MPIDBG_ERRH_INFO_PREDEFINED is set on the flags. */
    mqs_taddr_t eh_callback_func;
};

/**************************************************************************
 * Global variables
 *
 * mpidbg_dll_locations is in the MPI application; all others are in
 * the DLL.
 **************************************************************************/

/* Array of filenames instantiated IN THE MPI APPLICATION (*NOT* in
   the DLL) that provides an set of locations where DLLs may be found.
   The last pointer in the array will be a NULL sentinel value.  The
   debugger can scan the entries in the array, find one that matches
   the debugger (by examining a) whether the dlopen works or not, and
   b) if the dlopen succeeds, examine mpidbg_dll_is_big_endian and
   mpidbg_dll_bitness), and try to dynamically open the dl_filename.
   Notes:

   1. It is not an error if a dl_filename either does not exist or is
      otherwise un-openable (the debugger can just try the next
      match).
   2. This array values are not valid until MPIR_Breakpoint.
   3. If a filename is absolute, the debugger will attempt to load
      exactly that.  If the filename is relative, the debugger may try
      a few prefix variations to find the DLL.
 */
extern char **mpidbg_dll_locations;

/* Global variable *in the DLL* describing whether this DLL is big or
   little endian (1 = big endian, 0 = little endian).  This value is
   valid immediately upon opening of the DLL. */
extern char mpidbg_dll_is_big_endian;

/* Global variable *in the DLL* describing the bitness of the DLL (8,
   16, 32, 64, ...).  This value is valid immediately upon opening of
   the DLL. */
extern char mpidbg_dll_bitness;

/* Global variable *in the DLL* describing the DLL's capabilties with
   regards to communicators.  This value is valid after a successfull
   call to mpidbg_init_per_process(). */
extern enum mpidbg_comm_capabilities_t mpidbg_comm_capabilities;

/* Global variable *in the DLL* that is an array of MPI communicator
   handle names -> handle mappings (the last entry in the array is
   marked by a NULL string value).  For example, MPI_COMM_WORLD may
   not appear as a symbol in an MPI process, but the debugger needs to
   be able to map this name to a valid handle.  MPI implementations
   not requiring this mapping can either have a NULL value for this
   variable or have a single entry that has a NULL string value.  This
   variable is not valid until after a successfull call to
   mpidbg_init_per_process().  */
extern struct mpidbg_name_map_t *mpidbg_comm_name_map;

/* Global variable *in the DLL* describing the DLL's capabilties with
   regards to error handlers.  This value is valid after a successfull
   call to mpidbg_init_per_process(). */
extern enum mpidbg_errhandler_capabilities_t mpidbg_errhandler_capabilities;

/* Global variable *in the DLL* that is an array of MPI error handler
   handle names -> handle mappings.  It is analogous to
   mpidbg_comm_name_map; see above for details. */
extern struct mpidbg_name_map_t *mpidbg_errhandler_name_map;

/**************************************************************************
 * Functions
 **************************************************************************/

/*-----------------------------------------------------------------------
 * DLL infrastructure functions
 *-----------------------------------------------------------------------*/

/* This function must be called once before any other mpidbg_*()
   function is called, and before most other global mpidbg_* data is
   read.  It is only necessary to call this function once for a given
   debugger instantiation.  This function will initialize all mpidbg
   global state, to include setting all relevant global capability
   flags.

   Parameters:

   IN: callbacks: Table of pointers to the debugger functions. The DLL
                  need only save the pointer, the debugger promises to
                  maintain the table of functions valid for as long as
                  needed.  The table remains the property of the
                  debugger, and should not be altered or deallocated
                  by the DLL. This applies to all of the callback
                  tables.

   This function will return:

   MPIDBG_SUCCESS: if all initialization went well
   MPIDBG_ERR_*: if something went wrong.
*/
int mpidbg_init_once(const mqs_basic_callbacks *callbacks);

/*-----------------------------------------------------------------------*/

/* Query the DLL to find out what version of the interface it
   supports. 

   Parameters:

   None.

   This function will return:

   MPIDBG_INTERFACE_VERSION
*/

int mpidbg_interface_version_compatibility(void);

/*-----------------------------------------------------------------------*/

/* Returns a string describing this DLL.

   Parameters: 

   None

   This function will return:

   A null-terminated string describing this DLL.
*/   
char *mpidbg_version_string(void);

/*-----------------------------------------------------------------------*/

/* Returns the address width that this DLL was compiled with.

   Parameters: 

   None

   This function will return:

   sizeof(mqs_taddr_t)
*/

int mpidbg_dll_taddr_width(void);

/*-----------------------------------------------------------------------*/

/* Setup debug information for a specific image, this must save the
   callbacks (probably in the mqs_image_info), and use those functions
   for accessing this image.

   The DLL should use the mqs_put_image_info and mqs_get_image_info
   functions to associate whatever information it wants to keep with
   the image (e.g., all of the type offsets it needs could be kept
   here).  The debugger will call mqs_destroy_image_info when it no
   longer wants to keep information about the given executable.
 
   This will be called once for each executable image in the parallel
   job.

   Parameters:

   IN: image: the application image.
   IN: callbacks: Table of pointers to the debugger image-specific
                  functions. The DLL need only save the pointer, the
                  debugger promises to maintain the table of functions
                  valid for as long as needed.  The table remains the
                  property of the debugger, and should not be altered
                  or deallocated by the DLL. This applies to all of
                  the callback tables.
   IN/OUT: handle_types: a pointer to a pre-allocated struct
                         containing mqs_types for each of the MPI
                         handle types.  Must be filled in with results
                         from mqs_find_type for each MPI handle type.

   This function will return:

   MPIDBG_SUCCESS: if all initialization went well
   MPIDBG_ERR_NOT_SUPPORTED: if the image does not support the MPIDBG
                   interface.  In this case, no other mpidbg functions
                   will be invoked on this image (not even
                   mpidbg_finalize_per_image()).
   MPIDBG_ERR_*: if something went wrong.
*/
int mpidbg_init_per_image(mqs_image *image,
                          const mqs_image_callbacks *callbacks,
                          struct mpidbg_handle_info_t *handle_types);

/* This function will be called once when an application image that
   previously had mpidbg_init_per_image() successfully invoked that is
   now ending (e.g., the debugger is exiting, the debugger has
   unloaded this image, etc.).  This function can be used to clean up
   any image-specific data.

   Parameters:

   IN: image: the application image.
   IN: image_info: the info associated with the application image.
*/
void mpidbg_finalize_per_image(mqs_image *image, mqs_image_info *image_info);

/*-----------------------------------------------------------------------*/

/* This function will only be called if mpidbg_init_per_image()
   returned successfully, indicating that the image contains
   information for MPI handle information.  If you cannot tell whether
   a process will have MPI handle information in it by examining the
   image, you should return SUCCESS from mpidbg_init_per_image() and
   use this function to check whether MPI handle information is
   available in the process.

   Set up whatever process specific information we need.  For instance,
   addresses of global variables should be handled here rather than in
   the image information, because if data may be in dynamic libraries
   which could end up mapped differently in different processes.

   Note that certain global variables are not valid until after this
   call completes successfully (see above; e.g.,
   mpidbg_comm_capabilities, mpidbg_comm_name_mapping, etc.).

   Parameters:

   IN: process: the process
   IN: callbacks: Table of pointers to the debugger process-specific
                  functions. The DLL need only save the pointer, the
                  debugger promises to maintain the table of functions
                  valid for as long as needed.  The table remains the
                  property of the debugger, and should not be altered
                  or deallocated by the DLL. This applies to all of
                  the callback tables.
   IN/OUT: handle_types: the same handle_types that was passed to
                         mqs_init_per_image.  It can be left unaltered
                         if the results from mqs_init_per_image were
                         sufficient, or modified if necessary to be
                         specific to this process.

   This function will return:

   MPIDBG_SUCCESS: if all initialization went well
   MPIDBG_ERR_NOT_SUPPORTED: if the process does not support the MPIDBG
                   interface.  In this case, no other mpidbg functions
                   will be invoked on this image (not even
                   mpidbg_finalize_per_process()).
   MPIDBG_ERR_*: if something went wrong.
*/
int mpidbg_init_per_process(mqs_process *process, 
                            const mqs_process_callbacks *callbacks,
                            struct mpidbg_handle_info_t *handle_types);

/* This function will be called once when an application image that
   previously had mpidbg_init_per_process() successfully invoked that
   is now ending (e.g., the debugger is exiting, the debugger has
   stopped executing this process, etc.).  This function can be used
   to clean up any process-specific data.

   Parameters:

   IN: process: the application process.
   IN: process_info: the info associated with the application process.
*/
void mpidbg_finalize_per_process(mqs_process *process,
                                 mqs_process_info *process_info);

/*-----------------------------------------------------------------------
 * MPI handle query functions
 * MPI_Comm
 *-----------------------------------------------------------------------*/

/* Query a specific MPI_Comm handle and, if found and valid, allocate
   a new instance of the mpidbg_comm_info_t struct and all of its
   internal data, and fill it in with information about the underlying
   corresponding MPI communicator object.

   Parameters:

   IN: image: image
   IN: image_info: image info that was previously "put"
   IN: process: process
   IN: process_info: process info that was previously "put"
   IN: comm: communicator handle
   OUT: info: pointer to be filled with a newly-allocated struct
              mpidbg_comm_info_t

   This function will return:

   MPIDBG_SUCCESS: if the handle is valid, was found, and the info
                   parameter was filled in successfully.
   MPIDBG_ERR_NOT_FOUND: if the handle is not valid / found.
   MPIDBG_ERR_UNSUPPORTED: if this function is unsupported.
*/
int mpidbg_comm_query(mqs_image *image, mqs_image_info *image_info, 
                      mqs_process *process, mqs_process_info *process_info,
                      mqs_taddr_t c_comm, struct mpidbg_comm_info_t **info);

/* Query function to turn a Fortran INTEGER handle into its equivalent
   C handle (that can then be queried with mpidbg_comm_query()).
   mqs_taddr_t is used in order to guarantee to be large enough to
   hold a Fortran INTEGER.

   Parameters:

   IN: image: image
   IN: image_info: image info that was previously "put"
   IN: process: process
   IN: process_info: process info that was previously "put"
   IN: f77_comm: a zero-padded Fortran integer containing the Fortran
                 handle of the communicator.
   OUT: c_comm: a C handle suitable to pass to mpidbg_comm_query().

   This function returns:

   MPIDBG_SUCCESS: if the handle is valid, was found, and the c_comm
                   parameter was filled in successfully.
   MPIDBG_ERR_NOT_FOUND: if the handle is not valid / found.
   MPIDBG_ERR_UNSUPPORTED: if this function is unsupported.
*/
int mpidbg_comm_f2c(mqs_image *image, mqs_image_info *image_info, 
                    mqs_process *process, mqs_process_info *process_info,
                    mqs_taddr_t f77_comm, mqs_taddr_t *c_comm);

/* Query function to turn a C++ handle into its equivalent C handle
   (that can then be queried with mpidbg_comm_query()).  Pass the
   pointer to the object as the cxx_comm (because we can't pass the
   object itself); we return the C handle.

   --> JMS Need more discussion here -- George has some opinion.  He
       thinks we don't need this.
   Parameters:

   IN: image: image
   IN: image_info: image info that was previously "put"
   IN: process: process
   IN: process_info: process info that was previously "put"
   IN: cxx_comm: a pointer to the MPI handle object
   IN: comm_type: one of 0, MPIDBG_COMM_INFO_CARTESION,
                  MPIDBG_COMM_INFO_GRAPH, or
                  MPIDBG_COMM_INFO_INTERCOMM indicating whether the
                  object is an MPI::Comm, MPI::Cartcomm,
                  MPI::Graphcomm, or MPI::Intercomm.
   OUT: c_comm: a C handle suitable to pass to mpidbg_comm_query().

   This function returns:

   MPIDBG_SUCCESS: if the handle is valid, was found, and the c_comm
                   parameter was filled in successfully.
   MPIDBG_ERR_NOT_FOUND: if the handle is not valid / found.
   MPIDBG_ERR_UNSUPPORTED: if this function is unsupported.
*/
int mpidbg_comm_cxx2c(mqs_image *image, mqs_image_info *image_info, 
                      mqs_process *process, mqs_process_info *process_info,
                      mqs_taddr_t cxx_comm, 
                      enum mpidbg_comm_info_bitmap_t comm_type,
                      mqs_taddr_t *c_comm);

/*-----------------------------------------------------------------------
 * MPI handle query functions
 * MPI_Errhandler
 *-----------------------------------------------------------------------*/

/* These functions are analogous to the mpidbg_comm_* functions, but
   for MPI_Errhandler.  Note that there is no need for a
   "errhandler_type" argument to the cxx2c function because
   MPI::Errhandler has no derived classes. */

int mpidbg_errhandler_query(mqs_image *image, mqs_image_info *image_info, 
                            mqs_process *process, mqs_process_info *process_info,
                            mqs_taddr_t errhandler,
                            struct mpidbg_errhandler_info_t **info);
int mpidbg_errhandler_f2c(mqs_image *image, mqs_image_info *image_info, 
                          mqs_process *process, mqs_process_info *process_info,
                          mqs_taddr_t f77_errhandler, 
                          mqs_taddr_t *c_errhandler);
int mpidbg_errhandler_cxx2c(mqs_image *image, mqs_image_info *image_info, 
                            mqs_process *process, mqs_process_info *process_info,
                            mqs_taddr_t cxx_errhandler, 
                            mqs_taddr_t *c_errhandler);

/*-----------------------------------------------------------------------
 * MPI handle query functions
 * MPI_Request
 *-----------------------------------------------------------------------*/

/* These functions are analogous to the mpidbg_comm_* functions, but
   for MPI_Request. */

int mpidbg_request_query(mqs_image *image, mqs_image_info *image_info, 
                         mqs_process *process, mqs_process_info *process_info,
                         mqs_taddr_t request,
                         struct mpidbg_request_info_t **info);
int mpidbg_request_f2c(mqs_image *image, mqs_image_info *image_info, 
                       mqs_process *process, mqs_process_info *process_info,
                       mqs_taddr_t f77_request, mqs_taddr_t *c_request);
int mpidbg_request_cxx2c(mqs_image *image, mqs_image_info *image_info, 
                         mqs_process *process, mqs_process_info *process_info,
                         mqs_taddr_t cxx_request, 
                         enum mpidbg_request_info_bitmap_t request_type,
                         mqs_taddr_t *c_request);

/*-----------------------------------------------------------------------
 * MPI handle query functions
 * MPI_Status
 *-----------------------------------------------------------------------*/

/* These functions are analogous to the mpidbg_comm_* functions, but
   for MPI_Status. */

int mpidbg_status_query(mqs_image *image, mqs_image_info *image_info, 
                        mqs_process *process, mqs_process_info *process_info,
                        mqs_taddr_t status,
                        struct mpidbg_status_info_t **info);
int mpidbg_status_f2c(mqs_image *image, mqs_image_info *image_info, 
                      mqs_process *process, mqs_process_info *process_info,
                      mqs_taddr_t f77_status, mqs_taddr_t *c_status);
int mpidbg_status_cxx2c(mqs_image *image, mqs_image_info *image_info, 
                        mqs_process *process, mqs_process_info *process_info,
                        mqs_taddr_t cxx_status, 
                        mqs_taddr_t *c_status);

#endif /* __MPIDBG_INTERFACE_H__ */
