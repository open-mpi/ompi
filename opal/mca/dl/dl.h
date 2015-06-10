/*
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Dynamic library framework
 *
 * General Description:
 *
 * This framework provides portable access to dlopen- and dlsym-like
 * functionality, very similar to Libtool's libltdl.  Indeed, one of
 * the components in this framework will use libltdl, if it is
 * present/available.  However, on some common types systems where
 * libltdl headers and libraries are *not* available, we can support
 * plugins via this simple framework.
 *
 * This is a compile-time framework: a single component will be
 * selected by the priority that its configure.m4 provides.  All other
 * components will be ignored (i.e., not built/not part of the
 * installation).  Meaning: the static_components of the dl framework
 * will always contain 0 or 1 components.
 *
 * SIDENOTE: Open MPI used to embed libltdl.  However, as of early
 * 2015, this became problematic, for a variety of complex and
 * uninteresting reasons (see the following if you care about the
 * details: https://github.com/open-mpi/ompi/issues/311,
 * http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19370,
 * https://github.com/open-mpi/ompi/pull/366,
 * https://github.com/open-mpi/ompi/pull/390).  That being said, we,
 * as a developer community, still wanted to be able to natively use
 * DSOs by default.  A small/simple framework for DL functionality,
 * along with a simple component that supports dlopen/dlsym on POSIX
 * platforms and another component that natively uses libltdl seemed
 * like a good solution.
 */

#ifndef MCA_DLOPEN_H
#define MCA_DLOPEN_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

BEGIN_C_DECLS

/**
 * Handle for an opened file
 */
struct opal_dl_handle_t;
typedef struct opal_dl_handle_t opal_dl_handle_t;

/**
 * Dynamically open the file specified.
 *
 * Arguments:
 *   fname   = Base filename to open.  If NULL, open this process.
 *   use_ext = If true, try various filename suffixes that are
 *       relevant on this platform (e.g., .so, .dll, .dylib).  If
 *       false, just use exactly whatever was passed as fname.
 *   private = If true, open the file in a private namespace.
 *       Otherwise, open the file in a global namespace.
 *   handle = Upon successful open, a handle to the opened file will
 *       be returned.
 *   err_msg= if non-NULL and !=OPAL_SUCCESS is returned, will point to a
 *       string error message
 *
 * Returns:
 *   OPAL_SUCCESS on success, or OPAL_ERROR
 *
 * Space for the handle must be allocated by the module (it can be
 * freed during the call to opal_dl_base_module_dlclose_fn_t).
 *
 * The err_msg points to an internal string and should not be altered
 * or freed by the caller.  The contents of the err_msg string may
 * change after successive calls to opal_dl API calls.
 */
typedef int (*opal_dl_base_module_open_fn_t)
    (const char *fname, bool use_ext, bool private_namespace,
     opal_dl_handle_t **handle, char **err_msg);

/**
 * Lookup a symbol in an opened file.
 *
 * Arguments:
 *   handle = handle of a previously dynamically opened file
 *   symbol = name of the symbol to lookup
 *   ptr    = if found, a pointer to the symbol.  Otherwise, NULL.
 *   err_msg= if non-NULL and !=OPAL_SUCCESS is returned, will point to a
 *            string error message
 * Returns:
 *   OPAL_SUCCESS on success, or OPAL_ERROR
 *
 *
 * The err_msg points to an internal string and should not be altered
 * or freed by the caller.  The contents of the err_msg string may
 * change after successive calls to opal_dl API calls.
 */
typedef int (*opal_dl_base_module_lookup_fn_t)
    (opal_dl_handle_t *handle, const char *symbol, void **ptr, char **err_msg);

/**
 * Dynamically close a previously dynamically-opened file.
 *
 * Arguments:
 *   handle = handle of a previously dynamically opened file.
 * Returns:
 *   OPAL_SUCCESS on success, or OPAL_ERROR
 *
 * This function should close the file and free and resources
 * associated with it (e.g., whatever is cached on the handle).
 */
typedef int (*opal_dl_base_module_close_fn_t)
    (opal_dl_handle_t *handle);

/**
 * Search through a path of directories, invoking a callback on each
 * unique regular (non-Libtool) file basename found (e.g., will only
 * be invoked once for the files "foo.la" and "foo.so", with the
 * parameter "foo").
 *
 * Arguments:
 *   path   = OPAL_ENV_SEP-delimited list of directories
 *   cb_func= function to invoke on each filename found
 *   data   = context for callback function
 * Returns:
 *   OPAL_SUCESS on success, OPAL_ERR* otherwise
 */
typedef int (*opal_dl_base_module_foreachfile_fn_t)
    (const char *search_path,
     int (*cb_func)(const char *filename, void *context),
     void *context);

/**
 * Structure for DL components.
 */
struct opal_dl_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;

    /** Default priority */
    int priority;
};
typedef struct opal_dl_base_component_1_0_0_t opal_dl_base_component_1_0_0_t;
typedef struct opal_dl_base_component_1_0_0_t opal_dl_base_component_t;

/**
 * Structure for DL modules
 */
struct opal_dl_base_module_1_0_0_t {
    mca_base_module_2_0_0_t                 super;

    /** Open / close */
    opal_dl_base_module_open_fn_t           open;
    opal_dl_base_module_close_fn_t          close;

    /** Lookup a symbol */
    opal_dl_base_module_lookup_fn_t         lookup;

    /** Iterate looking for files */
    opal_dl_base_module_foreachfile_fn_t    foreachfile;
};
typedef struct opal_dl_base_module_1_0_0_t opal_dl_base_module_1_0_0_t;
typedef struct opal_dl_base_module_1_0_0_t opal_dl_base_module_t;

/**
 * Macro for use in components that are of type DL
 */
#define OPAL_DL_BASE_VERSION_1_0_0              \
    MCA_BASE_VERSION_2_0_0,                     \
    "dl", 1, 0, 0

END_C_DECLS

#endif /* OPAL_DL_H */
