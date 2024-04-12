/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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
 * This framework provides portable access to dlopen- and prtedlsym-like
 * functionality, very similar to Libtool's libltprtedl.  Indeed, one of
 * the components in this framework will use libltprtedl, if it is
 * present/available.  However, on some common types systems where
 * libltprtedl headers and libraries are *not* available, we can support
 * plugins via this simple framework.
 *
 * This is a compile-time framework: a single component will be
 * selected by the priority that its configure.m4 provides.  All other
 * components will be ignored (i.e., not built/not part of the
 * installation).  Meaning: the static_components of the prtedl framework
 * will always contain 0 or 1 components.
 *
 * SIDENOTE: PRTE used to embed libltprtedl.  However, as of early
 * 2015, this became problematic, for a variety of complex and
 * uninteresting reasons (see the following if you care about the
 * details: https://github.com/open-mpi/ompi/issues/311,
 * http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19370,
 * https://github.com/open-mpi/ompi/pull/366,
 * https://github.com/open-mpi/ompi/pull/390).  That being said, we,
 * as a developer community, still wanted to be able to natively use
 * DSOs by default.  A small/simple framework for DL functionality,
 * along with a simple component that supports dlopen/prtedlsym on POSIX
 * platforms and another component that natively uses libltprtedl seemed
 * like a good solution.
 */

#ifndef PRTE_MCA_DL_DL_H
#define PRTE_MCA_DL_DL_H

#include "prte_config.h"

#include "src/pmix/pmix-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

BEGIN_C_DECLS

/**
 * Handle for an opened file
 */
struct prte_dl_handle_t;
typedef struct prte_dl_handle_t prte_dl_handle_t;

/**
 * Dynamically open the file specified.
 *
 * Arguments:
 *   fname   = Base filename to open.  If NULL, open this process.
 *   use_ext = If true, try various filename suffixes that are
 *       relevant on this platform (e.g., .so, .prtedll, .dylib).  If
 *       false, just use exactly whatever was passed as fname.
 *   private = If true, open the file in a private namespace.
 *       Otherwise, open the file in a global namespace.
 *   handle = Upon successful open, a handle to the opened file will
 *       be returned.
 *   err_msg= if non-NULL and !=PRTE_SUCCESS is returned, will point to a
 *       string error message
 *
 * Returns:
 *   PRTE_SUCCESS on success, or PRTE_ERROR
 *
 * Space for the handle must be allocated by the module (it can be
 * freed during the call to prte_prtedl_base_module_prtedlclose_fn_t).
 *
 * The err_msg points to an internal string and should not be altered
 * or freed by the caller.  The contents of the err_msg string may
 * change after successive calls to prte_prtedl API calls.
 */
typedef int (*prte_prtedl_base_module_open_fn_t)(const char *fname, bool use_ext,
                                                 bool private_namespace, prte_dl_handle_t **handle,
                                                 char **err_msg);

/**
 * Lookup a symbol in an opened file.
 *
 * Arguments:
 *   handle = handle of a previously dynamically opened file
 *   symbol = name of the symbol to lookup
 *   ptr    = if found, a pointer to the symbol.  Otherwise, NULL.
 *   err_msg= if non-NULL and !=PRTE_SUCCESS is returned, will point to a
 *            string error message
 * Returns:
 *   PRTE_SUCCESS on success, or PRTE_ERROR
 *
 *
 * The err_msg points to an internal string and should not be altered
 * or freed by the caller.  The contents of the err_msg string may
 * change after successive calls to prte_prtedl API calls.
 */
typedef int (*prte_prtedl_base_module_lookup_fn_t)(prte_dl_handle_t *handle, const char *symbol,
                                                   void **ptr, char **err_msg);

/**
 * Dynamically close a previously dynamically-opened file.
 *
 * Arguments:
 *   handle = handle of a previously dynamically opened file.
 * Returns:
 *   PRTE_SUCCESS on success, or PRTE_ERROR
 *
 * This function should close the file and free and resources
 * associated with it (e.g., whatever is cached on the handle).
 */
typedef int (*prte_prtedl_base_module_close_fn_t)(prte_dl_handle_t *handle);

/**
 * Search through a path of directories, invoking a callback on each
 * unique regular (non-Libtool) file basename found (e.g., will only
 * be invoked once for the files "foo.la" and "foo.so", with the
 * parameter "foo").
 *
 * Arguments:
 *   path   = PRTE_ENV_SEP-delimited list of directories
 *   cb_func= function to invoke on each filename found
 *   data   = context for callback function
 * Returns:
 *   PRTE_SUCESS on success, PRTE_ERR* otherwise
 */
typedef int (*prte_prtedl_base_module_foreachfile_fn_t)(
    const char *search_path, int (*cb_func)(const char *filename, void *context), void *context);

/**
 * Structure for DL components.
 */
struct prte_prtedl_base_component_1_0_0_t {
    /** MCA base component */
    pmix_mca_base_component_t base_version;

    /** Default priority */
    int priority;
};
typedef struct prte_prtedl_base_component_1_0_0_t prte_prtedl_base_component_1_0_0_t;
typedef struct prte_prtedl_base_component_1_0_0_t prte_prtedl_base_component_t;

/**
 * Structure for DL modules
 */
struct prte_prtedl_base_module_1_0_0_t {
    pmix_mca_base_module_t super;

    /** Open / close */
    prte_prtedl_base_module_open_fn_t open;
    prte_prtedl_base_module_close_fn_t close;

    /** Lookup a symbol */
    prte_prtedl_base_module_lookup_fn_t lookup;

    /** Iterate looking for files */
    prte_prtedl_base_module_foreachfile_fn_t foreachfile;
};
typedef struct prte_prtedl_base_module_1_0_0_t prte_prtedl_base_module_1_0_0_t;
typedef struct prte_prtedl_base_module_1_0_0_t prte_prtedl_base_module_t;

/**
 * Macro for use in components that are of type DL
 */
#define PRTE_DL_BASE_VERSION_1_0_0 PRTE_MCA_BASE_VERSION_3_0_0("prtedl", 1, 0, 0)

END_C_DECLS

#endif /* PRTE_MCA_DL_DL_H */
