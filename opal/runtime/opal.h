/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008	   Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2016 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#ifndef OPAL_H
#define OPAL_H

#include "opal_config.h"
#include "opal/class/opal_list.h"
#include "opal/types.h"
#include "opal/util/proc.h"

#include <assert.h>

BEGIN_C_DECLS

/** version string of opal */
OPAL_DECLSPEC extern const char opal_version_string[];

/* Size of a cache line.  Initialized to a fixed value (see
   opal_init.c) until hwloc data is available, at which time it is
   filled with the smallest size of the lowest cache line (e.g., the
   smallest line size from all L2 caches found on the current system).
   If the hwloc data is available, opal_cache_line_size will be set to
   its final value by the end of orte_init(). */
OPAL_DECLSPEC extern int opal_cache_line_size;

/** Do we want to be warned on fork or not? */
OPAL_DECLSPEC extern bool opal_warn_on_fork;

/**
 * @brief list of cleanup functions that should be called as part of opal_finalize_util().
 *        opal_finalize()
 */
extern opal_list_t opal_finalize_cleanup_fns;

/**
 * Initialize the OPAL layer, including the MCA system.
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_init_util() should *not* be
 * called.
 */
OPAL_DECLSPEC int opal_init(int *pargc, char ***pargv);

/**
 * Finalize the OPAL layer, including the MCA system.
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_finalize_util() should *not*
 * be called.
 */
OPAL_DECLSPEC int opal_finalize(void);

/**
 * Initialize the OPAL layer, excluding the MCA system.
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_init() should *not*
 * be called.
 */
OPAL_DECLSPEC int opal_init_util(int *pargc, char ***pargv);

/**
 * Disable PSM/PSM2 signal hijacking.
 *
 * See comment in the function for more detail.
 */
OPAL_DECLSPEC int opal_init_psm(void);

/**
 * Finalize the OPAL layer, excluding the MCA system.
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_finalize() should *not*
 * be called.
 */
OPAL_DECLSPEC int opal_finalize_util(void);

OPAL_DECLSPEC void opal_warn_fork(void);

/**
 * Internal function.  Only valid when called from opal_init_util().
 */
OPAL_DECLSPEC int opal_register_params(void);

/**
 * Internal function.  Should not be called directly (should only be
 * invoked internally by opal_init() and opal_gethostname()).
 */
OPAL_DECLSPEC int opal_init_gethostname(void);

/**
 * Wrapper to return the hostname value that is in
 * opal_process_info.nodename, as opposed to calling gethostname()
 * directly, which is not guaranteed to be null-terminated and varies
 * in its behavior depending on implementation. The
 * opal_process_info.nodename value is first populated in
 * opal/runtime/opal_init.c.
 *
 * NOTE: In some cases (usually: developer debugging), it is possible
 * that this function is invoked (e.g., via opal_output()) before
 * opal_init() has been invoked, and therefore
 * opal_process_info.nodename is still NULL.  In those cases, just
 * call opal_init_gethostname() directly to fill in
 * opal_process_info.nodename.
 */
static inline const char *opal_gethostname(void)
{
    if (NULL == opal_process_info.nodename) {
        opal_init_gethostname();
    }
    return opal_process_info.nodename;
}

/* finalize cleanup */
/**
 * @brief Cleanup domain
 *
 * Cleanup domains are made up of a list of functions that need to be
 * called at finalize. A domain can be allocated/constructed then be
 * passed to opal_finalize_domain_init() to give it a name. The name
 * is optional and is used only for debugging purposes (this mean it
 * *is* optional *but* still recommended. You can then set the
 * finalize domain using opal_finalize_set_domain(). Once this is
 * called all cleanup functions registered with
 * opal_finalize_append_cleanup() will be registered to the set
 * domain. To call the finalize functions in a domain call
 * the opal_finalize_cleanup_domain() API.
 */
struct opal_finalize_domain_t {
    /** domains are opal lists */
    opal_list_t super;
    /** name of this finalize domain */
    char *domain_name;
};
typedef struct opal_finalize_domain_t opal_finalize_domain_t;

OBJ_CLASS_DECLARATION(opal_finalize_domain_t);

/**
 * @brief Initialize a finalize domain.
 *
 * @param[in] domain      Finalize domain to initialize
 * @param[in] domain_name Name for this finalize domain (may be NULL)
 *
 * This function sets the name of a finalize domain. The domain must
 * have already been initialized by OBJ_CONSTRUCT() or OBJ_NEW().
 */
void opal_finalize_domain_init(opal_finalize_domain_t *domain, const char *domain_name);

/**
 * @brief Set the current finalize domain for opal_finalize_append_cleanup()
 *
 * @param[in] domain     Finalize domain to use
 *
 * This function sets the current finalize domain. This API is not thread safe
 * and is must be protected from multi-threaded invocation.
 */
void opal_finalize_set_domain(opal_finalize_domain_t *domain);

/**
 * @brief Finalize a domain
 *
 * @param[in] domain      Finalize domain to cleanup
 *
 * This function calls all the finalization functions registered with the
 * specified domain in reverse-registration order. This function releases
 * any memory allocated by the relevant calls to opal_finalize_append_cleanup()
 * and effectively empties the cleanup domain.
 */
void opal_finalize_cleanup_domain(opal_finalize_domain_t *domain);

/**
 * @brief Cleanup domain function
 *
 * The argument is optional. It is valid to use the opal_finalize_register_cleanup()
 * macro to register a function that is of type void (*) (void).
 */
typedef void (*opal_cleanup_fn_t)(void *);

/**
 * @brief Append a cleanup function to the current domain
 *
 * @param[in] cleanup_fn     Cleanup function to register
 * @param[in] fn_name        Name of the cleanup function (for debugging)
 * @param[in] user_data      User data to pass to the cleanup function
 */
void opal_finalize_append_cleanup(opal_cleanup_fn_t cleanup_fn, const char *fn_name,
                                  void *user_data);

#define opal_finalize_register_cleanup_3(x, y, z) \
    opal_finalize_append_cleanup((opal_cleanup_fn_t) x, y, z)
#define opal_finalize_register_cleanup_arg(x, y) \
    opal_finalize_append_cleanup((opal_cleanup_fn_t) x, #x "(" #y ")", y)
#define opal_finalize_register_cleanup(x) \
    opal_finalize_register_cleanup_3((opal_cleanup_fn_t)(x), #x, NULL)

/* opal cleanup domains */
extern opal_finalize_domain_t opal_init_util_domain;
extern opal_finalize_domain_t opal_init_domain;

END_C_DECLS

#endif
