/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

/** @file **/

#ifndef OPAL_H
#define OPAL_H

#include "opal_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Determine if this is an OPAL level applicaiton or something else.
 * If something else (e.g., ORTE) then we don't want to start the
 * event and progress engines since ORTE does that during it's init.
 *
 * So if we are only doing opal_init, and not someone elses as well.
 * See the opal_init() function for the components that needed to be 
 * called if you are going to set this value to 'false'.
 */
OPAL_DECLSPEC extern bool opal_init_only;

/**
 * Initialize the OPAL layer, including the MCA system.
 *
 * @retval OPAL_SUCCESS Upon success.
 * @retval OPAL_ERROR Upon failure.
 *
 * \note If this function is called, opal_init_util() should *not* be
 * called.
 */
OPAL_DECLSPEC int opal_init(void);

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
OPAL_DECLSPEC int opal_init_util(void);

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

/**
 * Internal function.  Do not call.
 */
OPAL_DECLSPEC int opal_register_params(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
