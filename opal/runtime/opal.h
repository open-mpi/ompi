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

/** @file **/

#ifndef OPAL_H
#define OPAL_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Initialize the OPAL utilities
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 *
 * This function performs 
 */
int opal_init(void);

/**
 * Finalize the OPAL utilities
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 *
 * This function performs 
 */
int opal_finalize(void);

int opal_register_params(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
