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

/**
 * @file
 *
 * Processor affinity for Microsoft Windows.
 *
 */


#ifndef MCA_PAFFINITY_WINDOWS_EXPORT_H
#define MCA_PAFFINITY_WINDOWS_EXPORT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/paffinity/paffinity.h"


/**
 * Determine whether we have a working CPU_ZERO() macro or not.  If
 * not, use memset().
 */
#ifdef HAVE_CPU_ZERO
#define OMPI_CPU_ZERO(foo) CPU_ZERO(foo)
#else
#include <string.h>
#define OMPI_CPU_ZERO(foo) memset(foo, 0, sizeof(*foo))
#endif


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Globally exported variable
     */
    OPAL_MODULE_DECLSPEC extern const opal_paffinity_base_component_1_0_0_t
        mca_paffinity_windows_component;


    /**
     * paffinity query API function
     */
    const opal_paffinity_base_module_1_0_0_t *
        opal_paffinity_windows_component_query(int *query);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PAFFINITY_WINDOWS_EXPORT_H */
