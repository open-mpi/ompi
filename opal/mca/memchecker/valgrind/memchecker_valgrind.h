/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * This should be described well
 */

#ifndef MCA_MEMCHECKER_VALGRIND_EXPORT_H
#define MCA_MEMCHECKER_VALGRIND_EXPORT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/memchecker/memchecker.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern const opal_memchecker_base_component_1_0_0_t
    mca_memchecker_valgrind_component;

/**
 * memchecker query API function
 */
const opal_memchecker_base_module_1_0_0_t *
    opal_memchecker_valgrind_component_query(int *query);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_MEMCHECKER_VALGRIND_EXPORT_H */
