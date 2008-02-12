/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * memchecker (memory checker) framework component interface.
 *
 * Intent
 *
 * This is a very thin framework to abstract memory checking tools,
 * such as valgrind and possibly Sun rtc (memory checking available
 * possibly only under Solaris/Sparc).
 *
 * Currently, only functionality for hiding and unhiding of memory
 * is added; further functions provided by the memory checker/api
 * checker could be added, however, this comes (at least for valgrind)
 * with considerable overhead.
 * One possible option would be to have error_print_callbacks, that
 * output different error messages, depending on the memory location
 * being hit by certain error.
 */

#ifndef OPAL_MCA_MEMCHECKER_MEMCHECKER_H
#define OPAL_MCA_MEMCHECKER_MEMCHECKER_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

/**
 * Query function for memchecker components.  Simply returns a priority
 * to rank it against other available memchecker components (assumedly,
 * only one component will be available per platform, but it's
 * possible that there could be more than one available).
 */
typedef const struct opal_memchecker_base_module_1_0_0_t *
    (*opal_memchecker_base_component_query_1_0_0_fn_t)
    (int *priority);

/**
 * Module initialization function.  Should return OPAL_SUCCESS.
 */
typedef int (*opal_memchecker_base_module_init_1_0_0_fn_t)(void);

/**
 * Module function to query, whether we're under the memory
 * checking program, like valgrind
 */
typedef int (*opal_memchecker_base_module_runindebugger_fn_t)(void);

/**
 * Module function to check, whether memory region is addressible
 */
typedef int (*opal_memchecker_base_module_isaddressible_fn_t)(void * p, size_t len);

/**
 * Module function to check, whether memory region is defined
 */
typedef int (*opal_memchecker_base_module_isdefined_fn_t)(void * p, size_t len);

/**
 * Module function to set memory region to not accessible
 */
typedef int (*opal_memchecker_base_module_mem_noaccess_fn_t)(void * p, size_t len);

/**
 * Module function to set memory region to undefined
 */
typedef int (*opal_memchecker_base_module_mem_undefined_fn_t)(void * p, size_t len);

/**
 * Module function to set memory region to defined
 */
typedef int (*opal_memchecker_base_module_mem_defined_fn_t)(void * p, size_t len);

/**
 * Module function to set memory region to defined, but only if addressible
 */
typedef int (*opal_memchecker_base_module_mem_defined_if_addressible_fn_t)(void * p, size_t len);

/**
 * Module function name a specific memory region
 */
typedef int (*opal_memchecker_base_module_create_block_fn_t)(void * p, size_t len, char * description);

/**
 * Module function to discard a named memory region
 */
typedef int (*opal_memchecker_base_module_discard_block_fn_t)(void * p); /* Here, we need to do some mapping for valgrind */

/**
 * Module function to check for any leaks
 */
typedef int (*opal_memchecker_base_module_leakcheck_fn_t)(void);

/**
 * Module function to get vbits
 */
typedef int (*opal_memchecker_base_module_get_vbits_fn_t)(void * p, char * vbits, size_t len);

/**
 * Module function to set vbits
 */
typedef int (*opal_memchecker_base_module_set_vbits_fn_t)(void * p, char * vbits, size_t len);



/**
 * Structure for memchecker v1.0.0 components.
 * Chained to MCA v1.0.0
 */
struct opal_memchecker_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t memchecker_version;
    /** MCA base data */
    mca_base_component_data_1_0_0_t memchecker_data;

    /** Component query function */
    opal_memchecker_base_component_query_1_0_0_fn_t memchecker_query;
};

/**
 * Convenience typedef
 */
typedef struct opal_memchecker_base_component_1_0_0_t opal_memchecker_base_component_1_0_0_t;

/**
 * Structure for memchecker v1.0.0 modules
 */
struct opal_memchecker_base_module_1_0_0_t {

    /** Module initialization function */
    opal_memchecker_base_module_init_1_0_0_fn_t init;

    /** Module function to check, whether we are executed by memory debugger */
    opal_memchecker_base_module_runindebugger_fn_t runindebugger;

    /** Module function to check, whether memory region is addressible */
    opal_memchecker_base_module_isaddressible_fn_t isaddressible;

    /** Module function to check, whether memory region is defined */
    opal_memchecker_base_module_isdefined_fn_t isdefined;

    /** Module function to set memory region to not accessible */
    opal_memchecker_base_module_mem_noaccess_fn_t mem_noaccess;

    /** Module function to set memory region to undefined */
    opal_memchecker_base_module_mem_undefined_fn_t mem_undefined;

    /** Module function to set memory region to defined */
    opal_memchecker_base_module_mem_defined_fn_t mem_defined;

    /** Module function to set memory region to defined, but only if addressible */
    opal_memchecker_base_module_mem_defined_if_addressible_fn_t mem_defined_if_addressible;

    /** Module function name a specific memory region */
    opal_memchecker_base_module_create_block_fn_t create_block;

    /** Module function to discard a named memory region */
    opal_memchecker_base_module_discard_block_fn_t discard_block;

    /** Module function to check for any leaks */
    opal_memchecker_base_module_leakcheck_fn_t leakcheck;
    
    /** Module function to get vbits */
    opal_memchecker_base_module_get_vbits_fn_t get_vbits;
    
    /** Module function to set vbits */
    opal_memchecker_base_module_set_vbits_fn_t set_vbits;
};

/**
 * Convenience typedef
 */
typedef struct opal_memchecker_base_module_1_0_0_t opal_memchecker_base_module_1_0_0_t;


/**
 * Macro for use in components that are of type memchecker v1.0.0
 */
#define OPAL_MEMCHECKER_BASE_VERSION_1_0_0 \
    /* memchecker v1.0 is chained to MCA v1.0 */ \
    MCA_BASE_VERSION_1_0_0, \
    /* memchecker v1.0 */ \
    "memchecker", 1, 0, 0

#endif /* OPAL_MCA_MEMCHECKER_MEMCHECKER_H */
