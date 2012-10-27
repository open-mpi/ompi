/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
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
 * This should be described well
 */

#ifndef MCA_MEMCHECKER_PIN_EXPORT_H
#define MCA_MEMCHECKER_PIN_EXPORT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/memchecker/memchecker.h"

BEGIN_C_DECLS

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern const opal_memchecker_base_component_2_0_0_t
    mca_memchecker_pin_component;

/**
 * memchecker query API function
 *
 * Query function for memchecker components.  Simply returns a priority
 * to rank it against other available memchecker components (assumedly,
 * only one component will be available per platform, but it's
 * possible that there could be more than one available).
 */
int opal_memchecker_pin_component_query(mca_base_module_t **module, int *priority);


OPAL_DECLSPEC int MEMPIN_RUNNING_WITH_PIN(int *pin_alive);

OPAL_DECLSPEC int MEMPIN_REG_MEM_WATCH(void* addr, int size, int op, void * cb_func, void* cb_args);

OPAL_DECLSPEC int MEMPIN_UNREG_MEM_WATCH(void* addr, int size);

OPAL_DECLSPEC int MEMPIN_UNREG_ALL_MEM_WATCH();

OPAL_DECLSPEC int MEMPIN_SEARCH_MEM_INDEX(void* addr, int size, size_t *index);

OPAL_DECLSPEC int MEMPIN_MEM_WATCH_COUNT();
    
OPAL_DECLSPEC int MEMPIN_PRINT_CALLSTACK();
    
END_C_DECLS
#endif /* MCA_MEMCHECKER_PIN_EXPORT_H */
