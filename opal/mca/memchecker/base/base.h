/*
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef OPAL_MEMCHECKER_BASE_H
#define OPAL_MEMCHECKER_BASE_H

#include "opal_config.h"

#include "opal/mca/memchecker/memchecker.h"
#include "ompi/datatype/ompi_datatype.h"

/*
 * Global functions for MCA overall memchecker open and close
 */

BEGIN_C_DECLS

OPAL_DECLSPEC extern memchecker_rw_check_t *memchecker_rw_check;

OPAL_DECLSPEC extern size_t memchecker_num_mem;

OPAL_DECLSPEC extern bool memchecker_enable_check;

OPAL_DECLSPEC extern int memchecker_phase;

/**
 * Initialize the memchecker MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the first function invoked in the memchecker MCA
 * framework.  It initializes the memchecker MCA framework, finds
 * and opens memchecker components, etc.
 *
 * This function is invoked during opal_init() and during the
 * initialization of the special case of the laminfo command.
 *
 * This function fills in the internal global variable
 * opal_memchecker_base_components_opened, which is a list of all
 * memchecker components that were successfully opened.  This
 * variable should \em only be used by other memchecker base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 */
OPAL_DECLSPEC int opal_memchecker_base_open(void);

/**
 * Select one available component.
 *
 * @return OPAL_SUCCESS Upon success.
 *
 * This function invokes the selection process for memchecker
 * components
 */
OPAL_DECLSPEC int opal_memchecker_base_select(void);

/**
 * Shut down the memchecker MCA framework.
 *
 * @retval OPAL_SUCCESS Always
 *
 * This function shuts down everything in the memchecker MCA
 * framework, and is called during opal_finalize() and the
 * special case of the laminfo command.
 *
 * It must be the last function invoked on the memchecker MCA framework.
 */
OPAL_DECLSPEC int opal_memchecker_base_close(void);

/**
 * List of all opened components; created when the memchecker
 * framework is initialized and destroyed, when we reduce the list
 * to all available memchecker components (actually one).
 */
OPAL_DECLSPEC extern opal_list_t opal_memchecker_base_components_opened;

/**
 * Indication of whether one component was successfully selected
 */
OPAL_DECLSPEC extern bool opal_memchecker_base_selected;

/**
 * Global component struct for the selected component
 */
OPAL_DECLSPEC extern const opal_memchecker_base_component_2_0_0_t
    *opal_memchecker_base_component;

/**
 * Global module struct for the selected module
 */
OPAL_DECLSPEC extern const opal_memchecker_base_module_1_0_0_t
    *opal_memchecker_base_module;

/**
 * Debugging output stream
 */
extern int opal_memchecker_base_output;

/**
 * Check if we are running under the memory debugger.
 *
 * @retval 0   if not running under memory debugger
 *         !=0 if running under memory debugger
 *
 */
OPAL_DECLSPEC int opal_memchecker_base_runindebugger(void);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_runindebugger() 0
#endif


/**
 * Check if a memory region is valid to address
 *
 * @param p Pointer to the memory region
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, whether
 *  every Byte of this memory region is addressable
 */
OPAL_DECLSPEC int opal_memchecker_base_isaddressable(void * p, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_isaddressable(p, len) 0
#endif


/**
 * Check if a memory region is defined
 *
 * @param p Pointer to the memory region
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, whether
 * every Byte of this memory region is correctly initialized.
 */
OPAL_DECLSPEC int opal_memchecker_base_isdefined(void * p, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_isdefined(p, len) 0
#endif

/**
 * Set a memory region to not accessible
 *
 * @param p Pointer to the memory region
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to set
 * every Byte of this memory region to not accessible.
 */
OPAL_DECLSPEC int opal_memchecker_base_mem_noaccess(void * p, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_mem_noaccess(p, len)
#endif

/**
 * Set a memory region to undefined
 *
 * @param p Pointer to the memory region
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to set
 * every Byte of this memory region to not contain initialized data.
 */
OPAL_DECLSPEC int opal_memchecker_base_mem_undefined(void * p, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_mem_undefined(p, len)
#endif

/**
 * Set a memory region to defined
 *
 * @param p Pointer to the memory region
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to set
 * every Byte of this memory region to contain valid, initialized data.
 */
OPAL_DECLSPEC int opal_memchecker_base_mem_defined(void * p, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_mem_defined(p, len)
#endif

/**
 * Set a memory region to defined only if the region is addressable
 *
 * @param p Pointer to the memory region
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to set
 * every Byte of this memory region to contain valid, initialized data,
 *  but only, if the memory region is addressable.
 */
OPAL_DECLSPEC int opal_memchecker_base_mem_defined_if_addressable(void * p, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_mem_defined_if_addressable(p, len)
#endif

/**
 * Create a named memory region
 *
 * @param p Pointer to the memory region
 * @param len Length of the memory region
 * @param description Name of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to name
 * this memory region.
 */
OPAL_DECLSPEC int opal_memchecker_base_create_block(void * p, size_t len, char * description);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_create_block(p, len, description)
#endif

/**
 * Discard a named memory region
 *
 * @param p Pointer to the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to discard
 * the name information of the memory region.
 */
OPAL_DECLSPEC int opal_memchecker_base_discard_block(void * p);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_discard_block(p)
#endif

/**
 * Perform leak check on lost allocated memory.
 *
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to output
 * information regarding lost allocated memory.
 */
OPAL_DECLSPEC int opal_memchecker_base_leakcheck(void);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_leakcheck
#endif

/**
 * Get vbits of the memory
 *
 * @param p Pointer to the memory region
 * @param vbits Pointer to the vbit table
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to get
 * every vbit of this memory region.
 */
OPAL_DECLSPEC int opal_memchecker_base_get_vbits(void * p, char * vbits, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_get_vbits(p, vbits, len)
#endif

/**
 * Set vbits of the memory
 *
 * @param p Pointer to the memory region
 * @param vbits Pointer to the vbit table
 * @param len Length of the memory region
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to get
 * every vbit of this memory region.
 */
OPAL_DECLSPEC int opal_memchecker_base_set_vbits(void * p, char * vbits, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_set_vbits(p, vbits, len)
#endif

/**
 * Register memory watch
 *
 * @param p    Pointer to the memory region
 * @param len  Length of the memory region
 * @param op   type of operation under watched
 * @param cb   callback function pointer
 * @param info information returned from cb
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to register
 * the memory region for specified operation.
 */
OPAL_DECLSPEC int opal_memchecker_base_reg_mem_watch(void * p, size_t len, int op, void *cb, void *info);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_reg_mem_watch(p, len, op, cb, info) 0
#endif

/**
 * Unregister memory watch
 *
 * @param p    Pointer to the memory region
 * @param len  Length of the memory region
 * @param cb   callback function pointer
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to unregister
 * the memory watch.
 */
OPAL_DECLSPEC int opal_memchecker_base_unreg_mem_watch(void * p, size_t len);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_unreg_mem_watch(p, len) 0
#endif


/**
 * Unregister all memory watch
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to unregister
 * all memory watch.
 */
OPAL_DECLSPEC int opal_memchecker_base_unreg_all_mem_watch();
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_unreg_all_mem_watch()
#endif


/**
 * Search the entry index
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to search
 * the entry index.
 */
OPAL_DECLSPEC int opal_memchecker_base_search_mem_index(void * p, size_t len, size_t *index);
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_search_mem_index(p, len, index) 0
#endif


/**
 * Check mem watch count
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to get mem count
 */
OPAL_DECLSPEC int opal_memchecker_base_mem_watch_count();
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_mem_watch_count()
#endif


/**
 * Print user application callstack
 *
 * @retval OPAL_SUCCESS upon success.
 *
 * This function calls the selected memchecker, to print
 * user application callstack.
 */
OPAL_DECLSPEC int opal_memchecker_base_print_callstack();
#if OMPI_WANT_MEMCHECKER == 0
#define opal_memchecker_base_print_callstack()
#endif

END_C_DECLS

#endif /* OPAL_MEMCHECKER_BASE_H */
