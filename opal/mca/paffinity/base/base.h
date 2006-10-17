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
 *
 */

#ifndef OPAL_PAFFINITY_BASE_H
#define OPAL_PAFFINITY_BASE_H

#include "opal_config.h"

#include "opal/mca/paffinity/paffinity.h"


/*
 * Global functions for MCA overall paffinity open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Initialize the paffinity MCA framework
     *
     * @retval OPAL_SUCCESS Upon success
     * @retval OPAL_ERROR Upon failure
     *
     * This must be the first function invoked in the paffinity MCA
     * framework.  It initializes the paffinity MCA framework, finds
     * and opens paffinity components, etc.
     *
     * This function is invoked during opal_init().
     * 
     * This function fills in the internal global variable
     * opal_paffinity_base_components_opened, which is a list of all
     * paffinity components that were successfully opened.  This
     * variable should \em only be used by other paffinity base
     * functions -- it is not considered a public interface member --
     * and is only mentioned here for completeness.
     */
    OPAL_DECLSPEC int opal_paffinity_base_open(void);
    
    /**
     * Select an available component.
     *
     * @return OPAL_SUCCESS Upon success.
     * @return OPAL_NOT_FOUND If no component can be selected.
     * @return OPAL_ERROR Upon other failure.
     *
     * This function invokes the selection process for paffinity components,
     * which works as follows:
     *
     * - If the \em paffinity MCA parameter is not specified, the
     *   selection set is all available paffinity components.
     * - If the \em paffinity MCA parameter is specified, the
     *   selection set is just that component.
     * - All components in the selection set are queried to see if
     *   they want to run.  All components that want to run are ranked
     *   by their priority and the highest priority component is
     *   selected.  All non-selected components have their "close"
     *   function invoked to let them know that they were not selected.
     * - The selected component will have its "init" function invoked to
     *   let it know that it was selected.
     *
     * If we fall through this entire process and no component is
     * selected, then return OPAL_NOT_FOUND (this is not a fatal
     * error).
     *
     * At the end of this process, we'll either have a single
     * component that is selected and initialized, or no component was
     * selected.  If no component was selected, subsequent invocation
     * of the paffinity wrapper functions will return an error.
     */
    OPAL_DECLSPEC int opal_paffinity_base_select(void);

    /**
     * Get the available number of processors
     *
     * @param num_procs Pointer to int where the number of processors
     * is returned
     *
     * @retval OPAL_SUCCESS upon success.
     * @retval OPAL_NOT_FOUND if no paffinity components are available.
     * @retval OPAL_ERROR upon other error.
     *
     * This function queries the selected paffinity component to find
     * out how many physical processors exist.  
     *
     * If no paffinity components were available, or if the
     * opal_paffinity_base_select() was never invoked, OPAL_NOT_FOUND
     * is returned and num_procs is set to -1.
     */
    OPAL_DECLSPEC int opal_paffinity_base_get_num_processors(int *num_procs);

    /**
     * Set this process' affinity.
     *
     * @param id Virtual processor ID number
     *
     * @retval OPAL_SUCCESS upon success
     * @retval OPAL_NOT_FOUND if no paffinity components are available.
     * @retval OPAL_ERROR upon other error.
     *
     * Set this process' affinity to the CPU ID \em id.  
     *
     * If no paffinity components were available, or if the
     * opal_paffinity_base_select() was never invoked, OPAL_NOT_FOUND
     * is returned.
     */
    OPAL_DECLSPEC int opal_paffinity_base_set(int id);

    /**
     * Get this process' affinity.
     *
     * @param id Pointer to virtual processor ID number
     *
     * @retval OPAL_SUCCESS upon success
     * @retval OPAL_NOT_FOUND if no paffinity components are available.
     * @retval OPAL_ERROR upon other error.
     *
     * Get this process' CPU affinitity virtual ID number and assign
     * it to \em id.  
     *
     * If no paffinity components were available, or if the
     * opal_paffinity_base_select() was never invoked, OPAL_NOT_FOUND
     * is returned and id is set to -1.
     */
    OPAL_DECLSPEC int opal_paffinity_base_get(int *id);

    /**
     * Shut down the paffinity MCA framework.
     *
     * @retval OPAL_SUCCESS Always
     *
     * This function shuts down everything in the paffinity MCA
     * framework, and is called during opal_finalize().
     *
     * It must be the last function invoked on the paffinity MCA
     * framework.
     */
    OPAL_DECLSPEC int opal_paffinity_base_close(void);

    /**
     * Indication of whether a component was successfully selected or
     * not
     */
    OPAL_DECLSPEC extern bool opal_paffinity_base_selected;

    /**
     * Global component struct for the selected component
     */
    OPAL_DECLSPEC extern const opal_paffinity_base_component_1_0_0_t 
        *opal_paffinity_base_component;
    /**
     * Global module struct for the selected module
     */
    OPAL_DECLSPEC extern const opal_paffinity_base_module_1_0_0_t 
        *opal_paffinity_base_module;

    /**
     * Indicator as to whether the list of opened paffinity components
     * is valid or not.
     */
    OPAL_DECLSPEC extern bool opal_paffinity_base_components_opened_valid;
    /**
     * List of all opened components; created when the paffinity
     * framework is initialized and destroyed when we reduce the list
     * to all available paffinity components.
     */
    OPAL_DECLSPEC extern opal_list_t opal_paffinity_base_components_opened;

    /**
     * Debugging output stream
     */
    extern int opal_paffinity_base_output;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OPAL_BASE_PAFFINITY_H */
