/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_RTE_BASE_H
#define OMPI_RTE_BASE_H

#include "ompi_config.h"

#include "opal/class/opal_list.h"

#include "ompi/mca/rte/rte.h"

/*
 * Global functions for MCA overall rte open and close
 */

BEGIN_C_DECLS

extern int ompi_rte_base_inited;

/**
 * Initialize the rte MCA framework
 *
 * @retval OMPI_SUCCESS Upon success
 * @retval OMPI_ERROR Upon failure
 *
 * This must be the first function invoked in the rte MCA
 * framework.  It initializes the rte MCA framework, finds
 * and opens rte components, etc.
 *
 * This function is invoked during ompi_init().
 * 
 * This function fills in the internal global variable
 * ompi_rte_base_components_opened, which is a list of all
 * rte components that were successfully opened.  This
 * variable should \em only be used by other rte base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 */
OMPI_DECLSPEC int ompi_rte_base_open(void);

/**
 * Select an available component.
 *
 * @return OMPI_SUCCESS Upon success.
 * @return OMPI_NOT_FOUND If no component can be selected.
 * @return OMPI_ERROR Upon other failure.
 *
 * This function invokes the selection process for rte components,
 * which works as follows:
 *
 * - If the \em rte MCA parameter is not specified, the
 *   selection set is all available rte components.
 * - If the \em rte MCA parameter is specified, the
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
 * selected, then return OMPI_NOT_FOUND (this is not a fatal
 * error).
 *
 * At the end of this process, we'll either have a single
 * component that is selected and initialized, or no component was
 * selected.  If no component was selected, subsequent invocation
 * of the rte wrapper functions will return an error.
 */
OMPI_DECLSPEC int ompi_rte_base_select(void);

/**
 * Shut down the rte MCA framework.
 *
 * @retval OMPI_SUCCESS Always
 *
 * This function shuts down everything in the rte MCA
 * framework, and is called during ompi_finalize().
 *
 * It must be the last function invoked on the rte MCA
 * framework.
 */
OMPI_DECLSPEC int ompi_rte_base_close(void);

/**
 * Debugging output stream
 */
OMPI_DECLSPEC extern int ompi_rte_base_output;
OMPI_DECLSPEC extern opal_list_t ompi_rte_components;

END_C_DECLS

#endif /* OMPI_BASE_RTE_H */
