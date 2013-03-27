/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
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

/*
 * MCA Framework
 */
OMPI_DECLSPEC extern mca_base_framework_t ompi_rte_base_framework;

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

END_C_DECLS

#endif /* OMPI_BASE_RTE_H */
