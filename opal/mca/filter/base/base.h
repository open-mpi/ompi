/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OPAL_FILTER_BASE_H
#define OPAL_FILTER_BASE_H

#include "opal_config.h"

#include "opal/class/opal_list.h"

#include "opal/mca/filter/filter.h"

/*
 * Global functions for MCA overall CRS
 */

BEGIN_C_DECLS

/**
 * Initialize the FILTER MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR   Upon failures
 * 
 * This function is invoked during opal_init();
 */
OPAL_DECLSPEC int opal_filter_base_open(void);

/**
 * Select an available component.
 *
 * @retval OPAL_SUCCESS Upon Success
 * @retval OPAL_NOT_FOUND If no component can be selected
 * @retval OPAL_ERROR Upon other failure
 *
 */
OPAL_DECLSPEC int opal_filter_base_select(void);

/**
 * Finalize the FILTER MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR   Upon failures
 * 
 * This function is invoked during opal_finalize();
 */
OPAL_DECLSPEC int opal_filter_base_close(void);


/**** No-op base functions ****/
OPAL_DECLSPEC char* opal_filter_base_process(char *str, int major_id, int minor_id, int num_tags, char **tags);

OPAL_DECLSPEC extern int opal_filter_base_output;
OPAL_DECLSPEC extern opal_list_t opal_filter_base_components_available;
OPAL_DECLSPEC extern opal_filter_base_component_t opal_filter_base_selected_component;

END_C_DECLS

#endif /* OPAL_FILTER_BASE_H */
