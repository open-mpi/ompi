/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef ORCA_STEMS_BASE_H
#define ORCA_STEMS_BASE_H

#include "orca_config.h"
#include "orca/mca/stems/stems.h"

/*
 * Global functions for MCA overall STEMS
 */

BEGIN_C_DECLS

    /**
     * Initialize the STEMS MCA framework
     *
     * @retval OPAL_SUCCESS Upon success
     * @retval OPAL_ERROR   Upon failures
     * 
     * This function is invoked during orca_init();
     */
    ORCA_DECLSPEC int orca_stems_base_open(void);
    
    /**
     * Select an available component.
     *
     * @retval OPAL_SUCCESS Upon Success
     * @retval OPAL_NOT_FOUND If no component can be selected
     * @retval OPAL_ERROR Upon other failure
     *
     */
    ORCA_DECLSPEC int orca_stems_base_select(void);
    
    /**
     * Finalize the STEMS MCA framework
     *
     * @retval OPAL_SUCCESS Upon success
     * @retval OPAL_ERROR   Upon failures
     * 
     * This function is invoked during orca_finalize();
     */
    ORCA_DECLSPEC int orca_stems_base_close(void);

    /**
     * Globals
     */
    ORCA_DECLSPEC extern int  orca_stems_base_output;
    ORCA_DECLSPEC extern opal_list_t orca_stems_base_components_available;
    ORCA_DECLSPEC extern orca_stems_base_component_t orca_stems_base_selected_component;

ORCA_DECLSPEC int orca_stems_base_notifier_show_help(const char *filename,
                                                               const char *topic, 
                                                               bool want_error_header,
                                                               va_list arglist);

END_C_DECLS

#endif /* ORCA_STEMS_BASE_H */
