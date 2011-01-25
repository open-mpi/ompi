/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_EVENT_BASE_H
#define OPAL_EVENT_BASE_H

#include "opal_config.h"

#include "opal/mca/event/event.h"

/*
 * Global functions for MCA overall event open and close
 */

BEGIN_C_DECLS

extern int opal_event_base_inited;

/**
 * Initialize the event MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the first function invoked in the event MCA
 * framework.  It initializes the event MCA framework, finds
 * and opens event components, etc.
 *
 * This function is invoked during opal_init().
 * 
 * This function fills in the internal global variable
 * opal_event_base_components_opened, which is a list of all
 * event components that were successfully opened.  This
 * variable should \em only be used by other event base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 */
OPAL_DECLSPEC int opal_event_base_open(void);

/**
 * Select an available component.
 *
 * @return OPAL_SUCCESS Upon success.
 * @return OPAL_NOT_FOUND If no component can be selected.
 * @return OPAL_ERROR Upon other failure.
 *
 * This function invokes the selection process for event components,
 * which works as follows:
 *
 * - If the \em event MCA parameter is not specified, the
 *   selection set is all available event components.
 * - If the \em event MCA parameter is specified, the
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
 * of the event wrapper functions will return an error.
 */
OPAL_DECLSPEC int opal_event_base_select(void);

/**
 * Shut down the event MCA framework.
 *
 * @retval OPAL_SUCCESS Always
 *
 * This function shuts down everything in the event MCA
 * framework, and is called during opal_finalize().
 *
 * It must be the last function invoked on the event MCA
 * framework.
 */
OPAL_DECLSPEC int opal_event_base_close(void);

/**
 * Debugging output stream
 */
OPAL_DECLSPEC extern int opal_event_base_output;
OPAL_DECLSPEC extern opal_list_t opal_event_components;


END_C_DECLS

#endif /* OPAL_BASE_EVENT_H */
