/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rmcast/base/base.h"
#include "orte/mca/rmcast/base/private.h"

int orte_rmcast_base_close(void)
{
    opal_list_item_t *item;

    if (!orte_rmcast_base.opened) {
        return ORTE_SUCCESS;
    }
    
    /* finalize the active module */
    if (NULL != orte_rmcast.finalize) {
        orte_rmcast.finalize();
    }

    while (NULL != (item = opal_list_remove_first(&orte_rmcast_base.recvs))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_rmcast_base.recvs);
    while (NULL != (item = opal_list_remove_first(&orte_rmcast_base.channels))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_rmcast_base.channels);

    /* cleanup thread stuff */
    OBJ_DESTRUCT(&orte_rmcast_base.main_ctl);
    OBJ_DESTRUCT(&orte_rmcast_base.recv_thread);
    OBJ_DESTRUCT(&orte_rmcast_base.recv_ctl);
    OBJ_DESTRUCT(&orte_rmcast_base.recv_process);
    OBJ_DESTRUCT(&orte_rmcast_base.recv_process_ctl);

    if (orte_progress_threads_enabled) {
        opal_event_base_finalize(orte_rmcast_base.event_base);
    }

    /* Close all remaining available components (may be one if this is a
       Open RTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(orte_rmcast_base.rmcast_output, 
                              &orte_rmcast_base.rmcast_opened, NULL);
  
    orte_rmcast_base.opened = false;
    return ORTE_SUCCESS;
}
