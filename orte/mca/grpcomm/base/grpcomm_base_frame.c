/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"

#include "orte/mca/grpcomm/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/grpcomm/base/static-components.h"

/*
 * Global variables
 */
orte_grpcomm_base_t orte_grpcomm_base;

orte_grpcomm_base_module_t orte_grpcomm = {0};

static bool recv_issued = false;

static int orte_grpcomm_base_close(void)
{
    orte_grpcomm_base_active_t *active;

    if (recv_issued) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_XCAST);
        recv_issued = false;
    }

    /* Close the active modules */
    OPAL_LIST_FOREACH(active, &orte_grpcomm_base.actives, orte_grpcomm_base_active_t) {
        if (NULL != active->module->finalize) {
            active->module->finalize();
        }
    }
    OPAL_LIST_DESTRUCT(&orte_grpcomm_base.actives);

    return mca_base_framework_components_close(&orte_grpcomm_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int orte_grpcomm_base_open(mca_base_open_flag_t flags)
{
    OBJ_CONSTRUCT(&orte_grpcomm_base.actives, opal_list_t);

    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_DAEMON) {
        orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                ORTE_RML_TAG_XCAST,
                                ORTE_RML_PERSISTENT,
                                orte_grpcomm_base_xcast_recv, NULL);
        recv_issued = true;
    }
    return mca_base_framework_components_open(&orte_grpcomm_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, grpcomm, NULL, NULL, orte_grpcomm_base_open, orte_grpcomm_base_close,
                           mca_grpcomm_base_static_components, 0);

OBJ_CLASS_INSTANCE(orte_grpcomm_base_active_t,
                   opal_list_item_t,
                   NULL, NULL);
