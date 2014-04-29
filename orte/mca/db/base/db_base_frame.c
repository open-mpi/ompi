/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
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
#include "opal/dss/dss_types.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/db/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * dbments and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/db/base/static-components.h"

orte_db_API_module_t orte_db = {
    orte_db_base_open,
    orte_db_base_close,
    orte_db_base_store,
    orte_db_base_commit,
    orte_db_base_fetch,
    orte_db_base_remove_data
};
orte_db_base_t orte_db_base;

static bool orte_db_base_create_evbase;

static int orte_db_base_register(mca_base_register_flag_t flags)
{
    orte_db_base_create_evbase = false;
    mca_base_var_register("orte", "db", "base", "create_evbase",
                          "Create a separate event base for processing db operations",
                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                          OPAL_INFO_LVL_9,
                          MCA_BASE_VAR_SCOPE_READONLY,
                          &orte_db_base_create_evbase);
    return OPAL_SUCCESS;
}

static int orte_db_base_frame_close(void)
{
    orte_db_base_component_t *component;
    int i;
    orte_db_handle_t *hdl;

    /* cleanup the globals */
    for (i=0; i < orte_db_base.handles.size; i++) {
        if (NULL != (hdl = (orte_db_handle_t*)opal_pointer_array_get_item(&orte_db_base.handles, i))) {
            OBJ_RELEASE(hdl);
        }
    }
    OBJ_DESTRUCT(&orte_db_base.handles);

    /* cycle across all the active db components and let them cleanup - order
     * doesn't matter in this case
     */
    while (NULL != (component = (orte_db_base_component_t*)opal_list_remove_first(&orte_db_base.actives))) {
        if (NULL != component->finalize) {
            component->finalize();
        }
    }
    OBJ_DESTRUCT(&orte_db_base.actives);

    return mca_base_framework_components_close(&orte_db_base_framework, NULL);
}

static int orte_db_base_frame_open(mca_base_open_flag_t flags)
{
    OBJ_CONSTRUCT(&orte_db_base.actives, opal_list_t);
    OBJ_CONSTRUCT(&orte_db_base.handles, opal_pointer_array_t);
    opal_pointer_array_init(&orte_db_base.handles, 3, INT_MAX, 1);

    if (orte_db_base_create_evbase) {
        /* create our own event base */
        /* spin off a progress thread for it */
    } else {
        /* tie us to the orte_event_base */
        orte_db_base.ev_base = orte_event_base;
    }

    /* Open up all available components */
    return mca_base_framework_components_open(&orte_db_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, db, "ORTE Database Framework",
                           orte_db_base_register,
                           orte_db_base_frame_open,
                           orte_db_base_frame_close,
                           mca_db_base_static_components, 0);

static void req_con(orte_db_request_t *p)
{
    p->properties = NULL;
    p->primary_key = NULL;
    p->key = NULL;
    p->kvs = NULL;
}
OBJ_CLASS_INSTANCE(orte_db_request_t,
                   opal_object_t,
                   req_con, NULL);

OBJ_CLASS_INSTANCE(orte_db_handle_t,
                   opal_object_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(orte_db_base_active_component_t,
                   opal_list_item_t,
                   NULL, NULL);
