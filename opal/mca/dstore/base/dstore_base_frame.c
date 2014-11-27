/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "opal/mca/dstore/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * dstorements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "opal/mca/dstore/base/static-components.h"

opal_dstore_base_API_t opal_dstore = {
    opal_dstore_base_open,
    opal_dstore_base_update,
    opal_dstore_base_close,
    opal_dstore_base_store,
    opal_dstore_base_fetch,
    opal_dstore_base_remove_data,
    opal_dstore_base_get_handle
};
opal_dstore_base_t opal_dstore_base;

int opal_dstore_internal = -1;
int opal_dstore_modex = -1;

static int opal_dstore_base_frame_close(void)
{
    opal_dstore_handle_t *hdl;
    opal_list_item_t *item;
    int i;

    /* cycle across all the active dstore handles and let them cleanup - order
     * doesn't matter in this case
     */
    for (i=0; i < opal_dstore_base.handles.size; i++) {
        if (NULL != (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, i))) {
            OBJ_RELEASE(hdl);
        }
    }
    OBJ_DESTRUCT(&opal_dstore_base.handles);

    for (item = opal_list_remove_first(&opal_dstore_base.available_components);
            NULL != item;
            item = opal_list_remove_first(&opal_dstore_base.available_components)) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&opal_dstore_base.available_components);

    /* let the backfill module finalize, should it wish to do so */
    if (NULL != opal_dstore_base.backfill_module && NULL != opal_dstore_base.backfill_module->finalize) {
        opal_dstore_base.backfill_module->finalize((struct opal_dstore_base_module_t*)opal_dstore_base.backfill_module);
    }

    return mca_base_framework_components_close(&opal_dstore_base_framework, NULL);
}

static int opal_dstore_base_frame_open(mca_base_open_flag_t flags)
{
    OBJ_CONSTRUCT(&opal_dstore_base.handles, opal_pointer_array_t);
    opal_pointer_array_init(&opal_dstore_base.handles, 5, INT_MAX, 1);

    OBJ_CONSTRUCT(&opal_dstore_base.available_components, opal_list_t);

    /* Open up all available components */
    return mca_base_framework_components_open(&opal_dstore_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(opal, dstore, NULL, NULL,
                           opal_dstore_base_frame_open,
                           opal_dstore_base_frame_close,
                           mca_dstore_base_static_components, 0);

/***  CLASS INSTANCES   ***/
static void hdl_con(opal_dstore_handle_t *p)
{
    p->name = NULL;
    p->module = NULL;
    p->storage_component = NULL;
}
static void hdl_des(opal_dstore_handle_t *p)
{
    opal_dstore_base_module_t *mod;

    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->module) {
        mod = (opal_dstore_base_module_t*)p->module;
        if (NULL != mod->finalize) {
            mod->finalize((struct opal_dstore_base_module_t*)mod);
        }
        free(mod);
    }
}
OBJ_CLASS_INSTANCE(opal_dstore_handle_t,
                   opal_object_t,
                   hdl_con, hdl_des);

static void proc_data_construct(opal_dstore_proc_data_t *ptr)
{
    ptr->loaded = false;
    OBJ_CONSTRUCT(&ptr->data, opal_list_t);
}

static void proc_data_destruct(opal_dstore_proc_data_t *ptr)
{
    OPAL_LIST_DESTRUCT(&ptr->data);
}
OBJ_CLASS_INSTANCE(opal_dstore_proc_data_t,
                   opal_list_item_t,
                   proc_data_construct,
                   proc_data_destruct);

static void attr_construct(opal_dstore_attr_t *attr)
{
    attr->connection_info = NULL;
}

static void attr_destruct(opal_dstore_attr_t *attr)
{
    if (NULL != attr->connection_info) {
        free(attr->connection_info);
    }
}
OBJ_CLASS_INSTANCE(opal_dstore_attr_t,
                   opal_list_item_t,
                   attr_construct, attr_destruct);


