/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
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

#include "opal/mca/db/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * dbments and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "opal/mca/db/base/static-components.h"

opal_db_base_module_t opal_db = {
    NULL,
    NULL,
    opal_db_base_store,
    opal_db_base_store_pointer,
    opal_db_base_fetch,
    opal_db_base_fetch_pointer,
    opal_db_base_fetch_multiple,
    opal_db_base_remove_data,
    opal_db_base_add_log
};
opal_db_base_t opal_db_base;

static int opal_db_base_close(void)
{
    if (NULL != opal_db.finalize) {
        opal_db.finalize();
    }

    return mca_base_framework_components_close(&opal_db_base_framework, NULL);
}

static int opal_db_base_open(mca_base_open_flag_t flags)
{
    OBJ_CONSTRUCT(&opal_db_base.fetch_order, opal_list_t);
    OBJ_CONSTRUCT(&opal_db_base.store_order, opal_list_t);

    /* Open up all available components */
    return mca_base_framework_components_open(&opal_db_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(opal, db, NULL, NULL, opal_db_base_open, opal_db_base_close,
                           mca_db_base_static_components, 0);

OBJ_CLASS_INSTANCE(opal_db_active_module_t,
                   opal_list_item_t,
                   NULL, NULL);
