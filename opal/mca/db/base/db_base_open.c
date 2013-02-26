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

int opal_db_base_open(void)
{
    opal_db_base.output = opal_output_open(NULL);
    
    OBJ_CONSTRUCT(&opal_db_base.available_components, opal_list_t);
    OBJ_CONSTRUCT(&opal_db_base.fetch_order, opal_list_t);
    OBJ_CONSTRUCT(&opal_db_base.store_order, opal_list_t);

    /* Open up all available components */
    if (OPAL_SUCCESS != 
        mca_base_components_open("db", opal_db_base.output, mca_db_base_static_components, 
                                 &opal_db_base.available_components,
                                 true)) {
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

OBJ_CLASS_INSTANCE(opal_db_active_module_t,
                   opal_list_item_t,
                   NULL, NULL);
