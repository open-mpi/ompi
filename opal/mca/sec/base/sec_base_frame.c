/*
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
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

#include "opal/mca/sec/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * secments and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "opal/mca/sec/base/static-components.h"

opal_sec_API_module_t opal_sec = {
    opal_sec_base_get_cred,
    opal_sec_base_validate
};
opal_list_t opal_sec_base_actives;

static int opal_sec_base_close(void)
{
    opal_sec_handle_t *hdl;
    
    /* let the selected modules finalize */
    OPAL_LIST_FOREACH(hdl, &opal_sec_base_actives, opal_sec_handle_t) {
        if (NULL != hdl->module->finalize) {
            hdl->module->finalize();
        }
    }

    OPAL_LIST_DESTRUCT(&opal_sec_base_actives);
    
    return mca_base_framework_components_close(&opal_sec_base_framework, NULL);
}

static int opal_sec_base_open(mca_base_open_flag_t flags)
{
    OBJ_CONSTRUCT(&opal_sec_base_actives, opal_list_t);
    
    /* Open up all available components */
    return mca_base_framework_components_open(&opal_sec_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(opal, sec, NULL, NULL, opal_sec_base_open, opal_sec_base_close,
                           mca_sec_base_static_components, 0);

static void hcon(opal_sec_handle_t *p)
{
    p->pri = 0;
    p->module = NULL;
    p->component = NULL;
}
OBJ_CLASS_INSTANCE(opal_sec_handle_t,
                   opal_list_item_t,
                   hcon, NULL);
