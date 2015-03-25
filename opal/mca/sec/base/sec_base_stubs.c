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

int opal_sec_base_get_cred(char *method,
                           int dstorehandle,
                           opal_process_name_t *my_id,
                           opal_sec_cred_t **cred)
{
    opal_sec_handle_t *hdl;

    opal_output_verbose(5, opal_sec_base_framework.framework_output,
                        "Requesting credential from source %s",
                        (NULL == method) ? "ANY" : method);
    
    OPAL_LIST_FOREACH(hdl, &opal_sec_base_actives, opal_sec_handle_t) {
        if (NULL != method && 0 != strcmp(method, hdl->component->mca_component_name)) {
            continue;
        }
        if (OPAL_SUCCESS == hdl->module->get_my_credential(dstorehandle, my_id, cred)) {
            opal_output_verbose(5, opal_sec_base_framework.framework_output,
                                "Created credential from source %s", hdl->component->mca_component_name);
            /* record the source */
            (*cred)->method = strdup(hdl->component->mca_component_name);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


int opal_sec_base_validate(opal_sec_cred_t *cred)
{
    opal_sec_handle_t *hdl;

    opal_output_verbose(5, opal_sec_base_framework.framework_output,
                        "Received credential %s from source %s",
                        (NULL == cred->credential) ? "NULL" : cred->credential,
                        (NULL == cred->method) ? "NULL" : cred->method);
    
    OPAL_LIST_FOREACH(hdl, &opal_sec_base_actives, opal_sec_handle_t) {
        if (NULL != cred->method &&
            0 != strcmp(cred->method, hdl->component->mca_component_name)) {
            continue;
        }
        if (OPAL_SUCCESS == hdl->module->authenticate(cred)) {
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


