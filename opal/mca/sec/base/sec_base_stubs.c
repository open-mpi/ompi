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
                           char **payload, size_t *size)
{
    opal_sec_handle_t *hdl;
    opal_sec_cred_t cred;
    opal_buffer_t buf;
    
    opal_output_verbose(5, opal_sec_base_framework.framework_output,
                        "Requesting credential from source %s",
                        (NULL == method) ? "ANY" : method);

    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    OPAL_LIST_FOREACH(hdl, &opal_sec_base_actives, opal_sec_handle_t) {
        if (NULL != method && 0 != strcmp(method, hdl->component->mca_component_name)) {
            continue;
        }
        if (OPAL_SUCCESS == hdl->module->get_my_credential(dstorehandle, my_id, &cred)) {
            opal_output_verbose(5, opal_sec_base_framework.framework_output,
                                "Created credential from source %s", hdl->component->mca_component_name);
            /* pack the credential */
            if (OPAL_SUCCESS != opal_dss.pack(&buf, &cred, 1, OPAL_SEC_CRED)) {
                
            }
            /* record the source */
            (*cred)->method = strdup(hdl->component->mca_component_name);
            return OPAL_SUCCESS;
        }
    }
    return OPAL_ERROR;
}


int opal_sec_base_validate(char *payload, size_t size)
{
    opal_sec_handle_t *hdl;
    opal_buffer_t buf;
    int cnt;
    opal_sec_cred_t *cred;
    
    opal_output_verbose(5, opal_sec_base_framework.framework_output,
                        "Received credential %s from source %s",
                        (NULL == cred->credential) ? "NULL" : cred->credential,
                        (NULL == cred->method) ? "NULL" : cred->method);
    
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    buf.base_ptr = payload;
    buf.bytes_used = size;
    
    cnt = 1;
    while (OPAL_SUCCESS == opal_dss.unpack(&buf, &cred, &cnt, OPAL_SEC_CRED)) {
        OPAL_LIST_FOREACH(hdl, &opal_sec_base_actives, opal_sec_handle_t) {
            if (NULL != cred->method &&
                0 != strcmp(cred->method, hdl->component->mca_component_name)) {
                OBJ_RELEASE(cred);
                continue;
            }
            if (OPAL_SUCCESS == hdl->module->authenticate(cred)) {
                OBJ_RELEASE(cred);
                buf.base_ptr = NULL;
                OBJ_DESTRUCT(&buf);
                return OPAL_SUCCESS;
            }
        }
        cnt = 1;
    }
    buf.base_ptr = NULL;
    OBJ_DESTRUCT(&buf);
    return OPAL_ERROR;
}


