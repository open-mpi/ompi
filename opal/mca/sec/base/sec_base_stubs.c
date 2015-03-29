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
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "opal/mca/sec/base/base.h"

static void cleanup_cred(opal_sec_cred_t *cred)
{
    if (NULL == cred) {
        return;
    }
    if (NULL != cred->method) {
        free(cred->method);
    }
    if (NULL != cred->credential) {
        free(cred->credential);
    }
}

int opal_sec_base_get_cred(char *method,
                           int dstorehandle,
                           opal_process_name_t *my_id,
                           char **payload, size_t *size)
{
    opal_sec_handle_t *hdl;
    opal_sec_cred_t cred;
    opal_buffer_t buf;
    int rc;
    
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
            if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &cred.method, 1, OPAL_STRING))) {
                OPAL_ERROR_LOG(rc);
                cleanup_cred(&cred);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
            if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, &cred.size, 1, OPAL_SIZE))) {
                OPAL_ERROR_LOG(rc);
                cleanup_cred(&cred);
                OBJ_DESTRUCT(&buf);
                return rc;
            }
            if (0 < cred.size) {
                if (OPAL_SUCCESS != (rc = opal_dss.pack(&buf, cred.credential, cred.size, OPAL_BYTE))) {
                    OPAL_ERROR_LOG(rc);
                    cleanup_cred(&cred);
                    OBJ_DESTRUCT(&buf);
                    return rc;
                }
            }
            opal_output_verbose(5, opal_sec_base_framework.framework_output,
                                "opal_sec: Created credential %s of size %lu",
                                cred.credential, (unsigned long)cred.size);
            cleanup_cred(&cred);
        }
    }
    if (0 == buf.bytes_used) {
        OBJ_DESTRUCT(&buf);
        return OPAL_ERROR;
    }
    *payload = buf.base_ptr;
    *size = buf.bytes_used;
    buf.base_ptr = NULL;
    buf.bytes_used = 0;
    OBJ_DESTRUCT(&buf);
    return OPAL_SUCCESS;
}


int opal_sec_base_validate(char *payload, size_t size, char **method)
{
    opal_sec_handle_t *hdl;
    opal_buffer_t buf;
    int cnt, rc;
    opal_sec_cred_t cred;
    
    opal_output_verbose(5, opal_sec_base_framework.framework_output,
                        "opal_sec: Received credential of size %lu",
                        (unsigned long)size);
    
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, payload, size);
    
    cnt = 1;
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &cred.method, &cnt, OPAL_STRING))) {
        opal_output_verbose(5, opal_sec_base_framework.framework_output,
                            "Received credential from source %s", cred.method);
        cnt=1;
        if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, &cred.size, &cnt, OPAL_SIZE))) {
            OPAL_ERROR_LOG(rc);
            cleanup_cred(&cred);
            goto done;
        }
        opal_output_verbose(5, opal_sec_base_framework.framework_output,
                            "Received credential of size %lu", (unsigned long)cred.size);
        if (0 < cred.size) {
            cred.credential = (char*)malloc(cred.size);
            cnt=cred.size;
            if (OPAL_SUCCESS != (rc = opal_dss.unpack(&buf, cred.credential, &cnt, OPAL_BYTE))) {
                OPAL_ERROR_LOG(rc);
                cleanup_cred(&cred);
                goto done;
            }
            opal_output_verbose(5, opal_sec_base_framework.framework_output,
                                "Received credential %s", cred.credential);
        }
        OPAL_LIST_FOREACH(hdl, &opal_sec_base_actives, opal_sec_handle_t) {
            if (NULL != cred.method &&
                0 != strcmp(cred.method, hdl->component->mca_component_name)) {
                continue;
            }
            if (OPAL_SUCCESS == hdl->module->authenticate(&cred)) {
                rc = OPAL_SUCCESS;
                /* record the method */
                if (NULL != method) {
                    if (NULL != *method) {
                        free(*method);
                    }
                    *method = strdup(cred.method);
                }
                cleanup_cred(&cred);
                goto done;
            }
        }
        cleanup_cred(&cred);
        cnt = 1;
    }
    /* if we get here, then nothing authenticated */
    rc = OPAL_ERR_AUTHENTICATION_FAILED;
    
 done:
    buf.base_ptr = NULL;
    OBJ_DESTRUCT(&buf);
    return rc;
}


