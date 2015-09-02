/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/util/output.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"
#include "opal/mca/pmix/pmix_types.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/runtime/orte_data_server.h"

/* define an object to hold data */
typedef struct {
    /* base object */
    opal_object_t super;
    /* index of this object in the storage array */
    orte_std_cntr_t index;
    /* process that owns this data - only the
    * owner can remove it
    */
    orte_process_name_t owner;
    /* uid of the owner - helps control
     * access rights */
    uint32_t uid;
    /* characteristics */
    opal_pmix_data_range_t range;
    opal_pmix_persistence_t persistence;
    /* and the values themselves */
    opal_list_t values;
    /* the value itself */
} orte_data_object_t;

static void construct(orte_data_object_t *ptr)
{
    ptr->index = -1;
    OBJ_CONSTRUCT(&ptr->values, opal_list_t);
}

static void destruct(orte_data_object_t *ptr)
{
    OPAL_LIST_DESTRUCT(&ptr->values);
}

OBJ_CLASS_INSTANCE(orte_data_object_t,
                   opal_object_t,
                   construct, destruct);

/* local globals */
static opal_pointer_array_t orte_data_server_store;

int orte_data_server_init(void)
{
    int rc;

    OBJ_CONSTRUCT(&orte_data_server_store, opal_pointer_array_t);
    if (ORTE_SUCCESS != (rc = opal_pointer_array_init(&orte_data_server_store,
                                                      1,
                                                      INT_MAX,
                                                      1))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_DATA_SERVER,
                            ORTE_RML_PERSISTENT,
                            orte_data_server,
                            NULL);

    return ORTE_SUCCESS;
}

void orte_data_server_finalize(void)
{
    orte_std_cntr_t i;
    orte_data_object_t *data;

    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DATA_SERVER);

    for (i=0; i < orte_data_server_store.size; i++) {
        if (NULL != (data = (orte_data_object_t*)opal_pointer_array_get_item(&orte_data_server_store, i))) {
            OBJ_RELEASE(data);
        }
    }
    OBJ_DESTRUCT(&orte_data_server_store);
}

void orte_data_server(int status, orte_process_name_t* sender,
                      opal_buffer_t* buffer, orte_rml_tag_t tag,
                      void* cbdata)
{
    uint8_t command;
    orte_std_cntr_t count;
    opal_process_name_t requestor;
    orte_data_object_t *data;
    opal_buffer_t *answer;
    int rc, ret, k;
    opal_value_t *iptr, *inext;
    uint32_t ninfo, i;
    char **keys = NULL, *str;
    bool ret_packed = false;
    int room_number;
    uint32_t uid;
    opal_pmix_data_range_t range;

    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s data server got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* unpack the room number of the caller's request */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &room_number, &count, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the command */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &command, &count, OPAL_UINT8))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    answer = OBJ_NEW(opal_buffer_t);
    /* pack the room number as this must lead any response */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &room_number, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return;
    }

    switch(command) {
    case ORTE_PMIX_PUBLISH_CMD:
        data = OBJ_NEW(orte_data_object_t);
        /* unpack the requestor */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &data->owner, &count, OPAL_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(data);
            goto SEND_ERROR;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: publishing data from %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&data->owner)));

        /* unpack the range */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &data->range, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(data);
            goto SEND_ERROR;
        }
        /* unpack the persistence */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &data->persistence, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(data);
            goto SEND_ERROR;
        }

        /* unpack the number of info elements */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ninfo, &count, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(data);
            goto SEND_ERROR;
        }

        if (0 < ninfo) {
            for (i=0; i < ninfo; i++) {
                count = 1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &iptr, &count, OPAL_VALUE))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(data);
                    goto SEND_ERROR;
                }
                /* if this is the userid, separate it out */
                if (0 == strcmp(iptr->key, OPAL_PMIX_USERID)) {
                    data->uid = iptr->data.uint32;
                    OBJ_RELEASE(iptr);
                } else {
                    opal_list_append(&data->values, &iptr->super);
                }
            }
        }

        data->index = opal_pointer_array_add(&orte_data_server_store, data);

        /* tell the user it was wonderful... */
        ret = ORTE_SUCCESS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            /* if we can't pack it, we probably can't pack the
             * rc value either, so just send whatever is there */
        }
        goto SEND_ANSWER;
        break;

    case ORTE_PMIX_LOOKUP_CMD:
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: lookup data from %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(sender)));

        /* unpack the range - this sets some constraints on the range of data to be considered */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &range, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }

        /* unpack the number of info elements */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ninfo, &count, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }
        if (0 < ninfo) {
            for (i=0; i < ninfo; i++) {
                count = 1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &iptr, &count, OPAL_VALUE))) {
                    ORTE_ERROR_LOG(rc);
                    goto SEND_ERROR;
                }
                /* if this is the userid, separate it out */
                if (0 == strcmp(iptr->key, OPAL_PMIX_USERID)) {
                    uid = iptr->data.uint32;
                }
                /* ignore anything else for now */
                OBJ_RELEASE(iptr);
            }
        }

        /* unpack the number of keys */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ninfo, &count, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }
        if (0 == ninfo) {
            /* they forgot to send us the keys?? */
            rc = ORTE_ERR_BAD_PARAM;
            goto SEND_ERROR;
        }

        /* unpack the keys */
        for (i=0; i < ninfo; i++) {
            count = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &str, &count, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(keys);
                goto SEND_ERROR;
            }
            opal_argv_append_nosize(&keys, str);
            free(str);
        }

        /* cycle across the provided keys */
        for (i=0; NULL != keys[i]; i++) {
            /* cycle across the stored data, looking for a match */
            for (k=0; k < orte_data_server_store.size; k++) {
                data = (orte_data_object_t*)opal_pointer_array_get_item(&orte_data_server_store, k);
                if (NULL == data) {
                    continue;
                }
                /* can only access data posted by the same user id */
                if (uid != data->uid) {
                    continue;
                }
                /* if the range doesn't match, then we cannot consider it */
                if (range != data->range) {
                    continue;
                }
                /* see if we have this key */
                OPAL_LIST_FOREACH(iptr, &data->values, opal_value_t) {
                    if (0 == strcmp(iptr->key, keys[i])) {
                        /* found it - package it for return */
                        if (!ret_packed) {
                            ret = ORTE_SUCCESS;
                            if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
                                ORTE_ERROR_LOG(rc);
                                opal_argv_free(keys);
                                goto SEND_ERROR;
                            }
                            ret_packed = true;
                        }
                        if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &data->owner, 1, OPAL_NAME))) {
                            ORTE_ERROR_LOG(rc);
                            opal_argv_free(keys);
                            goto SEND_ERROR;
                        }
                        if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &iptr, 1, OPAL_VALUE))) {
                            ORTE_ERROR_LOG(rc);
                            opal_argv_free(keys);
                            goto SEND_ERROR;
                        }
                    }
                }
            }
        }
        opal_argv_free(keys);
        if (!ret_packed) {
            /* nothing was found - indicate that situation */
            rc = ORTE_ERR_NOT_FOUND;
            goto SEND_ERROR;
        }
        goto SEND_ANSWER;
        break;

    case ORTE_PMIX_UNPUBLISH_CMD:
        /* unpack the requestor */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &requestor, &count, OPAL_NAME))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: unpublish data from %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&requestor)));

        /* unpack the range - this sets some constraints on the range of data to be considered */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &range, &count, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }

        /* unpack the number of info elements */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ninfo, &count, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }
        if (0 < ninfo) {
            for (i=0; i < ninfo; i++) {
                count = 1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &iptr, &count, OPAL_VALUE))) {
                    ORTE_ERROR_LOG(rc);
                    goto SEND_ERROR;
                }
                /* if this is the userid, separate it out */
                if (0 == strcmp(iptr->key, OPAL_PMIX_USERID)) {
                    uid = iptr->data.uint32;
                }
                /* ignore anything else for now */
                OBJ_RELEASE(iptr);
            }
        }

        /* unpack the number of keys */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &ninfo, &count, OPAL_UINT32))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }
        if (0 == ninfo) {
            /* they forgot to send us the keys?? */
            rc = ORTE_ERR_BAD_PARAM;
            goto SEND_ERROR;
        }

        /* unpack the keys */
        for (i=0; i < ninfo; i++) {
            count = 1;
            if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &str, &count, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(keys);
                goto SEND_ERROR;
            }
            opal_argv_append_nosize(&keys, str);
            free(str);
        }

        /* cycle across the provided keys */
        for (i=0; NULL != keys[i]; i++) {
            /* cycle across the stored data, looking for a match */
            for (k=0; k < orte_data_server_store.size; k++) {
                data = (orte_data_object_t*)opal_pointer_array_get_item(&orte_data_server_store, k);
                if (NULL == data) {
                    continue;
                }
                /* can only access data posted by the same user id */
                if (uid != data->uid) {
                    continue;
                }
                /* can only access data posted by the same process */
                if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &data->owner, &requestor)) {
                    continue;
                }
                /* can only access data posted for the same range */
                if (range != data->range) {
                    continue;
                }
                /* see if we have this key */
                OPAL_LIST_FOREACH_SAFE(iptr, inext, &data->values, opal_value_t) {
                    if (0 == strcmp(iptr->key, keys[i])) {
                        /* found it -  delete the object from the data store */
                        opal_list_remove_item(&data->values, &iptr->super);
                        OBJ_RELEASE(iptr);
                    }
                }
                /* if all the data has been removed, then remove the object */
                if (0 == opal_list_get_size(&data->values)) {
                    opal_pointer_array_set_item(&orte_data_server_store, k, NULL);
                    OBJ_RELEASE(data);
                }
            }
        }
        opal_argv_free(keys);

        /* tell the sender this succeeded */
        ret = ORTE_SUCCESS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
        }
        goto SEND_ANSWER;
        break;

    default:
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        rc = ORTE_ERR_BAD_PARAM;
        break;
    }

 SEND_ERROR:
    /* pack the error code */
    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &rc, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
    }

 SEND_ANSWER:
    if (0 > (rc = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_DATA_CLIENT,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
    }
}


