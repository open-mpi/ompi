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
    /* service name */
    char *service;
    /* port */
    char *port;
} orte_data_object_t;

static void construct(orte_data_object_t *ptr)
{
    ptr->index = -1;
    ptr->service = NULL;
    ptr->port = NULL;
}

static void destruct(orte_data_object_t *ptr)
{
    if (NULL != ptr->service) free(ptr->service);
    if (NULL != ptr->port) free(ptr->port);
}

OBJ_CLASS_INSTANCE(orte_data_object_t,
                   opal_object_t,
                   construct, destruct);

/* local globals */
static opal_pointer_array_t *orte_data_server_store=NULL;
static bool recv_issued=false;

int orte_data_server_init(void)
{
    int rc;

    orte_data_server_store = OBJ_NEW(opal_pointer_array_t);
    if (ORTE_SUCCESS != (rc = opal_pointer_array_init(orte_data_server_store,
                                                      1,
                                                      INT_MAX,
                                                      1))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (!recv_issued) {
        orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                ORTE_RML_TAG_DATA_SERVER,
                                ORTE_RML_PERSISTENT,
                                orte_data_server,
                                NULL);
        recv_issued = true;
    }

    return ORTE_SUCCESS;
}

void orte_data_server_finalize(void)
{
    orte_std_cntr_t i;
    orte_data_object_t **data;

    if (NULL != orte_data_server_store) {
        data = (orte_data_object_t**)orte_data_server_store->addr;
        for (i=0; i < orte_data_server_store->size; i++) {
            if (NULL != data[i]) OBJ_RELEASE(data[i]);
        }
        OBJ_RELEASE(orte_data_server_store);
    }

    if (recv_issued) {
        orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DATA_SERVER);
        recv_issued = false;
    }
}

static orte_data_object_t *lookup(char *service)
{
    orte_std_cntr_t i;
    orte_data_object_t **data;

    data = (orte_data_object_t**)orte_data_server_store->addr;
    for (i=0; i < orte_data_server_store->size; i++) {
        if (NULL != data[i]) {
            if (0 == strcmp(data[i]->service, service)) {
                return data[i];
            }
        }
    }

    /* get here if not found - return NULL */
    return NULL;
}

static void rml_cbfunc(int status, orte_process_name_t* sender,
                       opal_buffer_t* buffer, orte_rml_tag_t tag,
                       void* cbdata)
{
    OBJ_RELEASE(buffer);
}

void orte_data_server(int status, orte_process_name_t* sender,
                      opal_buffer_t* buffer, orte_rml_tag_t tag,
                      void* cbdata)
{
    orte_data_server_cmd_t command;
    orte_std_cntr_t count;
    char *service_name, *port_name;
    orte_data_object_t *data;
    opal_buffer_t *answer;
    int rc, ret;
    bool unique;

    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s data server got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &command, &count, ORTE_DATA_SERVER_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    answer = OBJ_NEW(opal_buffer_t);

    switch(command) {
    case ORTE_DATA_SERVER_PUBLISH:
        /* unpack the service name */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &service_name, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }

        /* unpack the port name */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &port_name, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }

        /* unpack uniqueness flag */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &unique, &count, OPAL_BOOL))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: publishing service %s port %s %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             service_name, port_name,
                             unique ? "UNIQUE" : "OVERWRITE"));

        /* check the current data store to see if this service name has already
         * been published
         */
        if (NULL != (data = lookup(service_name))) {
            /* already exists - see if overwrite allowed */
            if (unique) {
                /* return ORTE_EXISTS error code */

                OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                     "%s data server: publishing service %s port %s already exists",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     service_name, port_name));

                ret = ORTE_EXISTS;
            } else {
                OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                     "%s data server: overwriting service %s with port %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     service_name, port_name));
                if (NULL != data->port) {
                    free(data->port);
                }
                data->port = port_name;
                data->owner.jobid = sender->jobid;
                data->owner.vpid = sender->vpid;
                ret = ORTE_SUCCESS;
            }
            if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
                ORTE_ERROR_LOG(rc);
                /* if we can't pack it, we probably can't pack the
                 * rc value either, so just send whatever is there
                 */
            }
            goto SEND_ANSWER;

        }

        /* create a new data object */
        data = OBJ_NEW(orte_data_object_t);

        /* pass over the data values - these were malloc'd when unpacked,
         * so we don't need to strdup them here
         */
        data->service = service_name;
        data->port = port_name;
        data->owner.jobid = sender->jobid;
        data->owner.vpid = sender->vpid;

        /* store the data */
        data->index = opal_pointer_array_add(orte_data_server_store, data);

        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: successfully published service %s port %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             service_name, port_name));

        /* tell the user it was wonderful... */
        ret = ORTE_SUCCESS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            /* if we can't pack it, we probably can't pack the
             * rc value either, so just send whatever is there
             */
        }
        goto SEND_ANSWER;
        break;

    case ORTE_DATA_SERVER_LOOKUP:
        /* unpack the service name */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &service_name, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: lookup on service %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             service_name));

        /* locate this record in the data store */
        if (NULL == (data = lookup(service_name))) {

            OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                 "%s data server: service %s not found",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 service_name));

            /* return ORTE_ERR_NOT_FOUND error code */
            ret = ORTE_ERR_NOT_FOUND;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
                ORTE_ERROR_LOG(rc);
                /* if we can't pack it, we probably can't pack the
                 * rc value either, so just send whatever is there
                 */
            }
            goto SEND_ANSWER;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: successful lookup on service %s port %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             service_name, data->port));

        /* pack success so the unpack on the other end can
         * always unpack an int first
         */
        ret = ORTE_SUCCESS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            /* if we can't pack it, we probably can't pack the
             * rc value either, so just send whatever is there
             */
            goto SEND_ANSWER;
        }

        /* pack the returned port */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &data->port, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            /* if we can't pack it, we probably can't pack the
             * rc value either, so just send whatever is there
             */
        }
        goto SEND_ANSWER;
        break;

    case ORTE_DATA_SERVER_UNPUBLISH:
        /* unpack the service name */
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &service_name, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto SEND_ERROR;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: unpublish on service %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             service_name));

        /* locate this record in the data store */
        if (NULL == (data = lookup(service_name))) {

            OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                 "%s data server: service %s not found",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 service_name));

            /* return ORTE_ERR_NOT_FOUND error code */
            ret = ORTE_ERR_NOT_FOUND;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
                ORTE_ERROR_LOG(rc);
                /* if we can't pack it, we probably can't pack the
                 * rc value either, so just send whatever is there
                 */
            }
            goto SEND_ANSWER;
        }

        /* check to see if the sender owns it - must be exact match */
        if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                        &data->owner, sender)) {

            OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                 "%s data server: service %s not owned by sender %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 service_name, ORTE_NAME_PRINT(sender)));

            /* nope - return ORTE_ERR_PERM error code */
            ret = ORTE_ERR_PERM;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
                ORTE_ERROR_LOG(rc);
                /* if we can't pack it, we probably can't pack the
                 * rc value either, so just send whatever is there
                 */
            }
            goto SEND_ANSWER;
        }

        /* delete the object from the data store */
        opal_pointer_array_set_item(orte_data_server_store, data->index, NULL);
        OBJ_RELEASE(data);

        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s data server: service %s unpublished",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             service_name));

        /* tell the sender this succeeded */
        ret = ORTE_SUCCESS;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(answer, &ret, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            /* if we can't pack it, we probably can't pack the
             * rc value either, so just send whatever is there
             */
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
    if (0 > (rc = orte_rml.send_buffer_nb(sender, answer, ORTE_RML_TAG_DATA_CLIENT, rml_cbfunc, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
    }
}


