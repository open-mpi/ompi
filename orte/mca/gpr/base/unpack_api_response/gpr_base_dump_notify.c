/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "opal/util/trace.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"
#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/util/output.h"

#include "orte/mca/gpr/base/base.h"

static void orte_gpr_base_dump_load_string(orte_buffer_t *buffer, char **tmp);

int orte_gpr_base_dump_notify_msg(orte_buffer_t *buffer,
                                  orte_gpr_notify_message_t *msg)
{
    char *tmp_out;
    int rc;

    OPAL_TRACE(3);

    asprintf(&tmp_out, "\nDUMP OF NOTIFY MESSAGE STRUCTURE");
    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    if (NULL == msg) {
        asprintf(&tmp_out, "\tNULL msg pointer");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp_out, "\t", msg, ORTE_GPR_NOTIFY_MSG))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    orte_gpr_base_dump_load_string(buffer, &tmp_out);


    return ORTE_SUCCESS;
}

int orte_gpr_base_dump_notify_data(orte_buffer_t *buffer,
                                   orte_gpr_notify_data_t *data)
{
    char *tmp_out;
    int rc;

    OPAL_TRACE(3);

    asprintf(&tmp_out, "\nDUMP OF NOTIFY DATA STRUCTURE");
    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    if (NULL == data) {
        asprintf(&tmp_out, "\tNULL data pointer");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp_out, "\t", data, ORTE_GPR_NOTIFY_DATA))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    return ORTE_SUCCESS;
}

int orte_gpr_base_dump_value(orte_buffer_t *buffer, orte_gpr_value_t *value)
{
    char *tmp_out;
    int rc;

    OPAL_TRACE(3);

    asprintf(&tmp_out, "\nDUMP OF GPR VALUE STRUCTURE");
    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    if (NULL == value) {
        asprintf(&tmp_out, "\tNULL pointer");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp_out, "", value, ORTE_GPR_VALUE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    return ORTE_SUCCESS;
}

int orte_gpr_base_dump_keyval_value(orte_buffer_t *buffer, orte_gpr_keyval_t *iptr)
{
    char *tmp_out;
    int rc;

    asprintf(&tmp_out, "\nDUMP OF GPR KEYVAL STRUCTURE");
    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    if (NULL == iptr) {
        asprintf(&tmp_out, "\tNULL pointer");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (rc = orte_dss.print(&tmp_out, "", iptr, ORTE_GPR_KEYVAL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    return ORTE_SUCCESS;
}


static void orte_gpr_base_dump_load_string(orte_buffer_t *buffer, char **tmp)
{
    orte_dss.pack(buffer, tmp, 1, ORTE_STRING);
    free(*tmp);

}
