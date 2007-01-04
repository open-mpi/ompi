/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "orte_config.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/ns_private.h"

static void orte_ns_base_quick_print(char **output, char *type_name, char *pfx, void *src, size_t src_size);

/*
 * STANDARD PRINT FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_ns_base_std_print(char **output, char *prefix, void *src, orte_data_type_t type)
{
    /* set default result */
    *output = NULL;

    switch(type) {
        case ORTE_VPID:
            orte_ns_base_quick_print(output, "ORTE_VPID", prefix, src, sizeof(orte_vpid_t));
            break;

        case ORTE_JOBID:
            orte_ns_base_quick_print(output, "ORTE_JOBID", prefix, src, sizeof(orte_jobid_t));
            break;

        case ORTE_CELLID:
            orte_ns_base_quick_print(output, "ORTE_CELLID", prefix, src, sizeof(orte_cellid_t));
            break;

        case ORTE_NODEID:
            orte_ns_base_quick_print(output, "ORTE_NODEID", prefix, src, sizeof(orte_nodeid_t));
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    return ORTE_SUCCESS;
}

/*
 * NAME
 */
int orte_ns_base_print_name(char **output, char *prefix, orte_process_name_t *name, orte_data_type_t type)
{
    /* set default result */
    *output = NULL;

    if (NULL == name) {
        asprintf(output, "%sData type: ORTE_PROCESS_NAME\tData Value: NULL",
                 (NULL == prefix ? " " : prefix));
    } else {
        asprintf(output, "%sData type: ORTE_PROCESS_NAME\tData Value: [%ld,%ld,%ld]",
             (NULL == prefix ? " " : prefix), (long)name->cellid,
             (long)name->jobid, (long)name->vpid);
    }

    return ORTE_SUCCESS;
}


static void orte_ns_base_quick_print(char **output, char *type_name, char *prefix, void *src, size_t src_size)
{
    int8_t *i8;
    int16_t *i16;
    int32_t *i32;
    int64_t *i64;
    char *pfx;

    /* set default result */
    *output = NULL;

    /* protect against NULL pfx */
    if (NULL == prefix) {
        asprintf(&pfx, " ");
    } else {
        asprintf(&pfx, "%s", prefix);
    }


    switch(src_size) {
        case 1:
            i8 = (int8_t*)src;
            asprintf(output, "%sData type: %s\tValue: %d", pfx, type_name, (int) *i8);
            break;

        case 2:
            i16 = (int16_t*)src;
            asprintf(output, "%sData type: %s\tValue: %d", pfx, type_name, (int) *i16);
            break;

        case 4:
            i32 = (int32_t*)src;
            asprintf(output, "%sData type: %s\tValue: %ld", pfx, type_name, (long) *i32);
            break;

        case 8:
            i64 = (int64_t*)src;
            asprintf(output, "%sData type: %s\tValue: %ld", pfx, type_name, (long) *i64);
            break;

        default:
            return;
    }

    free(pfx);
    return;
}
