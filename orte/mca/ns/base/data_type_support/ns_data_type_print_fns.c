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

#include "orte/mca/ns/base/base.h"

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
        asprintf(output, "%sData type: ORTE_PROCESS_NAME\tData Value: [%lu,%lu,%lu]",
             (NULL == prefix ? " " : prefix), (unsigned long)name->cellid,
             (unsigned long)name->jobid, (unsigned long)name->vpid);
    }

    return ORTE_SUCCESS;
}


static void orte_ns_base_quick_print(char **output, char *type_name, char *prefix, void *src, size_t src_size)
{
    uint8_t *ui8;
    uint16_t *ui16;
    uint32_t *ui32;
    uint64_t *ui64;
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
            ui8 = (uint8_t*)src;
            asprintf(output, "%sData type: %s\tValue: %d", pfx, type_name, (int) *ui8);
            break;

        case 2:
            ui16 = (uint16_t*)src;
            asprintf(output, "%sData type: %s\tValue: %d", pfx, type_name, (int) *ui16);
            break;

        case 4:
            ui32 = (uint32_t*)src;
            asprintf(output, "%sData type: %s\tValue: %lu", pfx, type_name, (unsigned long) *ui32);
            break;

        case 8:
            ui64 = (uint64_t*)src;
            asprintf(output, "%sData type: %s\tValue: %lu", pfx, type_name, (unsigned long) *ui64);
            break;

        default:
            return;
    }

    free(pfx);
    return;
}
