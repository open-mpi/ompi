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

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/smr/base/smr_private.h"

static void orte_smr_base_quick_print(char **output, char *type_name, char *prefix, void *src, size_t src_size);

/*
 * STANDARD PRINT FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_smr_base_std_print(char **output, char *prefix, void *src, orte_data_type_t type)
{
    /* set default result */
    *output = NULL;

    switch(type) {
        case ORTE_PROC_STATE:
            orte_smr_base_quick_print(output, "ORTE_PROC_STATE", prefix, src, sizeof(orte_proc_state_t));
            break;

        case ORTE_JOB_STATE:
            orte_smr_base_quick_print(output, "ORTE_JOB_STATE", prefix, src, sizeof(orte_job_state_t));
            break;

        case ORTE_NODE_STATE:
            orte_smr_base_quick_print(output, "ORTE_NODE_STATE", prefix, src, sizeof(orte_node_state_t));
            break;

        case ORTE_EXIT_CODE:
            orte_smr_base_quick_print(output, "ORTE_EXIT_CODE", prefix, src, sizeof(orte_exit_code_t));
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }

    return ORTE_SUCCESS;
}

static void orte_smr_base_quick_print(char **output, char *type_name, char *prefix, void *src, size_t src_size)
{
    uint8_t *ui8;
    uint16_t *ui16;
    uint32_t *ui32;
    uint64_t *ui64;

    switch(src_size) {
        case 1:
            ui8 = (uint8_t*)src;
            if (NULL == prefix) {
                asprintf(output, "Data type: %s\tValue: %d", type_name, (int) *ui8);
            } else {
                asprintf(output, "%sData type: %s\tValue: %d", prefix, type_name, (int) *ui8);
            }
            break;

        case 2:
            ui16 = (uint16_t*)src;
            if (NULL == prefix) {
                asprintf(output, "Data type: %s\tValue: %d", type_name, (int) *ui16);
            } else {
                asprintf(output, "%sData type: %s\tValue: %d", prefix, type_name, (int) *ui16);
            }
            break;

        case 4:
            ui32 = (uint32_t*)src;
            if (NULL == prefix) {
                asprintf(output, "Data type: %s\tValue: %lu", type_name, (unsigned long) *ui32);
            } else {
                asprintf(output, "%sData type: %s\tValue: %lu", prefix, type_name, (unsigned long) *ui32);
            }
            break;

        case 8:
            ui64 = (uint64_t*)src;
            if (NULL == prefix) {
                asprintf(output, "Data type: %s\tValue: %lu", type_name, (unsigned long) *ui64);
            } else {
                asprintf(output, "%sData type: %s\tValue: %lu", prefix, type_name, (unsigned long) *ui64);
            }
            break;

        default:
           return;
    }

    return;
}
