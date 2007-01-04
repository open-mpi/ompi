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
 */

#include "orte_config.h"

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/mca/mca.h"

#include "orte/mca/schema/schema_types.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/ns_private.h"

/*
 * "not available" functions
 */
int
orte_ns_base_create_my_name_not_available(void)
{
    ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
    return ORTE_ERR_UNREACH;
}

/****    NAME STRING FUNCTIONS    ****/

int orte_ns_base_get_proc_name_string(char **name_string,
                                      const orte_process_name_t* name)
{
    char *tmp, *tmp2;
    
    if (NULL == name) { /* got an error */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* check for wildcard and invalid values - where encountered, insert the
     * corresponding string so we can correctly parse the name string when
     * it is passed back to us later
     */
    if (ORTE_CELLID_WILDCARD == name->cellid) {
        tmp = strdup(ORTE_SCHEMA_WILDCARD_STRING);
    } else if (ORTE_CELLID_INVALID == name->cellid) {
        tmp = strdup(ORTE_SCHEMA_INVALID_STRING);
    } else {
        asprintf(&tmp, "%ld", (long)name->cellid);
    }

    if (ORTE_JOBID_WILDCARD == name->jobid) {
        asprintf(&tmp2, "%s%c%s", tmp, ORTE_SCHEMA_DELIMITER_CHAR, ORTE_SCHEMA_WILDCARD_STRING);
    } else if (ORTE_JOBID_INVALID == name->jobid) {
        asprintf(&tmp2, "%s%c%s", tmp, ORTE_SCHEMA_DELIMITER_CHAR, ORTE_SCHEMA_INVALID_STRING);
    } else {
        asprintf(&tmp2, "%s%c%ld", tmp, ORTE_SCHEMA_DELIMITER_CHAR, (long)name->jobid);
    }
    free(tmp);

    if (ORTE_VPID_WILDCARD == name->vpid) {
        asprintf(name_string, "%s%c%s", tmp2, ORTE_SCHEMA_DELIMITER_CHAR, ORTE_SCHEMA_WILDCARD_STRING);
    } else if (ORTE_VPID_INVALID == name->vpid) {
        asprintf(name_string, "%s%c%s", tmp2, ORTE_SCHEMA_DELIMITER_CHAR, ORTE_SCHEMA_INVALID_STRING);
    } else {
        asprintf(name_string, "%s%c%ld", tmp2, ORTE_SCHEMA_DELIMITER_CHAR, (long)name->vpid);
    }
    free(tmp2);

    return ORTE_SUCCESS;
}

int orte_ns_base_convert_string_to_process_name(orte_process_name_t **name,
                                                const char* name_string)
{
    char *temp, *token;
    orte_cellid_t cell;
    orte_jobid_t job;
    orte_vpid_t vpid;
    long int tmpint;
    int return_code=ORTE_SUCCESS;
    
    /* check for NULL string - error */
    if (NULL == name_string) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    temp = strdup(name_string);  /** copy input string as the strtok process is destructive */
    token = strtok(temp, ORTE_SCHEMA_DELIMITER_STRING); /** get first field -> cellid */
    
    /* check for error */
    if (NULL == token) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* convert to largest possible int - then
     * check to ensure it is within range of cellid_t before casting
     */
    
    /* first, though, check for WILDCARD character - assign
     * value accordingly, if found
     */
    if (0 == strcmp(token, ORTE_SCHEMA_WILDCARD_STRING)) {
        cell = ORTE_CELLID_WILDCARD;
    } else if (0 == strcmp(token, ORTE_SCHEMA_INVALID_STRING)) {
        cell = ORTE_CELLID_INVALID;
    } else {
        tmpint = strtol(token, NULL, 10);
        if (ORTE_CELLID_MAX >= tmpint && ORTE_CELLID_MIN <= tmpint) {
            cell = (orte_cellid_t)tmpint;
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return_code = ORTE_ERR_BAD_PARAM;
            goto CLEANUP;
        }
    }
    
    token = strtok(NULL, ORTE_SCHEMA_DELIMITER_STRING); /** get next field -> jobid */
    
    /** convert to largest possible int - then
     * check to ensure it is within range of jobid_t before casting */
    
    /* check for error */
    if (NULL == token) {
        return ORTE_ERR_BAD_PARAM;
    }

    /** first, though, check for WILDCARD character - assign
     * value accordingly, if found
     */
    if (0 == strcmp(token, ORTE_SCHEMA_WILDCARD_STRING)) {
        job = ORTE_JOBID_WILDCARD;
    } else if (0 == strcmp(token, ORTE_SCHEMA_INVALID_STRING)) {
        job = ORTE_JOBID_INVALID;
    } else {
        tmpint = strtol(token, NULL, 10);
        if (ORTE_JOBID_MAX >= tmpint && ORTE_JOBID_MIN <= tmpint) {
            job = (orte_jobid_t)tmpint;
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return_code = ORTE_ERR_BAD_PARAM;
            goto CLEANUP;
        }
    }
        
    token = strtok(NULL, ORTE_SCHEMA_DELIMITER_STRING);  /** get next field -> vpid */
    
    /* check for error */
    if (NULL == token) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /** convert to largest possible int then
     * check to ensure it is within range of vpid_t before casting */
    
    /** first, though, check for WILDCARD character - assign
    * value accordingly, if found
    */
    if (0 == strcmp(token, ORTE_SCHEMA_WILDCARD_STRING)) {
        vpid = ORTE_VPID_WILDCARD;
    } else if (0 == strcmp(token, ORTE_SCHEMA_INVALID_STRING)) {
        vpid = ORTE_VPID_INVALID;
    } else {
        tmpint = strtol(token, NULL, 10);
        if (ORTE_VPID_MAX >= tmpint && ORTE_VPID_MIN <= tmpint) {
            vpid = (orte_vpid_t)tmpint;
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            return_code = ORTE_ERR_BAD_PARAM;
            goto CLEANUP;
        }
    }
    
    if (ORTE_SUCCESS != (return_code =
                         orte_ns_base_create_process_name(name, cell, job, vpid))) {
        ORTE_ERROR_LOG(return_code);
    }
        
CLEANUP:
    free(temp);
        
    return return_code;
}

/****    CREATE PROCESS NAME    ****/
int orte_ns_base_create_process_name(orte_process_name_t **name,
                                  orte_cellid_t cell,
                                  orte_jobid_t job,
                                  orte_vpid_t vpid)
{
    *name = NULL;

    *name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    if (NULL == *name) { /* got an error */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    (*name)->cellid = cell;
    (*name)->jobid = job;
    (*name)->vpid = vpid;
    return ORTE_SUCCESS;
}


/****    VPID STRING FUNCTIONS    ****/
int orte_ns_base_get_vpid_string(char **vpid_string, const orte_process_name_t* name)
{
    if (NULL == name) { /* got an error */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        *vpid_string = NULL;
        return ORTE_ERR_BAD_PARAM;
    }

    /* check for wildcard value - handle appropriately */
    if (ORTE_VPID_WILDCARD == name->vpid) {
        *vpid_string = strdup(ORTE_SCHEMA_WILDCARD_STRING);
        return ORTE_SUCCESS;
    }

    /* check for invalid value - handle appropriately */
    if (ORTE_VPID_INVALID == name->vpid) {
        *vpid_string = strdup(ORTE_SCHEMA_INVALID_STRING);
        return ORTE_SUCCESS;
    }

    if (0 > asprintf(vpid_string, "%ld", (long) name->vpid)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_vpid_to_string(char **vpid_string, const orte_vpid_t vpid)
{
    /* check for wildcard value - handle appropriately */
    if (ORTE_VPID_WILDCARD == vpid) {
        *vpid_string = strdup(ORTE_SCHEMA_WILDCARD_STRING);
        return ORTE_SUCCESS;
    }
    
    /* check for invalid value - handle appropriately */
    if (ORTE_VPID_INVALID == vpid) {
        *vpid_string = strdup(ORTE_SCHEMA_INVALID_STRING);
        return ORTE_SUCCESS;
    }
    
    if (0 > asprintf(vpid_string, "%ld", (long) vpid)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_string_to_vpid(orte_vpid_t *vpid, const char* vpidstring)
{
    long int tmpint;

    if (NULL == vpidstring) {  /* got an error */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        *vpid = ORTE_VPID_INVALID;
        return ORTE_ERR_BAD_PARAM;
    }

    /** check for wildcard character - handle appropriately */
    if (0 == strcmp(ORTE_SCHEMA_WILDCARD_STRING, vpidstring)) {
        *vpid = ORTE_VPID_WILDCARD;
        return ORTE_SUCCESS;
    }

    /* check for invalid value */
    if (0 == strcmp(ORTE_SCHEMA_INVALID_STRING, vpidstring)) {
        *vpid = ORTE_VPID_INVALID;
        return ORTE_SUCCESS;
    }

    tmpint = strtol(vpidstring, NULL, 10);
        
    if (ORTE_VPID_MAX >= tmpint && ORTE_VPID_MIN <= tmpint) {
        *vpid = (orte_vpid_t)tmpint;
    } else {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        *vpid = ORTE_VPID_INVALID;
        return ORTE_ERR_BAD_PARAM;
    }

    return ORTE_SUCCESS;
}

/****    COMPARE NAME FIELDS     ****/
int orte_ns_base_compare_fields(orte_ns_cmp_bitmask_t fields,
                                const orte_process_name_t* name1,
                                const orte_process_name_t* name2)
{
    /* handle the NULL pointer case */
    if (NULL == name1 && NULL == name2) {
       return ORTE_EQUAL;
    } else if (NULL == name1) {
       return ORTE_VALUE2_GREATER;
    } else if (NULL == name2) {
       return ORTE_VALUE1_GREATER;
    }

    /* in this comparison function, we check for exact equalities.
     * In the case of wildcards, we check to ensure that the fields
     * actually match those values - thus, a "wildcard" in this
     * function does not actually stand for a wildcard value, but
     * rather a specific value
     */
    if (ORTE_NS_CMP_CELLID & fields) { /* check cellid field */
        if (name1->cellid < name2->cellid) {
            return ORTE_VALUE2_GREATER;
        } else if (name1->cellid > name2->cellid) {
            return ORTE_VALUE1_GREATER;
        }
    }

    /* get here if cellid's are equal, or cellid not being checked */
    /* now check job id */

    if (ORTE_NS_CMP_JOBID & fields) {
        if (name1->jobid < name2->jobid) {
            return ORTE_VALUE2_GREATER;
        } else if (name1->jobid > name2->jobid) {
            return ORTE_VALUE1_GREATER;
        }
    }

    /* get here if cellid's and jobid's are equal, or neither being checked,
     * or cellid not checked and jobid's equal.
     * now check vpid
     */

    if (ORTE_NS_CMP_VPID & fields) {
        if (name1->vpid < name2->vpid) {
            return ORTE_VALUE2_GREATER;
        } else if (name1->vpid > name2->vpid) {
            return ORTE_VALUE1_GREATER;
        }
    }

    /* only way to get here is if all fields are being checked and are equal,
     * or cellid not checked, but jobid and vpid equal,
     * or cellid and jobid not checked, but vpid equal,
     * only vpid being checked, and equal
     * return that fact
     */
    return ORTE_EQUAL;
}
