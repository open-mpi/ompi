/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "include/constants.h"
#include "util/output.h"
#include "util/printf.h"
#include "mca/mca.h"
#include "mca/ns/base/base.h"

/**
 * globals
 */


/*
 * "not available" functions
 */
int 
orte_ns_base_module_init_not_available(void)
{
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_create_cellid_not_available(orte_cellid_t *cellid)
{
    *cellid = ORTE_CELLID_MAX;
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_create_jobid_not_available(orte_jobid_t *jobid)
{
    *jobid = ORTE_JOBID_MAX;
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_get_vpid_range_not_available(orte_jobid_t job,
                                       orte_vpid_t range,
                                       orte_vpid_t *startvpid)
{
    *startvpid = ORTE_VPID_MAX;
    return ORTE_ERR_UNREACH;
}

int
orte_ns_base_assign_rml_tag_not_available(orte_rml_tag_t *tag, char *name)
{
    *tag = ORTE_RML_TAG_MAX;
    return ORTE_ERR_UNREACH;
}


/*
 * functions
 */

int orte_ns_base_assign_cellid_to_process(orte_process_name_t *name)
{
    if (NULL == name) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    name->cellid = 0;
    return OMPI_SUCCESS;
}


int orte_ns_base_create_process_name(orte_process_name_t **name,
                                  orte_cellid_t cell,
                                  orte_jobid_t job,
                                  orte_vpid_t vpid)
{
    *name = NULL;
    
    if (ORTE_CELLID_MAX < cell ||
        ORTE_JOBID_MAX < job ||
	    ORTE_VPID_MAX < vpid) {
	   return ORTE_ERR_BAD_PARAM;
    }

    *name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    if (NULL == *name) { /* got an error */
	   return ORTE_ERR_OUT_OF_RESOURCE;
    }

    (*name)->cellid = cell;
    (*name)->jobid = job;
    (*name)->vpid = vpid;
    return ORTE_SUCCESS;
}

int orte_ns_base_derive_vpid(orte_vpid_t *vpid, orte_vpid_t base_vpid, int offset)
{
    *vpid = base_vpid + (orte_vpid_t)offset;
    
    return ORTE_SUCCESS;
}


int orte_ns_base_copy_process_name(orte_process_name_t **dest,
                                orte_process_name_t* src)
{
    orte_cellid_t cell;
    orte_jobid_t job;
    orte_vpid_t vpid;
    
    *dest = NULL;
    
    if (NULL == src) {
	   return ORTE_ERR_BAD_PARAM;
    }

    if (ORTE_SUCCESS != orte_ns_base_get_cellid(&cell, src)) {
        return ORTE_ERR_BAD_PARAM;
    }
    if (ORTE_SUCCESS != orte_ns_base_get_jobid(&job, src)) {
        return ORTE_ERR_BAD_PARAM;
    }
    if (ORTE_SUCCESS != orte_ns_base_get_vpid(&vpid, src)) {
        return ORTE_ERR_BAD_PARAM;
    }

    return orte_ns_base_create_process_name(dest, cell, job, vpid);
}

int orte_ns_base_get_proc_name_string(char **name_string,
                                   const orte_process_name_t* name)
{
    *name_string = NULL;
    
    if (NULL == name) { /* got an error */
	   return ORTE_ERR_BAD_PARAM;
    }

    if (0 > asprintf(name_string, "%0X.%0X.%0X", name->cellid, name->jobid, name->vpid)) {
	   return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}

int orte_ns_base_convert_string_to_process_name(orte_process_name_t **name,
                                             const char* name_string)
{
    char *temp, *token;
    orte_cellid_t cell;
    orte_jobid_t job;
    orte_vpid_t vpid;
    unsigned long int tmpint;
    int return_code=ORTE_SUCCESS;

    const char delimiters[] = ".";

    *name = NULL;

    /* check for NULL string - error */
    if (NULL == name_string) {
	   return ORTE_ERR_BAD_PARAM;
    }

    temp = strdup(name_string);
    token = strtok(temp, delimiters); /* get first field -> cellid */

    /* convert to largest possible unsigned int - unsigned long long is only supported
     * in C99, so we have to use unsigned long for backward compatibility - then
     * check to ensure it is within range of cellid_t before casting */

    tmpint = strtoul(token, NULL, 16);
    if (ORTE_CELLID_MAX >= tmpint) {
	   cell = (orte_cellid_t)tmpint;
    } else {
       return_code = ORTE_ERR_BAD_PARAM;
	   goto CLEANUP;
    }

    token = strtok(NULL, delimiters);  /* get second field -> jobid */

    /* convert to largest possible unsigned int - then
     * check to ensure it is within range of jobid_t before casting */

    tmpint = strtoul(token, NULL, 16);
    if (ORTE_JOBID_MAX >= tmpint) {
	   job = (orte_jobid_t)tmpint;
    } else {
       return_code = ORTE_ERR_BAD_PARAM;
	   goto CLEANUP;
    }

    token = strtok(NULL, delimiters);  /* get third field -> vpid */

    /* convert to largest possible unsigned int then
     * check to ensure it is within range of vpid_t before casting */

    tmpint = strtoul(token, NULL, 16);
    if (ORTE_VPID_MAX >= tmpint) {
	   vpid = (orte_vpid_t)tmpint;
    } else {
       return_code = ORTE_ERR_BAD_PARAM;
	   goto CLEANUP;
    }

    return_code = orte_ns_base_create_process_name(name, cell, job, vpid);

 CLEANUP:
    if (temp) {
	   free(temp);
    }

    return return_code;
}


int orte_ns_base_get_vpid_string(char **vpid_string, const orte_process_name_t* name)
{
    *vpid_string = NULL;
    
    if (NULL == name) { /* got an error */
	   return ORTE_ERR_BAD_PARAM;
    }

    if (0 > asprintf(vpid_string, "%0X", name->vpid)) {
	   return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_vpid_to_string(char **vpid_string, const orte_vpid_t vpid)
{
    *vpid_string = NULL;
    
    if (0 > asprintf(vpid_string, "%0X", vpid)) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_string_to_vpid(orte_vpid_t *vpid, const char* vpidstring)
{
    unsigned long int tmpint;

    *vpid = ORTE_VPID_MAX;
    
    if (NULL == vpidstring) {  /* got an error */
        return ORTE_ERR_BAD_PARAM;
    }
    
    tmpint = strtoul(vpidstring, NULL, 16);
    if (ORTE_VPID_MAX >= tmpint) {
        *vpid = (orte_vpid_t)tmpint;
    } else {
        *vpid = ORTE_VPID_MAX;
        return ORTE_ERR_BAD_PARAM;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_get_jobid_string(char **jobid_string, const orte_process_name_t* name)
{
    *jobid_string = NULL;
    
    if (NULL == name) { /* got an error */
	   return ORTE_ERR_BAD_PARAM;
    }

    if (0 > asprintf(jobid_string, "%0X", name->jobid)) {
	   return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_jobid_to_string(char **jobid_string, const orte_jobid_t jobid)
{
    *jobid_string = NULL;
    
    if (0 > asprintf(jobid_string, "%0X", jobid)) {
	   return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_string_to_jobid(orte_jobid_t *jobid, const char* jobidstring)
{
    unsigned long int tmpint;

    *jobid = ORTE_JOBID_MAX;
    
    if (NULL == jobidstring) {  /* got an error */
        return ORTE_ERR_BAD_PARAM;
    }
    
    tmpint = strtoul(jobidstring, NULL, 16);
    if (ORTE_JOBID_MAX >= tmpint) {
	   *jobid = (orte_jobid_t)tmpint;
    } else {
	   *jobid = ORTE_JOBID_MAX;
       return ORTE_ERR_BAD_PARAM;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_get_cellid_string(char **cellid_string, const orte_process_name_t* name)
{
    *cellid_string = NULL;
    
    if (NULL == name) { /* got an error */
	   return ORTE_ERR_BAD_PARAM;
    }

    if (0 > asprintf(cellid_string, "%0X", name->cellid)) {
	   return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}


int orte_ns_base_convert_cellid_to_string(char **cellid_string, const orte_cellid_t cellid)
{
    *cellid_string = NULL;
    
    if (0 > asprintf(cellid_string, "%0X", cellid)) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    return ORTE_SUCCESS;
}


int orte_ns_base_convert_string_to_cellid(orte_cellid_t *cellid, const char *cellidstring)
{
    unsigned long int tmpint;
    
    *cellid = ORTE_CELLID_MAX;
    
    if (NULL == cellidstring) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    tmpint = strtoul(cellidstring, NULL, 16);
    if (ORTE_CELLID_MAX >= tmpint) {
        *cellid = (orte_cellid_t)tmpint;
    } else {
        *cellid = ORTE_CELLID_MAX;
        return ORTE_ERR_BAD_PARAM;
    }
    
    return ORTE_SUCCESS;
}


int orte_ns_base_get_vpid(orte_vpid_t *vpid, const orte_process_name_t* name)
{
    *vpid = ORTE_VPID_MAX;
    
    if (NULL == name) { /* got an error */
	   return ORTE_ERR_BAD_PARAM;
    }

    *vpid = name->vpid;
    
    return ORTE_SUCCESS;
}


int orte_ns_base_get_jobid(orte_jobid_t *jobid, const orte_process_name_t* name)
{
    *jobid = ORTE_JOBID_MAX;
    
    if (NULL == name) { /* got an error */
	   return ORTE_ERR_BAD_PARAM;
    }

    *jobid = name->jobid;
    
    return ORTE_SUCCESS;
}

int orte_ns_base_get_cellid(orte_cellid_t *cellid, const orte_process_name_t* name)
{
    *cellid = ORTE_CELLID_MAX;
    
    if (NULL == name) { /* got an error */
	   return ORTE_ERR_BAD_PARAM;
    }

    *cellid = name->cellid;
    
    return ORTE_SUCCESS;
}


int orte_ns_base_compare(orte_ns_cmp_bitmask_t fields,
		    const orte_process_name_t* name1,
		    const orte_process_name_t* name2)
{
    if (NULL == name1 && NULL == name2) {
	   return 0;
    } else if (NULL == name1) {
       return -1;
    } else if (NULL == name2) {
       return 1;
    }

    if (ORTE_NS_CMP_CELLID & fields) { /* check cellid field */
	if (name1->cellid < name2->cellid) {
        return -1;
	} else if (name1->cellid > name2->cellid) {
        return 1;
	}
    }

    /* get here if cellid's are equal, or cellid not being checked */
    /* now check job id */

    if (ORTE_NS_CMP_JOBID & fields) {
	if (name1->jobid < name2->jobid) {
        return -1;
	} else if (name1->jobid > name2->jobid) {
        return 1;
	}
    }

    /* get here if cellid's and jobid's are equal, or neither being checked,
     * or cellid not checked and jobid's equal.
     * now check vpid
     */

    if (ORTE_NS_CMP_VPID & fields) {
	if (name1->vpid < name2->vpid) {
        return -1;
	} else if (name1->vpid > name2->vpid) {
        return 1;
	}
    }

    /* only way to get here is if all fields are being checked and are equal,
     * or cellid not checked, but jobid and vpid equal,
     * or cellid and jobid not checked, but vpid equal,
     * only vpid being checked, and equal
     * return that fact
     */
    return 0;
}


int orte_ns_base_free_name(orte_process_name_t **name)
{
    if (NULL != name && NULL != *name) {
	   free(*name);
    }

    *name = NULL;
    
    return ORTE_SUCCESS;
}
