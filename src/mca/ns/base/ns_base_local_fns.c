/*
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

#include "include/constants.h"
#include "util/output.h"
#include "util/printf.h"
#include "mca/mca.h"
#include "mca/ns/base/base.h"

/**
 * globals
 */

/*
 * functions
 */

int ns_base_assign_cellid_to_process(ompi_process_name_t* name)
{
    name->cellid = 0;
    return OMPI_SUCCESS;
}


ompi_process_name_t* ns_base_create_process_name(mca_ns_base_cellid_t cell,
		             mca_ns_base_jobid_t job, mca_ns_base_vpid_t vpid)
{
    ompi_process_name_t *newname;

    if (MCA_NS_BASE_CELLID_MAX < cell ||
        MCA_NS_BASE_JOBID_MAX < job ||
	MCA_NS_BASE_VPID_MAX < vpid) {
	return(NULL);
    }

    newname = (ompi_process_name_t*)malloc(sizeof(ompi_process_name_t));
    if (NULL == newname) { /* got an error */
	return(NULL);
    }

    newname->cellid = cell;
    newname->jobid = job;
    newname->vpid = vpid;
    return(newname);
}

ompi_process_name_t* ns_base_copy_process_name(ompi_process_name_t* name)
{
    mca_ns_base_cellid_t cell;
    mca_ns_base_jobid_t job;
    mca_ns_base_vpid_t vpid;
    ompi_process_name_t *newname;

    if (NULL == name) {
	return NULL;
    }

    cell = ns_base_get_cellid(name);
    job = ns_base_get_jobid(name);
    vpid = ns_base_get_vpid(name);

    newname = ns_base_create_process_name(cell, job, vpid);
    return newname;
}

char* ns_base_get_proc_name_string(const ompi_process_name_t* name)
{
    char *name_string;

    if (NULL == name) { /* got an error */
	return(NULL);
    }

    if (0 > asprintf(&name_string, "%0X.%0X.%0X", name->cellid, name->jobid, name->vpid)) {
	return NULL;
    }

    return(name_string);
}

ompi_process_name_t* ns_base_convert_string_to_process_name(const char* name)
{
    char *temp, *token;
    mca_ns_base_cellid_t cell;
    mca_ns_base_jobid_t job;
    mca_ns_base_vpid_t vpid;
    unsigned long int tmpint;

    const char delimiters[] = ".";
    ompi_process_name_t *return_code;

    return_code = NULL;

    /* check for NULL string - error */
    if (NULL == name) {
	return NULL;
    }

    temp = strdup(name);
    token = strtok(temp, delimiters); /* get first field -> cellid */

    /* convert to largest possible unsigned int - unsigned long long is only supported
     * in C99, so we have to use unsigned long for backward compatibility - then
     * check to ensure it is within range of cellid_t before casting */

    tmpint = strtoul(token, NULL, 16);
    if (MCA_NS_BASE_CELLID_MAX >= tmpint) {
	cell = (mca_ns_base_cellid_t)tmpint;
    } else {
	goto CLEANUP;
    }

    token = strtok(NULL, delimiters);  /* get second field -> jobid */

    /* convert to largest possible unsigned int - then
     * check to ensure it is within range of jobid_t before casting */

    tmpint = strtoul(token, NULL, 16);
    if (MCA_NS_BASE_JOBID_MAX >= tmpint) {
	job = (mca_ns_base_jobid_t)tmpint;
    } else {
	goto CLEANUP;
    }

    token = strtok(NULL, delimiters);  /* get third field -> vpid */

    /* convert to largest possible unsigned int then
     * check to ensure it is within range of vpid_t before casting */

    tmpint = strtoul(token, NULL, 16);
    if (MCA_NS_BASE_VPID_MAX >= tmpint) {
	vpid = (mca_ns_base_vpid_t)tmpint;
    } else {
	goto CLEANUP;
    }

    return_code = ns_base_create_process_name(cell, job, vpid);

 CLEANUP:
    if (temp) {
	free(temp);
    }

    return return_code;
}


char* ns_base_get_vpid_string(const ompi_process_name_t* name)
{
    char *name_string;

    if (NULL == name) { /* got an error */
	return(NULL);
    }

    if (0 > asprintf(&name_string, "%0X", name->vpid)) {
	return NULL;
    }

    return(name_string);
}


char* ns_base_get_jobid_string(const ompi_process_name_t* name)
{
    char *name_string;

    if (NULL == name) { /* got an error */
	return(NULL);
    }

    if (0 > asprintf(&name_string, "%0X", name->jobid)) {
	return NULL;
    }

    return(name_string);
}


char* ns_base_convert_jobid_to_string(const mca_ns_base_jobid_t jobid)
{
    char *jobid_string;

    if (0 > asprintf(&jobid_string, "%0X", jobid)) {
	return NULL;
    }

    return jobid_string;
}


char* ns_base_get_cellid_string(const ompi_process_name_t* name)
{
    char *name_string;

    if (NULL == name) { /* got an error */
	return(NULL);
    }

    if (0 > asprintf(&name_string, "%0X", name->cellid)) {
	return NULL;
    }

    return(name_string);
}


mca_ns_base_vpid_t ns_base_get_vpid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(MCA_NS_BASE_VPID_MAX);
    }

    return(name->vpid);
}


mca_ns_base_jobid_t ns_base_get_jobid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(MCA_NS_BASE_JOBID_MAX);
    }

    return(name->jobid);
}

mca_ns_base_cellid_t ns_base_get_cellid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(MCA_NS_BASE_CELLID_MAX);
    }

    return(name->cellid);
}


int ns_base_compare(ompi_ns_cmp_bitmask_t fields,
		    const ompi_process_name_t* name1,
		    const ompi_process_name_t* name2)
{
    if (NULL == name1 || NULL == name2) {  /* got an error */
	return(-100);
    }

    if (OMPI_NS_CMP_CELLID & fields) { /* check cellid field */
	if (name1->cellid < name2->cellid) {
	    return(-1);
	} else if (name1->cellid > name2->cellid) {
	    return(1);
	}
    }

    /* get here if cellid's are equal, or cellid not being checked */
    /* now check job id */

    if (OMPI_NS_CMP_JOBID & fields) {
	if (name1->jobid < name2->jobid) {
	    return(-1);
	} else if (name1->jobid > name2->jobid) {
	    return(1);
	}
    }

    /* get here if cellid's and jobid's are equal, or neither being checked,
     * or cellid not checked and jobid's equal.
     * now check vpid
     */

    if (OMPI_NS_CMP_VPID & fields) {
	if (name1->vpid < name2->vpid) {
	    return(-1);
	} else if (name1->vpid > name2->vpid) {
	    return(1);
	}
    }

    /* only way to get here is if all fields are being checked and are equal,
     * or cellid not checked, but jobid and vpid equal,
     * or cellid and jobid not checked, but vpid equal,
     * only vpid being checked, and equal
     * return that fact
     */
    return(0);
}


int ns_base_free_name(ompi_process_name_t* name)
{
    if (NULL != name) {
	free(name);
    }

    return OMPI_SUCCESS;
}
