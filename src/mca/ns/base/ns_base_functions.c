/*
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"
#include "mca/mca.h"
#include "mca/ns/base/base.h"
#include "ns_replica.h"

/**
 * globals
 */

/*
 * functions
 */

ompi_process_name_t* ns_base_create_process_name(ompi_process_id_t cell,
		             ompi_process_id_t job, ompi_process_id_t vpid)
{
    ompi_process_name_t *newname;

    newname = OBJ_NEW(ompi_process_name_t);
    if (NULL == newname) { /* got an error */
	return(NULL);
    }

    newname->cellid = cell;
    newname->jobid = job;
    newname->vpid = vpid;
    return(newname);
}


char* ns_base_get_proc_name_string(const ompi_process_name_t* name)
{
    char *name_string;
    int size;

    if (NULL == name) { /* got an error */
	return(NULL);
    }

    size = (3*sizeof(name->cellid)/4) + 3;
    name_string = (char*)malloc(27*sizeof(char));
    if (NULL == name_string) { /* got an error */
	return(NULL);
    }

    sprintf(name_string, "%0x.%0x.%0x", name->cellid, name->jobid, name->vpid);
    return(name_string);
}


char* ns_base_get_vpid_string(const ompi_process_name_t* name)
{
    char *name_string;
    int size;

    if (NULL == name) { /* got an error */
	return(NULL);
    }

    size = 1 + sizeof(name->vpid)/4;
    name_string = (char*)malloc(size*sizeof(char));
    if (NULL == name_string) { /* got an error */
	return(NULL);
    }

    sprintf(name_string, "%0x", name->vpid);
    return(name_string);
}


char* ns_base_get_jobid_string(const ompi_process_name_t* name)
{
    char *name_string;
    int size;

    if (NULL == name) { /* got an error */
	return(NULL);
    }

    size = 1 + sizeof(name->jobid);
    name_string = (char*)malloc(size*sizeof(char));
    if (NULL == name_string) { /* got an error */
	return(NULL);
    }

    sprintf(name_string, "%0x", name->jobid);
    return(name_string);
}


char* ns_base_get_cellid_string(const ompi_process_name_t* name)
{
    char *name_string;
    int size;

    if (NULL == name) { /* got an error */
	return(NULL);
    }

    size = 1 + sizeof(name->cellid);
    name_string = (char*)malloc(size*sizeof(char));
    if (NULL == name_string) { /* got an error */
	return(NULL);
    }

    sprintf(name_string, "%0x", name->cellid);
    return(name_string);
}


ompi_process_id_t ns_base_get_vpid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(OMPI_NAME_SERVICE_MAX);
    }

    return(name->vpid);
}


ompi_process_id_t ns_base_get_jobid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(OMPI_NAME_SERVICE_MAX);
    }

    return(name->jobid);
}

ompi_process_id_t ns_base_get_cellid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(OMPI_NAME_SERVICE_MAX);
    }

    return(name->cellid);
}


int ns_base_compare(ompi_ns_cmp_bitmask_t fields,
		    const ompi_process_name_t* name1,
		    const ompi_process_name_t* name2)
{
    if ((fields <= 0) || (fields > 7) || NULL == name1 || NULL == name2) {  /* got an error */
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

