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
/* constructor - used to initialize state of name instance */
static void ompi_name_construct(ompi_process_name_t* name)
{
    name->cellid = 0;
    name->jobid = 0;
    name->vpid = 0;
}

/* destructor - used to free any resources held by instance */
static void ompi_name_destructor(ompi_process_name_t* name)
{
}

OBJ_CLASS_INSTANCE(
		   ompi_process_name_t,   /* type name */
		   ompi_object_t,         /* parent "class" name */
		   ompi_name_construct,   /* constructor */
		   ompi_name_destructor);  /* destructor */

/*
 * functions
 */

ompi_process_id_t ns_replica_create_cellid(void)
{
    if ((OMPI_NAME_SERVICE_MAX-1) < last_used_cellid) {
	last_used_cellid = last_used_cellid + 1;
	return(last_used_cellid);
    } else {
	return(0);
    }
}

ompi_process_id_t ns_replica_create_jobid(void)
{
    ompi_name_tracker_t *new;

    if ((OMPI_NAME_SERVICE_MAX-1) < last_used_jobid) {
	last_used_jobid = last_used_jobid + 1;
	new = OBJ_NEW(ompi_name_tracker_t);
	new->job = last_used_jobid;
	new->last_used_vpid = 0;
	ompi_list_append(&ompi_name_tracker, &new->item);
	return(last_used_jobid);
    } else {
	return(0);
    }
}

ompi_process_name_t* ns_replica_create_process_name(ompi_process_id_t cell,
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


ompi_process_id_t ns_replica_reserve_range(ompi_process_id_t job, ompi_process_id_t range)
{
    ompi_name_tracker_t *ptr;
    ompi_process_id_t start;

    for (ptr = (ompi_name_tracker_t*)ompi_list_get_first(&ompi_name_tracker);
	 ptr != (ompi_name_tracker_t*)ompi_list_get_end(&ompi_name_tracker);
	 ptr = (ompi_name_tracker_t*)ompi_list_get_next(ptr)) {
	if (job == ptr->job) { /* found the specified job */
	    if ((OMPI_NAME_SERVICE_MAX-range-1) > ptr->last_used_vpid) {  /* requested range available */
		start = ptr->last_used_vpid + 1;
		ptr->last_used_vpid = ptr->last_used_vpid + range;
		return(start);
	    }
	}
    }
    return(0);
}


int ns_replica_free_name(ompi_process_name_t* name)
{
    return OMPI_SUCCESS;
}


char* ns_replica_get_proc_name_string(const ompi_process_name_t* name)
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


char* ns_replica_get_vpid_string(const ompi_process_name_t* name)
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


char* ns_replica_get_jobid_string(const ompi_process_name_t* name)
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


char* ns_replica_get_cellid_string(const ompi_process_name_t* name)
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


ompi_process_id_t ns_replica_get_vpid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(OMPI_NAME_SERVICE_MAX);
    }

    return(name->vpid);
}


ompi_process_id_t ns_replica_get_jobid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(OMPI_NAME_SERVICE_MAX);
    }

    return(name->jobid);
}

ompi_process_id_t ns_replica_get_cellid(const ompi_process_name_t* name)
{
    if (NULL == name) { /* got an error */
	return(OMPI_NAME_SERVICE_MAX);
    }

    return(name->cellid);
}


int ns_replica_compare(ompi_ns_cmp_bitmask_t fields,
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

