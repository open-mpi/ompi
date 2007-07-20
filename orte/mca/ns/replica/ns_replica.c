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

#include "opal/threads/mutex.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/ns/base/base.h"
#include "orte/mca/ns/base/ns_private.h"
#include "ns_replica.h"

/**
 * globals
 */
#define NS_REPLICA_MAX_STRING_SIZE  256

/*
 * DIAGNOSTIC functions
 */
int orte_ns_replica_dump_cells(void)
{
    orte_buffer_t buffer;
    int rc;

    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_cells_fn(&buffer))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&buffer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }

    OBJ_DESTRUCT(&buffer);
    return ORTE_SUCCESS;
}

int orte_ns_replica_dump_cells_fn(orte_buffer_t *buffer)
{
    orte_std_cntr_t i;
    orte_cellid_t j;
    orte_ns_replica_cell_tracker_t **cell;
    char tmp_out[NS_REPLICA_MAX_STRING_SIZE], *tmp;
    int rc;

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    tmp = tmp_out;
    snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "Dump of Name Service Cell Tracker\n");
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    cell = (orte_ns_replica_cell_tracker_t**)(orte_ns_replica.cells)->addr;
    for (i=0, j=0; j < orte_ns_replica.num_cells &&
                   i < (orte_ns_replica.cells)->size; i++) {
        if (NULL != cell[i]) {
            j++;
            snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "Num: %lu\tCell: %lu\n",
                (unsigned long)j, (unsigned long)cell[i]->cell);
            if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return rc;
            }
            snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "\tSite: %s\n\tResource: %s\n",
                cell[i]->site, cell[i]->resource);
            if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return rc;
            }
        }
    }

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);

    return ORTE_SUCCESS;
}


int orte_ns_replica_dump_jobs(void)
{
    orte_buffer_t buffer;
    int rc;

    OBJ_CONSTRUCT(&buffer, orte_buffer_t);

    if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_jobs_fn(&buffer))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&buffer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }

    OBJ_DESTRUCT(&buffer);
    return ORTE_SUCCESS;
}

int orte_ns_replica_dump_jobs_fn(orte_buffer_t *buffer)
{
    orte_std_cntr_t i;
    orte_cellid_t j;
    orte_ns_replica_jobid_tracker_t **ptr;
    char tmp_out[NS_REPLICA_MAX_STRING_SIZE], *tmp;
    int rc;

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    tmp = tmp_out;
    snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "Dump of Name Service Jobid Tracker\n");
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    ptr = (orte_ns_replica_jobid_tracker_t**)(orte_ns_replica.jobids)->addr;
    for (i=0, j=0; j < orte_ns_replica.num_jobids &&
                   i < (orte_ns_replica.jobids)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "Num: %lu\tJobid: %lu\tNext vpid: %lu\n",
                (unsigned long)j, (unsigned long)ptr[i]->jobid,
                (unsigned long)ptr[i]->next_vpid);
            if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return rc;
            }
        }
    }

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);

    return ORTE_SUCCESS;
}


int orte_ns_replica_dump_tags(void)
{
    orte_buffer_t buffer;
    int rc;

    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_tags_fn(&buffer))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&buffer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }

    OBJ_DESTRUCT(&buffer);
    return ORTE_SUCCESS;
}


int orte_ns_replica_dump_tags_fn(orte_buffer_t *buffer)
{
    orte_std_cntr_t i;
    orte_rml_tag_t j;
    orte_ns_replica_tagitem_t **ptr;
    char tmp_out[NS_REPLICA_MAX_STRING_SIZE], *tmp;
    int rc;

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    tmp = tmp_out;
    snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "Dump of Name Service RML Tag Tracker\n");
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    ptr = (orte_ns_replica_tagitem_t**)(orte_ns_replica.tags)->addr;
    for (i=0, j=0; j < orte_ns_replica.num_tags &&
                   i < (orte_ns_replica.tags)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "Num: %lu\tTag id: %lu\tName: %s\n",
                (unsigned long)j, (unsigned long)ptr[i]->tag, ptr[i]->name);
            if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return rc;
            }
        }
    }

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);

    return ORTE_SUCCESS;
}


int orte_ns_replica_dump_datatypes(void)
{
    orte_buffer_t buffer;
    int rc;

    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_datatypes_fn(&buffer))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_ns_base_print_dump(&buffer))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }

    OBJ_DESTRUCT(&buffer);
    return ORTE_SUCCESS;
}

int orte_ns_replica_dump_datatypes_fn(orte_buffer_t *buffer)
{
    orte_std_cntr_t i, j;
    orte_ns_replica_dti_t **ptr;
    char tmp_out[NS_REPLICA_MAX_STRING_SIZE], *tmp;
    int rc;

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    tmp = tmp_out;
    snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "Dump of Name Service Datatype Tracker\n");
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    ptr = (orte_ns_replica_dti_t**)(orte_ns_replica.dts)->addr;
    for (i=0, j=0; j < orte_ns_replica.num_dts &&
                   i < (orte_ns_replica.dts)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            snprintf(tmp, NS_REPLICA_MAX_STRING_SIZE, "Num: %lu\tDatatype id: %lu\tName: %s\n",
                (unsigned long)j, (unsigned long)ptr[i]->id, ptr[i]->name);
            if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return rc;
            }
        }
    }

    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);

    return ORTE_SUCCESS;
}


/*
 * TAG SERVER functions
 */
int orte_ns_replica_assign_rml_tag(orte_rml_tag_t *tag,
                                   char *name)
{
    orte_ns_replica_tagitem_t *tagitem, **tags;
    orte_std_cntr_t i;
    orte_rml_tag_t j;
    int rc;

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    if (NULL != name) {
        /* see if this name is already in list - if so, return tag */
        tags = (orte_ns_replica_tagitem_t**)orte_ns_replica.tags->addr;
        for (i=0, j=0; j < orte_ns_replica.num_tags &&
                       i < (orte_ns_replica.tags)->size; i++) {
            if (NULL != tags[i]) {
                j++;
                if (tags[i]->name != NULL &&
                    0 == strcmp(name, tags[i]->name)) { /* found name on list */
                    *tag = tags[i]->tag;
                    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                    return ORTE_SUCCESS;
                }
            }
        }
    }

    /* not in list or not provided, so allocate next tag */
    *tag = ORTE_RML_TAG_MAX;

    /* check if tag is available - need to do this since the tag type
     * is probably not going to be a orte_std_cntr_t, so we cannot just rely
     * on the pointer_array's size limits to protect us. NOTE: need to
     * reserve ORTE_RML_TAG_MAX as an invalid value, so can't let
     * num_tags get there
      */
    if (ORTE_RML_TAG_MAX-2 < orte_ns_replica.num_tags) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    tagitem = OBJ_NEW(orte_ns_replica_tagitem_t);
    if (NULL == tagitem) { /* out of memory */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&i,
                                orte_ns_replica.tags, tagitem))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    tagitem->tag = orte_ns_replica.num_tags + ORTE_RML_TAG_DYNAMIC;
    (orte_ns_replica.num_tags)++;
    if (NULL != name) {  /* provided - can look it up later */
        tagitem->name = strdup(name);
    } else {
        tagitem->name = NULL;
    }

    *tag = tagitem->tag;
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}


/*
 * DATA TYPE SERVER functions
 */
int orte_ns_replica_define_data_type(const char *name,
                                     orte_data_type_t *type)
{
    orte_ns_replica_dti_t **dti, *dtip;
    orte_std_cntr_t i, j;
    int rc;

    if (NULL == name || 0 < *type) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

    dti = (orte_ns_replica_dti_t**)orte_ns_replica.dts->addr;
    for (i=0, j=0; j < orte_ns_replica.num_dts &&
                   i < orte_ns_replica.dts->size; i++) {
        if (NULL != dti[i]) {
            j++;
            if (dti[i]->name != NULL &&
                0 == strcmp(name, dti[i]->name)) { /* found name on list */
                *type = dti[i]->id;
                OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
                return ORTE_SUCCESS;
            }
        }
    }

    /* not in list or not provided, so allocate next id */
    *type = ORTE_DSS_ID_MAX;

    /* check if id is available - need to do this since the data type
     * is probably not going to be a orte_std_cntr_t, so we cannot just rely
     * on the pointer_array's size limits to protect us.
      */
    if (ORTE_DSS_ID_MAX-2 < orte_ns_replica.num_dts) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
     }

    dtip = OBJ_NEW(orte_ns_replica_dti_t);
    if (NULL == dtip) { /* out of memory */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    dtip->name = strdup(name);
    if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&i,
                                orte_ns_replica.dts, dtip))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
        return rc;
    }
    dtip->id = orte_ns_replica.num_dts;
    (orte_ns_replica.num_dts)++;

    *type = dtip->id;
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}



/*
 * NAME functions
 */
int orte_ns_replica_create_my_name(void)
{
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_ns.create_jobid(&jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, 1, &vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_ns.create_process_name(&(orte_process_info.my_name),
                                                0, jobid, vpid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;
}
