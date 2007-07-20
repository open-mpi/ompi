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

    OPAL_THREAD_LOCK(&orte_ns_replica.mutex);

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
    
    OPAL_THREAD_UNLOCK(&orte_ns_replica.mutex);
    return ORTE_SUCCESS;
}

static int dump_child_jobs(orte_ns_replica_jobitem_t *ptr, char *prefix, orte_buffer_t *buffer)
{
    opal_list_item_t *item;
    orte_ns_replica_jobitem_t *child;
    char *tmp;
    int rc;
    char *pfx;

    asprintf(&pfx, "%s    ", prefix);
    
    /* print out the children's info */
    for (item = opal_list_get_first(&ptr->children);
         item != opal_list_get_end(&ptr->children);
         item = opal_list_get_next(item)) {
        child = (orte_ns_replica_jobitem_t*)item;
        asprintf(&tmp, "%sChild jobid: %ld    Next vpid: %ld    Num direct children: %ld\n",
                 pfx, (long)child->jobid, (long)child->next_vpid, (long)opal_list_get_size(&child->children));
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        free(tmp);
        if (ORTE_SUCCESS != (rc = dump_child_jobs(child, pfx, buffer))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    free(pfx);
    
    return ORTE_SUCCESS;
}

int orte_ns_replica_dump_jobs_fn(orte_buffer_t *buffer)
{
    orte_ns_replica_jobitem_t *root;
    opal_list_item_t *item;
    char *tmp;
    int rc;
    char *prefix = "        ";

    asprintf(&tmp, "Dump of Name Service Jobid Tracker\n");
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    free(tmp);
    
    for (item = opal_list_get_first(&orte_ns_replica.jobs);
         item != opal_list_get_end(&orte_ns_replica.jobs);
         item = opal_list_get_next(item)) {
        root = (orte_ns_replica_jobitem_t*)item;
        asprintf(&tmp, "    Data for job family with root %ld\n", (long)root->jobid);
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        free(tmp);
        asprintf(&tmp, "%sNext vpid: %ld    Num direct children: %ld\n",
                 prefix, (long)root->next_vpid, (long)opal_list_get_size(&root->children));
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &tmp, 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        free(tmp);
        if (ORTE_SUCCESS != (rc = dump_child_jobs(root, prefix, buffer))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

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

