/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
  * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <time.h>

#include "opal/class/opal_hash_table.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss_types.h"
#include "opal/util/output.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/db/base/base.h"
#include "db_hash.h"

static int init(void);
static void finalize(void);
static int store(const orte_process_name_t *proc,
                 const char *key, const void *object, int32_t size);
static int fetch(const orte_process_name_t *proc,
                 const char *key,
                 opal_list_t *values);
static int remove_data(const orte_process_name_t *proc, const char *key);

orte_db_base_module_t orte_db_hash_module = {
    init,
    finalize,
    store,
    fetch,
    remove_data
};

/* Local "globals" */
static opal_pointer_array_t job_data;

/**
 * Data for a particular orte process
 * The name association is maintained in the
 * proc_data hash table.
 */
typedef struct {
    /** Structure can be put on lists (including in hash tables) */
    opal_list_item_t super;
    /* List of local_data_t structures containing all data
       received from this process, sorted by key. */
    opal_list_t data;
} proc_data_t;

static void proc_data_construct(proc_data_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->data, opal_list_t);
}

static void proc_data_destruct(proc_data_t *ptr)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first(&ptr->data))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ptr->data);
}
OBJ_CLASS_INSTANCE(proc_data_t, opal_list_item_t,
                   proc_data_construct, proc_data_destruct);

 
/* Data for a given job
 */
typedef struct {
    opal_object_t super;
    orte_jobid_t jobid;
    opal_hash_table_t *data;
} job_data_t;

static void jobdata_constructor(job_data_t *ptr)
{
    ptr->jobid = ORTE_JOBID_INVALID;
    ptr->data = OBJ_NEW(opal_hash_table_t);    
    opal_hash_table_init(ptr->data, 256);
}
static void jobdata_destructor(job_data_t *ptr)
{
    opal_hash_table_remove_all(ptr->data);
    OBJ_RELEASE(ptr->data);
}
OBJ_CLASS_INSTANCE(job_data_t,
                   opal_object_t,
                   jobdata_constructor,
                   jobdata_destructor);


static int init(void)
{
    OBJ_CONSTRUCT(&job_data, opal_pointer_array_t);
    opal_pointer_array_init(&job_data, 1, INT_MAX, 1);
    return ORTE_SUCCESS;
}

static void finalize(void)
{
    int i;
    job_data_t *jtable;

    for (i=0; i < job_data.size; i++) {
        if (NULL == (jtable = (job_data_t*)opal_pointer_array_get_item(&job_data, i))) {
            continue;
        }
        OBJ_RELEASE(jtable);
    }
    OBJ_DESTRUCT(&job_data);
}



/**
 * Find data for a given key in a given proc_data_t
 * container.
 */
static orte_db_keyval_t* lookup_keyval(proc_data_t *proc_data,
                                       const char *key)
{
    orte_db_keyval_t *kv = NULL;
    for (kv = (orte_db_keyval_t *) opal_list_get_first(&proc_data->data);
         kv != (orte_db_keyval_t *) opal_list_get_end(&proc_data->data);
         kv = (orte_db_keyval_t *) opal_list_get_next(kv)) {
        if (0 == strcmp(key, kv->key)) {
            return kv;
        }
    }

    return NULL;
}


/**
* Find proc_data_t container associated with given
 * orte_process_name_t.
 */
static proc_data_t* lookup_orte_proc(opal_hash_table_t *jtable, orte_vpid_t vpid)
{
    proc_data_t *proc_data = NULL;
    
    opal_hash_table_get_value_uint32(jtable, orte_util_hash_vpid(vpid), (void**)&proc_data);
    if (NULL == proc_data) {
        /* The proc clearly exists, so create a data structure for it */
        proc_data = OBJ_NEW(proc_data_t);
        if (NULL == proc_data) {
            opal_output(0, "db:hash:lookup_orte_proc: unable to allocate proc_data_t\n");
            return NULL;
        }
        opal_hash_table_set_value_uint32(jtable, orte_util_hash_vpid(vpid), proc_data);
    }
    
    return proc_data;
}

static int store(const orte_process_name_t *proc,
                 const char *key, const void *object, int32_t size)
{
    int i;
    job_data_t *jtable, *jtab;
    proc_data_t *proc_data;
    orte_db_keyval_t *kv;

    OPAL_OUTPUT_VERBOSE((5, orte_db_base.output,
                         "%s db:hash:store: storing key %s data size %lu for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         key, (unsigned long)size, ORTE_NAME_PRINT(proc)));

    /* get the job data object for this proc */
    jtable = NULL;
    for (i=0; i < job_data.size; i++) {
        if (NULL == (jtab = (job_data_t*)opal_pointer_array_get_item(&job_data, i))) {
            continue;
        }
        if (jtab->jobid == proc->jobid) {
            jtable = jtab;
            break;
        }
    }
    if (NULL == jtable) {
        /* need to add an entry for this job */
        jtable = OBJ_NEW(job_data_t);
        jtable->jobid = proc->jobid;
        opal_pointer_array_add(&job_data, jtable);
    }

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = lookup_orte_proc(jtable->data, proc->vpid))) {
        /* unrecoverable error */
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* see if we already have this key in the data - means we are updating
     * a pre-existing value
     */
    if (NULL != (kv = lookup_keyval(proc_data, key))) {
        /* release the old data */
        if (NULL != kv->value.bytes) {
            free(kv->value.bytes);
        }
    } else {
        kv = OBJ_NEW(orte_db_keyval_t);
        kv->key = strdup(key);
        opal_list_append(&proc_data->data, &kv->super);
    }
    kv->value.bytes = (uint8_t*)malloc(size);
    memcpy(kv->value.bytes, object, size);
    kv->value.size = size;

    return ORTE_SUCCESS;
}

static int fetch(const orte_process_name_t *proc,
                 const char *key,
                 opal_list_t *values)
{
    int i;
    job_data_t *jtable, *jtab;
    proc_data_t *proc_data;
    orte_db_keyval_t *kv, *ans;

    OPAL_OUTPUT_VERBOSE((5, orte_db_base.output,
                         "%s db:hash:fetch: searching for key %s on proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == key) ? "NULL" : key, ORTE_NAME_PRINT(proc)));

    /* get the job data object for this proc */
    jtable = NULL;
    for (i=0; i < job_data.size; i++) {
        if (NULL == (jtab = (job_data_t*)opal_pointer_array_get_item(&job_data, i))) {
            continue;
        }
        if (jtab->jobid == proc->jobid) {
            jtable = jtab;
            break;
        }
    }
    if (NULL == jtable) {
        /* eventually, we will fetch this data - but for now, this
         * is simply an error
         */
        return ORTE_ERR_NOT_FOUND;
    }

    /* lookup the proc data object for this proc */
    if (NULL == (proc_data = lookup_orte_proc(jtable->data, proc->vpid))) {
        /* unrecoverable error */
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* if the key is NULL, then return all data for this proc */
    if (NULL == key) {
        for (kv = (orte_db_keyval_t *) opal_list_get_first(&proc_data->data);
             kv != (orte_db_keyval_t *) opal_list_get_end(&proc_data->data);
             kv = (orte_db_keyval_t *) opal_list_get_next(kv)) {
            ans = OBJ_NEW(orte_db_keyval_t);
            ans->key = strdup(kv->key);
            ans->value.bytes = malloc(kv->value.size);
            memcpy(ans->value.bytes, kv->value.bytes, kv->value.size);
            ans->value.size = kv->value.size;
            opal_list_append(values, &ans->super);
        }
        return ORTE_SUCCESS;
    }

    /* find the value */
    if (NULL == (kv = lookup_keyval(proc_data, key))) {
        /* again, we eventually will attempt to fetch the data - for
         * now, just report it as an error */
        return ORTE_ERR_NOT_FOUND;
    }

    /* copy the data across */
    ans = OBJ_NEW(orte_db_keyval_t);
    ans->value.bytes = (uint8_t*)malloc(kv->value.size);
    memcpy(ans->value.bytes, kv->value.bytes, kv->value.size);
    ans->value.size = kv->value.size;
    opal_list_append(values, &ans->super);

    return ORTE_SUCCESS;
}

static int remove_data(const orte_process_name_t *proc, const char *key)
{
    int i, save_loc;
    job_data_t *jtable, *jtab;
    proc_data_t *proc_data;
    orte_db_keyval_t *kv;

    /* if proc is NULL, remove all data from the database */
    if (NULL == proc) {
        for (i=0; i < job_data.size; i++) {
            if (NULL == (jtable = (job_data_t*)opal_pointer_array_get_item(&job_data, i))) {
                continue;
            }
            OBJ_RELEASE(jtable);
        }
        /* leave the job pointer array itself as we may add data back */
        return ORTE_SUCCESS;
    }

    /* lookup the specified jobid */
    jtable = NULL;
    for (i=0; i < job_data.size; i++) {
        if (NULL == (jtab = (job_data_t*)opal_pointer_array_get_item(&job_data, i))) {
            continue;
        }
        if (jtab->jobid == proc->jobid) {
            jtable = jtab;
            save_loc = i;
            break;
        }
    }
    if (NULL == jtable) {
        /* don't have any data for this job */
        return ORTE_SUCCESS;
    }

    /* if vpid is WILDCARD, remove all data for this job */
    if (ORTE_VPID_WILDCARD == proc->vpid) {
        opal_pointer_array_set_item(&job_data, save_loc, NULL);
        OBJ_RELEASE(jtable);
        return ORTE_SUCCESS;
    }

    /* lookup the specified proc */
    if (NULL == (proc_data = lookup_orte_proc(jtable->data, proc->vpid))) {
        /* no data for this proc */
        return ORTE_SUCCESS;
    }

    /* if key is NULL, remove all data for this proc */
    if (NULL == key) {
        while (NULL != (kv = (orte_db_keyval_t *) opal_list_remove_first(&proc_data->data))) {
            OBJ_RELEASE(kv);
        }
        /* remove the proc_data object itself from the jtable */
        opal_hash_table_remove_value_uint32(jtable->data, orte_util_hash_vpid(proc->vpid));
        /* cleanup */
        OBJ_RELEASE(proc_data);
        return ORTE_SUCCESS;
    }

    /* remove this item */
    for (kv = (orte_db_keyval_t*) opal_list_get_first(&proc_data->data);
         kv != (orte_db_keyval_t*) opal_list_get_end(&proc_data->data);
         kv = (orte_db_keyval_t*) opal_list_get_next(kv)) {
        if (0 == strcmp(key, kv->key)) {
            OBJ_RELEASE(kv);
            break;
        }
    }

    return ORTE_SUCCESS;
}

