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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/proc/proc.h"
#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "orte/class/orte_proc_table.h"

#include "orte/dss/dss.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/ns/ns.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/runtime/ompi_module_exchange.h"


/**
 * @file
 * 
 * MODEX DESIGN
 *
 * Modex data is always associated with a given orte process name, in
 * an orte hash table.  A backpointer is kept on an ompi_proc_t for
 * fast access.  The hash table is necessary because modex data is
 * received from the GPR for entire jobids and when working with
 * dynamic processes, it is possible we will receive data for a
 * process not yet in the ompi_proc_all() list of process. This
 * information must be kept for later use, because if accept/connect
 * causes the proc to be added to the ompi_proc_all() list, the
 * subscription to the mdoex information can not be reliably fired
 * without causing a potential connection storm.  Therefore, we use an
 * orte_proc_table backing store to contain all modex information.
 * Backpointers are provided from the ompi_proc_t structure to improve
 * lookup performance in the common case.
 *
 * While we could add the now discovered proc into the ompi_proc_all()
 * list, this has some problems, in that we don't have the
 * architecture and hostname information needed to properly fill in
 * the ompi_proc_t structure and we don't want to cause GPR
 * communication to get it when we dont' really need to know anything
 * about the remote proc.
 *
 * All data put into the modex (or received from the modex) is
 * associated with a given proc,component pair.  The data structures
 * to maintain this data look something like:
 *
 * orte_hash_table_t ompi_modex_data -> list of ompi_modex_proc_t objects
 * 
 * +-----------------------------+
 * | ompi_modex_proc_data_t      |
 * |  - opal_list_item_t         |
 * +-----------------------------+
 * | opal_mutex_t modex_lock     |
 * | opal_condition_t modex_cond |
 * | bool modex_received_data    |     1
 * | opal_list_t modules         |     ---------+
 * +-----------------------------+              |
 *                                      *       |
 * +--------------------------------+  <--------+
 * | ompi_modex_module_data_t       |
 * |  - opal_list_item_t            |
 * +--------------------------------+
 * | mca_base_component_t component |
 * | void *module_data              |
 * | size_t module_data_size        | 1
 * | opal_list_t module_cbs         | ---------+
 * +--------------------------------+          |
 *                                     *       |
 * +---------------------------+      <--------+
 * | ompi_modex_cb_t           |
 * |  - opal_list_item_t       |
 * +---------------------------+
 * | ompi_modex_cb_fn_t cbfunc |
 * | void *cbdata              |
 * +---------------------------+
 *
 * In order to maintain subscriptions to the registry for modex
 * information, a list of all active subscriptions is maintained as a
 * list (ompi_modex_subscriptions) of ompi_modex_subscription_t
 * structures.  The structure contains the jobid used in the
 * subscription.
 */


/**
 * Modex data for a particular orte process
 *
 * Locking infrastructure and list of module data for a given orte
 * process name.  The name association is maintained in the
 * ompi_modex_proc_list hash table.
 */
struct ompi_modex_proc_data_t {
    /** Structure can be put on lists (including in hash tables) */
    opal_list_item_t super;
    /* Lock held whenever the modex data for this proc is being
       modified */
    opal_mutex_t modex_lock;
    /* Condition variable used when blocking on data from this
       process.  Should be signalled whenever data is updated for this
       process. */
    opal_condition_t modex_cond;
    /* True if modex data has ever been received from this process,
       false otherwise. */
    bool modex_received_data;
    /* List of ompi_modex_module_data_t structures containing all data
       received from this process, sorted by component name. */
    opal_list_t modex_module_data;
};
typedef struct ompi_modex_proc_data_t ompi_modex_proc_data_t;

static void
ompi_modex_construct(ompi_modex_proc_data_t * modex)
{
    OBJ_CONSTRUCT(&modex->modex_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&modex->modex_cond, opal_condition_t);
    modex->modex_received_data = false;
    OBJ_CONSTRUCT(&modex->modex_module_data, opal_list_t);
}

static void
ompi_modex_destruct(ompi_modex_proc_data_t * modex)
{
    OBJ_DESTRUCT(&modex->modex_module_data);
    OBJ_DESTRUCT(&modex->modex_cond);
    OBJ_DESTRUCT(&modex->modex_lock);
}

OBJ_CLASS_INSTANCE(ompi_modex_proc_data_t, opal_object_t,
		   ompi_modex_construct, ompi_modex_destruct);



/**
 * Modex data for a particular component name
 *
 * Container for data for a particular proc,component pair.  This
 * structure should be contained in the modules list in an
 * ompi_modex_proc_data_t structure to maintain an association with a
 * given proc.  The list is then searched for a matching component
 * name.
 *
 * While searching the list or reading from (or writing to) this
 * structure, the lock in the proc_data_t should be held.
 */
struct ompi_modex_module_data_t {
    /** Structure can be put on lists */
    opal_list_item_t super;
    /** Component information for this data */
    mca_base_component_t component;
    /** Binary blob of data associated with this proc,component pair */
    void *module_data;
    /** Size (in bytes) of module_data */
    size_t module_data_size;
    /** callbacks that should be fired when module_data changes. */
    opal_list_t module_cbs;
};
typedef struct ompi_modex_module_data_t ompi_modex_module_data_t;

static void
ompi_modex_module_construct(ompi_modex_module_data_t * module)
{
    memset(&module->component, 0, sizeof(module->component));
    module->module_data = NULL;
    module->module_data_size = 0;
    OBJ_CONSTRUCT(&module->module_cbs, opal_list_t);
}

static void
ompi_modex_module_destruct(ompi_modex_module_data_t * module)
{
    opal_list_item_t *item;
    while (NULL != (item = opal_list_remove_first(&module->module_cbs))) {
	OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&module->module_cbs);
}

OBJ_CLASS_INSTANCE(ompi_modex_module_data_t,
		   opal_list_item_t,
		   ompi_modex_module_construct,
		   ompi_modex_module_destruct);

/**
 * Callback data for modex updates
 *
 * Data container for update callbacks that should be fired whenever a
 * given proc,component pair has a modex data update.
 */
struct ompi_modex_cb_t {
    opal_list_item_t super;
    ompi_modex_cb_fn_t cbfunc;
    void *cbdata;
};
typedef struct ompi_modex_cb_t ompi_modex_cb_t;

OBJ_CLASS_INSTANCE(ompi_modex_cb_t,
		   opal_list_item_t,
		   NULL,
		   NULL);



/**
 * Container for segment subscription data
 *
 * Track segments we have subscribed to.  Any jobid segment we are
 * subscribed to for updates will have one of these containers,
 * hopefully put on the ompi_modex_subscriptions list.
 */
struct ompi_modex_subscription_t {
    opal_list_item_t item;
    orte_jobid_t jobid;
};
typedef struct ompi_modex_subscription_t ompi_modex_subscription_t;

OBJ_CLASS_INSTANCE(ompi_modex_subscription_t,
		   opal_list_item_t,
		   NULL,
		   NULL);


/**
 * Global modex list for tracking subscriptions
 *
 * A list of ompi_modex_subscription_t structures, each representing a
 * jobid to which we have subscribed for modex updates.
 *
 * \note The ompi_modex_lock mutex should be held whenever this list
 * is being updated or searched.
 */
static opal_list_t ompi_modex_subscriptions;


/**
 * Global modex list of proc data
 *
 * Global bhash table associating orte_process_name_t values with an
 * ompi_modex_proc_data_t container.
 *
 * \note The ompi_modex_lock mutex should be held whenever this list
 * is being updated or searched.
 */
static opal_hash_table_t ompi_modex_data; 

/**
 * Global modex lock
 *
 * Global lock for modex usage, particularily protecting the
 * ompi_modex_subscriptions list and the ompi_modex_data hash table.
 */
static opal_mutex_t ompi_modex_lock;


int
ompi_modex_init(void)
{
    OBJ_CONSTRUCT(&ompi_modex_data, opal_hash_table_t);
    OBJ_CONSTRUCT(&ompi_modex_subscriptions, opal_list_t);
    OBJ_CONSTRUCT(&ompi_modex_lock, opal_mutex_t);

    opal_hash_table_init(&ompi_modex_data, 256);

    return OMPI_SUCCESS;
}


int
ompi_modex_finalize(void)
{
    opal_list_item_t *item;

    opal_hash_table_remove_all(&ompi_modex_data);
    OBJ_DESTRUCT(&ompi_modex_data);

    while (NULL != (item = opal_list_remove_first(&ompi_modex_subscriptions)))
	OBJ_RELEASE(item);
    OBJ_DESTRUCT(&ompi_modex_subscriptions);

    OBJ_DESTRUCT(&ompi_modex_lock);

    return OMPI_SUCCESS;
}


/**
 * Find data for a given component in a given modex_proc_data_t
 * container.
 *
 * Find data for a given component in a given modex_proc_data_t
 * container.  The proc_data's modex_lock must be held during this
 * search.
 */
static ompi_modex_module_data_t *
ompi_modex_lookup_module(ompi_modex_proc_data_t *proc_data,
                         mca_base_component_t *component,
                         bool create_if_not_found)
{
    ompi_modex_module_data_t *module_data = NULL;
    for (module_data = (ompi_modex_module_data_t *) opal_list_get_first(&proc_data->modex_module_data);
	 module_data != (ompi_modex_module_data_t *) opal_list_get_end(&proc_data->modex_module_data);
	 module_data = (ompi_modex_module_data_t *) opal_list_get_next(module_data)) {
	if (mca_base_component_compatible(&module_data->component, component) == 0) {
	    return module_data;
	}
    }

    if (create_if_not_found) {
        module_data = OBJ_NEW(ompi_modex_module_data_t);
        if (NULL == module_data) return NULL;

        memcpy(&module_data->component, component, sizeof(mca_base_component_t));
        opal_list_append(&proc_data->modex_module_data, &module_data->super);

        return module_data;
    }

    return NULL;
}


/**
 * Find ompi_modex_proc_data_t container associated with given
 * orte_process_name_t.
 *
 * Find ompi_modex_proc_data_t container associated with given
 * orte_process_name_t.  The global lock should *NOT* be held when
 * calling this function.
 */
static ompi_modex_proc_data_t*
ompi_modex_lookup_orte_proc(orte_process_name_t *orte_proc)
{
    ompi_modex_proc_data_t *proc_data;

    OPAL_THREAD_LOCK(&ompi_modex_lock);
    proc_data = (ompi_modex_proc_data_t*)
        orte_hash_table_get_proc(&ompi_modex_data, orte_proc);
    if (NULL == proc_data) {
        /* The proc clearly exists, so create a modex structure
           for it and try to subscribe */
        proc_data = OBJ_NEW(ompi_modex_proc_data_t);
        if (NULL == proc_data) {
            opal_output(0, "ompi_modex_lookup_orte_proc: unable to allocate ompi_modex_proc_data_t\n");
            OPAL_THREAD_UNLOCK(&ompi_modex_lock);
            return NULL;
        }
        orte_hash_table_set_proc(&ompi_modex_data, orte_proc, proc_data);
    }
    OPAL_THREAD_UNLOCK(&ompi_modex_lock);

    return proc_data;
}


/**
 * Find ompi_modex_proc_data_t container associated with given ompi_proc_t
 *
 * Find ompi_modex_proc_data_t container associated with given
 * ompi_proc_t.  The global lock should *NOT* be held when calling
 * this function.
 */
static ompi_modex_proc_data_t*
ompi_modex_lookup_proc(ompi_proc_t *proc)
{
    ompi_modex_proc_data_t *proc_data =
        (ompi_modex_proc_data_t *) proc->proc_modex;

    if (NULL == proc_data) {
        proc_data = ompi_modex_lookup_orte_proc(&proc->proc_name);
        if (NULL == proc_data) return NULL;

        /* set the association with the ompi_proc, if not already done. */
        OPAL_THREAD_LOCK(&ompi_modex_lock);
        if (NULL == proc->proc_modex) {
            OBJ_RETAIN(proc_data);
            proc->proc_modex = &proc_data->super.super;
            /* verify that we have subscribed to this segment */
            ompi_modex_subscribe_job(proc->proc_name.jobid);
        } 
        OPAL_THREAD_LOCK(&ompi_modex_lock);
    }

    return proc_data;
}


/**
 *  Callback for registry notifications.
 */
static void
ompi_modex_registry_callback(orte_gpr_notify_data_t * data,
                             void *cbdata)
{
    orte_std_cntr_t i, j, k;
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t **keyval;
    orte_process_name_t *proc_name;
    ompi_modex_proc_data_t *proc_data;
    ompi_modex_module_data_t *module_data;
    mca_base_component_t component;
    int rc;

    /* process the callback */
    values = (orte_gpr_value_t **) (data->values)->addr;
    for (i = 0, k = 0; k < data->cnt &&
                       i < (data->values)->size; i++) {
        if (NULL != values[i]) {
            k++;
            value = values[i];
            if (0 < value->cnt) {      /* needs to be at least one keyval */
                /* Find the process name in the keyvals */
                keyval = value->keyvals;
                for (j = 0; j < value->cnt; j++) {
                    if (0 != strcmp(keyval[j]->key, ORTE_PROC_NAME_KEY)) continue;
                    /* this is the process name - extract it */
                    if (ORTE_SUCCESS != orte_dss.get((void**)&proc_name, keyval[j]->value, ORTE_NAME)) {
                        opal_output(0, "ompi_modex_registry_callback: unable to extract process name\n");
                        return;  /* nothing we can do */
                    }
                    goto GOTNAME;
                }
                opal_output(0, "ompi_modex_registry_callback: unable to find process name in notify message\n");
                return;  /* if the name wasn't here, there is nothing we can do */
GOTNAME:
                /* look up the modex data structure */
                proc_data = ompi_modex_lookup_orte_proc(proc_name);
                if (proc_data == NULL) continue;

                OPAL_THREAD_LOCK(&proc_data->modex_lock);

                /*
                 * Extract the component name and version from the keyval object's key
                 * Could be multiple keyvals returned since there is one for each
                 * component type/name/version - process them all
                 */
                keyval = value->keyvals;
                for (j = 0; j < value->cnt; j++) {
                    orte_buffer_t buffer;
                    opal_list_item_t *item;
                    char *ptr;
                    void *bytes = NULL;
                    orte_std_cntr_t cnt;
                    size_t num_bytes;
                    orte_byte_object_t *bo;

                    if (strcmp(keyval[j]->key, OMPI_MODEX_KEY) != 0)
                        continue;

                    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void **) &bo, keyval[j]->value, ORTE_BYTE_OBJECT))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
                    if (ORTE_SUCCESS != (rc = orte_dss.load(&buffer, bo->bytes, bo->size))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
                    cnt = 1;
                    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buffer, &ptr, &cnt, ORTE_STRING))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
                    strcpy(component.mca_type_name, ptr);
                    free(ptr);

                    cnt = 1;
                    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buffer, &ptr, &cnt, ORTE_STRING))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
                    strcpy(component.mca_component_name, ptr);
                    free(ptr);

                    cnt = 1;
                    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buffer,
                                      &component.mca_component_major_version, &cnt, ORTE_INT32))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
                    cnt = 1;
                    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buffer,
                                      &component.mca_component_minor_version, &cnt, ORTE_INT32))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
                    cnt = 1;
                    if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buffer, &num_bytes, &cnt, ORTE_SIZE))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
                    if (num_bytes != 0) {
                        if (NULL == (bytes = malloc(num_bytes))) {
                            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                            continue;
                        }
                        cnt = (orte_std_cntr_t) num_bytes;
                        if (ORTE_SUCCESS != (rc = orte_dss.unpack(&buffer, bytes, &cnt, ORTE_BYTE))) {
                            ORTE_ERROR_LOG(rc);
                            continue;
                        }
                        num_bytes = cnt;
                    } else {
                        bytes = NULL;
                    }

                    /*
                     * Lookup the corresponding modex structure
                     */
                    if (NULL == (module_data = ompi_modex_lookup_module(proc_data, 
                                                                        &component, 
                                                                        true))) {
                        opal_output(0, "ompi_modex_registry_callback: ompi_modex_lookup_module failed\n");
                        OBJ_RELEASE(data);
                        OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
                        return;
                    }
                    module_data->module_data = bytes;
                    module_data->module_data_size = num_bytes;
                    proc_data->modex_received_data = true;
                    opal_condition_signal(&proc_data->modex_cond);

                    if (opal_list_get_size(&module_data->module_cbs)) {
                        ompi_proc_t *proc = ompi_proc_find(proc_name);

                        if (NULL != proc) {
                            OPAL_THREAD_LOCK(&proc->proc_lock);
                            /* call any registered callbacks */
                            for (item = opal_list_get_first(&module_data->module_cbs);
                                 item != opal_list_get_end(&module_data->module_cbs);
                                 item = opal_list_get_next(item)) {
                                ompi_modex_cb_t *cb = (ompi_modex_cb_t *) item;
                                cb->cbfunc(&module_data->component, 
                                           proc, bytes, num_bytes, cb->cbdata);
                            }
                            OPAL_THREAD_UNLOCK(&proc->proc_lock);
                        }
                    }
                }
                OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
            } /* if value[i]->cnt > 0 */
        }  /* if value[i] != NULL */
    }
}


int
ompi_modex_subscribe_job(orte_jobid_t jobid)
{
    char *segment, *sub_name, *trig_name;
    orte_gpr_subscription_id_t sub_id;
    opal_list_item_t *item;
    ompi_modex_subscription_t *subscription;
    int rc;
    char *keys[] = {
        ORTE_PROC_NAME_KEY,
        OMPI_MODEX_KEY,
        NULL
    };

    /* check for an existing subscription */
    OPAL_THREAD_LOCK(&ompi_modex_lock);
    for (item = opal_list_get_first(&ompi_modex_subscriptions) ;
         item != opal_list_get_end(&ompi_modex_subscriptions) ;
         item = opal_list_get_next(item)) {
        subscription = (ompi_modex_subscription_t *) item;
        if (subscription->jobid == jobid) {
            OPAL_THREAD_UNLOCK(&ompi_modex_lock);
            return OMPI_SUCCESS;
	}
    }
    OPAL_THREAD_UNLOCK(&ompi_modex_lock);

    /* otherwise - subscribe to get this jobid's contact info */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_subscription_name(&sub_name,
				      OMPI_MODEX_SUBSCRIPTION, jobid))) {
	ORTE_ERROR_LOG(rc);
	return rc;
    }
    /* attach to the stage-1 standard trigger */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&trig_name,
					    ORTE_STG1_TRIGGER, jobid))) {
	ORTE_ERROR_LOG(rc);
	free(sub_name);
	return rc;
    }
    /* define the segment */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
	ORTE_ERROR_LOG(rc);
	free(sub_name);
	free(trig_name);
	return rc;
    }
    if (jobid != orte_process_info.my_name->jobid) {
        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_N(&sub_id, NULL, NULL,
                              ORTE_GPR_NOTIFY_ADD_ENTRY |
                              ORTE_GPR_NOTIFY_VALUE_CHG |
                              ORTE_GPR_NOTIFY_PRE_EXISTING,
                              ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR | ORTE_GPR_STRIPPED,
                              segment,
                              NULL,	/* look at all
                                     * containers on this
                                     * segment */
                              2, keys,
                              ompi_modex_registry_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(sub_name);
            free(trig_name);
            free(segment);
            return rc;
        }
    } else {
        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_N(&sub_id, trig_name, sub_name,
                              ORTE_GPR_NOTIFY_ADD_ENTRY |
                              ORTE_GPR_NOTIFY_VALUE_CHG |
                              ORTE_GPR_NOTIFY_STARTS_AFTER_TRIG,
                              ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR | ORTE_GPR_STRIPPED,
                              segment,
                              NULL,	/* look at all
                                     * containers on this
                                     * segment */
                              2, keys,
                              ompi_modex_registry_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(sub_name);
            free(trig_name);
            free(segment);
            return rc;
        }
    }
    free(sub_name);
    free(trig_name);
    free(segment);

    /* add this jobid to our list of subscriptions */
    OPAL_THREAD_LOCK(&ompi_modex_lock);
    subscription = OBJ_NEW(ompi_modex_subscription_t);
    subscription->jobid = jobid;
    opal_list_append(&ompi_modex_subscriptions, &subscription->item);
    OPAL_THREAD_UNLOCK(&ompi_modex_lock);
    return OMPI_SUCCESS;
}


int
ompi_modex_send(mca_base_component_t * source_component,
			const void *data,
			size_t size)
{
    orte_jobid_t jobid;
    int rc;
    orte_buffer_t buffer;
    orte_std_cntr_t i, num_tokens;
    char *ptr, *segment, **tokens;
    orte_byte_object_t bo;
    orte_data_value_t value = ORTE_DATA_VALUE_EMPTY;

    /* get location in GPR for the data */
    jobid = ORTE_PROC_MY_NAME->jobid;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
	ORTE_ERROR_LOG(rc);
	return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens,
			      &num_tokens, orte_process_info.my_name))) {
	ORTE_ERROR_LOG(rc);
	free(segment);
	return rc;
    }

    OBJ_CONSTRUCT(&buffer, orte_buffer_t);

    /* Pack the component name information into the buffer */
    ptr = source_component->mca_type_name;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &ptr, 1, ORTE_STRING))) {
	ORTE_ERROR_LOG(rc);
	goto cleanup;
    }
    ptr = source_component->mca_component_name;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &ptr, 1, ORTE_STRING))) {
	ORTE_ERROR_LOG(rc);
	goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &source_component->mca_component_major_version, 1, ORTE_INT32))) {
	ORTE_ERROR_LOG(rc);
	goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &source_component->mca_component_minor_version, 1, ORTE_INT32))) {
	ORTE_ERROR_LOG(rc);
	goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &size, 1, ORTE_SIZE))) {
	ORTE_ERROR_LOG(rc);
	goto cleanup;
    }

    /* Pack the actual data into the buffer */
    if (0 != size) {
	if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, (void *) data, size, ORTE_BYTE))) {
	    ORTE_ERROR_LOG(rc);
	    goto cleanup;
	}
    }
    if (ORTE_SUCCESS != (rc = orte_dss.unload(&buffer, (void **) &(bo.bytes), &(bo.size)))) {
	ORTE_ERROR_LOG(rc);
	goto cleanup;
    }
    OBJ_DESTRUCT(&buffer);

    /* setup the data_value structure to hold the byte object */
    if (ORTE_SUCCESS != (rc = orte_dss.set(&value, (void *) &bo, ORTE_BYTE_OBJECT))) {
	ORTE_ERROR_LOG(rc);
	goto cleanup;
    }

    /* Put data in registry */
    rc = orte_gpr.put_1(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
			segment, tokens, OMPI_MODEX_KEY, &value);

cleanup:
    free(segment);
    for (i = 0; i < num_tokens; i++) {
	free(tokens[i]);
	tokens[i] = NULL;
    }
    if (NULL != tokens)
	free(tokens);

    return rc;
}


int
ompi_modex_recv(mca_base_component_t * component,
                ompi_proc_t * proc,
                void **buffer,
                size_t * size)
{
    ompi_modex_proc_data_t *proc_data;
    ompi_modex_module_data_t *module_data;

    /* make sure we could possibly have modex data */
    if (0 == strcmp(orte_gpr_base_selected_component.gpr_version.mca_component_name,
                    "null")) {
        return OMPI_ERR_NOT_IMPLEMENTED;
    }

    proc_data = ompi_modex_lookup_proc(proc);
    if (NULL == proc_data) return OMPI_ERR_NOT_FOUND;

    OPAL_THREAD_LOCK(&proc_data->modex_lock);

    /* wait until data is available */
    while (proc_data->modex_received_data == false) {
	opal_condition_wait(&proc_data->modex_cond, &proc_data->modex_lock);
    }

    /* look up module */
    module_data = ompi_modex_lookup_module(proc_data, component, false);
    
    /* copy the data out to the user */
    if ((NULL == module_data) ||
        (module_data->module_data_size == 0)) {
	*buffer = NULL;
	*size = 0;
    } else {
	void *copy = malloc(module_data->module_data_size);
	if (copy == NULL) {
            OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}
	memcpy(copy, module_data->module_data, module_data->module_data_size);
	*buffer = copy;
	*size = module_data->module_data_size;
    }
    OPAL_THREAD_UNLOCK(&proc_data->modex_lock);

    return OMPI_SUCCESS;
}


int
ompi_modex_recv_nb(mca_base_component_t *component,
                   ompi_proc_t *proc,
                   ompi_modex_cb_fn_t cbfunc,
                   void *cbdata)
{
    ompi_modex_proc_data_t *proc_data;
    ompi_modex_module_data_t *module_data;
    ompi_modex_cb_t *cb;

    proc_data = ompi_modex_lookup_proc(proc);
    if (NULL == proc_data) return OMPI_ERR_NOT_FOUND;

    OPAL_THREAD_LOCK(&proc_data->modex_lock);

    /* lookup / create module */
    module_data = ompi_modex_lookup_module(proc_data, component, true);
    if (NULL == module_data) {
	OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* register the callback */
    cb = OBJ_NEW(ompi_modex_cb_t);
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;
    opal_list_append(&module_data->module_cbs, &cb->super);
    OPAL_THREAD_UNLOCK(&proc_data->modex_lock);

    return OMPI_SUCCESS;
}
