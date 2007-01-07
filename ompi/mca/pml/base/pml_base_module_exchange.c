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
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
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
#include "ompi/mca/pml/base/pml_base_module_exchange.h"

/* MODEX DESIGN
 *
 * Modex data is always associated with a given ompi_proc_t.  However,
 * because modex data is received from the GPR for entire jobids, it
 * is possible that the modex callback will receive data for a process
 * not yet in the ompi_proc_all() list of processes.  This information
 * must be kept for later use, because if accept/connect causes the
 * proc to be added to the ompi_proc_all() list, the subscription to
 * the mdoex information can not be reliably fired without causing a
 * potential connection storm.  Therefore, we use an orte_proc_table
 * backing store to contain all modex information.  Backpointers are
 * provided from the ompi_proc_t structure to improve lookup
 * performance in the common case.
 *
 * While we could add the now discovered proc into the ompi_proc_all()
 * list, this has some problems, in that we don't have the
 * architecture and hostname information needed to properly fill in
 * the ompi_proc_t structure and we don't want to cause GPR
 * communication to get it when we dont' really need to know anything
 * about the remote proc.
 *
 */

/**
 * callback data for modex
 */
struct mca_pml_base_modex_cb_t {
    opal_list_item_t super;
    mca_base_component_t *component;
    mca_pml_base_modex_cb_fn_t cbfunc;
    void *cbdata;
};
typedef struct mca_pml_base_modex_cb_t mca_pml_base_modex_cb_t;

OBJ_CLASS_INSTANCE(mca_pml_base_modex_cb_t,
		   opal_list_item_t,
		   NULL,
		   NULL);


/**
 * mca_pml_base_modex_module_t
 *
 * Data for a specic proc and module.
 */
struct mca_pml_base_modex_module_t {
    opal_list_item_t super;
    mca_base_component_t component;
    void *module_data;
    size_t module_data_size;
    bool module_data_avail;
    opal_list_t module_cbs;
    opal_condition_t module_data_cond;
};
typedef struct mca_pml_base_modex_module_t mca_pml_base_modex_module_t;

static void
mca_pml_base_modex_module_construct(mca_pml_base_modex_module_t * module)
{
    OBJ_CONSTRUCT(&module->module_data_cond, opal_condition_t);
    OBJ_CONSTRUCT(&module->module_cbs, opal_list_t);
    memset(&module->component, 0, sizeof(module->component));
    module->module_data = NULL;
    module->module_data_size = 0;
    module->module_data_avail = false;
}

static void
mca_pml_base_modex_module_destruct(mca_pml_base_modex_module_t * module)
{
    OBJ_DESTRUCT(&module->module_data_cond);
}

OBJ_CLASS_INSTANCE(mca_pml_base_modex_module_t,
		   opal_list_item_t,
		   mca_pml_base_modex_module_construct,
		   mca_pml_base_modex_module_destruct);


/**
 * mca_pml_base_modex_t
 *
 * List of modules (mca_pml_base_modex_module_t) for which data has been
 * received from peers.
 */
struct mca_pml_base_modex_t {
    opal_list_item_t super;
    opal_mutex_t modex_lock;
    opal_list_t modex_modules;
};
typedef struct mca_pml_base_modex_t mca_pml_base_modex_t;

static void
mca_pml_base_modex_construct(mca_pml_base_modex_t * modex)
{
    OBJ_CONSTRUCT(&modex->modex_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&modex->modex_modules, opal_list_t);
}

static void
mca_pml_base_modex_destruct(mca_pml_base_modex_t * modex)
{
    OBJ_DESTRUCT(&modex->modex_modules);
    OBJ_DESTRUCT(&modex->modex_lock);
}

OBJ_CLASS_INSTANCE(mca_pml_base_modex_t,
		   opal_object_t,
		   mca_pml_base_modex_construct,
		   mca_pml_base_modex_destruct);


/**
 * mca_pml_base_modex_subscription_t
 *
 * Track segments we have subscribed to.
 */
struct mca_pml_base_modex_subscription_t {
    opal_list_item_t item;
    orte_jobid_t jobid;
};
typedef struct mca_pml_base_modex_subscription_t mca_pml_base_modex_subscription_t;

OBJ_CLASS_INSTANCE(mca_pml_base_modex_subscription_t,
		   opal_list_item_t,
		   NULL,
		   NULL);


/**
 * Globals to track the list of subscriptions.
 */
static opal_list_t mca_pml_base_modex_subscriptions;
static opal_hash_table_t mca_pml_base_modex_data;
static opal_mutex_t mca_pml_base_modex_lock;


/**
 * Initialize global state.
 */
int
mca_pml_base_modex_init(void)
{
    OBJ_CONSTRUCT(&mca_pml_base_modex_data, opal_hash_table_t);
    OBJ_CONSTRUCT(&mca_pml_base_modex_subscriptions, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_base_modex_lock, opal_mutex_t);

    opal_hash_table_init(&mca_pml_base_modex_data, 256);

    return OMPI_SUCCESS;
}


/**
 * Cleanup global state.
 */
int
mca_pml_base_modex_finalize(void)
{
    opal_list_item_t *item;

    opal_hash_table_remove_all(&mca_pml_base_modex_data);
    OBJ_DESTRUCT(&mca_pml_base_modex_data);

    while (NULL != (item = opal_list_remove_first(&mca_pml_base_modex_subscriptions)))
	OBJ_RELEASE(item);
    OBJ_DESTRUCT(&mca_pml_base_modex_subscriptions);

    return OMPI_SUCCESS;
}


/**
 *  Look to see if there is any data associated with a specified module.
 */
static mca_pml_base_modex_module_t *
mca_pml_base_modex_lookup_module(mca_pml_base_modex_t * modex,
				 mca_base_component_t * component)
{
    mca_pml_base_modex_module_t *modex_module;
    for (modex_module = (mca_pml_base_modex_module_t *) opal_list_get_first(&modex->modex_modules);
	 modex_module != (mca_pml_base_modex_module_t *) opal_list_get_end(&modex->modex_modules);
	 modex_module = (mca_pml_base_modex_module_t *) opal_list_get_next(modex_module)) {
	if (mca_base_component_compatible(&modex_module->component, component) == 0) {
	    return modex_module;
	}
    }
    return NULL;
}


/**
 *  Create a placeholder for data associated with the specified module.
 */
static mca_pml_base_modex_module_t *
mca_pml_base_modex_create_module(mca_pml_base_modex_t * modex,
				 mca_base_component_t * component)
{
    mca_pml_base_modex_module_t *modex_module;
    if (NULL == (modex_module = mca_pml_base_modex_lookup_module(modex, component))) {
	modex_module = OBJ_NEW(mca_pml_base_modex_module_t);
	if (NULL != modex_module) {
	    modex_module->component = *component;
	    opal_list_append(&modex->modex_modules, (opal_list_item_t *) modex_module);
	}
    }
    return modex_module;
}


/**
 *  Callback for registry notifications.
 */
static void
mca_pml_base_modex_registry_callback(orte_gpr_notify_data_t * data,
				     void *cbdata)
{
    orte_std_cntr_t i, j, k;
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t **keyval;
    orte_process_name_t *proc_name;
    mca_pml_base_modex_t *modex;
    mca_pml_base_modex_module_t *modex_module;
    mca_base_component_t component;
    int rc;
    ompi_proc_t *proc;

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
                        opal_output(0, "mca_pml_base_modex_registry_callback: unable to extract process name\n");
                        return;  /* nothing we can do */
                    }
                    goto GOTNAME;
                }
                opal_output(0, "mca_pml_base_modex_registry_callback: unable to find process name in notify message\n");
                return;  /* if the name wasn't here, there is nothing we can do */
                
GOTNAME:
                /* look up the modex data structure */
                OPAL_THREAD_LOCK(&mca_pml_base_modex_lock);
                modex = orte_hash_table_get_proc(&mca_pml_base_modex_data, proc_name);
                if (modex == NULL) {
                    /* create a modex data structure for this proc */
                    modex = OBJ_NEW(mca_pml_base_modex_t);
                    if (NULL == modex) {
                        opal_output(0, "mca_pml_base_modex_registry_callback: unable to allocate mca_pml_base_modex_t\n");
                        OPAL_THREAD_UNLOCK(&mca_pml_base_modex_lock);
                        return;
                    }

                    orte_hash_table_set_proc(&mca_pml_base_modex_data, proc_name, modex);
                }
                OPAL_THREAD_UNLOCK(&mca_pml_base_modex_lock);
                OPAL_THREAD_LOCK(&modex->modex_lock);
                proc = NULL;

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
                    if (NULL == (modex_module = mca_pml_base_modex_create_module(modex, &component))) {
                        opal_output(0, "mca_pml_base_modex_registry_callback: mca_pml_base_modex_create_module failed\n");
                        OBJ_RELEASE(data);
                        OPAL_THREAD_UNLOCK(&modex->modex_lock);
                        OBJ_RELEASE(modex);
                        return;
                    }
                    modex_module->module_data = bytes;
                    modex_module->module_data_size = num_bytes;
                    modex_module->module_data_avail = true;
                    opal_condition_signal(&modex_module->module_data_cond);

                    if (opal_list_get_size(&modex_module->module_cbs)) {
                        if (NULL == proc) {
                            proc = ompi_proc_find(proc_name);
                        }

                        if (NULL != proc) {
                            OPAL_THREAD_LOCK(&proc->proc_lock);
                            /* call any registered callbacks */
                            for (item = opal_list_get_first(&modex_module->module_cbs);
                                 item != opal_list_get_end(&modex_module->module_cbs);
                                 item = opal_list_get_next(item)) {
                                mca_pml_base_modex_cb_t *cb = (mca_pml_base_modex_cb_t *) item;
                                cb->cbfunc(cb->component, proc, bytes, num_bytes, cb->cbdata);
                            }
                            OPAL_THREAD_UNLOCK(&proc->proc_lock);
                        }
                    }
                }
                OPAL_THREAD_UNLOCK(&modex->modex_lock);
            } /* if value[i]->cnt > 0 */
        }  /* if value[i] != NULL */
    }
}

/**
 * Make sure we have subscribed to this segment.
 */

static int
mca_pml_base_modex_subscribe(orte_process_name_t * name)
{
    char *segment, *sub_name, *trig_name;
    orte_gpr_subscription_id_t sub_id;
    orte_jobid_t jobid;
    opal_list_item_t *item;
    mca_pml_base_modex_subscription_t *subscription;
    int rc;
    char *keys[] = {
        ORTE_PROC_NAME_KEY,
        OMPI_MODEX_KEY,
        NULL
    };

    /* check for an existing subscription */
    OPAL_LOCK(&mca_pml_base_modex_lock);
    if (!opal_list_is_empty(&mca_pml_base_modex_subscriptions)) {
	for (item = opal_list_get_first(&mca_pml_base_modex_subscriptions);
	     item != opal_list_get_end(&mca_pml_base_modex_subscriptions);
	     item = opal_list_get_next(item)) {
	    subscription = (mca_pml_base_modex_subscription_t *) item;
	    if (subscription->jobid == name->jobid) {
		OPAL_UNLOCK(&mca_pml_base_modex_lock);
		return OMPI_SUCCESS;
	    }
	}
    }
    OPAL_UNLOCK(&mca_pml_base_modex_lock);

    /* otherwise - subscribe to get this jobid's contact info */
    jobid = name->jobid;

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
                              mca_pml_base_modex_registry_callback, NULL))) {
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
                              mca_pml_base_modex_registry_callback, NULL))) {
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
    OPAL_LOCK(&mca_pml_base_modex_lock);
    subscription = OBJ_NEW(mca_pml_base_modex_subscription_t);
    subscription->jobid = name->jobid;
    opal_list_append(&mca_pml_base_modex_subscriptions, &subscription->item);
    OPAL_UNLOCK(&mca_pml_base_modex_lock);
    return OMPI_SUCCESS;
}


/**
 *  Store the data associated with the specified module in the
 *  gpr. Note that the gpr is in a mode where it caches
 *  individual puts during startup and sends them as an aggregate
 *  command.
 */

int
mca_pml_base_modex_send(mca_base_component_t * source_component,
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


/**
 *  Retreive the data for the specified module from the source process.
 */

int
mca_pml_base_modex_recv(mca_base_component_t * component,
			ompi_proc_t * proc,
			void **buffer,
			size_t * size)
{
    mca_pml_base_modex_t *modex;
    mca_pml_base_modex_module_t *modex_module;

    /* check the proc for cached data */
    if (NULL == (modex = (mca_pml_base_modex_t *) proc->proc_modex)) {
        /* see if we already have data for this proc... */
        OPAL_THREAD_LOCK(&mca_pml_base_modex_lock);
        modex = orte_hash_table_get_proc(&mca_pml_base_modex_data, &proc->proc_name);
        if (NULL == modex) {
            /* create an empty modex data... */
            modex = OBJ_NEW(mca_pml_base_modex_t);
            if (NULL == modex) {
                opal_output(0, "mca_pml_base_modex_recv: unable to allocate mca_pml_base_modex_t\n");
                OPAL_THREAD_UNLOCK(&mca_pml_base_modex_lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            orte_hash_table_set_proc(&mca_pml_base_modex_data, &proc->proc_name, modex);
            OBJ_RETAIN(modex);
            proc->proc_modex = &modex->super.super;
            OPAL_THREAD_UNLOCK(&mca_pml_base_modex_lock);
            /* verify that we have subscribed to this segment */
            mca_pml_base_modex_subscribe(&proc->proc_name);
        } else {
            /* create a backpointer from the proc to the modex data */
            OBJ_RETAIN(modex);
            proc->proc_modex = &modex->super.super;
            OPAL_THREAD_UNLOCK(&mca_pml_base_modex_lock);
        }
    }
    OPAL_THREAD_LOCK(&modex->modex_lock);

    /* lookup/create the module */
    if (NULL == (modex_module = mca_pml_base_modex_create_module(modex, component))) {
	OPAL_THREAD_UNLOCK(&modex->modex_lock);
	return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* wait until data is available */
    while (modex_module->module_data_avail == false) {
	opal_condition_wait(&modex_module->module_data_cond, &modex->modex_lock);
    }

    /* copy the data out to the user */
    if (modex_module->module_data_size == 0) {
	*buffer = NULL;
	*size = 0;
    } else {
	void *copy = malloc(modex_module->module_data_size);
	if (copy == NULL) {
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}
	memcpy(copy, modex_module->module_data, modex_module->module_data_size);
	*buffer = copy;
	*size = modex_module->module_data_size;
    }
    OPAL_THREAD_UNLOCK(&modex->modex_lock);
    return OMPI_SUCCESS;
}


/**
 *
 */

int
mca_pml_base_modex_recv_nb(mca_base_component_t * component,
			   ompi_proc_t * proc,
			   mca_pml_base_modex_cb_fn_t cbfunc,
			   void *cbdata)
{
    mca_pml_base_modex_t *modex;
    mca_pml_base_modex_module_t *module;
    mca_pml_base_modex_cb_t *cb;

    /* check the proc for cached data */
    if (NULL == (modex = (mca_pml_base_modex_t *) proc->proc_modex)) {
        /* see if we already have data for this proc... */
        OPAL_THREAD_LOCK(&mca_pml_base_modex_lock);
        modex = orte_hash_table_get_proc(&mca_pml_base_modex_data, &proc->proc_name);
        if (NULL == modex) {
            /* create an empty modex data... */
            modex = OBJ_NEW(mca_pml_base_modex_t);
            if (NULL == modex) {
                opal_output(0, "mca_pml_base_modex_recv: unable to allocate mca_pml_base_modex_t\n");
                OPAL_THREAD_UNLOCK(&mca_pml_base_modex_lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            orte_hash_table_set_proc(&mca_pml_base_modex_data, &proc->proc_name, modex);
            OBJ_RETAIN(modex);
            proc->proc_modex = &modex->super.super;
            OPAL_THREAD_UNLOCK(&mca_pml_base_modex_lock);
            /* verify that we have subscribed to this segment */
            mca_pml_base_modex_subscribe(&proc->proc_name);
        } else {
            /* create a backpointer from the proc to the modex data */
            OBJ_RETAIN(modex);
            proc->proc_modex = &modex->super.super;
            OPAL_THREAD_UNLOCK(&mca_pml_base_modex_lock);
        }
    }
    OPAL_THREAD_LOCK(&modex->modex_lock);

    /* lookup/create the module */
    if (NULL == (module = mca_pml_base_modex_create_module(modex, component))) {
	OPAL_THREAD_UNLOCK(&modex->modex_lock);
	return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* register the callback */
    cb = OBJ_NEW(mca_pml_base_modex_cb_t);
    cb->component = component;
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;
    opal_list_append(&module->module_cbs, (opal_list_item_t *) cb);
    OPAL_THREAD_UNLOCK(&modex->modex_lock);
    return OMPI_SUCCESS;
}


/**
 * Subscribe to the segment corresponding
 * to this job.
 */

int
mca_pml_base_modex_exchange(void)
{
    return mca_pml_base_modex_subscribe(orte_process_info.my_name);
}
