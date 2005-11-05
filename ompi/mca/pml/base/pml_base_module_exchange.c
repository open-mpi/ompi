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

#include "ompi_config.h"

#include "ompi/proc/proc.h"
#include "opal/threads/condition.h"
#include "opal/util/output.h"
#include "util/proc_info.h"

#include "dps/dps.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"
#include "mca/schema/schema.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"
#include "mca/ns/ns.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/pml_base_module_exchange.h"
#include "runtime/runtime.h"

/**
 *
 */

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
    opal_condition_t module_data_cond;
};
typedef struct mca_pml_base_modex_module_t mca_pml_base_modex_module_t;

static void mca_pml_base_modex_module_construct(mca_pml_base_modex_module_t *module)
{
    OBJ_CONSTRUCT(&module->module_data_cond, opal_condition_t);
    memset(&module->component, 0, sizeof(module->component));
    module->module_data = NULL;
    module->module_data_size = 0;
    module->module_data_avail = false;
}

static void mca_pml_base_modex_module_destruct(mca_pml_base_modex_module_t *module)
{
    OBJ_DESTRUCT(&module->module_data_cond);
}

OBJ_CLASS_INSTANCE(
    mca_pml_base_modex_module_t,
    opal_list_item_t,
    mca_pml_base_modex_module_construct,
    mca_pml_base_modex_module_destruct
);

/**
 * mca_pml_base_modex_t
 *
 * List of modules (mca_pml_base_modex_module_t) for which data has been
 * received from peers.
 */
struct mca_pml_base_modex_t {
    opal_object_t super;
    opal_list_t modex_modules;
};
typedef struct mca_pml_base_modex_t mca_pml_base_modex_t;

static void mca_pml_base_modex_construct(mca_pml_base_modex_t* modex)
{
    OBJ_CONSTRUCT(&modex->modex_modules, opal_list_t);
}

static void mca_pml_base_modex_destruct(mca_pml_base_modex_t* modex)
{
    OBJ_DESTRUCT(&modex->modex_modules);
}

OBJ_CLASS_INSTANCE(
    mca_pml_base_modex_t,
    opal_object_t,
    mca_pml_base_modex_construct,
    mca_pml_base_modex_destruct
);

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

OBJ_CLASS_INSTANCE(
    mca_pml_base_modex_subscription_t,
    opal_list_item_t,
    NULL,
    NULL);

/**
 * Globals to track the list of subscriptions.
 */

static opal_list_t  mca_pml_base_modex_subscriptions;
static opal_mutex_t mca_pml_base_modex_lock;


/**
 * Initialize global state.
 */
int mca_pml_base_modex_init(void)
{
    OBJ_CONSTRUCT(&mca_pml_base_modex_subscriptions, opal_list_t);
    OBJ_CONSTRUCT(&mca_pml_base_modex_lock, opal_mutex_t);
    return OMPI_SUCCESS;
}

/**
 * Cleanup global state.
 */
int mca_pml_base_modex_finalize(void)
{
    opal_list_item_t *item;
    while(NULL != (item = opal_list_remove_first(&mca_pml_base_modex_subscriptions)))
        OBJ_RELEASE(item);
    OBJ_DESTRUCT(&mca_pml_base_modex_subscriptions);
    return OMPI_SUCCESS;
}


/**
 *  Look to see if there is any data associated with a specified module.
 */

static mca_pml_base_modex_module_t* mca_pml_base_modex_lookup_module(
    mca_pml_base_modex_t* modex,
    mca_base_component_t* component)
{
    mca_pml_base_modex_module_t* modex_module;
    for(modex_module =  (mca_pml_base_modex_module_t*)opal_list_get_first(&modex->modex_modules);
        modex_module != (mca_pml_base_modex_module_t*)opal_list_get_end(&modex->modex_modules);
        modex_module =  (mca_pml_base_modex_module_t*)opal_list_get_next(modex_module)) {
        if(mca_base_component_compatible(&modex_module->component, component) == 0) {
            return modex_module;
        }
    }
    return NULL;
}


/**
 *  Create a placeholder for data associated with the specified module.
 */

static mca_pml_base_modex_module_t* mca_pml_base_modex_create_module(
    mca_pml_base_modex_t* modex,
    mca_base_component_t* component)
{
    mca_pml_base_modex_module_t* modex_module;
    if(NULL == (modex_module = mca_pml_base_modex_lookup_module(modex, component))) {
        modex_module = OBJ_NEW(mca_pml_base_modex_module_t);
        if(NULL != modex_module) {
            modex_module->component = *component;
            opal_list_append(&modex->modex_modules, (opal_list_item_t*)modex_module);
        }
    }
    return modex_module;
}


/**
 *  Callback for registry notifications.
 */

static void mca_pml_base_modex_registry_callback(
    orte_gpr_notify_data_t* data,
    void* cbdata)
{
    size_t i, j, k;
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t **keyval;
    ompi_proc_t *proc;
    ompi_proc_t **new_procs = NULL;
    size_t new_proc_count = 0;
    char **token;
    orte_process_name_t *proc_name;
    mca_pml_base_modex_t *modex;
    mca_pml_base_modex_module_t *modex_module;
    mca_base_component_t component;
    bool isnew = false;
    int rc;

    if(data->cnt) {
        new_procs = (ompi_proc_t**)malloc(sizeof(ompi_proc_t*) * data->cnt);
        if(NULL == new_procs) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return;
        }
    }

    /* process the callback */
    values = (orte_gpr_value_t**)(data->values)->addr;
    for (i=0, k=0; k < data->cnt &&
                   i < (data->values)->size; i++) {
        if (NULL != values[i]) {
            k++;
            value = values[i];
            if (0 < value->cnt) {  /* needs to be at least one keyval */
                /*
                 * Token for the value should be the process name - look it up
                 */
                token = value->tokens;
                if (ORTE_SUCCESS == orte_ns.convert_string_to_process_name(&proc_name, token[0])) {
                    proc = ompi_proc_find_and_add(proc_name, &isnew);
                    if(NULL == proc)
                        continue;

                    if(isnew) {
                        new_procs[new_proc_count] = proc;
                        new_proc_count++;
                    }

                    /*
                     * Lookup the modex data structure.
                     */

                    OPAL_THREAD_LOCK(&proc->proc_lock);
                    if(NULL == (modex = (mca_pml_base_modex_t*)proc->proc_modex)) {
                        modex = OBJ_NEW(mca_pml_base_modex_t);
                        if(NULL == modex) {
                            opal_output(0, "mca_pml_base_modex_registry_callback: unable to allocate mca_pml_base_modex_t\n");
                            OPAL_THREAD_UNLOCK(&proc->proc_lock);
                            return;
                        }
                        proc->proc_modex = &modex->super;
                    }

                    /*
                     * Extract the component name and version from the keyval object's key
                     * Could be multiple keyvals returned since there is one for each
                     * component type/name/version - process them all
                     */
                    keyval = value->keyvals;
                    for (j=0; j < value->cnt; j++) {
                        orte_buffer_t buffer;
                        char *ptr;
                        void* bytes = NULL;
                        size_t cnt;
                        size_t num_bytes;
                        if(strcmp(keyval[j]->key,OMPI_MODEX_KEY) != 0)
                            continue;

                        OBJ_CONSTRUCT(&buffer, orte_buffer_t);
                        if (ORTE_SUCCESS != (rc = orte_dps.load(&buffer,
                            keyval[j]->value.byteobject.bytes,
                            keyval[j]->value.byteobject.size))) {
                            ORTE_ERROR_LOG(rc);
                            continue;
                        }
                        cnt = 1;
                        if (ORTE_SUCCESS != (rc = orte_dps.unpack(&buffer, &ptr, &cnt, ORTE_STRING))) {
                            ORTE_ERROR_LOG(rc);
                            continue;
                        }
                        strcpy(component.mca_type_name,ptr);
                        free(ptr);

                        cnt = 1;
                        if (ORTE_SUCCESS != (rc = orte_dps.unpack(&buffer, &ptr, &cnt, ORTE_STRING))) {
                            ORTE_ERROR_LOG(rc);
                            continue;
                        }
                        strcpy(component.mca_component_name,ptr);
                        free(ptr);

                        cnt = 1;
                        if (ORTE_SUCCESS != (rc = orte_dps.unpack(&buffer,
                            &component.mca_component_major_version, &cnt, ORTE_INT32))) {
                            ORTE_ERROR_LOG(rc);
                            continue;
                        }
                        cnt = 1;
                        if (ORTE_SUCCESS != (rc = orte_dps.unpack(&buffer,
                            &component.mca_component_minor_version, &cnt, ORTE_INT32))) {
                            ORTE_ERROR_LOG(rc);
                            continue;
                        }
                        cnt = 1;
                        if (ORTE_SUCCESS != (rc = orte_dps.unpack(&buffer,
                            &num_bytes, &cnt, ORTE_SIZE))) {
                            ORTE_ERROR_LOG(rc);
                            continue;
                        }
                        if (num_bytes != 0) {
                            if(NULL == (bytes = malloc(num_bytes))) {
                                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                                continue;
                            }
                            if (ORTE_SUCCESS != (rc = orte_dps.unpack(&buffer, bytes, &num_bytes, ORTE_BYTE))) {
                                ORTE_ERROR_LOG(rc);
                                continue;
                            }
                        } else {
                            bytes = NULL;
                        }

                        /*
                         * Lookup the corresponding modex structure
                         */
                        if(NULL == (modex_module = mca_pml_base_modex_create_module(modex, &component))) {
                            opal_output(0, "mca_pml_base_modex_registry_callback: mca_pml_base_modex_create_module failed\n");
                            OBJ_RELEASE(data);
                            OPAL_THREAD_UNLOCK(&proc->proc_lock);
                            return;
                        }

                        modex_module->module_data = bytes;
                        modex_module->module_data_size = num_bytes;
                        modex_module->module_data_avail = true;
#if 0
opal_output(0, "[%lu,%lu,%lu] mca_pml_base_modex_registry_callback: %s-%s-%d-%d received %d bytes\n",
    ORTE_NAME_ARGS(orte_process_info.my_name),
    component.mca_type_name,
    component.mca_component_name,
    component.mca_component_major_version,
    component.mca_component_minor_version,
    num_bytes);
#endif
                        opal_condition_signal(&modex_module->module_data_cond);
                    }
                    OPAL_THREAD_UNLOCK(&proc->proc_lock);
                }  /* convert string to process name */

            }  /* if value[i]->cnt > 0 */
        }
    }

    /* pml add procs */
    if(NULL != new_procs) {
        if(new_proc_count > 0) {
            MCA_PML_CALL(add_procs(new_procs, new_proc_count));
        }
        free(new_procs);
    }
}

/**
 * Make sure we have subscribed to this segment.
 */

static int mca_pml_base_modex_subscribe(orte_process_name_t* name)
{
    char *segment, *sub_name, *trig_name;
    orte_gpr_subscription_id_t sub_id;
    orte_jobid_t jobid;
    opal_list_item_t* item;
    mca_pml_base_modex_subscription_t* subscription;
    int rc;

    /* check for an existing subscription */
    OPAL_LOCK(&mca_pml_base_modex_lock);
    if (!opal_list_is_empty(&mca_pml_base_modex_subscriptions)) {
        for(item =  opal_list_get_first(&mca_pml_base_modex_subscriptions);
            item != opal_list_get_end(&mca_pml_base_modex_subscriptions);
            item = opal_list_get_next(item)) {
            subscription = (mca_pml_base_modex_subscription_t*)item;
            if(subscription->jobid == name->jobid) {
                OPAL_UNLOCK(&mca_pml_base_modex_lock);
                return OMPI_SUCCESS;
            }
        }
    }
    OPAL_UNLOCK(&mca_pml_base_modex_lock);

    /* otherwise - subscribe to get this jobid's ptl contact info */
    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

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
        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&sub_id, NULL, NULL,
                                             ORTE_GPR_NOTIFY_ADD_ENTRY |
                                             ORTE_GPR_NOTIFY_VALUE_CHG |
                                             ORTE_GPR_NOTIFY_PRE_EXISTING,
                                             ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR,
                                             segment,
                                             NULL,  /* look at all containers on this segment */
                                             OMPI_MODEX_KEY,
                                             mca_pml_base_modex_registry_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(sub_name);
            free(trig_name);
            free(segment);
            return rc;
        }
    } else {
        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&sub_id, trig_name, sub_name,
                                             ORTE_GPR_NOTIFY_ADD_ENTRY |
                                             ORTE_GPR_NOTIFY_VALUE_CHG |
                                             ORTE_GPR_NOTIFY_STARTS_AFTER_TRIG,
                                             ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR,
                                             segment,
                                             NULL,  /* look at all containers on this segment */
                                             OMPI_MODEX_KEY,
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

int mca_pml_base_modex_send(
    mca_base_component_t *source_component,
    const void *data,
    size_t size)
{
    orte_jobid_t jobid;
    orte_gpr_value_union_t value;
    int rc;
    orte_buffer_t buffer;
    size_t i, num_tokens;
    char *ptr, *segment, **tokens;

    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

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


#if 0
    value.byteobject.size = size;
    value.byteobject.bytes = (void *)malloc(size);
    if(NULL == value.byteobject.bytes) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memcpy((value.byteobject.bytes, data, size));

    asprintf(&((value->keyvals[0])->key), "modex-%s-%s-%d-%d",
        source_component->mca_type_name,
        source_component->mca_component_name,
        source_component->mca_component_major_version,
        source_component->mca_component_minor_version);
#else
    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
    ptr = source_component->mca_type_name;
    if (ORTE_SUCCESS != (rc = orte_dps.pack(&buffer, &ptr, 1, ORTE_STRING))) {
        goto cleanup;
    }
    ptr = source_component->mca_component_name;
    if (ORTE_SUCCESS != (rc = orte_dps.pack(&buffer, &ptr, 1, ORTE_STRING))) {
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dps.pack(&buffer, &source_component->mca_component_major_version, 1, ORTE_INT32))) {
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dps.pack(&buffer, &source_component->mca_component_minor_version, 1, ORTE_INT32))) {
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dps.pack(&buffer, &size, 1, ORTE_SIZE))) {
        goto cleanup;
    }
    if (0 != size) {
        if (ORTE_SUCCESS != (rc = orte_dps.pack(&buffer, (void*)data, size, ORTE_BYTE))) {
            goto cleanup;
        }
    }
    if (ORTE_SUCCESS != (rc = orte_dps.unload(&buffer, (void**)&value.byteobject.bytes,
        (size_t*)&value.byteobject.size))) {
        goto cleanup;
    }
    OBJ_DESTRUCT(&buffer);
#endif

    rc = orte_gpr.put_1(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                        segment, tokens, OMPI_MODEX_KEY, ORTE_BYTE_OBJECT, value);

cleanup:
    free(segment);
    for (i=0; i < num_tokens; i++) {
        free(tokens[i]);
        tokens[i] = NULL;
    }
    if (NULL != tokens) free(tokens);

    return rc;
}


/**
 *  Retreive the data for the specified module from the source process.
 */

int mca_pml_base_modex_recv(
    mca_base_component_t *component,
    ompi_proc_t *proc,
    void **buffer,
    size_t *size)
{
    mca_pml_base_modex_t* modex;
    mca_pml_base_modex_module_t* modex_module;

    /* check the proc for cached data */
    OPAL_THREAD_LOCK(&proc->proc_lock);
    if(NULL == (modex = (mca_pml_base_modex_t*)proc->proc_modex)) {
        modex = OBJ_NEW(mca_pml_base_modex_t);
        if(modex == NULL) {
            OPAL_THREAD_UNLOCK(&proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        proc->proc_modex = &modex->super;

        /* verify that we have subscribed to this segment */
        OPAL_THREAD_UNLOCK(&proc->proc_lock);
        mca_pml_base_modex_subscribe(&proc->proc_name);
        OPAL_THREAD_LOCK(&proc->proc_lock);
    }

    /* lookup/create the module */
    if(NULL == (modex_module = mca_pml_base_modex_create_module(modex, component))) {
        OPAL_THREAD_UNLOCK(&proc->proc_lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* wait until data is available */
    while(modex_module->module_data_avail == false) {
#if 0
opal_output(0, "[%lu,%lu,%lu] mca_pml_base_modex_registry_callback: waiting for %s-%s-%d-%d\n",
    ORTE_NAME_ARGS(orte_process_info.my_name),
    component->mca_type_name,
    component->mca_component_name,
    component->mca_component_major_version,
    component->mca_component_minor_version);
#endif
        opal_condition_wait(&modex_module->module_data_cond, &proc->proc_lock);
    }

    /* copy the data out to the user */
    if(modex_module->module_data_size == 0) {
        *buffer = NULL;
        *size = 0;
    } else {
        void *copy = malloc(modex_module->module_data_size);
        if(copy == NULL) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        memcpy(copy, modex_module->module_data, modex_module->module_data_size);
        *buffer = copy;
        *size = modex_module->module_data_size;
    }
    OPAL_THREAD_UNLOCK(&proc->proc_lock);
    return OMPI_SUCCESS;
}


/**
 * Subscribe to the segment corresponding
 * to this job.
 */

int mca_pml_base_modex_exchange(void)
{
    return mca_pml_base_modex_subscribe(orte_process_info.my_name);
}
