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

#include "ompi_config.h"

#include <string.h>
#include "class/ompi_hash_table.h"
#include "threads/condition.h"
#include "util/output.h"
#include "util/proc_info.h"

#include "dps/dps.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"
#include "mca/ns/ns.h"
#include "mca/pml/pml.h"
#include "mca/base/mca_base_module_exchange.h"
#include "runtime/runtime.h"

/**
 *
 */

/**
 * mca_base_modex_module_t
 *
 * Data for a specic proc and module.
 */

struct mca_base_modex_module_t {
    ompi_list_item_t super;
    mca_base_component_t component;
    void *module_data;
    size_t module_data_size;
    bool module_data_avail;
    ompi_condition_t module_data_cond;
};
typedef struct mca_base_modex_module_t mca_base_modex_module_t;

static void mca_base_modex_module_construct(mca_base_modex_module_t *module)
{
    OBJ_CONSTRUCT(&module->module_data_cond, ompi_condition_t);
    memset(&module->component, 0, sizeof(module->component));
    module->module_data = NULL;
    module->module_data_size = 0;
    module->module_data_avail = false;
}

static void mca_base_modex_module_destruct(mca_base_modex_module_t *module)
{
    OBJ_DESTRUCT(&module->module_data_cond);
}

OBJ_CLASS_INSTANCE(
    mca_base_modex_module_t, 
    ompi_list_item_t, 
    mca_base_modex_module_construct, 
    mca_base_modex_module_destruct
);

/**
 * mca_base_modex_t
 *
 * List of modules (mca_base_modex_module_t) for which data has been 
 * received from peers.
 */
struct mca_base_modex_t {
    ompi_object_t super;
    ompi_list_t modex_modules;
};
typedef struct mca_base_modex_t mca_base_modex_t;

static void mca_base_modex_construct(mca_base_modex_t* modex)
{
    OBJ_CONSTRUCT(&modex->modex_modules, ompi_list_t);
}

static void mca_base_modex_destruct(mca_base_modex_t* modex)
{
    OBJ_DESTRUCT(&modex->modex_modules);
}

OBJ_CLASS_INSTANCE(
    mca_base_modex_t, 
    ompi_object_t, 
    mca_base_modex_construct, 
    mca_base_modex_destruct
);

/**
 * mca_base_modex_subscription_t
 *
 * Track segments we have subscribed to.
 */

struct mca_base_modex_subscription_t {
    ompi_list_item_t item;
    orte_jobid_t jobid;
};
typedef struct mca_base_modex_subscription_t mca_base_modex_subscription_t;

OBJ_CLASS_INSTANCE(
    mca_base_modex_subscription_t,
    ompi_list_item_t,
    NULL,
    NULL);

/**
 * Globals to track the list of subscriptions.
 */

static ompi_list_t  mca_base_modex_subscriptions;
static ompi_mutex_t mca_base_modex_lock;


/**
 * Initialize global state.
 */
int mca_base_modex_init(void)
{
    OBJ_CONSTRUCT(&mca_base_modex_subscriptions, ompi_list_t);
    OBJ_CONSTRUCT(&mca_base_modex_lock, ompi_mutex_t);
    return OMPI_SUCCESS;
}

/**
 * Cleanup global state.
 */
int mca_base_modex_finalize(void)
{
    ompi_list_item_t *item;
    while(NULL != (item = ompi_list_remove_first(&mca_base_modex_subscriptions)))
        OBJ_RELEASE(item);
    OBJ_DESTRUCT(&mca_base_modex_subscriptions);
    return OMPI_SUCCESS;
}


/**
 *  Look to see if there is any data associated with a specified module.
 */

static mca_base_modex_module_t* mca_base_modex_lookup_module(
    mca_base_modex_t* modex,
    mca_base_component_t* component)
{
    mca_base_modex_module_t* modex_module;
    for(modex_module =  (mca_base_modex_module_t*)ompi_list_get_first(&modex->modex_modules);
        modex_module != (mca_base_modex_module_t*)ompi_list_get_end(&modex->modex_modules);
        modex_module =  (mca_base_modex_module_t*)ompi_list_get_next(modex_module)) {
        if(mca_base_component_compatible(&modex_module->component, component) == 0) {
            return modex_module;
        }
    }
    return NULL;
}


/**
 *  Create a placeholder for data associated with the specified module.
 */

static mca_base_modex_module_t* mca_base_modex_create_module(
    mca_base_modex_t* modex,
    mca_base_component_t* component)
{
    mca_base_modex_module_t* modex_module;
    if(NULL == (modex_module = mca_base_modex_lookup_module(modex, component))) {
        modex_module = OBJ_NEW(mca_base_modex_module_t);
        if(NULL != modex_module) {
            modex_module->component = *component;
            ompi_list_append(&modex->modex_modules, (ompi_list_item_t*)modex_module);
        }
    }
    return modex_module;
}


/**
 *  Callback for registry notifications.
 */

static void mca_base_modex_registry_callback(
    orte_gpr_notify_data_t* data,
    void* cbdata)
{
    ompi_proc_t **new_procs = NULL;
    size_t new_proc_count = 0;
    int32_t i, j;
    orte_gpr_keyval_t **keyval;
    orte_gpr_value_t **value;
    ompi_proc_t *proc;
    char **token;
    orte_process_name_t *proc_name;
    mca_base_modex_t *modex;
    mca_base_modex_module_t *modex_module;
    mca_base_component_t component;
    bool isnew = false;
    int rc;

#if 0
ompi_output(0, "[%d,%d,%d] mca_base_modex_registry_callback\n", 
    ORTE_NAME_ARGS(orte_process_info.my_name));
orte_gpr_base_dump_notify_data(data,0);
#endif

    /* process the callback */
    value = data->values;
    for (i=0; i < data->cnt; i++) {

        if (0 < value[i]->cnt) {  /* needs to be at least one value */
            new_procs = malloc(sizeof(ompi_proc_t*) * value[i]->cnt);

            /*
             * Token for the value should be the process name - look it up
             */
            token = value[i]->tokens;
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
        
                OMPI_THREAD_LOCK(&proc->proc_lock);
                if(NULL == (modex = (mca_base_modex_t*)proc->proc_modex)) {
                    modex = OBJ_NEW(mca_base_modex_t);
                    if(NULL == modex) {
                        ompi_output(0, "mca_base_modex_registry_callback: unable to allocate mca_base_modex_t\n");
                        OMPI_THREAD_UNLOCK(&proc->proc_lock);
                        return;
                    }
                    proc->proc_modex = &modex->super;
                }
                
                /*
                 * Extract the component name and version from the keyval object's key
                 * Could be multiple keyvals returned since there is one for each
                 * component type/name/version - process them all
                 */
                keyval = value[i]->keyvals;
                for (j=0; j < value[i]->cnt; j++) {
#if 0
                    if(sscanf(keyval[j]->key, "modex-%[^-]-%[^-]-%d-%d", 
                        component.mca_type_name,
                        component.mca_component_name,
                        &component.mca_component_major_version,
                        &component.mca_component_minor_version) != 4) {
                        ompi_output(0, "mca_base_modex_registry_callback: invalid component name %s\n", 
                            keyval[j]->key);
                        OMPI_THREAD_UNLOCK(&proc->proc_lock);
                        continue;
                    }
#else
                    orte_buffer_t buffer;
                    char *ptr;
                    void* bytes = NULL;
                    size_t cnt;
                    size_t num_bytes;
                    if(strcmp(keyval[j]->key,"modex") != 0)
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
                        &num_bytes, &cnt, ORTE_UINT32))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
                    if (num_bytes != 0) {
                        if(NULL == (bytes = malloc(num_bytes))) {
                            ORTE_ERROR_LOG(rc);
                            continue;
                        }
                    } 
                    if (ORTE_SUCCESS != (rc = orte_dps.unpack(&buffer, bytes, &num_bytes, ORTE_BYTE))) {
                        ORTE_ERROR_LOG(rc);
                        continue;
                    }
#endif
        
                    /*
                     * Lookup the corresponding modex structure
                     */
                    if(NULL == (modex_module = mca_base_modex_create_module(modex, &component))) {
                        ompi_output(0, "mca_base_modex_registry_callback: mca_base_modex_create_module failed\n");
                        OBJ_RELEASE(data);
                        OMPI_THREAD_UNLOCK(&proc->proc_lock);
                        return;
                    }

#if 0
                    /* 
                     * Create a copy of the data.
                     */
                    modex_module->module_data = (void*)keyval[j]->value.byteobject.bytes;
                    keyval[j]->value.byteobject.bytes = NULL;  /* dereference this pointer to avoid free'ng space */
                    modex_module->module_data_size = keyval[j]->value.byteobject.size;
#else
                    modex_module->module_data = bytes;
                    modex_module->module_data_size = num_bytes;
#endif
                    modex_module->module_data_avail = true;
#if 0
ompi_output(0, "[%d,%d,%d] mca_base_modex_registry_callback: %s-%s-%d-%d received %d bytes\n",
    ORTE_NAME_ARGS(orte_process_info.my_name),
    component.mca_type_name,
    component.mca_component_name,
    component.mca_component_major_version,
    component.mca_component_minor_version,
    num_bytes);
#endif
                    ompi_condition_signal(&modex_module->module_data_cond);
                }
                OMPI_THREAD_UNLOCK(&proc->proc_lock);
            }  /* convert string to process name */
        }  /* if value[i]->cnt > 0 */
        
        if(NULL != new_procs) {
            mca_pml.pml_add_procs(new_procs, new_proc_count);
            free(new_procs);
        }
    }
}

/**
 * Make sure we have subscribed to this segment.
 */

static int mca_base_modex_subscribe(orte_process_name_t* name)
{
    orte_gpr_notify_id_t rctag;
    orte_gpr_value_t trig, *trigs;
    orte_gpr_subscription_t sub, *subs;
    orte_jobid_t jobid;
    ompi_list_item_t* item;
    mca_base_modex_subscription_t* subscription;
    int rc;

    /* check for an existing subscription */
    OMPI_LOCK(&mca_base_modex_lock);
    for(item =  ompi_list_get_first(&mca_base_modex_subscriptions);
        item != ompi_list_get_end(&mca_base_modex_subscriptions);
        item = ompi_list_get_next(item)) {
        subscription = (mca_base_modex_subscription_t*)item;
        if(subscription->jobid == name->jobid) {
            OMPI_UNLOCK(&mca_base_modex_lock);
            return OMPI_SUCCESS;
        }
    }
    OMPI_UNLOCK(&mca_base_modex_lock);

    /* otherwise - subscribe to get this jobid's ptl contact info */
    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the subscription definition */
    OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(sub.segment), jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&sub);
        return rc;
    }
    sub.addr_mode = ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR;
    sub.tokens = NULL;
    sub.num_tokens = 0;
    sub.num_keys = 1;
    sub.keys = (char**)malloc(sizeof(char*));
    if (NULL == sub.keys) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.keys[0] = strdup("modex");
    if (NULL == sub.keys[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.cbfunc = mca_base_modex_registry_callback;
    sub.user_tag = NULL;
    
    /* setup the trigger definition */
    OBJ_CONSTRUCT(&trig, orte_gpr_value_t);
    trig.addr_mode = ORTE_GPR_TOKENS_XAND;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(trig.segment), jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return rc;
    }
    trig.tokens = (char**)malloc(sizeof(char*));
    if (NULL == trig.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.tokens[0] = strdup(ORTE_JOB_GLOBALS);
    trig.num_tokens = 1;

    trig.cnt = 2;
    trig.keyvals = (orte_gpr_keyval_t**)malloc(2*sizeof(orte_gpr_keyval_t*));
    if (NULL == trig.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0]->key = strdup(ORTE_JOB_SLOTS_KEY);
    trig.keyvals[0]->type = ORTE_NULL;
    
    trig.keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[1]->key = strdup(ORTE_PROC_NUM_AT_STG1);
    trig.keyvals[1]->type = ORTE_NULL;

    /* register the subscription */
    subs = &sub;
    trigs = &trig;
    rc = orte_gpr.subscribe(
        	ORTE_GPR_NOTIFY_ADD_ENTRY | ORTE_GPR_NOTIFY_VALUE_CHG |
            ORTE_GPR_TRIG_CMP_LEVELS | ORTE_GPR_TRIG_ONE_SHOT,
        	1, &subs,
         1, &trigs,
         &rctag);
    if(ORTE_SUCCESS != rc) {
        ompi_output(0, "mca_base_modex_exchange: "
		    "ompi_gpr.subscribe failed with return code %d\n", rc);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
	    return OMPI_ERROR;
    }

    /* add this jobid to our list of subscriptions */
    OMPI_LOCK(&mca_base_modex_lock);
    subscription = OBJ_NEW(mca_base_modex_subscription_t);
    subscription->jobid = name->jobid;
    ompi_list_append(&mca_base_modex_subscriptions, &subscription->item);
    OMPI_UNLOCK(&mca_base_modex_lock);
    OBJ_DESTRUCT(&sub);
    OBJ_DESTRUCT(&trig);
    return OMPI_SUCCESS;
}


/**
 *  Store the data associated with the specified module in the
 *  gpr. Note that the gpr is in a mode where it caches
 *  individual puts during startup and sends them as an aggregate
 *  command.
 */

int mca_base_modex_send(
    mca_base_component_t *source_component, 
    const void *data, 
    size_t size)
{
    orte_jobid_t jobid;
    orte_gpr_value_t *value;
    int rc;
    orte_buffer_t buffer;
    char* ptr;

    value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value->segment), jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }


    if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(value->tokens),
                                &(value->num_tokens), orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    value->addr_mode = ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR;
    value->cnt = 1;
    value->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    value->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (value->keyvals[0])->type = ORTE_BYTE_OBJECT;
#if 0
    (value->keyvals[0])->value.byteobject.size = size;
    (value->keyvals[0])->value.byteobject.bytes = (void *)malloc(size);
    if(NULL == (value->keyvals[0])->value.byteobject.bytes) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memcpy((value->keyvals[0])->value.byteobject.bytes, data, size);

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
    if (ORTE_SUCCESS != (rc = orte_dps.pack(&buffer, &size, 1, ORTE_UINT32))) {
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dps.pack(&buffer, (void*)data, size, ORTE_BYTE))) {
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dps.unload(&buffer, 
        (void**)&(value->keyvals[0])->value.byteobject.bytes,
        (size_t*)&(value->keyvals[0])->value.byteobject.size))) {
        goto cleanup;
    }
    OBJ_DESTRUCT(&buffer);
    value->keyvals[0]->key = strdup("modex");
#endif

    rc = orte_gpr.put(1, &value);

cleanup:
    OBJ_RELEASE(value);
    return rc;
}


/**
 *  Retreive the data for the specified module from the source process.
 */

int mca_base_modex_recv(
    mca_base_component_t *component,
    ompi_proc_t *proc, 
    void **buffer, 
    size_t *size)
{
    mca_base_modex_t* modex;
    mca_base_modex_module_t* modex_module;

    /* check the proc for cached data */
    OMPI_THREAD_LOCK(&proc->proc_lock);
    if(NULL == (modex = (mca_base_modex_t*)proc->proc_modex)) {
        modex = OBJ_NEW(mca_base_modex_t);
        if(modex == NULL) {
            OMPI_THREAD_UNLOCK(&proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        proc->proc_modex = &modex->super;

        /* verify that we have subscribed to this segment */
        OMPI_THREAD_UNLOCK(&proc->proc_lock);
        mca_base_modex_subscribe(&proc->proc_name);
        OMPI_THREAD_LOCK(&proc->proc_lock);
    }

    /* lookup/create the module */
    if(NULL == (modex_module = mca_base_modex_create_module(modex, component))) {
        OMPI_THREAD_UNLOCK(&proc->proc_lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* wait until data is available */
    while(modex_module->module_data_avail == false) {
#if 0
ompi_output(0, "[%d,%d,%d] mca_base_modex_registry_callback: waiting for %s-%s-%d-%d\n",
    ORTE_NAME_ARGS(orte_process_info.my_name),
    component->mca_type_name,
    component->mca_component_name,
    component->mca_component_major_version,
    component->mca_component_minor_version);
#endif
        ompi_condition_wait(&modex_module->module_data_cond, &proc->proc_lock);
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
    OMPI_THREAD_UNLOCK(&proc->proc_lock);
    return OMPI_SUCCESS;
}


/**
 * Subscribe to the segment corresponding
 * to this job.
 */

int mca_base_modex_exchange(void)
{
    return mca_base_modex_subscribe(orte_process_info.my_name);
}


