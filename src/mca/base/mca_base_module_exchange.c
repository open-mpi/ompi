/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include "class/ompi_hash_table.h"
#include "threads/condition.h"
#include "util/proc_info.h"
#include "util/output.h"
#include "proc/proc.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/oob/oob.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
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
    mca_ns_base_jobid_t jobid;
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
    ompi_registry_notify_message_t* msg,
    void* cbdata)
{
    ompi_list_item_t* item;

    /* process the callback */
    while((item = ompi_list_remove_first(&msg->data)) != NULL) {
                                                                                                          
        ompi_registry_value_t* value = (ompi_registry_value_t*)item;
        ompi_buffer_t buffer;
        ompi_proc_t* proc;
        char* component_name_version;
        ompi_process_name_t proc_name;
        mca_base_modex_t* modex;
        mca_base_modex_module_t* modex_module;
        mca_base_component_t component;
        void* bptr;
        int32_t bsize;
                                                                                                       
        /* transfer ownership of registry object to buffer and unpack */
        ompi_buffer_init_preallocated(&buffer, value->object, value->object_size);
        value->object = NULL;
        value->object_size = 0;
        OBJ_RELEASE(value);

        /*
         * Lookup the process.
         */
        ompi_unpack(buffer, &proc_name, 1, OMPI_NAME);
        proc = ompi_proc_find_and_add(&proc_name);
        if(NULL == proc)
            continue;

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
         * Unpack the component name and version.
         */

        ompi_unpack_string(buffer, &component_name_version);
        if(sscanf(component_name_version, "%[^-]-%[^-]-%d-%d", 
            component.mca_type_name,
            component.mca_component_name,
            &component.mca_component_major_version,
            &component.mca_component_minor_version) != 4) {
            ompi_output(0, "mca_base_modex_registry_callback: invalid component name %s\n", 
                component_name_version);
            free(component_name_version);
            OMPI_THREAD_UNLOCK(&proc->proc_lock);
            continue;
        }
        free(component_name_version);

        /*
         * Lookup the corresponding modex structure
         */
        if(NULL == (modex_module = mca_base_modex_create_module(modex, &component))) {
            ompi_output(0, "mca_base_modex_registry_callback: mca_base_modex_create_module failed\n");
            OMPI_THREAD_UNLOCK(&proc->proc_lock);
            return;
        }

        /* 
         * Create a copy of the data.
         */

        ompi_unpack(buffer, &bsize, 1, OMPI_INT32);
        if(NULL == (bptr = malloc(bsize))) {
            ompi_output(0, "mca_base_modex_registry_callback: mca_base_modex_create_module failed\n");
            OMPI_THREAD_UNLOCK(&proc->proc_lock);
            return;
        }
        ompi_unpack(buffer, bptr, bsize, OMPI_BYTE);
        modex_module->module_data = bptr;
        modex_module->module_data_size = bsize;
        modex_module->module_data_avail = true;
        ompi_condition_signal(&modex_module->module_data_cond);

        /* release buffer */
        ompi_buffer_free(buffer);
        OMPI_THREAD_UNLOCK(&proc->proc_lock);
    }
}

/**
 * Make sure we have subscribed to this segment.
 */

static int mca_base_modex_subscribe(ompi_process_name_t* name)
{
    int rc;
    char segment[32];
    ompi_list_item_t* item;
    mca_base_modex_subscription_t* subscription;

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

    /* otherwise - subscribe */
    sprintf(segment, "modex-%X", name->jobid);
    rc = ompi_registry.subscribe(
        OMPI_REGISTRY_OR,
        OMPI_REGISTRY_NOTIFY_ADD_ENTRY|OMPI_REGISTRY_NOTIFY_DELETE_ENTRY|
        OMPI_REGISTRY_NOTIFY_MODIFICATION|OMPI_REGISTRY_NOTIFY_PRE_EXISTING,
        segment,
        NULL,
        mca_base_modex_registry_callback,
        NULL);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_base_modex_exchange: "
            "ompi_registry.subscribe failed with return code %d\n", rc);
    }

    /* add this jobid to our list of subscriptions */
    OMPI_LOCK(&mca_base_modex_lock);
    subscription = OBJ_NEW(mca_base_modex_subscription_t);
    subscription->jobid = name->jobid;
    ompi_list_append(&mca_base_modex_subscriptions, &subscription->item);
    OMPI_UNLOCK(&mca_base_modex_lock);
    return rc;
}


/**
 *  Store the data associated with the specified module in the
 *  registry. Note that the registry is in a mode where it caches
 *  individual puts during startup and sends them as an aggregate
 *  command.
 */

int mca_base_modex_send(
    mca_base_component_t *source_component, 
    const void *data, 
    size_t size)
{
    char segment[32];
    char component_name_version[256];
    char *keys[3];
    ompi_buffer_t buffer;
    void* bptr;
    int bsize;
    int rc;

    sprintf(component_name_version, "%s-%s-%d-%d", 
        source_component->mca_type_name,
        source_component->mca_component_name,
        source_component->mca_component_major_version,
        source_component->mca_component_minor_version);

    keys[0] = ompi_name_server.get_proc_name_string(ompi_rte_get_self());
    keys[1] = component_name_version;
    keys[2] = NULL;

    ompi_buffer_init(&buffer, size+256);
    ompi_pack(buffer, ompi_rte_get_self(), 1, OMPI_NAME);
    ompi_pack_string(buffer, component_name_version);
    ompi_pack(buffer, &size, 1, OMPI_INT32);
    ompi_pack(buffer, (void*)data, size, OMPI_BYTE);
    ompi_buffer_get(buffer, &bptr, &bsize);

    sprintf(segment, "modex-%X", mca_oob_name_self.jobid);
    rc = ompi_registry.put(
        OMPI_REGISTRY_OVERWRITE, 
        segment,
        keys,
        (ompi_registry_object_t)bptr,
        (ompi_registry_object_size_t)bsize);
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
        mca_base_modex_subscribe(&proc->proc_name);
    }

    /* lookup/create the module */
    if(NULL == (modex_module = mca_base_modex_create_module(modex, component))) {
        OMPI_THREAD_UNLOCK(&proc->proc_lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* wait until data is available */
    while(modex_module->module_data_avail == false) {
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
    return mca_base_modex_subscribe(ompi_rte_get_self());
}


