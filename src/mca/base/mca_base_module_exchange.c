/*
 * $HEADER$
 */

#include <string.h>
#include "ompi_config.h"
#include "class/ompi_hash_table.h"
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



/**
 * mca_base_modex_module_t
 *
 * Data for a specic proc and module.
 */

struct mca_base_modex_module_t {
    ompi_list_item_t super;
    mca_base_component_t *component;
    void *module_data;
    size_t module_data_size;
};
typedef struct mca_base_modex_module_t mca_base_modex_module_t;

static void mca_base_modex_module_construct(mca_base_modex_module_t *module)
{
    module->component = NULL;
    module->module_data = NULL;
    module->module_data_size = 0;
}

static void mca_base_modex_module_destruct(mca_base_modex_module_t *module)
{
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
        if(mca_base_component_compare(modex_module->component, 
                                      component) == 0) {
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
    if(NULL == (modex_module = mca_base_modex_lookup_module(modex, 
                                                            component))) {
        modex_module = OBJ_NEW(mca_base_modex_module_t);
        if(NULL != modex_module) {
            modex_module->component = component;
            ompi_list_append(&modex->modex_modules, 
                             (ompi_list_item_t*)modex_module);
        }
    }
    return modex_module;
}


/**
 *  Store the data associated with the specified module on the 
 *  local process, which will be exchanged with all other processes
 *  during mca_base_modex_exchange().
 */

int mca_base_modex_send(
    mca_base_component_t *source_component, 
    const void *buffer, 
    size_t size)
{
    char segment[32];
    char comp_name_version[255];
    char *keys[3];

    sprintf(comp_name_version, "%s-%s-%d-%d", 
        source_component->mca_type_name,
        source_component->mca_component_name,
        source_component->mca_type_major_version,
        source_component->mca_type_minor_version);

    keys[0] = ompi_name_server.get_proc_name_string(&mca_oob_name_self);
    keys[1] = comp_name_version;
    keys[2] = NULL;

    sprintf(segment, "mca-%u", mca_oob_name_self.jobid);
    return ompi_registry.put(
        OMPI_REGISTRY_OVERWRITE, 
        segment,
        keys,
        (ompi_registry_object_t)buffer,
        (ompi_registry_object_size_t)size);
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
    char segment[32];
    char comp_name_version[255];
    char *keys[3];
    ompi_list_t *results;
    ompi_registry_value_t* value;

    sprintf(comp_name_version, "%s-%s-%d-%d", 
        component->mca_type_name,
        component->mca_component_name,
        component->mca_type_major_version,
        component->mca_type_minor_version);

    keys[0] = ompi_name_server.get_proc_name_string(&proc->proc_name);
    keys[1] = comp_name_version;
    keys[2] = NULL;

    sprintf(segment, "mca-%u", proc->proc_name.jobid);
    results = ompi_registry.get(
        OMPI_REGISTRY_AND, 
        segment,
        keys);
    if(results == NULL || ompi_list_get_size(results) == 0)
        return OMPI_ERR_NOT_FOUND;
    value = ompi_list_remove_first(results);
    *buffer = value->object;
    *size = value->object_size;
    value->object = NULL;
    OBJ_RELEASE(value);
    OBJ_RELEASE(results);
    return OMPI_SUCCESS;
}


/**
 * Barrier until all processes have registered.
 */

int mca_base_modex_exchange(void)
{
   return mca_oob_barrier();
}


