/*
 * $HEADER$
 */

#include <string.h>
#include "lam_config.h"
#include "lfc/lam_hash_table.h"
#include "util/output.h"
#include "proc/proc.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/oob/oob.h"
#include "mca/base/mca_base_module_exchange.h"



/**
 * mca_base_modex_module_t
 *
 * Data for a specic proc and module.
 */

struct mca_base_modex_module_t {
    lam_list_item_t super;
    mca_base_module_t *module;
    void *module_data;
    size_t module_data_size;
};
typedef struct mca_base_modex_module_t mca_base_modex_module_t;

static void mca_base_modex_module_construct(mca_base_modex_module_t *module)
{
    module->module = NULL;
    module->module_data = NULL;
    module->module_data_size = 0;
}

static void mca_base_modex_module_destruct(mca_base_modex_module_t *module)
{
}

OBJ_CLASS_INSTANCE(
    mca_base_modex_module_t, 
    lam_list_item_t, 
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
    lam_object_t super;
    lam_list_t modex_modules;
};
typedef struct mca_base_modex_t mca_base_modex_t;

static void mca_base_modex_construct(mca_base_modex_t* modex)
{
    OBJ_CONSTRUCT(&modex->modex_modules, lam_list_t);
}

static void mca_base_modex_destruct(mca_base_modex_t* modex)
{
    OBJ_DESTRUCT(&modex->modex_modules);
}

OBJ_CLASS_INSTANCE(
    mca_base_modex_t, 
    lam_object_t, 
    mca_base_modex_construct, 
    mca_base_modex_destruct
);


/**
 *  Look to see if there is any data associated with a specified module.
 */

static inline mca_base_modex_module_t* mca_base_modex_lookup_module(
    mca_base_modex_t* modex,
    mca_base_module_t* module)
{
    mca_base_modex_module_t* modex_module;
    for(modex_module =  (mca_base_modex_module_t*)lam_list_get_first(&modex->modex_modules);
        modex_module != (mca_base_modex_module_t*)lam_list_get_end(&modex->modex_modules);
        modex_module =  (mca_base_modex_module_t*)lam_list_get_next(modex_module)) {
        if(mca_base_module_compare(modex_module->module, module) == 0) {
            return modex_module;
        }
    }
    return NULL;
}


/**
 *  Create a placeholder for data associated with the specified module.
 */

static inline mca_base_modex_module_t* mca_base_modex_create_module(
    mca_base_modex_t* modex,
    mca_base_module_t* module)
{
    mca_base_modex_module_t* modex_module;
    if(NULL == (modex_module = mca_base_modex_lookup_module(modex, module))) {
        modex_module = OBJ_NEW(mca_base_modex_module_t);
        if(NULL != modex_module) {
            modex_module->module = module;
            lam_list_append(&modex->modex_modules, (lam_list_item_t*)modex_module);
        }
    }
    return modex_module;
}


/**
 *  Store the data associated with the specified module on the 
 *  local process, which will be exchanged with all other processes
 *  during mca_base_modex_exchange().
 */

int mca_base_modex_send(mca_base_module_t *source_module, const void *buffer, size_t size)
{
    lam_proc_t *self = lam_proc_local();
    mca_base_modex_t* modex;
    mca_base_modex_module_t* modex_module;

    if(NULL == self)
        return LAM_ERROR;

    THREAD_LOCK(&self->proc_lock);
    if(NULL == (modex = self->proc_modex)) {
        self->proc_modex = modex = OBJ_NEW(mca_base_modex_t);
    }

    if(NULL == (modex_module = mca_base_modex_create_module(modex, source_module))) {
        THREAD_UNLOCK(&self->proc_lock);
        return LAM_ERROR;
    }

    modex_module->module_data = malloc(size);
    if(NULL == modex_module->module_data) {
        THREAD_UNLOCK(&self->proc_lock);
        return LAM_ERR_OUT_OF_RESOURCE;
    }
    memcpy(modex_module->module_data, buffer, size);
    modex_module->module_data_size = size;
    THREAD_UNLOCK(&self->proc_lock);
    return LAM_SUCCESS;
}


/**
 *  Retreive the data for the specified module from the source process.
 *  This (data) should have already been cached on the process during
 *  mca_base_modex_exchange().
 */

int mca_base_modex_recv(mca_base_module_t *module, lam_proc_t *source_proc, void **buffer, size_t *size)
{
    mca_base_modex_t* modex;
    mca_base_modex_module_t* modex_module;
    void *copy;
    
    THREAD_LOCK(&source_proc->proc_lock);
    if(NULL == (modex = source_proc->proc_modex) ||
       NULL == (modex_module = mca_base_modex_lookup_module(modex, module))) {
        THREAD_UNLOCK(&source_proc->proc_lock);
        return LAM_ERR_NOT_FOUND;
    }

    if(0 == modex_module->module_data_size) {
        *buffer = NULL;
        *size = 0;
        THREAD_UNLOCK(&source_proc->proc_lock);
        return LAM_SUCCESS;
    }

    copy = malloc(modex_module->module_data_size);
    if(NULL == copy) {
       THREAD_UNLOCK(&source_proc->proc_lock);
       return LAM_ERR_OUT_OF_RESOURCE;
    }
    memcpy(copy, modex_module->module_data, modex_module->module_data_size);
    *buffer = copy;
    *size = modex_module->module_data_size;
    THREAD_UNLOCK(&source_proc->proc_lock);
    return LAM_SUCCESS;
}


/**
 *  Iterate over all modules for which the local proc has data associated
 *  with it, and exchange this with all other currently known processes.
 *  Note that we will have to expand this as procs are added/deleted...
 */

int mca_base_modex_exchange(void)
{
    lam_proc_t *self = lam_proc_local();
    mca_base_modex_t* modex;
    mca_base_modex_module_t* self_module;
    size_t nprocs;
    lam_proc_t **procs = lam_proc_all(&nprocs);

    if(nprocs <= 1) {
        if(procs) free(procs);
        return LAM_SUCCESS;
    }

    if(NULL == self) {
        free(procs);
        return LAM_ERROR;
    }

    if(NULL == (modex = self->proc_modex)) {
        self->proc_modex = modex = OBJ_NEW(mca_base_modex_t);
    }
    
    /* loop through all modules with data cached on local process and send to all peers */
    THREAD_LOCK(&self->proc_lock);
    for(self_module =  (mca_base_modex_module_t*)lam_list_get_first(&modex->modex_modules);
        self_module != (mca_base_modex_module_t*)lam_list_get_end(&modex->modex_modules);
        self_module =  (mca_base_modex_module_t*)lam_list_get_next(self_module)) {
        size_t i;
        for(i=0; i<nprocs; i++) {
            lam_proc_t *proc = procs[i];
            int rc;

            if(proc == self) 
                continue;

            rc = mca_oob.oob_send(
                proc->proc_job, 
                proc->proc_vpid, 
                MCA_OOB_ANY_TAG, 
                self_module->module_data, 
                self_module->module_data_size);
            if(rc != LAM_SUCCESS) {
               free(procs); 
               THREAD_UNLOCK(&self->proc_lock);
               return rc;
            }
        }
    }

    /* loop through the same modules and receive data from all peers */
    for(self_module =  (mca_base_modex_module_t*)lam_list_get_first(&modex->modex_modules);
        self_module != (mca_base_modex_module_t*)lam_list_get_end(&modex->modex_modules);
        self_module =  (mca_base_modex_module_t*)lam_list_get_next(self_module)) {
        size_t i;
        for(i=0; i<nprocs; i++) {
            lam_proc_t *proc = procs[i];
            mca_base_modex_module_t* proc_module;
            int tag = MCA_OOB_ANY_TAG;
            int rc;

            if(proc == self) 
                continue;

            THREAD_LOCK(&proc->proc_lock);
            if(NULL == proc->proc_modex) {
                proc->proc_modex = OBJ_NEW(mca_base_modex_t);
                if(NULL == proc->proc_modex) {
                    free(procs);
                    THREAD_UNLOCK(&self->proc_lock);
                    return LAM_ERR_OUT_OF_RESOURCE;
                }
            }
            proc_module = mca_base_modex_create_module(proc->proc_modex, self_module->module);
            if(NULL == proc_module) {
                free(procs);
                THREAD_UNLOCK(&proc->proc_lock);
                THREAD_UNLOCK(&self->proc_lock);
                return LAM_ERR_OUT_OF_RESOURCE;
            }

            rc = mca_oob.oob_recv(
                proc->proc_job, 
                proc->proc_vpid, 
                &tag, 
                &proc_module->module_data, 
                &proc_module->module_data_size);
            if(rc != LAM_SUCCESS) {
                free(procs);
                THREAD_UNLOCK(&proc->proc_lock);
                THREAD_UNLOCK(&self->proc_lock);
                return rc;
            }
            THREAD_UNLOCK(&proc->proc_lock);
        }
    }
    free(procs);
    THREAD_UNLOCK(&self->proc_lock);
    return LAM_SUCCESS;
}


