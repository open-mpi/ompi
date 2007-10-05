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

static opal_mutex_t ompi_modex_string_lock;

/*
 * Global buffer we use to collect modex info for later
 * transmission
 */
static orte_buffer_t ompi_modex_buffer;
static orte_std_cntr_t ompi_modex_num_entries;


int
ompi_modex_init(void)
{
    OBJ_CONSTRUCT(&ompi_modex_data, opal_hash_table_t);
    OBJ_CONSTRUCT(&ompi_modex_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ompi_modex_string_lock, opal_mutex_t);
    
    OBJ_CONSTRUCT(&ompi_modex_buffer, orte_buffer_t);
    ompi_modex_num_entries = 0;
    
    opal_hash_table_init(&ompi_modex_data, 256);

    return OMPI_SUCCESS;
}


int
ompi_modex_finalize(void)
{
    opal_hash_table_remove_all(&ompi_modex_data);
    OBJ_DESTRUCT(&ompi_modex_data);

    OBJ_DESTRUCT(&ompi_modex_string_lock);
    OBJ_DESTRUCT(&ompi_modex_lock);
    OBJ_DESTRUCT(&ompi_modex_buffer);

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
           for it */
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
            OPAL_THREAD_UNLOCK(&ompi_modex_lock);
        } else {
            OPAL_THREAD_UNLOCK(&ompi_modex_lock);
        }
    }

    return proc_data;
}


/**
 * Get the local buffer's data
 */
int
ompi_modex_get_my_buffer(orte_buffer_t *buf)
{
    int rc;
    
    OPAL_THREAD_LOCK(&ompi_modex_lock);
    /* put our process name in the buffer so it can be unpacked later */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, ORTE_PROC_MY_NAME, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&ompi_modex_lock);
        return rc;
    }

    /* put the number of entries into the buffer */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buf, &ompi_modex_num_entries, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        OPAL_THREAD_UNLOCK(&ompi_modex_lock);
        return rc;
    }
    
    /* if there are entries, copy the data across */
    if (0 < ompi_modex_num_entries) {
        if (ORTE_SUCCESS != (orte_dss.copy_payload(buf, &ompi_modex_buffer))) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&ompi_modex_lock);
            return rc;
        }
    }
    
    OPAL_THREAD_UNLOCK(&ompi_modex_lock);
    return ORTE_SUCCESS;
}

/**
 *  Process modex data
 */
int
ompi_modex_process_data(orte_buffer_t *buf)
{
    orte_std_cntr_t i, j, num_procs, num_entries;
    opal_list_item_t *item;
    void *bytes = NULL;
    orte_std_cntr_t cnt;
    orte_process_name_t proc_name;
    ompi_modex_proc_data_t *proc_data;
    ompi_modex_module_data_t *module_data;
    mca_base_component_t component;
    int rc;

    /* extract the number of entries in the buffer */
    cnt=1;
    if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf, &num_procs, &cnt, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* process the buffer */
    for (i=0; i < num_procs; i++) {
        /* unpack the process name */
        cnt=1;
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf, &proc_name, &cnt, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* look up the modex data structure */
        proc_data = ompi_modex_lookup_orte_proc(&proc_name);
        if (proc_data == NULL) {
            /* report the error */
            opal_output(0, "ompi_modex_process_data: received modex info for unknown proc %s\n",
                        ORTE_NAME_PRINT(&proc_name));
            return OMPI_ERR_NOT_FOUND;
        }

        /* unpack the number of entries for this proc */
        cnt=1;
        if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf, &num_entries, &cnt, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        OPAL_THREAD_LOCK(&proc_data->modex_lock);

        /*
         * Extract the component name and version - since there is one for each
         * component type/name/version - process them all
         */
        for (j = 0; j < num_entries; j++) {
            size_t num_bytes;
            char *ptr;
                        
            cnt = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf, &ptr, &cnt, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            strcpy(component.mca_type_name, ptr);
            free(ptr);
            
            cnt = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf, &ptr, &cnt, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            strcpy(component.mca_component_name, ptr);
            free(ptr);
            
            cnt = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf,
                                                      &component.mca_component_major_version, &cnt, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            cnt = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf,
                                                      &component.mca_component_minor_version, &cnt, ORTE_INT32))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            cnt = 1;
            if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf, &num_bytes, &cnt, ORTE_SIZE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (num_bytes != 0) {
                if (NULL == (bytes = malloc(num_bytes))) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                cnt = (orte_std_cntr_t) num_bytes;
                if (ORTE_SUCCESS != (rc = orte_dss.unpack(buf, bytes, &cnt, ORTE_BYTE))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
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
                opal_output(0, "ompi_modex_process_data: ompi_modex_lookup_module failed\n");
                OPAL_THREAD_UNLOCK(&proc_data->modex_lock);
                return OMPI_ERR_NOT_FOUND;
            }
            module_data->module_data = bytes;
            module_data->module_data_size = num_bytes;
            proc_data->modex_received_data = true;
            opal_condition_signal(&proc_data->modex_cond);
            
            if (opal_list_get_size(&module_data->module_cbs)) {
                ompi_proc_t *proc = ompi_proc_find(&proc_name);
                
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
    }
    
    return OMPI_SUCCESS;
}


int
ompi_modex_send(mca_base_component_t * source_component,
                const void *data,
                size_t size)
{
    int rc;
    char *ptr;

    OPAL_THREAD_LOCK(&ompi_modex_lock);
    
    /* Pack the component name information into the local buffer */
    ptr = source_component->mca_type_name;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&ompi_modex_buffer, &ptr, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    ptr = source_component->mca_component_name;
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&ompi_modex_buffer, &ptr, 1, ORTE_STRING))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&ompi_modex_buffer, &source_component->mca_component_major_version, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&ompi_modex_buffer, &source_component->mca_component_minor_version, 1, ORTE_INT32))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&ompi_modex_buffer, &size, 1, ORTE_SIZE))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* Pack the actual data into the buffer */
    if (0 != size) {
        if (ORTE_SUCCESS != (rc = orte_dss.pack(&ompi_modex_buffer, (void *) data, size, ORTE_BYTE))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    
    /* track the number of entries */
    ++ompi_modex_num_entries;

 cleanup:
    OPAL_THREAD_UNLOCK(&ompi_modex_lock);

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
        opal_output(0, "modex recv: no module avail or zero byte size");
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


static mca_base_component_t modex_component = {
    MCA_BASE_VERSION_1_0_0,
    "modex",
    MCA_BASE_VERSION_1_0_0,
    "",
    MCA_BASE_VERSION_1_0_0,
    NULL,
    NULL
};


int
ompi_modex_send_string(const char* key,
                       const void *buffer, size_t size)
{
    int ret;

    OPAL_THREAD_LOCK(&ompi_modex_string_lock);
    strncpy(modex_component.mca_component_name, key,
            MCA_BASE_MAX_COMPONENT_NAME_LEN);
    ret = ompi_modex_send(&modex_component, buffer, size);
    OPAL_THREAD_UNLOCK(&ompi_modex_string_lock);

    return ret;
}


int
ompi_modex_recv_string(const char* key,
                       struct ompi_proc_t *source_proc,
                       void **buffer, size_t *size)
{
    int ret;

    OPAL_THREAD_LOCK(&ompi_modex_string_lock);
    strncpy(modex_component.mca_component_name, key,
            MCA_BASE_MAX_COMPONENT_NAME_LEN);
    ret = ompi_modex_recv(&modex_component, source_proc, buffer, size);
    OPAL_THREAD_UNLOCK(&ompi_modex_string_lock);

    return ret;
}
