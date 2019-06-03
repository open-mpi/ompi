/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/argv.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/memory/base/base.h"
#include <ucm/api/ucm.h>

#include <string.h>

#include "btl_uct_device_context.h"
#include "btl_uct_am.h"

static int mca_btl_uct_component_register(void)
{
    mca_btl_uct_module_t *module = &mca_btl_uct_module_template;

    mca_btl_uct_component.memory_domains = "none";
    (void) mca_base_component_var_register(&mca_btl_uct_component.super.btl_version,
                                           "memory_domains", "Comma-delimited list of memory domains of the form "
                                           "to use for communication. Memory domains MUST provide transports that "
                                           "support put, get, and amos. Special values: all (all available), none."
                                           " (default: none)", MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_btl_uct_component.memory_domains);

    mca_btl_uct_component.allowed_transports = "dc_mlx5,rc_mlx5,ud,ugni_rdma,ugni_smsg,any";
    (void) mca_base_component_var_register(&mca_btl_uct_component.super.btl_version,
                                           "transports", "Comma-delimited list of transports to use sorted by increasing "
                                           "priority. The list of transports available can be queried using ucx_info. Special"
                                           "values: any (any available) (default: dc_mlx5,rc_mlx5,ud,any)",
                                           MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.allowed_transports);

    mca_btl_uct_component.num_contexts_per_module = 0;
    (void) mca_base_component_var_register(&mca_btl_uct_component.super.btl_version,
                                           "num_contexts_per_module", "Number of UCT worker contexts "
                                           "to create for each BTL module. Larger numbers will improve "
                                           "multi-threaded performance but may increase memory usage. "
                                           "A good rule of thumb is one context per application thread "
                                           "that will be calling into MPI. (default: 0 -- autoselect "
                                           "based on the number of cores)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0 ,MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL, &mca_btl_uct_component.num_contexts_per_module);

    mca_btl_uct_component.disable_ucx_memory_hooks = true;
    (void) mca_base_component_var_register(&mca_btl_uct_component.super.btl_version,
                                           "disable_ucx_memory_hooks", "Disable the munmap memory hook "
                                           "inside UCX. These hooks are not necessary when using the "
                                           "uct btl and tend to cause performance problems when using "
                                           "multiple threads (default: true)", MCA_BASE_VAR_TYPE_BOOL,
                                           NULL, 0 ,MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL, &mca_btl_uct_component.disable_ucx_memory_hooks);


#if OPAL_C_HAVE__THREAD_LOCAL
    mca_btl_uct_component.bind_threads_to_contexts = true;
    (void) mca_base_component_var_register(&mca_btl_uct_component.super.btl_version,
                                           "bind_threads_to_contexts", "Bind threads to device contexts. "
                                           "In general this should improve the multi-threaded performance "
                                           "when threads are used. (default: true)", MCA_BASE_VAR_TYPE_BOOL,
                                           NULL, 0 ,MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL, &mca_btl_uct_component.bind_threads_to_contexts);
#endif

    /* for now we want this component to lose to btl/ugni and btl/vader */
    module->super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH;

    return mca_btl_base_param_register (&mca_btl_uct_component.super.btl_version,
                                        &module->super);
}

static void mca_btl_uct_mem_release_cb(void *buf, size_t length, void *cbdata, bool from_alloc)
{
    ucm_vm_munmap(buf, length);
}

static int mca_btl_uct_component_open(void)
{
    if (0 == mca_btl_uct_component.num_contexts_per_module) {
        /* use the core count and the number of local processes to determine
         * how many UCT workers to create */
        int core_count = 36;

        (void) opal_hwloc_base_get_topology ();
        core_count = hwloc_get_nbobjs_by_type (opal_hwloc_topology, HWLOC_OBJ_CORE);

        if (core_count <= opal_process_info.num_local_peers || !opal_using_threads()) {
            /* there is probably no benefit to using multiple device contexts when not
             * using threads or oversubscribing the node with mpi processes. */
            mca_btl_uct_component.num_contexts_per_module = 1;
        } else {
            mca_btl_uct_component.num_contexts_per_module = core_count / (opal_process_info.num_local_peers + 1);
        }
    }

    if (mca_btl_uct_component.num_contexts_per_module > MCA_BTL_UCT_MAX_WORKERS) {
        mca_btl_uct_component.num_contexts_per_module = MCA_BTL_UCT_MAX_WORKERS;
    }

    if (mca_btl_uct_component.disable_ucx_memory_hooks &&
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) ==
        ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT) &
         opal_mem_hooks_support_level()))) {
        ucm_set_external_event(UCM_EVENT_VM_UNMAPPED);
        opal_mem_hooks_register_release(mca_btl_uct_mem_release_cb, NULL);
    }

    return OPAL_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */
static int mca_btl_uct_component_close(void)
{
    if (mca_btl_uct_component.disable_ucx_memory_hooks) {
        opal_mem_hooks_unregister_release (mca_btl_uct_mem_release_cb);
    }

    return OPAL_SUCCESS;
}

static size_t mca_btl_uct_tl_modex_size (mca_btl_uct_tl_t *tl)
{
    const size_t size = strlen (tl->uct_tl_name) + 1;

    if (MCA_BTL_UCT_TL_ATTR(tl, 0).cap.flags & UCT_IFACE_FLAG_CONNECT_TO_IFACE) {
        /* pad out to a multiple of 4 bytes */
        return (4 + 3 + size + MCA_BTL_UCT_TL_ATTR(tl, 0).device_addr_len + MCA_BTL_UCT_TL_ATTR(tl, 0).iface_addr_len) & ~3;
    }

    return (4 + 3 + size + MCA_BTL_UCT_TL_ATTR(tl, 0).device_addr_len) & ~3;
}

static size_t mca_btl_uct_module_modex_size (mca_btl_uct_module_t *module)
{
    size_t modex_size = 4 + strlen (module->md_name) + 1;

    if (module->rdma_tl) {
        modex_size += mca_btl_uct_tl_modex_size (module->rdma_tl);
    }

    if (module->am_tl && module->am_tl != module->rdma_tl) {
        modex_size += mca_btl_uct_tl_modex_size (module->am_tl);
    }

    if (module->conn_tl && module->conn_tl != module->rdma_tl && module->conn_tl != module->am_tl) {
        modex_size += mca_btl_uct_tl_modex_size (module->conn_tl);
    }

    return modex_size;
}

static size_t mca_btl_uct_tl_modex_pack (mca_btl_uct_tl_t *tl, uint8_t *modex_data)
{
    mca_btl_uct_device_context_t *dev_context = tl->uct_dev_contexts[0];
    size_t modex_size = mca_btl_uct_tl_modex_size (tl);

    *((uint32_t *) modex_data) = (uint32_t) modex_size;
    modex_data += 4;

    strcpy ((char *) modex_data, tl->uct_tl_name);
    modex_data += strlen (tl->uct_tl_name) + 1;

    /* NTH: only the first context is available. i assume the device addresses of the
     * contexts will be the same but they will have different iface addresses. i also
     * am assuming that it doesn't really matter if all remote contexts connect to
     * the same endpoint since we are only doing RDMA. if any of these assumptions are
     * wrong then we can't delay creating the other contexts and must include their
     * information in the modex. */
    if (MCA_BTL_UCT_TL_ATTR(tl, 0).cap.flags & UCT_IFACE_FLAG_CONNECT_TO_IFACE) {
        uct_iface_get_address (dev_context->uct_iface, (uct_iface_addr_t *) modex_data);
        modex_data += MCA_BTL_UCT_TL_ATTR(tl, 0).iface_addr_len;
    }

    uct_iface_get_device_address (dev_context->uct_iface, (uct_device_addr_t *) modex_data);
    modex_data += MCA_BTL_UCT_TL_ATTR(tl, 0).device_addr_len;

    return modex_size;
}

static int mca_btl_uct_modex_send (void)
{
    size_t modex_size = sizeof (mca_btl_uct_modex_t);
    mca_btl_uct_modex_t *modex;
    uint8_t *modex_data;
    int rc;

    for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
        modex_size += mca_btl_uct_module_modex_size (mca_btl_uct_component.modules[i]);
    }

    modex = alloca (modex_size);
    modex_data = modex->data;

    modex->module_count = mca_btl_uct_component.module_count;

    for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
        mca_btl_uct_module_t *module = mca_btl_uct_component.modules[i];
        size_t name_len = strlen (module->md_name);

        /* pack the size */
        *((uint32_t *) modex_data) = (uint32_t) mca_btl_uct_module_modex_size (module);

        modex_data += 4;

        strcpy ((char *) modex_data, module->md_name);
        modex_data += name_len + 1;

        if (module->rdma_tl) {
            modex_data += mca_btl_uct_tl_modex_pack (module->rdma_tl, modex_data);
        }

        if (module->am_tl && module->am_tl != module->rdma_tl) {
            modex_data += mca_btl_uct_tl_modex_pack (module->am_tl, modex_data);
        }

        if (module->conn_tl && module->conn_tl != module->rdma_tl && module->conn_tl != module->am_tl) {
            modex_data += mca_btl_uct_tl_modex_pack (module->conn_tl, modex_data);
        }
    }

    OPAL_MODEX_SEND(rc, OPAL_PMIX_GLOBAL, &mca_btl_uct_component.super.btl_version, modex, modex_size);
    return rc;
}

static mca_btl_uct_module_t *mca_btl_uct_alloc_module (const char *md_name, mca_btl_uct_md_t *md,
                                                       size_t registration_size)
{
    mca_btl_uct_module_t *module;
    ucs_status_t ucs_status;

    module = malloc (sizeof (*module));
    if (NULL == module) {
        return NULL;
    }

    /* copy the module template */
    *module = mca_btl_uct_module_template;

    OBJ_CONSTRUCT(&module->id_to_endpoint, opal_hash_table_t);
    OBJ_CONSTRUCT(&module->endpoint_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->short_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&module->eager_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&module->max_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&module->pending_frags, opal_list_t);
    OBJ_CONSTRUCT(&module->lock, opal_recursive_mutex_t);
    OBJ_CONSTRUCT(&module->pending_connection_reqs, opal_fifo_t);

    module->md = md;
    module->md_name = strdup (md_name);
    module->super.btl_registration_handle_size = registration_size;

    ucs_status = ucs_async_context_create (UCS_ASYNC_MODE_THREAD, &module->ucs_async);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("Could not create a UCT async context"));
        mca_btl_uct_finalize (&module->super);
        return NULL;
    }

    return module;
}

ucs_status_t mca_btl_uct_am_handler (void *arg, void *data, size_t length, unsigned flags)
{
    mca_btl_uct_device_context_t *tl_context = (mca_btl_uct_device_context_t *) arg;
    mca_btl_uct_module_t *uct_btl = tl_context->uct_btl;
    mca_btl_uct_am_header_t *header = (mca_btl_uct_am_header_t *) data;
    mca_btl_active_message_callback_t *reg;
    mca_btl_base_segment_t seg = {.seg_addr = {.pval = (void *) ((intptr_t) data + sizeof (*header))},
                                  .seg_len = length - sizeof (*header)};
    mca_btl_uct_base_frag_t frag = {.base = {.des_segments = &seg, .des_segment_count = 1}};

    /* prevent recursion */
    tl_context->in_am_callback = true;

    reg = mca_btl_base_active_message_trigger + header->data.tag;
    reg->cbfunc (&uct_btl->super, header->data.tag, &frag.base, reg->cbdata);

    tl_context->in_am_callback = false;

    return UCS_OK;
}

static int mca_btl_uct_component_process_uct_md (uct_md_resource_desc_t *md_desc, char **allowed_ifaces)
{
    mca_rcache_base_resources_t rcache_resources;
    uct_tl_resource_desc_t *tl_desc;
    mca_btl_uct_module_t *module;
    uct_md_config_t *uct_config;
    uct_md_attr_t md_attr;
    mca_btl_uct_md_t *md;
    bool found = false;
    unsigned num_tls;
    char *tmp;

    if (MCA_BTL_UCT_MAX_MODULES == mca_btl_uct_component.module_count) {
        BTL_VERBOSE(("created the maximum number of allowable modules"));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    BTL_VERBOSE(("processing memory domain %s", md_desc->md_name));

    for (int j = 0 ; allowed_ifaces[j] ; ++j) {
        if (0 == strncmp (allowed_ifaces[j], md_desc->md_name, strlen (md_desc->md_name)) ||
            0 == strcmp (allowed_ifaces[j], "all")) {
            found = true;
            break;
        }
    }

    if (!found) {
        /* nothing to do */
        return OPAL_SUCCESS;
    }

    md = OBJ_NEW(mca_btl_uct_md_t);

    uct_md_config_read (md_desc->md_name, NULL, NULL, &uct_config);
    uct_md_open (md_desc->md_name, uct_config, &md->uct_md);
    uct_config_release (uct_config);

    uct_md_query (md->uct_md, &md_attr);
    uct_md_query_tl_resources (md->uct_md, &tl_desc, &num_tls);

    module = mca_btl_uct_alloc_module (md_desc->md_name, md, md_attr.rkey_packed_size);
    if (NULL == module) {
        uct_release_tl_resource_list (tl_desc);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (void) mca_btl_uct_query_tls (module, md, tl_desc, num_tls);

    uct_release_tl_resource_list (tl_desc);

    /* release the initial reference to the md object. if any modules were created the UCT md will remain
     * open until those modules are finalized. */
    OBJ_RELEASE(md);

    if (NULL == module->am_tl && NULL == module->rdma_tl) {
        BTL_VERBOSE(("uct memory domain %s does not have any appropriate tls", md_desc->md_name));
        mca_btl_uct_finalize (&module->super);
        return OPAL_ERR_NOT_AVAILABLE;
    }

    mca_btl_uct_component.modules[mca_btl_uct_component.module_count++] = module;

    /* NTH: a registration cache shouldn't be necessary when using UCT but there are measurable
     * performance benefits to using rcache/grdma instead of assuming UCT will do the right
     * thing. */
    (void) asprintf (&tmp, "uct.%s", module->md_name);

    rcache_resources.cache_name     = tmp;
    rcache_resources.reg_data       = (void *) module;
    rcache_resources.sizeof_reg     = sizeof (mca_btl_uct_reg_t) + module->super.btl_registration_handle_size;
    rcache_resources.register_mem   = mca_btl_uct_reg_mem;
    rcache_resources.deregister_mem = mca_btl_uct_dereg_mem;

    module->rcache = mca_rcache_base_module_create ("grdma", module, &rcache_resources);
    free (tmp);
    if (NULL == module->rcache) {
        /* something when horribly wrong */
        BTL_VERBOSE(("could not allocate a registration cache for this btl module"));
        mca_btl_uct_finalize (&module->super);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

/*
 *  UCT component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup UCT listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

static mca_btl_base_module_t **mca_btl_uct_component_init (int *num_btl_modules, bool enable_progress_threads,
                                                           bool enable_mpi_threads)
{
    /* for this BTL to be useful the interface needs to support RDMA and certain atomic operations */
    struct mca_btl_base_module_t **base_modules;
    uct_md_resource_desc_t *resources;
    unsigned resource_count;
    char **allowed_ifaces;
    int rc;

    BTL_VERBOSE(("initializing uct btl"));

    if (NULL == mca_btl_uct_component.memory_domains || 0 == strlen (mca_btl_uct_component.memory_domains) ||
        0 == strcmp (mca_btl_uct_component.memory_domains, "none")) {
        BTL_VERBOSE(("no uct memory domains specified"));
        return NULL;
    }

    allowed_ifaces = opal_argv_split (mca_btl_uct_component.memory_domains, ',');
    if (NULL == allowed_ifaces) {
        return NULL;
    }

    uct_query_md_resources (&resources, &resource_count);

    mca_btl_uct_component.module_count = 0;

    /* generate all suitable btl modules */
    for (unsigned i = 0 ; i < resource_count ; ++i) {
        rc = mca_btl_uct_component_process_uct_md (resources + i, allowed_ifaces);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    opal_argv_free (allowed_ifaces);
    uct_release_md_resource_list (resources);

    mca_btl_uct_modex_send ();

    /* pass module array back to caller */
    base_modules = calloc (mca_btl_uct_component.module_count, sizeof (*base_modules));
    if (NULL == base_modules) {
        return NULL;
    }

    memcpy (base_modules, mca_btl_uct_component.modules, mca_btl_uct_component.module_count *
            sizeof (mca_btl_uct_component.modules[0]));

    *num_btl_modules = mca_btl_uct_component.module_count;

    BTL_VERBOSE(("uct btl initialization complete. found %d suitable memory domains",
                 mca_btl_uct_component.module_count));

    return base_modules;
}

static int mca_btl_uct_tl_progress (mca_btl_uct_tl_t *tl, int starting_index)
{
    unsigned int ret = 0;

    if (NULL == tl) {
        return 0;
    }

    for (int j = 0 ; j < tl->max_device_contexts ; ++j) {
        if (tl->uct_dev_contexts[j]) {
            ret += mca_btl_uct_context_progress (tl->uct_dev_contexts[j]);
        }
    }

    return ret;
}

static int mca_btl_uct_component_progress_pending (mca_btl_uct_module_t *uct_btl)
{
    mca_btl_uct_base_frag_t *frag, *next;
    size_t count;

    if (0 == (count = opal_list_get_size (&uct_btl->pending_frags))) {
        return 0;
    }

    OPAL_THREAD_LOCK(&uct_btl->lock);
    OPAL_LIST_FOREACH_SAFE(frag, next, &uct_btl->pending_frags, mca_btl_uct_base_frag_t) {
        if (!frag->ready) {
            continue;
        }

        opal_list_remove_item (&uct_btl->pending_frags, (opal_list_item_t *) frag);

        if (OPAL_SUCCESS > mca_btl_uct_send_frag (uct_btl, frag, false)) {
            opal_list_prepend (&uct_btl->pending_frags, (opal_list_item_t *) frag);
        }
    }
    OPAL_THREAD_UNLOCK(&uct_btl->lock);

    return OPAL_SUCCESS;
}

/**
 * @brief UCT BTL progress function
 *
 * This function explictly progresses all workers.
 */
static int mca_btl_uct_component_progress (void)
{
    int starting_index = mca_btl_uct_get_context_index ();
    unsigned ret = 0;

    for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
        mca_btl_uct_module_t *module = mca_btl_uct_component.modules[i];

        /* unlike ucp, uct actually tells us something useful! its almost like it was "inspired"
         * by the btl progress functions.... */
        ret += mca_btl_uct_tl_progress (module->rdma_tl, starting_index);

        if (module->am_tl != module->rdma_tl) {
            ret += mca_btl_uct_tl_progress (module->am_tl, starting_index);
        }

        if (module->conn_tl) {
            mca_btl_uct_pending_connection_request_t *request;

            if (module->conn_tl != module->am_tl && module->conn_tl != module->rdma_tl) {
                ret += mca_btl_uct_tl_progress (module->conn_tl, 0);
            }

            while (NULL != (request = (mca_btl_uct_pending_connection_request_t *) opal_fifo_pop_atomic (&module->pending_connection_reqs))) {
                mca_btl_uct_process_connection_request (module, (mca_btl_uct_conn_req_t *) request->request_data);
                OBJ_RELEASE(request);
            }
        }

        if (0 != opal_list_get_size (&module->pending_frags)) {
            mca_btl_uct_component_progress_pending (module);
        }
    }

    return (int) ret;
}

/** UCT btl component */
mca_btl_uct_component_t mca_btl_uct_component = {
    .super = {
        .btl_version = {
            MCA_BTL_DEFAULT_VERSION("uct"),
            .mca_open_component = mca_btl_uct_component_open,
            .mca_close_component = mca_btl_uct_component_close,
            .mca_register_component_params = mca_btl_uct_component_register,
        },
        .btl_data = {
            /* The component is not checkpoint ready */
            .param_field = MCA_BASE_METADATA_PARAM_NONE
        },

        .btl_init = mca_btl_uct_component_init,
        .btl_progress = mca_btl_uct_component_progress,
    }
};
